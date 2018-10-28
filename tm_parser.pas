unit tm_parser;

// {$mode objfpc}{$H+}

interface

uses Classes, SysUtils, tp_tinylist;

function tm_parse(tokenlist: TTinyList): TTinyList; // Stack of TTreeNodes

implementation

uses tm_constants, tm_lexer, tm_syntax_tree, tm_error_reporting, tm_utils;

function tm_parse(tokenlist: TTinyList): TTinyList;
var stack: TTinyList;
    error_token: TToken;

// creates a new node element and pushes it to the main stack of nodes
procedure add_node(operation: Byte; s: String; position: Integer);
begin
  stack.Add(create_node(operation, s, position));
end;


// reads a token from the input
function preview_token: TToken;
begin
  Result := nil;

  if tokenlist.Count < 1 then
  begin
    report_error('Unexpected End of Input', 0, -1, C_Error);
    Result := error_token;
  end
  else Result := TToken(tokenlist.GetFirst);
end;

// reads a token from the input AND deletes it (pop)
procedure read_token;
var token: TToken;
begin
  if global_error then Exit;

  token := preview_token;
  tokenlist.RemoveFirst;
  FreeAndNil(token);
end;

// compares the string with the token, peek() does NOT read the token!
function peek(s: String): Boolean;
begin
  if tokenlist.Count < 1 then Result := false
  else                        Result := (UpperCase(s) = UpperCase(preview_token.s));
end;

// compares the string and reads (accepts) the token on a match
function accept(s: String): Boolean;
begin
  Result := false;
  if peek(s) then
  begin
    read_token;
    Result := true;
  end;
end;

// the token is read and the string must match
procedure expect(s: String);
begin
  if not accept(s) then report_error(s + ' expected, but ' + preview_token.s + ' found', preview_token.position, -1, C_Error);
end;

procedure add_expr; forward;

// primary expressions
procedure prim_expr;
var token: TToken;
begin
  if global_error then Exit;

  token := preview_token;
  if token.id = CtokenNumber then
  begin
    add_node(CopNumber, token.s, token.position); // Number 0123.4
    read_token;

    // prevents cases like 4(3+2)
    if peek('(') then report_error('Unexpected (', preview_token.position, -1, C_Error);
  end
  else
  if token.id = CtokenIdentifier then // variable or function
  begin
    add_node(CopString, token.s, token.position);
    read_token;
  end
  else
  if accept('(') then
  begin
    add_expr;
    expect(')');
    // we don't need to add any braket elements, the structure already takes care of the right execution order
  end
  else
  if accept('<') then
  begin
    token := preview_token;
    if token.id = CtokenIdentifier then // variable or function
    begin
      add_node(CopConstant, token.s, token.position);
      read_token;
    end
    else report_error('Unexpected ''' + token.s + '''', token.position, -1, C_Error);

    expect('>');
    // we don't need to add any braket elements, the structure already takes care of the right execution order
  end
  else report_error('Unexpected ''' + token.s + '''', token.position, -1, C_Error);
end;

// function calls and array / string index
procedure postfix_expr;
var position: Integer;
begin
  if global_error then Exit;
  prim_expr;
  if global_error then Exit;


  if accept('(') then // function, procedures, normal brakets like (1+5)*3 are recognized before as PRIMARY expressions!
  begin
    position := preview_token.position;

    if not accept(')') then // allows for function()
    begin
      add_expr;
      add_node(CopFunctionArg, '', position); // function argument - a function with NO arguments is recognized before, as PRIMARY expression!
      expect(')');
    end;

    add_node(CopFunctionCall, '', position); // function
  end;
end;

// order of math operations is set by the order of procedure calls
// calling the next higher one as first line in the lower one recognizes them before
procedure mul_expr;
var position: Integer;
begin
  if global_error then Exit;
  postfix_expr;
  if global_error then Exit;

  while (peek('*') or peek('/') or peek('mod') or peek ('div') or peek ('^')) and (not global_error) do
  begin
    position := preview_token.position;

    if accept('*') then
    begin
      postfix_expr;
      add_node(CopMathMul, '', position); // *
    end
    else
    if accept('/') then
    begin
      postfix_expr;
      add_node(CopMathDiv, '', position); // /
    end
    else
    if accept('mod') then
    begin
      postfix_expr;
      add_node(CopMathMod, '', position); // mod
    end
    else
    if accept('div') then
    begin
      postfix_expr;
      add_node(CopMathDivInt, '', position); // div
    end
    else
    if accept('^') then
    begin
      postfix_expr;
      add_node(CopMathExp, '', position); // Power (Exponent)
    end;
  end;
end;

// lower math expressions
procedure add_expr;
var position: Integer;
begin
  if global_error then Exit;
  mul_expr;
  if global_error then Exit;

  while (peek('+') or peek('-')) and (not global_error) do
  begin
    position := preview_token.position;

    if accept('+') then
    begin
      mul_expr; // checks for higher expressions like the 2*3 in 1+2*3
      add_node(CopMathAdd, '', position); // +
    end
    else
    if accept('-') then
    begin
      mul_expr;
      add_node(CopMathSub, '', position); // -
    end;
  end;
end;

var token: TToken;
    i: Integer;
    node: TTreeNode;
begin
  if tokenlist = nil then Exit;

  if CtmDebugMode then
  begin
    writeln;
    writeln('*** Parser ***');
  end;

  stack := TTinyList.Create; // create a new stack...

  // create dummy token for easier error handling...
  error_token := TToken.Create;
  error_token.id := 255; // unknown token
  error_token.s := '<EOF>'; // unknown string
  error_token.position := -1; // unknown line

  // accept our language....
  if (tokenlist.Count > 0) then add_expr;

  // we just expect a single expression...
  if (tokenlist.Count > 0) and (not global_error) then report_error('Unexpected expression ' + tokentostr(preview_token.id) + '(' + preview_token.s + ')! (only the first one is parsed)', preview_token.position, -1, C_Warning);

  // the syntax of the language is coded into the statement and expression procedures,
  // the parser is basically a state machine and accepts any valid program in the given language,
  // if the program is not valid, an error along the way is printed out
  if (CtmDebugMode) and (not global_error) then
  begin
    writeln;
    writeln('# Program accepted');
  end;

  // the result of this operation is a huge stack in RPN (reverse polish notation) form of arguments
  // 1 + 2 becomes 1 2 +, which is very easy to parse for e.g. a stack based machine
  if CtmDebugMode and (not global_error) then
  begin
    for i := 0 to stack.Count - 1 do
    begin
      node := TTreeNode(stack.Items(i));
      write(operationtostr(node.operation));
      if node.s <> '' then write('(' + node.s + ') ')
                      else write(' ');
    end;
    writeln;
  end;

  // finally delete the token list...
  for i := 0 to tokenlist.Count - 1 do
  begin
    token := TToken(tokenlist.Items(i));
    FreeAndNil(token);
  end;

  FreeAndNil(tokenlist);
  if error_token <> nil then FreeAndNil(error_token); // it is nil, if it was used and freed before!

  Result := stack;
end;

end.
