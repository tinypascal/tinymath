unit tm_interpreter;

// {$mode objfpc}{$H+}

interface

uses tp_tinylist;

type TFuncConstants = Function(keyword: String): Double; // evaluates constants, is set externally...

function tm_interpret(rpn_stack: TTinyList): Double;

// these are set externally to communicate with the outer world
var tm_constants_func: TFuncConstants = nil;

implementation

uses Classes, Windows, SysUtils, Math, tm_syntax_tree, tm_utils, tm_constants, tm_error_reporting;

function getConstant(node: TTreeNode): Double;
begin
  Result := -1;
  if not Assigned(tm_constants_func) then Exit;

  Result := tm_constants_func(UpperCase(node.s));
end;

// In the case of .5, the algorithm uses "banker's rounding": .5 values are always rounded towards the even number.
// --> unusable for our case...
function cround(x: Double): Longint;
var z: Double;
begin
  z := abs(x);
  Result := trunc(z);
  if frac(z) >= 0.5 then inc(Result);

  Result := Result * Sign(x); // add sign again...
end;

function getFunctionResult(node: TTreeNode; function_name: String; function_arg: Double): Double;
begin
  function_name := UpperCase(function_name);
  Result := -1;
  if function_name = 'ROUND' then Result := cround(function_arg)
  else
  if function_name = 'FLOOR' then Result := floor(function_arg)
  else
  if function_name = 'CEIL' then Result := ceil(function_arg)
  else
  if function_name = 'SQR' then Result := sqr(function_arg)
  else
  if function_name = 'SQRT' then Result := sqrt(function_arg)
  else
  if function_name = 'ABS' then Result := abs(function_arg)
  else
  if function_name = 'SIGN' then Result := Sign(function_arg)
  else
  report_error('Unknown Function! ' + function_name, node.position, CopFunctionCall, C_Error);
end;

// Simple Stack Machine
function tm_interpret(rpn_stack: TTinyList): Double;
var i: Integer;
    node: TTreeNode;
    workstack: TTinyList;

function getFloat(s: String): Double;
var Double_temp: Double;
    error: Integer;
begin
  Result := -1;
  Val(s, Double_temp, error);

  if error > 0 then report_error('Expected a Number, found ' + s, -1, -1, C_Error)
               else Result := Double_temp;
end;

function getString(f: Double): String;
var check_comma: Integer;
begin
  // DoubleToStr is a very expensive function, this check is overal faster!
  if (abs(frac(f)) > 0) then Result := FloatToStrF(f, ffFixed, 8, 4)
                        else Result := IntToStr(trunc(f));

  // make sure we have . (gets rid of formatsettings)
  check_comma := Pos(',', Result);
  if check_comma > 0 then Result[check_comma] := '.';
end;

function pop_operand: Double;
var node: TTreeNode;
begin
  Result := -1;
  if workstack.Count > 0 then
  begin
    node := TTreeNode(workstack.RemoveFirst);
    Result := getFloat(node.s);
    FreeAndNil(node);
  end
  else report_error('Workstack Empty!', -1, -1, C_Error);
end;

function pop_string: String;
var node: TTreeNode;
begin
  Result := '';
  if workstack.Count > 0 then
  begin
    node := TTreeNode(workstack.RemoveFirst);
    Result := node.s;
    FreeAndNil(node);
  end
  else report_error('Workstack Empty!', -1, -1, C_Error);
end;

var op1, op2: Double;
    function_name: String;
begin
  if rpn_stack = nil then Exit;

  if CtmDebugMode then
  begin
    writeln;
    writeln('*** Interpreter ***');
  end;

  Result := 0;
  workstack := TTinyList.Create;

  while (rpn_stack.Count > 0) and (not global_error) do
  begin
//    if CtmDebugMode then writeln('');
    node := StackPop(rpn_stack);

    if not global_error then
    case node.operation of
      CopNumber, CopString:
        begin
//          if CtmDebugMode then writeln('# Push Number ', node.s);
          workstack.AddFirst(node);
        end;

      CopConstant:
        begin
//          if CtmDebugMode then writeln('# Push Constant ', node.s);
          node.s := getString(getConstant(node));
          node.operation := CopNumber;
          workstack.AddFirst(node);
        end;

      CopFunctionArg:
        begin
//          if CtmDebugMode then writeln('# Push Function Arg ', node.s);
          op1 := pop_operand;
          node.s := getString(op1);
          workstack.AddFirst(node);
        end;

      CopFunctionCall:
        begin
          op1 := pop_operand;
          function_name := pop_string;
//          if CtmDebugMode then writeln('# Function Call ', function_name, '(', getString(op1), ')');
          workstack.AddFirst(create_node(CopNumber, getString(getFunctionResult(node, function_name, op1)), -1));
          FreeAndNil(node);
        end;

      CopMathAdd:
        begin
          op2 := pop_operand;
          op1 := pop_operand;
//          if CtmDebugMode then writeln('# Add ', getString(op1), ' + ', getString(op2));
          workstack.AddFirst(create_node(CopNumber, getString(op1 + op2), -1));
          FreeAndNil(node);
        end;

      CopMathSub:
        begin
          op2 := pop_operand;
          op1 := pop_operand;
//          if CtmDebugMode then writeln('# Sub ', getString(op1), ' - ', getString(op2));
          workstack.AddFirst(create_node(CopNumber, getString(op1 - op2), -1));
          FreeAndNil(node);
        end;

      CopMathMul:
        begin
          op2 := pop_operand;
          op1 := pop_operand;
//          if CtmDebugMode then writeln('# Mul ', getString(op1), ' * ', getString(op2));
          workstack.AddFirst(create_node(CopNumber, getString(op1 * op2), -1));
          FreeAndNil(node);
        end;

      CopMathDiv:
        begin
          op2 := pop_operand;
          op1 := pop_operand;
//          if CtmDebugMode then writeln('# Div ', getString(op1), ' / ', getString(op2));
          if op2 = 0 then
          begin
            report_error('Division by Zero', node.position, CopMathDiv, C_Error);
            op2 := 1;
          end;
          workstack.AddFirst(create_node(CopNumber, getString(op1 / op2), -1));
          FreeAndNil(node);
        end;

      CopMathExp:
        begin
          op2 := pop_operand;
          op1 := pop_operand;
//          if CtmDebugMode then writeln('# Power ', getString(op1), ' ^ ', getString(op2));
          workstack.AddFirst(create_node(CopNumber, getString(power(op1, op2)), -1));
          FreeAndNil(node);
        end;

      else
        begin
          if CtmDebugMode then writeln('Ignoring Operation ' + operationtostr(node.operation));
          if node.s <> '' then write('(' + node.s + ') ')
                          else write(' ');
          FreeAndNil(node);
        end;
    end;

    // debug workstack
    if CtmDebugMode then
    begin
      write('Workstack: ');
      for i := 0 to workstack.Count - 1 do
      begin
        node := TTreeNode(workstack.Items(i));
        write(operationtostr(node.operation));
        if node.s <> '' then write('(' + node.s + ') ')
                        else write(' ');
      end;
      writeln;
    end;
  end;

  while (workstack.Count > 0) do
  begin
    node := TTreeNode(workstack.RemoveFirst);
    Result := getFloat(node.s); // gather result...
    FreeAndNil(node);
  end;
  FreeAndNil(workstack);

  while (rpn_stack.Count > 0) do
  begin
    node := TTreeNode(rpn_stack.RemoveFirst);
    Result := getFloat(node.s); // gather result...
    FreeAndNil(node);
  end;
  FreeAndNil(rpn_stack);
end;

end.

