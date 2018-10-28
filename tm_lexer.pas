unit tm_lexer;

// {$mode objfpc}{$H+}

interface

uses Classes, SysUtils, tp_tinylist;

type
  TToken = class(TObject)
             id: Byte;
             s: String;
             position: Integer;
           end;

function tm_lex(inputstring: String): TTinyList;

implementation

uses tm_constants, tm_utils;

{
CtokenIdentifier    variables or function calls round() ceil() floor() etc.
CtokenOperator      + - * /
CtokenNumber        100 100.34 -100
CtokenIdentifier    starts with alpha and then any combination of alpha _ and numeric for example counter_2
CtokenSingle        any single character left unrecognized, such as ; , ( ) [ ]
}
function tm_lex(inputstring: String): TTinyList;
var c: Char;
    position, token_start_position: Integer;
    tokenlist: TTinyList;

// preview next char
function next_ch: Char;
begin
  Result := c;
end;

// return buffer and fill it with the next char from file
function get_ch: Char;
begin
  Result := c;

  if inputstring = '' then c := #0
  else
  begin
    c := inputstring[1];
    Delete(inputstring, 1, 1);
    inc(position);
  end;
end;

procedure skip_whitespace;
begin
  while (tm_is_whitespace(next_ch)) do get_ch;
end;

// recognizes and reads (= deletes from input stream) a token
procedure tm_lex_read_token;
var s: String;
    id: Byte;
    token: TToken;
begin
  token_start_position := position;

  s := get_ch;

  if tm_is_alpha(s[1]) then // Identifier: keywords, variables, functions
  begin
    id := CtokenIdentifier;
    while ((tm_is_alpha(next_ch)) or (tm_is_numeric(next_ch)) or (next_ch = '_')) do s := s + get_ch;
  end
  else
  if tm_is_operator(s[1]) then // Operator: + - * /
  begin
    // exception for negative number or additional + ...
    if ((s[1] = '-') or (s[1] = '+')) and (tm_is_numeric(next_ch)) then
    begin
      id := CtokenNumber;
      while (tm_is_numeric(next_ch) or (next_ch = '.')) do s := s + get_ch;
    end
    else
    begin
      id := CtokenOperator;
      while (tm_is_operator(next_ch)) do s := s + get_ch;
    end
  end
  else
  if tm_is_numeric(s[1]) then // Number: 78 or 0.234, + and - are ignored and evaluated later
  begin
    id := CtokenNumber;
    while (tm_is_numeric(next_ch) or (next_ch = '.') or (next_ch = ',')) do s := s + get_ch;
  end
  else id := CtokenSingle; // single token: ( ) ;

  token := TToken.Create;
  token.id := id;
  token.s := s;
  token.position := token_start_position;
  tokenlist.Add(token);

  skip_whitespace; // skips any whitespace between two commands, before or after a linebreak doesn't matter
end;

var i: Integer;
begin
  Result := nil;

  if CtmDebugMode then writeln('*** Lexer ***');

  tokenlist := TTinyList.Create;
  position := 0; // Pos is increased after read, so 1 for first element
  token_start_position := position;

  get_ch; // fill "next_ch" buffer for first time use

  skip_whitespace; // skip the initial whitespace in the file, if any
  while (next_ch <> #0) do tm_lex_read_token; // #0 is our internal symbol for end of file / input, it is NOT allowed as character in the sourcecode!

  // Debug: output our list of token...
  if CtmDebugMode then
  for i := 0 to tokenlist.Count - 1 do
  begin
    with TToken(tokenlist.Items(i)) do
    WriteLn(IntToStr(position) + ' ' + tokentostr(id) + ': ' + s);
  end;

  Result := tokenlist;
end;

end.

