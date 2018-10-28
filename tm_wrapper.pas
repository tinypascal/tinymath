unit tm_wrapper;

// {$mode objfpc}{$H+}

interface

uses Classes, tm_constants, tm_lexer, tm_parser, tm_interpreter, tm_error_reporting;

function tm_execute(s: String; errorlist: TStringList): Double;

implementation

uses SysUtils;

function tm_execute(s: String; errorlist: TStringList): Double;
var x: Double;
begin
  CtmDebugMode := true; // enable for extended debug information

  Result := 0;

  s := Trim(s);
  // skip execution on empty string, also set error flag so the result value is invalid...
  if s = '' then
  begin
    global_error := true;
    Exit;
  end;

  global_error := false; // reset error
  global_errorlist := errorlist;
  x := tm_interpret(tm_parse(tm_lex(s)));
  if not global_error then Result := x;
end;

end.

