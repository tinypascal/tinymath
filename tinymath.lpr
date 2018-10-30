program tinymath;

{$mode objfpc}{$H+}

uses Classes, SysUtils, tm_wrapper;

var errorlist: TStringList;
    i: Integer;
    s: String;
begin
  errorlist := TStringList.Create;

  s := '4 + 2 * (5 - 2)'; // = 10
  // using a wrapper function to call lexer, parser and interpreter
  writeln('Result: ', tm_execute(s, errorlist):2:8);

  if errorlist.Count > 0 then
  begin
    writeln('Errors occured:');
    for i := 0 to errorlist.Count - 1 do WriteLn(errorlist.Strings[i]);
  end;

  FreeAndNil(errorlist);
  readln;
end.

