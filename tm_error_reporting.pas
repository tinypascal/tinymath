unit tm_error_reporting;

// {$mode objfpc}

interface

// global_errorlist must be initialized externally!!

uses Classes, SysUtils;

const C_Error = 0;
      C_Warning = 1;
      C_Notice = 2;

procedure report_error(s: String; position, operation, errortype: Integer);

var global_error: Boolean;
    global_errorlist: TStringList;

implementation

uses tm_utils, tm_constants;

procedure report_error(s: String; position, operation, errortype: Integer);
var prefix, errortext: String;
begin
  case errortype of
    C_Error:     prefix := 'Error! ';
    C_Warning:   prefix := 'Warning! ';
    C_Notice:    prefix := 'Notice! ';
    else         prefix := '';
  end;

  if operation <> -1 then
    errortext := prefix + 'Operation ' + operationtostr(operation) + ': ' + s + ' at Position ' + IntToStr(position) + '.'
  else
    errortext := prefix + s + ' at Position ' + IntToStr(position) + '.';

  if CtmDebugMode then WriteLn(errortext)
                  else global_errorlist.Add(errortext);

  if errortype = C_Error then global_error := true;
end;


begin
  global_error := false;
  global_errorlist := nil; // must be initialized externally!!
end.

