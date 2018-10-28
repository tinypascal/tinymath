unit tm_utils;

// {$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

function tm_is_alpha(c: Char): Boolean;
function tm_is_numeric(c: Char): Boolean;
function tm_is_operator(c: Char): Boolean;
function tm_is_whitespace(c: Char): Boolean;

function isIdentifier(op: Byte): Boolean;
function isMathOperation(op: Byte): Boolean;

function tokentostr(id: Byte): String;
function operationtostr(op: Byte): String;

implementation

uses tm_constants;

function tm_is_alpha(c: Char): Boolean;
begin
  Result := c in ['A' .. 'Z', 'a' .. 'z'];
end;

function tm_is_numeric(c: Char): Boolean;
begin
  Result := c in ['0' .. '9'];
end;

function tm_is_operator(c: Char): Boolean;
begin
  Result := c in ['+', '-', '*', '/'];
end;

function tm_is_whitespace(c: Char): Boolean;
begin
  Result := c in [#9, #10, #13, #32]; // Tab, Line Feed, Carriage Return, Space
end;

function isIdentifier(op: Byte): Boolean;
begin
  case op of
    CopNumber:
      Result := true;

    else Result := false;
  end;
end;

function isMathOperation(op: Byte): Boolean;
begin
  case op of
    CopMathAdd,
    CopMathSub,
    CopMathMul,
    CopMathDiv,
    CopMathDivInt,
    CopMathMod:
      Result := true;

    else Result := false;
  end;
end;

function tokentostr(id: Byte): String;
begin
  case id of
    CtokenIdentifier: Result := 'Identifier';
    CtokenOperator: Result := 'Operator';
    CtokenString: Result := 'String';
    CtokenNumber: Result := 'Number';
    CtokenComment: Result := 'Comment';
    CtokenSingle: Result := 'Single';
    else Result := '<Unknown Token ID ' + IntToStr(id) + '!>';
  end;
end;

function operationtostr(op: Byte): String;
begin
  case op of
    CopMathAdd: Result := '+';
    CopMathSub: Result := '-';
    CopMathMul: Result := '*';
    CopMathDiv: Result := '/';
    CopMathDivInt: Result := 'div';
    CopMathMod: Result := 'mod';
    CopNumber: Result := 'Number';
    CopString: Result := 'String';
    CopConstant: Result := 'Constant';
    CopFunctionCall: Result := 'F_Call';
    CopFunctionArg: Result := 'F_Arg';
    else Result := '<Unknown Operation ID ' + IntToStr(op) + '!>';
  end;
end;

end.

