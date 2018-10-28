unit tm_constants;

// {$mode objfpc}{$H+}

interface

const
  CtmDebugMode: Boolean = false;

  CtokenIdentifier = 1;
  CtokenOperator = 2;
  CtokenString = 3;
  CtokenNumber = 4;
  CtokenComment = 5;
  CtokenSingle = 6;

  CopMathAdd = 1;
  CopMathSub = 2;
  CopMathMul = 3;
  CopMathDiv = 4;
  CopMathDivInt = 5;
  CopMathMod = 6;
  CopMathExp = 7;
  CopNumber = 8;
  CopString = 9;
  CopConstant = 10;
  CopFunctionCall = 11;
  CopFunctionArg = 12;

implementation

end.

