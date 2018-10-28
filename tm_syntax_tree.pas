unit tm_syntax_tree;

// {$mode objfpc}{$H+}

interface

uses Classes, tp_tinylist;

type
    TTreeNode = class(TObject)
                  operation: Byte;
                  s: String; // all strings like identifier, constants etc
                  position: Integer; // original line in source
                  constructor Create(param_operation: Byte; param_s: String; param_position: Integer);
                end;

function create_node(operation: Byte; s: String; line: Integer): TTreeNode;
function StackPop(list: TTinyList): TTreeNode;

implementation

uses SysUtils, tm_error_reporting;

function create_node(operation: Byte; s: String; line: Integer): TTreeNode;
begin
  Result := TTreeNode.Create(operation, s, line);
end;

constructor TTreeNode.Create(param_operation: Byte; param_s: String; param_position: Integer);
begin
  operation := param_operation;
  s := param_s;
  position := param_position;
end;

function StackPeek(list: TTinyList): TTreeNode;
begin
  if list.Count < 1 then Result := nil
                    else Result := TTreeNode(list.GetFirst);
end;

function StackPop(list: TTinyList): TTreeNode;
begin
  Result := nil;
  if StackPeek(list) = nil then report_error('Stack Pop, unexpected end of Stack', -1, -1, C_Error)
  else
  begin
    Result := StackPeek(list);
    list.RemoveFirst;
  end;
end;


end.

