unit UFiguresBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures;

type
  TFiguresList = array of TFigureClass;

procedure RegisterFigures(AFigures: array of TClass);

var
  FiguresBase: TFiguresList;

implementation

procedure RegisterFigures(AFigures: array of TClass);
var
  i: TClass;
begin
  for i in AFigures do
  begin
    SetLength(FiguresBase, Length(FiguresBase) + 1);
    FiguresBase[High(FiguresBase)] := TFigureClass(i);
  end;
end;

initialization

RegisterFigures([
  TPolyLine,
  TLine,
  TRectangle,
  TEllipse
  //and so on...
]);
end.

