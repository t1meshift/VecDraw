unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, UTransform;

type
  TCanvasState = record
    Figures: TFigureList;
    CanvasScale: double;
    Offset: TDoublePoint;
  end;

procedure Undo;
procedure Redo;
procedure ClearHistory;
function CanvasState(AFigures: TFigureList; AScale: double;
  AOffset: TDoublePoint): TCanvasState;

var
  CanvasHistory: array of TCanvasState;

implementation

procedure Undo;
begin
  SetLength(CanvasHistory, Length(CanvasHistory)+1);
  CanvasHistory[High(CanvasHistory)] := CanvasState(CanvasItems, Scale,
    CanvasOffset);
  SetLength(CanvasItems, Length(CanvasItems)-1);
end;

procedure Redo;
begin

end;

procedure ClearHistory;
var
  i, j: integer;
begin
  for i := Low(CanvasHistory) to High(CanvasHistory) do
  begin
    for j := Low(CanvasHistory[i].Figures) to High(CanvasHistory[i].Figures) do
      FreeAndNil(CanvasHistory[i].Figures[j]);
    SetLength((CanvasHistory[i].Figures, 0);
    CanvasHistory[i] := nil;
  end;
end;

function CanvasState(AFigures: TFigureList; AScale: double;
  AOffset: TDoublePoint): TCanvasState;
begin
  with Result do
  begin
    Figures := AFigures;
    CanvasScale := AScale;
    Offset := AOffset;
  end;
end;

end.

