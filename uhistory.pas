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

  { TCanvasHistory }

  TCanvasHistory = class(TPersistent)
    private
      FStates: array of TCanvasState;
      FCurrentStateIndex: integer;
    public
      constructor Create;
      procedure PushState(AState: TCanvasState);
      procedure PushState(AFigures: TFigureList; AScale: double;
        AOffset: TDoublePoint);
      procedure Undo;
      procedure Redo;
      procedure Clear;
      function GetCurrentState: TCanvasState;
  end;

function CanvasState(AFigures: TFigureList; AScale: double;
  AOffset: TDoublePoint): TCanvasState;

var
  CanvasHistory: TCanvasHistory;

implementation

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

{ TCanvasHistory }

constructor TCanvasHistory.Create;
begin
  FCurrentStateIndex := -1;
end;

procedure TCanvasHistory.PushState(AState: TCanvasState);
var
  i: integer;
begin
  inc(FCurrentStateIndex);
  SetLength(FStates, FCurrentStateIndex);
  FStates[High(FStates)] := AState;
end;

procedure TCanvasHistory.PushState(AFigures: TFigureList; AScale: double;
  AOffset: TDoublePoint);
begin
  PushState(CanvasState(AFigures, AScale, AOffset));
end;

procedure TCanvasHistory.Undo;
begin
  FCurrentStateIndex := Max(0, FCurrentStateIndex-1);
end;

procedure TCanvasHistory.Redo;
begin
  FCurrentStateIndex := Min(FCurrentStateIndex+1, High(FStates));
end;

procedure TCanvasHistory.Clear;
begin
  SetLength(FStates, 0);
  FCurrentStateIndex := -1;
end;

function TCanvasHistory.GetCurrentState: TCanvasState;
begin
  Result := FStates[FCurrentStateIndex];
end;

end.

