unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, UTransform, Math, UAppState;

type
  TCanvasState = record
    Figures: TFigureList;
    CanvasScale: double;
    Offset: TDoublePoint;
    Saved: boolean;
  end;

  { TCanvasHistory }

  TCanvasHistory = class(TPersistent)
    private
      FStates: array of TCanvasState;
      FCurrentStateIndex: integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure PushState(AState: TCanvasState);
      procedure PushState(AFigures: TFigureList; AScale: double;
        AOffset: TDoublePoint; ASaved: boolean);
      procedure PushState;
      procedure RefreshSaveStates;
      procedure Undo;
      procedure Redo;
      function CanUndo: boolean;
      function CanRedo: boolean;
      procedure Clear;
      function GetCurrentState: TCanvasState;
  end;

function CanvasState(AFigures: TFigureList; AScale: double;
  AOffset: TDoublePoint; ASaved: boolean): TCanvasState;

var
  History: TCanvasHistory;

implementation

function CanvasState(AFigures: TFigureList; AScale: double;
  AOffset: TDoublePoint; ASaved: boolean): TCanvasState;
begin
  with Result do
  begin
    Figures := AFigures;
    CanvasScale := AScale;
    Offset := AOffset;
    Saved := ASaved;
  end;
end;

{ TCanvasHistory }

constructor TCanvasHistory.Create;
begin
  FCurrentStateIndex := -1;
end;

destructor TCanvasHistory.Destroy;
var
  i, j: integer;
begin
  for i := Low(FStates) to High(FStates) do
    for j := Low(FStates[i].Figures) to High(FStates[i].Figures) do
      FreeAndNil(FStates[i].Figures[j]);
  SetLength(FStates, 0);
  inherited Destroy;
end;

procedure TCanvasHistory.PushState(AState: TCanvasState);
var
  i, j: integer;
begin
  inc(FCurrentStateIndex);
  for i := FCurrentStateIndex + 1 to High(FStates) do
    for j := Low(FStates[i].Figures) to High(FStates[i].Figures) do
      FreeAndNil(FStates[i].Figures[j]);
  SetLength(FStates, FCurrentStateIndex + 1);
  FStates[High(FStates)] := AState;
end;

procedure TCanvasHistory.PushState(AFigures: TFigureList; AScale: double;
  AOffset: TDoublePoint; ASaved: boolean);
begin
  PushState(CanvasState(AFigures, AScale, AOffset, ASaved));
end;

procedure TCanvasHistory.PushState;
begin
  PushState(CloneCanvasItems, Scale, CanvasOffset, not Modified);
end;

procedure TCanvasHistory.RefreshSaveStates;
var
  i: integer;
begin
  for i := Low(FStates) to High(FStates) do
    FStates[i].Saved := false;
  FStates[FCurrentStateIndex].Saved := not Modified;
end;

procedure TCanvasHistory.Undo;
begin
  SelectAll;
  DeleteSelected;
  FCurrentStateIndex := Max(0, FCurrentStateIndex-1);
  CanvasItems := CloneFigures(GetCurrentState.Figures);
  Modified := not GetCurrentState.Saved;
end;

procedure TCanvasHistory.Redo;
begin
  SelectAll;
  DeleteSelected;
  FCurrentStateIndex := Min(FCurrentStateIndex+1, High(FStates));
  CanvasItems := CloneFigures(GetCurrentState.Figures);
  Modified := not GetCurrentState.Saved;
end;

function TCanvasHistory.CanUndo: boolean;
begin
  Result := FCurrentStateIndex > 0;
end;

function TCanvasHistory.CanRedo: boolean;
begin
  Result := (FCurrentStateIndex > -1) and
    (FCurrentStateIndex < High(FStates));
end;

procedure TCanvasHistory.Clear;
var
  i, j: integer;
begin
  for i := Low(FStates) to High(FStates) do
    for j := Low(FStates[i].Figures) to High(FStates[i].Figures) do
      FreeAndNil(FStates[i].Figures[j]);
  SetLength(FStates, 0);
  FCurrentStateIndex := -1;
  PushState;
end;

function TCanvasHistory.GetCurrentState: TCanvasState;
begin
  Result := FStates[FCurrentStateIndex];
end;

initialization
  History := TCanvasHistory.Create;
end.

