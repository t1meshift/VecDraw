unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, UFigures, UToolParams, UTransform, Graphics,
  Math;

type

  { TTool }

  TTool = class
    private
      FFigure: TFigure;
      FMButton: TMouseButton;
      FFigureDestroyed: boolean;
      FStartPoint: TPoint;
    public
      constructor Create; virtual; abstract;
      property FigureDestroyed: boolean read FFigureDestroyed;
      property Figure: TFigure read FFigure;
      function MouseDown(X, Y: integer; Button: TMouseButton): TFigure; virtual;
      procedure MouseMove(X, Y: integer; Shift: TShiftState); virtual; abstract;
      procedure MouseUp(X, Y: integer); virtual; abstract;
      function GetParams: TToolParamList; virtual; abstract;
  end;
  TToolClass = class of TTool;
  TToolList = array of TTool;
  TToolClassList = array of TToolClass;

  { TDrawableTool }

  TDrawableTool = class(TTool)
    private
      FLineWidth: TIntegerParam;
      FLineColor: TColorParam;
      FLineStyle: TLineStyleParam;
    public
      constructor Create; override;
      destructor Destroy; override;
      procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
      procedure MouseUp(X, Y: integer); override;
      function GetParams: TToolParamList; override;
  end;

  { TFillableTool }

  TFillableTool = class(TDrawableTool)
    private
      FFillColor: TColorParam;
      FFillStyle: TFillStyleParam;
    public
      constructor Create; override;
      destructor Destroy; override;
      procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
      function GetParams: TToolParamList; override;
  end;

  { THandTool }

  THandTool = class(TTool)
    public
      constructor Create; override;
      function MouseDown(X, Y: integer; Button: TMouseButton):TFigure; override;
      procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
      procedure MouseUp(X, Y: integer); override;
      function GetParams: TToolParamList; override;
  end;

  { TPolylineTool }

  TPolylineTool = class(TDrawableTool)
    function MouseDown(X, Y: integer; Button: TMouseButton):TFigure; override;
  end;

  { TLineTool }

  TLineTool = class(TDrawableTool)
    function MouseDown(X, Y: integer; Button: TMouseButton): TFigure; override;
  end;

  { TMagnifierTool }

  TMagnifierTool = class(TTool)
    public
      constructor Create; override;
      function MouseDown(X, Y: integer; Button: TMouseButton):TFigure; override;
      procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
      procedure MouseUp(X, Y: integer); override;
      function GetParams: TToolParamList; override;
  end;

  { TRectangleTool }

  TRectangleTool = class(TFillableTool)
    function MouseDown(X, Y: integer; Button: TMouseButton):TFigure; override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TFillableTool)
    function MouseDown(X, Y: integer; Button: TMouseButton): TFigure; override;
  end;

  { TRoundRectTool }

  TRoundRectTool = class(TFillableTool)
    private
      FRoundRadiusX: TIntegerParam;
      FRoundRadiusY: TIntegerParam;
    public
      constructor Create; override;
      destructor Destroy; override;
      function MouseDown(X, Y: integer; Button: TMouseButton):TFigure; override;
      function GetParams: TToolParamList; override;
  end;

  { TPolygonTool }

  TPolygonTool = class(TFillableTool)
    private
      FVertexCount: TIntegerParam;
    public
      constructor Create; override;
      destructor Destroy; override;
      function MouseDown(X, Y: integer; Button: TMouseButton):TFigure; override;
      procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
      function GetParams: TToolParamList; override;
  end;

var
  ToolsBase: TToolList;

implementation

{ TTool }

function TTool.MouseDown(X, Y: integer; Button: TMouseButton): TFigure;
begin
  FStartPoint := Point(X, Y);
end;

{ TFillableTool }

constructor TFillableTool.Create;
begin
  inherited Create;
  FFillColor := TColorParam.Create('Fill color', clWhite);
  FFillStyle := TFillStyleParam.Create;
end;

destructor TFillableTool.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FFillStyle);
  FreeAndNil(FFillColor);
end;

procedure TFillableTool.MouseMove(X, Y: integer; Shift: TShiftState);
var
  Diff: TPoint;
  MinDist: integer;
begin
  if FFigure <> nil then
  begin
    Diff := Point(X - FStartPoint.x, Y - FStartPoint.y);
    MinDist := Min(Diff.x, Diff.y);
    if ssShift in Shift then
      FFigure.MouseMove(FStartPoint.x + MinDist, FStartPoint.y + MinDist)
    else
      FFigure.MouseMove(X, Y);
  end;
end;

function TFillableTool.GetParams: TToolParamList;
begin
  Result := TToolParamList.Create(FLineWidth, FLineStyle, FLineColor,
    FFillStyle, FFillColor);
end;

{ TDrawableTool }

constructor TDrawableTool.Create;
begin
  FFigureDestroyed := false;
  FFigure := nil;
  FLineWidth := TIntegerParam.Create('Line width', 1, 100, 1);
  FLineColor := TColorParam.Create('Line color', clBlack);
  FLineStyle := TLineStyleParam.Create;
end;

destructor TDrawableTool.Destroy;
begin
  inherited Destroy;
  //they aren't being destroyed without this. is it a bug or a feature?
  FreeAndNil(FLineWidth);
  FreeAndNil(FLineColor);
  FreeAndNil(FLineStyle);
end;

procedure TDrawableTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  if FFigure <> nil then
    FFigure.MouseMove(X, Y);
end;

procedure TDrawableTool.MouseUp(X, Y: integer);
begin
  FFigure := nil;
end;

function TDrawableTool.GetParams: TToolParamList;
begin
  Result := TToolParamList.Create(FLineWidth, FLineStyle, FLineColor);
end;


{ TPolygonTool }

constructor TPolygonTool.Create;
begin
  inherited Create;
  FVertexCount := TIntegerParam.Create('Vertexes count', 3, 50, 3);
end;

destructor TPolygonTool.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FVertexCount);
end;

function TPolygonTool.MouseDown(X, Y: integer; Button: TMouseButton): TFigure;
var
  WorldCoords: TDoublePoint;
begin
  inherited MouseDown(X, Y, Button);
  WorldCoords := CanvasToWorld(X, Y);
  if Button = mbLeft then
  begin
    FFigure := TPolygon.Create(WorldCoords.x, WorldCoords.y, FLineWidth.Value,
      FLineColor.Value, FLineStyle.Value, FFillColor.Value, FFillStyle.Value,
      FVertexCount.Value);
  end;
  Result := FFigure;
end;

procedure TPolygonTool.MouseMove(X, Y: integer; Shift: TShiftState);
var
  Diff: TPoint;
  MaxDist: integer;
begin
  if FFigure <> nil then
  begin
    Diff := Point(X - FStartPoint.x, Y - FStartPoint.y);
    MaxDist := Max(Diff.x, Diff.y);
    if ssShift in Shift then
      FFigure.MouseMove(FStartPoint.x, FStartPoint.y - MaxDist)
    else
      FFigure.MouseMove(X, Y);
  end;
end;

function TPolygonTool.GetParams: TToolParamList;
begin
  Result := TToolParamList.Create(FVertexCount, FLineWidth, FLineStyle,
    FLineColor, FFillStyle, FFillColor);
end;

{ TRoundRectTool }

constructor TRoundRectTool.Create;
begin
  inherited Create;
  FRoundRadiusX := TIntegerParam.Create('Rounding radius (X)', 0, 1000, 30);
  FRoundRadiusY := TIntegerParam.Create('Rounding radius (Y)', 0, 1000, 30);
end;

destructor TRoundRectTool.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FRoundRadiusX);
  FreeAndNil(FRoundRadiusY);
end;

function TRoundRectTool.MouseDown(X, Y: integer; Button: TMouseButton): TFigure;
var
  WorldCoords: TDoublePoint;
begin
  inherited MouseDown(X, Y, Button);
  WorldCoords := CanvasToWorld(X, Y);
  if Button = mbLeft then
  begin
    FFigure := TRoundRect.Create(WorldCoords.x, WorldCoords.y, FLineWidth.Value,
      FLineColor.Value, FLineStyle.Value, FFillColor.Value, FFillStyle.Value,
      FRoundRadiusX.Value, FRoundRadiusY.Value);
  end;
  Result := FFigure;
end;

function TRoundRectTool.GetParams: TToolParamList;
begin
  Result := TToolParamList.Create(FLineWidth, FLineStyle, FRoundRadiusX,
    FRoundRadiusY, FLineColor, FFillStyle, FFillColor);
end;

{ TEllipseTool }

function TEllipseTool.MouseDown(X, Y: integer; Button: TMouseButton): TFigure;
var
  WorldCoords: TDoublePoint;
begin
  inherited MouseDown(X, Y, Button);
  WorldCoords := CanvasToWorld(X, Y);
  if Button = mbLeft then
  begin
    FFigure := TEllipse.Create(WorldCoords.x, WorldCoords.y, FLineWidth.Value,
      FLineColor.Value, FLineStyle.Value, FFillColor.Value, FFillStyle.Value);
  end;
  Result := FFigure;
end;

{ TRectangleTool }

function TRectangleTool.MouseDown(X, Y: integer; Button: TMouseButton): TFigure;
var
  WorldCoords: TDoublePoint;
begin
  inherited MouseDown(X, Y, Button);
  WorldCoords := CanvasToWorld(X, Y);
  if Button = mbLeft then
  begin
    FFigure := TRectangle.Create(WorldCoords.x, WorldCoords.y, FLineWidth.Value,
      FLineColor.Value, FLineStyle.Value, FFillColor.Value, FFillStyle.Value);
  end;
  Result := FFigure;
end;

{ TMagnifierTool }

constructor TMagnifierTool.Create;
begin
  FMButton := mbExtra2;
  FFigureDestroyed := false;
end;

function TMagnifierTool.MouseDown(X, Y: integer; Button: TMouseButton): TFigure;
begin
  inherited MouseDown(X, Y, Button);
  FFigureDestroyed := false;
  FFigure := nil;
  if (Button <> mbLeft) and (Button <> mbRight) then
    exit(nil);
  FMButton := Button;
  FFigure := TMagnifier.Create(FStartPoint.x, FStartPoint.y);
  Result := FFigure;
end;

procedure TMagnifierTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  if FFigure <> nil then
    FFigure.MouseMove(X, Y);
end;

procedure TMagnifierTool.MouseUp(X, Y: integer);
const
  eps = 16;
var
  WorldStartCoord, WorldEndCoord: TDoublePoint;
  TopLeft, BottomRight: TPoint;
  RegionCenter: TDoublePoint;
  NewScale: double;
begin
  if FFigure = nil then
    exit;
  WorldStartCoord := CanvasToWorld(FStartPoint);
  WorldEndCoord := CanvasToWorld(X, Y);

  TopLeft := WorldToCanvas(Min(WorldStartCoord.x, WorldEndCoord.x),
    Min(WorldStartCoord.y, WorldEndCoord.y));
  BottomRight := WorldToCanvas(Max(FStartPoint.x, WorldEndCoord.x),
    Max(WorldStartCoord.y, WorldEndCoord.y));

  if (sqr(TopLeft.x - BottomRight.x) + sqr(TopLeft.y - BottomRight.y) < eps*eps)
  then
  begin
    case FMButton of
      mbLeft: ZoomPoint(CanvasToWorld(X, Y), Scale*2);
      mbRight: ZoomPoint(CanvasToWorld(X, Y), Scale/2);
    end;
  end
  else
  begin
    if (TopLeft.x <> BottomRight.x) and (TopLeft.y <> BottomRight.y) then
    begin
      RegionCenter := DoublePoint((TopLeft.x + BottomRight.x) / 2,
        (TopLeft.y + BottomRight.y) / 2);
      NewScale := Max(CanvasWidth / (BottomRight.x - TopLeft.x),
        CanvasHeight / (BottomRight.y - TopLeft.y));
      ZoomPoint(RegionCenter, NewScale);
      CenterToPoint(RegionCenter);
    end;
  end;
  FFigure := nil;
  FFigureDestroyed := true;
  FMButton := mbExtra2;
end;

function TMagnifierTool.GetParams: TToolParamList;
begin
  Result := nil;
end;

{ TLineTool }

function TLineTool.MouseDown(X, Y: integer; Button: TMouseButton): TFigure;
var
  WorldCoords: TDoublePoint;
begin
  inherited MouseDown(X, Y, Button);
  WorldCoords := CanvasToWorld(X, Y);
  if Button = mbLeft then
  begin
    FFigure := TLine.Create(WorldCoords.x, WorldCoords.y, FLineWidth.Value,
      FLineColor.Value, FLineStyle.Value);
  end;
  Result := FFigure;
end;

{ THandTool }

constructor THandTool.Create;
begin
  FMButton := mbExtra2;
  FFigureDestroyed := false;
end;

function THandTool.MouseDown(X, Y: integer; Button: TMouseButton): TFigure;
begin
  inherited MouseDown(X, Y, Button);
  FMButton := Button;
  Result := nil;
end;

procedure THandTool.MouseMove(X, Y: integer; Shift: TShiftState);
var
  WorldStartCoord, WorldEndCoord: TDoublePoint;
begin
  if FMButton = mbLeft then
  begin
    WorldStartCoord := CanvasToWorld(FStartPoint);
    WorldEndCoord := CanvasToWorld(X, Y);
    CanvasOffset.x := CanvasOffset.x + WorldStartCoord.x - WorldEndCoord.x;
    CanvasOffset.y := CanvasOffset.y + WorldStartCoord.y - WorldEndCoord.y;
  end;
end;

procedure THandTool.MouseUp(X, Y: integer);
begin
  FMButton := mbExtra2;
end;

function THandTool.GetParams: TToolParamList;
begin
  Result := nil;
end;

{ TPolylineTool }

function TPolylineTool.MouseDown(X, Y: integer; Button: TMouseButton): TFigure;
var
  WorldCoords: TDoublePoint;
begin
  inherited MouseDown(X, Y, Button);
  WorldCoords := CanvasToWorld(X, Y);
  if Button = mbLeft then
  begin
    FFigure := TPolyLine.Create(WorldCoords.x, WorldCoords.y, FLineWidth.Value,
      FLineColor.Value, FLineStyle.Value);
  end;
  Result := FFigure;
end;

initialization
ToolsBase := TToolList.Create(
  THandTool.Create,
  TMagnifierTool.Create,
  TPolylineTool.Create,
  TLineTool.Create,
  TRectangleTool.Create,
  TEllipseTool.Create,
  TRoundRectTool.Create,
  TPolygonTool.Create
);

end.

