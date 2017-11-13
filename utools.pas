unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, UFigures, UToolParams, UTransform, Graphics,
  Math;

type
  TTool = class
    private
      FFigure: TFigure;
      FMButton: TMouseButton;
      FFigureDestroyed: boolean;
    public
      constructor Create; virtual; abstract;
      property FigureDestroyed: boolean read FFigureDestroyed;
      property Figure: TFigure read FFigure;
      function MouseDown(X, Y: integer; Button: TMouseButton): TFigure; virtual;
        abstract;
      procedure MouseMove(X, Y: integer); virtual; abstract;
      procedure MouseUp(X, Y: integer); virtual; abstract;
      function GetParams: TToolParamList; virtual; abstract;
  end;
  TToolClass = class of TTool;
  TToolList = array of TTool;
  TToolClassList = array of TToolClass;

  { TDrawableTool }

  TDrawableTool = class(TTool)
    private
      FLineWidth: TLineWidthParam;
      FLineColor: TLineColorParam;
      FLineStyle: TLineStyleParam;
    public
      constructor Create; override;
      destructor Destroy; override;
      procedure MouseMove(X, Y: integer); override;
      procedure MouseUp(X, Y: integer); override;
      function GetParams: TToolParamList; override;
  end;

  { TFillableTool }

  TFillableTool = class(TDrawableTool)
    private
      FFillColor: TFillColorParam;
      FFillStyle: TFillStyleParam;
    public
      constructor Create; override;
      destructor Destroy; override;
      function GetParams: TToolParamList; override;
  end;

  { THandTool }

  THandTool = class(TTool)
    private
      FStartPoint: TDoublePoint;
    public
      constructor Create; override;
      function MouseDown(X, Y: integer; Button: TMouseButton):TFigure; override;
      procedure MouseMove(X, Y: integer); override;
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
    private
      FStartPoint: TDoublePoint;
    public
      constructor Create; override;
      function MouseDown(X, Y: integer; Button: TMouseButton):TFigure; override;
      procedure MouseMove(X, Y: integer); override;
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
      FRoundRadiusX: TRoundRadiusXParam;
      FRoundRadiusY: TRoundRadiusYParam;
    public
      constructor Create; override;
      destructor Destroy; override;
      function MouseDown(X, Y: integer; Button: TMouseButton):TFigure; override;
      function GetParams: TToolParamList; override;
  end;

  { TPolygonTool }

  TPolygonTool = class(TFillableTool)
    private
      FVertexCount: TVertexCountParam;
    public
      constructor Create; override;
      destructor Destroy; override;
      function MouseDown(X, Y: integer; Button: TMouseButton):TFigure; override;
      function GetParams: TToolParamList; override;
  end;

var
  ToolClassBase: TToolClassList;

implementation

{ TFillableTool }

constructor TFillableTool.Create;
begin
  inherited Create;
  FFillColor := TFillColorParam.Create;
  FFillStyle := TFillStyleParam.Create;
end;

destructor TFillableTool.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FFillStyle);
  FreeAndNil(FFillColor);
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
  FLineWidth := TLineWidthParam.Create;
  FLineColor := TLineColorParam.Create;
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

procedure TDrawableTool.MouseMove(X, Y: integer);
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
  FVertexCount := TVertexCountParam.Create;
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
  WorldCoords := CanvasToWorld(X, Y);
  if Button = mbLeft then
  begin
    FFigure := TPolygon.Create(WorldCoords.x, WorldCoords.y, FLineWidth.Value,
      FLineColor.Value, FLineStyle.Value, FFillColor.Value, FFillStyle.Value,
      FVertexCount.Value);
  end;
  Result := FFigure;
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
  FRoundRadiusX := TRoundRadiusXParam.Create;
  FRoundRadiusY := TRoundRadiusYParam.Create;
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
  Result := TToolParamList.Create(FLineWidth, FLineStyle, FRoundRadiusX, FRoundRadiusY,
    FLineColor, FFillStyle, FFillColor);
end;

{ TEllipseTool }

function TEllipseTool.MouseDown(X, Y: integer; Button: TMouseButton): TFigure;
var
  WorldCoords: TDoublePoint;
begin
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
  FFigureDestroyed := false;
  FFigure := nil;
  if (Button <> mbLeft) and (Button <> mbRight) then
    exit(nil);
  FMButton := Button;
  FStartPoint := CanvasToWorld(X, Y);
  FFigure := TMagnifier.Create(FStartPoint.x, FStartPoint.y);
  Result := FFigure;
end;

procedure TMagnifierTool.MouseMove(X, Y: integer);
begin
  if FFigure <> nil then
    FFigure.MouseMove(X, Y);
end;

procedure TMagnifierTool.MouseUp(X, Y: integer);
const
  eps = 16;
var
  WorldEndCoord: TDoublePoint;
  TopLeft, BottomRight: TDoublePoint;
  NewScale: double;
begin
  if FFigure = nil then
    exit;
  WorldEndCoord := CanvasToWorld(X, Y);

  TopLeft := DoublePoint(Min(FStartPoint.x, WorldEndCoord.x),
    Min(FStartPoint.y, WorldEndCoord.y));
  BottomRight := DoublePoint(Max(FStartPoint.x, WorldEndCoord.x),
    Max(FStartPoint.y, WorldEndCoord.y));

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
      NewScale := Max(CanvasWidth / (BottomRight.x - TopLeft.x),
        CanvasHeight / (BottomRight.y - TopLeft.y));
      ZoomPoint(DoublePoint((TopLeft.x + BottomRight.x) / 2,
        (TopLeft.y + BottomRight.y) / 2), NewScale);
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
  FStartPoint := CanvasToWorld(X, Y);
  FMButton := Button;
  Result := nil;
end;

procedure THandTool.MouseMove(X, Y: integer);
var
  EndCoord: TDoublePoint;
begin
  if FMButton = mbLeft then
  begin
    EndCoord := CanvasToWorld(X, Y);
    CanvasOffset.x := CanvasOffset.x + FStartPoint.x - EndCoord.x;
    CanvasOffset.y := CanvasOffset.y + FStartPoint.y - EndCoord.y;
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
  WorldCoords := CanvasToWorld(X, Y);
  if Button = mbLeft then
  begin
    FFigure := TPolyLine.Create(WorldCoords.x, WorldCoords.y, FLineWidth.Value,
      FLineColor.Value, FLineStyle.Value);
  end;
  Result := FFigure;
end;

initialization
ToolClassBase := TToolClassList.Create(
  THandTool,
  TMagnifierTool,
  TPolylineTool,
  TLineTool,
  TRectangleTool,
  TEllipseTool,
  TRoundRectTool,
  TPolygonTool
);

end.

