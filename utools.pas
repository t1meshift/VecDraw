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
      FStartPoint: TPoint;
    public
      constructor Create; virtual; abstract;
      property Figure: TFigure read FFigure;
      procedure MouseDown(X, Y: integer; Button: TMouseButton); virtual;
      procedure MouseMove(X, Y: integer; Shift: TShiftState); virtual; abstract;
      function MouseUp(X, Y: integer): TFigure; virtual; abstract;
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
      function MouseUp(X, Y: integer): TFigure; override;
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
    private
      WorldStartCoord: TDoublePoint;
    public
      constructor Create; override;
      procedure MouseDown(X, Y: integer; Button: TMouseButton); override;
      procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
      function MouseUp(X, Y: integer): TFigure; override;
      function GetParams: TToolParamList; override;
  end;

  { TPolylineTool }

  TPolylineTool = class(TDrawableTool)
    procedure MouseDown(X, Y: integer; Button: TMouseButton); override;
  end;

  { TLineTool }

  TLineTool = class(TDrawableTool)
    procedure MouseDown(X, Y: integer; Button: TMouseButton); override;
  end;

  { TMagnifierTool }

  TMagnifierTool = class(TTool)
    public
      constructor Create; override;
      procedure MouseDown(X, Y: integer; Button: TMouseButton); override;
      procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
      function MouseUp(X, Y: integer): TFigure; override;
      function GetParams: TToolParamList; override;
  end;

  { TRectangleTool }

  TRectangleTool = class(TFillableTool)
    procedure MouseDown(X, Y: integer; Button: TMouseButton); override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TFillableTool)
    procedure MouseDown(X, Y: integer; Button: TMouseButton); override;
  end;

  { TRoundRectTool }

  TRoundRectTool = class(TFillableTool)
    private
      FRoundRadiusX: TIntegerParam;
      FRoundRadiusY: TIntegerParam;
    public
      constructor Create; override;
      destructor Destroy; override;
      procedure MouseDown(X, Y: integer; Button: TMouseButton); override;
      function GetParams: TToolParamList; override;
  end;

  { TPolygonTool }

  TPolygonTool = class(TFillableTool)
    private
      FVertexCount: TIntegerParam;
    public
      constructor Create; override;
      destructor Destroy; override;
      procedure MouseDown(X, Y: integer; Button: TMouseButton); override;
      procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
      function GetParams: TToolParamList; override;
  end;

var
  ToolsBase: TToolList;

implementation

{ TTool }

procedure TTool.MouseDown(X, Y: integer; Button: TMouseButton);
begin
  FStartPoint := Point(X, Y);
  FMButton := Button;
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

function TDrawableTool.MouseUp(X, Y: integer): TFigure;
begin
  Result := FFigure;
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

procedure TPolygonTool.MouseDown(X, Y: integer; Button: TMouseButton);
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
end;

procedure TPolygonTool.MouseMove(X, Y: integer; Shift: TShiftState);
var
  Diff: TPoint;
  MinDist: integer;
  DistSign: TValueSign;
begin
  if FFigure <> nil then
  begin
    Diff := Point(X - FStartPoint.x, Y - FStartPoint.y);
    MinDist := Min(abs(Diff.x), abs(Diff.y));
    DistSign := Sign(Max(Diff.x, Diff.y));
    if ssShift in Shift then
      FFigure.MouseMove(FStartPoint.x, FStartPoint.y - MinDist*DistSign)
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

procedure TRoundRectTool.MouseDown(X, Y: integer; Button: TMouseButton);
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
end;

function TRoundRectTool.GetParams: TToolParamList;
begin
  Result := TToolParamList.Create(FLineWidth, FLineStyle, FRoundRadiusX,
    FRoundRadiusY, FLineColor, FFillStyle, FFillColor);
end;

{ TEllipseTool }

procedure TEllipseTool.MouseDown(X, Y: integer; Button: TMouseButton);
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
end;

{ TRectangleTool }

procedure TRectangleTool.MouseDown(X, Y: integer; Button: TMouseButton);
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
end;

{ TMagnifierTool }

constructor TMagnifierTool.Create;
begin
  FMButton := mbExtra2;
end;

procedure TMagnifierTool.MouseDown(X, Y: integer; Button: TMouseButton);
var
  WorldStartCoord: TDoublePoint;
begin
  inherited MouseDown(X, Y, Button);
  WorldStartCoord := CanvasToWorld(FStartPoint);
  FFigure := nil;
  if (Button <> mbLeft) and (Button <> mbRight) then
    exit;
  FMButton := Button;
  FFigure := TMagnifier.Create(WorldStartCoord.x, WorldStartCoord.y);
end;

procedure TMagnifierTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  if FFigure <> nil then
    FFigure.MouseMove(X, Y);
end;

function TMagnifierTool.MouseUp(X, Y: integer): TFigure;
const
  eps = 16;
var
  WorldStartCoord, WorldEndCoord: TDoublePoint;
  TopLeft, BottomRight: TDoublePoint;
  NewScale: double;
begin
  if FFigure = nil then
    exit(nil);

  WorldStartCoord := CanvasToWorld(FStartPoint);
  WorldEndCoord := CanvasToWorld(X, Y);

  TopLeft := DoublePoint(Min(WorldStartCoord.x, WorldEndCoord.x),
    Min(WorldStartCoord.y, WorldEndCoord.y));
  BottomRight := DoublePoint(Max(WorldStartCoord.x, WorldEndCoord.x),
    Max(WorldStartCoord.y, WorldEndCoord.y));

  case FMButton of
    mbLeft:
      begin
        if Dist(FStartPoint, Point(X, Y)) < eps then
          ZoomPoint(WorldEndCoord, Scale*2)
        else
        begin
          NewScale := Max(CanvasWidth / (BottomRight.x - TopLeft.x),
            CanvasHeight / (BottomRight.y - TopLeft.y));
          if NewScale > Scale then
            ZoomRect(TopLeft, BottomRight, NewScale);
        end;
      end;
    mbRight:
      if Dist(FStartPoint, Point(X, Y)) < eps then
        ZoomPoint(WorldEndCoord, Scale/2);
  end;
  Result := nil;
  FreeAndNil(FFigure);
  FMButton := mbExtra2;
end;

function TMagnifierTool.GetParams: TToolParamList;
begin
  Result := nil;
end;

{ TLineTool }

procedure TLineTool.MouseDown(X, Y: integer; Button: TMouseButton);
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
end;

{ THandTool }

constructor THandTool.Create;
begin
  FMButton := mbExtra2;
end;

procedure THandTool.MouseDown(X, Y: integer; Button: TMouseButton);
begin
  inherited MouseDown(X, Y, Button);
  WorldStartCoord := CanvasToWorld(X, Y);
end;

procedure THandTool.MouseMove(X, Y: integer; Shift: TShiftState);
var
  WorldEndCoord: TDoublePoint;
begin
  if FMButton = mbLeft then
  begin
    WorldEndCoord := CanvasToWorld(X, Y);

    CanvasOffset.x := CanvasOffset.x + WorldStartCoord.x - WorldEndCoord.x;
    CanvasOffset.y := CanvasOffset.y + WorldStartCoord.y - WorldEndCoord.y;
  end;
end;

function THandTool.MouseUp(X, Y: integer): TFigure;
begin
  Result := nil;
  FMButton := mbExtra2;
end;

function THandTool.GetParams: TToolParamList;
begin
  Result := nil;
end;

{ TPolylineTool }

procedure TPolylineTool.MouseDown(X, Y: integer; Button: TMouseButton);
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

