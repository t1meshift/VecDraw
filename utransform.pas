unit UTransform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDoublePoint = record
    x, y: double;
  end;

function DoublePoint(AX, AY: double): TDoublePoint;

function CanvasToWorld(AX, AY: integer): TDoublePoint;
function CanvasToWorld(APoint: TPoint): TDoublePoint;
function WorldToCanvas(AX, AY: double): TPoint;
function WorldToCanvas(ADoublePoint: TDoublePoint): TPoint;

procedure SetScale(AScale: double);
procedure ZoomPoint(APoint: TDoublePoint; AScale: double);
procedure CenterToPoint(APoint: TDoublePoint);

const
  ZOOM_MIN = 0.01;
  ZOOM_MAX = 32.00;

var
  Scale: double;
  CanvasOffset: TDoublePoint;
  CanvasWidth, CanvasHeight: integer;

implementation

function DoublePoint(AX, AY: double): TDoublePoint;
begin
  with Result do
  begin
    x := AX;
    y := AY;
  end;
end;

function CanvasToWorld(AX, AY: integer): TDoublePoint;
begin
  with Result do
  begin
    x := AX / Scale + CanvasOffset.x;
    y := AY / Scale + CanvasOffset.y;
  end;
end;

function CanvasToWorld(APoint: TPoint): TDoublePoint;
begin
  Result := CanvasToWorld(APoint.x, APoint.y);
end;

function WorldToCanvas(AX, AY: double): TPoint;
begin
  with Result do
  begin
    x := Round((AX - CanvasOffset.x) * Scale);
    y := Round((AY - CanvasOffset.y) * Scale);
  end;
end;

function WorldToCanvas(ADoublePoint: TDoublePoint): TPoint;
begin
  Result := WorldToCanvas(ADoublePoint.x, ADoublePoint.y);
end;

procedure SetScale(AScale: double);
begin
  if AScale <= ZOOM_MIN then
     Scale := ZOOM_MIN
  else if AScale >= ZOOM_MAX then
    Scale := ZOOM_MAX
  else
    Scale := AScale;
end;

procedure ZoomPoint(APoint: TDoublePoint; AScale: double);
var
  PrevScale: double;
  ScreenPos: TPoint;
begin
  ScreenPos := WorldToCanvas(APoint);
  PrevScale := Scale;
  SetScale(AScale);
  if Scale = PrevScale then
    exit;
  CanvasOffset.x := APoint.x - (ScreenPos.x / Scale);
  CanvasOffset.y := APoint.y - (ScreenPos.y / Scale);
end;



procedure CenterToPoint(APoint: TDoublePoint);
var
  CanvasCorner: TDoublePoint;
begin
  CanvasCorner := CanvasToWorld(CanvasWidth, CanvasHeight);
  CanvasOffset.x := APoint.x - (CanvasCorner.x - CanvasOffset.x) / 2;
  CanvasOffset.y := APoint.y - (CanvasCorner.y - CanvasOffset.y) / 2;
end;



initialization

CanvasOffset := DoublePoint(0,0);
Scale := 1.0;

end.

