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

function CanvasToWorld(APoint: TPoint): TDoublePoint;
function WorldToCanvas(AX, AY: double): TPoint;
function WorldToCanvas(ADoublePoint: TDoublePoint): TPoint;

procedure SetScale(AScale: double);
procedure ZoomPoint(APoint: TDoublePoint; AScale: double);

const
  ZOOM_MIN = 0.01;
  ZOOM_MAX = 16.00;

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

function CanvasToWorld(APoint: TPoint): TDoublePoint;
begin
  with Result do
  begin
    x := APoint.x / Scale + CanvasOffset.x;
    y := APoint.y / Scale + CanvasOffset.y;
  end;
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
  if Scale < ZOOM_MIN then
     Scale := ZOOM_MIN
  else if Scale > ZOOM_MAX then
    Scale := ZOOM_MAX
  else
    Scale := AScale;
end;

procedure ZoomPoint(APoint: TDoublePoint; AScale: double);
var
  CanvasCorner, CanvasBegin: TDoublePoint;
begin
  SetScale(AScale);
  CanvasBegin := CanvasToWorld(Point(0,0));
  CanvasCorner := CanvasToWorld(Point(CanvasWidth, CanvasHeight));
  CanvasOffset.x := APoint.x - (CanvasCorner.x - CanvasBegin.x) / 2;
  CanvasOffset.y := APoint.y - (CanvasCorner.y - CanvasBegin.y) / 2;
end;

initialization

CanvasOffset := DoublePoint(0,0);
Scale := 1.0;

end.
