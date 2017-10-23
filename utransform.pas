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
    x := (APoint.x + CanvasOffset.x) / Scale;
    y := (APoint.y + CanvasOffset.y) / Scale;
  end;
end;

function WorldToCanvas(AX, AY: double): TPoint;
begin
  with Result do
  begin
    x := Round(AX * Scale - CanvasOffset.x);
    y := Round(AY * Scale - CanvasOffset.y);
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
  ScreenCorner, ScreenCenter, ScreenBegin, t: TDoublePoint;
begin
  //ScreenCenter := CanvasToWorld(Point(CanvasWidth div 2, CanvasHeight div 2));
  SetScale(AScale);
  ScreenBegin := CanvasToWorld(Point(0,0));
  ScreenCorner := CanvasToWorld(Point(CanvasWidth, CanvasHeight));
  t.x := APoint.x;// - (ScreenCorner.x-ScreenBegin.x)/2;
  t.y := APoint.y;//S - (ScreenCorner.y-ScreenBegin.y)/2;
  CanvasOffset.x := t.x;
  CanvasOffset.y := t.y;
end;

initialization

CanvasOffset := DoublePoint(0,0);
Scale := 1.0;

end.

