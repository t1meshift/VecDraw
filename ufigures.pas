unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UTransform, Math, Controls;

type

  TPointList = array of TPoint;
  TDPointList = array of TDoublePoint;

 { TFigure }

  TFigure = class
  protected
    Vertexes: TDPointList;
    LineColor, FillColor: TColor;
    LineWidth: integer;
    LineStyle: TPenStyle;
    FillStyle: TBrushStyle;
    Button: TMouseButton;
    procedure SetCanvasStyles(ACanvas: TCanvas);
  public
    CanBeDestroyed: boolean;
    constructor Create(X, Y: double; ALineColor: TColor; ALineWidth: integer;
      AFillColor: TColor; ALineStyle: TPenStyle; AFillStyle: TBrushStyle;
      AButton: TMouseButton);
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    procedure MouseMove(X, Y: integer); virtual; abstract;
    procedure MouseUp(X, Y: integer); virtual;
  end;

  TFigureList = array of TFigure;
  TFigureClass = class of TFigure;
  TFigureClassList = array of TFigureClass;

  { THand }

  THand = class(TFigure)
    procedure MouseMove(X, Y: integer); override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TMagnifier }

  TMagnifier = class(TFigure)
    procedure MouseMove(X, Y: integer); override;
    procedure MouseUp(X, Y: integer); override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

 { TPolyLine }

  TPolyLine = class(TFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
    procedure MouseMove(X, Y: integer); override;
  end;

  { TLine }

  TLine = class(TPolyLine)
  public
    procedure MouseMove(X, Y: integer); override;
  end;

  { TRectangle }

  TRectangle = class(TFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
    procedure MouseMove(X, Y: integer); override;
  end;

  { TEllipse }

  TEllipse = class(TFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
    procedure MouseMove(X, Y: integer); override;
  end;

procedure RegisterFigures(AFigures: TFigureClassList);

var
  FiguresBase: TFigureClassList;

implementation

{ TMagnifier }

procedure TMagnifier.MouseMove(X, Y: integer);
begin
  Vertexes[1] := CanvasToWorld(Point(X, Y));
end;

procedure TMagnifier.MouseUp(X, Y: integer);
const
  eps = 10;
var
  TopLeft, BottomRight: TDoublePoint;
  NewScale: double;
begin
  inherited MouseUp(X, Y);

  TopLeft := DoublePoint(Min(Vertexes[0].x, Vertexes[1].x),
    Min(Vertexes[0].y, Vertexes[1].y));

  BottomRight := DoublePoint(Max(Vertexes[0].x, Vertexes[1].x),
    Max(Vertexes[0].y, Vertexes[1].y));

  if sqr(TopLeft.x - BottomRight.x) + sqr(TopLeft.y - BottomRight.y) < sqr(eps)
  then
  begin
    case Button of
      mbLeft: ZoomPoint(CanvasToWorld(Point(X, Y)), Scale*2);
      mbRight: ZoomPoint(CanvasToWorld(Point(X, Y)), Scale/2);
    end;
  end
  else
  begin
    NewScale := Scale * Max((CanvasWidth / Scale) / (BottomRight.x - TopLeft.x),
      (CanvasHeight / Scale) / (BottomRight.y - TopLeft.y));
    ZoomPoint(DoublePoint((TopLeft.x + BottomRight.x) / 2,
      (TopLeft.y + BottomRight.y) / 2), NewScale);
  end;

  Vertexes[0] := DoublePoint(0,0);
  Vertexes[1] := Vertexes[0];
  CanBeDestroyed := true;
end;

procedure TMagnifier.Draw(ACanvas: TCanvas);
var
  TopLeft, BottomRight: TPoint;
begin
  TopLeft := WorldToCanvas(Vertexes[0]);
  BottomRight := WorldToCanvas(Vertexes[1]);
  with ACanvas do
  begin
    Pen.Style := psDash;
    Pen.Width := 1;
    Pen.Color := clBlack;
    Pen.Mode := pmNot;
    Brush.Style := bsClear;
    Rectangle(TopLeft.x, TopLeft.y, BottomRight.x, BottomRight.y);
    Pen.Mode := pmCopy;
  end;
end;

{ THand }

procedure THand.MouseMove(X, Y: integer);
begin
  Vertexes[1] := CanvasToWorld(Point(X, Y));
  CanvasOffset.x := CanvasOffset.x + (Vertexes[0].x - Vertexes[1].x);
  CanvasOffset.y := CanvasOffset.y + (Vertexes[0].y - Vertexes[1].y);
end;

procedure THand.Draw(ACanvas: TCanvas);
begin
  //Dummy
end;

{ TFigure }

constructor TFigure.Create(X, Y: double;
  ALineColor: TColor; ALineWidth: integer; AFillColor: TColor;
  ALineStyle: TPenStyle; AFillStyle: TBrushStyle; AButton: TMouseButton);
begin
  SetLength(Vertexes, 2);
  Vertexes[0] := DoublePoint(X, Y);
  Vertexes[1] := Vertexes[0];
  LineColor := ALineColor;
  LineWidth := ALineWidth;
  LineStyle := ALineStyle;
  FillColor := AFillColor;
  FillStyle := AFillStyle;
  Button := AButton;
  CanBeDestroyed := false;
end;

procedure TFigure.MouseUp(X, Y: integer);
begin
  //dummy
end;

procedure TFigure.SetCanvasStyles(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Color := LineColor;
    Pen.Width := LineWidth;
    Pen.Style := LineStyle;
    Brush.Color := FillColor;
    Brush.Style := FillStyle;
  end;
end;

{ TPolyLine }

procedure TPolyLine.Draw(ACanvas: TCanvas);
var
  CanvasVertexes: TPointList;
  i: integer;
begin
  SetCanvasStyles(ACanvas);
  SetLength(CanvasVertexes, Length(Vertexes));
  for i := Low(Vertexes) to High(Vertexes) do
    CanvasVertexes[i] := WorldToCanvas(Vertexes[i]);
  ACanvas.Polyline(CanvasVertexes);
end;

procedure TPolyLine.MouseMove(X, Y: integer);
begin
  if Button = mbLeft then
  begin
    SetLength(Vertexes, Length(Vertexes) + 1);
    Vertexes[High(Vertexes)] := CanvasToWorld(Point(X, Y));
  end
  else
    CanBeDestroyed := true;
end;

{ TLine }

procedure TLine.MouseMove(X, Y: integer);
begin
  if Button = mbLeft then
    Vertexes[High(Vertexes)] := CanvasToWorld(Point(X, Y))
  else
    CanBeDestroyed := true;
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
var
  CanvasTopLeft, CanvasBottomRight: TPoint;
begin
  SetCanvasStyles(ACanvas);
  CanvasTopLeft := WorldToCanvas(Vertexes[0].x, Vertexes[0].y);
  CanvasBottomRight := WorldToCanvas(Vertexes[1].x, Vertexes[1].y);
  ACanvas.Rectangle(CanvasTopLeft.x, CanvasTopLeft.y,
    CanvasBottomRight.x, CanvasBottomRight.y);
end;

procedure TRectangle.MouseMove(X, Y: integer);
begin
  if Button = mbLeft then
    Vertexes[High(Vertexes)] := CanvasToWorld(Point(X, Y))
  else
    CanBeDestroyed := true;
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
var
  CanvasTopLeft, CanvasBottomRight: TPoint;
begin
  SetCanvasStyles(ACanvas);
  CanvasTopLeft := WorldToCanvas(Vertexes[0].x, Vertexes[0].y);
  CanvasBottomRight := WorldToCanvas(Vertexes[1].x, Vertexes[1].y);
  ACanvas.Ellipse(CanvasTopLeft.x, CanvasTopLeft.y,
    CanvasBottomRight.x, CanvasBottomRight.y);
end;

procedure TEllipse.MouseMove(X, Y: integer);
begin
  if Button = mbLeft then
    Vertexes[High(Vertexes)] := CanvasToWorld(Point(X, Y))
  else
    CanBeDestroyed := true;
end;

procedure RegisterFigures(AFigures: TFigureClassList);
var
  i: TFigureClass;
begin
  for i in AFigures do
  begin
    SetLength(FiguresBase, Length(FiguresBase) + 1);
    FiguresBase[High(FiguresBase)] := i;
  end;
end;

initialization

RegisterFigures(TFigureClassList.Create(
  THand,
  TMagnifier,
  TPolyLine,
  TLine,
  TRectangle,
  TEllipse
  //and so on...
));

end.

