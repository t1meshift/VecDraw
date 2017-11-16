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
      function FTopLeft: TDoublePoint;
      function FBottomRight: TDoublePoint;
      procedure SetCanvasStyles(ACanvas: TCanvas);
    public
      property TopLeftBorder: TDoublePoint read FTopLeft;
      property BottomRightBorder: TDoublePoint read FBottomRight;
      procedure Draw(ACanvas: TCanvas); virtual; abstract;
      procedure MouseMove(X, Y: integer); virtual; abstract;
  end;

  TFigureList = array of TFigure;
  TFigureClass = class of TFigure;

  { TMagnifier }

  TMagnifier = class(TFigure)
    constructor Create(X, Y: double);
    procedure MouseMove(X, Y: integer); override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

 { TPolyLine }

  TPolyLine = class(TFigure)
    public
      constructor Create(X, Y: double; ALineWidth: integer; ALineColor: TColor;
        ALineStyle: TPenStyle);
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
      constructor Create(X, Y: double; ALineWidth: integer; ALineColor: TColor;
        ALineStyle: TPenStyle; AFillColor: TColor; AFillStyle: TBrushStyle);
      procedure Draw(ACanvas: TCanvas); override;
      procedure MouseMove(X, Y: integer); override;
  end;

  { TRoundRect }

  TRoundRect = class(TFigure)
    private
      RoundX, RoundY: integer;
    public
      constructor Create(X, Y: double; ALineWidth: integer; ALineColor: TColor;
        ALineStyle: TPenStyle; AFillColor: TColor; AFillStyle: TBrushStyle;
        ARoundX, ARoundY: integer);
      procedure Draw(ACanvas: TCanvas); override;
      procedure MouseMove(X, Y: integer); override;
  end;

  { TEllipse }

  TEllipse = class(TFigure)
    public
      constructor Create(X, Y: double; ALineWidth: integer; ALineColor: TColor;
        ALineStyle: TPenStyle; AFillColor: TColor; AFillStyle: TBrushStyle);
      procedure Draw(ACanvas: TCanvas); override;
      procedure MouseMove(X, Y: integer); override;
  end;

  { TPolygon }

  TPolygon = class(TFigure)
    private
      VertexCount: integer;
      StartPoint: TDoublePoint;
    public
      constructor Create(X, Y: double; ALineWidth: integer; ALineColor: TColor;
        ALineStyle: TPenStyle; AFillColor: TColor; AFillStyle: TBrushStyle;
        AVertexCount: integer);
     procedure Draw(ACanvas: TCanvas); override;
     procedure MouseMove(X, Y: integer); override;
  end;

implementation

{ TPolygon }

constructor TPolygon.Create(X, Y: double; ALineWidth: integer;
  ALineColor: TColor; ALineStyle: TPenStyle; AFillColor: TColor;
  AFillStyle: TBrushStyle; AVertexCount: integer);
var
  ScreenPoint: TPoint;
begin
  SetLength(Vertexes, AVertexCount);
  StartPoint := DoublePoint(X, Y);
  ScreenPoint := WorldToCanvas(X, Y);
  VertexCount := AVertexCount;
  MouseMove(ScreenPoint.x, ScreenPoint.y);
  LineColor := ALineColor;
  LineWidth := ALineWidth;
  LineStyle := ALineStyle;
  FillColor := AFillColor;
  FillStyle := AFillStyle;
end;

procedure TPolygon.Draw(ACanvas: TCanvas);
var
  i: integer;
  CanvasVertexes: TPointList;
begin
  SetCanvasStyles(ACanvas);
  SetLength(CanvasVertexes, Length(Vertexes));
  for i := Low(Vertexes) to High(Vertexes) do
    CanvasVertexes[i] := WorldToCanvas(Vertexes[i]);
  ACanvas.Polygon(CanvasVertexes);
end;

procedure TPolygon.MouseMove(X, Y: integer);
var
  i: integer;
  CurrentRotation, Radius: double;
  WorldPos: TDoublePoint;
begin
  WorldPos := CanvasToWorld(X, Y);
  CurrentRotation := arctan2(WorldPos.y - StartPoint.y,
    WorldPos.x - StartPoint.x);
  Radius := Dist(StartPoint, WorldPos);
  for i := Low(Vertexes) to High(Vertexes) do
  begin
    Vertexes[i].x := StartPoint.x + Radius*cos(CurrentRotation
      + (i * 2*pi / VertexCount));
    Vertexes[i].y := StartPoint.y + Radius*sin(CurrentRotation
      + (i * 2*pi / VertexCount));
  end;
end;

{ TRoundRect }

constructor TRoundRect.Create(X, Y: double; ALineWidth: integer;
  ALineColor: TColor; ALineStyle: TPenStyle; AFillColor: TColor;
  AFillStyle: TBrushStyle; ARoundX, ARoundY: integer);
begin
  SetLength(Vertexes, 2);
  Vertexes[0] := DoublePoint(X, Y);
  Vertexes[1] := Vertexes[0];
  LineColor := ALineColor;
  LineWidth := ALineWidth;
  LineStyle := ALineStyle;
  FillColor := AFillColor;
  FillStyle := AFillStyle;
  RoundX := ARoundX;
  RoundY := ARoundY;
end;

procedure TRoundRect.Draw(ACanvas: TCanvas);
var
  CanvasTopLeft, CanvasBottomRight: TPoint;
begin
  SetCanvasStyles(ACanvas);
  CanvasTopLeft := WorldToCanvas(Vertexes[0].x, Vertexes[0].y);
  CanvasBottomRight := WorldToCanvas(Vertexes[1].x, Vertexes[1].y);
  ACanvas.RoundRect(CanvasTopLeft.x, CanvasTopLeft.y,
    CanvasBottomRight.x, CanvasBottomRight.y, RoundX*2, RoundY*2);
end;

procedure TRoundRect.MouseMove(X, Y: integer);
begin
  Vertexes[High(Vertexes)] := CanvasToWorld(X, Y);
end;

{ TFigure }

function TFigure.FTopLeft: TDoublePoint;
var
  i: TDoublePoint;
begin
  Result := Vertexes[0];
  for i in Vertexes do
  begin
    Result.x := min(Result.x, i.x);
    Result.y := min(Result.y, i.y);
  end;
end;

function TFigure.FBottomRight: TDoublePoint;
var
  i: TDoublePoint;
begin
  Result := Vertexes[0];
  for i in Vertexes do
  begin
    Result.x := max(Result.x, i.x);
    Result.y := max(Result.y, i.y);
  end;
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

{ TMagnifier }

constructor TMagnifier.Create(X, Y: double);
begin
  SetLength(Vertexes, 2);
  Vertexes[0] := DoublePoint(X, Y);
  Vertexes[1] := Vertexes[0];
end;

procedure TMagnifier.MouseMove(X, Y: integer);
begin
  Vertexes[1] := CanvasToWorld(X, Y)
end;

procedure TMagnifier.Draw(ACanvas: TCanvas);
const
  eps = 16;
var
  TopLeft, BottomRight: TPoint;
begin
  TopLeft := WorldToCanvas(Vertexes[0]);
  BottomRight := WorldToCanvas(Vertexes[1]);

  if Dist(TopLeft, BottomRight) >= eps then
  begin
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
end;

{ TPolyLine }

constructor TPolyLine.Create(X, Y: double; ALineWidth: integer;
  ALineColor: TColor; ALineStyle: TPenStyle);
begin
  SetLength(Vertexes, 2);
  Vertexes[0] := DoublePoint(X, Y);
  Vertexes[1] := Vertexes[0];
  LineColor := ALineColor;
  LineWidth := ALineWidth;
  LineStyle := ALineStyle;
end;

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
  SetLength(Vertexes, Length(Vertexes) + 1);
  Vertexes[High(Vertexes)] := CanvasToWorld(X, Y);
end;

{ TLine }

procedure TLine.MouseMove(X, Y: integer);
begin
  Vertexes[High(Vertexes)] := CanvasToWorld(X, Y)
end;

{ TRectangle }

constructor TRectangle.Create(X, Y: double; ALineWidth: integer;
  ALineColor: TColor; ALineStyle: TPenStyle; AFillColor: TColor;
  AFillStyle: TBrushStyle);
begin
  SetLength(Vertexes, 2);
  Vertexes[0] := DoublePoint(X, Y);
  Vertexes[1] := Vertexes[0];
  LineColor := ALineColor;
  LineWidth := ALineWidth;
  LineStyle := ALineStyle;
  FillColor := AFillColor;
  FillStyle := AFillStyle;
end;

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
  Vertexes[High(Vertexes)] := CanvasToWorld(X, Y)
end;

{ TEllipse }

constructor TEllipse.Create(X, Y: double; ALineWidth: integer;
  ALineColor: TColor; ALineStyle: TPenStyle; AFillColor: TColor;
  AFillStyle: TBrushStyle);
begin
  SetLength(Vertexes, 2);
  Vertexes[0] := DoublePoint(X, Y);
  Vertexes[1] := Vertexes[0];
  LineColor := ALineColor;
  LineWidth := ALineWidth;
  LineStyle := ALineStyle;
  FillColor := AFillColor;
  FillStyle := AFillStyle;
end;

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
  Vertexes[High(Vertexes)] := CanvasToWorld(X, Y);
end;

end.

