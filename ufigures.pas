unit UFigures;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, Graphics;

type

  TPointList = array of TPoint;

 { TFigure }

  TFigure = class
  protected
    Vertexes: TPointList;
    LineColor, FillColor: TColor;
    LineWidth: integer;
    LineStyle: TPenStyle;
    FillStyle: TBrushStyle;
    procedure SetCanvasStyles(ACanvas: TCanvas);
  public
    constructor Create(X, Y: integer; ALineColor: TColor; ALineWidth: integer;
      AFillColor: TColor; ALineStyle: TPenStyle; AFillStyle: TBrushStyle);
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    procedure MouseMove(X, Y: integer); virtual; abstract;
  end;

  TFigureClass = class of TFigure;

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

implementation

{ TFigure }

constructor TFigure.Create(X, Y: integer;
  ALineColor: TColor; ALineWidth: integer; AFillColor: TColor;
  ALineStyle: TPenStyle; AFillStyle: TBrushStyle);
begin
  SetLength(Vertexes, 2);
  Vertexes[0] := Point(X, Y);
  Vertexes[1] := Vertexes[0];
  LineColor := ALineColor;
  LineWidth := ALineWidth;
  LineStyle := ALineStyle;
  FillColor := AFillColor;
  FillStyle := AFillStyle;
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
begin
  SetCanvasStyles(ACanvas);
  ACanvas.Polyline(Vertexes);
end;

procedure TPolyLine.MouseMove(X, Y: integer);
begin
  SetLength(Vertexes, Length(Vertexes) + 1);
  Vertexes[High(Vertexes)] := Point(X, Y);
end;

{ TLine }

procedure TLine.MouseMove(X, Y: integer);
begin
  Vertexes[High(Vertexes)] := Point(X, Y);
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
    SetCanvasStyles(ACanvas);
    ACanvas.Rectangle(Vertexes[0].x, Vertexes[0].y, Vertexes[1].x, Vertexes[1].y);
end;

procedure TRectangle.MouseMove(X, Y: integer);
begin
  Vertexes[High(Vertexes)] := Point(X, Y);
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  SetCanvasStyles(ACanvas);
  ACanvas.Ellipse(Vertexes[0].x, Vertexes[0].y, Vertexes[1].x, Vertexes[1].y);
end;

procedure TEllipse.MouseMove(X, Y: integer);
begin
  Vertexes[High(Vertexes)] := Point(X, Y);
end;

end.

