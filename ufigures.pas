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
  public
    constructor Create(X, Y: integer; ALineColor: TColor;
      ALineWidth: integer; AFillColor: TColor);
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

constructor TFigure.Create(X, Y: integer; ALineColor: TColor;
  ALineWidth: integer; AFillColor: TColor);
begin
  SetLength(Vertexes, 2);
  Vertexes[0] := Point(X, Y);
  Vertexes[1] := Vertexes[0];
  LineColor := ALineColor;
  LineWidth := ALineWidth;
  FillColor := AFillColor;
end;

{ TPolyLine }

procedure TPolyLine.Draw(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Color := LineColor;
    Pen.Width := LineWidth;
    if Length(Vertexes) < 3 then
      Line(Vertexes[0], Vertexes[0]);
    Polyline(Vertexes);
  end;
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
  with ACanvas do
  begin
    Pen.Color := LineColor;
    Pen.Width := LineWidth;
    Brush.Color := FillColor;
    Rectangle(Vertexes[0].x, Vertexes[0].y, Vertexes[1].x, Vertexes[1].y);
  end;
end;

procedure TRectangle.MouseMove(X, Y: integer);
begin
  Vertexes[High(Vertexes)] := Point(X, Y);
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Color := LineColor;
    Pen.Width := LineWidth;
    Brush.Color := FillColor;
    Ellipse(Vertexes[0].x, Vertexes[0].y, Vertexes[1].x, Vertexes[1].y);
  end;
end;

procedure TEllipse.MouseMove(X, Y: integer);
begin
  Vertexes[High(Vertexes)] := Point(X, Y);
end;

end.

