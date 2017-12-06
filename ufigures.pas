unit UFigures;

{$mode objfpc}{$H+}
{$M+}

interface

uses
  Classes, SysUtils, Graphics, UTransform, Windows, Math, Controls;

type

  TPointList = array of TPoint;
  TDPointList = array of TDoublePoint;

  { TFigure }

  TFigure = class(TPersistent)
  protected
    FVertexes: TDPointList;
    function FTopLeft: TDoublePoint;
    function FBottomRight: TDoublePoint;
  public
    Selected: boolean;
    property Vertexes: TDPointList read FVertexes;
    property TopLeftBorder: TDoublePoint read FTopLeft;
    property BottomRightBorder: TDoublePoint read FBottomRight;
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    procedure MouseMove(X, Y: integer); virtual; abstract;
    function InRect(RectTL, RectBR: TDoublePoint): boolean;
    function GetVertexIndexAtPos(APoint: TDoublePoint): integer;
    function UnderPoint(APoint: TDoublePoint): boolean; virtual; abstract;
    procedure MoveFigure(dx, dy: double);
    procedure MoveVertex(VertexIndex: integer; ANewPos: TDoublePoint);
  end;

  TFigureList = array of TFigure;
  TFigureClass = class of TFigure;

  { TDrawableFigure }

  TDrawableFigure = class(TFigure)
  private
    FLineWidth: integer;
    FLineColor: TColor;
    FLineStyle: TPenStyle;
    procedure SetLineWidth(AWidth: integer);
    procedure SetCanvasStyles(ACanvas: TCanvas);
    function GetCanvasLineWidth: integer;
  public
    constructor Create(X, Y: double; ALineWidth: integer; ALineColor: TColor;
      ALineStyle: TPenStyle);
    procedure MouseMove(X, Y: integer); override;
    function UnderPoint(APoint: TDoublePoint): boolean; override;
    property LineWidth: integer read FLineWidth write SetLineWidth;
    property LineColor: TColor read FLineColor write FLineColor;
    property LineStyle: TPenStyle read FLineStyle write FLineStyle;
  end;

  { TFillableFigure }

  TFillableFigure = class(TDrawableFigure)
  private
    FFillColor: TColor;
    FFillStyle: TBrushStyle;
    procedure SetCanvasStyles(ACanvas: TCanvas);
  public
    constructor Create(X, Y: double; ALineWidth: integer; ALineColor: TColor;
      ALineStyle: TPenStyle; AFillColor: TColor; AFillStyle: TBrushStyle);
    property FillColor: TColor read FFillColor write FFillColor;
    property FillStyle: TBrushStyle read FFillStyle write FFillStyle;
  end;

  { TRegionSelection }

  TRegionSelection = class(TFigure)
    constructor Create(X, Y: double);
    procedure MouseMove(X, Y: integer); override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TPolyLine }

  TPolyLine = class(TDrawableFigure)
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

  TRectangle = class(TFillableFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
    function UnderPoint(APoint: TDoublePoint): boolean; override;
  end;

  { TRoundRect }

  TRoundRect = class(TFillableFigure)
  private
    FRoundX, FRoundY: integer;
  public
    constructor Create(X, Y: double; ALineWidth: integer; ALineColor: TColor;
      ALineStyle: TPenStyle; AFillColor: TColor; AFillStyle: TBrushStyle;
      ARoundX, ARoundY: integer);
    procedure Draw(ACanvas: TCanvas); override;
    function UnderPoint(APoint: TDoublePoint): boolean; override;
    property RoundX: integer read FRoundX write FRoundX;
    property RoundY: integer read FRoundY write FRoundY;
  end;

  { TEllipse }

  TEllipse = class(TFillableFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
    function UnderPoint(APoint: TDoublePoint): boolean; override;
  end;

  { TPolygon }

  TPolygon = class(TFillableFigure)
  private
    FVertexCount: integer;
    StartPoint: TDoublePoint;
  public
    constructor Create(X, Y: double; ALineWidth: integer; ALineColor: TColor;
      ALineStyle: TPenStyle; AFillColor: TColor; AFillStyle: TBrushStyle;
      AVertexCount: integer);
    procedure Draw(ACanvas: TCanvas); override;
    procedure MouseMove(X, Y: integer); override;
    function UnderPoint(APoint: TDoublePoint): boolean; override;
    property VertexCount: integer read FVertexCount write FVertexCount;
  end;

function GetSelectionTopLeft: TDoublePoint;
function GetSelectionBottomRight: TDoublePoint;
procedure DrawSelection(ACanvas: TCanvas);
procedure SelectAll;
procedure RemoveSelection;
procedure DeleteSelected;
procedure MoveSelectedOnTop;
procedure MoveSelectedOnBottom;

var
  CanvasItems: TFigureList;

implementation

function GetSelectionTopLeft: TDoublePoint;
var
  HasSelection: boolean;
  f: TFigure;
  d: double;
begin
  HasSelection := false;
  Result := DoublePoint(0,0);
  for f in CanvasItems do
  begin
    if f.Selected then
    begin
      d := (f as TDrawableFigure).LineWidth / 2;
      if not HasSelection then
      begin
        Result.x := f.FTopLeft.x - d;
        Result.y := f.FTopLeft.y - d;
      end
      else
      begin
        Result.x := min(Result.x, f.FTopLeft.x - d);
        Result.y := min(Result.y, f.FTopLeft.y - d);
      end;
      HasSelection := True;
    end;
  end;
end;

function GetSelectionBottomRight: TDoublePoint;
var
  HasSelection: boolean;
  f: TFigure;
  d: double;
begin
  HasSelection := false;
  Result := DoublePoint(0,0);
  for f in CanvasItems do
  begin
    if f.Selected then
    begin
      d := (f as TDrawableFigure).LineWidth / 2;
      if not HasSelection then
      begin
        Result.x := f.FBottomRight.x + d;
        Result.y := f.FBottomRight.y + d;
      end
      else
      begin
        Result.x := max(Result.x, f.FBottomRight.x + d);
        Result.y := max(Result.y, f.FBottomRight.y + d);
      end;
      HasSelection := True;
    end;
  end;
end;

procedure DrawSelection(ACanvas: TCanvas);
const
  PADDING = 5;
var
  SelectionTL, SelectionBR: TDoublePoint;
  CSelectionTL, CSelectionBR, CFigureTL, CFigureBR: TPoint;
  CurrentVertex: TPoint;
  f: TFigure;
  d: double;
  i: integer;
begin
  for f in CanvasItems do
  begin
    if f.Selected then
    begin
      CFigureTL := WorldToCanvas(f.TopLeftBorder);
      CFigureBR := WorldToCanvas(f.BottomRightBorder);
      with ACanvas do
      begin
        Pen.Width := 2;
        Pen.Style := psDash;
        Pen.Mode := pmNot;
        Brush.Style := bsClear;
        Rectangle(CFigureTL.x, CFigureTL.y, CFigureBR.x, CFigureBR.y);
        Pen.Mode := pmCopy;
      end;
      for i := Low(f.FVertexes) to High(f.FVertexes) do
      begin
        CurrentVertex := WorldToCanvas(f.FVertexes[i]);
        with ACanvas do
        begin
          Pen.Width := 1;
          Pen.Style := psSolid;
          Pen.Mode := pmCopy;
          Brush.Style := bsSolid;
          Rectangle(CurrentVertex.x - PADDING, CurrentVertex.y - PADDING,
            CurrentVertex.x + PADDING, CurrentVertex.y + PADDING);
        end;
      end;
    end;
  end;

  SelectionTL := GetSelectionTopLeft;
  SelectionBR := GetSelectionBottomRight;

  if SelectionTL <> SelectionBR then
  begin
    CSelectionTL := WorldToCanvas(SelectionTL);
    CSelectionBR := WorldToCanvas(SelectionBR);
    with ACanvas do
    begin
      Pen.Width := 2;
      Pen.Style := psDash;
      Pen.Mode := pmNot;
      Brush.Style := bsClear;
      Rectangle(CSelectionTL.x - PADDING, CSelectionTL.y - PADDING,
        CSelectionBR.x + PADDING, CSelectionBR.y + PADDING);
      Pen.Mode := pmCopy;
    end;
  end;
end;

procedure SelectAll;
var
  CurrentFigure: TFigure;
begin
  for CurrentFigure in CanvasItems do
    if CurrentFigure <> nil then
      CurrentFigure.Selected := True;
end;

procedure RemoveSelection;
var
  CurrentFigure: TFigure;
begin
  for CurrentFigure in CanvasItems do
    if CurrentFigure <> nil then
      CurrentFigure.Selected := False;
end;

procedure DeleteSelected;
var
  i, j, k: integer;
begin
  j := 0;
  for i := Low(CanvasItems) to High(CanvasItems) do
    if (CanvasItems[i] <> nil) and (CanvasItems[i].Selected) then
    begin
      FreeAndNil(CanvasItems[i]);
      Inc(j);
    end;
  for k := 1 to j do
    for i := Low(CanvasItems) to High(CanvasItems) do
    begin
      if (CanvasItems[i] = nil) and (i + 1 < Length(CanvasItems)) then
      begin
        CanvasItems[i] := CanvasItems[i + 1];
        CanvasItems[i + 1] := nil;
      end;
    end;
  SetLength(CanvasItems, Length(CanvasItems) - j);
end;

procedure MoveSelectedOnTop;
var
  i: integer;
  t: TFigure;
begin
  for i := High(CanvasItems) downto Low(CanvasItems) do
  begin
    if CanvasItems[i].Selected and (i + 1 < Length(CanvasItems)) then
    begin
      t := CanvasItems[i + 1];
      CanvasItems[i + 1] := CanvasItems[i];
      CanvasItems[i] := t;
    end;
  end;
end;

procedure MoveSelectedOnBottom;
var
  i: integer;
  t: TFigure;
begin
  for i := Low(CanvasItems) to High(CanvasItems) do
  begin
    if CanvasItems[i].Selected and (i - 1 >= 0) then
    begin
      t := CanvasItems[i - 1];
      CanvasItems[i - 1] := CanvasItems[i];
      CanvasItems[i] := t;
    end;
  end;
end;

{ TFillableFigure }

procedure TFillableFigure.SetCanvasStyles(ACanvas: TCanvas);
begin
  inherited SetCanvasStyles(ACanvas);
  with ACanvas do
  begin
    Brush.Color := FFillColor;
    Brush.Style := FFillStyle;
  end;
end;

constructor TFillableFigure.Create(X, Y: double; ALineWidth: integer;
  ALineColor: TColor; ALineStyle: TPenStyle; AFillColor: TColor;
  AFillStyle: TBrushStyle);
begin
  SetLength(FVertexes, 2);
  FVertexes[0] := DoublePoint(X, Y);
  FVertexes[1] := FVertexes[0];
  LineColor := ALineColor;
  LineWidth := ALineWidth;
  LineStyle := ALineStyle;
  FillColor := AFillColor;
  FillStyle := AFillStyle;
  Selected := False;
end;

{ TDrawableFigure }

procedure TDrawableFigure.SetLineWidth(AWidth: integer);
begin
  FLineWidth := AWidth;
end;

procedure TDrawableFigure.SetCanvasStyles(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Color := FLineColor;
    Pen.Width := GetCanvasLineWidth;
    Pen.Style := FLineStyle;
  end;
end;

function TDrawableFigure.GetCanvasLineWidth: integer;
begin
  Result := max(1, Round(FLineWidth * Scale));
end;

constructor TDrawableFigure.Create(X, Y: double; ALineWidth: integer;
  ALineColor: TColor; ALineStyle: TPenStyle);
begin
  SetLength(FVertexes, 2);
  FVertexes[0] := DoublePoint(X, Y);
  FVertexes[1] := FVertexes[0];
  LineColor := ALineColor;
  LineWidth := ALineWidth;
  LineStyle := ALineStyle;
  Selected := False;
end;

procedure TDrawableFigure.MouseMove(X, Y: integer);
begin
  FVertexes[High(FVertexes)] := CanvasToWorld(X, Y);
end;

function TDrawableFigure.UnderPoint(APoint: TDoublePoint): boolean;
const
  eps = 0.01;
  PADDING = 5;
var
  i: integer;
  p0, p1: TDoublePoint;
  RegionTL, RegionBR: TDoublePoint;
  a, b, c, x0, y0: double;
  d: double;
begin
  x0 := APoint.x;
  y0 := APoint.y;
  for i := Low(FVertexes) to High(FVertexes) - 1 do
  begin
    //Ax+By+C=0
    p0 := FVertexes[i + 1];
    p1 := FVertexes[i];
    a := p0.y - p1.y;
    b := p1.x - p0.x;
    c := p0.x * p1.y - p1.x * p0.y;
    d := LineWidth div 2 + PADDING;
    if (abs(a) < eps) and (abs(b) < eps) then
    begin
      //just a point
      if (abs(p0.x - x0) <= d) and (abs(p0.y - y0) <= d) then
        exit(True);
    end
    else
    if abs(a) < eps then
    begin
      // y = const
      RegionTL := DoublePoint(min(p0.x, p1.x) - d, p0.y - d);
      RegionBR := DoublePoint(max(p0.x, p1.x) + d, p0.y + d);
      if (x0 >= RegionTL.x) and (x0 <= RegionBR.x) and (y0 >= RegionTL.y) and
        (y0 <= RegionBR.y) then
        exit(True);
    end
    else
    if abs(b) < eps then
    begin
      // x = const
      RegionTL := DoublePoint(p0.x - d, min(p0.y, p1.y) - d);
      RegionBR := DoublePoint(p0.x + d, max(p0.y, p1.y) + d);
      if (x0 >= RegionTL.x) and (x0 <= RegionBR.x) and (y0 >= RegionTL.y) and
        (y0 <= RegionBR.y) then
        exit(True);
    end
    else
    begin
      // regular line
      if (abs(a * x0 + b * y0 + c) / Dist(p0, p1) <= d) and
        (x0 >= min(p0.x, p1.x) - d) and (x0 <= max(p0.x, p1.x) + d) and
        (y0 >= min(p0.y, p1.y) - d) and (y0 <= max(p0.y, p1.y) + d) then
        exit(True);
    end;
  end;
  Result := False;
end;

{ TPolygon }

constructor TPolygon.Create(X, Y: double; ALineWidth: integer;
  ALineColor: TColor; ALineStyle: TPenStyle; AFillColor: TColor;
  AFillStyle: TBrushStyle; AVertexCount: integer);
var
  ScreenPoint: TPoint;
begin
  inherited Create(X, Y, ALineWidth, ALineColor, ALineStyle,
    AFillColor, AFillStyle);
  SetLength(FVertexes, AVertexCount);
  StartPoint := DoublePoint(X, Y);
  ScreenPoint := WorldToCanvas(X, Y);
  VertexCount := AVertexCount;
  MouseMove(ScreenPoint.x, ScreenPoint.y);
end;

procedure TPolygon.Draw(ACanvas: TCanvas);
var
  i: integer;
  CanvasVertexes: TPointList;
begin
  SetCanvasStyles(ACanvas);
  SetLength(CanvasVertexes, Length(FVertexes));
  for i := Low(FVertexes) to High(FVertexes) do
    CanvasVertexes[i] := WorldToCanvas(FVertexes[i]);
  ACanvas.Polygon(CanvasVertexes);
end;

procedure TPolygon.MouseMove(X, Y: integer);
var
  i: integer;
  CurrentRotation, Radius: double;
  WorldPos: TDoublePoint;
begin
  WorldPos := CanvasToWorld(X, Y);
  CurrentRotation := arctan2(WorldPos.y - StartPoint.y, WorldPos.x - StartPoint.x);
  Radius := Dist(StartPoint, WorldPos);
  for i := Low(FVertexes) to High(FVertexes) do
  begin
    FVertexes[i].x := StartPoint.x + Radius * cos(CurrentRotation +
      (i * 2 * pi / VertexCount));
    FVertexes[i].y := StartPoint.y + Radius * sin(CurrentRotation +
      (i * 2 * pi / VertexCount));
  end;
end;

function TPolygon.UnderPoint(APoint: TDoublePoint): boolean;
var
  RegionFigure: HRGN;
  CanvasVertexes: TPointList;
  CanvasPoint: TPoint;
  i: integer;
begin
  SetLength(CanvasVertexes, VertexCount);
  for i := Low(FVertexes) to High(FVertexes) do
    CanvasVertexes[i] := WorldToCanvas(FVertexes[i]);
  RegionFigure := CreatePolygonRgn(CanvasVertexes[0], VertexCount, WINDING);
  CanvasPoint := WorldToCanvas(APoint);
  Result := PtInRegion(RegionFigure, CanvasPoint.x, CanvasPoint.y);
  DeleteObject(RegionFigure);
end;

{ TRoundRect }

constructor TRoundRect.Create(X, Y: double; ALineWidth: integer;
  ALineColor: TColor; ALineStyle: TPenStyle; AFillColor: TColor;
  AFillStyle: TBrushStyle; ARoundX, ARoundY: integer);
begin
  inherited Create(X, Y, ALineWidth, ALineColor, ALineStyle,
    AFillColor, AFillStyle);
  FRoundX := ARoundX;
  FRoundY := ARoundY;
end;

procedure TRoundRect.Draw(ACanvas: TCanvas);
var
  CanvasTopLeft, CanvasBottomRight: TPoint;
begin
  SetCanvasStyles(ACanvas);
  CanvasTopLeft := WorldToCanvas(FVertexes[0].x, FVertexes[0].y);
  CanvasBottomRight := WorldToCanvas(FVertexes[1].x, FVertexes[1].y);
  ACanvas.RoundRect(CanvasTopLeft.x, CanvasTopLeft.y,
    CanvasBottomRight.x, CanvasBottomRight.y, FRoundX * 2, FRoundY * 2);
end;

function TRoundRect.UnderPoint(APoint: TDoublePoint): boolean;
const
  PADDING = 5;
var
  RegionFigure, RegionInner: HRGN;
  CanvasVertexes: TPointList;
  CanvasPoint: TPoint;
  d: integer;
begin
  SetLength(CanvasVertexes, 2);
  CanvasVertexes[0] := WorldToCanvas(FVertexes[0]);
  CanvasVertexes[1] := WorldToCanvas(FVertexes[1]);
  d := LineWidth div 2 + PADDING;

  RegionFigure := CreateRoundRectRgn(
    min(CanvasVertexes[0].x, CanvasVertexes[1].x) - d,
    min(CanvasVertexes[0].y, CanvasVertexes[1].y) - d,
    max(CanvasVertexes[0].x, CanvasVertexes[1].x) + d,
    max(CanvasVertexes[0].y, CanvasVertexes[1].y) + d, FRoundX * 2, FRoundY * 2);
  CanvasPoint := WorldToCanvas(APoint);
  if FillStyle = bsClear then
  begin
    RegionInner := CreateRoundRectRgn(
      max(CanvasVertexes[0].x, CanvasVertexes[1].x) - d,
      max(CanvasVertexes[0].y, CanvasVertexes[1].y) - d,
      min(CanvasVertexes[0].x, CanvasVertexes[1].x) + d,
      min(CanvasVertexes[0].y, CanvasVertexes[1].y) + d, FRoundX * 2, FRoundY * 2);
    CombineRgn(RegionFigure, RegionFigure, RegionInner, RGN_DIFF);
    DeleteObject(RegionInner);
  end;
  Result := PtInRegion(RegionFigure, CanvasPoint.x, CanvasPoint.y);
  DeleteObject(RegionFigure);
end;

{ TFigure }

function TFigure.FTopLeft: TDoublePoint;
var
  i: TDoublePoint;
begin
  Result := FVertexes[0];
  for i in FVertexes do
  begin
    Result.x := min(Result.x, i.x);
    Result.y := min(Result.y, i.y);
  end;
end;

function TFigure.FBottomRight: TDoublePoint;
var
  i: TDoublePoint;
begin
  Result := FVertexes[0];
  for i in FVertexes do
  begin
    Result.x := max(Result.x, i.x);
    Result.y := max(Result.y, i.y);
  end;
end;

function TFigure.InRect(RectTL, RectBR: TDoublePoint): boolean;
var
  FigureTL, FigureBR: TDoublePoint;
begin
  FigureTL := FTopLeft;
  FigureBR := FBottomRight;
  Result := (RectTL.x <= FigureTL.x) and (RectTL.y <= FigureTL.y) and
    (RectBR.x >= FigureBR.x) and (RectBR.y >= FigureBR.y);
end;

function TFigure.GetVertexIndexAtPos(APoint: TDoublePoint): integer;
const
  PADDING = 5;
var
  i: integer;
begin
  Result := -1;
  for i := High(FVertexes) downto Low(FVertexes) do
  begin
    if (abs(APoint.x - FVertexes[i].x) <= PADDING) and
      (abs(APoint.y - FVertexes[i].y) <= PADDING) then
      Exit(i);
  end;
end;

procedure TFigure.MoveFigure(dx, dy: double);
var
  i: integer;
begin
  for i := Low(FVertexes) to High(FVertexes) do
  begin
    FVertexes[i].x := FVertexes[i].x + dx;
    FVertexes[i].y := FVertexes[i].y + dy;
  end;
end;

procedure TFigure.MoveVertex(VertexIndex: integer; ANewPos: TDoublePoint);
begin
  FVertexes[VertexIndex] := ANewPos;
end;

{ TRegionSelection }

constructor TRegionSelection.Create(X, Y: double);
begin
  SetLength(FVertexes, 2);
  FVertexes[0] := DoublePoint(X, Y);
  FVertexes[1] := FVertexes[0];
end;

procedure TRegionSelection.MouseMove(X, Y: integer);
begin
  FVertexes[1] := CanvasToWorld(X, Y);
end;

procedure TRegionSelection.Draw(ACanvas: TCanvas);
const
  eps = 16;
var
  TopLeft, BottomRight: TPoint;
begin
  TopLeft := WorldToCanvas(FVertexes[0]);
  BottomRight := WorldToCanvas(FVertexes[1]);

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

procedure TPolyLine.Draw(ACanvas: TCanvas);
var
  CanvasVertexes: TPointList;
  i: integer;
begin
  SetCanvasStyles(ACanvas);
  SetLength(CanvasVertexes, Length(FVertexes));
  for i := Low(FVertexes) to High(FVertexes) do
    CanvasVertexes[i] := WorldToCanvas(FVertexes[i]);
  ACanvas.Polyline(CanvasVertexes);
end;

procedure TPolyLine.MouseMove(X, Y: integer);
begin
  SetLength(FVertexes, Length(FVertexes) + 1);
  FVertexes[High(FVertexes)] := CanvasToWorld(X, Y);
end;

{ TLine }

procedure TLine.MouseMove(X, Y: integer);
begin
  FVertexes[High(FVertexes)] := CanvasToWorld(X, Y);
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
var
  CanvasTopLeft, CanvasBottomRight: TPoint;
begin
  SetCanvasStyles(ACanvas);
  CanvasTopLeft := WorldToCanvas(FVertexes[0].x, FVertexes[0].y);
  CanvasBottomRight := WorldToCanvas(FVertexes[1].x, FVertexes[1].y);
  ACanvas.Rectangle(CanvasTopLeft.x, CanvasTopLeft.y,
    CanvasBottomRight.x, CanvasBottomRight.y);
end;

function TRectangle.UnderPoint(APoint: TDoublePoint): boolean;
const
  PADDING = 5;
var
  RegionFigure, RegionInner: HRGN;
  CanvasVertexes: TPointList;
  CanvasPoint: TPoint;
  d: integer;
begin
  SetLength(CanvasVertexes, 2);
  CanvasVertexes[0] := WorldToCanvas(FVertexes[0]);
  CanvasVertexes[1] := WorldToCanvas(FVertexes[1]);
  d := LineWidth div 2 + PADDING;

  CanvasPoint := WorldToCanvas(APoint);
  RegionFigure := CreateRectRgn(min(CanvasVertexes[0].x, CanvasVertexes[1].x) -
    d, min(CanvasVertexes[0].y, CanvasVertexes[1].y) - d,
    max(CanvasVertexes[0].x, CanvasVertexes[1].x) + d,
    max(CanvasVertexes[0].y, CanvasVertexes[1].y) + d);
  if FillStyle = bsClear then
  begin
    RegionInner := CreateRectRgn(max(CanvasVertexes[0].x, CanvasVertexes[1].x) -
      d, max(CanvasVertexes[0].y, CanvasVertexes[1].y) - d,
      min(CanvasVertexes[0].x, CanvasVertexes[1].x) + d,
      min(CanvasVertexes[0].y, CanvasVertexes[1].y) + d);
    CombineRgn(RegionFigure, RegionFigure, RegionInner, RGN_DIFF);
    DeleteObject(RegionInner);
  end;

  Result := PtInRegion(RegionFigure, CanvasPoint.x, CanvasPoint.y);
  DeleteObject(RegionFigure);
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
var
  CanvasTopLeft, CanvasBottomRight: TPoint;
begin
  SetCanvasStyles(ACanvas);
  CanvasTopLeft := WorldToCanvas(FVertexes[0].x, FVertexes[0].y);
  CanvasBottomRight := WorldToCanvas(FVertexes[1].x, FVertexes[1].y);
  ACanvas.Ellipse(CanvasTopLeft.x, CanvasTopLeft.y,
    CanvasBottomRight.x, CanvasBottomRight.y);
end;

function TEllipse.UnderPoint(APoint: TDoublePoint): boolean;
const
  PADDING = 5;
var
  RegionFigure, RegionInner: HRGN;
  CanvasVertexes: TPointList;
  CanvasPoint: TPoint;
  d: integer;
begin
  SetLength(CanvasVertexes, 2);
  CanvasVertexes[0] := WorldToCanvas(FVertexes[0]);
  CanvasVertexes[1] := WorldToCanvas(FVertexes[1]);
  d := LineWidth div 2 + PADDING;
  CanvasPoint := WorldToCanvas(APoint);

  RegionFigure := CreateEllipticRgn(
    min(CanvasVertexes[0].x - d, CanvasVertexes[1].x - d),
    min(CanvasVertexes[0].y - d, CanvasVertexes[1].y - d),
    max(CanvasVertexes[0].x + d, CanvasVertexes[1].x + d),
    max(CanvasVertexes[0].y + d, CanvasVertexes[1].y + d));

  if FillStyle = bsClear then
  begin
    RegionInner := CreateEllipticRgn(
      max(CanvasVertexes[0].x, CanvasVertexes[1].x) - d,
      max(CanvasVertexes[0].y, CanvasVertexes[1].y) - d,
      min(CanvasVertexes[0].x, CanvasVertexes[1].x) + d,
      min(CanvasVertexes[0].y, CanvasVertexes[1].y) + d);
    CombineRgn(RegionFigure, RegionFigure, RegionInner, RGN_DIFF);
    DeleteObject(RegionInner);
  end;

  Result := PtInRegion(RegionFigure, CanvasPoint.x, CanvasPoint.y);
  DeleteObject(RegionFigure);
end;

end.
