unit UFigures;

{$mode objfpc}{$H+}
{$M+}

interface

uses
  Classes, SysUtils, Graphics, UTransform, Windows, Math, Controls, UToolParams,
  TypInfo;

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
    function GetVertexIndexAtPos(const APoint: TPoint): integer;
    function GetVertexIndexAtPos(const APoint: TDoublePoint): integer;
    function UnderPoint(const APoint: TDoublePoint): boolean; virtual; abstract;
    procedure MoveFigure(dx, dy: double);
    procedure MoveVertex(VertexIndex: integer; ANewPos: TDoublePoint); virtual;
    function GetParams: TToolParamList; virtual; abstract;
  end;

  TFigureList = array of TFigure;
  TFigureClass = class of TFigure;

  { TDrawableFigure }

  TDrawableFigure = class(TFigure)
  private
    FLineWidth: TIntegerParam;
    FLineColor: TColorParam;
    FLineStyle: TLineStyleParam;
    procedure SetCanvasStyles(ACanvas: TCanvas);
    function GetCanvasLineWidth: integer;
  public
    constructor Create(X, Y: double; ALineWidth: TIntegerParam;
      ALineColor: TColorParam; ALineStyle: TLineStyleParam);
    destructor Destroy; override;
    procedure MouseMove(X, Y: integer); override;
    function UnderPoint(const APoint: TDoublePoint): boolean; override;
    property LineWidth: TIntegerParam read FLineWidth write FLineWidth;
    property LineWidthCanvas: integer read GetCanvasLineWidth;
    property LineColor: TColorParam read FLineColor write FLineColor;
    property LineStyle: TLineStyleParam read FLineStyle write FLineStyle;
    function GetParams: TToolParamList; override;
  end;

  { TFillableFigure }

  TFillableFigure = class(TDrawableFigure)
  private
    FFillColor: TColorParam;
    FFillStyle: TFillStyleParam;
    procedure SetCanvasStyles(ACanvas: TCanvas);
  public
    constructor Create(X, Y: double; ALineWidth: TIntegerParam;
      ALineColor: TColorParam; ALineStyle: TLineStyleParam;
      AFillColor: TColorParam; AFillStyle: TFillStyleParam);
    destructor Destroy; override;
    property FillColor: TColorParam read FFillColor write FFillColor;
    property FillStyle: TFillStyleParam read FFillStyle write FFillStyle;
    function GetParams: TToolParamList; override;
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
    function UnderPoint(const APoint: TDoublePoint): boolean; override;
  end;

  { TRoundRect }

  TRoundRect = class(TFillableFigure)
  private
    FRoundX, FRoundY: TIntegerParam;
  public
    constructor Create(X, Y: double; ALineWidth: TIntegerParam;
      ALineColor: TColorParam; ALineStyle: TLineStyleParam;
      AFillColor: TColorParam; AFillStyle: TFillStyleParam;
      ARoundX, ARoundY: TIntegerParam);
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas); override;
    function UnderPoint(const APoint: TDoublePoint): boolean; override;
    property RoundX: TIntegerParam read FRoundX write FRoundX;
    property RoundY: TIntegerParam read FRoundY write FRoundY;
    function GetParams: TToolParamList; override;
  end;

  { TEllipse }

  TEllipse = class(TFillableFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
    function UnderPoint(const APoint: TDoublePoint): boolean; override;
  end;

  { TPolygon }

  TPolygon = class(TFillableFigure)
  private
    FVertexCount: TIntegerParam;
    StartPoint: TDoublePoint;
    EndPoint: TPoint;
  public
    constructor Create(X, Y: double; ALineWidth: TIntegerParam;
      ALineColor: TColorParam; ALineStyle: TLineStyleParam;
      AFillColor: TColorParam; AFillStyle: TFillStyleParam;
      AVertexCount: TIntegerParam);
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure MouseMove(X, Y: integer); override;
    function UnderPoint(const APoint: TDoublePoint): boolean; override;
    property VertexCount: TIntegerParam read FVertexCount write FVertexCount;
    function GetParams: TToolParamList; override;
  end;

function GetSelectionTopLeft: TDoublePoint;
function GetSelectionBottomRight: TDoublePoint;
procedure DrawSelection(ACanvas: TCanvas);
function GetSelectionVertexIndexAtPos(APoint: TPoint): integer;
procedure ResizeSelection(VertexIndex: integer; ANewPos: TDoublePoint);
procedure SelectAll;
procedure RemoveSelection;
procedure DeleteSelected;
procedure MoveSelectedOnTop;
procedure MoveSelectedOnBottom;
function GetSelectionParams: TToolParamList;

const
  PADDING = 5;
var
  CanvasItems: TFigureList;

implementation

function GetSelectionTopLeft: TDoublePoint;
var
  HasSelection: boolean;
  f: TFigure;
  d: double;
begin
  HasSelection := False;
  Result := DoublePoint(0, 0);
  for f in CanvasItems do
  begin
    if f.Selected then
    begin
      d := (f as TDrawableFigure).LineWidth.Value / 2;
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
  HasSelection := False;
  Result := DoublePoint(0, 0);
  for f in CanvasItems do
  begin
    if f.Selected then
    begin
      d := (f as TDrawableFigure).LineWidth.Value / 2;
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
var
  SelectionTL, SelectionBR: TDoublePoint;
  CSelectionTL, CSelectionBR, CFigureTL, CFigureBR: TPoint;
  CurrentVertex: TPoint;
  f: TFigure;
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
        Pen.Color := clBlack;
        Pen.Style := psDash;
        Pen.Mode := pmNot;
        Brush.Color := clWhite;
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
          Pen.Color := clBlack;
          Pen.Style := psSolid;
          Pen.Mode := pmCopy;
          Brush.Color := clWhite;
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
      Pen.Color := clBlack;
      Pen.Style := psDash;
      Pen.Mode := pmNot;
      Brush.Style := bsClear;
      Rectangle(CSelectionTL.x - PADDING, CSelectionTL.y - PADDING,
        CSelectionBR.x + PADDING, CSelectionBR.y + PADDING);
      Pen.Mode := pmCopy;
      Pen.Width := 1;
      Pen.Style := psSolid;
      Brush.Color := clBlack;
      Brush.Style := bsSolid;
      Rectangle(CSelectionBR.x, CSelectionBR.y,
        CSelectionBR.x + 2*PADDING, CSelectionBR.y + 2*PADDING); //0th vertex
      Rectangle(CSelectionTL.x, CSelectionTL.y,
        CSelectionTL.x - 2*PADDING, CSelectionTL.y - 2*PADDING); //1st vertex
      Rectangle(CSelectionTL.x, CSelectionBR.y,
        CSelectionTL.x - 2*PADDING, CSelectionBR.y + 2*PADDING); //2nd vertex
      Rectangle(CSelectionBR.x, CSelectionTL.y,
        CSelectionBR.x + 2*PADDING, CSelectionTL.y - 2*PADDING); //3rd vertex
    end;
  end;
end;

function GetSelectionVertexIndexAtPos(APoint: TPoint): integer;
var
  CSelectionTL, CSelectionBR: TPoint;
begin
  Result := -1;
  CSelectionTL := WorldToCanvas(GetSelectionTopLeft);
  CSelectionBR := WorldToCanvas(GetSelectionBottomRight);
  if ((APoint.x - CSelectionBR.x) >= 0) and
    ((APoint.x - CSelectionBR.x) <= 2*PADDING) and
    ((APoint.y - CSelectionBR.y) >= 0) and
    ((APoint.y - CSelectionBR.y) <= 2*PADDING) then
    Exit(0); // Bottom Right
  if ((APoint.x - CSelectionTL.x) <= 0) and
    ((APoint.x - CSelectionTL.x) >= -2*PADDING) and
    ((APoint.y - CSelectionTL.y) <= 0) and
    ((APoint.y - CSelectionTL.y) >= -2*PADDING) then
    Exit(1); // Top Left
  if ((APoint.x - CSelectionTL.x) <= 0) and
    ((APoint.x - CSelectionTL.x) >= -2*PADDING) and
    ((APoint.y - CSelectionBR.y) >= 0) and
    ((APoint.y - CSelectionBR.y) <= 2*PADDING) then
    Exit(2); // Bottom Left
  if ((APoint.x - CSelectionBR.x) >= 0) and
    ((APoint.x - CSelectionBR.x) <= 2*PADDING) and
    ((APoint.y - CSelectionTL.y) <= 0) and
    ((APoint.y - CSelectionTL.y) >= -2*PADDING) then
    Exit(3); // Top Right
end;

procedure ResizeSelection(VertexIndex: integer; ANewPos: TDoublePoint);
var
  f: TFigure;
  v: integer;
  sx, sy, dx, dy: double;
  Anchor, SelectionVertex: TDoublePoint;
  SelectionTL, SelectionBR: TDoublePoint;
begin
  SelectionTL := GetSelectionTopLeft;
  SelectionBR := GetSelectionBottomRight;
  case VertexIndex of
    0:
    begin
      Anchor := SelectionTL;
      SelectionVertex := SelectionBR;
      sx := (Anchor.x - ANewPos.x) / (Anchor.x - SelectionVertex.x);
      sy := (Anchor.y - ANewPos.y) / (Anchor.y - SelectionVertex.y);
    end;
    1:
    begin
      Anchor := SelectionBR;
      SelectionVertex := SelectionTL;
      sx := (ANewPos.x - Anchor.x) / (SelectionVertex.x - Anchor.x);
      sy := (ANewPos.y - Anchor.y) / (SelectionVertex.y - Anchor.y);
    end;
    2:
    begin
      Anchor := DoublePoint(SelectionBR.x, SelectionTL.y);
      SelectionVertex := DoublePoint(SelectionTL.x, SelectionBR.y);
      sx := (Anchor.x - ANewPos.x) / (Anchor.x - SelectionVertex.x);
      sy := (ANewPos.y - Anchor.y) / (SelectionVertex.y - Anchor.y);
    end;
    3:
    begin
      Anchor := DoublePoint(SelectionTL.x, SelectionBR.y);
      SelectionVertex := DoublePoint(SelectionBR.x, SelectionTL.y);
      sx := (ANewPos.x - Anchor.x) / (SelectionVertex.x - Anchor.x);
      sy := (Anchor.y - ANewPos.y) / (Anchor.y - SelectionVertex.y);
    end;
    else
      Exit;
  end;
  for f in CanvasItems do
  begin
    if f.Selected then
      for v := Low(f.Vertexes) to High(f.Vertexes) do
      begin
        dx := (f.Vertexes[v].x - Anchor.x) * sx;
        dy := (f.Vertexes[v].y - Anchor.y) * sy;
        f.MoveVertex(v, DoublePoint(Anchor.x + dx, Anchor.y + dy));
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
  sp: TToolParamList;
  p: TToolParam;
begin
  if CanvasItems <> nil then
  begin
    sp := GetSelectionParams;
    if sp <> nil then
      for p in sp do
        p.UnattachAll;
    for CurrentFigure in CanvasItems do
      if CurrentFigure <> nil then
        CurrentFigure.Selected := False;
  end;
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

function GetSelectionParams: TToolParamList;
var
  f: TFigure;
  FigureParams: TToolParamList;
  CommonParams: array of TToolParamList;
  p, p0: TToolParam;
  i, k, SelCount: integer;
begin
  SelCount := 0;
  if CanvasItems = nil then
    Exit(nil);
  for f in CanvasItems do
  begin
    if (f <> nil) and (f.Selected) then
    begin
      inc(SelCount);
      FigureParams := f.GetParams;
      if Length(CommonParams) = 0 then
      begin
        SetLength(CommonParams, Length(FigureParams));
        for i := Low(FigureParams) to High(FigureParams) do
        begin
          SetLength(CommonParams[i], 1);
          CommonParams[i][0] := FigureParams[i];
        end;
      end
      else
      begin
        for p in FigureParams do
        begin
          for i := Low(CommonParams) to High(CommonParams) do
          begin
            p0 := CommonParams[i][Length(CommonParams[i]) - 1];
            if (p0.ClassType = p.ClassType) and (p0.Name = p.Name) then
            begin
              SetLength(CommonParams[i], Length(CommonParams[i]) + 1);
              CommonParams[i][Length(CommonParams[i]) - 1] := p;
            end;
          end;
        end;
      end;
    end;
  end;
  for i := Low(CommonParams) to High(CommonParams) do
  begin
    if Length(CommonParams[i]) = SelCount then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := CommonParams[i][0];
      for k := 0 to High(CommonParams[i]) do
        Result[Length(Result) - 1].AttachParam(CommonParams[i][k]);
    end;
  end;
end;

{ TFillableFigure }

procedure TFillableFigure.SetCanvasStyles(ACanvas: TCanvas);
begin
  inherited SetCanvasStyles(ACanvas);
  with ACanvas do
  begin
    Brush.Color := FillColor.Value;
    Brush.Style := FillStyle.Value;
  end;
end;

constructor TFillableFigure.Create(X, Y: double; ALineWidth: TIntegerParam;
  ALineColor: TColorParam; ALineStyle: TLineStyleParam;
  AFillColor: TColorParam; AFillStyle: TFillStyleParam);
begin
  inherited Create(X, Y, ALineWidth, ALineColor, ALineStyle);
  AFillColor.Assign(FFillColor);
  AFillStyle.Assign(FFillStyle);
end;

destructor TFillableFigure.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FFillColor);
  FreeAndNil(FFillStyle);
end;

function TFillableFigure.GetParams: TToolParamList;
begin
  Result := TToolParamList.Create(LineWidth, LineColor, LineStyle,
    FillColor, FillStyle);
end;

{ TDrawableFigure }

procedure TDrawableFigure.SetCanvasStyles(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Color := LineColor.Value;
    Pen.Width := GetCanvasLineWidth;
    Pen.Style := LineStyle.Value;
  end;
end;

function TDrawableFigure.GetCanvasLineWidth: integer;
begin
  Result := max(1, Round(LineWidth.Value * Scale));
end;

constructor TDrawableFigure.Create(X, Y: double; ALineWidth: TIntegerParam;
  ALineColor: TColorParam; ALineStyle: TLineStyleParam);
begin
  SetLength(FVertexes, 2);
  FVertexes[0] := DoublePoint(X, Y);
  FVertexes[1] := FVertexes[0];
  ALineColor.Assign(FLineColor);
  ALineWidth.Assign(FLineWidth);
  ALineStyle.Assign(FLineStyle);
  Selected := False;
end;

destructor TDrawableFigure.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FLineWidth);
  FreeAndNil(FLineColor);
  FreeAndNil(FLineStyle);
end;

procedure TDrawableFigure.MouseMove(X, Y: integer);
begin
  FVertexes[High(FVertexes)] := CanvasToWorld(X, Y);
end;

function TDrawableFigure.UnderPoint(const APoint: TDoublePoint): boolean;
const
  eps = 0.01;
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
    d := LineWidth.Value / 2 + PADDING / Scale;
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

function TDrawableFigure.GetParams: TToolParamList;
begin
  Result := TToolParamList.Create(LineWidth, LineColor, LineStyle);
end;

{ TPolygon }

constructor TPolygon.Create(X, Y: double; ALineWidth: TIntegerParam;
  ALineColor: TColorParam; ALineStyle: TLineStyleParam;
  AFillColor: TColorParam; AFillStyle: TFillStyleParam;
  AVertexCount: TIntegerParam);
var
  ScreenPoint: TPoint;
begin
  inherited Create(X, Y, ALineWidth, ALineColor, ALineStyle,
    AFillColor, AFillStyle);
  AVertexCount.Assign(FVertexCount);
  SetLength(FVertexes, VertexCount.Value);
  StartPoint := DoublePoint(X, Y);
  ScreenPoint := WorldToCanvas(X, Y);
  EndPoint := ScreenPoint;
  MouseMove(ScreenPoint.x, ScreenPoint.y);
end;

destructor TPolygon.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FVertexCount);
end;

procedure TPolygon.Draw(ACanvas: TCanvas);
var
  i: integer;
  CanvasVertexes: TPointList;
begin
  SetCanvasStyles(ACanvas);
  if Length(FVertexes) <> VertexCount.Value then
    MouseMove(EndPoint.x, EndPoint.y);
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
  EndPoint.x := X;
  EndPoint.y := Y;
  WorldPos := CanvasToWorld(X, Y);
  CurrentRotation := arctan2(WorldPos.y - StartPoint.y, WorldPos.x - StartPoint.x);
  Radius := Dist(StartPoint, WorldPos);
  if Length(FVertexes) <> VertexCount.Value then
    SetLength(FVertexes, VertexCount.Value);
  for i := Low(FVertexes) to High(FVertexes) do
  begin
    FVertexes[i].x := StartPoint.x + Radius * cos(CurrentRotation +
      (i * 2 * pi / VertexCount.Value));
    FVertexes[i].y := StartPoint.y + Radius * sin(CurrentRotation +
      (i * 2 * pi / VertexCount.Value));
  end;
end;

function TPolygon.UnderPoint(const APoint: TDoublePoint): boolean;
var
  RegionFigure: HRGN;
  CanvasVertexes: TPointList;
  CanvasPoint: TPoint;
  i: integer;
begin
  SetLength(CanvasVertexes, VertexCount.Value);
  for i := Low(FVertexes) to High(FVertexes) do
    CanvasVertexes[i] := WorldToCanvas(FVertexes[i]);
  RegionFigure := CreatePolygonRgn(CanvasVertexes[0], VertexCount.Value, WINDING);
  CanvasPoint := WorldToCanvas(APoint);
  Result := PtInRegion(RegionFigure, CanvasPoint.x, CanvasPoint.y);
  DeleteObject(RegionFigure);
end;

function TPolygon.GetParams: TToolParamList;
begin
  Result := TToolParamList.Create(VertexCount, LineWidth, LineColor, LineStyle,
    FillColor, FillStyle);
end;

{ TRoundRect }

constructor TRoundRect.Create(X, Y: double; ALineWidth: TIntegerParam;
  ALineColor: TColorParam; ALineStyle: TLineStyleParam;
  AFillColor: TColorParam; AFillStyle: TFillStyleParam; ARoundX,
  ARoundY: TIntegerParam);
begin
  inherited Create(X, Y, ALineWidth, ALineColor, ALineStyle,
    AFillColor, AFillStyle);
  ARoundX.Assign(FRoundX);
  ARoundY.Assign(FRoundY);
end;

destructor TRoundRect.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FRoundX);
  FreeAndNil(FRoundY);
end;

procedure TRoundRect.Draw(ACanvas: TCanvas);
var
  CanvasTopLeft, CanvasBottomRight: TPoint;
begin
  SetCanvasStyles(ACanvas);
  CanvasTopLeft := WorldToCanvas(FVertexes[0].x, FVertexes[0].y);
  CanvasBottomRight := WorldToCanvas(FVertexes[1].x, FVertexes[1].y);
  ACanvas.RoundRect(CanvasTopLeft.x, CanvasTopLeft.y,
    CanvasBottomRight.x, CanvasBottomRight.y, RoundX.Value*2, RoundY.Value*2);
end;

function TRoundRect.UnderPoint(const APoint: TDoublePoint): boolean;
var
  RegionFigure, RegionInner: HRGN;
  CanvasVertexes: TPointList;
  CanvasPoint: TPoint;
  d: integer;
begin
  SetLength(CanvasVertexes, 2);
  CanvasVertexes[0] := WorldToCanvas(FVertexes[0]);
  CanvasVertexes[1] := WorldToCanvas(FVertexes[1]);
  d := LineWidthCanvas div 2 + PADDING;

  RegionFigure := CreateRoundRectRgn(
    min(CanvasVertexes[0].x, CanvasVertexes[1].x) - d,
    min(CanvasVertexes[0].y, CanvasVertexes[1].y) - d,
    max(CanvasVertexes[0].x, CanvasVertexes[1].x) + d,
    max(CanvasVertexes[0].y, CanvasVertexes[1].y) + d,
    FRoundX.Value * 2, FRoundY.Value * 2);
  CanvasPoint := WorldToCanvas(APoint);
  if FillStyle.Value = bsClear then
  begin
    RegionInner := CreateRoundRectRgn(
      max(CanvasVertexes[0].x, CanvasVertexes[1].x) - d,
      max(CanvasVertexes[0].y, CanvasVertexes[1].y) - d,
      min(CanvasVertexes[0].x, CanvasVertexes[1].x) + d,
      min(CanvasVertexes[0].y, CanvasVertexes[1].y) + d,
      FRoundX.Value * 2, FRoundY.Value * 2);
    CombineRgn(RegionFigure, RegionFigure, RegionInner, RGN_DIFF);
    DeleteObject(RegionInner);
  end;
  Result := PtInRegion(RegionFigure, CanvasPoint.x, CanvasPoint.y);
  DeleteObject(RegionFigure);
end;

function TRoundRect.GetParams: TToolParamList;
begin
  Result := TToolParamList.Create(RoundX, RoundY, LineWidth, LineColor,
    LineStyle, FillColor, FillStyle);
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

function TFigure.GetVertexIndexAtPos(const APoint: TPoint): integer;
var
  i: integer;
  v: TPoint;
begin
  Result := -1;
  for i := High(FVertexes) downto Low(FVertexes) do
  begin
    v := WorldToCanvas(FVertexes[i]);
    if (abs(APoint.x - v.x) <= PADDING) and
      (abs(APoint.y - v.y) <= PADDING) then
      Exit(i);
  end;
end;

function TFigure.GetVertexIndexAtPos(const APoint: TDoublePoint): integer;
begin
  Result := GetVertexIndexAtPos(WorldToCanvas(APoint));
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

function TRectangle.UnderPoint(const APoint: TDoublePoint): boolean;
var
  RegionFigure, RegionInner: HRGN;
  CanvasVertexes: TPointList;
  CanvasPoint: TPoint;
  d: integer;
begin
  SetLength(CanvasVertexes, 2);
  CanvasVertexes[0] := WorldToCanvas(FVertexes[0]);
  CanvasVertexes[1] := WorldToCanvas(FVertexes[1]);
  d := LineWidthCanvas div 2 + PADDING;

  CanvasPoint := WorldToCanvas(APoint);
  RegionFigure := CreateRectRgn(min(CanvasVertexes[0].x, CanvasVertexes[1].x) -
    d, min(CanvasVertexes[0].y, CanvasVertexes[1].y) - d,
    max(CanvasVertexes[0].x, CanvasVertexes[1].x) + d,
    max(CanvasVertexes[0].y, CanvasVertexes[1].y) + d);
  if FillStyle.Value = bsClear then
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

function TEllipse.UnderPoint(const APoint: TDoublePoint): boolean;
var
  RegionFigure, RegionInner: HRGN;
  CanvasVertexes: TPointList;
  CanvasPoint: TPoint;
  d: integer;
begin
  SetLength(CanvasVertexes, 2);
  CanvasVertexes[0] := WorldToCanvas(FVertexes[0]);
  CanvasVertexes[1] := WorldToCanvas(FVertexes[1]);
  d := LineWidthCanvas div 2 + PADDING;
  CanvasPoint := WorldToCanvas(APoint);

  RegionFigure := CreateEllipticRgn(
    min(CanvasVertexes[0].x - d, CanvasVertexes[1].x - d),
    min(CanvasVertexes[0].y - d, CanvasVertexes[1].y - d),
    max(CanvasVertexes[0].x + d, CanvasVertexes[1].x + d),
    max(CanvasVertexes[0].y + d, CanvasVertexes[1].y + d));

  if FillStyle.Value = bsClear then
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
