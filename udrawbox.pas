unit UDrawBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Math,
  ExtCtrls, Spin, ActnList, Buttons, StdCtrls, UAbout, UFigures, UTransform, Types;

type

  { TDrawForm }

  TDrawForm = class(TForm)
    ChangeFillColorButton: TColorButton;
    ChangeFillStyleComboBox: TComboBox;
    ChangeLineStyleComboBox: TComboBox;
    FillColorLabel: TLabel;
    FillStyleLabel: TLabel;
    CanvasPropsPanel: TPanel;
    ScaleFloatSpin: TFloatSpinEdit;
    ScaleLabel: TLabel;
    LineStyleLabel: TLabel;
    LineWidthLabel: TLabel;
    DelimeterMenuItem: TMenuItem;
    ClearAllMenuItem: TMenuItem;
    HorizontalScrollBar: TScrollBar;
    ShowAllMenuItem: TMenuItem;
    VerticalScrollBar: TScrollBar;
    ToolsPanel: TPanel;
    LineColorLabel: TLabel;
    RedoAction: TAction;
    UndoAction: TAction;
    EditorActionList: TActionList;
    ChangeLineColorButton: TColorButton;
    DrawColorDialog: TColorDialog;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    EditMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    UndoMenuItem: TMenuItem;
    MainPaintBox: TPaintBox;
    PenWidthSpinEdit: TSpinEdit;
    procedure ChangeFillStyleComboBoxChange(Sender: TObject);
    procedure ChangeLineStyleComboBoxChange(Sender: TObject);
    procedure ClearAllMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MainPaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ShowAllMenuItemClick(Sender: TObject);
    procedure SetScrollBars;
    procedure MainPaintBoxResize(Sender: TObject);
    procedure ScaleFloatSpinChange(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure MainPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MainPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainPaintBoxPaint(Sender: TObject);
    procedure ChangeLineColorButtonColorChanged(Sender: TObject);
    procedure ChangeFillColorButtonColorChanged(Sender: TObject);
    procedure PenWidthSpinEditChange(Sender: TObject);
    procedure UndoActionExecute(Sender: TObject);
    procedure RedoActionExecute(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

  private
    { private declarations }
  public
    { public declarations }
  end;

  TFillStyleItem = record
    Name: string;
    BrushStyle: TBrushStyle;
  end;
  TLineStyleItem = record
    Name: string;
    PenStyle: TPenStyle;
  end;

const
  TOOL_BUTTON_SIZE: integer = 32;
  TOOL_BUTTON_MARGIN: integer = 4;
  TOOL_BUTTON_PADDING: integer = 1;
  START_LINE_COLOR: TColor = clBlack;
  START_FILL_COLOR: TColor = clWhite;
  START_LINE_STYLE: integer = 0; //index of LINE_STYLES
  START_FILL_STYLE: integer = 1; //index of FILL_STYLES
  START_WIDTH: integer = 1;
  CANVAS_OFFSET_BORDER_SIZE: integer = 10;
  LINE_STYLES: array[0..5] of TLineStyleItem =
    (
      (Name: 'Solid';         PenStyle: psSolid),
      (Name: 'No line';       PenStyle: psClear),
      (Name: 'Dots';          PenStyle: psDot),
      (Name: 'Dashes';        PenStyle: psDash),
      (Name: 'Dash dots';     PenStyle: psDashDot),
      (Name: 'Dash dot dots'; PenStyle: psDashDotDot)
    );
  FILL_STYLES: array[0..7] of TFillStyleItem =
    (
      (Name: 'Solid';              BrushStyle: bsSolid),
      (Name: 'Hollow';             BrushStyle: bsClear),
      (Name: 'Horizontal stripes'; BrushStyle: bsHorizontal),
      (Name: 'Vertical stripes';   BrushStyle: bsVertical),
      (Name: 'Left diagonal';      BrushStyle: bsFDiagonal),
      (Name: 'Right diagonal';     BrushStyle: bsBDiagonal),
      (Name: 'Cross';              BrushStyle: bsCross),
      (Name: 'Diagonal cross';     BrushStyle: bsDiagCross)
    );

var
  DrawForm: TDrawForm;
  IsDrawing: boolean;
  CurrentFigure: TFigureClass;
  CanvasItems, UndoHistory: TFigureList;
  CurrentLineColor, CurrentFillColor: TColor;
  CurrentLineWidth, CurrentLineStyle, CurrentFillStyle: integer;
  WorldTopLeft, WorldBottomRight: TDoublePoint;

implementation

{$R *.lfm}

{ TDrawForm }

procedure TDrawForm.FormCreate(Sender: TObject);
var
  b: TSpeedButton;
  i: integer;
  FillStyle: TFillStyleItem;
  LineStyle: TLineStyleItem;
  CurrentIcon: TPicture;
  IconsPerRow: integer;
begin
  DrawForm.DoubleBuffered := true;
  DrawForm.Caption := ApplicationName;
  isDrawing := false;

  CurrentLineColor := START_LINE_COLOR;
  ChangeLineColorButton.ButtonColor := CurrentLineColor;

  CurrentFillColor := START_FILL_COLOR;
  ChangeFillColorButton.ButtonColor := CurrentFillColor;

  CurrentLineWidth := START_WIDTH;
  PenWidthSpinEdit.Value := CurrentLineWidth;

  IconsPerRow := ToolsPanel.Width div
    (TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN + TOOL_BUTTON_PADDING);

  for i:=Low(FiguresBase) to High(FiguresBase) do
  begin
    b := TSpeedButton.Create(ToolsPanel);
    b.Parent := ToolsPanel;
    b.Name := 'ToolButton' + FiguresBase[i].ClassName;
    b.GroupIndex := 1;
    b.Tag := i;
    b.OnClick := @ToolButtonClick;

    CurrentIcon := TPicture.Create;
    CurrentIcon.LoadFromFile(FiguresBase[i].ClassName + '.png');
    b.Glyph := CurrentIcon.Bitmap;
    CurrentIcon.Free;

    b.Left := (i mod IconsPerRow) * (TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN) +
              TOOL_BUTTON_PADDING;
    b.Top  := (i div IconsPerRow) * (TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN) +
              TOOL_BUTTON_PADDING;
    b.Width := TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN;
    b.Height := b.Width;

    if i = 0 then
      b.Click;
  end;

  for LineStyle in LINE_STYLES do
    ChangeLineStyleComboBox.Items.Add(LineStyle.Name);
  CurrentLineStyle := START_Line_STYLE;
  ChangeLineStyleComboBox.ItemIndex := CurrentLineStyle;

  for FillStyle in FILL_STYLES do
    ChangeFillStyleComboBox.Items.Add(FillStyle.Name);
  CurrentFillStyle := START_FILL_STYLE;
  ChangeFillStyleComboBox.ItemIndex := CurrentFillStyle;

  ScaleFloatSpin.MinValue := ZOOM_MIN * 100;
  ScaleFloatSpin.MaxValue := ZOOM_MAX * 100;

  CanvasWidth := MainPaintBox.Width;
  CanvasHeight := MainPaintBox.Height;

  SetScrollBars;
end;

procedure TDrawForm.MainPaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    ZoomPoint(CanvasToWorld(MousePos), Scale*2)
  else
    ZoomPoint(CanvasToWorld(MousePos), Scale/2);
  SetScrollBars;
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.ShowAllMenuItemClick(Sender: TObject);
var
  NewScale: double;
  i: TFigure;
begin
  for i in CanvasItems do
  begin
    WorldTopLeft.x := Min(WorldTopLeft.x, i.TopLeftBorder.x);
    WorldTopLeft.y := Min(WorldTopLeft.y, i.TopLeftBorder.y);
    WorldBottomRight.x := Max(WorldBottomRight.x, i.BottomRightBorder.x);
    WorldBottomRight.y := Max(WorldBottomRight.y, i.BottomRightBorder.y);
  end;
  if (WorldTopLeft.x <> WorldBottomRight.x) and
    (WorldTopLeft.y <> WorldBottomRight.y) then
  begin
    NewScale := Scale*Min(CanvasWidth / Scale
      / (WorldBottomRight.x - WorldTopLeft.x), CanvasHeight / Scale
      / (WorldBottomRight.y - WorldTopLeft.y));
    ZoomPoint(DoublePoint((WorldBottomRight.x + WorldTopLeft.x)/2,
      (WorldBottomRight.y + WorldTopLeft.y)/2), NewScale);
    SetScrollBars;
    MainPaintBox.Invalidate;
  end;
end;


procedure TDrawForm.SetScrollBars;
var
  i: TFigure;
  HorMin, HorMax, VerMin, VerMax: integer;
  CanvasCorner: TDoublePoint;
begin
  HorMin := Round(Min(CanvasOffset.x - CANVAS_OFFSET_BORDER_SIZE,
    -CANVAS_OFFSET_BORDER_SIZE));
  HorMax := Round(Max(CanvasOffset.x + CANVAS_OFFSET_BORDER_SIZE,
    CANVAS_OFFSET_BORDER_SIZE));
  VerMin := Round(Min(CanvasOffset.y - CANVAS_OFFSET_BORDER_SIZE,
    -CANVAS_OFFSET_BORDER_SIZE));
  VerMax := Round(Max(CanvasOffset.y + CANVAS_OFFSET_BORDER_SIZE,
    CANVAS_OFFSET_BORDER_SIZE));

  CanvasCorner := CanvasToWorld(CanvasWidth, CanvasHeight);
  for i in CanvasItems do
  begin
    HorMin := Min(HorMin, Round(i.TopLeftBorder.x - CANVAS_OFFSET_BORDER_SIZE));
    HorMax := Max(HorMax, Round(CanvasOffset.x + i.BottomRightBorder.x
      - CanvasCorner.x + CANVAS_OFFSET_BORDER_SIZE));
    VerMin := Min(VerMin, Round(i.TopLeftBorder.y - CANVAS_OFFSET_BORDER_SIZE));
    VerMax := Max(VerMax, Round(CanvasOffset.y + i.BottomRightBorder.y
      - CanvasCorner.y + CANVAS_OFFSET_BORDER_SIZE));
  end;

  HorizontalScrollBar.Min := HorMin;
  HorizontalScrollBar.Max := HorMax;
  VerticalScrollBar.Min := VerMin;
  VerticalScrollBar.Max := VerMax;

  HorizontalScrollBar.Position := Round(CanvasOffset.x);
  VerticalScrollBar.Position := Round(CanvasOffset.y);

  HorizontalScrollBar.PageSize := Floor((CanvasCorner.x-CanvasOffset.x)
    / (HorMax-HorMin));
  VerticalScrollBar.PageSize := Floor((CanvasCorner.y-CanvasOffset.y)
    / (VerMax-VerMin));
end;

procedure TDrawForm.MainPaintBoxResize(Sender: TObject);
begin
  CanvasWidth := MainPaintBox.Width;
  CanvasHeight := MainPaintBox.Height;
  SetScrollBars;
end;

procedure TDrawForm.ScaleFloatSpinChange(Sender: TObject);
var
  CanvasCenter: TDoublePoint;
begin
  CanvasCenter := CanvasToWorld(CanvasWidth div 2, CanvasHeight div 2);
  ZoomPoint(CanvasCenter, ScaleFloatSpin.Value / 100);
  SetScrollBars;
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.ToolButtonClick(Sender: TObject);
var
  b: TSpeedButton;
begin
  b := Sender as TSpeedButton;
  CurrentFigure := FiguresBase[b.Tag];
  b.Down := true;
end;

procedure TDrawForm.MainPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  WorldStartPoint: TDoublePoint;
begin
  IsDrawing := true;
  SetLength(CanvasItems, Length(CanvasItems) + 1);
  WorldStartPoint := CanvasToWorld(X, Y);
  CanvasItems[High(CanvasItems)] := CurrentFigure.Create(WorldStartPoint.x,
    WorldStartPoint.y, CurrentLineColor, CurrentLineWidth, CurrentFillColor,
    LINE_STYLES[CurrentLineStyle].PenStyle,
    FILL_STYLES[CurrentFillStyle].BrushStyle, Button);
  MainPaintBox.Invalidate;
  SetScrollBars;
end;

procedure TDrawForm.MainPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if IsDrawing then
  begin
    CanvasItems[High(CanvasItems)].MouseMove(X, Y);
    if CanvasItems[High(CanvasItems)].CanBeDestroyed then
    begin
      FreeAndNil(CanvasItems[High(CanvasItems)]);
      SetLength(CanvasItems, Length(CanvasItems) - 1);
      IsDrawing := false;
    end;
    MainPaintBox.Invalidate;
    SetScrollBars;
  end;
end;

procedure TDrawForm.MainPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  IsDrawing := false;
  CanvasItems[High(CanvasItems)].MouseUp(X, Y);
  if CanvasItems[High(CanvasItems)].CanBeDestroyed then
  begin
    FreeAndNil(CanvasItems[High(CanvasItems)]);
    SetLength(CanvasItems, Length(CanvasItems) - 1);
  end
  else
  begin
    for i := Low(UndoHistory) to High(UndoHistory) do
      FreeAndNil(UndoHistory[i]);
    SetLength(UndoHistory, 0);
  end;
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.MainPaintBoxPaint(Sender: TObject);
var
  i: TFigure;
begin
  ScaleFloatSpin.Value := Scale * 100;
  with MainPaintBox.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(0, 0, MainPaintBox.Width, MainPaintBox.Height);
  end;
  for i in CanvasItems do
  begin
    WorldTopLeft.x := Min(WorldTopLeft.x, i.TopLeftBorder.x);
    WorldTopLeft.y := Min(WorldTopLeft.y, i.TopLeftBorder.y);
    WorldBottomRight.x := Max(WorldBottomRight.x, i.BottomRightBorder.x);
    WorldBottomRight.y := Max(WorldBottomRight.y, i.BottomRightBorder.y);
    i.Draw(MainPaintBox.Canvas);
  end;
end;

procedure TDrawForm.ChangeLineColorButtonColorChanged(Sender: TObject);
begin
  CurrentLineColor := ChangeLineColorButton.ButtonColor;
end;

procedure TDrawForm.ChangeFillColorButtonColorChanged(Sender: TObject);
begin
  CurrentFillColor := ChangeFillColorButton.ButtonColor;
end;

procedure TDrawForm.ChangeFillStyleComboBoxChange(Sender: TObject);
begin
  CurrentFillStyle := ChangeFillStyleComboBox.ItemIndex;
end;

procedure TDrawForm.ChangeLineStyleComboBoxChange(Sender: TObject);
begin
  CurrentLineStyle := ChangeLineStyleComboBox.ItemIndex;
end;

procedure TDrawForm.PenWidthSpinEditChange(Sender: TObject);
begin
  CurrentLineWidth := PenWidthSpinEdit.Value;
end;

procedure TDrawForm.UndoActionExecute(Sender: TObject);
begin
  if Length(CanvasItems) > 0 then
  begin
    SetLength(UndoHistory, Length(UndoHistory) + 1);
    UndoHistory[High(UndoHistory)] := CanvasItems[High(CanvasItems)];
    SetLength(CanvasItems, Length(CanvasItems) - 1);
    MainPaintBox.Invalidate;
    SetScrollBars;
  end;
end;

procedure TDrawForm.RedoActionExecute(Sender: TObject);
begin
  if Length(UndoHistory) > 0 then
  begin
    SetLength(CanvasItems, Length(CanvasItems) + 1);
    CanvasItems[High(CanvasItems)] := UndoHistory[High(UndoHistory)];
    SetLength(UndoHistory, Length(UndoHistory) - 1);
    MainPaintBox.Invalidate;
    SetScrollBars;
  end;
end;

procedure TDrawForm.ClearAllMenuItemClick(Sender: TObject);
var
  i: integer;
begin
  for i := Low(UndoHistory) to High(UndoHistory) do
    FreeAndNil(UndoHistory[i]);
  SetLength(UndoHistory, 0);
  for i := Low(CanvasItems) to High(CanvasItems) do
    FreeAndNil(CanvasItems[i]);
  SetLength(CanvasItems, 0);
  MainPaintBox.Invalidate;
  SetScrollBars;
end;

procedure TDrawForm.AboutMenuItemClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TDrawForm.ExitMenuItemClick(Sender: TObject);
begin
  DrawForm.Close;
end;

procedure TDrawForm.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  CanvasOffset.x := HorizontalScrollBar.Position;
  CanvasOffset.y := VerticalScrollBar.Position;
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ClearAllMenuItemClick(nil);
end;

end.

