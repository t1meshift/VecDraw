unit UDrawBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Math,
  ExtCtrls, Spin, ActnList, Buttons, StdCtrls, UAbout, UFigures, UTransform;

type

  { TDrawForm }

  TDrawForm = class(TForm)
    ChangeFillColorButton: TColorButton;
    ChangeFillStyleComboBox: TComboBox;
    ChangeLineStyleComboBox: TComboBox;
    FillColorLabel: TLabel;
    FillStyleLabel: TLabel;
    CurrentToolImage: TImage;
    ScaleFloatSpin: TFloatSpinEdit;
    ScaleLabel: TLabel;
    LineStyleLabel: TLabel;
    LineWidthLabel: TLabel;
    DelimeterMenuItem: TMenuItem;
    ClearAllMenuItem: TMenuItem;
    HorizontalScrollBar: TScrollBar;
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
    procedure HorizontalScrollBarChange(Sender: TObject);
    procedure VerticalScrollBarChange(Sender: TObject);
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
  TOOL_BUTTON_MARGIN: integer = 2;
  TOOL_BUTTON_PADDING: integer = 1;
  START_LINE_COLOR: TColor = clBlack;
  START_FILL_COLOR: TColor = clWhite;
  START_LINE_STYLE: integer = 0; //index of LINE_STYLES
  START_FILL_STYLE: integer = 1; //index of FILL_STYLES
  START_WIDTH: integer = 1;
  CANVAS_OFFSET_BORDER_SIZE: integer = 10;
  LINE_STYLES: array[0..5] of TLineStyleItem =
    (
      (
        Name: 'Solid';
        PenStyle: psSolid
      ),
      (
        Name: 'No line';
        PenStyle: psClear
      ),
      (
        Name: 'Dots';
        PenStyle: psDot
      ),
      (
        Name: 'Dashes';
        PenStyle: psDash
      ),
      (
        Name: 'Dash dots';
        PenStyle: psDashDot
      ),
      (
        Name: 'Dash dot dots';
        PenStyle: psDashDotDot
      )
    );
  FILL_STYLES: array[0..7] of TFillStyleItem =
    (
      (
        Name: 'Solid';
        BrushStyle: bsSolid
      ),
      (
        Name: 'Hollow';
        BrushStyle: bsClear
      ),
      (
        Name: 'Horizontal stripes';
        BrushStyle: bsHorizontal
      ),
      (
        Name: 'Vertical stripes' ;
        BrushStyle: bsVertical
      ),
      (
        Name: 'Left diagonal';
        BrushStyle: bsFDiagonal
      ),
      (
        Name: 'Right diagonal';
        BrushStyle: bsBDiagonal
      ),
      (
        Name: 'Cross';
        BrushStyle: bsCross
      ),
      (
        Name: 'Diagonal cross';
        BrushStyle: bsDiagCross
      )
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

  ToolsPanel.Height := Ceil(Length(FiguresBase) / IconsPerRow) *
    (TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN) + TOOL_BUTTON_PADDING * 2;

  for i:=Low(FiguresBase) to High(FiguresBase) do
  begin
    b := TSpeedButton.Create(ToolsPanel);
    b.Parent := ToolsPanel;
    b.Name := 'ToolButton' + FiguresBase[i].ClassName;
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

  HorizontalScrollBar.Min := -CANVAS_OFFSET_BORDER_SIZE;
  HorizontalScrollBar.Max := CANVAS_OFFSET_BORDER_SIZE;

  VerticalScrollBar.Min := -CANVAS_OFFSET_BORDER_SIZE;
  VerticalScrollBar.Max := CANVAS_OFFSET_BORDER_SIZE;

  CanvasWidth := MainPaintBox.Width;
  CanvasHeight := MainPaintBox.Height;
end;

procedure TDrawForm.HorizontalScrollBarChange(Sender: TObject);
begin
  CanvasOffset.x := HorizontalScrollBar.Position;
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.VerticalScrollBarChange(Sender: TObject);
begin
  CanvasOffset.y := VerticalScrollBar.Position;
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.SetScrollBars;
var
  i: TFigure;
  HorMin, HorMax, VerMin, VerMax: integer;
begin
  HorMin := Round(Min(CanvasOffset.x - CANVAS_OFFSET_BORDER_SIZE,
    -CANVAS_OFFSET_BORDER_SIZE));
  HorMax := Round(Max(CanvasOffset.x + CANVAS_OFFSET_BORDER_SIZE,
    CANVAS_OFFSET_BORDER_SIZE));
  VerMin := Round(Min(CanvasOffset.y - CANVAS_OFFSET_BORDER_SIZE,
    -CANVAS_OFFSET_BORDER_SIZE));
  VerMax := Round(Max(CanvasOffset.y + CANVAS_OFFSET_BORDER_SIZE,
    CANVAS_OFFSET_BORDER_SIZE));

  if Length(CanvasItems) > 0 then
  begin
    i := CanvasItems[High(CanvasItems)];
    HorMin := Round(Min(HorizontalScrollBar.Min,
      i.TopLeftBorder.x - CANVAS_OFFSET_BORDER_SIZE));
    HorMax := Round(Max(HorizontalScrollBar.Max,
      i.BottomRightBorder.x + CANVAS_OFFSET_BORDER_SIZE));
    VerMin := Round(Min(VerticalScrollBar.Min,
      i.TopLeftBorder.y - CANVAS_OFFSET_BORDER_SIZE));
    VerMax := Round(Max(VerticalScrollBar.Max,
      i.BottomRightBorder.y + CANVAS_OFFSET_BORDER_SIZE));
  end;
  HorizontalScrollBar.Min := HorMin;
  HorizontalScrollBar.Max := HorMax;
  VerticalScrollBar.Min := VerMin;
  VerticalScrollBar.Max := VerMax;
  HorizontalScrollBar.Position := Round(CanvasOffset.x);
  VerticalScrollBar.Position := Round(CanvasOffset.y);
end;

procedure TDrawForm.MainPaintBoxResize(Sender: TObject);
begin
  CanvasWidth := MainPaintBox.Width;
  CanvasHeight := MainPaintBox.Height;
end;

procedure TDrawForm.ScaleFloatSpinChange(Sender: TObject);
var
  CanvasCenter: TDoublePoint;
begin
  CanvasCenter := CanvasToWorld(Point(CanvasWidth div 2, CanvasHeight div 2));
  ZoomPoint(CanvasCenter, ScaleFloatSpin.Value / 100);
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.ToolButtonClick(Sender: TObject);
var
  b: TSpeedButton;
begin
  b := Sender as TSpeedButton;
  CurrentFigure := FiguresBase[b.Tag];
  CurrentToolImage.Picture.Bitmap := b.Glyph;
end;

procedure TDrawForm.MainPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  WorldStartPoint: TDoublePoint;
begin
  IsDrawing := true;
  SetLength(CanvasItems, Length(CanvasItems) + 1);
  WorldStartPoint := CanvasToWorld(Point(X, Y));
  CanvasItems[High(CanvasItems)] := CurrentFigure.Create(WorldStartPoint.x,
    WorldStartPoint.y, CurrentLineColor, CurrentLineWidth, CurrentFillColor,
    LINE_STYLES[CurrentLineStyle].PenStyle,
    FILL_STYLES[CurrentFillStyle].BrushStyle, Button);
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.MainPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if IsDrawing then
  begin
    CanvasItems[High(CanvasItems)].MouseMove(X, Y);
    SetScrollBars;
    MainPaintBox.Invalidate;
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
  SetScrollBars;
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
    i.Draw(MainPaintBox.Canvas);
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
end;

procedure TDrawForm.AboutMenuItemClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TDrawForm.ExitMenuItemClick(Sender: TObject);
begin
  DrawForm.Close;
end;

procedure TDrawForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ClearAllMenuItemClick(nil);
end;

end.

