unit UDrawBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Spin, ActnList, Buttons, StdCtrls, UAbout, UFigures, UFiguresBase;

type

  { TDrawForm }

  TDrawForm = class(TForm)
    ChangeFillColorButton: TColorButton;
    ChangeFillStyleComboBox: TComboBox;
    FillColorLabel: TLabel;
    FillStyleLabel: TLabel;
    LineWidthLabel: TLabel;
    DelimeterMenuItem: TMenuItem;
    ClearAllMenuItem: TMenuItem;
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
    procedure ClearAllMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
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

const
  TOOL_BUTTON_SIZE = 32;
  TOOL_BUTTON_MARGIN = 2;
  TOOL_BUTTON_PADDING = 1;
  START_LINE_COLOR = clBlack;
  START_FILL_COLOR = clWhite;
  START_FILL_STYLE = 1; //index of FILL_STYLES
  START_WIDTH = 1;
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
  CanvasItems, UndoHistory: array of TFigure;
  CurrentLineColor, CurrentFillColor: TColor;
  CurrentLineWidth, CurrentFillStyle: integer;

implementation

{$R *.lfm}

{ TDrawForm }

procedure TDrawForm.FormCreate(Sender: TObject);
var
  b: TSpeedButton;
  i: integer;
  FillStyle: TFillStyleItem;
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

  ToolsPanel.Height := (Length(FiguresBase) div IconsPerRow) *
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


  for FillStyle in FILL_STYLES do
    ChangeFillStyleComboBox.Items.Add(FillStyle.Name);
  CurrentFillStyle := START_FILL_STYLE;
  ChangeFillStyleComboBox.ItemIndex := CurrentFillStyle;
end;

procedure TDrawForm.ToolButtonClick(Sender: TObject);
var
  b: TSpeedButton;
begin
  b := Sender as TSpeedButton;
  CurrentFigure := FiguresBase[b.Tag];
end;

procedure TDrawForm.MainPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    IsDrawing := true;
    SetLength(CanvasItems, Length(CanvasItems) + 1);
    CanvasItems[High(CanvasItems)] := CurrentFigure.Create(X, Y,
      CurrentLineColor, CurrentLineWidth,
      CurrentFillColor, FILL_STYLES[CurrentFillStyle].BrushStyle);
  end;
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.MainPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if IsDrawing then
  begin
    CanvasItems[High(CanvasItems)].MouseMove(X, Y);
    MainPaintBox.Invalidate;
  end;
end;

procedure TDrawForm.MainPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  if Button = mbLeft then
  begin
    IsDrawing := false;
    MainPaintBox.Invalidate;
    for i := Low(UndoHistory) to High(UndoHistory) do
      FreeAndNil(UndoHistory[i]);
    SetLength(UndoHistory, 0);
  end;
end;

procedure TDrawForm.MainPaintBoxPaint(Sender: TObject);
var
  i: TFigure;
begin
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

