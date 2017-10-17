unit UDrawBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Spin, ActnList, Buttons, StdCtrls, UAbout, UFigures, UFiguresBase,
  strutils;

type

  { TDrawForm }

  TDrawForm = class(TForm)
    ChangeFillColorButton: TColorButton;
    FillColorLabel: TLabel;
    LineWidthLabel: TLabel;
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

const
  START_LINE_COLOR = clBlack;
  START_FILL_COLOR = clWhite;
  START_WIDTH = 1;

var
  DrawForm: TDrawForm;
  IsDrawing: boolean;
  CurrentFigure: TFigureClass;
  CanvasItems, UndoHistory: array of TFigure;
  CurrentLineColor, CurrentFillColor: TColor;
  CurrentLineWidth: integer;

implementation

{$R *.lfm}

{ TDrawForm }

procedure TDrawForm.FormCreate(Sender: TObject);
var
  b: TSpeedButton;
  i: integer;
  icn: TPicture;
  ipr: integer; //icons per row
begin
  DrawForm.Caption := ApplicationName;
  isDrawing := false;

  CurrentLineColor := START_LINE_COLOR;
  ChangeLineColorButton.ButtonColor := CurrentLineColor;

  CurrentFillColor := START_FILL_COLOR;
  ChangeFillColorButton.ButtonColor := CurrentFillColor;

  CurrentLineWidth := START_WIDTH;
  PenWidthSpinEdit.Value := CurrentLineWidth;

  ipr := ToolsPanel.Width div (32+2+1);

  for i:=Low(FiguresBase) to High(FiguresBase) do
  begin
    b := TSpeedButton.Create(ToolsPanel);
    b.Parent := ToolsPanel;
    b.Name := 'ToolButton' + FiguresBase[i].ClassName;
    b.Tag := i;
    b.OnClick := @ToolButtonClick;

    icn := TPicture.Create;
    icn.LoadFromFile(FiguresBase[i].ClassName + '.png');
    b.Glyph := icn.Bitmap;
    icn.Free;

    b.Left := 1 + (i mod ipr)*(32+2); //buttons are always 32x32(+2px margin and 1px padding)
    b.Top := 1 + (i div ipr)*(32+2);
    b.Width := (32+2);
    b.Height := (32+2);
  end;

  CurrentFigure := FiguresBase[0];
end;

procedure TDrawForm.ToolButtonClick(Sender: TObject);
begin
  CurrentFigure := FiguresBase[(Sender as TSpeedButton).Tag];
end;

procedure TDrawForm.MainPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    IsDrawing := true;
    SetLength(CanvasItems, Length(CanvasItems) + 1);
    CanvasItems[High(CanvasItems)] := CurrentFigure.Create(X, Y, CurrentLineColor,
      CurrentLineWidth, CurrentFillColor);
  end;
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.MainPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
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
    Pen.Color := clWhite;
    Brush.Color := clWhite;
    Rectangle(0, 0, MainPaintBox.Width, MainPaintBox.Height);
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

procedure TDrawForm.AboutMenuItemClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TDrawForm.ExitMenuItemClick(Sender: TObject);
begin
  DrawForm.Close;
end;

end.

