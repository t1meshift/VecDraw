unit UDrawBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Spin, ActnList, Buttons, UAbout;

type

  { TDrawForm }

  TDrawForm = class(TForm)
    RedoAction: TAction;
    RedoButton: TSpeedButton;
    UndoButton: TSpeedButton;
    UndoAction: TAction;
    EditorActionList: TActionList;
    ChangeColorButton: TColorButton;
    DrawColorDialog: TColorDialog;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    EditMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    UndoMenuItem: TMenuItem;
    PaintBox: TPaintBox;
    PenWidthSpinEdit: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure DrawColorDialogClose(Sender: TObject);
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

  TPolyLine = record
    Vertexes: array of TPoint;
    Color: TColor;
    Width: integer;
  end;

const
  START_COLOR = clBlack;
  START_WIDTH = 1;

var
  DrawForm: TDrawForm;
  IsDrawing: boolean;
  CanvasItems, UndoHistory: array of TPolyLine;
  CurrentColor: TColor;
  CurrentWidth: integer;

implementation

{$R *.lfm}

{ TDrawForm }

procedure TDrawForm.FormCreate(Sender: TObject);
begin
  DrawForm.Caption := ApplicationName;
  isDrawing := false;
  CurrentColor := START_COLOR;
  ChangeColorButton.Color := CurrentColor;
  CurrentWidth := START_WIDTH;
  PenWidthSpinEdit.Value := CurrentWidth;
end;

procedure TDrawForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    IsDrawing := true;
    SetLength(CanvasItems, Length(CanvasItems) + 1);
  end;
end;

procedure TDrawForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if IsDrawing then
  begin
    with CanvasItems[High(CanvasItems)] do
    begin
      SetLength(Vertexes, Length(Vertexes) + 1);
      Vertexes[High(Vertexes)] := Point(X, Y);
      Color := CurrentColor;
      Width := CurrentWidth;
    end;
    PaintBox.Invalidate;
  end;
end;

procedure TDrawForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    IsDrawing := false;
    if Length(CanvasItems[High(CanvasItems)].Vertexes) = 0 then
      SetLength(CanvasItems, Length(CanvasItems) - 1);
    PaintBox.Invalidate;
  end;
end;

procedure TDrawForm.PaintBoxPaint(Sender: TObject);
var
  i: TPolyLine;
begin
  with PaintBox.Canvas do
  begin
    Pen.Color := clWhite;
    Brush.Color := clWhite;
    Rectangle(0, 0, PaintBox.Width, PaintBox.Height);
    for i in CanvasItems do
    begin
      Pen.Color := i.Color;
      Pen.Width := i.Width;
      Polyline(i.Vertexes);
    end;
  end;
end;

procedure TDrawForm.DrawColorDialogClose(Sender: TObject);
begin
  CurrentColor := DrawColorDialog.Color;
end;

procedure TDrawForm.PenWidthSpinEditChange(Sender: TObject);
begin
  CurrentWidth := PenWidthSpinEdit.Value;
end;

procedure TDrawForm.UndoActionExecute(Sender: TObject);
begin
  if Length(CanvasItems) > 0 then
  begin
    SetLength(UndoHistory, Length(UndoHistory) + 1);
    UndoHistory[High(UndoHistory)] := CanvasItems[High(CanvasItems)];
    SetLength(CanvasItems, Length(CanvasItems) - 1);
    PaintBox.Invalidate;
  end;
end;

procedure TDrawForm.RedoActionExecute(Sender: TObject);
begin
  if Length(UndoHistory) > 0 then
  begin
    SetLength(CanvasItems, Length(CanvasItems) + 1);
    CanvasItems[High(CanvasItems)] := UndoHistory[High(UndoHistory)];
    SetLength(UndoHistory, Length(UndoHistory) - 1);
    PaintBox.Invalidate;
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

