unit UDrawBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Math,
  ExtCtrls, Spin, ActnList, Buttons, StdCtrls, UAbout, UTools, UFigures, Types,
  UTransform, UToolParams;

type

  { TDrawForm }

  TDrawForm = class(TForm)
    CanvasPropsPanel: TPanel;
    PercentSignLabel: TLabel;
    ToolParamsPanel: TPanel;
    ScaleFloatSpin: TFloatSpinEdit;
    ScaleLabel: TLabel;
    DelimeterMenuItem: TMenuItem;
    ClearAllMenuItem: TMenuItem;
    HorizontalScrollBar: TScrollBar;
    ShowAllMenuItem: TMenuItem;
    VerticalScrollBar: TScrollBar;
    ToolsPanel: TPanel;
    RedoAction: TAction;
    UndoAction: TAction;
    EditorActionList: TActionList;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    EditMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    UndoMenuItem: TMenuItem;
    MainPaintBox: TPaintBox;
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

const
  TOOL_BUTTON_SIZE: integer = 32;
  TOOL_BUTTON_MARGIN: integer = 5;
  TOOL_BUTTON_PADDING: integer = 1;
  CANVAS_OFFSET_BORDER_SIZE: integer = 10;

var
  ToolsBase: TToolList;
  DrawForm: TDrawForm;
  IsDrawing: boolean;
  CurrentTool: TTool;
  CanvasItems, UndoHistory: TFigureList;
  WorldTopLeft, WorldBottomRight: TDoublePoint;

implementation

{$R *.lfm}

{ TDrawForm }

procedure TDrawForm.FormCreate(Sender: TObject);
var
  b: TSpeedButton;
  i: integer;
  t: TToolClass;
  CurrentIcon: TPicture;
  IconsPerRow: integer;
begin
  DrawForm.DoubleBuffered := true;
  DrawForm.Caption := ApplicationName;
  IsDrawing := false;
  IconsPerRow := ToolsPanel.Width div
    (TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN + TOOL_BUTTON_PADDING);

  for t in ToolClassBase do
  begin
    SetLength(ToolsBase, Length(ToolsBase) + 1);
    ToolsBase[High(ToolsBase)] := t.Create;
  end;

  for i:=Low(ToolsBase) to High(ToolsBase) do
  begin
    b := TSpeedButton.Create(ToolsPanel);
    b.Parent := ToolsPanel;
    b.Name := 'ToolButton' + ToolsBase[i].ClassName;
    b.GroupIndex := 1;
    b.Tag := i;
    b.OnClick := @ToolButtonClick;

    CurrentIcon := TPicture.Create;
    CurrentIcon.LoadFromFile('./icons/' + ToolsBase[i].ClassName + '.png');
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

  ScaleFloatSpin.MinValue := ZOOM_MIN * 100;
  ScaleFloatSpin.MaxValue := ZOOM_MAX * 100;

  CanvasWidth := MainPaintBox.Width;
  CanvasHeight := MainPaintBox.Height;

  WorldTopLeft := DoublePoint(0, 0);
  WorldBottomRight := DoublePoint(0, 0);

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
begin
  if (WorldTopLeft.x <> WorldBottomRight.x) and
    (WorldTopLeft.y <> WorldBottomRight.y) then
  begin
    NewScale := Min(CanvasWidth / (WorldBottomRight.x - WorldTopLeft.x
      + 2 * CANVAS_OFFSET_BORDER_SIZE), CanvasHeight / (WorldBottomRight.y
      - WorldTopLeft.y + 2 * CANVAS_OFFSET_BORDER_SIZE));
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

  HorizontalScrollBar.PageSize := Round((CanvasCorner.x-CanvasOffset.x)
    / (HorMax-HorMin));
  VerticalScrollBar.PageSize := Round((CanvasCorner.y-CanvasOffset.y)
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
  i: TToolParam;
  l: TLabel;
  c: TControl;
  ParamsList: TToolParamList;
begin
  b := Sender as TSpeedButton;
  CurrentTool := ToolsBase[b.Tag];
  b.Down := true;

  ToolParamsPanel.DestroyComponents;
  ParamsList := CurrentTool.GetParams;
  if ParamsList <> nil then
  begin
    for i in ParamsList do
    begin
      ToolParamsPanel.Visible := false;
      c := i.ToControl(ToolParamsPanel);
      c.Align := alBottom;

      l := TLabel.Create(ToolParamsPanel);
      l.Parent := ToolParamsPanel;
      l.Caption := i.Name;
      l.Align := alBottom;
      ToolParamsPanel.Visible := true;
    end;
  end;
end;

procedure TDrawForm.MainPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsDrawing := true;
  SetLength(CanvasItems, Length(CanvasItems) + 1);
  CanvasItems[High(CanvasItems)] := CurrentTool.MouseDown(X, Y, Button);
  if CanvasItems[High(CanvasItems)] = nil then
  begin
    SetLength(CanvasItems, Length(CanvasItems) - 1);
    IsDrawing := false;
  end;
  MainPaintBox.Invalidate;
  SetScrollBars;
end;

procedure TDrawForm.MainPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  CurrentTool.MouseMove(X, Y);
  MainPaintBox.Invalidate;
  SetScrollBars;
end;

procedure TDrawForm.MainPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  CurrentTool.MouseUp(X, Y);
  if CurrentTool.FigureDestroyed then
  begin
    FreeAndNil(CanvasItems[High(CanvasItems)]);
    SetLength(CanvasItems, Max(Length(CanvasItems) - 1, 0));
    IsDrawing := false;
  end;
  if IsDrawing then
  begin
    IsDrawing := false;
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
var
  i: integer;
begin
  ClearAllMenuItemClick(nil);
  for i := Low(ToolsBase) to High(ToolsBase) do
    FreeAndNil(ToolsBase[i]);
  SetLength(ToolsBase, 0);
  ToolParamsPanel.DestroyComponents;
end;

end.

