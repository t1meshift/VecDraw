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
    DeleteSelectedMenuItem: TMenuItem;
    MoveOnTopMenuItem: TMenuItem;
    MoveOnBottomMenuItem: TMenuItem;
    SelectAllMenuItem: TMenuItem;
    RemoveSelectionMenuItem: TMenuItem;
    PercentSignLabel: TLabel;
    ToolParamsPanel: TPanel;
    ScaleFloatSpin: TFloatSpinEdit;
    ScaleLabel: TLabel;
    VisibilitySeparator: TMenuItem;
    ClearAllMenuItem: TMenuItem;
    HorizontalScrollBar: TScrollBar;
    ShowAllMenuItem: TMenuItem;
    VerticalScrollBar: TScrollBar;
    ToolsPanel: TPanel;
    EditorActionList: TActionList;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    EditMenuItem: TMenuItem;
    MainPaintBox: TPaintBox;
    procedure ClearAllMenuItemClick(Sender: TObject);
    procedure DeleteSelectedMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MainPaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MoveOnBottomMenuItemClick(Sender: TObject);
    procedure MoveOnTopMenuItemClick(Sender: TObject);
    procedure RemoveSelectionMenuItemClick(Sender: TObject);
    procedure SelectAllMenuItemClick(Sender: TObject);
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
    procedure AboutMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

  private
    { private declarations }
  public
    { public declarations }
  end;

procedure CalculateWorldBorders;

const
  TOOL_BUTTON_SIZE: integer = 32;
  TOOL_BUTTON_MARGIN: integer = 5;
  TOOL_BUTTON_PADDING: integer = 1;
  CANVAS_OFFSET_BORDER_SIZE: integer = 10;

var
  DrawForm: TDrawForm;
  IsDrawing: boolean;
  CurrentTool: TTool;
  WorldTopLeft, WorldBottomRight: TDoublePoint;

implementation

procedure CalculateWorldBorders;
var
  i: TFigure;
begin
  if Length(CanvasItems) < 1 then
  begin
    if (CurrentTool <> nil) and (CurrentTool.Figure <> nil) then
    begin
      WorldTopLeft := CurrentTool.Figure.TopLeftBorder;
      WorldBottomRight := CurrentTool.Figure.BottomRightBorder;
    end
    else
    begin
      WorldTopLeft := DoublePoint(0, 0);
      WorldBottomRight := WorldTopLeft;
    end;
    exit;
  end;

  WorldTopLeft := CanvasItems[0].TopLeftBorder;
  WorldBottomRight := CanvasItems[0].BottomRightBorder;
  for i in CanvasItems do
  begin
    WorldTopLeft.x := Min(WorldTopLeft.x, i.TopLeftBorder.x);
    WorldTopLeft.y := Min(WorldTopLeft.y, i.TopLeftBorder.y);
    WorldBottomRight.x := Max(WorldBottomRight.x, i.BottomRightBorder.x);
    WorldBottomRight.y := Max(WorldBottomRight.y, i.BottomRightBorder.y);
  end;
  if CurrentTool.Figure <> nil then
    with CurrentTool.Figure do
    begin
      WorldTopLeft.x := Min(WorldTopLeft.x, TopLeftBorder.x);
      WorldTopLeft.y := Min(WorldTopLeft.y, TopLeftBorder.y);
      WorldBottomRight.x := Max(WorldBottomRight.x, BottomRightBorder.x);
      WorldBottomRight.y := Max(WorldBottomRight.y, BottomRightBorder.y);
    end;
end;

{$R *.lfm}

{ TDrawForm }

procedure TDrawForm.FormCreate(Sender: TObject);
var
  b: TSpeedButton;
  i: integer;
  CurrentIcon: TPicture;
  IconsPerRow: integer;
begin
  DrawForm.DoubleBuffered := true;
  DrawForm.Caption := ApplicationName;
  IsDrawing := false;
  IconsPerRow := ToolsPanel.Width div
    (TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN + TOOL_BUTTON_PADDING);

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

procedure TDrawForm.MoveOnBottomMenuItemClick(Sender: TObject);
begin
  MoveSelectedOnBottom;
  Invalidate;
end;

procedure TDrawForm.MoveOnTopMenuItemClick(Sender: TObject);
begin
  MoveSelectedOnTop;
  Invalidate;
end;

procedure TDrawForm.RemoveSelectionMenuItemClick(Sender: TObject);
begin
  RemoveSelection;
  Invalidate;
end;

procedure TDrawForm.SelectAllMenuItemClick(Sender: TObject);
begin
  SelectAll;
  Invalidate;
end;

procedure TDrawForm.ShowAllMenuItemClick(Sender: TObject);
var
  NewScale: double;
begin
  CalculateWorldBorders;
  if (WorldTopLeft.x <> WorldBottomRight.x) and
    (WorldTopLeft.y <> WorldBottomRight.y) then
  begin
    NewScale := Min(CanvasWidth / (WorldBottomRight.x - WorldTopLeft.x + 2),
      CanvasHeight / (WorldBottomRight.y - WorldTopLeft.y + 2));
    SetScale(NewScale);
    CenterToPoint(DoublePoint((WorldBottomRight.x + WorldTopLeft.x)/2,
      (WorldBottomRight.y + WorldTopLeft.y)/2));
    SetScrollBars;
    MainPaintBox.Invalidate;
  end;
end;

procedure TDrawForm.SetScrollBars;
var
  HorMin, HorMax, VerMin, VerMax: integer;
  CanvasCorner: TDoublePoint;
begin
  CanvasCorner := CanvasToWorld(CanvasWidth, CanvasHeight);

  HorMin := Round(Min(CanvasOffset.x - CANVAS_OFFSET_BORDER_SIZE / Scale,
    -CANVAS_OFFSET_BORDER_SIZE / Scale));
  HorMax := Round(Max(CanvasCorner.x + CANVAS_OFFSET_BORDER_SIZE / Scale,
    CanvasWidth + CANVAS_OFFSET_BORDER_SIZE / Scale));
  VerMin := Round(Min(CanvasOffset.y - CANVAS_OFFSET_BORDER_SIZE / Scale,
    -CANVAS_OFFSET_BORDER_SIZE / Scale));
  VerMax := Round(Max(CanvasCorner.y + CANVAS_OFFSET_BORDER_SIZE / Scale,
    CanvasHeight + CANVAS_OFFSET_BORDER_SIZE / Scale));

  CalculateWorldBorders;
  HorMin := Min(HorMin, Round(WorldTopLeft.x - CANVAS_OFFSET_BORDER_SIZE
    / Scale));
  VerMin := Min(VerMin, Round(WorldTopLeft.y - CANVAS_OFFSET_BORDER_SIZE
    / Scale));
  HorMax := Max(HorMax, Round(WorldBottomRight.x + CANVAS_OFFSET_BORDER_SIZE
    / Scale));
  VerMax := Max(VerMax, Round(WorldBottomRight.y + CANVAS_OFFSET_BORDER_SIZE
    / Scale));

  HorizontalScrollBar.SetParams(Round(CanvasOffset.x), HorMin, HorMax,
    Round(CanvasWidth / Scale));
  VerticalScrollBar.SetParams(Round(CanvasOffset.y), VerMin, VerMax,
    Round(CanvasHeight / Scale));
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
  CurrParam: TToolParam;
  i: integer;
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
    for i := High(ParamsList) downto Low(ParamsList) do
    begin
      CurrParam := ParamsList[i];
      ToolParamsPanel.Visible := false;
      c := CurrParam.ToControl(ToolParamsPanel);
      c.Align := alBottom;

      l := TLabel.Create(ToolParamsPanel);
      l.Parent := ToolParamsPanel;
      l.Caption := CurrParam.Name;
      l.Align := alBottom;
      ToolParamsPanel.Visible := true;
    end;
  end;
end;

procedure TDrawForm.MainPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsDrawing := true;
  CurrentTool.MouseDown(X, Y, Button);
  if CurrentTool.Figure = nil then
    IsDrawing := false;

  MainPaintBox.Invalidate;
  SetScrollBars;
end;

procedure TDrawForm.MainPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  CurrentTool.MouseMove(X, Y, Shift);
  MainPaintBox.Invalidate;
  SetScrollBars;
end;

procedure TDrawForm.MainPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SetLength(CanvasItems, Length(CanvasItems) + 1);
  CanvasItems[High(CanvasItems)] := CurrentTool.MouseUp(X, Y);
  if CanvasItems[High(CanvasItems)] = nil then
  begin
    SetLength(CanvasItems, Max(Length(CanvasItems) - 1, 0));
    IsDrawing := false;
  end;
  if IsDrawing then
  begin
    IsDrawing := false;
  end;
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.MainPaintBoxPaint(Sender: TObject);
var
  i: TFigure;
  WorldZeroTL, WorldZeroBR: TPoint;
begin
  ScaleFloatSpin.Value := Scale * 100;
  with MainPaintBox.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(0, 0, MainPaintBox.Width, MainPaintBox.Height);
    {$ifdef DEBUG}
    //show start field position
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Pen.Width := 1;
    WorldZeroTL := WorldToCanvas(0,0);
    WorldZeroBR := WorldToCanvas(CanvasWidth, CanvasHeight);
    Rectangle(WorldZeroTL.x, WorldZeroTL.y, WorldZeroBR.x, WorldZeroBR.y);
    {$endif}
  end;
  CalculateWorldBorders;
  for i in CanvasItems do
    i.Draw(MainPaintBox.Canvas);
  DrawSelection(MainPaintBox.Canvas);
  if (CurrentTool <> nil) and (CurrentTool.Figure <> nil) then
    CurrentTool.Figure.Draw(MainPaintBox.Canvas);
end;

procedure TDrawForm.ClearAllMenuItemClick(Sender: TObject);
var
  i: integer;
begin
  for i := Low(CanvasItems) to High(CanvasItems) do
    FreeAndNil(CanvasItems[i]);
  SetLength(CanvasItems, 0);
  MainPaintBox.Invalidate;
  SetScrollBars;
end;

procedure TDrawForm.DeleteSelectedMenuItemClick(Sender: TObject);
begin
  DeleteSelected;
  SetScrollBars;
  Invalidate;
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
  CurrentTool := nil;
  for i := Low(ToolsBase) to High(ToolsBase) do
    if ToolsBase[i] <> nil then
      FreeAndNil(ToolsBase[i]);
  SetLength(ToolsBase, 0);

  ClearAllMenuItemClick(nil);
  ToolParamsPanel.DestroyComponents;
end;

end.

