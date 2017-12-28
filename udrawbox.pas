unit UDrawBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Math,
  ExtCtrls, Spin, ActnList, Buttons, StdCtrls, UAbout, UTools, UFigures, Types,
  UTransform, UToolParams, UFileWorker, LazFileUtils, FPimage, UHistory,
  UAppState, UClipboard;

type

  { TDrawForm }

  TDrawForm = class(TForm)
    DeleteSelectedAction: TAction;
    CutMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CopyPopupItem: TMenuItem;
    DeletePopupItem: TMenuItem;
    PopupDelimeter: TMenuItem;
    PastePopupItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    ClipboardSeparator: TMenuItem;
    CutPopupItem: TMenuItem;
    PasteAction: TAction;
    CutAction: TAction;
    CopyAction: TAction;
    CanvasPropsPanel: TPanel;
    DeleteSelectedMenuItem: TMenuItem;
    LoadMenuItem: TMenuItem;
    ExportToBitmapMenuItem: TMenuItem;
    ExportBitmapDialog: TSaveDialog;
    ClipboardPopupMenu: TPopupMenu;
    UndoRedoSeparator: TMenuItem;
    RedoMenuItem: TMenuItem;
    UndoMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    NewMenuItem: TMenuItem;
    OpenImgDialog: TOpenDialog;
    SaveImgDialog: TSaveDialog;
    SaveAsMenuItem: TMenuItem;
    MoveUpMenuItem: TMenuItem;
    MoveDownMenuItem: TMenuItem;
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
    procedure ClipboardPopupMenuPopup(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure CutActionExecute(Sender: TObject);
    procedure DeleteSelectedActionExecute(Sender: TObject);
    procedure EditMenuItemClick(Sender: TObject);
    procedure ExportToBitmapMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LoadMenuItemClick(Sender: TObject);
    procedure MainPaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure NewMenuItemClick(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure RedoMenuItemClick(Sender: TObject);
    procedure SaveAsMenuItemClick(Sender: TObject);
    procedure MoveDownMenuItemClick(Sender: TObject);
    procedure MoveUpMenuItemClick(Sender: TObject);
    procedure RemoveSelectionMenuItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SelectAllMenuItemClick(Sender: TObject);
    procedure ShowAllMenuItemClick(Sender: TObject);
    procedure SetScrollBars;
    procedure MainPaintBoxResize(Sender: TObject);
    procedure ScaleFloatSpinChange(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure UndoMenuItemClick(Sender: TObject);
    procedure UpdateParamsPanel;
    procedure MainPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MainPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure MainPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MainPaintBoxPaint(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);

  private
    { private declarations }
  public
    { public declarations }
  end;

procedure CalculateWorldBorders;
procedure ParseCmdArgs;

const
  TOOL_BUTTON_SIZE: integer = 32;
  TOOL_BUTTON_MARGIN: integer = 5;
  TOOL_BUTTON_PADDING: integer = 1;
  CANVAS_OFFSET_BORDER_SIZE: integer = 10;

var
  DrawForm: TDrawForm;
  IsDrawing: boolean;
  CurrentTool: TTool;
  ParamsList: TToolParamList;
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

procedure ParseCmdArgs;
begin
  if system.ParamCount > 0 then
    if FileIsReadable(System.ParamStr(1)) then
    begin
      LoadFile(System.ParamStr(1));
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
  ParseCmdArgs;
  DrawForm.DoubleBuffered := True;
  SetWindowTitle(DrawForm, CurrentFile);
  SaveImgDialog.FileName := CurrentFile;
  IsDrawing := False;
  IconsPerRow := ToolsPanel.Width div (TOOL_BUTTON_SIZE +
    TOOL_BUTTON_MARGIN + TOOL_BUTTON_PADDING);

  for i := Low(ToolsBase) to High(ToolsBase) do
  begin
    b := TSpeedButton.Create(ToolsPanel);
    b.Parent := ToolsPanel;
    b.Name := 'ToolButton' + ToolsBase[i].ClassName;
    b.GroupIndex := 1;
    b.Tag := i;
    b.OnClick := @ToolButtonClick;

    CurrentIcon := TPicture.Create;
    CurrentIcon.LoadFromFile(ExtractFilePath(Application.ExeName) +
      './icons/' + ToolsBase[i].ClassName + '.png');
    b.Glyph := CurrentIcon.Bitmap;
    CurrentIcon.Free;

    b.Left := (i mod IconsPerRow) * (TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN) +
      TOOL_BUTTON_PADDING;
    b.Top := (i div IconsPerRow) * (TOOL_BUTTON_SIZE + TOOL_BUTTON_MARGIN) +
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

  History.PushState;
end;

procedure TDrawForm.LoadMenuItemClick(Sender: TObject);
begin
  if OpenImgDialog.Execute then
  begin
    ClearAllMenuItemClick(nil);
    Modified := false;
    if LoadFile(OpenImgDialog.FileName)<>0 then
    begin
      Application.MessageBox('Loading error. Make sure the file is valid.',
        'Error');
      SetLength(CanvasItems, 0);
    end
    else
      SetWindowTitle(DrawForm, CurrentFile);
    History.Clear;
  end;
end;

procedure TDrawForm.MainPaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if WheelDelta > 0 then
    ZoomPoint(CanvasToWorld(MousePos), Scale * 2)
  else
    ZoomPoint(CanvasToWorld(MousePos), Scale / 2);
  SetScrollBars;
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.NewMenuItemClick(Sender: TObject);
begin
  ClearAllMenuItemClick(nil);
  CurrentFile := NEW_FILE_NAME;
  Modified := false;
  SetWindowTitle(DrawForm, CurrentFile);
end;

procedure TDrawForm.PasteActionExecute(Sender: TObject);
begin
  RemoveSelection;
  PasteFromClipboard;
  History.PushState;
  SetScrollBars;
  Invalidate;
end;

procedure TDrawForm.RedoMenuItemClick(Sender: TObject);
begin
  if History.CanRedo then
  begin
    History.Redo;
    UpdateParamsPanel;
    SetWindowTitle(DrawForm, CurrentFile);
    if not History.CanRedo then
      RedoMenuItem.Enabled := false;
  end
  else
    RedoMenuItem.Enabled := false;
  Invalidate;
end;

procedure TDrawForm.SaveAsMenuItemClick(Sender: TObject);
begin
  if SaveImgDialog.Execute then
  begin
    if SaveFile(SaveImgDialog.FileName)<>0 then
    begin
      Application.MessageBox('Saving error. Make sure the file isn''t locked.',
        'Error');
    end
    else
    begin
      Modified := false;
      History.RefreshSaveStates;
      SetWindowTitle(DrawForm, CurrentFile);
      Application.MessageBox('Successfully saved!', 'Info');
    end;
  end;
end;

procedure TDrawForm.MoveDownMenuItemClick(Sender: TObject);
begin
  MoveSelectedOnBottom;
  SetWindowTitle(DrawForm, CurrentFile);
  History.PushState;
  Invalidate;
end;

procedure TDrawForm.MoveUpMenuItemClick(Sender: TObject);
begin
  MoveSelectedOnTop;
  SetWindowTitle(DrawForm, CurrentFile);
  History.PushState;
  Invalidate;
end;

procedure TDrawForm.RemoveSelectionMenuItemClick(Sender: TObject);
begin
  RemoveSelection;
  UpdateParamsPanel;
  Invalidate;
end;

procedure TDrawForm.SaveMenuItemClick(Sender: TObject);
begin
  if CurrentFile = NEW_FILE_NAME then
  begin
    SaveAsMenuItemClick(Sender);
    exit;
  end;
  if SaveFile(CurrentFile) <> 0 then
    Application.MessageBox('Saving error. Make sure the file isn''t locked.',
      'Error')
  else
  begin
    Modified := false;
    History.RefreshSaveStates;
    SetWindowTitle(DrawForm, CurrentFile);
    Application.MessageBox('Successfully saved!', 'Info');
  end;
end;

procedure TDrawForm.SelectAllMenuItemClick(Sender: TObject);
begin
  SelectAll;
  UpdateParamsPanel;
  Invalidate;
end;

procedure TDrawForm.ShowAllMenuItemClick(Sender: TObject);
var
  NewScale: double;
begin
  CalculateWorldBorders;
  if (WorldTopLeft.x <> WorldBottomRight.x) and (WorldTopLeft.y <>
    WorldBottomRight.y) then
  begin
    NewScale := Min(CanvasWidth / (WorldBottomRight.x - WorldTopLeft.x + 2),
      CanvasHeight / (WorldBottomRight.y - WorldTopLeft.y + 2));
    SetScale(NewScale);
    CenterToPoint(DoublePoint((WorldBottomRight.x + WorldTopLeft.x) / 2,
      (WorldBottomRight.y + WorldTopLeft.y) / 2));
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

  HorMin := Round(Min(CanvasOffset.x - CANVAS_OFFSET_BORDER_SIZE /
    Scale, -CANVAS_OFFSET_BORDER_SIZE / Scale));
  HorMax := Round(Max(CanvasCorner.x + CANVAS_OFFSET_BORDER_SIZE /
    Scale, CanvasWidth + CANVAS_OFFSET_BORDER_SIZE / Scale));
  VerMin := Round(Min(CanvasOffset.y - CANVAS_OFFSET_BORDER_SIZE /
    Scale, -CANVAS_OFFSET_BORDER_SIZE / Scale));
  VerMax := Round(Max(CanvasCorner.y + CANVAS_OFFSET_BORDER_SIZE /
    Scale, CanvasHeight + CANVAS_OFFSET_BORDER_SIZE / Scale));

  CalculateWorldBorders;
  HorMin := Min(HorMin, Round(WorldTopLeft.x - CANVAS_OFFSET_BORDER_SIZE / Scale));
  VerMin := Min(VerMin, Round(WorldTopLeft.y - CANVAS_OFFSET_BORDER_SIZE / Scale));
  HorMax := Max(HorMax, Round(WorldBottomRight.x +
    CANVAS_OFFSET_BORDER_SIZE / Scale));
  VerMax := Max(VerMax, Round(WorldBottomRight.y +
    CANVAS_OFFSET_BORDER_SIZE / Scale));

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
begin
  b := Sender as TSpeedButton;
  CurrentTool := ToolsBase[b.Tag];
  b.Down := True;
  UpdateParamsPanel;
end;

procedure TDrawForm.UndoMenuItemClick(Sender: TObject);
begin
  if History.CanUndo then
  begin
    History.Undo;
    UpdateParamsPanel;
    SetWindowTitle(DrawForm, CurrentFile);
    if not History.CanUndo then
      UndoMenuItem.Enabled := false;
  end
  else
    UndoMenuItem.Enabled := false;
  Invalidate;
end;

procedure TDrawForm.UpdateParamsPanel;
var
  CurrParam: TToolParam;
  i: integer;
  l: TLabel;
  c: TControl;
begin
  ToolParamsPanel.DestroyComponents;

  ParamsList := CurrentTool.GetParams;
  if ParamsList = nil then
    ParamsList := GetSelectionParams;

  if ParamsList <> nil then
  begin
    for i := High(ParamsList) downto Low(ParamsList) do
    begin
      CurrParam := ParamsList[i];
      ToolParamsPanel.Visible := False;
      c := CurrParam.ToControl(ToolParamsPanel);
      c.Align := alBottom;

      l := TLabel.Create(ToolParamsPanel);
      l.Parent := ToolParamsPanel;
      l.Caption := CurrParam.Name;
      l.Align := alBottom;
      ToolParamsPanel.Visible := True;
    end;
  end;
end;

procedure TDrawForm.MainPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  IsDrawing := True;
  CurrentTool.MouseDown(X, Y, Button);
  if CurrentTool.Figure = nil then
    IsDrawing := False;
  if Button = mbMiddle then
    ClipboardPopupMenu.PopUp;
  MainPaintBox.Invalidate;
  SetScrollBars;
end;

procedure TDrawForm.MainPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  CurrentTool.MouseMove(X, Y, Shift);
  MainPaintBox.Invalidate;
  SetScrollBars;
end;

procedure TDrawForm.MainPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  SetLength(CanvasItems, Length(CanvasItems) + 1);
  CanvasItems[High(CanvasItems)] := CurrentTool.MouseUp(X, Y);
  if CanvasItems[High(CanvasItems)] = nil then
  begin
    SetLength(CanvasItems, Max(Length(CanvasItems) - 1, 0));
    IsDrawing := False;
  end;
  if IsDrawing then
  begin
    History.PushState;
    IsDrawing := False;
  end;
  SetWindowTitle(DrawForm, CurrentFile);
  UpdateParamsPanel;
  MainPaintBox.Invalidate;
end;

procedure TDrawForm.MainPaintBoxPaint(Sender: TObject);
var
  i: TFigure;
  WorldZeroTL, WorldZeroBR: TPoint;
begin
  ScaleFloatSpin.Value := Scale * 100;
  SetWindowTitle(DrawForm, CurrentFile);
  if History <> nil then
  begin
    UndoMenuItem.Enabled := History.CanUndo;
    RedoMenuItem.Enabled := History.CanRedo;
  end;
  with MainPaintBox.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(0, 0, MainPaintBox.Width, MainPaintBox.Height);
    {$ifdef DEBUG}
    //show start field position
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Pen.Width := 1;
    WorldZeroTL := WorldToCanvas(0, 0);
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
  SetScale(1); //100%
  CanvasOffset := DoublePoint(0,0);
  if Length(CanvasItems) > 0 then
    Modified := true;
  for i := Low(CanvasItems) to High(CanvasItems) do
    FreeAndNil(CanvasItems[i]);
  SetLength(CanvasItems, 0);
  SetWindowTitle(DrawForm, CurrentFile);
  History.PushState;
  UpdateParamsPanel;
  MainPaintBox.Invalidate;
  SetScrollBars;
end;

procedure TDrawForm.ClipboardPopupMenuPopup(Sender: TObject);
begin
  CutPopupItem.Enabled := HasSelection;
  CopyPopupItem.Enabled := HasSelection;
  DeletePopupItem.Enabled := HasSelection;
end;

procedure TDrawForm.CopyActionExecute(Sender: TObject);
begin
  CopySelected;
  Invalidate;
end;

procedure TDrawForm.CutActionExecute(Sender: TObject);
begin
  CopySelected;
  DeleteSelected;
  History.PushState;
  SetScrollBars;
  Invalidate;
end;

procedure TDrawForm.DeleteSelectedActionExecute(Sender: TObject);
begin
  DeleteSelected;
  History.PushState;
  SetScrollBars;
  UpdateParamsPanel;
  Invalidate;
end;

procedure TDrawForm.EditMenuItemClick(Sender: TObject);
begin
  CutMenuItem.Enabled := HasSelection;
  CopyMenuItem.Enabled := HasSelection;
  PasteMenuItem.Enabled := HasSelection;
  DeleteSelectedMenuItem.Enabled := HasSelection;
  MoveUpMenuItem.Enabled := HasSelection;
  MoveDownMenuItem.Enabled := HasSelection;
  RemoveSelectionMenuItem.Enabled := HasSelection;
end;

procedure TDrawForm.ExportToBitmapMenuItemClick(Sender: TObject);
var
  bmp: TBitmap;
  png: TPortableNetworkGraphic;
  i: TFigure;
begin
  if ExportBitmapDialog.Execute then
  begin
    try
      bmp := GetFiguresBitmap;
      case LowerCase(ExtractFileExt(ExportBitmapDialog.FileName)) of
        '.bmp':
        begin
          bmp.SaveToFile(ExportBitmapDialog.FileName);
        end;
        '.png':
        begin
          png := TPortableNetworkGraphic.Create;
          png.LoadFromIntfImage(bmp.CreateIntfImage);
          png.SaveToFile(ExportBitmapDialog.FileName);
          FreeAndNil(png);
        end
        else
        begin
          Application.MessageBox(PChar('Not yet implemented! ' +
            ExtractFileExt(ExportBitmapDialog.FileName)), 'Info');
        end;
      end;
      FreeAndNil(bmp);
      Application.MessageBox('Successfully exported!', 'Info');
    except
      on E: Exception do
      begin
        Application.MessageBox('Exporting error!', 'Error');
      end;
    end;
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

procedure TDrawForm.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
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
  ToolParamsPanel.DestroyComponents;
  CurrentTool := nil;
  for i := Low(ToolsBase) to High(ToolsBase) do
    if ToolsBase[i] <> nil then
      FreeAndNil(ToolsBase[i]);
  SetLength(ToolsBase, 0);
  FreeAndNil(History);
end;

end.

