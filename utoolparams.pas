unit UToolParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, Dialogs, Spin, UAppState,
  StdCtrls;

type

  { TToolParam }

  TToolParam = class(TPersistent)
  private
    FName: string;
    AttachedParams: array of TToolParam;
    procedure OnChangeControl(Sender: TObject); virtual; abstract;
  public
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; virtual; abstract;
    procedure AttachParam(AParam: TToolParam);
    procedure UnattachAll;
  published
    property Name: string read FName write FName;
  end;
  TToolParamClass = class of TToolParam;
  TToolParamList = array of TToolParam;
  TToolParamClassList = array of TPersistentClass;

  { TIntegerParam }

  TIntegerParam = class(TToolParam)
  private
    FMinValue, FMaxValue: integer;
    FValue: integer;
    procedure FSetValue(AValue: integer);
    procedure OnChangeControl(Sender: TObject); override;
  public
    constructor Create(AParamName: string;
      AMinValue, AMaxValue, ADefaultValue: integer);
    function ToControl(AParentPanel: TPanel): TControl; override;
    function Clone: TIntegerParam;
  published
    property Value: integer read FValue write FSetValue;
    property MinValue: integer read FMinValue write FMinValue;
    property MaxValue: integer read FMaxValue write FMaxValue;
  end;

  { TColorParam }

  TColorParam = class(TToolParam)
  private
    FValue: TColor;
    procedure OnChangeControl(Sender: TObject); override;
  public
    constructor Create(AParamName: string; ADefaultValue: TColor);
    function ToControl(AParentPanel: TPanel): TControl; override;
    function Clone: TColorParam;
  published
    property Value: TColor read FValue write FValue;
  end;

  { TLineStyleParam }

  TLineStyleParam = class(TToolParam)
  private
    FLineIndex: integer;
  const
    FLineStyles: array[0..5] of TPenStyle = (psSolid, psClear, psDot,
      psDash, psDashDot, psDashDotDot);

    function FGetLineStyle: TPenStyle;
    procedure SetLineStyle(ALineStyle: TPenStyle);
    procedure OnChangeControl(Sender: TObject); override;
    procedure FDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; override;
    function Clone: TLineStyleParam;
  published
    property Value: TPenStyle read FGetLineStyle write SetLineStyle;
  end;

  { TFillStyleParam }

  TFillStyleParam = class(TToolParam)
  private
    FFillIndex: integer;
  const
    FFillStyles: array[0..7] of TBrushStyle = (bsSolid, bsClear,
      bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);

    function FGetFillStyle: TBrushStyle;
    procedure SetFillStyle(AFillStyle: TBrushStyle);
    procedure OnChangeControl(Sender: TObject); override;
    procedure FDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    constructor Create;
    function ToControl(AParentPanel: TPanel): TControl; override;
    function Clone: TFillStyleParam;
  published
    property Value: TBrushStyle read FGetFillStyle write SetFillStyle;
  end;

implementation
uses UHistory;
{ TToolParam }

constructor TToolParam.Create;
begin
  //Dummy
end;

procedure TToolParam.AttachParam(AParam: TToolParam);
begin
  SetLength(AttachedParams, Length(AttachedParams) + 1);
  AttachedParams[High(AttachedParams)] := AParam;
end;

procedure TToolParam.UnattachAll;
begin
  SetLength(AttachedParams, 0);
end;

{ TColorParam }

procedure TColorParam.OnChangeControl(Sender: TObject);
var
  p: TToolParam;
begin
  FValue := (Sender as TColorButton).ButtonColor;
  if AttachedParams <> nil then
  begin
    for p in AttachedParams do
      (p as TColorParam).FValue := (Sender as TColorButton).ButtonColor;
    Modified := true;
    (Sender as TControl).GetTopParent.Invalidate;
    History.PushState;
  end;
end;

constructor TColorParam.Create(AParamName: string; ADefaultValue: TColor);
begin
  FName := AParamName;
  FValue := ADefaultValue;
end;

function TColorParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TColorButton.Create(AParentPanel);
  with Result as TColorButton do
  begin
    Parent := AParentPanel;
    ButtonColor := FValue;
    OnColorChanged := @OnChangeControl;
  end;
end;

function TColorParam.Clone: TColorParam;
begin
  Result := TColorParam.Create(Self.Name, Self.Value);
  Result.FName := Self.FName;
  Result.FValue := Self.FValue;
end;

{ TIntegerParam }

procedure TIntegerParam.FSetValue(AValue: integer);
begin
  if AValue < FMinValue then
    FValue := FMinValue
  else if AValue > FMaxValue then
    FValue := FMaxValue
  else
    FValue := AValue;
end;

procedure TIntegerParam.OnChangeControl(Sender: TObject);
var
  p: TToolParam;
begin
  FSetValue((Sender as TSpinEdit).Value);
  if AttachedParams <> nil then
  begin
    for p in AttachedParams do
      (p as TIntegerParam).FSetValue((Sender as TSpinEdit).Value);
    Modified := true;
    (Sender as TControl).GetTopParent.Invalidate;
    History.PushState;
  end;
end;

constructor TIntegerParam.Create(AParamName: string;
  AMinValue, AMaxValue, ADefaultValue: integer);
begin
  if AMinValue > AMaxValue then
    raise ERangeError.Create('Min > max');
  FName := AParamName;
  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
  FSetValue(ADefaultValue);
end;

function TIntegerParam.ToControl(AParentPanel: TPanel): TControl;
begin
    Result := TSpinEdit.Create(AParentPanel);
    with Result as TSpinEdit do
    begin
      Parent := AParentPanel;
      MinValue := FMinValue;
      MaxValue := FMaxValue;
      Value := FValue;
      OnChange := @OnChangeControl;
    end;
end;

function TIntegerParam.Clone: TIntegerParam;
begin
  Result := TIntegerParam.Create(Self.Name, FMinValue, FMaxValue, Self.Value);
  Result.FName := Self.FName;
  Result.FValue := Self.FValue;
end;

{ TFillStyleParam }

function TFillStyleParam.FGetFillStyle: TBrushStyle;
begin
  Result := FFillStyles[FFillIndex];
end;

procedure TFillStyleParam.SetFillStyle(AFillStyle: TBrushStyle);
var
  i: integer;
begin
  for i := Low(FFillStyles) to High(FFillStyles) do
  begin
    if AFillStyle = FFillStyles[i] then
    begin
      FFillIndex := i;
      exit;
    end;
  end;
  raise Exception.Create('Invalid fill style');
end;

procedure TFillStyleParam.OnChangeControl(Sender: TObject);
var
  p: TToolParam;
begin
  FFillIndex := (Sender as TComboBox).ItemIndex;
  if AttachedParams <> nil then
  begin
    for p in AttachedParams do
      (p as TFillStyleParam).FFillIndex := (Sender as TComboBox).ItemIndex;
    Modified := true;
    (Sender as TControl).GetTopParent.Invalidate;
    History.PushState;
  end;
end;

procedure TFillStyleParam.FDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    FillRect(ARect);
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Pen.Width := 1;
    Brush.Color := clRed;
    Brush.Style := FFillStyles[Index];
    Rectangle(ARect.Left + 2, ARect.Top + 2, ARect.Right - 2,
      ARect.Bottom - 2);
  end;
end;

constructor TFillStyleParam.Create;
begin
  FName := 'Fill style';
  FFillIndex := 0;
end;

function TFillStyleParam.ToControl(AParentPanel: TPanel): TControl;
var
  i: TBrushStyle;
  s: string;
begin
  Result := TComboBox.Create(AParentPanel);
  with Result as TComboBox do
  begin
    Parent := AParentPanel;
    Style := csOwnerDrawFixed;
    OnDrawItem := @FDrawItem;
    OnSelect := @OnChangeControl;
    for i in FFillStyles do
    begin
      WriteStr(s, i);
      Items.Add(s);
    end;
    ReadOnly := True;
    ItemIndex := FFillIndex;
  end;
end;

function TFillStyleParam.Clone: TFillStyleParam;
begin
  Result := TFillStyleParam.Create;
  Result.FName := Self.FName;
  Result.FFillIndex := Self.FFillIndex;
end;

{ TLineStyleParam }

function TLineStyleParam.FGetLineStyle: TPenStyle;
begin
  Result := FLineStyles[FLineIndex];
end;

procedure TLineStyleParam.SetLineStyle(ALineStyle: TPenStyle);
var
  i: integer;
begin
  for i := Low(FLineStyles) to High(FLineStyles) do
  begin
    if ALineStyle = FLineStyles[i] then
    begin
      FLineIndex := i;
      exit;
    end;
  end;
  raise Exception.Create('Invalid line style');
end;

procedure TLineStyleParam.OnChangeControl(Sender: TObject);
var
  p: TToolParam;
begin
  FLineIndex := (Sender as TComboBox).ItemIndex;
  if AttachedParams <> nil then
  begin
    for p in AttachedParams do
      (p as TLineStyleParam).FLineIndex := (Sender as TComboBox).ItemIndex;
    Modified := true;
    (Sender as TControl).GetTopParent.Invalidate;
    History.PushState;
  end;
end;

procedure TLineStyleParam.FDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    FillRect(ARect);
    Pen.Color := clBlack;
    Pen.Style := FLineStyles[Index];
    Pen.Width := 1;
    Line(ARect.Left + 1, (ARect.Top + ARect.Bottom) div 2, ARect.Right - 1,
      (ARect.Top + ARect.Bottom) div 2);
  end;
end;

constructor TLineStyleParam.Create;
begin
  FName := 'Line style';
  FLineIndex := 0;
end;

function TLineStyleParam.ToControl(AParentPanel: TPanel): TControl;
var
  i: TPenStyle;
  s: string;
begin
  Result := TComboBox.Create(AParentPanel);
  with Result as TComboBox do
  begin
    Parent := AParentPanel;
    Style := csOwnerDrawFixed;
    OnDrawItem := @FDrawItem;
    OnSelect := @OnChangeControl;
    for i in FLineStyles do
    begin
      WriteStr(s, i);
      Items.Add(s);
    end;
    ReadOnly := True;
    ItemIndex := FLineIndex;
  end;
end;

function TLineStyleParam.Clone: TLineStyleParam;
begin
  Result := TLineStyleParam.Create;
  Result.FName := Self.FName;
  Result.FLineIndex := Self.FLineIndex;
end;

initialization
RegisterClasses(TToolParamClassList.Create(TIntegerParam, TColorParam,
  TLineStyleParam, TFillStyleParam));

end.
