unit UToolParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, Dialogs, Spin,
  StdCtrls;

type
  TToolParam = class
    private
      FName: string;
      procedure OnChangeControl(Sender: TObject); virtual; abstract;
    public
      property Name: string read FName;
      function ToControl(AParentPanel: TPanel): TControl; virtual; abstract;
  end;
  TToolParamList = array of TToolParam;

  { TIntegerParam }

  TIntegerParam = class(TToolParam)
    private
      FMinValue, FMaxValue: integer;
      FValue: integer;
      procedure FSetValue(AValue: integer);
      procedure OnChangeControl(Sender: TObject); override;
    public
      constructor Create(AParamName: string; AMinValue, AMaxValue,
        ADefaultValue: integer);
      property Value: integer read FValue write FSetValue;
      function ToControl(AParentPanel: TPanel): TControl; override;
  end;

  { TColorParam }

  TColorParam = class(TToolParam)
    private
      FValue: TColor;
      procedure OnChangeControl(Sender: TObject); override;
    public
      constructor Create(AParamName: string; ADefaultValue: TColor);
      property Value: TColor read FValue write FValue;
      function ToControl(AParentPanel: TPanel): TControl; override;
  end;

  { TBooleanParam }

  TBooleanParam = class(TToolParam)
    private
      FValue: boolean;
      procedure OnChangeControl(Sender: TObject); override;
    public
      constructor Create(AParamName: string; ADefaultValue: boolean);
      property Value: boolean read FValue write FValue;
      function ToControl(AParentPanel: TPanel): TControl; override;
  end;

  { TLineStyleParam }

  TLineStyleParam = class(TToolParam)
    private
      FLineIndex: integer;
      const FLineStyles: array[0..5] of TPenStyle = (psSolid, psClear, psDot,
        psDash, psDashDot, psDashDotDot);
      function FGetLineStyle: TPenStyle;
      procedure OnChangeControl(Sender: TObject); override;
      procedure FDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
        State: TOwnerDrawState);
    public
      property Value: TPenStyle read FGetLineStyle;
      constructor Create;
      function ToControl(AParentPanel: TPanel): TControl; override;
  end;

  { TFillStyleParam }

  TFillStyleParam = class(TToolParam)
    private
      FFillIndex: integer;
      const FFillStyles: array[0..7] of TBrushStyle = (bsSolid, bsClear,
      bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);
      function FGetFillStyle: TBrushStyle;
      procedure OnChangeControl(Sender: TObject); override;
      procedure FDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
        State: TOwnerDrawState);
    public
      property Value: TBrushStyle read FGetFillStyle;
      constructor Create;
      function ToControl(AParentPanel: TPanel): TControl; override;
  end;

implementation

{ TBooleanParam }

procedure TBooleanParam.OnChangeControl(Sender: TObject);
begin
  FValue := (Sender as TCheckBox).Checked;
end;

constructor TBooleanParam.Create(AParamName: string; ADefaultValue: boolean);
begin
  FName := AParamName;
  FValue := ADefaultValue;
end;

function TBooleanParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TCheckBox.Create(AParentPanel);
  with Result as TCheckBox do
  begin
    Parent := AParentPanel;
    Caption := FName;
    Checked := FValue;
    OnClick := @OnChangeControl;
  end;
end;

{ TColorParam }

procedure TColorParam.OnChangeControl(Sender: TObject);
begin
  FValue := (Sender as TColorButton).ButtonColor;
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
begin
  FSetValue((Sender as TSpinEdit).Value);
end;

constructor TIntegerParam.Create(AParamName: string; AMinValue, AMaxValue,
  ADefaultValue: integer);
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

{ TFillStyleParam }

function TFillStyleParam.FGetFillStyle: TBrushStyle;
begin
  Result := FFillStyles[FFillIndex];
end;

procedure TFillStyleParam.OnChangeControl(Sender: TObject);
begin
  FFillIndex := (Sender as TComboBox).ItemIndex;
end;

procedure TFillStyleParam.FDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    FillRect(ARect);
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Pen.Width := 1;
    Brush.Style := FFillStyles[Index];
    if Index <> 1 then
      Brush.Color := clBlack;
    Rectangle(ARect.Left + 1, ARect.Top + 1, ARect.Right - 1,
      ARect.Bottom - 1);
  end;
end;

constructor TFillStyleParam.Create;
begin
  FName := 'Fill style';
  FFillIndex := 1;
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
    ReadOnly := true;
    ItemIndex := FFillIndex;
  end;
end;

{ TLineStyleParam }

function TLineStyleParam.FGetLineStyle: TPenStyle;
begin
  Result := FLineStyles[FLineIndex];
end;

procedure TLineStyleParam.OnChangeControl(Sender: TObject);
begin
  FLineIndex := (Sender as TComboBox).ItemIndex;
end;

procedure TLineStyleParam.FDrawItem(Control: TWinControl; Index: Integer;
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
    ReadOnly := true;
    ItemIndex := FLineIndex;
  end;
end;

end.

