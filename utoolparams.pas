unit UToolParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, Dialogs, Spin,
  StdCtrls;
{
 TODO:
 - class for int values (SpinEdit)
 - class for color values (TColorButton)
 - class for selectable values (ComboBox)
 - class for booleans (CheckBox)
}
type
  TToolParam = class
    private
      FName: string;
      procedure OnChangeControl(Sender: TObject); virtual; abstract;
    public
      //Disabled: boolean; //TODO
      property Name: string read FName;
      function ToControl(AParentPanel: TPanel): TControl; virtual; abstract;
  end;
  TToolParamList = array of TToolParam;

  { TLineColorParam }

  TLineColorParam = class(TToolParam)
    private
      FLineColor: TColor;
      procedure OnChangeControl(Sender: TObject); override;
    public
      property Value: TColor read FLineColor;
      constructor Create;
      function ToControl(AParentPanel: TPanel): TControl; override;
  end;

  { TLineWidthParam }

  TLineWidthParam = class(TToolParam)
    private
      FLineWidth: integer;
      procedure FSetWidth(ALineWidth: integer);
      procedure OnChangeControl(Sender: TObject); override;
    public
      property Value: integer read FLineWidth write FSetWidth;
      constructor Create;
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

  { TFillColorParam }

  TFillColorParam = class(TToolParam)
    private
      FFillColor: TColor;
      procedure OnChangeControl(Sender: TObject); override;
    public
      property Value: TColor read FFillColor;
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

  { TRoundRadiusParam }

  TRoundRadiusParam = class(TToolParam)
    private
      FRadius: integer;
      procedure FSetRadius(ARadius: integer);
      procedure OnChangeControl(Sender: TObject); override;
    public
      property Value: integer read FRadius write FSetRadius;
      constructor Create;
      function ToControl(AParentPanel: TPanel): TControl; override;
  end;

  { TRoundRadiusXParam }

  TRoundRadiusXParam = class(TRoundRadiusParam)
    constructor Create;
  end;

  { TRoundRadiusYParam }

  TRoundRadiusYParam = class(TRoundRadiusParam)
    constructor Create;
  end;

  { TVertexCountParam }

  TVertexCountParam = class(TToolParam)
    private
      FVertexCount: integer;
      procedure FSetVertexCount(AVertexCount: integer);
      procedure OnChangeControl(Sender: TObject); override;
    public
      property Value: integer read FVertexCount write FSetVertexCount;
      constructor Create;
      function ToControl(AParentPanel: TPanel): TControl; override;
  end;

implementation

{ TVertexCountParam }

procedure TVertexCountParam.FSetVertexCount(AVertexCount: integer);
begin
  if AVertexCount < 3 then
    FVertexCount := 3
  else if AVertexCount > 50 then
    FVertexCount := 50
  else
    FVertexCount := AVertexCount;
end;

procedure TVertexCountParam.OnChangeControl(Sender: TObject);
begin
  FSetVertexCount((Sender as TSpinEdit).Value);
end;

constructor TVertexCountParam.Create;
begin
  FName := 'Vertex count';
  FVertexCount := 3;
end;

function TVertexCountParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TSpinEdit.Create(AParentPanel);
  with Result as TSpinEdit do
  begin
    Parent := AParentPanel;
    MinValue := 3;
    MaxValue := 50;
    Value := Self.Value;
    OnChange := @OnChangeControl;
  end;
end;

{ TRoundRadiusYParam }

constructor TRoundRadiusYParam.Create;
begin
  inherited Create;
  FName := FName + ' (Y)';
end;

{ TRoundRadiusXParam }

constructor TRoundRadiusXParam.Create;
begin
  inherited Create;
  FName := FName + ' (X)';
end;

{ TRoundRadiusParam }

procedure TRoundRadiusParam.FSetRadius(ARadius: integer);
begin
  if ARadius < 0 then
    FRadius := 0
  else if ARadius > 100 then
    FRadius := 100
  else
    FRadius := ARadius;
end;

procedure TRoundRadiusParam.OnChangeControl(Sender: TObject);
begin
  FSetRadius((Sender as TSpinEdit).Value);
end;

constructor TRoundRadiusParam.Create;
begin
  FName := 'Rounding radius';
  FRadius := 5;
end;

function TRoundRadiusParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TSpinEdit.Create(AParentPanel);
  with Result as TSpinEdit do
  begin
    Parent := AParentPanel;
    MinValue := 0;
    MaxValue := 50;
    Value := Self.Value;
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
    OnChange := @OnChangeControl;
    for i in FFillStyles do
    begin
      WriteStr(s, i);
      Items.Add(s);
    end;
    ReadOnly := true;
    ItemIndex := FFillIndex;
  end;
end;

{ TFillColorParam }

procedure TFillColorParam.OnChangeControl(Sender: TObject);
begin
  FFillColor := (Sender as TColorButton).ButtonColor;
end;

constructor TFillColorParam.Create;
begin
  FName := 'Fill color';
  FFillColor := clWhite;
end;

function TFillColorParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TColorButton.Create(AParentPanel);
  with Result as TColorButton do
  begin
    Parent := AParentPanel;
    ButtonColor := FFillColor;
    OnColorChanged := @OnChangeControl;
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
    OnChange := @OnChangeControl;
    for i in FLineStyles do
    begin
      WriteStr(s, i);
      Items.Add(s);
    end;
    ReadOnly := true;
    ItemIndex := FLineIndex;
  end;
end;

{ TLineWidthParam }

procedure TLineWidthParam.FSetWidth(ALineWidth: integer);
begin
  if ALineWidth < 1 then
    FLineWidth := 1
  else if ALineWidth > 100 then
    FLineWidth := 100
  else
    FLineWidth := ALineWidth;
end;

procedure TLineWidthParam.OnChangeControl(Sender: TObject);
begin
  FSetWidth((Sender as TSpinEdit).Value);
end;

constructor TLineWidthParam.Create;
begin
  FName := 'Line width';
  FLineWidth := 1;
end;

function TLineWidthParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TSpinEdit.Create(AParentPanel);
  with Result as TSpinEdit do
  begin
    Parent := AParentPanel;
    MinValue := 1;
    MaxValue := 100;
    Value := Self.Value;
    OnChange := @OnChangeControl;
  end;
end;

{ TLineColorParam }

procedure TLineColorParam.OnChangeControl(Sender: TObject);
begin
  FLineColor := (Sender as TColorButton).ButtonColor;
end;

constructor TLineColorParam.Create;
begin
  FName := 'Line color';
  FLineColor := clBlack;
end;

function TLineColorParam.ToControl(AParentPanel: TPanel): TControl;
begin
  Result := TColorButton.Create(AParentPanel);
  with Result as TColorButton do
  begin
    Parent := AParentPanel;
    ButtonColor := FLineColor;
    OnColorChanged := @OnChangeControl;
  end;
end;

end.

