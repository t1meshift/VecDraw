unit UClipboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Clipbrd, UFileWorker, fpjson, UFigures, Graphics;

const
  { JSON string }
  CLIPBRD_FORMAT_NAME = 'VecDraw';

procedure CopySelected;
procedure PasteFromClipboard;

implementation

procedure CopySelected;
var
  cfVecDraw: TClipboardFormat;
  s: TStringStream;
  json: TJSONObject;
  pic: TBitmap;
begin
  json := GetFiguresJSON(true);
  Clipboard.Open;
  if json.Arrays[FILE_HEADER].Count > 0 then
  begin
    cfVecDraw := Clipboard.FindFormatID(CLIPBRD_FORMAT_NAME);
    if cfVecDraw = 0 then
      cfVecDraw := RegisterClipboardFormat(CLIPBRD_FORMAT_NAME);
    s := TStringStream.Create(json.AsJSON);
    pic := GetFiguresBitmap(true);
    Clipboard.Assign(pic);
    Clipboard.AddFormat(cfVecDraw, s);
    FreeAndNil(json);
    FreeAndNil(s);
    FreeAndNil(pic);
  end;
  Clipboard.Close;
end;

procedure PasteFromClipboard;
var
  cfVecDraw: TClipboardFormat;
  s: TStringStream;
  json: TJSONObject;
  Figures: TFigureList;
  Figure: TFigure;
  i: integer;
begin
  cfVecDraw := Clipboard.FindFormatID(CLIPBRD_FORMAT_NAME);
  if cfVecDraw = 0 then
    cfVecDraw := RegisterClipboardFormat(CLIPBRD_FORMAT_NAME);
  s := TStringStream.Create('');
  if Clipboard.GetFormat(cfVecDraw, s) then
  begin
    json := TJSONObject(GetJSON(s.DataString));
    Figures := GetFiguresFromJSON(json);
    for Figure in Figures do
      if Figure <> nil then
      begin
        SetLength(CanvasItems, Length(CanvasItems) + 1);
        CanvasItems[High(CanvasItems)] := Figure.Clone;
        CanvasItems[High(CanvasItems)].Selected := true;
      end;
    FreeAndNil(json);
    for i := Low(Figures) to High(Figures) do
      FreeAndNil(Figures[i]);
    FreeAndNil(s);
  end;
end;

end.

