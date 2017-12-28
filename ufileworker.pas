unit UFileWorker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UFigures, UToolParams, UTransform, typinfo, fpjson,
  jsonparser;

const
  FILE_HEADER = 'VECDRAW';
  VERSION = 1;
  NEW_FILE_NAME = 'New Image.vd';

var
  CurrentFile: string;

function GetProps(AObject: TObject; out APropList: PPropList): integer;
function SaveFile(FileName: string): integer;
function LoadFile(FileName: string): integer;

implementation

function GetProps(AObject: TObject; out APropList: PPropList): integer;
var
  PropCount: Integer;
begin
  PropCount := GetPropList(AObject.ClassInfo, tkAny, Nil);
  GetMem(APropList, PropCount * SizeOf(PPropInfo));
  GetPropList(AObject.ClassInfo, tkAny, APropList);
  Result := PropCount;
end;

function SaveFile(FileName: string): integer;
var
  f: text;
  CurrentFigure: TFigure;
  Root, FigureJSON, CurrPropJSON: TJSONObject;
  FigureProps, PropProps: PPropList;
  i, j, PropCount, PropPropCount: integer;
  FiguresJSON, VertexesJSON, CurrVertexJSON: TJSONArray;
  PropInfo, PropPropInfo: TPropInfo;
  ParamProp: TToolParam;
  v: TDoublePoint;
begin
  Result := 0;
  try
    FiguresJSON := TJSONArray.Create;
    for CurrentFigure in CanvasItems do
    begin
      FigureJSON := TJSONObject.Create;
      FigureJSON.Add('type', CurrentFigure.ClassName);
      PropCount := GetProps(CurrentFigure, FigureProps);
      for i := 0 to PropCount-1 do
      begin
        PropInfo := FigureProps^[i]^;
        case PropInfo.PropType^.Kind of
          tkInt64, tkInteger:
          begin
            FigureJSON.Add(PropInfo.Name,
              GetInt64Prop(CurrentFigure, PropInfo.Name));
          end;
          tkFloat:
          begin
            FigureJSON.Add(PropInfo.Name,
              GetFloatProp(CurrentFigure, PropInfo.Name));
          end;
          else
          begin
            ParamProp := GetObjectProp(CurrentFigure, PropInfo.Name) as TToolParam;
            CurrPropJSON := TJSONObject.Create;
            CurrPropJSON.Add('class', ParamProp.ClassName);
            CurrPropJSON.Add('Name', ParamProp.Name);
            PropPropCount := GetProps(ParamProp, PropProps);
            for j := 0 to PropPropCount-1 do
            begin
              PropPropInfo := PropProps^[j]^;
              case PropPropInfo.PropType^.Kind of
                tkInt64, tkInteger:
                begin
                  CurrPropJSON.Add(PropPropInfo.Name,
                    GetInt64Prop(ParamProp, PropPropInfo.Name));
                end;
                tkEnumeration:
                begin
                  CurrPropJSON.Add(PropPropInfo.Name,
                    GetEnumProp(ParamProp, PropPropInfo.Name));
                end;
                tkString:
                begin
                  CurrPropJSON.Add(PropPropInfo.Name,
                    GetStrProp(ParamProp, PropPropInfo.Name));
                end;
              end;
            end;
            FigureJSON.Add(PropInfo.Name, CurrPropJSON.Clone);
            FreeAndNil(CurrPropJSON);
          end;
        end;
      end;
      VertexesJSON := TJSONArray.Create;
      for v in CurrentFigure.Vertexes do
      begin
        CurrVertexJSON := TJSONArray.Create;
        CurrVertexJSON.Add(v.x);
        CurrVertexJSON.Add(v.y);
        VertexesJSON.Add(TJSONData(CurrVertexJSON).Clone);
        FreeAndNil(CurrVertexJSON);
      end;
      FigureJSON.Add('vertexes', TJSONData(VertexesJSON).Clone);
      FreeAndNil(VertexesJSON);
      FiguresJSON.Add(FigureJSON.Clone);
      FreeAndNil(FigureJSON);
    end;
    Root := TJSONObject.Create;
    Root.CompressedJSON := true;
    Root.Add(FILE_HEADER, FiguresJSON.Clone);
    Root.Add('version', VERSION);
    FreeAndNil(FiguresJSON);
    AssignFile(f, FileName);
    Rewrite(f);
    System.Write(f, Root.AsJSON);
    FreeAndNil(Root);
    System.Close(f);
    CurrentFile := FileName;
  except
    on E: Exception do
    begin
      CurrentFile := NEW_FILE_NAME;
      Result := 1;
    end;
  end;
end;

function LoadFile(FileName: string): integer;
var
  f: TFileStream;
  Root, CurrFigureJSON, PropJSON: TJSONObject;
  FiguresJSON, FigureVertexesJSON: TJSONArray;
  i, j, FileVersion: integer;
  k, l: TJSONEnum;
  FigureClassName, PropClassName: string;
  FigureClass, PropClass: TPersistentClass;
  CurrFigure: TFigure;
  CurrProp: TToolParam;
  FigureVertexes: TDPointList;
begin
  Result := 0;
  f := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Root := TJSONObject(GetJSON(f));

    FileVersion := Root.Get('version', -1);
    if FileVersion = -1 then
      FileVersion := 1; //assuming it's the first version

    FiguresJSON := TJSONArray(Root.GetPath(FILE_HEADER));
    for i := 0 to FiguresJSON.Count-1 do
    begin
      CurrFigureJSON := FiguresJSON.Objects[i];
      FigureClassName := CurrFigureJSON.GetPath('type').AsString;
      FigureClass := FindClass(FigureClassName);
      CurrFigure := FigureClass.Create as TFigure;
      FigureVertexesJSON := CurrFigureJSON.GetPath('vertexes') as TJSONArray;
      for j := 0 to FigureVertexesJSON.Count-1 do
      begin
        SetLength(FigureVertexes, Length(FigureVertexes)+1);
        FigureVertexes[High(FigureVertexes)] := DoublePoint(
          FigureVertexesJSON.Arrays[j].Floats[0],
          FigureVertexesJSON.Arrays[j].Floats[1]);
      end;
      CurrFigure.Vertexes := FigureVertexes;
      SetLength(FigureVertexes, 0);
      for k in CurrFigureJSON do
      begin
        if (k.Key = 'class') or (k.Key = 'vertexes') then
          Continue;
        case k.Value.JSONType of
          jtNumber:
          begin
            SetFloatProp(CurrFigure, k.Key, k.Value.AsFloat);
          end;
          jtObject:
          begin
            PropJSON := k.Value as TJSONObject;
            PropClassName := PropJSON.Get('class');
            PropClass := FindClass(PropClassName);
            CurrProp := PropClass.Create as TToolParam;
            for l in PropJSON do
            begin
              case l.Value.JSONType of
                jtString:
                begin
                  if l.Key = 'Name' then
                    CurrProp.Name := l.Value.AsString
                  else
                  if l.Key = 'class' then
                    Continue
                  else
                  begin
                    SetEnumProp(CurrProp, l.Key, l.Value.AsString);
                  end;
                end;
                jtNumber:
                begin
                  SetInt64Prop(CurrProp, l.Key, l.Value.AsInteger);
                end;
              end;
            end;
            SetObjectProp(CurrFigure, k.Key, CurrProp);
          end;
        end;
      end;
      SetLength(CanvasItems, Length(CanvasItems) + 1);
      CanvasItems[High(CanvasItems)] := CurrFigure;
    end;
    FreeAndNil(f);
    FreeAndNil(Root);
    CurrentFile := FileName;
  except
    on E: Exception do
    begin
      CurrentFile := NEW_FILE_NAME;
      Result := 1;
    end;
  end;
end;

initialization
  CurrentFile := NEW_FILE_NAME;
end.

