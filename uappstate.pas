unit UAppState;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

procedure SetWindowTitle(AForm: TForm; AFileName: string);

var
  Modified: boolean;

implementation

procedure SetWindowTitle(AForm: TForm; AFileName: string);
var
  s: string;
begin
  if Modified then
    s := '* '
  else
    s := '';
  s := s + AFileName + ' - ' + ApplicationName;
  if s <> AForm.Caption then
    AForm.Caption := s;
end;

initialization
  Modified := false;
end.

