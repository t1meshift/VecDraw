unit UAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, LCLIntf;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    GithubButton: TBitBtn;
    Logo: TImage;
    ProductNameLabel: TLabel;
    AuthorNameLabel: TLabel;
    LicenseMemo: TMemo;
    OKButton: TSpeedButton;
    procedure GithubButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }


procedure TAboutForm.GithubButtonClick(Sender: TObject);
begin
  OpenURL('https://github.com/t1meshift/VecDraw');
end;

procedure TAboutForm.OKButtonClick(Sender: TObject);
begin
  AboutForm.Close;
end;

end.

