unit UFigureParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type
  TParameter = record
    Name: string;
    Control: TControl;
  end;
  TParameterList = array of TParameter;

procedure AddParam(Param: TParameter; var ParamList: TParameterList)
implementation

end.

