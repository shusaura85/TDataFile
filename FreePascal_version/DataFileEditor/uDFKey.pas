unit uDFKey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type
  TfrmDFkey = class(TForm)
    edit_filename: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edit_Password: TEdit;
    lbl_InfoOpen: TLabel;
    lbl_InfoNew: TLabel;
    Button1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDFkey: TfrmDFkey;

implementation

{$R *.lfm}

end.
