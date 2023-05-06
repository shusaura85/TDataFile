unit uDFKey;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

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

{$R *.dfm}

end.
