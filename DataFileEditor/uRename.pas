unit uRename;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmRename = class(TForm)
    Label6: TLabel;
    edit_OldPath: TEdit;
    Label7: TLabel;
    edit_OldName: TEdit;
    Shape1: TShape;
    Label1: TLabel;
    edit_NewPath: TEdit;
    Label2: TLabel;
    edit_NewName: TEdit;
    Panel1: TPanel;
    btn_Save: TButton;
    Button2: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRename: TfrmRename;

implementation

{$R *.dfm}

procedure TfrmRename.FormClose(Sender: TObject; var Action: TCloseAction);
begin
// drop the last "/" if set (DataFile Editor will not see any keys stored with a final /
if edit_NewPath.Text[Length(edit_NewPath.Text)] = '/' then
   edit_NewPath.Text := String(edit_NewPath.Text).TrimRight(['/']);

end;

end.
