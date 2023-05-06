unit Main;

interface

uses
  SysUtils, Variants, Classes,
  Windows, Messages,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Grids,
  BCHexEditor;

type
  TMainForm = class(TForm)
    BCHexEditor1: TBCHexEditor;
    procedure FormCreate(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  BCHexEditor1.LoadFromFile(Application.ExeName);
end;

end.
