program DataFileEditor;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {frmMain},
  DataFile in '..\DataFile.pas',
  u_extended_treeview in 'u_extended_treeview.pas',
  Vcl.Themes,
  Vcl.Styles,
  uEditValue in 'uEditValue.pas' {frmEdit},
  BCHexEditor in 'lib\BCHexEditor-main\Source\BCHexEditor.pas',
  uDFKey in 'uDFKey.pas' {frmDFkey},
  uAbout in 'uAbout.pas' {frmAbout},
  uRename in 'uRename.pas' {frmRename};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
//  Application.CreateForm(TfrmRename, frmRename);
//  Application.CreateForm(TfrmEdit, frmEdit);
  Application.Run;
end.
