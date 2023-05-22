program DataFileEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  uMain in 'uMain.pas' {frmMain},
  DataFile in '..\DataFile.pp',
  u_extended_treeview in 'u_extended_treeview.pas',
  uEditValue in 'uEditValue.pas' {frmEdit},
  uDFKey in 'uDFKey.pas' {frmDFkey},
  uAbout in 'uAbout.pas' {frmAbout};

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.
