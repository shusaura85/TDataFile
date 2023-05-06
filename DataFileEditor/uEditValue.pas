unit uEditValue;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
  BCHexEditor;

type
  TfrmEdit = class(TForm)
    pages: TPageControl;
    page_String: TTabSheet;
    Panel1: TPanel;
    btn_Save: TButton;
    Button2: TButton;
    edit_String: TMemo;
    page_Integer: TTabSheet;
    edit_Integer: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edit_Integer_Hex: TEdit;
    lbl_Integer_outside_range: TLabel;
    page_Stream: TTabSheet;
    Panel2: TPanel;
    edit_ValuePath: TEdit;
    page_Int64: TTabSheet;
    Label3: TLabel;
    edit_Int64: TEdit;
    lbl_Int64_outside_range: TLabel;
    Label5: TLabel;
    edit_Int64_Hex: TEdit;
    edit_ValueName: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edit_int32_type: TEdit;
    edit_int64_type: TEdit;
    Label9: TLabel;
    Panel3: TPanel;
    chk_hex_AllowInsert: TCheckBox;
    page_Float: TTabSheet;
    Label4: TLabel;
    edit_float_type: TEdit;
    Label10: TLabel;
    edit_float: TEdit;
    page_Calendar: TTabSheet;
    calendar_time: TDateTimePicker;
    Label13: TLabel;
    Label15: TLabel;
    calendar_date: TDateTimePicker;
    Button1: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    page_Int8: TTabSheet;
    edit_Int8: TEdit;
    edit_Int8_Hex: TEdit;
    Label11: TLabel;
    lbl_Int8_outside_range: TLabel;
    Label14: TLabel;
    edit_int8_type: TEdit;
    Label16: TLabel;
    page_Int16: TTabSheet;
    Label12: TLabel;
    edit_int16_type: TEdit;
    Label17: TLabel;
    edit_Int16: TEdit;
    lbl_Int16_outside_range: TLabel;
    Label19: TLabel;
    edit_Int16_Hex: TEdit;
    procedure edit_IntegerKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edit_Integer_HexKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure edit_Int64KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edit_Int64_HexKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chk_hex_AllowInsertClick(Sender: TObject);
    procedure calendar_dateChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure edit_Int8KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edit_Int8_HexKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edit_Int16KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edit_Int16_HexKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    hex_active: boolean;
  public
    { Public declarations }
    hex: TBCHexEditor; //THexDump; // see inc/HexDump.pas for details
    fs: TMemoryStream;

    df_type: string;

    procedure SetupHexViewer;
  end;

var
  frmEdit: TfrmEdit;

implementation

{$R *.dfm}

procedure TfrmEdit.SetupHexViewer;
begin
hex_active := true;
hex := TBCHexEditor.Create(page_Stream);
hex.Parent := page_Stream;
hex.Align := alClient;

hex.LoadFromStream(fs);

hex.ReadOnlyView := false;
// allow insert/delete bytes, if false, only edit
hex.InsertMode := chk_hex_AllowInsert.Checked;

end;


procedure TfrmEdit.Button1Click(Sender: TObject);
begin
SaveDialog1.FileName := edit_ValueName.Text;
if SaveDialog1.Execute then
   begin
   fs.SaveToFile(SaveDialog1.FileName);
   end;
end;

procedure TfrmEdit.Button3Click(Sender: TObject);
begin
if OpenDialog1.Execute then
   begin
   fs.Clear;
   fs.LoadFromFile(OpenDialog1.FileName);
   fs.Position := 0;
   hex.LoadFromStream(fs);
   end;
end;

procedure TfrmEdit.calendar_dateChange(Sender: TObject);
begin
calendar_time.Date := calendar_date.Date;
end;

procedure TfrmEdit.chk_hex_AllowInsertClick(Sender: TObject);
begin
hex.InsertMode := chk_hex_AllowInsert.Checked;
end;

procedure TfrmEdit.edit_Int16KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: Integer;
   ui: UInt32;
begin
lbl_Int16_outside_range.Caption := '';
if Length((Sender as TEdit).Text) > 0 then
   begin
   if df_type = 'Int16' then
      begin
      if TryStrToInt((Sender as TEdit).Text, i) then edit_Int16_Hex.Text := IntToHex(Int16(i))
      else
         begin
         edit_Int16_Hex.Text := '';
         lbl_Int16_outside_range.Caption := 'Entered value is not a valid Int16 number';
         end;

      end
   else
      begin
      if TryStrToUInt((Sender as TEdit).Text, ui) then edit_Int8_Hex.Text := IntToHex(UInt16(ui))
      else
         begin
         edit_Int16_Hex.Text := '';
         lbl_Int16_outside_range.Caption := 'Entered value is not a valid UInt16 number';
         end;
      end;
   end
else
   edit_Int16_Hex.Text := '';
end;

procedure TfrmEdit.edit_Int16_HexKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var  i: Integer;
    ui: UInt32;
begin
if Length((Sender as TEdit).Text) > 0 then
   begin
   if df_type = 'Int16' then
      begin
      if TryStrToInt('$'+(Sender as TEdit).Text, i) then edit_Int16.Text := IntToStr(StrToInt('$'+(Sender as TEdit).Text))
                                                    else edit_Int16.Text := '0';
      end
   else
      begin
      if TryStrToUInt('$'+(Sender as TEdit).Text, ui) then edit_Int16.Text := format('%u', [ StrToUInt('$'+(Sender as TEdit).Text) ])
                                                      else edit_Int16.Text := '0';
      end;
   end
else
   edit_Int16.Text := '0';
end;

procedure TfrmEdit.edit_Int64KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: Int64;
   ui: UInt64;
begin
lbl_Int64_outside_range.Caption := '';
if Length((Sender as TEdit).Text) > 0 then
   begin
   if df_type = 'Int64' then
      begin
      if TryStrToInt64((Sender as TEdit).Text, i) then edit_Int64_Hex.Text := IntToHex(i)
      else
          begin
          edit_Int64_Hex.Text := '';
          lbl_Int64_outside_range.Caption := 'Entered value is not a valid Int64 number';
          end;
      end
   else
      begin
      if TryStrToUInt64((Sender as TEdit).Text, ui) then edit_Int64_Hex.Text := IntToHex(ui)
      else
          begin
          edit_Int64_Hex.Text := '';
          lbl_Int64_outside_range.Caption := 'Entered value is not a valid UInt64 number';
          end;
      end;
   end
else
   edit_Int64_Hex.Text := '';
end;

procedure TfrmEdit.edit_Int64_HexKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var  i: Int64;
    ui: UInt64;
begin
if Length((Sender as TEdit).Text) > 0 then
   begin
   if df_type = 'Int32' then
      begin
      if TryStrToInt64('$'+(Sender as TEdit).Text, i) then edit_Int64.Text := IntToStr(StrToInt64('$'+(Sender as TEdit).Text))
                                                      else edit_Int64.Text := '0';
      end
   else
      begin
      if TryStrToUInt64('$'+(Sender as TEdit).Text, ui) then edit_Int64.Text := format('%u', [ StrToUInt64('$'+(Sender as TEdit).Text) ])
                                                        else edit_Int64.Text := '0';
      end;
   end
else
   edit_Integer.Text := '0';
end;

procedure TfrmEdit.edit_Int8KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: Integer;
   ui: UInt32;
begin
lbl_Int8_outside_range.Caption := '';
if Length((Sender as TEdit).Text) > 0 then
   begin
   if df_type = 'Int8' then
      begin
      if TryStrToInt((Sender as TEdit).Text, i) then edit_Int8_Hex.Text := IntToHex(Int8(i))
      else
         begin
         edit_Int8_Hex.Text := '';
         lbl_Int8_outside_range.Caption := 'Entered value is not a valid Int8 number';
         end;

      end
   else
      begin
      if TryStrToUInt((Sender as TEdit).Text, ui) then edit_Int8_Hex.Text := IntToHex(UInt8(ui))
      else
         begin
         edit_Int8_Hex.Text := '';
         lbl_Int8_outside_range.Caption := 'Entered value is not a valid UInt8 number';
         end;
      end;
   end
else
   edit_Int8_Hex.Text := '';
end;

procedure TfrmEdit.edit_Int8_HexKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var  i: Integer;
    ui: UInt32;
begin
if Length((Sender as TEdit).Text) > 0 then
   begin
   if df_type = 'Int8' then
      begin
      if TryStrToInt('$'+(Sender as TEdit).Text, i) then edit_Int8.Text := IntToStr(Int8(StrToInt('$'+(Sender as TEdit).Text)))
                                                    else edit_Int8.Text := '0';
      end
   else
      begin
      if TryStrToUInt('$'+(Sender as TEdit).Text, ui) then edit_Int8.Text := format('%u', [ UInt8(StrToUInt('$'+(Sender as TEdit).Text)) ])
                                                      else edit_Int8.Text := '0';
      end;
   end
else
   edit_Int8.Text := '0';
end;

procedure TfrmEdit.edit_IntegerKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: Integer;
   ui: UInt32;
begin
lbl_Integer_outside_range.Caption := '';
if Length((Sender as TEdit).Text) > 0 then
   begin
   if df_type = 'Int32' then
      begin
      if TryStrToInt((Sender as TEdit).Text, i) then edit_Integer_Hex.Text := IntToHex(i)
      else
         begin
         edit_Integer_Hex.Text := '';
         lbl_Integer_outside_range.Caption := 'Entered value is not a valid Int32 number';
         end;

      end
   else
      begin
      if TryStrToUInt((Sender as TEdit).Text, ui) then edit_Integer_Hex.Text := IntToHex(ui)
      else
         begin
         edit_Integer_Hex.Text := '';
         lbl_Integer_outside_range.Caption := 'Entered value is not a valid UInt32 number';
         end;
      end;
   end
else
   edit_Integer_Hex.Text := '';
end;

procedure TfrmEdit.edit_Integer_HexKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var  i: Integer;
    ui: UInt32;
begin
if Length((Sender as TEdit).Text) > 0 then
   begin
   if df_type = 'Int32' then
      begin
      if TryStrToInt('$'+(Sender as TEdit).Text, i) then edit_Integer.Text := IntToStr(StrToInt('$'+(Sender as TEdit).Text))
                                                    else edit_Integer.Text := '0';
      end
   else
      begin
      if TryStrToUInt('$'+(Sender as TEdit).Text, ui) then edit_Integer.Text := format('%u', [ StrToUInt('$'+(Sender as TEdit).Text) ])
                                                      else edit_Integer.Text := '0';
      end;
   end
else
   edit_Integer.Text := '0';
end;

procedure TfrmEdit.FormClose(Sender: TObject; var Action: TCloseAction);
var    i: Integer;
      ui: UInt32;
     i64: Int64;
    ui64: UInt64;
       f: double;
begin
// quick sanity check
if (df_type = 'Int8') then if not TryStrToInt(edit_Int8.Text, i) then edit_Integer.Text := '0';
if (df_type = 'UInt8') then if not TryStrToUInt(edit_Int8.Text, ui) then edit_Integer.Text := '0';

if (df_type = 'Int16') then if not TryStrToInt(edit_Int16.Text, i) then edit_Int16.Text := '0';
if (df_type = 'UInt16') then if not TryStrToUInt(edit_Int16.Text, ui) then edit_Int16.Text := '0';

if (df_type = 'Int32') then if not TryStrToInt(edit_Integer.Text, i) then edit_Integer.Text := '0';
if (df_type = 'UInt32') then if not TryStrToUInt(edit_Integer.Text, ui) then edit_Integer.Text := '0';

if (df_type = 'Int64') then if not TryStrToInt64(edit_Int64.Text, i64) then edit_Int64.Text := '0';
if (df_type = 'UInt64') then if not TryStrToUInt64(edit_Int64.Text, ui64) then edit_Int64.Text := '0';

if (df_type = 'Single') or (df_type = 'Double') or (df_type = 'Currency') then if not TryStrToFloat(edit_float.Text, f) then edit_float.Text := '0';

if hex_active then
  begin
  fs.Clear;
  hex.SaveToStream(fs);
  hex.Free;
  end;
end;

procedure TfrmEdit.FormCreate(Sender: TObject);
begin
hex_active := false;
end;

procedure TfrmEdit.FormShow(Sender: TObject);
var i:integer;
begin
lbl_Int8_outside_range.Caption := '';
lbl_Int16_outside_range.Caption := '';
lbl_Integer_outside_range.Caption := '';
lbl_Int64_outside_range.Caption := '';

for i:= 0 to pages.PageCount-1 do
    begin
    if pages.Pages[i] <> pages.ActivePage then pages.Pages[i].TabVisible := false;

    end;
end;

end.
