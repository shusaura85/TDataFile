unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.UITypes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.StdCtrls, Winapi.commctrl, System.ImageList,
  datafile, u_extended_treeview;

type
  TfrmMain = class(TForm)
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Splitter1: TSplitter;
    AddressBar: TEdit;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    New2: TMenuItem;
    Key1: TMenuItem;
    N2: TMenuItem;
    String1: TMenuItem;
    N3: TMenuItem;
    Delete1: TMenuItem;
    SaveDialog1: TSaveDialog;
    RootKey1: TMenuItem;
    Panel1: TPanel;
    KeyTree: TTreeView;
    panel_addKey: TPanel;
    lbl_addKey: TLabel;
    edit_addKey: TEdit;
    btn_addKey: TButton;
    btn_addKey_Cancel: TButton;
    Integer1: TMenuItem;
    Panel3: TPanel;
    ValueList: TListView;
    panel_addValue: TPanel;
    lbl_addValue: TLabel;
    edit_addValue: TEdit;
    btn_addValue: TButton;
    btn_addValue_Cancel: TButton;
    Boolean1: TMenuItem;
    AnsiString1: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N64bitInteger1: TMenuItem;
    N32bitIntegerunsigned1: TMenuItem;
    N54bitIntegerunsigned1: TMenuItem;
    N6: TMenuItem;
    N32bitFloatSingle1: TMenuItem;
    N64bitFloatDouble1: TMenuItem;
    Currency64bit1: TMenuItem;
    N7: TMenuItem;
    DateTime1: TMenuItem;
    Fontsettings1: TMenuItem;
    ImageList2: TImageList;
    Delete2: TMenuItem;
    FontDialog1: TFontDialog;
    N8: TMenuItem;
    Binary1: TMenuItem;
    Importfile1: TMenuItem;
    N8bitIntegersigned1: TMenuItem;
    N8bitIntegerunsigned1: TMenuItem;
    N16bitIntegersigned1: TMenuItem;
    N16bitIntegerunsigned1: TMenuItem;
    View1: TMenuItem;
    Refresh1: TMenuItem;
    Help1: TMenuItem;
    AboutDataFileEditor1: TMenuItem;
    StringList1: TMenuItem;
    N9: TMenuItem;
    Modify1: TMenuItem;
    Rename1: TMenuItem;
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure KeyTreeDblClick(Sender: TObject);
    procedure KeyTreeClick(Sender: TObject);
    procedure Key1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure KeyTreeCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure RootKey1Click(Sender: TObject);
    procedure btn_addKey_CancelClick(Sender: TObject);
    procedure btn_addKeyClick(Sender: TObject);
    procedure KeyTreeContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure btn_addValue_CancelClick(Sender: TObject);
    procedure addValueClick(Sender: TObject);
    procedure btn_addValueClick(Sender: TObject);
    procedure ValueListContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure edit_addKeyKeyPress(Sender: TObject; var Key: Char);
    procedure edit_addValueKeyPress(Sender: TObject; var Key: Char);
    procedure Delete2Click(Sender: TObject);
    procedure ValueListDblClick(Sender: TObject);
    procedure importfile1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure AboutDataFileEditor1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Modify1Click(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    df: TDataFile;
    procedure CreateNodes(str:string; path:string = ''; parent:TTreeNode = nil);
    procedure LoadHive;
    procedure LoadValues(section:string);
    procedure EditValue(section, identifier, datatype: string);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses uEditValue, uDFKey, uAbout, uRename;

const LABEL_ADDKEY = 'Enter new key name';
      LABEL_ADDROOT = 'Enter new root key name';

procedure TfrmMain.CreateNodes(str:string; path:string = ''; parent:TTreeNode = nil);
var li:TTreeNode;
    s,s2, section:string;
    p:integer;
begin
p := Pos('/', str);
if p > 0 then
   begin
   s:= Copy(str,1, p-1);
   s2 := Copy(str, p+1, Length(str)-p);

   if path <> '' then section := path +'/' + s
                 else section := s;

   li := GetNodeByText(KeyTree, section);
   if li = nil then
      begin
      li:= KeyTree.Items.AddChild(parent, s);
      li.ImageIndex := 0; li.SelectedIndex := 1;
      TDFTreeNode(li).Section := section;
      end;

   if s2 <> '' then CreateNodes(s2, section, li);

   end
else
   begin
   if path <> '' then section := path +'/' + str
                 else section := str;

   li := GetNodeByText(KeyTree, section);
   if li = nil then
      begin
      li:= KeyTree.Items.AddChild(parent, str);
      li.ImageIndex := 0; li.SelectedIndex := 1;
      TDFTreeNode(li).Section := section;
      end;

   end;
end;


procedure TfrmMain.LoadHive;
var sl:TStringList;
    i:integer;
    s:string;
begin
KeyTree.Items.Clear;
KeyTree.SortType := stNone;

sl := TStringList.Create;

df.GetSectionNames(sl);

KeyTree.Items.BeginUpdate;
for i := 0 to sl.Count-1 do
    begin
    s := trim(sl.Strings[i]);
    if s <> '' then
       begin
       CreateNodes(s, '', nil);
       end;
    end;
KeyTree.SortType := stText;
KeyTree.Items.EndUpdate;


FreeAndNil(sl);
end;

procedure TfrmMain.LoadValues(section: string);
var sl: TStringList;
    li: TListItem;
     i: integer;
// used to process entry values
     f: TFont;
     v: Int64;
     w: UInt64;
     d: Double;
    dt: TDateTime;
   str: string;
begin
ValueList.Items.Clear;

sl := TStringList.Create;

// use this if you don't need data type information
//df.GetValueNames(p, sl);
df.GetValueNamesAndTypes(section, sl);

ValueList.SortType := stNone;
ValueList.Items.BeginUpdate;

for i := 0 to sl.Count-1 do
   begin
   li := ValueList.Items.Add;
   li.ImageIndex := 0; // set default icon to unknown
   //li.Caption := sl.Strings[i];
   li.Caption := sl.Names[i];
   //li.SubItems.Add( df.GetValueTypeAsString(p, sl.Strings[i]) );
   li.SubItems.Add( sl.ValueFromIndex[i] );
//   li.SubItems.Add('data value: to do');
//   li.SubItems.Add(sl.Strings[i]);
   if sl.ValueFromIndex[i] = 'Unknown' then
      begin
      li.ImageIndex := 0;
      li.SubItems.Add('(unknown data type)');
      end;
   if sl.ValueFromIndex[i] = 'Stream' then
      begin
      li.ImageIndex := 1;
      li.SubItems.Add('(binary stream)');
      end;
   if sl.ValueFromIndex[i] = 'Strings' then
      begin
      li.ImageIndex := 1;
      li.SubItems.Add('(string list)');
      end;
   if sl.ValueFromIndex[i] = 'Boolean' then
      begin
      li.ImageIndex := 10;
      if df.ReadBoolean(section, sl.Names[i],false) then li.SubItems.Add('TRUE')
                                                    else li.SubItems.Add('FALSE');
      end;
   if sl.ValueFromIndex[i] = 'AnsiString' then
      begin
      li.ImageIndex := 2;
      str := String( df.ReadANSIString(section, sl.Names[i], '(empty ANSI string)') );
      if Length(str) > 30 then str := Copy(str,1,30)+'...';
      li.SubItems.Add( str );
      end;
   if sl.ValueFromIndex[i] = 'String' then
      begin
      li.ImageIndex := 3;
      str := df.ReadString(section, sl.Names[i], '(empty string)');
      if Length(str) > 30 then str := Copy(str,1,30)+'...';
      li.SubItems.Add( str );
      end;
   if sl.ValueFromIndex[i] = 'Int8' then
      begin
      li.ImageIndex := 4;
      v := df.ReadInt8(section, sl.Names[i], 0);
      str := IntToStr( v ) + ' (0x'+IntToHex(Int8(v))+')';
      li.SubItems.Add( str );
      end;
   if sl.ValueFromIndex[i] = 'Int16' then
      begin
      li.ImageIndex := 5;
      v := df.ReadInt8(section, sl.Names[i], 0);
      str := IntToStr( v ) + ' (0x'+IntToHex(Int16(v))+')';
      li.SubItems.Add( str );
      end;
   if sl.ValueFromIndex[i] = 'UInt8' then
      begin
      li.ImageIndex := 4;
      v := df.ReadUInt8(section, sl.Names[i], 0);
      str := IntToStr( v ) + ' (0x'+IntToHex(UInt8(v))+')';
      li.SubItems.Add( str );
      end;
   if sl.ValueFromIndex[i] = 'UInt16' then
      begin
      li.ImageIndex := 5;
      v := df.ReadUInt16(section, sl.Names[i], 0);
      str := IntToStr( v ) + ' (0x'+IntToHex(UInt16(v))+')';
      li.SubItems.Add( str );
      end;
   if sl.ValueFromIndex[i] = 'Int32' then
      begin
      li.ImageIndex := 4;
      v := df.ReadInteger(section, sl.Names[i], 0);
      str := IntToStr( v ) + ' (0x'+IntToHex(Integer(v))+')';
      li.SubItems.Add( str );
      end;
   if sl.ValueFromIndex[i] = 'Int64' then
      begin
      li.ImageIndex := 5;
      v := df.ReadInt64(section, sl.Names[i], 0);
      str := IntToStr( v ) + ' (0x'+IntToHex(v)+')';
      li.SubItems.Add( str );
      end;

   if sl.ValueFromIndex[i] = 'UInt32' then
      begin
      li.ImageIndex := 6;
      w := df.ReadUInt32(section, sl.Names[i], 0);
      str := format('%u', [ w ]) + ' (0x'+IntToHex(UInt32(w))+')';
      li.SubItems.Add( str );
      end;

   if sl.ValueFromIndex[i] = 'UInt64' then
      begin
      li.ImageIndex := 7;
      w := df.ReadUInt64(section, sl.Names[i], 0);
      str := format('%u', [ w ]) + ' (0x'+IntToHex(w)+')';
      li.SubItems.Add( str );
      end;

   if sl.ValueFromIndex[i] = 'Single' then
      begin
      li.ImageIndex := 8;
      d := df.ReadSingle(section, sl.Names[i], 0);
      str := FloatToStr( d );
      li.SubItems.Add( str );
      end;

   if sl.ValueFromIndex[i] = 'Double' then
      begin
      li.ImageIndex := 9;
      d := df.ReadDouble(section, sl.Names[i], 0);
      str := FloatToStr( d );
      li.SubItems.Add( str );
      end;

   if sl.ValueFromIndex[i] = 'Currency' then
      begin
      li.ImageIndex := 12;
      d := df.ReadCurrency(section, sl.Names[i], 0);
      str := CurrToStr( d );
      li.SubItems.Add( str );
      end;
   if sl.ValueFromIndex[i] = 'DateTime' then
      begin
      li.ImageIndex := 11;
      dt := df.ReadDateTime(section, sl.Names[i], Now() );
      str := DateTimeToStr(dt);
      li.SubItems.Add( str );
      end;
   if sl.ValueFromIndex[i] = 'Font' then
      begin
      li.ImageIndex := 13;
      f:= TFont.Create;
      df.ReadFont(section, sl.Names[i], f );
      str := f.Name;
      str := str + ', size '+IntToStr(f.Size);
      if TFontStyle.fsBold in f.Style then str := str + ', bold';
      if TFontStyle.fsItalic in f.Style then str := str + ', italic';
      if TFontStyle.fsUnderline in f.Style then str := str + ', underline';
      if TFontStyle.fsStrikeOut in f.Style then str := str + ', strikeout';
      li.SubItems.Add( str );
      f.Free;
      end;

   end;

ValueList.SortType := stText;
ValueList.Items.EndUpdate;
sl.Free;
end;


procedure TfrmMain.Modify1Click(Sender: TObject);
var li: TListItem;

    t:TTreeNode;
    p:string;
begin
if not Assigned(df) then exit;
if KeyTree.Selected = nil then exit;
if ValueList.Selected = nil then exit;


// build path
t := KeyTree.Selected;
p := GetPathFromNode(t);

li := ValueList.Selected;

EditValue(p, li.Caption, li.SubItems.Strings[0]);
end;

procedure TfrmMain.EditValue(section: string; identifier: string; datatype: string);
var frm: TfrmEdit;
      b: Boolean;
begin
// since booleans can only be TRUE of FALSE, we just flip it instead of showing the edit window
if datatype = 'Boolean' then
  begin
  b := not df.ReadBoolean(section, identifier, true);
  df.WriteBoolean(section, identifier, b);
  // refresh list view
  LoadValues(section);
  exit;
  end;

// font settings - we just show a font dialog
if datatype = 'Font' then
  begin
  FontDialog1.Font := frmMain.Font;
  df.ReadFont(section, identifier, FontDialog1.Font);
  if FontDialog1.Execute then
     begin
     df.WriteFont(section, identifier, FontDialog1.Font);
     LoadValues(section);
     exit;
     end
  else exit;
  end;

frm := TfrmEdit.Create(frmMain);
frm.Position := poOwnerFormCenter;

frm.df_type := datatype;

frm.edit_ValuePath.Text := section;
frm.edit_ValueName.Text := identifier;

if (datatype = 'String') or (datatype = 'AnsiString') then
   begin
   frm.pages.ActivePage := frm.page_String;
   if (datatype = 'AnsiString') then
      begin
      frm.edit_String.Text := String( df.ReadAnsiString(section, identifier, '') );
      end
   else
      begin
      frm.edit_String.Text := df.ReadString(section, identifier, '');
      end;

   frm.ActiveControl := frm.edit_String;
   frm.edit_String.SelectAll;
   end
else
if (datatype = 'Strings') then
   begin
   frm.pages.ActivePage := frm.page_String;
   df.ReadStrings(section, identifier, frm.edit_String.Lines);

   frm.ActiveControl := frm.edit_String;
   frm.edit_String.SelectAll;
   end
else
if (datatype = 'Int8') or (datatype = 'UInt8') then
   begin
   frm.pages.ActivePage := frm.page_Int8;
   if (datatype = 'Int8') then
      begin
      frm.edit_Int8.Text := IntToStr( df.ReadInt8(section, identifier, 0) );
      frm.edit_Int8_Hex.Text := IntToHex( df.ReadInt8(section, identifier, 0) );
      frm.edit_int8_type.Text := datatype + ' | 8-bit Signed Integer [-128 .. 127]';
      end
   else
      begin
      frm.edit_Int8.Text := format('%u', [  df.ReadUInt8(section, identifier, 0) ]);
      frm.edit_Int8_Hex.Text := IntToHex( df.ReadUInt8(section, identifier, 0) );
      frm.edit_int8_type.Text := datatype + ' | 8-bit Unsigned Integer | Byte [0 .. 255]';
      end;

   frm.ActiveControl := frm.edit_Int8;
   frm.edit_Int8.SelectAll;
   end
else
if (datatype = 'Int16') or (datatype = 'UInt16') then
   begin
   frm.pages.ActivePage := frm.page_Int16;
   if (datatype = 'Int16') then
      begin
      frm.edit_Int16.Text := IntToStr( df.ReadInt16(section, identifier, 0) );
      frm.edit_Int16_Hex.Text := IntToHex( df.ReadInt16(section, identifier, 0) );
      frm.edit_int16_type.Text := datatype + ' | 16-bit Signed Integer [-32768 .. 32767]';
      end
   else
      begin
      frm.edit_Int16.Text := format('%u', [  df.ReadUInt16(section, identifier, 0) ]);
      frm.edit_Int16_Hex.Text := IntToHex( df.ReadUInt16(section, identifier, 0) );
      frm.edit_int16_type.Text := datatype + ' | 16-bit Unsigned Integer | Word [0 .. 65535]';
      end;

   frm.ActiveControl := frm.edit_Int16;
   frm.edit_Int16.SelectAll;
   end
else
if (datatype = 'Int32') or (datatype = 'UInt32') then
   begin
   frm.pages.ActivePage := frm.page_Integer;
   if (datatype = 'Int32') then
      begin
      frm.edit_Integer.Text := IntToStr( df.ReadInteger(section, identifier, 0) );
      frm.edit_Integer_Hex.Text := IntToHex( df.ReadInteger(section, identifier, 0) );
      frm.edit_int32_type.Text := datatype + ' | 32-bit Signed Integer';
      end
   else
      begin
      frm.edit_Integer.Text := format('%u', [  df.ReadUInt32(section, identifier, 0) ]);
      frm.edit_Integer_Hex.Text := IntToHex( df.ReadUInt32(section, identifier, 0) );
      frm.edit_int32_type.Text := datatype + ' | 32-bit Unsigned Integer';
      end;

   frm.ActiveControl := frm.edit_Integer;
   frm.edit_Integer.SelectAll;
   end
else
if (datatype = 'Int64') or (datatype = 'UInt64') then
   begin
   frm.pages.ActivePage := frm.page_Int64;
   if (datatype = 'Int64') then
      begin
      frm.edit_Int64.Text := IntToStr( df.ReadInt64(section, identifier, 0) );
      frm.edit_Int64_Hex.Text := IntToHex( df.ReadInt64(section, identifier, 0) );
      frm.edit_int64_type.Text := datatype + ' | 64-bit Signed Integer';
      end
   else
      begin
      frm.edit_Int64.Text := format('%u', [ df.ReadUInt64(section, identifier, 0) ]);
      frm.edit_Int64_Hex.Text := IntToHex( df.ReadUInt64(section, identifier, 0) );
      frm.edit_int64_type.Text := datatype + ' | 64-bit Unsigned Integer';
      end;

   frm.ActiveControl := frm.edit_Int64;
   frm.edit_Int64.SelectAll;
   end
else
if (datatype = 'Single') or (datatype = 'Double') or (datatype = 'Currency') then
   begin
   frm.pages.ActivePage := frm.page_Float;
   if (datatype = 'Single') then
      begin
      frm.edit_Float.Text := FloatToStr( df.ReadSingle(section, identifier, 0.0) );
      frm.edit_Float_type.Text := datatype + ' | 32-bit Float';
      end
   else
   if (datatype = 'Double') then
      begin
      frm.edit_Float.Text := FloatToStr( df.ReadDouble(section, identifier, 0.0) );
      frm.edit_Float_type.Text := datatype + ' | 64-bit Float';
      end
   else
      begin
      //frm.edit_Float.Text := CurrToStrF( df.ReadCurrency(section, identifier, 0.0000) , ffCurrency, 4);
      frm.edit_Float.Text := CurrToStr( df.ReadCurrency(section, identifier, 0.0000) );
      frm.edit_Float_type.Text := datatype + ' | 64-bit Float';
      end;

   frm.ActiveControl := frm.edit_Float;
   frm.edit_Integer.SelectAll;
   end
else
if datatype = 'DateTime' then
   begin
   frm.pages.ActivePage := frm.page_Calendar;
   frm.calendar_time.DateTime := df.ReadDateTime(section, identifier, GetTime());
   frm.calendar_date.Date := frm.calendar_time.Date;
   end
else
if (datatype = 'Stream') or (datatype = 'Unknown') then
   begin
   frm.pages.ActivePage := frm.page_Stream;
   frm.fs := TMemoryStream.Create;
   df.ReadStream(section, identifier, frm.fs);
   frm.fs.Position := 0;

   frm.edit_String.Lines.LoadFromStream(frm.fs);

   frm.SetupHexViewer;
   end;

// save data
if frm.ShowModal = mrOk then
   begin
   if datatype = 'AnsiString' then df.WriteAnsiString(section, identifier, AnsiString(frm.edit_String.Text))
   else
   if datatype = 'String' then df.WriteString(section, identifier, frm.edit_String.Text)
   else
   if datatype = 'Strings' then df.WriteStrings(section, identifier, frm.edit_String.Lines)
   else
   if datatype = 'Int8' then df.WriteInt8(section, identifier, Int8(StrToInt(frm.edit_Int8.Text) ) )
   else
   if datatype = 'UInt8' then df.WriteUInt8(section, identifier, UInt8(StrToUInt(frm.edit_Int8.Text) ) )
   else
   if datatype = 'Int16' then df.WriteInt16(section, identifier, Int16(StrToInt(frm.edit_Int16.Text) ) )
   else
   if datatype = 'UInt16' then df.WriteUInt16(section, identifier, UInt16(StrToUInt(frm.edit_Int16.Text) ) )
   else
   if datatype = 'Int32' then df.WriteInteger(section, identifier, StrToInt(frm.edit_Integer.Text) )
   else
   if datatype = 'UInt32' then df.WriteUInt32(section, identifier, StrToUInt(frm.edit_Integer.Text) )
   else
   if datatype = 'Int64' then df.WriteInt64(section, identifier, StrToInt64(frm.edit_Int64.Text) )
   else
   if datatype = 'UInt64' then df.WriteUInt64(section, identifier, StrToUInt64(frm.edit_Int64.Text) )
   else
   if datatype = 'Single' then df.WriteSingle(section, identifier, Single(StrToFloat(frm.edit_Float.Text)) )
   else
   if datatype = 'Double' then df.WriteDouble(section, identifier, Double(StrToFloat(frm.edit_Float.Text)) )
   else
   if datatype = 'Currency' then df.WriteCurrency(section, identifier, StrToCurr(frm.edit_Float.Text) )

   else
   if datatype = 'DateTime' then df.WriteDateTime(section, identifier, frm.calendar_time.DateTime )

   else
   if datatype = 'Stream' then begin frm.fs.Position := 0; df.WriteStream(section, identifier, frm.fs); end
   else
   if datatype = 'Unknown' then begin frm.fs.Position := 0; df.WriteStream(section, identifier, frm.fs, TDFType.dfd_Unknown); end

   ;


   // refresh list
   LoadValues(section);
   end;

if datatype = 'Stream' then frm.fs.Free;

frm.Free;
end;



procedure TfrmMain.importfile1Click(Sender: TObject);
var fs: TMemoryStream;
     t: TTreeNode;
     p: string;
    li: TListItem;
begin
if KeyTree.Selected = nil then exit;

t := KeyTree.Selected;
p:=GetPathFromNode(t);

if OpenDialog1.Execute then
   begin
   fs := TMemoryStream.Create;
   fs.LoadFromFile(OpenDialog1.FileName);
   df.WriteStream(p, ExtractFileName(OpenDialog1.FileName), fs);
   fs.Free;

   li := ValueList.Items.Add;
   li.Caption := ExtractFileName(OpenDialog1.FileName);
   li.SubItems.Add('Stream');
   li.SubItems.Add('(new binary stream)');
   end;
end;

procedure TfrmMain.AboutDataFileEditor1Click(Sender: TObject);
begin
frmAbout.ShowModal;
end;

procedure TfrmMain.btn_addKeyClick(Sender: TObject);
var t:TTreeNode;
    p:string;
begin
if btn_addKey.Tag = 1 then  // key
   begin
   t := KeyTree.Selected;
   p:=GetPathFromNode(t);
   if p <> '' then p := p+'/'+edit_addKey.Text;
   end
else
   p := edit_addKey.Text;  // root key


df.WriteDateTime(p, 'creation.date', Now());
CreateNodes(p, '', nil);

panel_addKey.Visible := false;
edit_addKey.Text := '';
KeyTree.Enabled := true;
end;

procedure TfrmMain.btn_addKey_CancelClick(Sender: TObject);
begin
panel_addKey.Visible := false;
edit_addKey.Text := '';
KeyTree.Enabled := true;
end;

procedure TfrmMain.btn_addValueClick(Sender: TObject);
var t: TTreeNode;
    p, sdt, sdd: string;
    dt: integer;
    li: TListItem;
    ms: TMemoryStream;
begin
dt := (Sender as TButton).Tag;

t := KeyTree.Selected;
p := GetPathFromNode(t);

if df.ValueExists(p, edit_addValue.Text) then
   begin
   li := ValueList.FindCaption(0, edit_addValue.Text, false, true, false);
   li.Delete;
   end;

sdd := '(new entry) '; // value it contains
case dt of
     Integer(dfd_Unknown): begin end;  // unknown
     Integer(dfd_Stream): begin  // streams
        ms := TMemoryStream.Create;
        df.WriteStream(p, edit_addValue.Text, ms);
        ms.Free;
        sdt := 'Stream';
        sdd := sdd + '(binary stream)';
        end;
     Integer(dfd_Strings):    begin df.WriteStrings(p, edit_addValue.Text, TStringList.Create());  sdt := 'Strings'; end;
     Integer(dfd_AnsiString): begin df.WriteAnsiString(p, edit_addValue.Text, '');      sdt := 'AnsiString'; end;
     Integer(dfd_String):     begin df.WriteString(p, edit_addValue.Text, '');          sdt := 'String'; end;
     Integer(dfd_Boolean):    begin df.WriteBoolean(p, edit_addValue.Text, false);      sdt := 'Boolean'; sdd := sdd+'FALSE'; end;
     Integer(dfd_Int8):       begin df.WriteInt8(p, edit_addValue.Text, 0);             sdt := 'Int8';    sdd := sdd+'0'; end;
     Integer(dfd_Int16):      begin df.WriteInt16(p, edit_addValue.Text, 0);            sdt := 'Int16';   sdd := sdd+'0'; end;
     Integer(dfd_UInt8):      begin df.WriteUInt8(p, edit_addValue.Text, 0);            sdt := 'UInt8';   sdd := sdd+'0'; end;
     Integer(dfd_UInt16):     begin df.WriteUInt16(p, edit_addValue.Text, 0);           sdt := 'UInt16';  sdd := sdd+'0'; end;
     Integer(dfd_Int32):      begin df.WriteInteger(p, edit_addValue.Text, 0);          sdt := 'Int32';   sdd := sdd+'0'; end;
     Integer(dfd_Int64):      begin df.WriteInt64(p, edit_addValue.Text, 0);            sdt := 'Int64';   sdd := sdd+'0'; end;
     Integer(dfd_UInt32):     begin df.WriteUInt32(p, edit_addValue.Text, 0);           sdt := 'UInt32';  sdd := sdd+'0'; end;
     Integer(dfd_UInt64):     begin df.WriteUInt64(p, edit_addValue.Text, 0);           sdt := 'UInt64';  sdd := sdd+'0'; end;
     Integer(dfd_Real48):     begin end;  // not used: real48
     Integer(dfd_Extended):   begin end;  // not used: extended
     Integer(dfd_Single):     begin df.WriteSingle(p, edit_addValue.Text, 0);          sdt := 'Single';   sdd := sdd+'0'; end;
     Integer(dfd_Double):     begin df.WriteDouble(p, edit_addValue.Text, 0);          sdt := 'Double';   sdd := sdd+'0'; end;
     Integer(dfd_Currency):   begin df.WriteCurrency(p, edit_addValue.Text, 0);        sdt := 'Currency'; sdd := sdd+'0'; end;
     Integer(dfd_DateTime):   begin df.WriteDateTime(p, edit_addValue.Text, Now());    sdt := 'DateTime'; sdd := sdd+DateTimeToStr(Now()); end;
     Integer(dfd_Font):       begin df.WriteFont(p, edit_addValue.Text, frmMain.Font); sdt := 'Font'; end;
end;

li := ValueList.Items.Add;
li.Caption := edit_addValue.Text;
li.SubItems.Add(sdt);
li.SubItems.Add(sdd);

panel_addValue.Visible := false;
edit_addValue.Text := '';
ValueList.Enabled := true;

end;

procedure TfrmMain.btn_addValue_CancelClick(Sender: TObject);
begin
panel_addValue.Visible := false;
edit_addValue.Text := '';
ValueList.Enabled := true;
end;

procedure TfrmMain.Delete1Click(Sender: TObject);
var t:TTreeNode;
    p:string;
begin
if KeyTree.Selected = nil then exit;

t := KeyTree.Selected;
p:=GetPathFromNode(t);

df.DeleteSection(p);

t.Delete;
end;

procedure TfrmMain.Delete2Click(Sender: TObject);
var t:TTreeNode;
    p:string;
    li:TListItem;
begin
if KeyTree.Selected = nil then exit;
if ValueList.Selected = nil then exit;

t := KeyTree.Selected;
p:=GetPathFromNode(t);

li := ValueList.Selected;

df.Delete(p, li.Caption);

li.Delete;
end;

procedure TfrmMain.edit_addKeyKeyPress(Sender: TObject; var Key: Char);
begin
if Key = #13 then btn_addKey.Click;
end;

procedure TfrmMain.edit_addValueKeyPress(Sender: TObject; var Key: Char);
begin
if Key = #13 then btn_addValue.Click;
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
if (df is TDataFile) then df.Destroy;
Close;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
// assign data type values to the popup menu
//Integer(dfd_Unknown);
Binary1.Tag                := Integer(dfd_Stream);
StringList1.Tag            := Integer(dfd_Strings);
AnsiString1.Tag            := Integer(dfd_AnsiString);
String1.Tag                := Integer(dfd_String);
Boolean1.Tag               := Integer(dfd_Boolean);
N8bitIntegersigned1.Tag    := Integer(dfd_Int8);
N8bitIntegerunsigned1.Tag  := Integer(dfd_UInt8);
N16bitIntegersigned1.Tag   := Integer(dfd_Int16);
N16bitIntegerunsigned1.Tag := Integer(dfd_UInt16);
Integer1.Tag               := Integer(dfd_Int32);
N32bitIntegerunsigned1.Tag := Integer(dfd_UInt32);
N64bitInteger1.Tag         := Integer(dfd_Int64);
N54bitIntegerunsigned1.Tag := Integer(dfd_UInt64);
//Integer(dfd_Real48);
//Integer(dfd_Extended);
N32bitFloatSingle1.Tag     := Integer(dfd_Single);
N64bitFloatDouble1.Tag     := Integer(dfd_Double);
Currency64bit1.Tag         := Integer(dfd_Currency);
DateTime1.Tag              := Integer(dfd_DateTime);
Fontsettings1.Tag          := Integer(dfd_Font);





end;

procedure TfrmMain.Key1Click(Sender: TObject);
begin
if KeyTree.Selected = nil then exit;

lbl_addKey.Caption := LABEL_ADDKEY;
panel_addKey.Visible := true;
frmMain.FocusControl(edit_addKey);
btn_addKey.Tag := 1;
KeyTree.Enabled := false;
end;


procedure TfrmMain.KeyTreeClick(Sender: TObject);
var t:TTreeNode;
    p:string;
begin
if not Assigned(df) then exit;

if KeyTree.Selected = nil then exit;

// build path
t := KeyTree.Selected;
p := GetPathFromNode(t);

AddressBar.Text := p;

LoadValues(p);
end;

procedure TfrmMain.KeyTreeContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var t:TTreeNode;
begin
if not Assigned(df) then Handled:=true;

Modify1.Visible := false;   // value list only
Rename1.Visible := false;   // value list only
N9.Visible := false;        // value list only
Delete1.Visible := true;
Delete2.Visible := false;   // value list only

t := KeyTree.GetNodeAt(MousePos.X, MousePos.Y);
if Assigned(t) then t.Selected := True;

// call onclick
KeyTreeClick(Sender);
end;

procedure TfrmMain.KeyTreeCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
NodeClass := TDFTreeNode;
end;

procedure TfrmMain.KeyTreeDblClick(Sender: TObject);
var t:TTreeNode;
begin
if KeyTree.Selected = nil then exit;

t := KeyTree.Selected;
if t.Parent <> nil then
   if t.Expanded then t.Collapse(false) else t.Expand(false);
end;

procedure TfrmMain.New1Click(Sender: TObject);
var frm: TfrmDFKey;
     mr: TModalResult;
begin
if SaveDialog1.Execute then
   begin
   KeyTree.Items.Clear;
   if (df is TDataFile) then df.Destroy;

   // if file exists, empty it
   if FileExists(SaveDialog1.FileName) then TFileStream.Create( SaveDialog1.FileName, fmCreate ).Free;

   frm := TfrmDFKey.Create(frmMain);
   frm.Position := TPosition.poOwnerFormCenter;
   frm.lbl_InfoNew.Visible := true;
   frm.edit_filename.Text := SaveDialog1.FileName;
   frm.edit_Password.Text := '';
   frm.ActiveControl := frm.edit_Password;
   mr := frm.ShowModal;

   df := TDataFile.Create(SaveDialog1.FileName);
   if mr = mrOk then df.CodeKey := frm.edit_Password.Text
                else df.CodeKey := '';

   frm.Free;

   if not df.SectionExists('META') then
      begin
      df.WriteDateTime('META', 'creation.date', Now() );
      end;

   LoadHive;
   end;
end;

procedure TfrmMain.Open1Click(Sender: TObject);
var frm: TfrmDFKey;
     mr: TModalResult;
begin
if OpenDialog1.Execute then
   begin
   KeyTree.Items.Clear;
   if (df is TDataFile) then df.Destroy;

   if not FileExists(OpenDialog1.FileName) then
      begin
      MessageDlg( 'File not found!', TMsgDlgType.mtError, [mbOk], 0);
      exit;
      end;


   frm := TfrmDFKey.Create(frmMain);
   frm.Position := TPosition.poOwnerFormCenter;
   frm.lbl_InfoOpen.Visible := true;
   frm.edit_filename.Text := OpenDialog1.FileName;
   frm.edit_Password.Text := '';
   frm.ActiveControl := frm.edit_Password;
   mr := frm.ShowModal;

   df := TDataFile.Create(OpenDialog1.FileName);
   if mr = mrOk then df.CodeKey := frm.edit_Password.Text
                else df.CodeKey := '';

   if df.SectionCount = 0 then
   if not df.SectionExists('META') then
      begin
      df.WriteDateTime('META', 'creation.date', Now() );
      end;

   LoadHive;
   end;
end;

procedure TfrmMain.Rename1Click(Sender: TObject);
var li: TListItem;

    t:TTreeNode;
    p:string;

    frm: TfrmRename;
begin
if not Assigned(df) then exit;
if KeyTree.Selected = nil then exit;
if ValueList.Selected = nil then exit;

// build path
t := KeyTree.Selected;
p := GetPathFromNode(t);

li := ValueList.Selected;

//RenameValue(p, li.Caption, li.SubItems.Strings[0]);
frm := TfrmRename.Create(frmMain);
frm.Position := poOwnerFormCenter;

frm.edit_OldPath.Text := p;
frm.edit_OldName.Text := li.Caption;
frm.edit_NewPath.Text := p;
frm.edit_NewName.Text := li.Caption;

frm.edit_NewName.SelectAll;

if frm.ShowModal = mrOk then
   begin
   df.Rename(frm.edit_OldPath.Text, frm.edit_OldName.Text,
             frm.edit_NewPath.Text, frm.edit_NewName.Text);

   if frm.edit_OldPath.Text <> frm.edit_NewPath.Text then
     begin
     LoadHive;
     KeyTree.Selected := GetNodeByText(KeyTree, frm.edit_NewPath.Text, true);
     AddressBar.Text := frm.edit_NewPath.Text;
     end;
   LoadValues(frm.edit_NewPath.Text);
   end;

frm.Free;

end;

procedure TfrmMain.RootKey1Click(Sender: TObject);
begin
lbl_addKey.Caption := LABEL_ADDROOT;
panel_addKey.Visible := true;
frmMain.FocusControl(edit_addKey);
btn_addKey.Tag := 0;
KeyTree.Enabled := false;

end;

procedure TfrmMain.ValueListContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var li: TListItem;
begin
if not Assigned(df) then Handled:=true;

li := ValueList.GetItemAt(MousePos.X, MousePos.Y);
if Assigned(li) then li.Selected := True;

Modify1.Visible := true;
Rename1.Visible := true;
N9.Visible := true;
Delete1.Visible := false;   // key tree only
Delete2.Visible := true;
end;

procedure TfrmMain.ValueListDblClick(Sender: TObject);
var li: TListItem;
    pos:TPoint;

    t:TTreeNode;
    p:string;

begin
if not Assigned(df) then exit;
if KeyTree.Selected = nil then exit;

// build path
t := KeyTree.Selected;
p := GetPathFromNode(t);

pos := Mouse.CursorPos;
pos := ValueList.ScreenToClient(pos); // translate to be relative to list

li := ValueList.GetItemAt(pos.X, pos.Y);
if li = nil then exit;

EditValue(p, li.Caption, li.SubItems.Strings[0]);
end;

procedure TfrmMain.addValueClick(Sender: TObject);
begin
if KeyTree.Selected = nil then exit;

panel_addValue.Visible := true;
frmMain.FocusControl(edit_addValue);
btn_addValue.Tag := (Sender as TMenuItem).Tag;
ValueList.Enabled := false;

end;

end.
