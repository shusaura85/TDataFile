{----------------------------------------------------------------
  TDataFile v2.0
  Licensed under the MIT license
  https://github.com/shusaura85/TDataFile
  Inspired by TDataFile by Alexander Momot / Degisy Software
----------------------------------------------------------------}

unit DataFile;

interface

{$mode objfpc}{$H+}

{DEFINE DF_FONT_QUALITY}

uses
  //Windows,
  SysUtils,
  System.UITypes,
  Classes,
  Graphics,
  TypInfo   // lazarus runtime type information
  {,RTTI};  // delphi runtime type information

const
  MAX_SECTIONLEN = 512;
  MAX_NAMELEN    = 256;   // original: 36
  SECTION_TEST   = '$test';

type
  DF_SECTIONNAME = array[0..MAX_SECTIONLEN - 1]of Char;
  DF_IDENTNAME = array[0..MAX_NAMELEN - 1]of Char;

  TDFType = ( // for unknown or binary data types
             dfd_Unknown = 0, dfd_Stream, dfd_Strings,
             // ansi string - unused in lazarus version
             dfd_AnsiString,
              // strings
             dfd_String,
             // boolean type
             dfd_Boolean,
             // small integers
             //(Int8 = ShortInt, Int16 = SmallInt, UInt8 = Byte, UInt16 = Word)
             dfd_Int8, dfd_Int16, dfd_UInt8, dfd_UInt16,
             // integer types (Int32 = Integer)
             dfd_Int32, dfd_Int64, dfd_UInt32, dfd_UInt64,
             // float types (currenty unused)
             dfd_Real48, dfd_Extended,
             // float types
             dfd_Single, dfd_Double, dfd_Currency,
             // other types
             dfd_DateTime, dfd_Font);

  pDataHeader = ^IDataHeader;
  IDataHeader = packed record
   Id      : Int64;
   Section : DF_SECTIONNAME; //DF_IDENTNAME;
   Ident   : DF_IDENTNAME;
   DType   : TDFType;
   Size    : Integer;
  end;

  { TDataFile }

  TDataFile = class(TObject)
  private
    FFile: TFileStream;
    FFileName: string;
    FCodeKey: string;
    function  GetSectionCount: Integer;
    function  FindIdent(Section, Ident: string; pHdr: pDataHeader): boolean;
  public
    constructor Create(const FileName: string; readonly: boolean = false);
    destructor Destroy; override;
    //----------------------------------------------------
    procedure GetSectionNames(List: TStrings);
    procedure GetValueNames(Section: string; List: TStrings);
    //----------------------------------------------------
    procedure GetValueNamesAndTypes(Section: string; List: TStrings);
    function  GetValueType(Section, Ident: string): TDFType;
    function  GetValueTypeAsString(Section, Ident: string): string;
    //----------------------------------------------------
    function  SectionExists(Section: string): Boolean;
    function  ValueExists(Section, Ident: string): Boolean;
    //----------------------------------------------------
    function  ReadData(Section, Ident: string; pBuf: Pointer): Integer;
    function  ReadStream(Section, Ident: string; Stream: TStream): Integer;
    function  ReadString(Section, Ident, Default: string): string;
    function  ReadInt8(Section, Ident: string; Default: Int8): Int8;
    function  ReadInt16(Section, Ident: string; Default: Int16): Int16;
    function  ReadUInt8(Section, Ident: string; Default: UInt8): UInt8;
    function  ReadUInt16(Section, Ident: string; Default: UInt16): UInt16;
    function  ReadInteger(Section, Ident: string; Default: Integer): Integer;
    function  ReadInt32(Section, Ident: string; Default: Int32): Int32;         // alias for ReadInteger
    function  ReadInt64(Section, Ident: string; Default: Int64): Int64;
    function  ReadUInt32(Section, Ident: string; Default: UInt32): UInt32;
    function  ReadUInt64(Section, Ident: string; Default: UInt64): UInt64;
    function  ReadSingle(Section, Ident: string; Default: Single): Single;
    function  ReadDouble(Section, Ident: string; Default: Double): Double;
    function  ReadCurrency(Section, Ident: string; Default: Currency): Currency;
    function  ReadDateTime(Section, Ident: string; Default: TDateTime): TDateTime;
    function  ReadBoolean(Section, Ident: string; Default: Boolean): Boolean;
    procedure ReadStrings(Section, Ident: string; List: TStrings);
    procedure ReadFont(Section, Ident: string; Font: TFont);
    //----------------------------------------------------
    function  WriteData(Section, Ident: string; pBuf: Pointer; Count: Integer; DataType: TDFType = dfd_Unknown): Integer;
    function  WriteStream(Section, Ident: string; Stream: TStream; DataType: TDFType = dfd_Stream): Integer;
    procedure WriteString(Section, Ident, Value: string);
    procedure WriteInt8(Section, Ident: string; Value: Int8);
    procedure WriteInt16(Section, Ident: string; Value: Int16);
    procedure WriteUInt8(Section, Ident: string; Value: UInt8);
    procedure WriteUInt16(Section, Ident: string; Value: UInt16);
    procedure WriteInteger(Section, Ident: string; Value: Integer);
    procedure WriteInt32(Section, Ident: string; Value: Int32);                 // alias for WriteInteger
    procedure WriteInt64(Section, Ident: string; Value: Int64);
    procedure WriteUInt32(Section, Ident: string; Value: UInt32);
    procedure WriteUInt64(Section, Ident: string; Value: UInt64);
    procedure WriteSingle(Section, Ident: string; Value: Single);
    procedure WriteDouble(Section, Ident: string; Value: Double);
    procedure WriteCurrency(Section, Ident: string; Value: Currency);
    procedure WriteDateTime(Section, Ident: string; Value: TDateTime);
    procedure WriteBoolean(Section, Ident: string; Value: Boolean);
    procedure WriteStrings(Section, Ident: string; List: TStrings);
    procedure WriteFont(Section, Ident: string; Font: TFont);
    //----------------------------------------------------
    procedure Delete(Section, Ident: string);
    procedure DeleteSection(Section: string);
    //----------------------------------------------------
    procedure EncryptBuf(pBuf: Pointer; BufLen: Integer); dynamic;
    procedure DecryptBuf(pBuf: Pointer; BufLen: Integer); dynamic;
    //----------------------------------------------------
    property  CodeKey: string read FCodeKey write FCodeKey;
    property  FileName: string read FFileName;
    property  SectionCount: Integer read GetSectionCount;
  end;


implementation

const
  DF_HEADER_IDENT = $4C32764644;               // 44 46 76 32 4C // "DFv2L"

type
  TDfFont = class(TFont);

  pSaveFont = ^ISaveFont;
  ISaveFont = packed record
   CharSet : TFontCharSet;
   Color   : TColor;
   Pitch   : TFontPitch;
   {$IFDEF DF_FONT_QUALITY}
   Quality : TFontQuality;
   {$ENDIF}
   Size    : Word;
   Style   : TFontStyles;
  end;

{ TDataFile }

constructor TDataFile.Create(const FileName: string; readonly: boolean = false);
var
  OpenMode: integer;
begin
  FFileName := FileName;
  if FileExists(FFileName)then
        if readonly or FileIsReadOnly(FFileName) then OpenMode := fmOpenRead or fmShareDenyNone
                                                 else OpenMode := fmOpenReadWrite or fmShareDenyNone
  else
   OpenMode := fmCreate or fmShareDenyNone;
  FFile := TFileStream.Create(FileName, OpenMode);
//  FCodeKey := 'bd@0SZxAbub$CKhp6pcR=C%j1P4%2E734wy$tqaZx%_dHj5YN1F&9TfOdM4TPfMNL&jr1x9b7kH-*O8wHyYb7DXVDx7^86%*ZxtkcUP53?qVg1i+qZlIj87nT9$^H94?';
end;

//------------------------------------------------------------------------------

destructor  TDataFile.Destroy;
begin
  if Assigned( FFile )then FFile.Free;
end;

//------------------------------------------------------------------------------

function TDataFile.FindIdent(Section, Ident: string; pHdr: pDataHeader): boolean;
var
  Sect    : string;
  Iden    : string;
  Count   : integer;
  IsError : boolean;
begin
  IsError := False;
  Result  := False;
  FFile.Seek(0, soBeginning);
  repeat
   Count  := FFile.Read(pHdr^, SizeOf(IDataHeader));
   if( Count <> SizeOf(IDataHeader))then Break;
   DecryptBuf(pHdr, SizeOf(IDataHeader));
   if( pHdr^.ID <> DF_HEADER_IDENT )then
   begin
    IsError := True;
    Break;
   end;
   Sect := pHdr^.Section;
   Iden := pHdr^.Ident;
   Result := ( ANSICompareText(Sect, Section) = 0 )and
             (( ANSICompareText(Iden, Ident) = 0 )or
             ( Ident = SECTION_TEST ));
   if( Result )then Break;
   FFile.Seek(pHdr^.Size, soCurrent);
  until( False );
  if( IsError )then raise EInvalidOperation.Create('Invalid file format.');
end;

//------------------------------------------------------------------------------

function TDataFile.GetSectionCount: Integer;
var
  Hdr    : IDataHeader;
  Count  : integer;
  IsError: boolean;
begin
  IsError := False;
  Result  := 0;
  FFile.Seek(0, soBeginning);
  repeat
   Count  := FFile.Read(Hdr, SizeOf(IDataHeader));
   if( Count <> SizeOf(IDataHeader))then Break;
   DecryptBuf(@Hdr, SizeOf(IDataHeader));
   if( Hdr.ID <> DF_HEADER_IDENT )then
   begin
    IsError := True;
    Break;
   end else inc(Result);
   FFile.Seek(Hdr.Size, soCurrent);
  until( False );
  if( IsError )then raise EInvalidOperation.Create('Invalid file format.');
end;

//------------------------------------------------------------------------------

procedure TDataFile.GetSectionNames(List: TStrings);
var
  Hdr    : IDataHeader;
  Count  : integer;
  IsError: boolean;
begin
  IsError := False;
  List.Clear;
  FFile.Seek(0, soBeginning);
  repeat
   Count  := FFile.Read(Hdr, SizeOf(IDataHeader));
   if( Count <> SizeOf(IDataHeader))then Break;
   DecryptBuf(@Hdr, SizeOf(IDataHeader));
   if( Hdr.ID <> DF_HEADER_IDENT )then
   begin
    IsError := True;
    Break;
   end else
   if( List.IndexOf(Hdr.Section) = -1 )then
   List.Add(Hdr.Section);
   FFile.Seek(Hdr.Size, soCurrent);
  until( False );
  if( IsError )then raise EInvalidOperation.Create('Invalid file format.');
end;

//------------------------------------------------------------------------------

procedure TDataFile.GetValueNames(Section: string; List: TStrings);
var
  Hdr    : IDataHeader;
  Count  : integer;
  IsError: boolean;
begin
  IsError := False;
  List.Clear;
  FFile.Seek(0, soBeginning);
  repeat
   Count  := FFile.Read(Hdr, SizeOf(IDataHeader));
   if( Count <> SizeOf(IDataHeader))then Break;
   DecryptBuf(@Hdr, SizeOf(IDataHeader));
   if( Hdr.ID <> DF_HEADER_IDENT )then
   begin
    IsError := True;
    Break;
   end else
   if ANSICompareText(Section, Hdr.Section) = 0 then
   List.Add(Hdr.Ident);
   FFile.Seek(Hdr.Size, soCurrent);
  until( False );
  if( IsError )then raise EInvalidOperation.Create('Invalid file format.');
end;

{------------------------------------------------------------------------------}
{  data types                                                                  }
{------------------------------------------------------------------------------}

procedure TDataFile.GetValueNamesAndTypes(Section: string; List: TStrings);
var
  Hdr    : IDataHeader;
  Count  : integer;
  IsError: boolean;
  dt     : string;
begin
  IsError := False;
  List.Clear;
  List.NameValueSeparator := #9;
  FFile.Seek(0, soBeginning);
  repeat
   Count  := FFile.Read(Hdr, SizeOf(IDataHeader));
   if( Count <> SizeOf(IDataHeader))then Break;
   DecryptBuf(@Hdr, SizeOf(IDataHeader));
   if( Hdr.ID <> DF_HEADER_IDENT )then
   begin
    IsError := True;
    Break;
   end else
   if ANSICompareText(Section, Hdr.Section) = 0 then
      begin
      //dt := TRttiEnumerationType.GetName(Hdr.DType);   // delphi version
      dt := GetEnumName(TypeInfo(TDFType), integer(Hdr.DType));
      dt := Copy(dt, 5, Length(dt)-4);
      List.Add(String(Hdr.Ident) + #9 + dt);
      end;
   FFile.Seek(Hdr.Size, soCurrent);
  until( False );
  if( IsError )then raise EInvalidOperation.Create('Invalid file format.');
end;

function TDataFile.GetValueType(Section, Ident: string): TDFType;
var
  Hdr: IDataHeader;
begin
  if FindIdent(Section, Ident, @Hdr) then Result := Hdr.DType
                                     else Result := TDFType.dfd_Unknown;
end;

//------------------------------------------------------------------------------

function TDataFile.GetValueTypeAsString(Section, Ident: string): string;
var
  dt: TDFType;
  s: string;
begin
  dt := GetValueType(Section, Ident);
  // s := TRttiEnumerationType.GetName(dt);   // delphi version
  s := GetEnumName(TypeInfo(TDFType), integer(dt));
  Result := Copy(s, 5, Length(s)-4);
end;

{------------------------------------------------------------------------------}
{  find                                                                        }
{------------------------------------------------------------------------------}

function TDataFile.SectionExists(Section: string): Boolean;
var
  Hdr: IDataHeader;
begin
  Result := FindIdent(Section, SECTION_TEST, @Hdr);
end;

//------------------------------------------------------------------------------

function TDataFile.ValueExists(Section, Ident: string): Boolean;
var
  Hdr: IDataHeader;
begin
  Result := FindIdent(Section, Ident, @Hdr);
end;

{------------------------------------------------------------------------------}
{  read                                                                        }
{------------------------------------------------------------------------------}

function TDataFile.ReadData(Section, Ident: string; pBuf: Pointer): Integer;
var
  Found   : boolean;
  Hdr     : IDataHeader;
begin
  Found := FindIdent(Section, Ident, @Hdr);
  if( Found )then
  begin
   Result := FFile.Read(pBuf^, Hdr.Size);
   DecryptBuf(pBuf, Hdr.Size);
  end else
   Result := -1;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadStream(Section, Ident: string; Stream: TStream): Integer;
var
  Hdr  : IDataHeader;
  pBuf : Pointer;
begin
  if( FindIdent(Section, Ident, @Hdr)  )then
  begin
   Result := Hdr.Size;
   GetMem(pBuf, Result);
   try
    FFile.Read(pBuf^, Result);
    DecryptBuf(pBuf, Result);
    Stream.Size := 0;
    Stream.Write(pBuf^, Result);
    Stream.Seek(0, soBeginning);
   finally
    FreeMem(pBuf, Result);
   end;
  end else
   Result := -1;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadString(Section, Ident, Default: string): string;
var
  Buf   : TMemoryStream;
  pBuf  : PChar;
  Count : Integer;
begin
  Buf   := TMemoryStream.Create;
  try
   Count := ReadStream(Section, Ident, Buf);
   if( Count > -1 )then
   begin
    pBuf  := StrAlloc(Count);
    try
     Buf.Seek(0, soBeginning);
     Buf.Read(pBuf^, Count);
     Result := StrPas(pBuf);
    finally
     StrDispose(pBuf);
    end;
   end else
    Result := Default;
  finally
   Buf.Free;
  end;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadInt8(Section, Ident: string; Default: Int8): Int8;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(Int8) )then
   Move(Buf, Result, SizeOf(Int8))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadInt16(Section, Ident: string; Default: Int16): Int16;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(Int16) )then
   Move(Buf, Result, SizeOf(Int16))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadUInt8(Section, Ident: string; Default: UInt8): UInt8;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(UInt8) )then
   Move(Buf, Result, SizeOf(UInt8))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadUInt16(Section, Ident: string; Default: UInt16): UInt16;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(UInt16) )then
   Move(Buf, Result, SizeOf(UInt16))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadInteger(Section, Ident: string; Default: Integer): Integer;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(Integer) )then
   Move(Buf, Result, SizeOf(Integer))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadInt32(Section, Ident: string; Default: Int32): Int32;
begin
Result := ReadInteger(Section, Ident, Default);
end;

//------------------------------------------------------------------------------

function TDataFile.ReadInt64(Section, Ident: string; Default: Int64): Int64;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(Int64) )then
   Move(Buf, Result, SizeOf(Int64))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadUInt32(Section, Ident: string; Default: UInt32): UInt32;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(UInt32) )then
   Move(Buf, Result, SizeOf(UInt32))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadUInt64(Section, Ident: string; Default: UInt64): UInt64;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(UInt64) )then
   Move(Buf, Result, SizeOf(UInt64))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadSingle(Section, Ident: string; Default: Single): Single;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(Single) )then
   Move(Buf, Result, SizeOf(Single))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadDouble(Section, Ident: string; Default: Double): Double;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(Double) )then
   Move(Buf, Result, SizeOf(Double))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadCurrency(Section, Ident: string; Default: Currency): Currency;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(Currency) )then
   Move(Buf, Result, SizeOf(Currency))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadDateTime(Section, Ident: string; Default: TDateTime): TDateTime;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(TDateTime) )then
   Move(Buf, Result, SizeOf(TDateTime))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

function TDataFile.ReadBoolean(Section, Ident: string; Default: Boolean): Boolean;
var
  Buf   : array[0..1023]of Char;
  Count : Integer;
begin
  Count := ReadData(Section, Ident, @Buf);
  if( Count >= SizeOf(Boolean) )then
   Move(Buf, Result, SizeOf(Boolean))
  else
   Result := Default;
end;

//------------------------------------------------------------------------------

procedure TDataFile.ReadStrings(Section, Ident: string; List: TStrings);
var
  Buf   : TMemoryStream;
  Count : Integer;
begin
  List.Clear;
  Buf := TMemoryStream.Create;
  try
   Count := ReadStream(Section, Ident, Buf);
   if( Count > -1 )then
   List.LoadFromStream( Buf );
  finally
   Buf.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TDataFile.ReadFont(Section, Ident: string; Font: TFont);
var
  Buf   : TMemoryStream;
  pPos  : PChar;
  pBuf  : Pointer;
  Count : Integer;
  FontChange: TNotifyEvent;
begin
  Buf := TMemoryStream.Create;
  try
   Count := ReadStream(Section, Ident, Buf);
   if( Count > SizeOf(ISaveFont))then
   begin
    GetMem(pBuf, Count);
    FontChange := Font.OnChange;
    try
     Buf.Seek(0, soBeginning);
     Buf.Read(pBuf^, Count);
     Font.OnChange := nil;
     Font.Charset := pSaveFont(pBuf)^.CharSet;
     Font.Color := pSaveFont(pBuf)^.Color;
     Font.Pitch := pSaveFont(pBuf)^.Pitch;
     {$IFDEF DF_FONT_QUALITY}
     Font.Quality := pSaveFont(pBuf)^.Quality;
     {$ENDIF}
     Font.Size  := pSaveFont(pBuf)^.Size;
     Font.Style := pSaveFont(pBuf)^.Style;
     pPos := pBuf;
     inc(pPos, SizeOf(ISaveFont));
     Font.Name := StrPas(pPos);
    finally
     Font.OnChange := FontChange;
     TDfFont(Font).Changed;
     FreeMem(pBuf, Count);
    end;
   end;
  finally
   Buf.Free;
  end;
end;

{------------------------------------------------------------------------------}
{  write                                                                       }
{------------------------------------------------------------------------------}

function TDataFile.WriteData(Section, Ident: string; pBuf: Pointer; Count: Integer; DataType: TDFType = dfd_Unknown): Integer;
var
  Hdr : IDataHeader;
  P   : Pointer;
begin
  Delete(Section, Ident);
  FFile.Seek(0, soEnd);
  { feel header }
  Hdr.Id := DF_HEADER_IDENT;
  StrPCopy(Hdr.Section, Section);
  StrPCopy(Hdr.Ident, Ident);
  Hdr.DType := DataType;
  Hdr.Size := Count;
  { xor }
  EncryptBuf(@Hdr, SizeOf(IDataHeader));
  { write header }
  Result := FFile.Write(Hdr, SizeOf(IDataHeader));
  if( Result > -1 )then
  begin
   GetMem(P, Count);
   try
    Move(pBuf^, P^, Count);
    { xor data }
    EncryptBuf(P, Count);
    { write data }
    Result := FFile.Write(P^, Count);
   finally
    FreeMem(P, Count);
   end;
  end;
end;

//------------------------------------------------------------------------------

function TDataFile.WriteStream(Section, Ident: string; Stream: TStream; DataType: TDFType = dfd_Stream): Integer;
var
  pBuf : Pointer;
begin
  { init buffer }
  GetMem(pBuf, Stream.Size);
  try
   Stream.Seek(0, soBeginning);
   Stream.Read(pBuf^, Stream.Size);
   { write data }
   Result := WriteData(Section, Ident, pBuf, Stream.Size, DataType);
  finally
   FreeMem(pBuf, Stream.Size);
  end;
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteString(Section, Ident, Value: string);
var
  pBuf : pChar;
begin
  if Length(Value) > 0 then pBuf := StrNew(PChar(Value))
                       else pBuf := PChar('');
  try
//   WriteData(Section, Ident, pBuf, StrLen(pBuf) + 1);   //ByteLength
   WriteData(Section, Ident, pBuf, (StrLen(pBuf) + 1)*Sizeof(PChar), TDFType.dfd_String);
//   WriteData(Section, Ident, pBuf, ByteLength(Value) + SizeOf(PChar), TDFType.dfd_String);
  finally
   StrDispose(pBuf);
  end;
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteInt8(Section, Ident: string; Value: Int8);
begin
  WriteData(Section, Ident, @Value, SizeOf(Int8), TDFType.dfd_Int8);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteInt16(Section, Ident: string; Value: Int16);
begin
  WriteData(Section, Ident, @Value, SizeOf(Int16), TDFType.dfd_Int16);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteUInt8(Section, Ident: string; Value: UInt8);
begin
  WriteData(Section, Ident, @Value, SizeOf(UInt8), TDFType.dfd_UInt8);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteUInt16(Section, Ident: string; Value: UInt16);
begin
  WriteData(Section, Ident, @Value, SizeOf(UInt16), TDFType.dfd_UInt16);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteInteger(Section, Ident: string; Value: Integer);
begin
  WriteData(Section, Ident, @Value, SizeOf(Integer), TDFType.dfd_Int32);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteInt32(Section, Ident: string; Value: Int32);
begin
  WriteInteger(Section, Ident, Value);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteInt64(Section, Ident: string; Value: Int64);
begin
  WriteData(Section, Ident, @Value, SizeOf(Int64), TDFType.dfd_Int64);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteUInt32(Section, Ident: string; Value: UInt32);
begin
  WriteData(Section, Ident, @Value, SizeOf(UInt32), TDFType.dfd_UInt32);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteUInt64(Section, Ident: string; Value: UInt64);
begin
  WriteData(Section, Ident, @Value, SizeOf(UInt64), TDFType.dfd_UInt64);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteSingle(Section, Ident: string; Value: Single);
begin
  WriteData(Section, Ident, @Value, SizeOf(Single), TDFType.dfd_Single);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteDouble(Section, Ident: string; Value: Double);
begin
  WriteData(Section, Ident, @Value, SizeOf(Double), TDFType.dfd_Double);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteCurrency(Section, Ident: string; Value: Currency);
begin
  WriteData(Section, Ident, @Value, SizeOf(Currency), TDFType.dfd_Currency);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteDateTime(Section, Ident: string; Value: TDateTime);
begin
  WriteData(Section, Ident, @Value, SizeOf(TDateTime), TDFType.dfd_DateTime);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteBoolean(Section, Ident: string; Value: Boolean);
begin
  WriteData(Section, Ident, @Value, SizeOf(Boolean), TDFType.dfd_Boolean);
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteStrings(Section, Ident: string; List: TStrings);
var
  Buf   : TMemoryStream;
begin
  Buf := TMemoryStream.Create;
  try
   List.SaveToStream( Buf );
   WriteStream(Section, Ident, Buf, TDFType.dfd_Strings);
  finally
   Buf.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TDataFile.WriteFont(Section, Ident: string; Font: TFont);
var
  pBuf: Pointer;
  pPos: PChar;
  Len : Integer;
begin
  //Len := SizeOf(ISaveFont) + (Length(Font.Name) + 1)*SizeOf(String);
  Len := SizeOf(ISaveFont) + (ByteLength(Font.Name) + 1);
  GetMem(pBuf, Len);
  try
   pSaveFont(pBuf)^.CharSet := Font.Charset;
   pSaveFont(pBuf)^.Color := Font.Color;
   pSaveFont(pBuf)^.Pitch := Font.Pitch;
   {$IFDEF DF_FONT_QUALITY}
   pSaveFont(pBuf)^.Quality := Font.Quality;
   {$ENDIF}
   pSaveFont(pBuf)^.Size  := Font.Size;
   pSaveFont(pBuf)^.Style := Font.Style;
   pPos := pBuf;
   inc(pPos, SizeOf(ISaveFont));
   StrPCopy(pPos, Font.Name);
   WriteData(Section, Ident, pBuf, Len, TDFType.dfd_Font);
  finally
   FreeMem(pBuf, Len);
  end;
end;

{------------------------------------------------------------------------------}
{  delete                                                                      }
{------------------------------------------------------------------------------}

procedure TDataFile.Delete(Section, Ident: string);
var
  BufPos   : Int64; //Integer;
  HdrPos   : Int64; //Integer;
  EndPos   : Int64; //Integer;
  FileSize : Int64; //Integer;
  Count    : Int64; //Integer;
  Hdr      : IDataHeader;
  pBuf     : Pointer;
begin
  if( FindIdent(Section, Ident, @Hdr) )then
  begin
   FileSize := FFile.Size;
   BufPos   := FFile.Position;
   HdrPos   := BufPos - SizeOf(IDataHeader);
   { seek to end buffer }
   EndPos   := FFile.Seek(Hdr.Size, soCurrent);
   Count    := FileSize - EndPos;
   GetMem(pBuf, Count);
   try
    FFile.Read(pBuf^, Count);
    FFile.Seek(HdrPos, soBeginning);
    FFile.Write(pBuf^, Count);
    FFile.Size := FileSize - ( Hdr.Size + SizeOf(IDataHeader) );
   finally
    FreeMem(pBuf, Count);
   end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDataFile.DeleteSection(Section: string);
var
  BufPos : Int64; //Integer;
  HdrPos : Int64; //Integer;
  EndPos : Int64; //Integer;
  Size   : Int64; //Integer;
  Count  : Int64; //Integer;
  Hdr    : IDataHeader;
  pBuf   : Pointer;
begin
  while FindIdent(Section, SECTION_TEST, @Hdr)do
  begin
   Size := FFile.Size;
   BufPos := FFile.Position;
   HdrPos := BufPos - SizeOf(IDataHeader);
   { Seek to end buffer }
   EndPos := FFile.Seek(Hdr.Size, soCurrent);
   Count  := Size - EndPos;
   GetMem(pBuf, Count);
   try
    FFile.Read(pBuf^, Count);
    FFile.Seek(HdrPos, soBeginning);
    FFile.Write(pBuf^, Count);
    FFile.Size := Size - ( Hdr.Size + SizeOf(IDataHeader) );
   finally
    FreeMem(pBuf, Count);
   end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDataFile.DecryptBuf(pBuf: Pointer; BufLen: Integer);
var
  I: Integer;
  P: pByte;
begin
  P := pBuf;
  if( FCodeKey <> '' )then
  for I := 0 to BufLen - 1 do
  begin
   P^ := Byte(FCodeKey[1 + ((I - 1) mod Length(FCodeKey))]) xor P^;
   inc(P);
  end;
end;

//------------------------------------------------------------------------------

procedure TDataFile.EncryptBuf(pBuf: Pointer; BufLen: Integer);
begin
  DecryptBuf(pBuf, BufLen);
end;

//------------------------------------------------------------------------------

end.
