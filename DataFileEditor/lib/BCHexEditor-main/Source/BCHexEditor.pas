unit BCHexEditor;

{$R *.res}

interface

uses
  Windows, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Grids;

type
  // @exclude
  TGridCoord = Grids.TGridCoord;

  // character conversion type
  TBCHCharConvType = (cctFromAnsi, cctToAnsi);
  // character conversion table
  TBCHCharConvTable = array[0..255] of AnsiChar;
  // character conversion data storage
  TBCHCharConv = array[TBCHCharConvType] of TBCHCharConvTable;

const
  // block size in file i/o
  BCH_FILEIO_BLOCKSIZE = $F000;

  // this message is posted to the hex editor when it should update the caret position
  CM_INTUPDATECARET = CM_BASE + $100;

  // this message is posted when an OnSelectionChange event is to be fired
  CM_SELECTIONCHANGED = CM_BASE + $101;

  (* translation tables from/to ms windows ansi (~ MS Latin-1)  *)

  // macintosh..ms ansi conversion
  BCH_CCONV_MAC: TBCHCharConv = (
    //ansi to mac
    (#$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$09, #$0A, #$0B,
    #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1A, #$1B,
    #$1C, #$1D, #$1E, #$1F,
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27, #$28, #$29, #$2A, #$2B,
    #$2C, #$2D, #$2E, #$2F,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, #$38, #$39, #$3A, #$3B,
    #$3C, #$3D, #$3E, #$3F,
    #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$4A, #$4B,
    #$4C, #$4D, #$4E, #$4F,
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$5B,
    #$5C, #$5D, #$5E, #$5F,
    #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$6A, #$6B,
    #$6C, #$6D, #$6E, #$6F,
    #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77, #$78, #$79, #$7A, #$7B,
    #$7C, #$7D, #$7E, #$7F,
    #$C4, #$C5, #$AB, #$C9, #$D1, #$F7, #$DC, #$E1, #$E0, #$E2, #$E4, #$E3,
    #$AC, #$B0, #$AA, #$F8,
    #$D5, #$CE, #$C3, #$CF, #$D3, #$D4, #$D2, #$DB, #$DA, #$DD, #$F6, #$F5,
    #$FA, #$F9, #$FB, #$FC,
    #$A0, #$C1, #$A2, #$A3, #$DF, #$B4, #$B6, #$A4, #$C6, #$A9, #$BB, #$C7,
    #$C2, #$AD, #$A8, #$FF,
    #$A1, #$B1, #$B2, #$B3, #$A5, #$B5, #$A6, #$B7, #$B8, #$B9, #$BC, #$C8,
    #$BA, #$BD, #$CA, #$C0,
    #$CB, #$E7, #$E5, #$CC, #$80, #$81, #$AE, #$82, #$E9, #$83, #$E6, #$E8,
    #$ED, #$EA, #$EB, #$EC,
    #$D0, #$84, #$F1, #$EE, #$EF, #$CD, #$85, #$D7, #$AF, #$F4, #$F2, #$F3,
    #$86, #$D9, #$DE, #$A7,
    #$88, #$87, #$89, #$8B, #$8A, #$8C, #$BE, #$8D, #$8F, #$8E, #$90, #$91,
    #$93, #$92, #$94, #$95,
    #$F0, #$96, #$98, #$97, #$99, #$9B, #$9A, #$D6, #$BF, #$9D, #$9C, #$9E,
    #$9F, #$FD, #$FE, #$D8
    ),
    // mac to ansi
    (#$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$09, #$0A, #$0B,
    #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1A, #$1B,
    #$1C, #$1D, #$1E, #$1F,
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27, #$28, #$29, #$2A, #$2B,
    #$2C, #$2D, #$2E, #$2F,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, #$38, #$39, #$3A, #$3B,
    #$3C, #$3D, #$3E, #$3F,
    #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$4A, #$4B,
    #$4C, #$4D, #$4E, #$4F,
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$5B,
    #$5C, #$5D, #$5E, #$5F,
    #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$6A, #$6B,
    #$6C, #$6D, #$6E, #$6F,
    #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77, #$78, #$79, #$7A, #$7B,
    #$7C, #$7D, #$7E, #$7F,
    #$C4, #$C5, #$C7, #$C9, #$D1, #$D6, #$DC, #$E1, #$E0, #$E2, #$E4, #$E3,
    #$E5, #$E7, #$E9, #$E8,
    #$EA, #$EB, #$ED, #$EC, #$EE, #$EF, #$F1, #$F3, #$F2, #$F4, #$F6, #$F5,
    #$FA, #$F9, #$FB, #$FC,
    #$A0, #$B0, #$A2, #$A3, #$A7, #$B4, #$B6, #$DF, #$AE, #$A9, #$8E, #$82,
    #$8C, #$AD, #$C6, #$D8,
    #$8D, #$B1, #$B2, #$B3, #$A5, #$B5, #$A6, #$B7, #$B8, #$B9, #$BC, #$AA,
    #$BA, #$BD, #$E6, #$F8,
    #$BF, #$A1, #$AC, #$92, #$80, #$81, #$A8, #$AB, #$BB, #$83, #$BE, #$C0,
    #$C3, #$D5, #$91, #$93,
    #$D0, #$84, #$96, #$94, #$95, #$90, #$F7, #$D7, #$FF, #$DD, #$98, #$97,
    #$86, #$99, #$DE, #$A4,
    #$88, #$87, #$89, #$8B, #$8A, #$C2, #$CA, #$C1, #$CB, #$C8, #$CD, #$CE,
    #$CF, #$CC, #$D3, #$D4,
    #$F0, #$D2, #$DA, #$DB, #$D9, #$9B, #$9A, #$85, #$8F, #$9D, #$9C, #$9E,
    #$9F, #$FD, #$FE, #$AF
    )
    );

  // ebcdic cp38..ms ansi conversion
  BCH_CCONV_BCD38: TBCHCharConv = (
    //ansi to bcd (taken from recode 3.5)
    (#$00, #$01, #$02, #$03, #$37, #$2D, #$2E, #$2F, #$16, #$05, #$25, #$0B,
    #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$3C, #$3D, #$32, #$26, #$18, #$19, #$3F, #$27,
    #$1C, #$1D, #$1E, #$1F,
    #$40, #$4F, #$7F, #$7B, #$5B, #$6C, #$50, #$7D, #$4D, #$5D, #$5C, #$4E,
    #$6B, #$60, #$4B, #$61,
    #$F0, #$F1, #$F2, #$F3, #$F4, #$F5, #$F6, #$F7, #$F8, #$F9, #$7A, #$5E,
    #$4C, #$7E, #$6E, #$6F,
    #$7C, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, #$C8, #$C9, #$D1, #$D2,
    #$D3, #$D4, #$D5, #$D6,
    #$D7, #$D8, #$D9, #$E2, #$E3, #$E4, #$E5, #$E6, #$E7, #$E8, #$E9, #$4A,
    #$E0, #$5A, #$5F, #$6D,
    #$79, #$81, #$82, #$83, #$84, #$85, #$86, #$87, #$88, #$89, #$91, #$92,
    #$93, #$94, #$95, #$96,
    #$97, #$98, #$99, #$A2, #$A3, #$A4, #$A5, #$A6, #$A7, #$A8, #$A9, #$C0,
    #$20, #$D0, #$A1, #$07,
    #$80, #$22, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$8A, #$8B,
    #$8C, #$8D, #$8E, #$8F,
    #$90, #$77, #$2C, #$0A, #$3B, #$3E, #$1A, #$70, #$71, #$72, #$9A, #$9B,
    #$9C, #$9D, #$9E, #$9F,
    #$A0, #$15, #$73, #$74, #$75, #$76, #$6A, #$78, #$09, #$3A, #$AA, #$AB,
    #$AC, #$AD, #$AE, #$AF,
    #$B0, #$B1, #$B2, #$B3, #$B4, #$B5, #$B6, #$B7, #$B8, #$B9, #$BA, #$BB,
    #$BC, #$BD, #$BE, #$BF,
    #$23, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$CA, #$CB,
    #$CC, #$CD, #$CE, #$CF,
    #$1B, #$24, #$06, #$14, #$28, #$2B, #$21, #$17, #$51, #$52, #$DA, #$DB,
    #$DC, #$DD, #$DE, #$DF,
    #$2A, #$E1, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$29, #$EA, #$EB,
    #$EC, #$ED, #$EE, #$EF,
    #$30, #$31, #$08, #$33, #$34, #$35, #$36, #$04, #$38, #$39, #$FA, #$FB,
    #$FC, #$FD, #$FE, #$FF
    ),
    // bcd to ansi (taken from recode 3.5)
    (#$00, #$01, #$02, #$03, #$F7, #$09, #$D2, #$7F, #$F2, #$A8, #$93, #$0B,
    #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$D3, #$A1, #$08, #$D7, #$18, #$19, #$96, #$D0,
    #$1C, #$1D, #$1E, #$1F,
    #$7C, #$D6, #$81, #$C0, #$D1, #$0A, #$17, #$1B, #$D4, #$E9, #$E0, #$D5,
    #$92, #$05, #$06, #$07,
    #$F0, #$F1, #$16, #$F3, #$F4, #$F5, #$F6, #$04, #$F8, #$F9, #$A9, #$94,
    #$14, #$15, #$95, #$1A,
    #$20, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, #$C8, #$C9, #$5B, #$2E,
    #$3C, #$28, #$2B, #$21,
    #$26, #$D8, #$D9, #$E2, #$E3, #$E4, #$E5, #$E6, #$E7, #$E8, #$5D, #$24,
    #$2A, #$29, #$3B, #$5E,
    #$2D, #$2F, #$82, #$83, #$84, #$85, #$86, #$87, #$88, #$89, #$A6, #$2C,
    #$25, #$5F, #$3E, #$3F,
    #$97, #$98, #$99, #$A2, #$A3, #$A4, #$A5, #$91, #$A7, #$60, #$3A, #$23,
    #$40, #$27, #$3D, #$22,
    #$80, #$61, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$8A, #$8B,
    #$8C, #$8D, #$8E, #$8F,
    #$90, #$6A, #$6B, #$6C, #$6D, #$6E, #$6F, #$70, #$71, #$72, #$9A, #$9B,
    #$9C, #$9D, #$9E, #$9F,
    #$A0, #$7E, #$73, #$74, #$75, #$76, #$77, #$78, #$79, #$7A, #$AA, #$AB,
    #$AC, #$AD, #$AE, #$AF,
    #$B0, #$B1, #$B2, #$B3, #$B4, #$B5, #$B6, #$B7, #$B8, #$B9, #$BA, #$BB,
    #$BC, #$BD, #$BE, #$BF,
    #$7B, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$CA, #$CB,
    #$CC, #$CD, #$CE, #$CF,
    #$7D, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F, #$50, #$51, #$52, #$DA, #$DB,
    #$DC, #$DD, #$DE, #$DF,
    #$5C, #$E1, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$EA, #$EB,
    #$EC, #$ED, #$EE, #$EF,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, #$38, #$39, #$FA, #$FB,
    #$FC, #$FD, #$FE, #$FF
    )
    );

type
  // custom Exception class
  EBCHexEditor = class(Exception);

  (* bookmark record:<br>
     defined by pressing SHIFT+CTRL+[0..9], goto bookmark by pressing CTRL+[0..9]<br><br>

     - mPosition: file position<br>
     - mInCharField: cursor in character pane (True) or hex number pane
  *)
  TBCHBookmark = record
    mPosition: integer;
    mInCharField: boolean;
  end;

  // array of bookmarks, representing keys 0..9
  TBCHBookmarks = array[0..9] of TBCHBookmark;

  (* look of the editor's caret:<br>
     - ckFull: full block<br>
     - ckLeft: left line<br>
     - ckBottom: bottom line<br>
     - ckAuto: left line if @link(InsertMode), full block if overwrite,
               bottom line if ReadOnlyView
  *)
  TBCHCaretKind = (ckFull,
    ckLeft,
    ckBottom,
    ckAuto
    );

  (* how to show a file's content in the character pane of the editor:<br>
    - tkAsIs:    leave as is (current windows code page)<br>
    - tkDos8:    current dos codepage<br>
    - tkASCII:   7 bit ascii<br>
    - tkMac:     macintosh charset (translation always from/to ms cp 1252 (ms latin1)!!<br>
    - tkBCD:     ibm ebcdic codepage 38 (translation always from/to ms cp 1252 (ms latin1)!!<br>
    - tkCustom:  custom codepage stored in @link(BCHCustomCharConv)
  *)
  TBCHTranslationKind = (tkAsIs,
    tkDos8,
    tkASCII,
    tkMac,
    tkBCD

    , tkCustom

    );

  (* action indicator used in @link(OnProgress) event handler:<br>
    - pkLoad:  loading data<br>
    - pkSave:  saving data<br>
    - pkFind:  finding
  *)
  TBCHProgressKind = (pkLoad,
    pkSave, pkFind
    );

  (* progress event handler, used in @link(OnProgress)<br><br>

       - ProgressType: am i loading or saving? (see @link(TBCHProgressKind))<br>
       - aName: name of file to be load from/saved to<br>
       - Percent: current progress (0..100)<br>
       - Cancel: if set to true, the load/save procedure will abort (no meaning in Find* methods) <br>
  *)
  TBCHProgressEvent = procedure(Sender: TObject;
    const ProgressType: TBCHProgressKind;
    const aName: TFileName;
    const Percent: byte;
    var Cancel: boolean) of object;

  (* retrieve the "line number" to display by the application<br><br>

       - Number: the number to convert to text
       - OffsetText: the resulting text output
  *)
  TBCPHGetOffsetTextEvent = procedure(Sender: TObject;
    const Number: int64;
    var OffsetText: string) of object;

  //@exclude
  // flags internally used in the undo storage
  TBCHUndoFlag = (
    // kind of undo storage
    ufKindBytesChanged,
    ufKindByteRemoved,
    ufKindInsertBuffer,
    ufKindReplace,
    ufKindAppendBuffer,
    ufKindNibbleInsert,
    ufKindNibbleDelete,
    ufKindConvert,
    ufKindSelection, // store a selection
    ufKindCombined,
    ufKindAllData, // store current data and size for complete undo
    // additional information
    ufFlagByte1Changed,
    ufFlagByte2Changed,
    ufFlagModified,
    ufFlag2ndByteCol,
    ufFlagInCharField,
    ufFlagHasSelection,
    ufFlagInsertMode,
    ufFlagIsUnicode,
    ufFlagIsUnicodeBigEndian,
    ufFlagHasDescription
    );

  //@exclude
  // set of undo flags
  TBCHUndoFlags = set of TBCHUndoFlag;

type
  // persistent color storage (contains the colors in hex editors)
  TBCHColors = class(TPersistent)
  private const
    DefaultActiveFieldBackground = clWindow;
    DefaultBackground = clWindow;
    DefaultChangedBackground = $A8FFFF;
    DefaultChangedText = clMaroon;
    DefaultCurrentOffset = clBtnHighlight;
    DefaultCurrentOffsetBackground = clBtnShadow;
    DefaultCursorFrame = clNavy;
    DefaultEvenColumn = clNavy;
    DefaultGrid = clBtnFace;
    DefaultNonFocusCursorFrame = clAqua;
    DefaultOddColumn = clBlue;
    DefaultOffset = clBlack;
    DefaultOffsetBackground = clBtnFace;
  private
    FParent: TControl;
    FOffset: TColor;
    FOddColumn: TColor;
    FEvenColumn: TColor;
    FCursorFrame: TColor;
    FNonFocusCursorFrame: TColor;
    FBackground: TColor;
    FChangedText: TColor;
    FChangedBackground: TColor;
    FCurrentOffsetBackground: TColor;
    FOffsetBackground: TColor;
    FActiveFieldBackground: TColor;
    FCurrentOffset: TColor;
    FGrid: TColor;

    procedure SetOffsetBackground(const Value: TColor);
    procedure SetCurrentOffset(const Value: TColor);
    procedure SetParent(const Value: TControl);
    procedure SetGrid(const Value: TColor);
    procedure SetBackground(const Value: TColor);
    procedure SetChangedBackground(const Value: TColor);
    procedure SetChangedText(const Value: TColor);
    procedure SetCursorFrame(const Value: TColor);
    procedure SetEvenColumn(const Value: TColor);
    procedure SetOddColumn(const Value: TColor);
    procedure SetOffset(const Value: TColor);
    procedure SetActiveFieldBackground(const Value: TColor);
    procedure SetCurrentOffsetBackground(const Value: TColor);
    procedure SetNonFocusCursorFrame(const Value: TColor);
  protected
    function IsStored(): boolean;
  public
    // @exclude(constructor)
    constructor Create(Parent: TControl);
    // @exclude()
    procedure Assign(Source: TPersistent); override;
    // parent hex editor control
    property Parent: TControl read FParent write SetParent;
  published
    // background color of the active field (hex/chars)
    property ActiveFieldBackground: TColor read FActiveFieldBackground write SetActiveFieldBackground default DefaultActiveFieldBackground;
    // background color
    property Background: TColor read FBackground write SetBackground default DefaultBackground;
    // background color of modified bytes (in overwrite mode)
    property ChangedBackground: TColor read FChangedBackground write SetChangedBackground default DefaultChangedBackground;
    // foreground color of modified bytes (in overwrite mode)
    property ChangedText: TColor read FChangedText write SetChangedText default DefaultChangedText;
    // foreground color of the current line in the offset pane (gutter)
    property CurrentOffset: TColor read FCurrentOffset write SetCurrentOffset default DefaultCurrentOffset;
    // background color of the current line in the offset pane (gutter)
    property CurrentOffsetBackground: TColor read FCurrentOffsetBackground write SetCurrentOffsetBackground default DefaultCurrentOffsetBackground;
    // color of the cursor and position frame in the second pane
    property CursorFrame: TColor read FCursorFrame write SetCursorFrame default DefaultCursorFrame;
    // foreground color of even columns
    property EvenColumn: TColor read FEvenColumn write SetEvenColumn default DefaultEvenColumn;
    // pen color of the grid
    property Grid: TColor read FGrid write SetGrid default DefaultGrid;
    // color of a cursor frame in a non-focused editor
    property NonFocusCursorFrame: TColor read FNonFocusCursorFrame write SetNonFocusCursorFrame default DefaultNonFocusCursorFrame;
    // foreground color of odd columns
    property OddColumn: TColor read FOddColumn write SetOddColumn default DefaultOddColumn;
    // foreground color of the line offsets
    property Offset: TColor read FOffset write SetOffset default DefaultOffset;
    // background color of the offset pane (gutter)
    property OffsetBackground: TColor read FOffsetBackground write SetOffsetBackground default DefaultOffsetBackground;

  end;

  // @exclude(stream class for internal storage/undo)
  TBCHMemoryStream = class(TMemoryStream)
  private

    function PointerAt(const APosition, ACount: Integer): Pointer;
  protected

  public

    function GetAddress(const Index, Count: integer): PByte;
    procedure ReadBufferAt(var Buffer; const APosition, ACount: Integer);
    procedure WriteBufferAt(const Buffer; const APosition, ACount: Integer);
    procedure Move(const AFromPos, AToPos, ACount: Integer);
    procedure TranslateToAnsi(const FromTranslation: TBCHTranslationKind; const
      APosition, ACount: integer);
    procedure TranslateFromAnsi(const ToTranslation: TBCHTranslationKind; const
      APosition, ACount: integer);
    function GetAsHex(const APosition, ACount: integer; const SwapNibbles:
      Boolean): string;
  end;

  //@exclude
  // undo storage implementation
  TBCHUndoStorage = class;

  //@exclude
  // offset format flags
  TBCHOffsetFormatFlag = (offCalcWidth,
    // calculate minwidth depending on data size (width field = '-')
    offCalcRow,
    // calculate _BytesPerUnit depending on bytes per row (=real line numbers)
    offCalcColumn, // " bytes per column (= column numbers)
    offBytesPerUnit // use BytesPerUnit property
    );

  //@exclude
  // set of the above flags
  TBCHOffsetFormatFlags = set of TBCHOffsetFormatFlag;

  //@exclude
  // offset format record
  TBCHOffsetFormat = record
    Format: string; // format as string
    Prefix,
      Suffix: string; // splitted format
    MinWidth: integer; // min length of value (zero padded on the left)
    Flags: // auto calculation flags
    TBCHOffsetFormatFlags;
    Radix, // radix (base) of display (2..16)
    _BytesPerUnit: byte; // length of one unit (1 Byte...BytesPerRow Bytes)
  end;

  (* owner draw event type. parameters:<br><br>
     - Sender: the hex editor<br>
     - ACanvas: the editor's canvas<br>
     - ACol, ARow: the position to be drawn<br>
     - AWideText: the text to be drawn<br>
     - ARect: the cell rectangle<br>
     - ADefaultDraw: if set to True (default), default drawing isperformed after the event handler returns.
       if set to false, the event handler must do all cell painting.
  *)
  TBCHDrawCellEvent = procedure(Sender: TObject; ACanvas: TCanvas; ACol, ARow:
    Integer; var AWideText: WideString; ARect: TRect; var ADefaultDraw: Boolean)
    of object;

  // protected ancestor of the hex editor components

  TCustomBCHexEditor = class(TCustomGrid)

  protected const
    DefaultBytesPerCol = 2;
    DefaultBytesPerColumn = DefaultBytesPerCol div 2;
    DefaultBytesPerRow = 16;
    DefaultCursor = crIBeam;
    DefaultDrawGridLines = False;
    DefaultFocusFrame = True;
    DefaultFontName = 'Courier New';
    DefaultFontSize = 11;
    DefaultAllowInsertMode = True;
    DefaultOffsetFormat = '-!10:0x|';
    DefaultTranslation = tkAsIs;
  private

    FIsViewSyncing: boolean;
    FIntLastHexCol: integer;
    FIsMaxOffset: boolean;
    FBlockSize: Integer;
    FSepCharBlocks: boolean;
    FOnGetOffsetText: TBCPHGetOffsetTextEvent;
    FFixedFileSize: boolean;
    FCharWidth,
      FCharHeight: integer;
    FBookmarkImageList: TImageList;
    FInsertModeOn: boolean;
    FCaretBitmap: TBitmap;
    FColors: TBCHColors;
    FBytesPerRow: integer;
    FOffSetDisplayWidth: integer;
    FBytesPerRowDup: integer;
    FDataStorage: TBCHMemoryStream;
    FSwapNibbles: integer;
    FFocusFrame: boolean;
    FIsFileReadonly: boolean;
    FBytesPerCol: integer;
    FPosInCharField,
      FLastPosInCharField: boolean;
    FFileName: string;
    FModifiedBytes: TBits;
    FBookmarks: TBCHBookmarks;
    FSelStart,
      FSelPosition,
      FSelEnd: integer;
    FSelBeginPosition: integer;
    FTranslation: TBCHTranslationKind;
    FCaretKind: TBCHCaretKind;
    FReplaceUnprintableCharsBy: char;
    FAllowInsertMode: boolean;
    FWantTabs: boolean;
    FReadOnlyView: boolean;
    FHideSelection: boolean;
    FGraySelOnLostFocus: boolean;
    FOnProgress: TBCHProgressEvent;
    FMouseDownCol,
      FMouseDownRow: integer;
    FShowDrag: boolean;
    FDropCol,
      FDropRow: integer;
    FOnInvalidKey,
      FOnTopLeftChanged: TNotifyEvent;

    FAutoBytesPerRow: boolean;
    FSetAutoBytesPerRow: boolean;
    FDrawGridLines: boolean;
    FDrawGutter3D: boolean;
    FGutterWidth: integer;
    FOffsetFormat: TBCHOffsetFormat;
    FSelectionPossible: boolean;
    FBookmarkBitmap: TBitmap;
    FCursorList: array of integer;
    FHasCustomBMP: boolean;
    FStreamFileName: string;
    FHasFile: boolean;
    FMaxUndo: integer;
    FHexChars: array[0..15] of char;
    FHexLowerCase: boolean;
    FOnChange: TNotifyEvent;
    FShowRuler: boolean;
    FBytesPerUnit: Integer;
    FRulerBytesPerUnit: Integer;
    FOnSelectionChanged: TNotifyEvent;
    FSelectionChangedCount: Integer;
    FShowPositionIfNotFocused: Boolean;
    FOffsetHandler: Boolean;
    FUsedRulerBytesPerUnit: Integer;
    FIsSelecting: boolean;
    FMouseUpCanResetSel: boolean;
    FUndoStorage: TBCHUndoStorage;
    FUnicodeCharacters: Boolean;
    FUnicodeBigEndian: Boolean;
    FMaskedChars: string;

    FDrawDataPosition: integer;
    FDrawDataPositionIsHex: boolean;
    FOnDrawCell: TBCHDrawCellEvent;

    FOnBookmarkChanged: TNotifyEvent;

    FIsDrawDataSelected: boolean;

    FSetDataSizeFillByte: Byte;
    FRulerNumberBase: byte;
    property Color;

    function IsInsertModePossible: boolean;

    procedure RecalcBytesPerRow;
    function IsColorsStored(): boolean;
    function IsFileSizeFixed: boolean;
    function IsFontStored(): boolean;
    procedure InternalErase(const KeyWasBackspace: boolean; const UndoDesc:
      string = '');
    procedure SetReadOnlyView(const Value: boolean);
    procedure SetCaretKind(const Value: TBCHCaretKind);
    procedure SetFocusFrame(const Value: boolean);
    procedure SetBytesPerColumn(const Value: integer);
    procedure SetSwapNibbles(const Value: boolean);
    function GetSwapNibbles: boolean;
    function GetBytesPerColumn: integer;
    procedure SetOffsetDisplayWidth;
    procedure SetColors(const Value: TBCHColors);
    procedure SetReadOnlyFile(const Value: boolean);
    procedure SetTranslation(const Value: TBCHTranslationKind);
    procedure SetModified(const Value: boolean);
    procedure SetChanged(DataPos: integer; const Value: boolean);
    procedure SetFixedFileSize(const Value: boolean);
    procedure SetAllowInsertMode(const Value: boolean);
    function GetInsertMode: boolean;
    procedure SetWantTabs(const Value: boolean);
    procedure SetHideSelection(const Value: boolean);
    procedure SetGraySelectionIfNotFocused(const Value: boolean);
    function CalcColCount: integer;
    function GetLastCharCol: integer;
    function GetPropColCount: integer;
    function GetPropRowCount: integer;
    function GetMouseOverSelection: boolean;
    function CursorOverSelection(const X, Y: integer): boolean;
    function MouseOverFixed(const X, Y: integer): boolean;
    procedure AdjustBookmarks(const From, Offset: integer);
    procedure IntSetCaretPos(const X, Y, ACol: integer);
    procedure TruncMaxPosition(var DataPos: integer);
    procedure SetSelection(DataPos, StartPos, EndPos: integer);
    function GetCurrentValue: integer;
    procedure SetInsertMode(const Value: boolean);
    function GetModified: boolean;
    //function GetDataPointer: Pointer;
    procedure SetBytesPerRow(const Value: integer);
    procedure SetMaskChar(const Value: char);
    procedure SetAsText(const Value: AnsiString);
    procedure SetAsHex(const Value: string);
    function GetAsText: AnsiString;
    function GetAsHex: string;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    // show or hide caret depending on row/col in view
    procedure CheckSetCaret;
    // get the row according to the given buffer position
    function GetRow(const DataPos: integer): integer;
    // invalid key pressed (in ebcdic)
    procedure WrongKey;

    // create an inverting caret bitmap
    procedure CreateCaretGlyph;
    // get start of selection
    function GetSelStart: integer;
    // get end of selection
    function GetSelEnd: integer;
    // get selection count
    function GetSelCount: integer;
    // set selection start
    procedure SetSelStart(aValue: integer);
    // set selection end
    procedure SetSelEnd(aValue: integer);
    procedure SetSelCount(aValue: integer);
    // position the caret in the given field
    procedure SetInCharField(const Value: boolean);
    // is the caret in the char field ?
    function GetInCharField: boolean;
    // insert a buffer (internal)
    procedure InternalInsertBuffer(Buffer: PAnsiChar; const Size, Position:
      integer);
    // append some data (int)
    procedure InternalAppendBuffer(Buffer: PAnsiChar; const Size: integer);
    // store the caret properties
    procedure InternalGetCurSel(var StartPos, EndPos, ACol, ARow: integer);
    // delete data
    procedure InternalDelete(StartPos, EndPos, ACol, ARow: integer);
    // delete one half byte
    function InternalDeleteNibble(const Pos: integer;
      const HighNibble: boolean): boolean;
    // insert half byte
    function InternalInsertNibble(const Pos: integer; const HighNibble:
      boolean): boolean;
    // used by nibble functions
    function CreateShift4BitStream(const StartPos: integer; var FName:
      TFileName): TFileStream;
    // convert a given amount of data from ansi to something different and vice versa
    procedure InternalConvertRange(const aFrom, aTo: integer; const aTransFrom,
      aTransTo: TBCHTranslationKind);
    function IsOffsetFormatStored(): boolean;
    // move data in buffer to a different position
    procedure MoveFileMem(const aFrom, aTo, aCount: integer);
    function GetBookmark(Index: byte): TBCHBookmark;
    procedure SetBookmark(Index: byte; const Value: TBCHBookmark);
    procedure SetBookmarkVals(const Index: byte; const Position: integer; const
      InCharField: boolean);
    procedure SetDrawGridLines(const Value: boolean);
    procedure SetGutterWidth(const Value: integer);
    // images have changed
    procedure BookmarkBitmapChanged(Sender: TObject);
    procedure SetBookmarkBitmap(const Value: TBitmap);

    // free alloc'd memory of one of the storage streams;
    procedure FreeStorage(FreeUndo: boolean = False);
    function GetCanUndo: boolean;
    function GetCanRedo: boolean;
    function GetUndoDescription: string;
    function GetOffsetFormat: string;
    procedure SetOffsetFormat(const Value: string);
    // generate offset format
    procedure GenerateOffsetFormat(Value: string);
    procedure SetHexLowerCase(const Value: boolean);
    procedure SetDrawGutter3D(const Value: boolean);
    procedure SetShowRuler(const Value: boolean);
    procedure SetBytesPerUnit(const Value: integer);
    procedure SetRulerString;
    procedure CheckSelectUnit(var AStart, AEnd: Integer);
    procedure SetRulerBytesPerUnit(const Value: integer);
    procedure SetShowPositionIfNotFocused(const Value: Boolean);
    function GetDataAt(Index: integer): Byte;
    procedure SetDataAt(Index: integer; const Value: Byte);
    procedure SetUnicodeCharacters(const Value: Boolean);
    procedure SetUnicodeBigEndian(const Value: Boolean);
    procedure SetAutoBytesPerRow(const Value: Boolean);
    function GetPositionAtCursor(const ACol, ARow: integer): integer;
    function GetIsCharFieldCol(const ACol: integer): Boolean;
    procedure SetDataSize(const Value: integer);
    procedure SetBlockSize(const Value: Integer);
    procedure SetSepCharBlocks(const Value: boolean);
    procedure SetRulerNumberBase(const Value: byte);
    procedure SetMaskedChars(const Value: string);
  protected
    // @exclude()
    FRulerString: string;
    // @exclude()
    FRulerCharString: string;

    // @exclude(used by TBCHexEditorEx for internal drag 'n' drop)
    FFixedFileSizeOverride: boolean;
    // @exclude(used by TBCHexEditorEx for internal undo changing)
    FModified: boolean;

    // @exclude(overwrite mouse wheel for zooming)
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean;
      override;
    // @exclude(overwrite mouse wheel for zooming)
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean;
      override;
    // @exclude(actually used bytes per unit)
    property UsedRulerBytesPerUnit: Integer read FUsedRulerBytesPerUnit;
    // @exclude(True: cells are currently to be selected)
    property IsSelecting: boolean read FIsSelecting;
    // @exclude(True: MouseUp resets selection)
    property MouseUpCanResetSel: boolean read FMouseUpCanResetSel write
      FMouseUpCanResetSel;
    // @exclude(memory stream which contains the undo/redo data)
    property UndoStorage: TBCHUndoStorage read FUndoStorage;
    // @exclude(stream that contains the data)
    property DataStorage: TBCHMemoryStream read FDataStorage;
    // @exclude(fire OnSelectionChange)
    procedure SelectionChanged; virtual;
    // @exclude(set a new selection)
    procedure NewSelection(SelFrom, SelTo: integer);
    // @exclude(get the current mouse position)
    function CheckMouseCoord(var X, Y: integer): TGridCoord;
    // @exclude(assure the value is a multiple of FBytesPerUnit)
    procedure CheckUnit(var AValue: Integer);
    // call changed on every undo creation for OnChange event
    procedure Changed; virtual;
    // returns the drop file position after a drag'n'drop operation
    function DropPosition: integer;
    // copy a stream to a second one and fire the OnProgress handler
    procedure Stream2Stream(strFrom, strTo: TStream; const Operation:
      TBCHProgressKind; const Count: integer = -1);
    (* allows descendants to take special action if contents are to be saved
     to the file from where the data was load *)
    procedure PrepareOverwriteDiskFile; virtual;
    // store the current Cursor and set it to crHourGlass (see also @link(OldCursor))
    procedure WaitCursor;
    // reset the Cursor to the previous value (see also @link(WaitCursor))
    procedure OldCursor;
    // @exclude(override paint)
    procedure Paint; override;
    // @exclude(view changed)
    procedure TopLeftChanged; override;
    // adjust cell widths/heigths depending on font, offset format, bytes per row/column...
    procedure AdjustMetrics;
    // get the size of the contained data
    function GetDataSize: integer;
    // @exclude(calculate the grid sizes)
    procedure CalcSizes;
    // @exclude(select one cell)
    function SelectCell(ACol, ARow: longint): boolean; override;
    // @exclude(get the data position depending on col and row)
    function GetPosAtCursor(const aCol, aRow: integer): integer;
    // @exclude(vice versa)
    function GetCursorAtPos(const aPos: integer; const aChars: boolean):
      TGridCoord;
    // @exclude(get the column of the other field (hex<->char))
    function GetOtherFieldCol(const aCol: integer): integer;
    // @exclude(get the column of the other field (hex<->char))
    function GetOtherFieldColCheck(const aCol: integer): integer;
    // @exclude(can the cell be selected ?)
    function CheckSelectCell(aCol, aRow: integer): boolean;
    // @exclude(char message handler)
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    // @exclude(ime char message handler)
    procedure WMImeChar(var Msg: TWMChar); message WM_IME_CHAR;
    // @exclude(posted message to update the caret position)
    procedure CMINTUPDATECARET(var Msg: TMessage); message CM_INTUPDATECARET;
    // @exclude(posted message to fire an OnSelectionChanged event)
    procedure CMSelectionChanged(var Msg: TMessage); message
      CM_SELECTIONCHANGED;
    // @exclude(for shortcuts)
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    // @exclude(readjust grid sizes after font has changed)
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    // @exclude(change a byte at the given position)
    procedure IntChangeByte(const aOldByte, aNewByte: byte;
      aPos, aCol, aRow: integer; const UndoDesc: string = '');
    // @exclude(change two bytes at the given position)
    procedure IntChangeWideChar(const aOldChar, aNewChar: WideChar; aPos, aCol,
      aRow: integer; const UndoDesc: string = '');
    // @exclude(keydown handler)
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    // @exclude(keyup handler)
    //procedure KeyUp(var Key: word; Shift: TShiftState); override;
    // @exclude(has this byte been modified ?)
    function HasChanged(aPos: integer): boolean;
    // @exclude(make a selection)
    procedure Select(const aCurCol, aCurRow, aNewCol, aNewRow: integer);
    // @exclude(mouse down handler)
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      integer); override;
    // @exclude(mouse move handler)
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    // @exclude(mouse up handler)
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    // @exclude(is undo record creation possible?)
    function CanCreateUndo(const aKind: TBCHUndoFlag; const aCount, aReplCount:
      integer): Boolean; virtual;
    // @exclude(add an undo to the undo buffer)
    procedure CreateUndo(const aKind: TBCHUndoFlag; const aPos, aCount,
      aReplCount: integer; const sDesc: string = '');
    // @exclude(after loading)
    procedure Loaded; override;
    // @exclude(override CreateWnd)
    procedure CreateWnd; override;
    // @exclude(wm_setfocus handler)
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    // @exclude(wm_killfocus handler)
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    // @exclude(wm_vscroll handler)
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    // @exclude(wm_hscroll handler)
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    // @exclude(resize the control)
    procedure Resize; override;
    // @exclude(store bitmap ? (its set to true, if a custom bitmap has been stored in BookmarkBitmap))
    function HasCustomBookmarkBitmap: boolean;
    // automatically calculate @link(BytesPerRow) depending on the width of the editor
    property AutoBytesPerRow: boolean read FAutoBytesPerRow write SetAutoBytesPerRow default False;
    // number of bytes to show in each row
    property BytesPerRow: integer read FBytesPerRow write SetBytesPerRow default DefaultBytesPerRow;
    // number of bytes to show in each column
    property BytesPerColumn: integer read GetBytesPerColumn write SetBytesPerColumn default DefaultBytesPerColumn;
    (* translation kind of the data (used to show characters on and to handle key presses in the char pane),
       (see also @link(TBCHTranslationKind))
    *)
    property Translation: TBCHTranslationKind read FTranslation write SetTranslation default DefaultTranslation;
    (* offset display ("line numbers") format, in the form<br>
       [r|c|&lt;HEXNUM&gt;%][-|&lt;HEXNUM&gt;!]&lt;HEXNUM&gt;:[Prefix]|[Suffix]<br>
       (&lt;HEXNUM&gt; means a number in hexadecimal format (without prefix/suffix))<br><br>
       - first field (up to the percent sign):<br>
       <ul>
       <li>sets the "bytes per unit field" of the offset display format</li>
       <li>if it's set to 1, each row offset displays the data position in bytes</li>
       <li>if it's set to 2, each row offset displays the data position in words</li>
       <li>if it's set to 4, each row offset displays the data position in dwords</li>
       <li>if it's set to "r", each row offset displays the current row number (1st row=0,
       see also @link(BytesPerRow))</li>
       <li>if it's set to "c", each row offset displays the current column number (1st column=0,
       see also @link(BytesPerColumn))</li>
       <li>if this field is omitted, bytes per unit is set to the value of the
       @link(RulerBytesPerUnit) property</li>
       </ul><br>
       - second field (up to the exclamation mark):<br>
       <ul>
       <li>sets the minimum width of the number part, if the number is shorter, it will be padded
       by '0' chars at the left</li>
       <li>if this field reads -!, the the minimum width is automatically set to the longest number
       that can appear in the editor (the data's size)</li>
       <li>if this field is omitted, the minimum width is set to 1</li>
       </ul><br>
       - third field (up to the colon):<br>
       <ul>
       <li>sets the radix (base) of the offset format in hex notation</li>
       <li>set this to '10' (without quotes) for hexadecimal offset display, set it to '08' for
       octal and to '0a' for decimal offset display</li>
       <li>this field cannot be omitted, but the whole format string my be blank to avoid the display of
       offset identifiers</li>
       </ul></br>
       - fourth field (up to the pipe ('|') char):<br>
       <ul>
       <li>the prefix that is put in front of the "number" string (e.g. '0x' or '$' to show that numbers are in hex format)
       </li><li>this field may be omitted (but not the pipe char!)</li>
       </ul><br>
       - fifth (and last) field:<br>
       <ul>
       <li>the suffix to put after the "number string" (e.g. 'h' to show hex numbers)</li>
       <li>this field may be omitted</li></ul>
    *)
    property OffsetFormat: string read GetOffsetFormat write SetOffsetFormat stored IsOffsetFormatStored;

    (* if this handler is assigned, the @link(OffsetFormat) is not used to
       create "line numbers", but the application tells the editor how to format the offset text
    *)
    property OnGetOffsetText: TBCPHGetOffsetTextEvent read FOnGetOffsetText write
      FOnGetOffsetText;

    (* how many bytes form one block in a row? blocks are separated by a one character wide blank.
       -1 means no block separation (see also @link(SeparateBlocksInCharField)) *)
    property BytesPerBlock: Integer read FBlockSize write SetBlockSize default
      -1;

    (* if @link(BytesPerBlock) is used, this property tells the editor whether it should
       separate blocks of bytes in the character pane too or not *)
    property SeparateBlocksInCharField: boolean read FSepCharBlocks write
      SetSepCharBlocks default True;

    // look of the editor's caret (see @link(TBCHCaretKind))
    property CaretKind: TBCHCaretKind read FCaretKind write SetCaretKind default
      ckAuto;
    // colors to display (see @link(TBCHColors))
    property Colors: TBCHColors read FColors write SetColors stored IsColorsStored;
    (* if FocusFrame is set to True, the current caret position will be displayed in the
       second field (hex - characters) as a dotted focus frame, if set to False, it will
       be shown as an ordinary rectangle
    *)
    property FocusFrame: boolean read FFocusFrame write SetFocusFrame default DefaultFocusFrame;
    (* if SwapNibbles is set to True, the hex pane will show all bytes in the order
       lower 4 bits-higher 4 bits (i.e. the value 192 dec = C0 hex will be drawn as
       0C). if set to False, hex values will be displayed in usual order. this
       setting also affects hex data input and hex-string conversions
    *)
    property SwapNibbles: boolean read GetSwapNibbles write SetSwapNibbles
      default False;
    // replace @link(MaskedChars) with the following character in the character pane
    property MaskChar: char read FReplaceUnprintableCharsBy write SetMaskChar
      stored False;
    (* if set to True, the data size is readonly, e.g. no data may be appended, deleted
       or inserted, just overwriting is allowed. this also affects @link(InsertMode).
    *)
    property NoSizeChange: boolean read FFixedFileSize write SetFixedFileSize
      default False;
    (* if set to False, switching between overwrite and insert mode is not allowed
       (see also @link(InsertMode) and @link(NoSizeChange))
    *)
    property AllowInsertMode: boolean read FAllowInsertMode write
      SetAllowInsertMode default DefaultAllowInsertMode;
    (* if set to True, the Tab key is used to switch the caret between hex and character pane.
       if set to False, the Tab key can be used to switch between controls. then the
       combination CTRL+T is used to switch the panes
    *)
    property WantTabs: boolean read FWantTabs write SetWantTabs default True;
    // if set to True, the data can not be edited, just cursor movement is allowed ("Hex Viewer" mode)
    property ReadOnlyView: boolean read FReadOnlyView write SetReadOnlyView
      default False;
    // hide the current selection when the hex editor looses focus (see also @link(GraySelectionIfNotFocused))
    property HideSelection: boolean read FHideSelection write SetHideSelection
      default False;
    (* if set to True and @link(HideSelection) is False, then the current selection will be
       grayed when the hex editor looses focus (the values from the @link(Colors) property will
       be converted to grayscale colors)
    *)
    property GraySelectionIfNotFocused: boolean read FGraySelOnLostFocus write
      SetGraySelectionIfNotFocused default False;
    (* this event is called in @link(LoadFromFile), @link(SaveToFile), @link(Find) and
       @link(FindWithWildcard) routines, so a progress indicator may be updated
       (see also @link(TBCHProgressEvent), @link(FindProgress))
    *)
    property OnProgress: TBCHProgressEvent read FOnProgress write
      FOnProgress;
    (* this event is fired if an invalid character has been typed (like non-hex characters
       in the hex pane)
    *)
    property OnInvalidKey: TNotifyEvent read FOnInvalidKey write FOnInvalidKey;
    // this event is fired if the first visible row or column have been changed (e.g. on scrolling)
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write
      FOnTopLeftChanged;
    // returns the current selection in hex format ('00010203...') as string, uses @link(SwapNibbles)
    function GetSelectionAsHex: string;
    (* replace the current selection by a string containing data in hex format ('00 01 02 03' or similar),
      uses @link(SwapNibbles)
    *)
    procedure SetSelectionAsHex(const s: string);
    // returns a string containing the currently selected data
    function GetSelectionAsText: AnsiString;
    // replaces the currently selected data with the string's contents
    procedure SetSelectionAsText(const s: AnsiString);
    // if set to True, a grid is drawn
    property DrawGridLines: boolean read FDrawGridLines write SetDrawGridLines default DefaultDrawGridLines;
    // width of the offset display gutter, if set to -1, automatically adjust the gutter's width
    property GutterWidth: integer read FGutterWidth write SetGutterWidth default
      -1;
    (* bitmap containing 20 10x10 pixels pictures for bokkmarks (they are displayed in the offset
      gutter), the first ten pictures represent the bookmarks 0(10)..9, if they are set in the
      hexpane, the last 10 pics are shown if bookmarks are set in the character pane (see also
      @link(TBCHBookMark))
    *)
    property BookmarkBitmap: TBitmap read FBookmarkBitmap write SetBookmarkBitmap
      stored HasCustomBookmarkBitmap;

    // maximum memory that is used for undo storage (in bytes, approximately)
    property MaxUndo: integer read FMaxUndo write FMaxUndo default 1024 * 1024;
    (* insert mode (typed characters are inserted at the current position) or
       overwrite mode (typed characters replace values at the current position), see also
       @link(AllowInsertMode), @link(NoSizeChange) and @link(ReadOnlyView)
    *)
    property InsertMode: boolean read GetInsertMode write SetInsertMode default
      False;
    // if set to True, hex data and hex offsets are displayed in lower case
    property HexLowerCase: boolean read FHexLowerCase write SetHexLowerCase
      default False;
    // this event is called on every data change (load/empty/undo/redo)
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    // if set to True, a 3d line is drawn at the right of the offset gutter
    property DrawGutter3D: boolean read FDrawGutter3D write SetDrawGutter3D
      default True;
    // if set to True, a ruler is shown above the first row
    property ShowRuler: boolean read FShowRuler write SetShowRuler default
      False;
    (* number base (i.e. radix) for the ruler display (2-16), tells the component
       which number format to use when drawing the ruler
    *)
    property RulerNumberBase: byte read FRulerNumberBase write SetRulerNumberBase
      default 16;
    (* setting this property changes the way how mouse/keyboard selection
       works:<br>
       e.g. if set to two, two bytes will be treated as a unit, that means you
       cannot select a single byte, only two, four, six... bytes can be selected.
       also drag/drop and clipboard pasting is affected (data size
       is always a multiple of BytesPerUnit). See also @link(RulerBytesPerUnit)
    *)
    property BytesPerUnit: integer read FBytesPerUnit write SetBytesPerUnit
      default 1;
    (* setting this property affects the offset/ruler drawing:<br>
       e.g. if set to two, two bytes will be treated as a unit, that means the
       offset and ruler values will step by one each two bytes.
       if this property is set to -1, it will use the value of the
       @link(BytesPerUnit) property
    *)
    property RulerBytesPerUnit: integer read FRulerBytesPerUnit write
      SetRulerBytesPerUnit default -1;
    // mark the current position even if the editor is not focused
    property ShowPositionIfNotFocused: Boolean read FShowPositionIfNotFocused
      write SetShowPositionIfNotFocused default False;
    (* if set to True, the character pane displays unicode characters
       and the @link(BytesPerUnit) property is set to 2. @link(Translation) is
       set to tkAsIs. @link(BytesPerRow) and @link(BytesPerColumn) must be a
       multiple of two to be able to use the unicode mode.
       see also @link(UnicodeBigEndian)
    *)
    property UnicodeChars: Boolean read FUnicodeCharacters write
      SetUnicodeCharacters default False;
    (* if set to True, big endian unicode mode is used if @link(UnicodeChars) is
       enabled
    *)
    property UnicodeBigEndian: Boolean read FUnicodeBigEndian write
      SetUnicodeBigEndian default False;
    // this event is fired when the selection/caret position has changed
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write
      FOnSelectionChanged;

    // use this event to implement owner drawing. see also @link(TBCHDrawCellEvent)
    property OnDrawCell: TBCHDrawCellEvent read FOnDrawCell write FOnDrawCell;

    // fire OnBookmarkChanged
    procedure BookmarkChanged; virtual;

    procedure DoSetCellWidth(const Index: integer; Value: integer);
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadMaskChar(Reader: TReader);
    procedure ReadMaskChar_I(Reader: TReader);
    procedure WriteMaskChar_I(Writer: TWriter);
  public
    { Public-Deklarationen }

    // return the memory address at the given stream position after checking bounaries
    function GetFastPointer(const Index, Count: integer): PByte;
    //@exclude()
    constructor Create(aOwner: TComponent); override;
    //@exclude()
    destructor Destroy; override;
    // these characters are masked in the character pane using @link(MaskChar)
    property MaskedChars: string read FMaskedChars write SetMaskedChars;
    (* during OnDrawCell event handlers, this property tells the data position currently
       being drawn (-1, if offset or ruler are drawn)
    *)
    property DrawDataPosition: integer read FDrawDataPosition;

    (* during OnDrawCell event handlers, this property tells whether the cell is
       to be drawn in selected style (only valid if DrawDataPosition <> -1)
    *)
    property IsDrawDataSelected: boolean read FIsDrawDataSelected;

    // @exclude(use TBCHexEditor.ReadBuffer!)
    function GetMemory(const Index: Integer): char;
    (* @exclude(see http://info.borland.com/devsupport/delphi/fixes/delphi4/vcl.html,
      ref 279)
    *)

    function CanFocus: Boolean; override;
    // @exclude(use TBCHexEditor.WriteBuffer!)
    procedure SetMemory(const Index: integer; const Value: char);

    (* this property is valid only in the @link(OnGetOffsetText) event. if True,
       the component asks for the string of the highest possible offset, if False,
       a row's offset text is queried
    *)
    property IsMaxOffset: boolean read FIsMaxOffset;
    // seek behind the last position if @link(InsertMode) = True, goto last position otherwise
    procedure SeekToEOF;
    (* synchronize another TCustomBCHexEditor view (top, left, selection),
       the optional SyncOffset parameter may be used for a different viewpoint
    *)
    procedure SyncView(Source: TCustomBCHexEditor; SyncOffset: integer = 0);
    // return the offset of the first displayed data
    function DisplayStart: integer;
    // return the offset of the last displayed data
    function DisplayEnd: integer;
    // is the given position part of the selection?
    function IsSelected(const APosition: integer): boolean;
    // calculate a data position from a col/row pair
    property PositionAtCursor[const ACol, ARow: integer]: integer read
    GetPositionAtCursor;
    // is the given col in the hex or the character pane?
    property IsCharFieldCol[const ACol: integer]: Boolean read
    GetIsCharFieldCol;
    // this byte value is used to fill the data when setting @link(DataSize)
    // enlarges the stream
    property SetDataSizeFillByte: Byte read FSetDataSizeFillByte write
      FSetDataSizeFillByte;
    // has data been load from/saved to a file (or is the filename valid)
    property HasFile: boolean read FHasFile write FHasFile;
    (* each call to UndoBeginUpdate increments an internal counter that prevents using
       undo storage and also disables undo functionality (see also @link(UndoEndUpdate))
    *)
    function UndoBeginUpdate: integer; virtual;
    (* each call to UndoEndUpdate decrements an internal counter that prevents using
       undo storage and also disables undo functionality. the return value is the value
       of this counter. if the counter is reset to zero, undo creation is permitted again
       (see also @link(UndoBeginUpdate))
    *)
    function UndoEndUpdate: integer; virtual;
    // remove selection state from all data
    procedure ResetSelection(const aDraw: boolean);
    // see @link(GetSelectionAsHex) and @link(SetSelectionAsHex)
    property SelectionAsHex: string read GetSelectionAsHex write
      SetSelectionAsHex;
    // see @link(GetSelectionAsText) and @link(SetSelectionAsText)
    property SelectionAsText: AnsiString read GetSelectionAsText write
      SetSelectionAsText;
    function GetOffsetString(const Position: cardinal): string; virtual;
    (* returns the given position as it would be drawn in the offset gutter, exception:
      if @link(OffsetFormat) is set to an empty string, returns the hexadecimal representation
      of the Position value (see also @link(GetOffsetString))
    *)
    function GetAnyOffsetString(const Position: integer): string; virtual;
    // returns the height of one row in pixels
    function RowHeight: integer;
    // free the undo storage (discard all possible undo steps)
    procedure ResetUndo;
    // set the current position (like TStream.Seek)
    function Seek(const aOffset, aOrigin: integer): integer;
    (* searches for text or data in the data buffer, returns the find position (-1, if data have not been found):<br><br>
       - aBuffer: data to search for<br>
       - aCount: size of data in aBuffer<br>
       - aStart: start search at this position<br>
       - aEnd: searches up to this position<br>
       - IgnoreCase: if True, lowercase and uppercase characters are treated as if they were equal<br>
       - SearchText: if True, the current @link(Translation) is taken into account when searching textual data<br><br>
    *)
    function Find(aPattern: Pointer; aCount: integer; const aStart,
      aEnd: integer): integer; overload;
    function Find(aPattern: string; const aStart, aEnd: integer;
      const IgnoreCase: boolean): integer; overload;
    (* searches for text or data in the data buffer using a wildcard character
       returns the find position (-1, if data have not been found):<br><br>
       - aPattern: data to search for<br>
       - aCount: size of data in aBuffer<br>
       - aStart: start search at this position<br>
       - aEnd: searches up to this position<br>
       - IgnoreCase: if True, lowercase and uppercase characters are treated as if they were equal<br><br>
    *)
    (*
      store a selection as undo record, so you can restore the selection start and end by using
      @link(Undo). this can be useful e.g. to show position of replaced data
    *)
    procedure AddSelectionUndo(const AStart, ACount: integer);
    // read data into a buffer
    procedure ReadBuffer(var Buffer; const Index, Count: Integer);
    // write a buffer to the file data
    procedure WriteBuffer(const Buffer; const Index, Count: Integer); virtual;
    // delete the currently selected data
    procedure DeleteSelection(const UndoDesc: string = '');
    // load the contents of a stream into the data buffer
    procedure LoadFromStream(Strm: TStream);
    // load the contents of a file into the data buffer
    procedure LoadFromFile(const Filename: string);
    // save the contents of the data buffer into a stream
    procedure SaveToStream(Strm: TStream);
    // save the contents of the data buffer to a file
    procedure SaveToFile(const Filename: string;
      const aUnModify: boolean = True);
    // save a range of bytes to a stream
    procedure SaveRangeToStream(Strm: TStream; const APosition, ACount:
      integer);
    // undo the last modification, multiple undos are possible
    function Undo: boolean;
    // discard the last undo action (only one single redo is possible)
    function Redo: boolean;
    // empty the data buffer and set the filename (e.g. "Untitled")
    procedure CreateEmptyFile(const TempName: string);
    (* returns a buffer containing parts of the data buffer's contents. the buffer is allocated
       in this routine and must be freed by the caller
    *)
    function BufferFromFile(const aPos: integer; var aCount: integer): PChar;
    // insert some data at the specified position into the data buffer
    procedure InsertBuffer(aBuffer: PAnsiChar; const aSize, aPos: integer; const
      UndoDesc: string = ''; const MoveCursor: Boolean = True);
    // append some data at the end of the data buffer
    procedure AppendBuffer(aBuffer: PAnsiChar; const aSize: integer; const UndoDesc:
      string = ''; const MoveCursor: Boolean = True);
    // replace the currently selected data with some other data
    procedure ReplaceSelection(aBuffer: PAnsiChar; aSize: integer; const UndoDesc:
      string = ''; const MoveCursor: Boolean = True);
    // replace some amount of data
    function Replace(aBuffer: PChar; aPosition, aOldCount, aNewCount: integer;
      const UndoDesc:
      string = ''; const MoveCursor: Boolean = False): integer;
    // get the current data position (depending on the cursor/caret)
    function GetCursorPos: integer;
    // delete 4 bits (=half byte = nibble) from the data buffer (see also @link(InsertNibble))
    function DeleteNibble(const aPos: integer; const HighNibble: boolean; const
      UndoDesc: string = ''): boolean;
    // insert 4 bits (0000) into the data buffer (see also @link(DeleteNibble))
    function InsertNibble(const aPos: integer; const HighNibble: boolean; const
      UndoDesc: string = ''): boolean;
    // convert a part of the data buffer's content from one character table to a different one
    procedure ConvertRange(const aFrom, aTo: integer; const aTransFrom,
      aTransTo: TBCHTranslationKind; const UndoDesc: string = '');
    (* returns the data position of the top left cell and also whether the caret is in the
       character pane, see also @link(SetTopLeftPosition)
    *)
    function GetTopLeftPosition(var oInCharField: boolean): integer;
    (* set top left cell to the given data position and also whether the caret is in the
       character pane (see also @link(GetTopLeftPosition))
    *)
    procedure SetTopLeftPosition(const aPosition: integer; const aInCharField:
      boolean);
    (* show a drop position marker on the cell at the given mouse cursor position
      (see also @link(HideDragCell))
    *)
    function ShowDragCell(const X, Y: integer): integer;
    // hide the drop position marker (see also @link(ShowDragCell))
    procedure HideDragCell;
    // combine two or more changes, so @link(Undo) will discard the at once
    procedure CombineUndo(const aCount: integer; const sDesc: string = '');
    (* translate a byte from the current @link(Translation) to the Windows Codepage
      (see also @link(TranslateFromAnsiChar))
    *)
    function TranslateToAnsiChar(const aByte: byte): WideChar;
    (* translate a byte from Windows Codepage to the current @link(Translation)
      (see also @link(TranslateToAnsiChar))
    *)
    function TranslateFromAnsiChar(const aByte: byte): WideChar;
    // retrieve or set the selection start
    property SelStart: integer read GetSelStart write SetSelStart;
    // retrieve or set the selection end
    property SelEnd: integer read GetSelEnd write SetSelEnd;
    // retrieve or set the size of the selected data
    property SelCount: integer read GetSelCount write SetSelCount;
    // is @link(Undo) possible?
    property CanUndo: boolean read GetCanUndo;
    // is @link(Redo) possible?
    property CanRedo: boolean read GetCanRedo;
    // is the caret in the character or the hex pane ?
    property InCharField: boolean read GetInCharField write SetInCharField;
    // description of the next @link(Undo) action
    property UndoDescription: string read GetUndoDescription;
    // if True, the currently loaded file cannot be overwritten
    property ReadOnlyFile: boolean read FIsFileReadonly write SetReadOnlyFile;
    // if True, changes have been made to the data buffer content
    property Modified: boolean read GetModified write SetModified;
    // retrieves or stores the amount of data in the data buffer
    // when enlarging the data stream, the @link(SetDataSizeFillByte) property
    // tells which value to use to fill the new data
    property DataSize: integer read GetDataSize write SetDataSize;
    // array to the data buffer's content
    property Data[Index: integer]: Byte read GetDataAt write SetDataAt;
    // retrieve or set the data as string
    property AsText: AnsiString read GetAsText write SetAsText;
    // retrieve or set the data as hex formatted string (00 01 02 03...)
    property AsHex: string read GetAsHex write SetAsHex;
    // name of the file that has been loaded into the data buffer
    property Filename: string read FFileName;
    // retrieve or set bookmarks programmatically (see also @link(TBCHBookmark))
    property Bookmark[Index: byte]: TBCHBookmark read GetBookmark write
    SetBookmark;
    // has the byte at the given position been modified ? (only in overwrite mode)
    property ByteChanged[index: integer]: boolean read HasChanged write
    SetChanged;
    // retrieves the number of columns (grid columns)
    property ColCountRO: integer read GetPropColCount;
    // retrieves the number of rows (grid rows)
    property RowCountRO: integer read GetPropRowCount;
    // returns True if the mouse cursor is positionned over selected data
    property MouseOverSelection: boolean read GetMouseOverSelection;
    // get the data value at the current caret position, returns -1 if an error occured
    property CurrentValue: integer read GetCurrentValue;
    // pointer to the whole data buffer's contents
    //property DataPointer: Pointer read GetDataPointer;
    // select all data
    procedure SelectAll;
    // retrieves the number of visible columns
    property VisibleColCount;
    // retrieves the number of visible rows
    property VisibleRowCount;
    // the control's canvas
    property Canvas;
    // current column (grid column)
    property Col;
    // first visible column
    property LeftCol;
    // current row (grid row)
    property Row;
    // first visible row (grid row)
    property TopRow;
    // this event is fired when a bookmark is added/modifed/removed
    property OnBookmarkChanged: TNotifyEvent read FOnBookmarkChanged write
      FOnBookmarkChanged;
    // call this procedure to navigate to a bookmarked position
    function GotoBookmark(const Index: integer): boolean;
    // call this function if the external offset formatting changed (see @link(OnGetOffsetText))
    procedure UpdateGetOffsetText;
    // center the current position vertically
    procedure CenterCursorPosition;


  published
    property Cursor default DefaultCursor;
    property Font stored IsFontStored;
    property ParentFont default False;

  end;

  // published hex editor component
  TBCHexEditor = class(TCustomBCHexEditor)
  published
    // @exclude(inherited)
    property Align;
    // @exclude(inherited)
    property Anchors;
    // @exclude(inherited)
    property BiDiMode;
    // @exclude(inherited)
    property BorderStyle;
    // @exclude(inherited)
    property Constraints;
    // @exclude(inherited)
    property Ctl3D;
    // @exclude(inherited)
    property DragCursor;
    // @exclude(inherited)
    property DragKind;
    // @exclude(inherited)
    property DragMode;
    // @exclude(inherited)
    property Enabled;
    // @exclude(inherited)
    property Font;
    // @exclude(inherited)
    property ImeMode;
    // @exclude(inherited)
    property ImeName;
    // @exclude(inherited)
    property OnClick;
    // @exclude(inherited)
    property OnDblClick;
    // @exclude(inherited)
    property OnDragDrop;
    // @exclude(inherited)
    property OnDragOver;
    // @exclude(inherited)
    property OnEndDock;
    // @exclude(inherited)
    property OnEndDrag;
    // @exclude(inherited)
    property OnEnter;
    // @exclude(inherited)
    property OnExit;
    // @exclude(inherited)
    property OnKeyDown;

    // @exclude(inherited)
    property OnKeyPress;

    // @exclude(inherited)
    property OnKeyUp;
    // @exclude(inherited)
    property OnMouseDown;
    // @exclude(inherited)
    property OnMouseMove;
    // @exclude(inherited)
    property OnMouseUp;
    // @exclude(inherited)
    property OnMouseWheel;
    // @exclude(inherited)
    property OnMouseWheelDown;
    // @exclude(inherited)
    property OnMouseWheelUp;
    // @exclude(inherited)
    property OnStartDock;
    // @exclude(inherited)
    property OnStartDrag;
    // @exclude(inherited)
    property ParentBiDiMode;
    // @exclude(inherited)
    property ParentCtl3D;
    // @exclude(inherited)
    property ParentFont;
    // @exclude(inherited)
    property ParentShowHint;
    // @exclude(inherited)
    property PopupMenu;
    // @exclude(inherited)
    property ScrollBars;
    // @exclude(inherited)
    property ShowHint;
    // @exclude(inherited)
    property TabOrder;
    // @exclude(inherited)
    property TabStop;
    // @exclude(inherited)
    property Visible;
    // see inherited @inherited
    property BytesPerRow;
    // see inherited @inherited
    property BytesPerColumn;
    // see inherited @inherited
    property AutoBytesPerRow;
    // see inherited @inherited
    property Translation;
    // see inherited @inherited
    property OffsetFormat;
    // see inherited @inherited
    property CaretKind;
    // see inherited @inherited
    property Colors;
    // see inherited @inherited
    property FocusFrame;
    // see inherited @inherited
    property SwapNibbles;
    // see inherited @inherited
    property MaskChar;
    // see inherited @inherited
    property NoSizeChange;
    // see inherited @inherited
    property AllowInsertMode;
    // see inherited @inherited
    property DrawGridLines;
    // see inherited @inherited
    property WantTabs;
    // see inherited @inherited
    property ReadOnlyView;
    // see inherited @inherited
    property HideSelection;
    // see inherited @inherited
    property GraySelectionIfNotFocused;
    // see inherited @inherited
    property GutterWidth;
    // see inherited @inherited
    property BookmarkBitmap;

    // see inherited @inherited
    property MaxUndo;
    // see inherited @inherited
    property InsertMode;
    // see inherited @inherited
    property HexLowerCase;
    // see inherited @inherited
    property OnProgress;
    // see inherited @inherited
    property OnInvalidKey;
    // see inherited @inherited
    property OnTopLeftChanged;
    // see inherited @inherited
    property OnChange;
    // see inherited @inherited
    property DrawGutter3D;
    // see inherited @inherited
    property ShowRuler;
    // see inherited @inherited
    property BytesPerUnit;
    // see inherited @inherited
    property RulerBytesPerUnit;
    // see inherited @inherited
    property ShowPositionIfNotFocused;
    // see inherited @inherited
    property OnSelectionChanged;
    // see inherited @inherited
    property UnicodeChars;
    // see inherited @inherited
    property UnicodeBigEndian;

    // see inherited @inherited
    property OnDrawCell;

    // see inherited @inherited
    property OnBookmarkChanged;
    // see inherited @inherited
    property OnGetOffsetText;
    // see inherited @inherited
    property BytesPerBlock;
    // see inherited @inherited
    property SeparateBlocksInCharField;
    // see inherited @inherited
    property RulerNumberBase;
  end;

  // @exclude(undo storage record)
  PBCHUndoRec = ^TBCHUndoRec;
  // @exclude(undo storage record)
  TBCHUndoRec = packed record
    DataLen: integer;
    Flags: TBCHUndoFlags;
    CurPos: integer;
    Pos, Count, ReplCount: cardinal;
    CurTranslation: TBCHTranslationKind;
    CurBPU: Integer;
    Buffer: byte;
  end;

  // @exclude(implements undo/redo)
  TBCHUndoStorage = class(TMemoryStream)
  private
    FCount,
      FUpdateCount: integer;
    FEditor: TCustomBCHexEditor;
    FDescription: string;
    FRedoPointer,
      FLastUndo: PBCHUndoRec;
    FLastUndoSize: integer;
    FLastUndoDesc: string;
    procedure SetCount(const Value: integer);
    procedure ResetRedo;
    procedure CreateRedo(const Rec: TBCHUndoRec);
    function GetUndoKind(const Flags: TBCHUndoFlags): TBCHUndoFlag;
    procedure AddSelection(const APos, ACount: integer);
    function ReadUndoRecord(var aUR: TBCHUndoRec; var SDescription: string):
      TBCHUndoFlag;
    function GetLastUndoKind: TBCHUndoFlag;

  public
    constructor Create(AEditor: TCustomBCHexEditor);
    destructor Destroy; override;
    procedure SetSize(NewSize: longint); override;
    procedure CreateUndo(aKind: TBCHUndoFlag; APosition, ACount, AReplaceCount:
      integer; const SDescription: string = '');
    function CanUndo: boolean;
    function CanRedo: boolean;
    function Redo: boolean;
    function Undo: boolean;
    function BeginUpdate: integer;
    function EndUpdate: integer;
    procedure Reset(AResetRedo: boolean = True);
    procedure RemoveLastUndo;
    property Count: integer read FCount write SetCount;
    property UpdateCount: integer read FUpdateCount;
    property Description: string read FDescription;
    property UndoKind: TBCHUndoFlag read GetLastUndoKind;
  end;

resourcestring

  // long descriptive names of character translations
  // tkAsIs
  BCH_TK_ASIS = 'Windows';
  // tkDos8
  BCH_TK_DOS8 = 'Dos 8 bits';
  // tkASCII
  BCH_TK_ASCII7 = 'ASCII 7 bits';
  // tkMac
  BCH_TK_MAC = 'Macintosh';
  // tkBCD
  BCH_TK_BCD38 = 'EBCDIC codepage 38';

  // unicode
  BCH_UC = 'Unicode little endian';
  // unicode be
  BCH_UC_BE = 'Unicode big endian';

  // short names (e.g. for status bars) of character translations
  // tkAsIs
  BCH_TK_ASIS_S = 'WIN';
  // tkDos8
  BCH_TK_DOS8_S = 'DOS';
  // tkASCII
  BCH_TK_ASCII7_S = 'ASC';
  // tkMac
  BCH_TK_MAC_S = 'MAC';
  // tkBCD
  BCH_TK_BCD38_S = 'BCD';

  // tkCustom
  BCH_TK_CUSTOM_S = 'Cust';
  // tkCustom
  BCH_TK_CUSTOM = 'Custom translation';

  // unicode
  BCH_UC_S = 'UCLE';
  // unicode be
  BCH_UC_BE_S = 'UCBE';

const
  // long descriptions of the different translations (e.g. for menues)
  BCHTranslationDesc: array[TBCHTranslationKind] of string = (BCH_TK_ASIS,
    BCH_TK_DOS8, BCH_TK_ASCII7, BCH_TK_MAC,
    BCH_TK_BCD38,
    BCH_TK_CUSTOM);

  // short descriptions of the different translations (e.g. for status bars)
  BCHTranslationDescShort: array[TBCHTranslationKind] of string =
  (BCH_TK_ASIS_S, BCH_TK_DOS8_S, BCH_TK_ASCII7_S, BCH_TK_MAC_S,
    BCH_TK_BCD38_S, BCH_TK_CUSTOM_S);

// public utility functions

(* translate a hexadecimal data representation ("a000 cc45 d3 42"...) to binary data
 (see @link(SwapNibbles) for the meaning of the SwapNibbles value)
*)
function ConvertHexToBin(aFrom: PChar; aTo: PAnsiChar; const aCount: integer; const
  SwapNibbles: boolean; var BytesTranslated: integer): PAnsiChar;

(* translate binary data to its hex representation (see @link(ConvertHexToBin)),
   (see @link(SwapNibbles) for the meaning of the SwapNibbles value)
*)
function ConvertBinToHex(aFrom: PAnsiChar; aTo: PChar; const aCount: integer; const
  SwapNibbles: boolean): PChar;

// convert X and Y into a TGridCoord record
function GridCoord(aX, aY: longint): TGridCoord;
// check whether the given key (VK_...) is currently down
function IsKeyDown(aKey: integer): boolean;

// get a unique filename in the temporary directory
function GetTempName: string;

(* translate an integer to a radix (base) coded string, e.g.<br>
  - IntToRadix(100,16) converts into a hexadecimal (number) string<br>
  - IntToRadix(100,2) converts into a string consisting only of 0 and 1<br>
  - IntToRadix(100,8) means IntToOctal<br>
  <br>
  hint: Radix must be in the range of 2..16*)

function IntToRadix(Value: uint64; Radix: byte; Len: byte = 0): string;
// translate an integer to an octal string (see also @link(IntToRadix))
function IntToOctal(const Value: integer): string;

(* translate a radix coded number string into an integer, e.g.<br>
  - RadixToInt('0f', 16) => 15<br>
  - RadixToInt('755', 8) => 493
*)
function RadixToInt(Value: string; Radix: byte): int64;

(* try to find the correct radix (based on prefix/suffix) and return the number, known
   prefixes/suffixes are:<br>
   0x&lt;number&gt;, 0X&lt;number&gt;, $&lt;number&gt;, &lt;number&gt;h, &lt;number&gt;H: radix 16<br>
   o&lt;number&gt;, O&lt;number&gt;, 0&lt;number&gt;, &lt;number&gt;o, &lt;number&gt;O: radix 8<br>
   %&lt;number&gt;, &lt;number&gt;%: radix 2<br>
   otherwise: radix 10
*)
function CheckRadixToInt(Value: string): int64;

// translate an number string built on radix 8 into an integer (see also @link(RadixToInt))
function OctalToInt(const Value: string): integer;

// swap lo and high byte of a widechar
procedure SwapWideChar(var WChar: WideChar);

// @exclude(fade a color to a gray value)
function FadeToGray(aColor: TColor): TColor;

(* translate data from Ansi to a different character set (see also @link(TBCHTranslationKind))<br>
  - TType: translate to this character set<br>
  - aBuffer: pointer to source data<br>
  - bBuffer: pointer to target data, must be allocated (may equal to aBuffer)<br>
  - aCount: number of bytes to translate
*)
procedure TranslateBufferFromAnsi(const TType: TBCHTranslationKind; aBuffer,
  bBuffer: PAnsiChar; const aCount: integer);
// translate data from a different character set to Ansi (see also @link(TranslateBufferFromAnsi))
procedure TranslateBufferToAnsi(const TType: TBCHTranslationKind; aBuffer,
  bBuffer: PAnsiChar; const aCount: integer);

// compatibility

// returns the lower of the two numbers
function Min(a1, a2: integer): integer;
// returns the higer of the two numbers
function Max(a1, a2: integer): integer;

var
  (* translation tables for tkCustom *)

  // this character conversion is used in translations from tkAsIs to tkCustom (see @link(TBCHTranslationKind))
  BCHCustomCharConv: TBCHCharConv;

const
  (* standard offset formats *)

  // standard offset format: hex, auto min width, prefixed by 0x
  BCHOffsetHex = '-!10:0x|';
  // standard offset format: decimal
  BCHOffsetDec = 'a:|';
  // standard offset format: octal, suffixed by a small "o"
  BCHOffsetOct = '0!8:o|';

implementation

uses
  Consts, RTLConsts, ImgList, StdCtrls, SysConst;

resourcestring

  // undo descriptions
  UNDO_BYTESCHANGED = 'Change byte(s)';
  UNDO_REMOVED = 'Remove data';
  UNDO_INSERT = 'Insert buffer';
  UNDO_REPLACE = 'Replace';
  UNDO_APPEND = 'Append buffer';
  UNDO_INSNIBBLE = 'Insert nibble';
  UNDO_DELNIBBLE = 'Delete nibble';
  UNDO_CONVERT = 'Convert';
  UNDO_SELECTION = 'Cursor movement';
  UNDO_COMBINED = 'Multiple modification';
  UNDO_ALLDATA = 'All data saved';
  UNDO_NOUNDO = 'No undo';

  // error messages
  ERR_FILE_OPEN_FAILED = 'Cannot open %s.'#13#10'(%s.)';
  ERR_FILE_READONLY = 'Cannot save readonly file %s.';
  ERR_INVALID_BOOKMARK = 'Invalid bookmark index';
  ERR_INVALID_SELSTART = 'Invalid selection start';
  ERR_INVALID_SELEND = 'Invalid selection end';
  ERR_INVALID_BYTESPERLINE = 'Invalid bytes per line argument';
  ERR_INVALID_BUFFERFROMFILE = 'Invalid buffer from file argument';
  ERR_INVALID_BYTESPERCOL = 'Invalid bytes per column argument';
  ERR_INVALID_BOOKMARKBMP = 'Invalid bookmark bitmap (must be 10 x 200 px)';
  ERR_CANCELLED = 'Operation cancelled';
  ERR_MISSING_FORMATCHAR = 'Missing char in offset format: %s';
  ERR_INVALID_FORMATRADIX =
    'Invalid radix in offset format (%xh), allowed: 02h..10h';
  ERR_INVALID_RADIXCHAR =
    'Invalid character %s, cannot convert using radix %xh';
  ERR_INVALID_BPU = 'Invalid bytes per unit value %d, allowed: 1,2,4,8';
  ERR_INVALID_BPU_U = 'BytesPerUnit must be set to 2 in unicode mode';
  ERR_INVALID_RBPU =
    'Invalid ruler bytes per unit value %d, allowed: -1,1,2,4,8';
  ERR_DATA_BOUNDS = 'Data position/length out of data bounds';
  ERR_NO_TRANSLATION_IN_UNICODE_MODE =
    'Translations cannot be used in unicode mode';
  ERR_ODD_FILESIZE_UNICODE = 'Cannot use unicode mode with odd-sized files';

  ERR_FIXED_FILESIZE = 'Cannot change fixed filesize';
  ERR_NOUNDO = 'Cannot update undo storage';

  // new, empty file
  UNNAMED_FILE = 'Untitled';

const
  // fixed cols/rows
  GRID_FIXED = 2;

  // valid hex characters
  HEX_LOWER = '0123456789abcdef';
  HEX_UPPER = '0123456789ABCDEF';
  HEX_ALLCHARS = '0123456789abcdef0123456789ABCDEF';

  // available undo descriptions
  STRS_UNDODESC: array[ufKindBytesChanged..ufKindAllData] of string =
  (UNDO_BYTESCHANGED, UNDO_REMOVED, UNDO_INSERT, UNDO_REPLACE, UNDO_APPEND,
    UNDO_INSNIBBLE, UNDO_DELNIBBLE, UNDO_CONVERT, UNDO_SELECTION, UNDO_COMBINED,
    UNDO_ALLDATA);

// invert the given color

function Invert(Color: TColor): TColor;
begin
  Result := ColorToRGB(Color) xor $00FFFFFF;
end;

// translate the buffer from ANSI to the given translation mode

procedure TranslateBufferFromAnsi(const TType: TBCHTranslationKind; aBuffer,
  bBuffer: PAnsiChar; const aCount: integer);
var
  LIntLoop: integer;
begin
  case TType of
    // changed 04/18/04: bBuffer and aBuffer were interchanged!
    tkAsIs: Move(aBuffer^, bBuffer^, aCount);
    tkDOS8,
      tkASCII: CharToOEMBuffA(aBuffer, bBuffer, aCount);
    tkMAC: if aCount > 0 then
        for LIntLoop := 0 to Pred(aCount) do
          bBuffer[LIntLoop] :=
            BCH_CCONV_MAC[cctFromAnsi][Ord(aBuffer[LIntLoop])];
    tkBCD: if aCount > 0 then
        for LIntLoop := 0 to Pred(aCount) do
          bBuffer[LIntLoop] :=
            BCH_CCONV_BCD38[cctFromAnsi][Ord(aBuffer[LIntLoop])];

    tkCustom: if aCount > 0 then
        for LIntLoop := 0 to Pred(aCount) do
          bBuffer[LIntLoop] :=
            BCHCustomCharConv[cctFromAnsi][Ord(aBuffer[LIntLoop])];

  end;
end;

// translate the buffer to ANSI from the given translation mode

procedure TranslateBufferToAnsi(const TType: TBCHTranslationKind; aBuffer,
  bBuffer: PAnsiChar; const aCount: integer);
var
  LIntLoop: integer;
begin
  case TType of
    tkAsIs: Move(aBuffer^, bBuffer^, aCount);
    tkDOS8,
      tkASCII: OEMToCharBuffA(aBuffer, bBuffer, aCount);
    tkMAC: if aCount > 0 then
        for LIntLoop := 0 to Pred(aCount) do
          bBuffer[LIntLoop] := BCH_CCONV_MAC[cctToAnsi][Ord(aBuffer[LIntLoop])];
    tkBCD: if aCount > 0 then
        for LIntLoop := 0 to Pred(aCount) do
          bBuffer[LIntLoop] :=
            BCH_CCONV_BCD38[cctToAnsi][Ord(aBuffer[LIntLoop])];

    tkCustom: if aCount > 0 then
        for LIntLoop := 0 to Pred(aCount) do
          bBuffer[LIntLoop] :=
            BCHCustomCharConv[cctToAnsi][Ord(aBuffer[LIntLoop])];

  end;
end;

// ansi to oem

function OEM2Char(aByte: byte): char;
var
  LszBuf: array[0..1] of char;
begin
  LszBuf[0] := char(aByte);
  LszBuf[1] := #0;
  OEMToChar(@LSzBuf[0], LSzBuf);
  Result := LSzBuf[0];
end;

// oem to ansi

function Char2OEM(aByte: byte): char;
var
  LszBuf: array[0..1] of char;
begin
  LszBuf[0] := char(aByte);
  LszBuf[1] := #0;
  CharToOEM(LSzBuf, @LSzBuf[0]);
  Result := LSzBuf[0];
end;

(* helper functions *)

// get a temporary file name

function GetTempName: string;

var
  LStrTemp: string;
begin
  SetLength(LStrTemp, MAX_PATH + 1);
  SetLength(LStrTemp, GetTempPath(MAX_PATH, @LStrTemp[1]));
  LStrTemp := Trim(LStrTemp);
  LstrTemp := IncludeTrailingPathDelimiter(LstrTemp);
  repeat
    Result := LStrTemp + IntToHex(GetTickCount, 8) + '.MPHT';
  until GetFileAttributes(PChar(Result)) = $FFFFFFFF;

end;

// can the file be opened for reading (possibly read only) ?

function CanOpenFile(const aName: TFileName; var ReadOnly: boolean): boolean;
var
  LHdlFile: THandle;
begin
  Result := False;
  ReadOnly := True;
  LHdlFile := FileOpen(aName, fmOpenRead or fmShareDenyNone);
  if LHdlFile <> INVALID_HANDLE_VALUE then
  begin
    FileClose(LHdlFile);
    Result := True;
    try
      LHdlFile := FileOpen(aName, fmOpenReadWrite);
      if LHdlFile <> INVALID_HANDLE_VALUE then
      begin
        FileClose(LHdlFile);
        ReadOnly := False;
      end;
    except
      Result := True;
      ReadOnly := True;
    end;
  end;
end;

// is that key pressed ?

function IsKeyDown(aKey: integer): boolean;
begin
  Result := (GetKeyState(aKey) and (not 1)) <> 0;
end;

// return the lesser value

function Min(a1, a2: integer): integer;
begin
  if a1 < a2 then
    Result := a1
  else
    Result := a2;
end;

// return the bigger value

function Max(a1, a2: integer): integer;
begin
  if a1 > a2 then
    Result := a1
  else
    Result := a2;
end;

// cast x,y to grid coord

function GridCoord(aX, aY: longint): TGridCoord;
begin
  Result.x := aX;
  Result.y := aY;
end;

// convert '00 01 02...' to binary data

function ConvertHexToBin(aFrom: PChar; aTo: PAnsiChar; const aCount: integer;
  const SwapNibbles: boolean; var BytesTranslated: integer): PAnsiChar;
var
  LBoolHi: boolean;
  LIntLoop: integer;
  LBytCurrent: byte;
  LChrCurrent: char;
begin
  Result := aTo;
  BytesTranslated := 0;
  LBoolHi := True;
  LBytCurrent := 0;
  for LIntLoop := 0 to Pred(aCount) do
    if Pos(aFrom[LIntLoop], HEX_ALLCHARS) <> 0 then
    begin
      LChrCurrent := UpCase(aFrom[LIntLoop]);
      if LBoolHi then
        LBytCurrent := ((Pos(LChrCurrent, HEX_UPPER) - 1) * 16)
      else
        LBytCurrent := LBytCurrent or ((Pos(LChrCurrent, HEX_UPPER) - 1));

      LBoolHi := not LBoolHi;
      if LBoolHi then
      begin
        if SwapNibbles then
          aTo[BytesTranslated] := AnsiChar(((LBytCurrent and 15) * 16) or
            ((LBytCurrent and $F0) shr 4))
        else
          aTo[BytesTranslated] := AnsiChar(LBytCurrent);

        Inc(BytesTranslated);
      end;
    end;
end;

// convert binary data to '00 01 02...'

function ConvertBinToHex(aFrom: PAnsiChar; aTo: PChar; const aCount: integer;
  const SwapNibbles: boolean): PChar;
var
  LIntLoop: integer;
  LByteCurrent: byte;
  LIntLoop2: integer;
begin
  Result := aTo;
  LIntLoop2 := 0;
  for LIntLoop := 0 to Pred(aCount) do
  begin
    LByteCurrent := Ord(aFrom[LIntLoop]);
    if SwapNibbles then
    begin
      aTo[LIntLoop2] := UpCase(HEX_UPPER[(LByteCurrent and 15) + 1]);
      aTo[LIntLoop2 + 1] := UpCase(HEX_UPPER[(LByteCurrent shr 4) + 1])
    end
    else
    begin
      aTo[LIntLoop2 + 1] := UpCase(HEX_UPPER[(LByteCurrent and 15) + 1]);
      aTo[LIntLoop2] := UpCase(HEX_UPPER[(LByteCurrent shr 4) + 1])
    end;

    Inc(LIntLoop2, 2);
  end;
  aTO[LIntLoop2] := #0;
end;

// translate an integer to a radix coded string and left fill with 0

function IntToRadix(Value: uint64; Radix: byte; Len: byte = 0): string;
begin
  Result := '';
  repeat
    Result := HEX_UPPER[(Value mod Radix) + 1] + Result;
    Value := Value div Radix;
  until Value = 0;
  while Length(Result) < Len do
    Result := '0' + Result;
end;

// translate an integer value to an octal string

function IntToOctal(const Value: integer): string;
begin
  Result := IntToRadix(Value, 8);
end;

// translate a radix coded string into an integer

function RadixToInt(Value: string; Radix: byte): int64;
begin
  Result := 0;
  Value := UpperCase(Value);
  while Value <> '' do
  begin
    if not (Pos(Value[1], HEX_UPPER) in [1..Radix]) then
      raise EBCHexEditor.CreateFmt(ERR_INVALID_RADIXCHAR, [Value[1], Radix]);
    Result := Result * Radix + cardinal(Pos(Value[1], HEX_UPPER) - 1);
    Delete(Value, 1, 1);
  end;
end;

(* try to find the correct radix (based on prefix/suffix) and return the number, known
   prefixes/suffixes are:<br>
   0x<number>, 0X<number>, $<number>, <number>h, <number>H: radix 16<br>
   o<number>, O<number>, <number>o, <number>O: radix 8<br>
   %<number>, <number>%: radix 2<br>
   otherwise: radix 10
*)

function CheckRadixToInt(Value: string): int64;
begin
  // hex
  if UpperCase(Copy(Value, 1, 2)) = '0X' then
    Result := RadixToInt(Copy(Value, 3, MaxInt), 16)
  else if Copy(Value, 1, 1) = '$' then
    Result := RadixToInt(Copy(Value, 2, MaxInt), 16)
  else if UpperCase(Copy(Value, Length(Value), 1)) = 'H' then
    Result := RadixToInt(Copy(Value, 1, Length(Value) - 1), 16)
  else {// octal} if UpperCase(Copy(Value, Length(Value), 1)) = 'O' then
    Result := RadixToInt(Copy(Value, 1, Length(Value) - 1), 8)
  else if UpperCase(Copy(Value, 1, 1)) = 'O' then
    Result := RadixToInt(Copy(Value, 2, MaxInt), 8)
      (* removed, is ambigous else if (Copy(Value, 1, 1) = '0') and (AllCharsIn(['0'..'7'])) then
  Result := RadixToInt(Value, 8)*)
  else {// binary} if UpperCase(Copy(Value, Length(Value), 1)) = '%' then
    Result := RadixToInt(Copy(Value, 1, Length(Value) - 1), 2)
  else if UpperCase(Copy(Value, 1, 1)) = '%' then
    Result := RadixToInt(Copy(Value, 2, MaxInt), 2)
  else
    // decimal
    Result := StrToInt64(Value)
end;

// translate an octal to an integer

function OctalToInt(const Value: string): integer;
begin
  Result := RadixToInt(Value, 8);
end;

// swap lo and high byte of a widechar

procedure SwapWideChar(var WChar: WideChar);
var
  LWrdChar: word absolute WChar;
begin
  LWrdChar := Swap(LWrdChar);
end;

// fade a color to a gray value

function FadeToGray(aColor: TColor): TColor;
var
  LBytGray: byte;
begin
  aColor := ColorToRGB(aColor);
  LBytGray := HiByte(GetRValue(aColor) * 74 + GetGValue(aColor) * 146 +
    GetBValue(aColor) * 36);
  Result := RGB(LBytGray, LBytGray, LBytGray);
end;

(* TCustomMPHexEditor *)

constructor TCustomBCHexEditor.Create(aOwner: TComponent);
var
  LIntLoop: integer;
begin
  inherited Create(aOwner);
  FIsViewSyncing := False;
  FSetAutoBytesPerRow := False;
  FSetDataSizeFillByte := 0;
  FMaskedChars := '';
  for LIntLoop := 0 to 31 do
    FMaskedChars := FMaskedChars + Chr(LIntLoop);
  FAutoBytesPerRow := False;
  FRulerNumberBase := 16;
  FOffsetHandler := False;
  FBlockSize := -1;
  FSepCharBlocks := True;
  FUnicodeCharacters := False;
  FUnicodeBigEndian := False;
  FSelectionChangedCount := 0;
  FBytesPerUnit := 1;
  FRulerBytesPerUnit := -1;
  FUsedRulerBytesPerUnit := 1;
  FShowPositionIfNotFocused := False;
  FShowRuler := False;
  FDrawGutter3D := True;
  FHexLowerCase := True;
  SetHexLowerCase(False);
  DoubleBuffered := True;
  FBookmarkBitmap := TBitmap.Create;
  FCursorList := nil;
  FHasCustomBMP := False;
  FStreamFileName := '';
  FHasFile := False;
  FMaxUndo := 1024 * 1024;
  FPosInCharField := False;
  FLastPosInCharField := True;

  FGutterWidth := -1;
  GenerateOffsetFormat(BCHOffsetHex);
  FSelectionPossible := True;
  FBookmarkImageList := TImageList.Create(self);
  FBookmarkImageList.DrawingStyle := dsTransparent;
  FBookmarkImageList.BkColor := clBlack;
  FBookmarkImageList.Width := 10;
  FBookmarkImageList.Height := 10;

  Options := [goThumbTracking];
  DesignOptionsBoost := [];
  DefaultDrawing := False;
  FSaveCellExtents := False;

  FColors := TBCHColors.Create(Self);
  FDrawGridLines := DefaultDrawGridLines;

  ParentColor := False;
  FDataStorage := TBCHMemoryStream.Create;
  FUndoStorage := TBCHUndoStorage.Create(self);

  Color := FColors.Background;

  FCharWidth := -1;
  FOffSetDisplayWidth := -1;
  FBytesPerRow := DefaultBytesPerRow;
  FCaretKind := ckAuto;
  FFocusFrame := DefaultFocusFrame;
  FSwapNibbles := 0;
  FFileName := '---';

  Font.Name := DefaultFontName;
  Font.Size := DefaultFontSize;
  BorderStyle := bsSingle;
  FBytesPerCol := DefaultBytesPerCol;
  CTL3D := False;
  Cursor := crIBeam;
  FModifiedBytes := TBits.Create;
  for LIntLoop := Low(FBookmarks) to High(FBookmarks) do
    FBookmarks[LIntLoop].mPosition := -1;
  SetSelection(-1, -1, -1);
  FIsSelecting := False;
  ResetUndo;
  DefaultColWidth := 0;
  DefaultRowHeight := 0;
  RowHeights[0] := 0;
  RowHeights[1] := 0;
  ColCount := CalcColCount;
  RowCount := GRID_FIXED + 1;
  FTranslation := DefaultTranslation;
  FModified := False;
  FIsFileReadonly := True;
  FBytesPerRowDup := 2 * FBytesPerRow;
  FIntLastHexCol := (GRID_FIXED + FBytesPerRowDup - 1);
  FReplaceUnprintableCharsBy := '.';
  FCaretBitmap := TBitmap.Create;
  FFixedFileSize := False;
  FFixedFileSizeOverride := False;
  FAllowInsertMode := DefaultAllowInsertMode;
  FInsertModeOn := False;
  FWantTabs := True;
  FReadOnlyView := False;
  FHideSelection := False;
  FGraySelOnLostFocus := False;
  FOnProgress := nil;
  FShowDrag := False;
  FSelBeginPosition := -1;
  FBookmarkBitmap.OnChange := BookmarkBitmapChanged;
  FBookmarkBitmap.LoadFromResourceName(HINSTANCE, 'BOOKMARKICONS');
  SetRulerString;
  ControlStyle := ControlStyle + [csNeedsBorderPaint];
end;

destructor TCustomBCHexEditor.Destroy;
begin

  FCursorList := nil;
  FBookmarkBitmap.OnChange := nil;
  FreeStorage;
  FreeStorage(True);
  FUndoStorage.Free;
  FDataStorage.Free;
  FModifiedBytes.Free;
  FColors.Free;
  FCaretBitmap.Free;
  FBookmarkImageList.Free;
  FBookmarkBitmap.Free;
  inherited Destroy;
end;

procedure TCustomBCHexEditor.AdjustMetrics;
var
  LIntLoop: integer;
  LIntChWidth: integer;
begin
  Canvas.Font.Assign(Font);
  FCharWidth := Canvas.TextWidth('w');

  SetOffsetDisplayWidth;
  DoSetCellWidth(1, 6);

  for LIntLoop := 0 to FBytesPerRowDup do
  begin
    if LIntLoop = Pred(FBytesPerRowDup) then
      LIntChWidth := FCharWidth * 2
    else
    begin
      LIntChWidth := FCharWidth;
      if (((LIntLoop + GRID_FIXED) mod FBytesPerCol) = 1) then
        Inc(LIntChWidth, FCharWidth);
      if (FBlockSize > 1) and (((LIntLoop + GRID_FIXED) mod (FBlockSize * 2)) =
        1) then
        Inc(LIntChWidth, FCharWidth);
    end;
    DoSetCellWidth(LIntLoop + GRID_FIXED, LIntChWidth);
  end;

  if FUnicodeCharacters then
    LIntLoop := Pred(FBytesPerRow div 2)
  else
    LIntLoop := Pred(FBytesPerRow);
  for LIntLoop := 0 to LIntLoop do
    //FBytesPerRowDup + 1 to (FBytesPerRow * 3) - 1 do
  begin
    if (FUsedRulerBytesPerUnit > 1) and ((LIntLoop mod FUsedRulerBytesPerUnit)
      = Pred(FUsedRulerBytesPerUnit)) and (not FUnicodeCharacters) then
      LIntChWidth := (FCharWidth * 3 div 2) + 1
    else
      LIntChWidth := FCharWidth + 1;
    if not FUnicodeCharacters then
    begin
      if (FBlockSize > 1) and FSepCharBlocks and ((LIntLoop mod FBlockSize) =
        Pred(FBlockSize)) then
        Inc(LIntChWidth, FCharWidth);
    end
    else
    begin
      if (FBlockSize > 1) and FSepCharBlocks and ((LIntLoop mod (FBlockSize div
        2)) = Pred(FBlockSize div 2)) then
        Inc(LIntChWidth, FCharWidth);
    end;
    DoSetCellWidth(LIntLoop + GRID_FIXED + FBytesPerRowDup + 1, LIntChWidth);
  end;

  DoSetCellWidth(GetLastCharCol, (FCharWidth * 2) + 1);

  FCharHeight := Canvas.TextHeight('yY') + 2;
  DefaultRowHeight := FCharHeight;
  RowHeights[1] := 0;
  if FShowRuler then
    RowHeights[0] := DefaultRowHeight + 3
  else
    RowHeights[0] := 0;
  CheckSetCaret;
end;

function TCustomBCHexEditor.GetDataSize: integer;
begin
  Result := FDataStorage.Size;
end;

procedure TCustomBCHexEditor.CreateEmptyFile;
begin
  FreeStorage;
  if TempName = '' then
    FFileName := UNNAMED_FILE
  else
    FFileName := TempName;
  ResetUndo;
  ResetSelection(False);
  FModifiedBytes.Size := 0;
  CalcSizes;
  FModified := False;
  FIsFileReadonly := True;
  FHasFile := False;
  MoveColRow(GRID_FIXED, GRID_FIXED, True, True);
  Changed;
end;

procedure TCustomBCHexEditor.SaveToStream(Strm: TStream);
begin
  WaitCursor;
  try
    FDataStorage.Position := 0;

    Stream2Stream(FDataStorage, Strm, pkSave);
  finally
    Invalidate;
    OldCursor;
  end;
end;

procedure TCustomBCHexEditor.SaveRangeToStream(Strm: TStream; const APosition,
  ACount: integer);
begin
  WaitCursor;
  try
    FDataStorage.Position := APosition;
    Stream2Stream(FDataStorage, Strm, pkSave, ACount);
  finally
    Invalidate;
    OldCursor;
  end;
end;

procedure TCustomBCHexEditor.SaveToFile(const Filename: string;
  const aUnModify: boolean = True);
var
  LfstFile: TFileStream;
begin
  if (FFileName = FileName) then
    PrepareOverwriteDiskFile;

  LfstFile := TFileStream.Create(FileName, fmCreate);
  try
    FStreamFileName := FileName;
    SaveToStream(LfstFile);
    FHasFile := True;

    if aUnModify then

    begin
      FModifiedBytes.Size := 0;
      FModified := False;
      FIsFileReadonly := False;
      FFileName := Filename;
      FDataStorage.Position := 0;

      ResetUndo

    end;
  finally
    FStreamFileName := '';
    LfstFile.Free;
  end;
end;

procedure TCustomBCHexEditor.LoadFromStream(Strm: TStream);
begin
  try
    FreeStorage;
    CalcSizes;
    WaitCursor;
    try
      try
        Strm.Position := 0;
        FDataStorage.Size := Strm.Size;
        FDataStorage.Position := 0;

        Stream2Stream(Strm, FDataStorage, pkLoad);
        //FDataStorage.CopyFrom(Strm, Strm.Size - Strm.Position);

        FDataStorage.Position := 0;
      finally
        with FUndoStorage do
          if UpdateCount < 1 then
            Reset;
        FModifiedBytes.Size := 0;
        CalcSizes;
        FModified := False;
        FIsSelecting := False;
        MoveColRow(GRID_FIXED, GRID_FIXED, True, True);
        Changed;
      end;
    finally
      OldCursor;
    end;
  except
    FreeStorage;
    FreeStorage(True);
    FHasFile := False;
    raise;
  end;
end;

procedure TCustomBCHexEditor.LoadFromFile(const Filename: string);
var
  LfstFile: TFileStream;
begin
  if CanOpenFile(FileName, FIsFileReadonly) then
  begin
    LfstFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      FStreamFileName := FileName;
      try
        LoadFromStream(LfstFile);
      except
        FHasFile := False;
        raise;
      end;
      FFileName := FileName;
      FHasFile := True;
    finally
      FStreamFileName := '';
      LfstFile.Free;
    end;
  end
  else
    raise EFOpenError.CreateFmt(ERR_FILE_OPEN_FAILED, [FileName,
      SysErrorMessage(GetLastError)]);
end;

procedure TCustomBCHexEditor.CalcSizes;
var
  LIntRows: integer;
begin
  if FModifiedBytes.Size > DataSize then
    FModifiedBytes.Size := DataSize;

  if DataSize < 1 then
  begin
    RowCount := GRID_FIXED + 1;
    ColCount := CalcColCount;
    FixedCols := GRID_FIXED;
  end
  else
  begin
    LIntRows := (DataSize + (FBytesPerRow - 1)) div FBytesPerRow;
    if ((DataSize mod FBytesPerRow) = 0) and InsertMode then
      INC(LIntRows);
    RowCount := LIntRows + GRID_FIXED;

    ColCount := CalcColCount;
    FixedCols := GRID_FIXED;
  end;
  FixedRows := GRID_FIXED;
  AdjustMetrics;
end;

function TCustomBCHexEditor.TranslateFromAnsiChar(const aByte: byte): WideChar;
begin
  case FTranslation of
    tkAsIs: Result := WideChar(aByte);
    tkDos8,
      tkASCII:
      begin
        if ((FTranslation = tkDos8) or (aByte < 128)) and (aByte > 31) then
          Result := Char2Oem(aByte)
        else
          Result := #0;
      end;
    tkMac: Result := WideChar(BCH_CCONV_MAC[cctFromAnsi][aByte]);
    tkBCD: Result := WideChar(BCH_CCONV_BCD38[cctFromAnsi][aByte]);

    tkCustom: Result := WideChar(BCHCustomCharConv[cctFromAnsi][aByte]);

  else
    Result := #0;
  end;
  if Pos(Result, FMaskedChars) > 0 then
    Result := #0;
end;

function TCustomBCHexEditor.TranslateToAnsiChar(const aByte: byte): WideChar;
begin
  case FTranslation of
    tkAsIs: Result := WideChar(aByte);
    tkDos8,
      tkASCII:
      begin
        Result := Oem2Char(aByte);
        if ((FTranslation = tkASCII) and (aByte > 127)) then
          Result := FReplaceUnprintableCharsBy;
      end;
    tkMac: Result := WideChar(BCH_CCONV_MAC[cctToAnsi][aByte]);
    tkBCD: Result := WideChar(BCH_CCONV_BCD38[cctToAnsi][aByte]);

    tkCustom: Result := WideChar(BCHCustomCharConv[cctToAnsi][aByte]);

  else
    Result := FReplaceUnprintableCharsBy;
  end;

  if (FReplaceUnprintableCharsBy <> #0) and (Pos(Result, FMaskedChars) > 0) then
    Result := FReplaceUnprintableCharsBy;
end;

// get the position of the drag'n'drop marker

function TCustomBCHexEditor.DropPosition: integer;
var
  LBoolInCharField: boolean;
begin
  Result := -1;
  LBoolInCharField := FPosInCharField;
  try
    if FShowDrag then
    begin
      Result := GetPosAtCursor(FDropCol, FDropRow);
      CheckUnit(Result);
    end;
  finally
    FPosInCharField := LBoolInCharField;
  end;
end;

procedure TCustomBCHexEditor.Stream2Stream(strFrom, strTo: TStream;
  const Operation: TBCHProgressKind; const Count: integer = -1);
var
  LBytProgress, LBytLastProgress: byte;
  LIntRemain, LIntRead, LIntCount: integer;
  LBoolCancel: boolean;
  LStrFile: string;

  LBytBuffer: array[0..BCH_FILEIO_BLOCKSIZE - 1] of byte;
begin
  LIntCount := Count;
  if LIntCount = -1 then
    LIntCount := strFrom.Size - strFrom.Position;

  LIntRemain := LIntCount;
  LBoolCancel := False;
  LBytLastProgress := 255;
  LStrFile := FStreamFileName;
  if LStrFile = '' then
    LStrFile := FFileName;

  while LIntRemain > 0 do
  begin
    LBytProgress := Round(((LIntCount - LIntRemain) / LIntCount) * 100);
    if (LBytProgress <> LBytLastProgress) or (LIntRemain <=
      BCH_FILEIO_BLOCKSIZE) then
    begin
      if LIntRemain <= BCH_FILEIO_BLOCKSIZE then
        LBytLastProgress := 100
      else
        LBytLastProgress := LBytProgress;
      if Assigned(FOnProgress) then
      begin
        FOnProgress(self, Operation, LStrFile, LBytLastProgress,
          LBoolCancel);
        if LBoolCancel then
          raise EBCHexEditor.Create(ERR_CANCELLED);
      end
    end;

    LIntRead := Min(LIntRemain, BCH_FILEIO_BLOCKSIZE);
    strFrom.ReadBuffer(LBytBuffer, LIntRead);
    strTo.WriteBuffer(LBytBuffer, LIntRead);
    Dec(LIntRemain, LIntRead);
  end;
end;

function TCustomBCHexEditor.SelectCell(ACol, ARow: longint): boolean;
var
  LRctCellRect: TRect;
  LIntNewPosition, LIntPrevPosition: integer;
begin
  if DataSize > 0 then
    Result := CheckSelectCell(aCol, aRow)
  else
  begin
    if not ((aCol = GRID_FIXED) or (aCol = Max(GetOtherFieldColCheck(GRID_FIXED)
      , GRID_FIXED)) and (aRow = GRID_FIXED)) then
      Result := False
    else
    begin
      LRctCellRect := CellRect(aCol, aRow);
      if LRctCellRect.Left + LRctCellRect.Bottom = 0 then
        IntSetCaretPos(-50, -50, -1)
      else
        IntSetCaretPos(LRctCellRect.Left, LRctCellRect.Top, aCol);
      Result := True;
      Exit;
    end;
  end;

  if Result then
  begin
    // neu zeichnen
    if (aCol <> Col) or (aRow <> Row) then
      Invalidate;

    if FIsSelecting then
    begin
      LIntNewPosition := GetPosAtCursor(aCol, aRow);
      LIntPrevPosition := GetPosAtCursor(Col, Row);
      if FSelBeginPosition = -1 then
        FSelBeginPosition := LIntPrevPosition;
      if not InsertMode then
      begin
        CheckSelectUnit(FSelBeginPosition, LIntNewPosition);
        NewSelection(FSelBeginPosition, LIntNewPosition);
      end
      else
      begin
        if FSelBeginPosition > LIntNewPosition then
        begin
          CheckUnit(FSelBeginPosition);
          CheckUnit(LIntNewPosition);
          if FSelBeginPosition = LIntNewPosition then
          begin
            ResetSelection(True);
            FSelBeginPosition := LIntNewPosition;
            FIsSelecting := True;
          end
          else
          begin
            NewSelection(FSelBeginPosition - FBytesPerUnit, LIntNewPosition);
          end;
        end
        else if FSelBeginPosition < LIntNewPosition then
        begin
          CheckUnit(FSelBeginPosition);
          CheckUnit(LIntNewPosition);
          if FSelBeginPosition = LIntNewPosition then
          begin
            ResetSelection(True);
            FSelBeginPosition := LIntNewPosition;
            FIsSelecting := True;
          end
          else
          begin
            NewSelection(FSelBeginPosition, LIntNewPosition - FBytesPerUnit);
          end;
        end
        else
        begin
          ResetSelection(True);
          FSelBeginPosition := LIntNewPosition;
          FIsSelecting := True;
        end
      end;
    end
    else
      ResetSelection(True);

    // caret neu setzen
    //CheckSetCaret;
    LRctCellRect := CellRect(aCol, aRow);
    if LRctCellRect.Left + LRctCellRect.Bottom = 0 then
      IntSetCaretPos(-50, -50, -1)
    else
      IntSetCaretPos(LRctCellRect.Left, LRctCellRect.Top, aCol);
    SelectionChanged;
  end;
end;

// Obtient la position dans le fichier à partir de la position du curseur

function TCustomBCHexEditor.GetPosAtCursor(const aCol, aRow: integer): integer;
begin
  FPosInCharField := (aCol > (GRID_FIXED + FBytesPerRowDup));
  if FPosInCharField then
  begin
    Result := aCol - ((GRID_FIXED + 1) + FBytesPerRowDup);
    if FUnicodeCharacters then
      Result := Result * 2;
  end
  else
    Result := (aCol - GRID_FIXED) div 2;

  Result := Result + ((aRow - GRID_FIXED) * FBytesPerRow);
  if Result < 0 then
    Result := 0;
end;

function TCustomBCHexEditor.GetRow(const DataPos: integer): integer;
begin
  Result := (DataPos div FBytesPerRow) + GRID_FIXED;
end;

function TCustomBCHexEditor.GetCursorAtPos(const aPos: integer;
  const aChars: boolean): TGridCoord;
var
  LIntCol: integer;
begin
  if aPos < 0 then
  begin
    Result.y := GRID_FIXED;
    Result.x := GRID_FIXED;
    Exit;
  end;

  Result.y := GetRow(aPos);
  LIntCol := aPos mod FBytesPerRow;

  if aChars then
  begin
    if FUnicodeCharacters then
      Result.x := (LIntCol div 2) + (GRID_FIXED + 1) + FBytesPerRowDup
    else
      Result.x := LIntCol + (GRID_FIXED + 1 + FBytesPerRowDup)
  end
  else
    Result.x := (LIntCol * 2) + GRID_FIXED;
end;

function TCustomBCHexEditor.GetOtherFieldCol(const aCol: integer): integer;
var
  LIntCol: integer;
begin
  FPosInCharField := (aCol > (GRID_FIXED + FBytesPerRowDup));
  if FPosInCharField then
  begin
    LIntCol := (aCol - (GRID_FIXED + 1 + FBytesPerRowDup));
    if FUnicodeCharacters then
      Result := (LIntCol * 4) + GRID_FIXED
    else
      Result := (LIntCol * 2) + GRID_FIXED;
  end
  else
  begin
    if FUnicodeCharacters then
      LIntCol := ((aCol - GRID_FIXED) div 4)
    else
      LIntCol := ((aCol - GRID_FIXED) div 2);
    Result := LIntCol + (GRID_FIXED + 1 + FBytesPerRowDup);
  end;
end;

function TCustomBCHexEditor.GetOtherFieldColCheck(const aCol: integer): integer;
var
  LIntCol: integer;
begin
  if aCol > (GRID_FIXED + FBytesPerRowDup) then
  begin
    LIntCol := (aCol - (GRID_FIXED + 1 + FBytesPerRowDup));
    if FUnicodeCharacters then
      Result := (LIntCol * 4) + GRID_FIXED
    else
      Result := (LIntCol * 2) + GRID_FIXED;
  end
  else
  begin
    if FUnicodeCharacters then
      LIntCol := ((aCol - GRID_FIXED) div 4)
    else
      LIntCol := ((aCol - GRID_FIXED) div 2);
    Result := LIntCol + (GRID_FIXED + 1 + FBytesPerRowDup);
  end;
end;

function TCustomBCHexEditor.CheckSelectCell(aCol, aRow: integer): boolean;
var
  LgrcEndCoords: TGridCoord;
  LIntPos: integer;
begin
  Result := inherited SelectCell(aCol, aRow);

  if not FSelectionPossible then
    Exit;

  try
    FSelectionPossible := False;

    if Result then
    begin
      // überprüfen, ob linke maustaste oder shift gedrückt, sonst selection zurücksetzen
      if not (IsKeyDown(VK_SHIFT) or IsKeyDown(VK_LBUTTON)) then
        ResetSelection(True);

      // überprüfen, ob außerhalb der DateiGröße
      LIntPos := GetPosAtCursor(aCol, aRow);
      if (LIntPos >= DataSize) and not (InsertMode and (LIntPos = DataSize) and
        (FPosInCharField or ((aCol mod 2) = 0))) then
      begin
        if (not InsertMode) then
          LgrcEndCoords := GetCursorAtPos(DataSize - 1, InCharField)
        else
          LgrcEndCoords := GetCursorAtPos(DataSize, InCharField);

        MoveColRow(LgrcEndCoords.x, LgrcEndCoords.y, True, True);
        Result := False;
      end
      else if aCol = (GRID_FIXED + FBytesPerRowDup) then
      begin
        Result := False;
        if IsKeyDown(VK_LBUTTON) then
        begin
          aCol := aCol - 1;
          aCol := Max(GRID_FIXED, aCol);
          MoveColRow(aCol, aRow, True, True);
          Exit;
        end;
      end;
    end;

  finally
    FSelectionPossible := True;
  end;
end;

procedure TCustomBCHexEditor.WMImeChar(var Msg: TWMChar);
begin
  WMChar(Msg);
end;

procedure TCustomBCHexEditor.WMChar(var Msg: TWMChar);
var
  LIntPos: integer;
  LChrCharW: Widechar;
  LBytOldData, LBytNewData: byte;
  LArrNewData: packed array[0..7] of byte;
  LWChrNewData: WideChar absolute LArrNewData;
  LgrcPosition: TGridCoord;
  LWChrOldData: WideChar;
  LWrdKey: Word;

  LChr: Char;

begin

  LChrCharW := WideChar(Msg.CharCode);

  if Assigned(OnKeyPress) then
  begin
    LChr := Char(LChrCharW);
    OnKeyPress(Self, LChr);
    LChrCharW := WideChar(LChr);
  end;

  if FReadOnlyView or (Pos(LChrCharW, FMaskedChars) > 0) then
    Exit;

  LIntPos := GetPosAtCursor(Col, Row);
  if (LIntPos >= DataSize) and not InsertMode then
    Exit;

  if not FPosInCharField then
  begin
    // hex-eingabe, nur 0..9 , a..f erlaubt
    if Pos(LChrCharW, string(HEX_ALLCHARS)) <> 0 then
    begin
      LChrCharW := WideChar(UpCase(Char(LChrCharW)));

      if not InsertMode then
        ResetSelection(True);

      LgrcPosition := GetCursorAtPos(LIntPos, FPosInCharField);
      // Obtient la valeur du byte dans le fichier (OldByte)
      if DataSize > LIntPos then
        LBytOldData := Data[LIntPos]
      else
        LBytOldData := 0;

      if (LgrcPosition.x = (Col - FSwapNibbles)) or (SelCount <> 0) then
        LBytNewData := LBytOldData and 15 + ((Pos(LChrCharW, string(HEX_UPPER)) - 1) * 16)
      else
        LBytNewData := (LBytOldData and $F0) + (Pos(LChrCharW, string(HEX_UPPER)) - 1);

      FillChar(LArrNewData, sizeof(LArrNewData), #0);
      if InsertMode and ((((Col - GRID_FIXED) mod (FBytesPerUnit * 2)) = 0) or
        (SelCount > 0)) then
      begin
        if FSwapNibbles = 0 then
          LBytNewData := LBytNewData and $F0
        else
          LBytNewData := LBytNewData and $0F;
        LArrNewData[0] := LBytNewData;

        if DataSize = 0 then
          AppendBuffer(PAnsiChar(@LArrNewData), FBytesPerUnit, '', False)
        else if SelCount = 0 then
        begin
          InsertBuffer(PAnsiChar(@LArrNewData), FBytesPerUnit, LIntPos, '', False);
        end
        else
          ReplaceSelection(PAnsiChar(@LArrNewData), FBytesPerUnit, '', False);
      end
      else
      begin
        if LIntPos >= DataSize then
          Exit;
        IntChangeByte(LBytOldData, LBytNewData, LIntPos, Col, Row);
      end;
      FIsSelecting := False;

      LWrdKey := VK_RIGHT;
      KeyDown(LWrdKey, []);
    end
    else
      WrongKey
  end
  else
  begin
    // zeichen-eingabe, alle zeichen erlaubt
    if LChrCharW < #256 then
      LChrCharW := TranslateFromAnsiChar(Byte(LChrCharW));

    if (Word(LChrCharW) < 256) and (Pos(LChrCharW, FMaskedChars) > 0) then
    begin
      WrongKey;
      Exit;
    end;

    if not InsertMode then
      ResetSelection(True);

    LgrcPosition := GetCursorAtPos(LIntPos, FPosInCharField);

    FillChar(LArrNewData, sizeof(LArrNewData), #0);
    if not FUnicodeCharacters then
      LArrNewData[0] := Ord(LChrCharW)
    else
    begin
      LWChrNewData := LChrCharW;
      if FUnicodeBigEndian then
        SwapWideChar(LWChrNewData);
    end;
    if (DataSize = 0) or (DataSize = LIntPos) then
      LBytOldData := 0
    else
      LBytOldData := Data[LIntPos];
    if FUnicodeCharacters then
    begin
      if (DataSize = 0) or (DataSize = LIntPos) or (DataSize = (LIntPos + 1))
        then
        LWChrOldData := #0
      else
        ReadBuffer(LWChrOldData, LIntPos, 2);
    end;

    if InsertMode then
    begin
      if SelCount > 0 then
        ReplaceSelection(PAnsiChar(@LArrNewData), FBytesPerUnit, '', False)
      else
      begin
        if LIntPos = DataSize then
          AppendBuffer(PAnsiChar(@LArrNewData), FBytesPerUnit)
        else
        begin
          if (LIntPos mod FBytesPerUnit) = 0 then
            InsertBuffer(PAnsiChar(@LArrNewData), FBytesPerUnit, LIntPos, '', False)
          else
            IntChangeByte(LBytOldData, LArrNewData[0], LIntPos, Col, Row)
        end;
        FIsSelecting := False;
      end;
    end
    else
    begin
      if FUnicodeCharacters then
        IntChangeWideChar(LWChrOldData, LWChrNewData, LIntPos, Col, Row)
      else
        IntChangeByte(LBytOldData, Ord(LChrCharW), LIntPos, Col, Row);
    end;

    LWrdKey := VK_RIGHT;
    KeyDown(LWrdKey, []);
  end;
end;

{-------------------------------------------------------------------------------}
// *** procedure TCustomMPHexEditor.IntChangeByte***
// Change la valeur du byte
// Renseigne la structure Undo
{-------------------------------------------------------------------------------}

procedure TCustomBCHexEditor.IntChangeByte(const aOldByte, aNewByte: byte; aPos,
  aCol, aRow: integer; const UndoDesc: string = '');
begin
  if aOldByte = aNewByte then
    Exit;

  CreateUndo(ufKindBytesChanged, aPos, 1, 0, UndoDesc);

  // Ecrit dans le fichier
  Data[aPos] := aNewByte;

  if not InsertMode then
    FModifiedBytes.Bits[aPos] := True;

  Invalidate;
  Changed;
end;

procedure TCustomBCHexEditor.IntChangeWideChar(const aOldChar, aNewChar:
  WideChar; aPos, aCol, aRow: integer; const UndoDesc: string);
var
  LBArrOld: packed array[0..1] of Byte absolute aOldChar;
  LBArrNew: packed array[0..1] of Byte absolute aNewChar;
begin
  if aOldChar = aNewChar then
    Exit;

  CreateUndo(ufKindBytesChanged, aPos, 2, 0, UndoDesc);

  // Ecrit dans le fichier
  WriteBuffer(aNewChar, aPos, 2);

  if not InsertMode then
  begin
    FModifiedBytes.Bits[aPos] := LBArrOld[0] <> LBArrNew[0];
    FModifiedBytes.Bits[aPos + 1] := LBArrOld[1] <> LBArrNew[1];
  end;

  Invalidate;
  Changed;
end;

procedure TCustomBCHexEditor.KeyDown(var Key: word; Shift: TShiftState);
var
  LIntCol: integer;
  LgrcPosition: TGridCoord;
  LIntRow: integer;
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(self, Key, Shift);

  // reset selection if no shift key is pressed (except of SHIFT-Key)
  if not ((Shift <> []) or (KEY = VK_SHIFT)) then
    if not InsertMode then
      ResetSelection(True);

  case Key of

    VK_PRIOR:
      begin
        if ssCtrl in Shift then
        begin
          // go to the first visible line
          LIntRow := TopRow;
          LIntCol := Col;
          if LIntRow > -1 then
          begin
            MoveColRow(LIntCol, LIntRow, True, True);
          end;
        end
        else
        begin
          // scroll up one page
          LIntRow := Max(GRID_FIXED, Row - VisibleRowCount + 1);
          TopRow := Max(GRID_FIXED, TopRow - VisibleRowCount + 1);
          LIntCol := Col;
          if LIntRow > -1 then
          begin
            MoveColRow(LIntCol, LIntRow, True, True);
          end;
        end;
      end;

    VK_NEXT:
      begin
        if ssCtrl in Shift then
        begin
          // go to the Last visible line
          LIntRow := Min(RowCount - 1, TopRow + VisibleRowCount - 1);
          LIntCol := Col;
          if LIntRow > 0 then
          begin
            MoveColRow(LIntCol, LIntRow, True, True);
          end;
        end
        else
        begin
          // scroll down one page
          LIntRow := Min(RowCount - 1, Row + VisibleRowCount - 1);
          TopRow := Min(Max(GRID_FIXED, RowCount - VisibleRowCount),
            TopRow + VisibleRowCount - 1);
          LIntCol := Col;
          if LIntRow > 0 then
          begin
            MoveColRow(LIntCol, LIntRow, True, True);
          end;
        end;
      end;

    VK_HOME:
      begin
        InCharField;
        if (ssCtrl in Shift) then
        begin // strg+pos1
          if not FPosInCharField then
            MoveColRow(GRID_FIXED, GRID_FIXED, True, True)
          else
            MoveColRow(Max(GRID_FIXED, GetOtherFieldCol(GRID_FIXED)),
              GRID_FIXED, True, True);
        end
        else
        begin // normaler zeilenstart
          if not FPosInCharField then
            MoveColRow(GRID_FIXED, Row, True, True)
          else
            MoveColRow(Max(GRID_FIXED, GetOtherFieldCol(GRID_FIXED)),
              Row, True, True);
        end;
      end;

    VK_END:
      begin
        InCharField;
        if (ssCtrl in Shift) then
        begin // strg+end
          if (not InsertMode) then
            LgrcPosition := GetCursorAtPos(DataSize - 1, FPosInCharField)
          else
            LgrcPosition := GetCursorAtPos(DataSize, FPosInCharField);
          MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True)
        end
        else
        begin // normales zeilenende
          if not FPosInCharField then
          begin
            LIntCol := GetPosAtCursor(GRID_FIXED, Row + 1) - 1;
            TruncMaxPosition(LIntCol);
            LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
            MoveColRow(LgrcPosition.x + 1, LgrcPosition.y, True, True)
          end
          else
          begin
            LIntCol := GetPosAtCursor(GRID_FIXED, Row + 1) - 1;
            TruncMaxPosition(LIntCol);
            LgrcPosition := GetCursorAtPos(LIntCol, True);
            MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
          end
        end;
      end;

    VK_LEFT, VK_BACK:
      if (InsertMode and (not FReadOnlyView)) and (Key = VK_BACK) then
      begin
        if SelCount > 0 then
          DeleteSelection
        else
          InternalErase(True);
      end
      else if (not (ssCTRL in Shift)) then
      begin
        if FIsSelecting or (FUnicodeCharacters and FPosInCharField) then
          LIntCol := GetPosAtCursor(Col, Row) - FBytesPerUnit
        else
          LIntCol := GetPosAtCursor(Col, Row) - 1;
        if FPosInCharField then
        begin
          if LIntCol < 0 then
            LIntCol := 0;
          LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
          MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
        end
        else
        begin
          if FIsSelecting then
          begin
            CheckUnit(LIntCol);
            LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
            MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
          end
          else
          begin
            LIntCol := LIntCol + 1;
            LgrcPosition := GetCursorAtPos(LIntCol, False);
            if LgrcPosition.x < Col then
              MoveColRow(Col - 1, Row, True, True)
            else
            begin
              LIntCol := LIntCol - 1;
              if LIntCol >= 0 then
              begin
                LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
                MoveColRow(LgrcPosition.x + 1, LgrcPosition.y, True, True);
              end;
            end
          end;
        end;
      end
      else
      begin
        if Key = VK_LEFT then
        begin
          LIntCol := GRID_FIXED;
          MoveColRow(LIntCol, Row, True, True);
        end;
      end;

    VK_RIGHT:
      begin
        if (not (ssCTRL in Shift)) then
        begin
          if FIsSelecting or (FUnicodeCharacters and FPosInCharField) then
            LIntCol := GetPosAtCursor(Col, Row) + FBytesPerUnit
          else
            LIntCol := GetPosAtCursor(Col, Row) + 1;
          if FPosInCharField then
          begin
            TruncMaxPosition(LIntCol);
            LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
            MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
          end
          else
          begin
            if FIsSelecting then
            begin
              CheckUnit(LIntCol);
              TruncMaxPosition(LIntCol);
              LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
              MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
            end
            else
            begin
              LIntCol := LIntCol - 1;
              LgrcPosition := GetCursorAtPos(LIntCol, False);
              if (LgrcPosition.x = Col) and not (LIntCol = DataSize) then
                MoveColRow(Col + 1, Row, True, True)
              else
              begin
                LIntCol := LIntCol + 1;
                if (LIntCol < DataSize) or ((LIntCol = DataSize) and InsertMode)
                  then
                begin
                  LgrcPosition := GetCursorAtPos(LIntCol, FPosInCharField);
                  MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
                end;
              end
            end;
          end;
        end
        else
        begin
          LIntCol := GetLastCharCol;
          MoveColRow(LIntCol, Row, True, True);
        end;
      end;

    VK_DOWN:
      begin
        if (not (ssCTRL in Shift)) then
        begin
          LIntRow := Row + 1;

          LIntCol := Col;
          if LIntRow < RowCount then
          begin
            MoveColRow(LIntCol, LIntRow, True, True);
          end
          end;
      end;

    VK_UP:
      begin
        if (not (ssCTRL in Shift)) then
        begin
          LIntRow := Row - 1;
          LIntCol := Col;
          if LIntRow >= GRID_FIXED then
          begin
            MoveColRow(LIntCol, LIntRow, True, True);
          end
          end;
      end;

    Word('T'): if (ssCtrl in Shift) then
      begin
        Col := Max(GRID_FIXED, GetOtherFieldCol(Col));
      end;

    VK_TAB: if ((Shift = []) or (Shift = [ssShift])) then
      begin // tab-taste
        Col := Max(GRID_FIXED, GetOtherFieldCol(Col));
      end;

    Word('0')..Word('9'): if ssCtrl in Shift then
      begin
        if ssShift in Shift then
        begin
          LIntRow := GetPosAtCursor(Col, Row);
          SetBookmarkVals(Key - Ord('0'), LIntRow, FPosInCharField);
        end
        else
        begin
          GotoBookmark(Key - Ord('0'));
        end;
      end;

    VK_SHIFT: if (Shift = [ssShift]) or (Shift = [ssShift, ssCtrl]) then
      begin // selektion starten
        FIsSelecting := True;
      end;

    VK_DELETE: if (not FReadOnlyView) then
      begin
        if (SelCount > 0) and (InsertMode or (Shift = [ssCtrl])) then
          DeleteSelection
        else if InsertMode or (Shift = [ssCtrl]) then
          InternalErase(False)
      end;

    VK_INSERT: if (Shift = []) then InsertMode := not InsertMode;
  end;
end;

function TCustomBCHexEditor.HasChanged(aPos: integer): boolean;
begin
  Result := False;
  if InsertMode then
    Exit;

  if FModifiedBytes.Size > aPos then
    Result := FModifiedBytes.Bits[aPos];
end;

function TCustomBCHexEditor.IsSelected(const APosition: integer): boolean;
begin
  Result := False;
  if (FSelPosition <> -1) and (APosition >= FSelStart) and (APosition <= FSelEnd)
    then
  begin
    Result := True
  end;
end;

procedure TCustomBCHexEditor.NewSelection(SelFrom, SelTo: integer);
begin
  CheckSelectUnit(SelFrom, SelTo);
  SetSelection(SelFrom, Min(SelFrom, SelTo), Max(SelFrom, SelTo));
  Invalidate;
  SelectionChanged;
end;

function TCustomBCHexEditor.GetOffsetFormat: string;
begin
  Result := FOffsetFormat.Format;
end;

procedure TCustomBCHexEditor.SetOffsetFormat(const Value: string);
begin
  if Value <> FOffsetFormat.Format then
  try
    GenerateOffsetFormat(Value);
    SetOffsetDisplayWidth;
    Invalidate;
  except
    GenerateOffsetFormat(FOffsetFormat.Format);
    raise;
  end;
end;

procedure TCustomBCHexEditor.SetHexLowerCase(const Value: boolean);
begin
  if FHexLowerCase <> Value then
  begin
    FHexLowerCase := Value;
    if Value then
      Move(HEX_LOWER[1], FHexChars, sizeof(FHexChars))
    else
      Move(HEX_UPPER[1], FHexChars, sizeof(FHexChars));
    SetRulerString;
    Invalidate;
  end;
end;

procedure TCustomBCHexEditor.GenerateOffsetFormat(Value: string);
var
  LIntTemp: integer;
  LStrTemp: string;
begin
  with FOffsetFormat do
  begin
    Flags := [];
    LStrTemp := Value;
    // aufbau: [r|c|<HEXNUM>%][-|<HEXNUM>!]<HEXNUM>:[Prefix]|[Suffix]
    if LStrTemp <> '' then
    begin
      // bytes per unit
      if (Length(LStrTemp) >= 2) and (UpCase(LStrTemp[1]) = 'R') and (LStrTemp[2] = '%') then
      begin
        Flags := Flags + [offCalcRow];
        Delete(LStrTemp, 1, 2);
        _BytesPerUnit := BytesPerRow;
      end
      else if (Length(LStrTemp) >= 2) and (UpCase(LStrTemp[1]) = 'C') and (LStrTemp[2] = '%') then
      begin
        Flags := Flags + [offCalcColumn];
        Delete(LStrTemp, 1, 2);
        _BytesPerUnit := BytesPerColumn;
      end
      else
      begin
        LIntTemp := 1;
        while (LIntTemp <= Length(LStrTemp)) and
          CharInSet(LStrTemp[LIntTemp], ['0'..'9', 'A'..'F', 'a'..'f']) do
          Inc(LIntTemp);
        if Copy(LStrTemp, LIntTemp, 1) = '%' then
        begin
          // width field
          if LIntTemp = 1 then
          begin
            Flags := Flags + [offBytesPerUnit];
            _BytesPerUnit := FUsedRulerBytesPerUnit;
            Delete(LStrTemp, 1, 1)
          end
          else
          begin
            _BytesPerUnit := RadixToInt(Copy(LStrTemp, 1, LIntTemp - 1), 16);
            //  StrToInt('$'+Copy(LStrTemp, 1, LIntTemp-1));
            Delete(LStrTemp, 1, LIntTemp);
          end;
        end
        else
        begin
          Flags := Flags + [offBytesPerUnit];
          _BytesPerUnit := FUsedRulerBytesPerUnit;
        end;
      end;
      if not (_BytesPerUnit in [1, 2, 4, 8]) then
        raise EBCHexEditor.CreateFmt(ERR_INVALID_BPU, [_BytesPerUnit]);
      // auto calc width
      if Copy(LStrTemp, 1, 2) = '-!' then
      begin
        Flags := Flags + [offCalcWidth];
        Delete(LStrTemp, 1, 2);
        MinWidth := 1;
      end
      else
      begin
        // width ?
        LIntTemp := 1;
        while (LIntTemp <= Length(LStrTemp)) and
          CharInSet(LStrTemp[LIntTemp], ['0'..'9', 'A'..'F', 'a'..'f']) do
          Inc(LIntTemp);
        if Copy(LStrTemp, LIntTemp, 1) = '!' then
        begin
          // width field
          if LIntTemp = 1 then
          begin
            MinWidth := 1;
            Delete(LStrTemp, 1, 1)
          end
          else
          begin
            MinWidth := RadixToInt(Copy(LStrTemp, 1, LIntTemp - 1), 16);
            //  StrToInt('$'+Copy(LStrTemp, 1, LIntTemp-1));
            Delete(LStrTemp, 1, LIntTemp);
          end;
        end
        else
          MinWidth := 1;
      end;

      // radix
      LIntTemp := 1;
      while (LIntTemp <= Length(LStrTemp)) and
        CharInSet(LStrTemp[LIntTemp], ['0'..'9', 'A'..'F', 'a'..'f']) do
        Inc(LIntTemp);

      if LIntTemp = 1 then
        raise EBCHexEditor.CreateFmt(ERR_MISSING_FORMATCHAR, ['number radix']);

      if Copy(LStrTemp, LIntTemp, 1) <> ':' then
        raise EBCHexEditor.CreateFmt(ERR_MISSING_FORMATCHAR, [':']);

      Radix := RadixToInt(Copy(LStrTemp, 1, LIntTemp - 1), 16);
      if not (Radix in [2..16]) then
        raise EBCHexEditor.CreateFmt(ERR_INVALID_FORMATRADIX, [Radix]);

      Delete(LStrTemp, 1, LIntTemp);

      // prefix, suffix
      LIntTemp := Pos('|', string(LStrTemp));
      if LIntTemp = 0 then
        raise EBCHexEditor.CreateFmt(ERR_MISSING_FORMATCHAR, ['|']);

      Prefix := Copy(LStrTemp, 1, LIntTemp - 1);
      Suffix := Copy(LStrTemp, LIntTemp + 1, MaxInt);
    end;
    Format := Value;
  end;
end;

procedure TCustomBCHexEditor.Select(const aCurCol, aCurRow, aNewCol, aNewRow:
  integer);
var
  LIntOldStart,
    //LIntOldEnd,
  LIntNewStart,
    LIntNewEnd: integer;
begin
  //LIntOldEnd := FSelEnd;
  //LIntOldStart := FSelStart;
  LIntNewStart := GetPosAtCursor(aNewCol, aNewRow);
  if FSelPosition = -1 then
  begin
    LIntOldStart := LIntNewStart;
    //LIntOldEnd := LIntNewStart;
    LIntNewEnd := GetPosAtCursor(aCurCol, aCurRow);
    NewSelection(LIntNewEnd, LIntOldStart); // abcd
    //SetSelection(LIntNewEnd, Min(LIntOldStart, LIntNewEnd), Max(LIntNewEnd,
      //LIntOldEnd));
    //RedrawPos(FSelStart, FSelEnd)
  end
  else
    //begin
    NewSelection(FSelPosition, LIntNewStart); // abcd
  (*// testen, ob neue selection  /\ liegt als fSelPO
  // wenn ja, dann start = sel, ende = selpo
  if LIntNewStart < FSelPosition then
  begin
    NewSelection(FSelPosition, LIntNewStart);// abcd
    //SetSelection(FSelPosition, LIntNewStart, FSelPosition);
    //RedrawPos(Min(FSelStart, LIntOldStart), Max(FSelStart, LIntOldStart));
    //RedrawPos(Min(FSelEnd, LIntOldEnd), Max(FSelEnd, LIntOldEnd));
  end
  else
  begin
    NewSelection(FSelPosition, LIntNewStart); //abcd
    //SetSelection(FSelPosition, FSelPosition, LIntNewStart);
    //RedrawPos(Min(FSelStart, LIntOldStart), Max(FSelStart, LIntOldStart));
    //RedrawPos(Min(FSelEnd, LIntOldEnd), Max(FSelEnd, LIntOldEnd));
  end;
end;*)
end;

procedure TCustomBCHexEditor.ResetSelection(const aDraw: boolean);
var
  LIntOldStart: integer;
begin
  FIsSelecting := False;
  LIntOldStart := FSelStart;
  SetSelection(-1, -1, -1);
  FSelBeginPosition := -1;

  if aDraw and ((LIntOldStart > -1) or (LIntOldStart > -1)) then
    Invalidate;
end;

procedure TCustomBCHexEditor.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  LgrcDummy: TGridCoord;
  lboolInherited: boolean;
begin
  FIsSelecting := False;
  FMouseUpCanResetSel := False;

  if Button = mbLeft then
    LgrcDummy := CheckMouseCoord(X, Y);

  // do not change selection when clicking ruler or offset panel.
  if (not MouseOverSelection) and (not MouseOverFixed(x, y)) then
  begin
    lBoolInherited := True;
    inherited MouseDown(Button, Shift, x, y);
  end
  else
  begin
    lboolInherited := False;
    // but set focus if possible (05/27/2004)
    if not (csDesigning in ComponentState) and
      (CanFocus or (GetParentForm(Self) = nil)) then
      SetFocus;
  end;

  if (GetParentForm(self) <> nil) then
    if (GetParentForm(self).ActiveControl = self) then
      if GetParentForm(self) <> Screen.ActiveForm then
        if HandleAllocated then
          Windows.SetFocus(self.Handle);

  if (Button = mbLeft) and (not MouseOverSelection) and
    (LgrcDummy.X >= GRID_FIXED) and (LgrcDummy.Y >= GRID_FIXED) then
  begin
    ResetSelection(True);
    if not (ssDouble in Shift) then
      FIsSelecting := True;
  end;

  if (Button = mbLeft) and MouseOverSelection then
  begin
    FMouseDownCol := x;
    FMouseDownRow := y;
    FMouseUpCanResetSel := True;
  end;

  if (not lBoolInherited) and (Assigned(OnMouseDown)) and Focused then
    OnMouseDown(self, Button, Shift, X, Y);
end;

procedure TCustomBCHexEditor.InternalGetCurSel(var StartPos, EndPos, ACol, ARow:
  integer);
begin
  if FSelPosition = -1 then
  begin
    StartPos := GetPosAtCursor(Col, Row);
    EndPos := StartPos + 1;
    aCol := Col;
    aRow := Row;
  end
  else
  begin
    StartPos := FSelStart;
    EndPos := FSelEnd + 1;
    with GetCursorAtPos(FSelStart, InCharField) do
    begin
      aCOL := X;
      aROW := Y;
    end;
  end;

  if FModifiedBytes.Size > StartPos then
    FModifiedBytes.Size := StartPos;
end;

function TCustomBCHexEditor.CreateShift4BitStream(const StartPos: integer; var
  FName: TFileName): TFileStream;
var
  LByt1,
    LByt2: byte;
  LBytBuffer: array[0..511] of byte;
  LIntLoop,
    LIntRead: integer;
begin
  Result := nil;
  if StartPos >= DataSize then
    Exit;

  FName := GetTempName;
  Result := TFileStream.Create(FName, fmCreate);
  Result.Position := 0;
  FDataStorage.Position := StartPos;
  LByt1 := 0;
  while FDataStorage.Position < DataSize do
  begin
    FillChar(LBytBuffer[0], 512, 0);
    LIntRead := FDataStorage.Read(LBytBuffer[0], 512);
    for LIntLoop := 0 to Pred(LIntRead) do
    begin
      LByt2 := LBytBuffer[LIntLoop] and 15;
      LBytBuffer[LIntLoop] := (LBytBuffer[LIntLoop] shr 4) or (LByt1 shl 4);
      LByt1 := LByt2;
    end;
    Result.WriteBuffer(LBytBuffer[0], LIntRead);
  end;
  Result.Position := 0;
end;

function TCustomBCHexEditor.InternalInsertNibble(const Pos: integer; const
  HighNibble: boolean): boolean;
var
  LfstNibbleStream: TFileStream;
  LStrFName: TFileName;
  LIntOldSize: integer;
  LByteFirst,
    LByteLast: byte;
begin
  Result := False;

  if DataSize = 0 then
    Exit;

  LIntOldSize := FDataStorage.Size;

  WaitCursor;
  try
    // nun zuerst alle restlichen bits verschieben
    LByteFirst := Data[Pos];
    LByteLast := Data[Pred(DataSize)];

    LfstNibbleStream := CreateShift4BitStream(Pos, LStrFName);
    with LfstNibbleStream do
    try
      FDataStorage.Position := Pos;
      FDataStorage.CopyFrom(LfstNibbleStream, LfstNibbleStream.Size);
    finally
      Free;
      DeleteFile(LStrFName);
    end;

    if HighNibble then
      LByteFirst := LByteFirst shr 4
    else
      LByteFirst := LByteFirst and 240;
    Data[Pos] := LByteFirst;
    FDataStorage.Size := LIntOldSize + 1;
    Data[Pred(DataSize)] := LByteLast shl 4;
    Result := True;
  finally
    OldCursor;
  end;
end;

function TCustomBCHexEditor.InsertNibble(const aPos: integer; const HighNibble:
  boolean; const UndoDesc: string = ''): boolean;
const
  L_BytAppend: byte = 0;
begin
  Result := False;

  if DataSize < 1 then
  begin
    ResetSelection(False);
    AppendBuffer(PAnsiChar(@L_BytAppend), 1);
    Result := True;
    Exit;
  end;

  if (aPos >= DataSize) or (aPos < 0) then
    Exit;

  CreateUndo(ufKindNibbleInsert, aPos, 0, 0, UndoDesc);

  ResetSelection(False);
  Result := InternalInsertNibble(aPos, HighNibble);

  if Result and (FModifiedBytes.Size >= (aPos)) then
    FModifiedBytes.Size := aPos;

  CalcSizes;
  Changed;
end;

function TCustomBCHexEditor.InternalDeleteNibble(const Pos: integer; const
  HighNibble: boolean): boolean;
var
  LfstNibbleStream: TFileStream;
  LStrFName: TFileName;
  LIntOldSize: integer;
  LByt1: byte;
begin
  Result := False;
  if DataSize = 0 then
    Exit;

  LIntOldSize := FDataStorage.Size;
  WaitCursor;
  try
    // nun zuerst alle restlichen bits verschieben
    LByt1 := Data[Pos];

    LfstNibbleStream := CreateShift4BitStream(Pos, LStrFName);
    with LfstNibbleStream do
    try
      FDataStorage.Position := Pos;
      Position := 1;
      FDataStorage.CopyFrom(LfstNibbleStream, LfstNibbleStream.Size - 1);
    finally
      Free;
      DeleteFile(LStrFName);
    end;

    if not HighNibble then
      Data[Pos] := (LByt1 and 240) or (Data[Pos] and 15);

    Result := True;
    FDataStorage.Size := LIntOldSize;
    Data[Pred(DataSize)] := Data[Pred(DataSize)] shl 4;
  finally
    OldCursor;
  end;
end;

function TCustomBCHexEditor.DeleteNibble(const aPos: integer; const HighNibble:
  boolean; const UndoDesc: string = ''): boolean;
begin
  Result := False;

  if (aPos >= DataSize) or (aPos < 0) then
    Exit;

  CreateUndo(ufKindNibbleDelete, aPos, 0, 0, UndoDesc);

  ResetSelection(False);
  Result := InternalDeleteNibble(aPos, HighNibble);

  if Result and (FModifiedBytes.Size >= (aPos)) then
    FModifiedBytes.Size := aPos;

  CalcSizes;
  Changed;
end;

procedure TCustomBCHexEditor.InternalConvertRange(const aFrom, aTo: integer;
  const aTransFrom, aTransTo: TBCHTranslationKind);
var
  LIntSize: integer;
begin
  LIntSize := (aTo - aFrom) + 1;
  WaitCursor;
  try
    FDataStorage.TranslateToAnsi(aTransFrom, aFrom, LIntSize);
    FDataStorage.TranslateFromAnsi(aTransTo, aFrom, LIntSize);
  finally
    OldCursor;
  end;
end;

function TCustomBCHexEditor.IsOffsetFormatStored(): boolean;
begin
  Result := FOffsetFormat.Format <> DefaultOffsetFormat;
end;

procedure TCustomBCHexEditor.ConvertRange(const aFrom, aTo: integer; const
  aTransFrom, aTransTo: TBCHTranslationKind; const UndoDesc: string = '');
begin
  if aFrom > aTo then
    Exit;

  if aTransFrom = aTransTo then
    Exit;

  if (aTo >= DataSize) or (aFrom < 0) then
    Exit;

  CreateUndo(ufKindConvert, aFrom, (aTo - aFrom) + 1, 0, UndoDesc);

  InternalConvertRange(aFrom, aTo, aTransFrom, aTransTo);

  Invalidate;
  Changed;
end;

procedure TCustomBCHexEditor.InternalDelete(StartPos, EndPos, ACol, ARow:
  integer);
var
  LgrdEndPos: TGridCoord;
  LIntNewCol: integer;
begin
  if EndPos <= (DataSize - 1) then
    MoveFileMem(EndPos, StartPos, DataSize - EndPos);

  FDataStorage.Size := DataSize - (EndPos - StartPos);
  EndPos := GetPosAtCursor(aCol, aRow);

  if DataSize < 1 then
  begin
    LIntNewCol := GRID_FIXED;
    if FPosInCharField then
      LIntNewCol := Max(GRID_FIXED, GetOtherFieldColCheck(LIntNewCol));
    MoveColRow(LIntNewCol, GRID_FIXED, True, True)
  end
  else if EndPos >= DataSize then
  begin
    if InsertMode then
      LgrdEndPos := GetCursorAtPos(DataSize, FPosInCharField)
    else
      LgrdEndPos := GetCursorAtPos(DataSize - 1, FPosInCharField);
    MoveColRow(LgrdEndPos.x, LgrdEndPos.y, True, True);
  end
  else if ACol > -1 then
    MoveColRow(aCol, aRow, True, True);

  CalcSizes;
  ResetSelection(False);

  Invalidate;
end;

procedure TCustomBCHexEditor.DeleteSelection(const UndoDesc: string = '');
var
  LIntSelStart,
    LIntSelEnd,
    LIntCol,
    LIntRow: integer;
begin
  InternalGetCurSel(LIntSelStart, LIntSelEnd, LIntCol, LIntRow);
  CreateUndo(ufKindByteRemoved, LIntSelStart, LIntSelEnd - LIntSelStart,
    0, UndoDesc);

  InternalDelete(LIntSelStart, LIntSelEnd, LIntCol, LIntRow);
  Changed;
end;

procedure TCustomBCHexEditor.CreateUndo(const aKind: TBCHUndoFlag; const aPos,
  aCount, aReplCount: integer; const sDesc: string = '');
begin

  if CanCreateUndo(aKind, aCount, aReplCount) then
  begin
    if FUndoStorage.UpdateCount = 0 then
      FUndoStorage.CreateUndo(aKind, aPos, aCount, aReplCount, sDesc);
    FModified := True;
    //Changed;
  end
  else
    raise EBCHexEditor.Create(ERR_NOUNDO);
end;

procedure TCustomBCHexEditor.ResetUndo;
begin
  FUndoStorage.Reset;
end;

function TCustomBCHexEditor.GetCanUndo: boolean;
begin
  Result := (not FReadOnlyView) and FUndoStorage.CanUndo;
end;

function TCustomBCHexEditor.GetCanRedo: boolean;
begin
  Result := (not FReadOnlyView) and FUndoStorage.CanRedo;
end;

function TCustomBCHexEditor.GetUndoDescription: string;
begin
  if not (csDestroying in ComponentState) then
  begin
    with FUndoStorage do
      if CanUndo then
        Result := Description
      else
        Result := UNDO_NOUNDO;
  end
  else
    Result := UNDO_NOUNDO;
end;

function TCustomBCHexEditor.GetSelStart: integer;
begin
  if FSelPosition = -1 then
  begin
    Result := GetPosAtCursor(Col, Row);
  end
  else
    Result := FSelPosition;
end;

function TCustomBCHexEditor.GetSelEnd: integer;
begin
  if FSelPosition = -1 then
    Result := GetPosAtCursor(Col, Row)
  else
  begin
    Result := FSelEnd;
    if FSelPosition = FSelEnd then
      Result := FSelStart;
  end;
end;

procedure TCustomBCHexEditor.SetSelStart(aValue: integer);
begin
  if (aValue < 0) or (aValue >= DataSize) then
    raise EBCHexEditor.Create(ERR_INVALID_SELSTART)
  else
  begin
    ResetSelection(True);
    with GetCursorAtPos(aValue, InCharField) do
      MoveColRow(X, Y, True, True);
  end;
end;

procedure TCustomBCHexEditor.SetSelEnd(aValue: integer);
begin
  if (aValue < -1) or (aValue >= DataSize) then
    raise EBCHexEditor.Create(ERR_INVALID_SELEND)
  else
  begin
    ResetSelection(True);
    if aValue > -1 then
    begin
      with GetCursorAtPos(aValue, InCharField) do
        Select(Col, Row, X, Y);
      SelectionChanged;
    end;
  end;
end;

procedure TCustomBCHexEditor.SetSelCount(aValue: integer);
begin
  SetSelEnd(Min(SelStart+aValue-1, DataSize-1));
end;

procedure TCustomBCHexEditor.SetInCharField(const Value: boolean);
begin
  if (DataSize < 1) then
    Exit;

  if InCharField <> Value then
    MoveColRow(GetOtherFieldCol(Col), Row, True, True);
end;

function TCustomBCHexEditor.GetInCharField: boolean;
begin
  Result := False;
  if DataSize < 1 then
    Exit;

  GetPosAtCursor(Col, Row);
  Result := FPosInCharField;
end;

procedure TCustomBCHexEditor.Loaded;
begin
  inherited;
  CreateEmptyFile(UNNAMED_FILE);
end;

procedure TCustomBCHexEditor.CreateWnd;
begin
  inherited;
  if (csDesigning in ComponentState) or (FFileName = '---') then
    CreateEmptyFile(UNNAMED_FILE);
end;

procedure TCustomBCHexEditor.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  CreateCaretGlyph;
  CheckSetCaret;
  Invalidate;
end;

procedure TCustomBCHexEditor.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  HideCaret(Handle);
  DestroyCaret();
  FIsSelecting := False;
  Invalidate;
end;

procedure TCustomBCHexEditor.CMINTUPDATECARET(var Msg: TMessage);
begin
  if Msg.WParam = 7 then
  begin
    CheckSetCaret;
  end;
end;

procedure TCustomBCHexEditor.SetTranslation(const Value: TBCHTranslationKind);
begin
  if FTranslation <> Value then
  begin
    if (Value <> tkAsIs) and FUnicodeCharacters then
      raise EBCHexEditor.Create(ERR_NO_TRANSLATION_IN_UNICODE_MODE);
    FTranslation := Value;
    Invalidate;
  end;
end;

procedure TCustomBCHexEditor.SetModified(const Value: boolean);
begin
  FModified := Value;
  if not Value then
  begin
    ResetUndo;
    FModifiedBytes.Size := 0;
    Invalidate;
  end;
end;

procedure TCustomBCHexEditor.SetBytesPerRow(const Value: integer);
var
  LIntPos,
    LIntSelPos,
    LIntSelStart,
    LIntSelEnd: integer;
  LBoolInCharField,
    LBool2ndCol: boolean;
  LCoord: TGridCoord;
begin
  if FAutoBytesPerRow and (not FSetAutoBytesPerRow) then
    Exit;
  if ((Value < 1) or (Value > 256)) or
    (FUnicodeCharacters and ((Value mod 2) <> 0)) then
    raise EBCHexEditor.Create(ERR_INVALID_BYTESPERLINE)
  else if FBytesPerRow <> Value then
  begin
    with FOffsetFormat do
      if offCalcRow in Flags then
        _BytesPerUnit := Value;
    LIntSelPos := FSelPosition;
    LIntSelStart := FSelStart;
    LIntSelEnd := FSelEnd;
    LIntPos := GetPosAtCursor(Col, Row);
    LBoolInCharField := FPosInCharField;
    LBool2ndCol := GetCursorAtPos(LIntPos, LBoolInCharField).x <> Col;
    FBytesPerRow := Value;
    FBytesPerRowDup := Value * 2;
    FIntLastHexCol := (GRID_FIXED + FBytesPerRowDup - 1);
    SetRulerString;
    CalcSizes;
    if (LIntPos >= DataSize) or (InsertMode and (LIntPos > DataSize)) then
      LIntPos := DataSize - 1;

    LCoord := GetCursorAtPos(LIntPos, LBoolInCharField);
    with LCoord do
    begin
      if LBool2ndCol then
        Inc(x);

      MoveColRow(x, y, True, True);
    end;

    SetSelection(LIntSelPos, LIntSelStart, LIntSelEnd);
  end;
end;

procedure TCustomBCHexEditor.InternalAppendBuffer(Buffer: PAnsiChar; const Size:
  integer);
var
  LIntSize: integer;
begin
  if DataSize = 0 then
  begin
    FDataStorage.Position := 0;
    FModifiedBytes.Size := 0;
  end;

  LIntSize := DataSize;
  FDataStorage.Size := LIntSize + Size;
  WriteBuffer(Buffer^, LIntSize, Size);
  CalcSizes;
end;

procedure TCustomBCHexEditor.InternalInsertBuffer(Buffer: PAnsiChar; const Size,
  Position: integer);
var
  LIntSize: integer;
begin
  if DataSize = 0 then
  begin
    FDataStorage.Position := 0;
    FModifiedBytes.Size := 0;
  end;

  LIntSize := DataSize;
  FDataStorage.Size := LIntSize + Size;
  if Position < LIntSize then
    // nur, wenn nicht hinter streamende, dann platz schaffen
    MoveFileMem(Position, Position + Size, DataSize - Position - Size); //+ 1);

  if Buffer <> nil then
    WriteBuffer(Buffer^, Position, Size);
  CalcSizes;
end;

procedure TCustomBCHexEditor.InsertBuffer(aBuffer: PAnsiChar; const aSize, aPos:
  integer; const UndoDesc: string = ''; const MoveCursor: Boolean = True);
begin
  //FDataStorage.CheckBounds(aPos);
  CreateUndo(ufKindInsertBuffer, aPos, aSize, 0, UndoDesc);

  InternalInsertBuffer(aBuffer, aSize, aPos);

  if FModifiedBytes.Size >= (aPos) then
    FModifiedBytes.Size := aPos;

  if Enabled then
  begin
    SetSelection(aPos, aPos, aPos + aSize - 1);
    if MoveCursor then
    begin
      with GetCursorAtPos(FSelEnd, InCharField) do
        MoveColRow(x, y, True, True);
      SetSelection(aPos, aPos, aPos + aSize - 1);
    end;
    Invalidate;
  end;
  Changed;
end;

procedure TCustomBCHexEditor.AppendBuffer(aBuffer: PAnsiChar; const aSize: integer;
  const UndoDesc: string = ''; const MoveCursor: Boolean = True);
var
  LIntSize: integer;
begin
  if (not Assigned(aBuffer)) or (aSize = 0) then
    Exit;

  CreateUndo(ufKindAppendBuffer, DataSize, aSize, 0, UndoDesc);

  if FModifiedBytes.Size >= (DataSize) then
    FModifiedBytes.Size := DataSize;

  LIntSize := DataSize;
  InternalAppendBuffer(aBuffer, aSize);

  if MoveCursor then
    with GetCursorAtPos(LIntSize, InCharField) do
      MoveColRow(x, y, True, True);
  SetSelection(LIntSize, LIntSize, LIntSize + aSize - 1);
  Invalidate;
  Changed;
end;

procedure TCustomBCHexEditor.ReplaceSelection(aBuffer: PAnsiChar; aSize: integer;
  const UndoDesc: string = ''; const MoveCursor: Boolean = True);
var
  LIntStart,
    LIntEnd,
    LIntCol,
    LIntRow: integer;
  LBoolInCharField: boolean;
begin
  // auswahl berechnen
  LBoolInCharField := GetInCharField;
  if FSelPosition = -1 then
    InsertBuffer(aBuffer, aSize, SelStart, UndoDesc, MoveCursor)
  else
  begin
    if IsFileSizeFixed then
    begin
      if aSize > SelCount then
        aSize := SelCount
      else if SelCount > aSize then
      begin
        SelStart := Min(SelStart, SelEnd);
        SelEnd := SelStart + aSize - 1;
      end;
    end;

    CreateUndo(ufKindReplace, FSelStart, aSize, SelCount, UndoDesc);

    // zuerst aktuelle auswahl löschen
    InternalGetCurSel(LIntStart, LIntEnd, LIntCol, LIntRow);
    InternalDelete(LIntStart, LIntEnd, LIntCol, LIntRow);
    InternalInsertBuffer(aBuffer, aSize, LIntStart);
    if FModifiedBytes.Size >= LIntStart then
      FModifiedBytes.Size := Max(0, LIntStart);

    if MoveCursor then
    begin
      with GetCursorAtPos(LIntStart + aSize - 1, LBoolInCharField) do
        MoveColRow(x, y, True, True);
      SetSelection(LIntStart + aSize - 1, LIntStart, LIntStart + aSize - 1);
    end;
    Invalidate;
    Changed;
  end;
end;

procedure TCustomBCHexEditor.SetChanged(DataPos: integer; const Value: boolean);
begin
  if InsertMode then
    FModifiedBytes.Size := 0;

  if not Value then
    if FModifiedBytes.Size <= DataPos then
      Exit;

  FModifiedBytes[DataPos] := Value;
end;

procedure TCustomBCHexEditor.MoveFileMem(const aFrom, aTo, aCount: integer);
begin
  FDataStorage.Move(aFrom, aTo, aCount);
end;

function TCustomBCHexEditor.GetCursorPos: integer;
begin
  Result := GetPosAtCursor(Col, Row);
  if Result < 0 then
    Result := 0;

  if Result > Max(0, DataSize - 1) then
    Result := Max(0, DataSize - 1)
end;

function TCustomBCHexEditor.GetSelCount: integer;
begin
  if FSelPosition = -1 then
    Result := 0
  else
    Result := Max(FSelStart, FSelEnd) - Min(FSelStart, FSelEnd) + 1;
end;

procedure TCustomBCHexEditor.SetReadOnlyFile(const Value: boolean);
begin
  if Value and (not FIsFileReadonly) then
  begin
    FIsFileReadonly := True;
  end;
end;

function TCustomBCHexEditor.BufferFromFile(const aPos: integer; var aCount:
  integer): PChar;
begin
  if (aPos < 0) or (aPos >= DataSize) then
    raise EBCHexEditor.Create(ERR_INVALID_BUFFERFROMFILE)
  else
  begin
    if (aPos + aCount) > DataSize then
      aCount := (DataSize - aPos) + 1;

    GetMem(Result, aCount);
    try
      FDataStorage.ReadBufferAt(Result^, aPos, aCount);
    except
      try
        FreeMem(Result);
      except
      end;
      Result := nil;
      aCount := 0;
    end;
  end;
end;

procedure TCustomBCHexEditor.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  CheckSetCaret;
end;

procedure TCustomBCHexEditor.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  CheckSetCaret;
end;

procedure TCustomBCHexEditor.CreateCaretGlyph;
begin
  DestroyCaret();
  FCaretBitmap.Width := FCharWidth;
  FCaretBitmap.Height := FCharHeight - 2;
  FCaretBitmap.Canvas.Brush.Color := clBlack;
  FCaretBitmap.Canvas.FillRect(Rect(0, 0, FCharWidth, FCharHeight - 2));
  FCaretBitmap.Canvas.Brush.Color := clWhite;
  case FCaretKind of
    ckFull: FCaretBitmap.Canvas.FillRect(Rect(0, 0, FCharWidth, FCharHeight -
        2));
    ckLeft: FCaretBitmap.Canvas.FillRect(Rect(0, 0, 2, FCharHeight - 2));
    ckBottom: FCaretBitmap.Canvas.FillRect(Rect(0, FCharHeight - 4, FCharWidth,
        FCharHeight - 2));
    ckAuto:
      begin
        if FReadOnlyView then
          FCaretBitmap.Canvas.FillRect(Rect(0, FCharHeight - 4, FCharWidth,
            FCharHeight - 2))
        else
        begin
          if FInsertModeOn then
            FCaretBitmap.Canvas.FillRect(Rect(0, 0, 2, FCharHeight - 2))
          else
            FCaretBitmap.Canvas.FillRect(Rect(0, 0, FCharWidth, FCharHeight -
              2));
        end;
      end;
  end;
  CreateCaret(Handle, FCaretBitmap.Handle, 0, 0);
  ShowCaret(Handle);
end;

procedure TCustomBCHexEditor.SetBytesPerColumn(const Value: integer);
begin
  if ((Value < 1) or (Value > 256)) or
    (FUnicodeCharacters and ((Value mod 2) <> 0)) then
    raise EBCHexEditor.Create(ERR_INVALID_BYTESPERCOL)
  else if FBytesPerCol <> (Value * 2) then
  begin
    with FOffsetFormat do
      if offCalcColumn in Flags then
        _BytesPerUnit := Value;
    FBytesPerCol := Value * 2;
    AdjustMetrics;
    SetRulerString;
    Invalidate;
  end;
end;

function TCustomBCHexEditor.GetBytesPerColumn: integer;
begin
  Result := FBytesPerCol div 2;
end;

function TCustomBCHexEditor.Find(aPattern: Pointer; aCount: integer; const aStart,
  aEnd: integer): integer;
var
  I: Integer;
  J: Integer;
  LData: PAnsiChar;
begin
  LData := PAnsiChar(GetFastPointer(aStart, aEnd - AStart));

  for I := 0 to aEnd - aStart - aCount do
  begin
    J := 0;
    while ((J < aCount) and (PAnsiChar(aPattern)[J] = LData[I + J])) do
      Inc(J);
    if (J < aCount) then
      Exit(aStart + I);
  end;

  Result := -1;
end;

function TCustomBCHexEditor.Find(aPattern: string; const aStart, aEnd: integer;
  const IgnoreCase: boolean): integer;
var
  LData: string;
  LPattern: string;
begin
  if (aPattern = '') then
    Result := -1
  else if ((aEnd >= DataSize) or (Length(aPattern) > aEnd - aStart)) then
    raise ERangeError.Create(SRangeError)
  else
  begin
    if (not IgnoreCase) then
      LPattern := aPattern
    else
    begin
      LPattern := Copy(aPattern, 1, Length(aPattern)); // Make sure, the compiler creates a new string
      CharLowerBuff(PChar(LPattern), Length(lPattern));
    end;

    SetLength(LData, (aEnd - AStart) div 2);
    if (not UnicodeChars) then
      MultiByteToWideChar(CP_ACP, MB_ERR_INVALID_CHARS, PAnsiChar(GetFastPointer(aStart, aEnd - AStart)), aEnd - AStart, PChar(LData), Length(LData))
    else
      MoveMemory(PChar(LData), GetFastPointer(aStart, aEnd - AStart), aEnd - AStart);
    if (IgnoreCase) then
      CharLowerBuff(PChar(LData), (aEnd - AStart) div 2);

    Result := Pos(LPattern, LData);
    if (Result >= 0) then
      Inc(Result, aStart);
  end;
end;

procedure TCustomBCHexEditor.AddSelectionUndo(const AStart,
  ACount: integer);
begin
  CreateUndo(ufKindSelection, AStart, aCount, 0, '');
end;

procedure TCustomBCHexEditor.SetOffsetDisplayWidth;
var
  s: string;
begin
  if Assigned(FOnGetOffsetText) and (not FOffsetHandler) then
  begin
    FOffsetHandler := True;
    try
      FIsMaxOffset := True;
      FOnGetOffsetText(self, (RowCount - 3) * FBytesPerRow, s);
    finally
      FOffsetHandler := False;
    end;
    FOffsetDisplayWidth := Length(s) + 1;
  end
  else
  begin
    with FOffsetFormat do
      if offCalcWidth in Flags then
        MinWidth := Length(IntToRadix(((RowCount - 3) * FBytesPerRow) div
          _BytesPerUnit, Radix));

    FOffSetDisplayWidth := Length(GetOffsetString((RowCount - 3) * FBytesPerRow))
      + 1;
  end;
  if FGutterWidth = -1 then
    DoSetCellWidth(0, FOffSetDisplayWidth * FCharWidth + 20 + 1)
  else
    DoSetCellWidth(0, FGutterWidth);
end;

function TCustomBCHexEditor.Seek(const aOffset, aOrigin: integer): integer;
var
  LIntPos: integer;
begin
  Result := -1;
  LIntPos := GetCursorPos;
  case aOrigin of
    soFromBeginning: LIntPos := aOffset;
    soFromCurrent: LIntPos := GetCursorPos + aOffset;
    soFromEnd: LIntPos := DataSize + aOffset - 1;
  end;

  if DataSize < 1 then
    Exit;

  LIntPos := Min(Max(0, LIntPos), DataSize - 1);

  SelStart := LIntPos;
  Result := LIntPos;
end;

procedure TCustomBCHexEditor.SetSwapNibbles(const Value: boolean);
begin
  if integer(Value) <> FSwapNibbles then
  begin
    FSwapNibbles := integer(Value);
    Invalidate;
  end;
end;

function TCustomBCHexEditor.GetSwapNibbles: boolean;
begin
  Result := boolean(FSwapNibbles);
end;

procedure TCustomBCHexEditor.SetColors(const Value: TBCHColors);
begin
  FColors.Assign(Value);
end;

procedure TCustomBCHexEditor.SetCaretKind(const Value: TBCHCaretKind);
begin
  if FCaretKind <> Value then
  begin
    FCaretKind := Value;
    if Focused then
    begin
      CreateCaretGlyph;
      IntSetCaretPos(-50, -50, -1);
      Invalidate;
    end;
  end;
end;

procedure TCustomBCHexEditor.SetFocusFrame(const Value: boolean);
begin
  if FFocusFrame <> Value then
  begin
    FFocusFrame := Value;
    Invalidate;
  end;
end;

procedure TCustomBCHexEditor.SetMaskChar(const Value: char);
begin
  if FReplaceUnprintableCharsBy <> Value then
  begin
    FReplaceUnprintableCharsBy := Value;
    Invalidate;
  end;
end;

procedure TCustomBCHexEditor.SetAsText(const Value: AnsiString);
var
  LpszBuffer: PAnsiChar;
begin
  if DataSize > 0 then
  begin
    // alles selektieren
    SelStart := 0;
    SelEnd := DataSize - 1;
  end;
  // do translation (thanks to philippe chessa)  dec 17 98
  GetMem(LpszBuffer, Length(Value));
  try
    Move(Value[1], LpszBuffer^, Length(Value));
    TranslateBufferFromANSI(FTranslation, @Value[1], LpszBuffer, Length(Value));
    ReplaceSelection(LpszBuffer, Length(Value));
  finally
    FreeMem(LpszBuffer);
  end;
end;

procedure TCustomBCHexEditor.SetAsHex(const Value: string);
var
  LpszBuffer: PAnsiChar;
  LIntAmount: integer;
begin
  if DataSize > 0 then
  begin
    // alles selektieren
    SelStart := 0;
    SelEnd := DataSize - 1;
  end;

  GetMem(LpszBuffer, Length(Value) * SizeOf(Value[1]));
  try
    ConvertHexToBin(@Value[1], LpszBuffer, Length(Value), SwapNibbles,
      LIntAmount);
    ReplaceSelection(LpszBuffer, LIntAmount);
  finally
    FreeMem(LpszBuffer);
  end;
end;

function TCustomBCHexEditor.GetAsText: AnsiString;
begin
  if DataSize < 1 then
    Result := ''
  else
  begin
    SetLength(Result, DataSize);
    ReadBuffer(Result[1], 0, DataSize);
  end;
end;

function TCustomBCHexEditor.GetAsHex: string;
begin
  Result := FDataStorage.GetAsHex(0, DataSize, SwapNibbles)
end;

function TCustomBCHexEditor.GetSelectionAsHex: string;
begin
  if (DataSize < 1) or (SelCount < 1) then
    Result := ''
  else
    Result := FDataStorage.GetAsHex(Min(SelStart, SelEnd), SelCount,
      SwapNibbles);
end;

function TCustomBCHexEditor.GetInsertMode: boolean;
begin
  Result := FInsertModeOn and IsInsertModePossible;
end;

procedure TCustomBCHexEditor.SetAllowInsertMode(const Value: boolean);
begin
  if not Value then
  begin
    if FInsertModeOn then
      InsertMode := False;
  end;
  FAllowInsertMode := Value;
end;

procedure TCustomBCHexEditor.SetFixedFileSize(const Value: boolean);
begin
  if Value <> FFixedFileSize then
  begin
    if Value then
      InsertMode := False;
    FFixedFileSize := Value;
  end;
end;

procedure TCustomBCHexEditor.InternalErase(const KeyWasBackspace: boolean; const
  UndoDesc: string = '');
var
  LIntPos: integer;
  LIntSavePos: integer;
  LIntCount: integer;
begin
  LIntPos := GetCursorPos div FBytesPerUnit * FBytesPerUnit;
  LIntCount := FBytesPerUnit;
  LIntSavePos := LIntPos;
  if KeyWasBackspace then
  begin // Delete previous byte(s)
    if InsertMode and (SelCount = 0) then
    begin
      LIntPos := GetPosAtCursor(Col, Row);
      if (LIntPos = DataSize) and ((DataSize mod FBytesPerUnit) <> 0) then
        LIntCount := 1
      else
      begin
        LIntPos := LIntPos div FBytesPerUnit * FBytesPerUnit;
        LIntCount := FBytesPerUnit;
      end;
    end;

    if LIntPos = 0 then
      Exit; // Can't delete at offset -1

    CreateUndo(ufKindByteRemoved, LIntPos - LIntCount, LIntCount,
      0, UndoDesc);

    InternalDelete(LIntPos - LIntCount, LIntPos, Col, Row);
    if LIntSavePos = LIntPos then
      Seek(LIntPos - LIntCount, soFromBeginning) // Move caret
    else
    begin
      if (Col + 1) <= GetLastCharCol then
        Col := Col + 1;
    end;
    Changed;
  end
  else
  begin // Delete next byte
    if LIntPos >= DataSize then
      Exit; // Cant delete at EOF
    while (LIntPos + LIntCount) > DataSize do
      Dec(LIntCount);
    CreateUndo(ufKindByteRemoved, LIntPos, LIntCount, 0, UndoDesc);
    InternalDelete(LIntPos, LIntPos + LIntCount, Col, Row);
    Changed;
  end;
end;

procedure TCustomBCHexEditor.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTARROWS or DLGC_WANTCHARS or
    DLGC_WANTALLKEYS;
  if FWantTabs then
    Msg.Result := Msg.Result or DLGC_WANTTAB
  else
    Msg.Result := Msg.Result and not DLGC_WANTTAB;
end;

procedure TCustomBCHexEditor.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then
  begin
    AdjustMetrics;
    if Focused then
    begin
      CreateCaretGlyph;
    end;
  end;
end;

procedure TCustomBCHexEditor.SetWantTabs(const Value: boolean);
begin
  FWantTabs := Value;
end;

procedure TCustomBCHexEditor.SetReadOnlyView(const Value: boolean);
begin
  FReadOnlyView := Value;

  if (FCaretKind = ckAuto) and Focused then
    CreateCaretGlyph;
end;

procedure TCustomBCHexEditor.SetHideSelection(const Value: boolean);
begin
  if FHideSelection <> Value then
  begin
    FHideSelection := Value;
    if (not Focused) and (GetSelCount > 0) then
      Invalidate;
  end;
end;

procedure TCustomBCHexEditor.SetGraySelectionIfNotFocused(const Value: boolean);
begin
  if FGraySelOnLostFocus <> Value then
  begin
    FGraySelOnLostFocus := Value;
    if (not Focused) and (GetSelCount > 0) and (not FHideSelection) then
      Invalidate;
  end;
end;

function TCustomBCHexEditor.CalcColCount: integer;
begin
  if FUnicodeCharacters then
    Result := (FBytesPerRow * 2) + (FBytesPerRow div 2) + 1 + GRID_FIXED
  else
    Result := FBytesPerRow * 3 + 1 + GRID_FIXED;
end;

function TCustomBCHexEditor.GetLastCharCol: integer;
begin
  Result := ColCount - 1;
end;

function TCustomBCHexEditor.GetTopLeftPosition(var oInCharField: boolean):
  integer;
begin
  Result := GetPosAtCursor(Max(LeftCol, GRID_FIXED), TopRow);
  oInCharField := InCharField;
end;

procedure TCustomBCHexEditor.SetTopLeftPosition(const aPosition: integer; const
  aInCharField: boolean);
begin
  with GetCursorAtPos(aPosition, aInCharField) do
  begin
    TopRow := y;
    LeftCol := x;
  end;
end;

function TCustomBCHexEditor.GetPropColCount: integer;
begin
  Result := inherited ColCount;
end;

function TCustomBCHexEditor.GetPropRowCount: integer;
begin
  Result := inherited RowCount;
end;

function TCustomBCHexEditor.ShowDragCell(const X, Y: integer): integer;
var
  LIntDragPos,
    LIntMouseX,
    LIntMouseY: integer;
  LCoord: TGridCoord;
begin
  LCoord := MouseCoord(X, Y);
  with LCoord do
  begin
    LIntMouseX := X;
    LIntMouseY := Y;
    if X < GRID_FIXED then
      X := GRID_FIXED;
    if Y >= RowCount then
      Y := RowCount - 1;
    if Y < GRID_FIXED then
      Y := GRID_FIXED;
    LIntDragPos := GetPosAtCursor(X, Y)
  end;

  if LIntDragPos < 0 then
    LIntDragPos := 0;
  if LIntDragPos > DataSize then
    LIntDragPos := DataSize;
  if IsSelected(LIntDragPos) then
    LIntDragPos := Min(SelStart, SelEnd);
  CheckUnit(LIntDragPos);
  Result := LIntDragPos;
  FShowDrag := True;

  if (LIntMouseY <= TopRow) and (LIntMouseY > GRID_FIXED) then
  begin
    // nach oben scrollen
    TopRow := TopRow - 1;
  end
  else if (LIntMouseY >= (TopRow + VisibleRowCount - 1)) and (LIntMouseY <
    Pred(RowCount)) then
  begin
    // nach unten scrollen
    TopRow := TopRow + 1;
  end;

  if (LIntMouseX <= LeftCol) and (LIntMouseX > GRID_FIXED) then
  begin
    // nach links scrollen
    LeftCol := LeftCol - 1;
  end
  else if (LIntMouseX >= (LeftCol + VisibleColCount - 1)) and
    (LIntMouseX < GetLastCharCol) then
  begin
    // nach unten scrollen
    LeftCol := LeftCol + 1;
  end;

  with GetCursorAtPos(LIntDragPos, FPosInCharField) do
  begin
    if (x = FDropCol) and (y = FDropRow) then
      Exit;
    FDropCol := x;
    FDropRow := y;
    Invalidate;
  end;
end;

procedure TCustomBCHexEditor.HideDragCell;
begin
  FShowDrag := False;
  Invalidate;
end;

procedure TCustomBCHexEditor.CombineUndo(const aCount: integer; const sDesc:
  string = '');
begin
  CreateUndo(ufKindCombined, 0, aCount, 0, sDesc);
end;

function TCustomBCHexEditor.GetMouseOverSelection: boolean;
var
  LPntMouse: TPoint;
begin
  Windows.GetCursorPos(LPntMouse);
  LPntMouse := ScreenToClient(LPntMouse);
  Result := CursorOverSelection(LPntMouse.x, LPntMouse.y);
end;

function TCustomBCHexEditor.CursorOverSelection(const X, Y: integer): boolean;
var
  LIntPos: integer;
  LBoolInCharField: boolean;
begin
  Result := False;
  if (SelCount = 0) or (DataSize = 0) then
    Exit;

  LBoolInCharField := FPosInCharField;
  with MouseCoord(x, y) do
  begin
    if (x < GRID_FIXED) or (y < GRID_FIXED) then
      Exit;

    LIntPos := GetPosAtCursor(X, Y);
    FPosInCharField := (LBoolInCharField);
    if (LIntPos < 0) or (LIntPos >= DataSize) then
      Exit;
  end;

  Result := IsSelected(LIntPos);
end;

function TCustomBCHexEditor.MouseOverFixed(const X, Y: integer): boolean;
begin
  with MouseCoord(x, y) do
    Result := (x < GRID_FIXED) or (y < GRID_FIXED);
end;

procedure TCustomBCHexEditor.MouseMove(Shift: TShiftState; X, Y: integer);
var
  LgrcCoords: TGridCoord;
begin
  if Shift = [ssLeft] then
    LgrcCoords := CheckMouseCoord(X, Y);

  inherited MouseMove(Shift, x, y);

  if FMouseUpCanResetSel then
  begin
    FMouseUpCanResetSel := (LgrcCoords.x = FMouseDownCol) and
      (LgrcCoords.y = FMouseDownRow);
  end;

  if (Shift = []) and (CursorOverSelection(X, Y) or MouseOverFixed(X, Y)) then
    Cursor := crArrow
  else
    Cursor := crIBeam;
end;

procedure TCustomBCHexEditor.WMTimer(var Msg: TWMTimer);
var
  LPtMouse: TPoint;
  LgrcCoord: TGridCoord;
begin
  if FGridState <> gsSelecting then
    Exit;
  Windows.GetCursorPos(LPtMouse);
  LPtMouse := ScreenToClient(LPtMouse);
  LgrcCoord := CheckMouseCoord(LPtMouse.X, LPtMouse.Y);
  if (LGrcCoord.X <> -1) and (LGrcCoord.Y <> -1) then
    inherited;
end;

function TCustomBCHexEditor.CheckMouseCoord(var X, Y: integer): TGridCoord;
var
  LRctCell: TRect;
begin
  Result := MouseCoord(X, Y);
  if FInsertModeOn then
  begin
    // use the following cell if the mouse is over the second half of the cell
    LRctCell := CellRect(Result.X, Result.Y);
    if (LRctCell.Left + (FCharWidth div 2)) <= X then
    begin
      if not (Result.X in [GetLastCharCol, FBytesPerRowDup + GRID_FIXED - 1])
        then
      begin
        X := LRctCell.Right + 1;
        Inc(Result.X);
        LRctCell := CellRect(Result.X, Result.Y);
      end;
    end;
    if (Result.X = GetLastCharCol) then
    begin
      if (X - LRctCell.Left) > (FCharWidth div 2) then
      begin
        Y := Y + RowHeight;
        Result.Y := Result.Y + 1;
        Result.X := FBytesPerRowDup + 1 + GRID_FIXED;
        X := CellRect(Result.X, Result.Y - 1).Left;
        //Dec(X, FCharWidth * FBytesPerRow);
      end;
    end
    else if Result.X = (FBytesPerRowDup + GRID_FIXED - 1) then
    begin
      if (X - LRctCell.Left) > (FCharWidth div 2) then
      begin
        Y := Y + RowHeight;
        Result.Y := Result.Y + 1;
        Result.X := GRID_FIXED;
        X := CellRect(Result.X, Result.Y - 1).Left;
        //Dec(X, FCharWidth * FBytesPerRow);
      end;
    end;
  end;
end;

procedure TCustomBCHexEditor.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  CheckMouseCoord(X, Y);
  inherited;
  if FMouseUpCanResetSel then
  begin
    FMouseUpCanResetSel := False;
    ResetSelection(True);
    with MouseCoord(x, y) do
      MoveColRow(x, y, True, True);
  end;
  if FShowDrag then
    HideDragCell;
end;

procedure TCustomBCHexEditor.AdjustBookmarks(const From, Offset: integer);
var
  LIntLoop: integer;
  LBoolChanged: boolean;
begin
  LBoolChanged := False;
  if From >= 0 then
    for LIntLoop := 0 to 9 do
      with FBookmarks[LIntLoop] do
        if mPosition >= From then
        begin
          LBoolChanged := True;
          Inc(mPosition, Offset);
          if mPosition > DataSize then
            mPosition := -1;
        end;
  if LBoolChanged then
    BookMarkChanged;
end;

procedure TCustomBCHexEditor.IntSetCaretPos(const X, Y, aCol: integer);
begin
  if Focused then
  begin
    if aCol <> -1 then
    begin
      FPosInCharField := (aCol > (GRID_FIXED + FBytesPerRowDup));
      if FLastPosInCharField <> FPosInCharField then
      begin
        FLastPosInCharField := FPosInCharField;
        Invalidate;
      end;
    end;
    SetCaretPos(X, Y);
  end;
end;

procedure TCustomBCHexEditor.TruncMaxPosition(var DataPos: integer);
begin
  if DataPos >= DataSize then
  begin
    DataPos := DataSize - 1;
    if InsertMode then
      DataPos := DataSize;
  end;
end;

function TCustomBCHexEditor.GetCurrentValue: integer;
var
  LIntPos: integer;
begin
  Result := -1;
  LIntPos := GetPosAtCursor(Col, Row);
  if (LIntPos >= DataSize) or (LIntPos < 0) then
    Exit;
  Result := Data[LIntPos]
end;

procedure TCustomBCHexEditor.SetInsertMode(const Value: boolean);
var
  LIntPos: integer;
begin
  if Value = FInsertModeOn then
    Exit;
  if IsInsertModePossible then
  begin
    FInsertModeOn := Value;
    if (FCaretKind = ckAuto) and Focused then
      CreateCaretGlyph;
    if DataSize < 1 then
      Exit;
    if not FInsertModeOn then
    begin
      if ((DataSize mod FBytesPerRow) = 0) and (DataSize > 0) then
        RowCount := RowCount - 1;
      LIntPos := GetPosAtCursor(Col, Row);
      if LIntPos = DataSize then
        SelStart := DataSize - 1;
    end
    else
    begin
      if ((DataSize mod FBytesPerRow) = 0) and (DataSize > 0) then
        RowCount := RowCount + 1;
    end;
    FModifiedBytes.Size := 0;
    Invalidate;
  end;
end;

function TCustomBCHexEditor.GetModified: boolean;
begin
  Result := FModified and ((DataSize > 0) or FileExists(FileName));
end;

procedure TCustomBCHexEditor.SetSelection(DataPos, StartPos, EndPos:
  integer);
begin
  //CheckSelectUnit(StartPos, EndPos);
  FSelEnd := Max(-1, Min(EndPos, DataSize - 1));
  FSelPosition := Max(-1, Min(DataPos, DataSize - 1));
  FSelStart := Max(-1, Min(StartPos, DataSize - 1));
end;

procedure TCustomBCHexEditor.RecalcBytesPerRow;

  function CalcWidths(BPR: integer): integer;
  var
    LIntCnt: integer;
  begin
    Result := 0;
    for LIntCnt := 0 to BPR * 2 do
    begin
      if LIntCnt = Pred(BPR * 2) then
        Inc(Result,FCharWidth * 2)
      else
      begin
        Inc(Result, FCharWidth);
        if ((LIntCnt mod FBytesPerCol) = 1) then
          Inc(Result, FCharWidth);
        if (FBlockSize > 1) and ((LIntCnt mod (FBlockSize * 2)) = 1) then
          Inc(Result, FCharWidth);
      end;
    end;
    if FUnicodeCharacters then
      LIntCnt := Pred(BPR div 2)
    else
      LIntCnt := Pred(BPR);
    for LIntCnt := 0 to LIntCnt do
    begin
      if (FUsedRulerBytesPerUnit > 1) and ((LIntCnt mod FUsedRulerBytesPerUnit)
        = Pred(FUsedRulerBytesPerUnit)) and (not FUnicodeCharacters) then
        Inc(Result, (FCharWidth * 3 div 2) + 1)
      else
        Inc(Result, FCharWidth + 1);
      if not FUnicodeCharacters then
      begin
        if (FBlockSize > 1) and FSepCharBlocks and ((LIntCnt mod FBlockSize) =
          Pred(FBlockSize)) then
          Inc(Result, FCharWidth);
      end
      else
      begin
        if (FBlockSize > 1) and FSepCharBlocks and ((LIntCnt mod (FBlockSize div
          2)) = Pred(FBlockSize div 2)) then
          Inc(Result, FCharWidth);
      end;
    end;
  end;

var
  LIntWidth1, LIntWidth2, LIntBPR: integer;
begin
  FSetAutoBytesPerRow := True;
  try
    try
      LIntWidth1 := ClientWidth - ColWidths[0] - ColWidths[1] -8;
      LIntBPR := 2;
      repeat
        LIntWidth2 := CalcWidths(LIntBPR + 2);
        if LIntWidth2 > LIntWidth1 then
        begin
          BytesPerRow := LIntBPR;
          Break;
        end;
        Inc(LIntBPR,2);
        if LIntBPR >= 256 then
        begin
          BytesPerRow := LIntBPR;
          Break;
        end;
      until False;
    except
    end;
  finally
    FSetAutoBytesPerRow := False;
  end;
end;

procedure TCustomBCHexEditor.SetAutoBytesPerRow(const Value: Boolean);
begin
  if Value <> FAutoBytesPerRow then
  begin
    FAutoBytesPerRow := Value;
    if Value then
      RecalcBytesPerRow;
  end;
end;

procedure TCustomBCHexEditor.Resize;
begin
  if FAutoBytesPerRow then
    RecalcBytesPerRow;
  PostMessage(Handle, CM_INTUPDATECARET, 7, 7);
  inherited;
end;

procedure TCustomBCHexEditor.WrongKey;
begin
  if Assigned(FOnInvalidKey) then
    FOnInvalidKey(self);
end;

procedure TCustomBCHexEditor.TopLeftChanged;
begin
  CheckSetCaret;
  if Assigned(FOnTopLeftChanged) then
    FOnTopLeftChanged(self);
end;

function TCustomBCHexEditor.GetOffsetString(const Position: cardinal): string;
var
  I: Integer;
begin
  Result := '';
  if Assigned(FOnGetOffsetText) and (not FOffsetHandler) then
  begin
    FOffsetHandler := True;
    try
      FIsMaxOffset := False;
      FOnGetOffsetText(self, Position, Result)
    finally
      FOffsetHandler := False;
    end;
  end
  else
  begin
    with FOffsetFormat do
    begin
      if Format <> '' then
      begin
        if (MinWidth <> 0) or (Position <> 0) then
        begin
          Result := IntToRadix(Position div _BytesPerUnit, Radix, MinWidth);
          for I := 1 to Length(Result) - 1 do
            if not FHexLowercase then
              Result[I] := UpCase(Result[I])
            else if CharInSet(Result[I], ['a'..'z']) then
              Inc(Result[I], Ord('a')-Ord('A'));
        end;
        Result := Prefix + Result + Suffix;
      end;
    end;
  end;
end;

function TCustomBCHexEditor.GetAnyOffsetString(const Position: integer): string;
begin
  if FOffsetFormat.Format = '' then
    Result := IntToRadix(Position, 16)
  else
    Result := GetOffsetString(Position);
end;

function TCustomBCHexEditor.RowHeight: integer;
begin
  Result := DefaultRowHeight;
end;

function TCustomBCHexEditor.GetBookmark(Index: byte): TBCHBookmark;
begin
  if Index > 9 then
    raise EBCHexEditor.Create(ERR_INVALID_BOOKMARK);

  Result := FBookmarks[Index];
end;

procedure TCustomBCHexEditor.SetBookmark(Index: byte; const Value:
  TBCHBookmark);
begin
  SetBookmarkVals(Index, Value.mPosition, Value.mInCharField);
end;

procedure TCustomBCHexEditor.SetBookmarkVals(const Index: byte; const Position:
  integer; const InCharField: boolean);
begin
  if Index > 9 then
    raise EBCHexEditor.Create(ERR_INVALID_BOOKMARK);

  if (FBookmarks[Index].mPosition <> Position) or
    (FBookmarks[Index].mInCharField <> InCharField) then
  begin
    FBookmarks[Index].mPosition := Position;
    FBookmarks[Index].mInCharField := InCharField;
    Invalidate;
  end
  else
  begin
    FBookmarks[Index].mPosition := -1;
    FBookmarks[Index].mInCharField := InCharField;
    Invalidate;
  end;
  BookmarkChanged;
end;

{.$DEFINE TESTCOLOR}// check for unneeded drawings

type
  TestColor = TColor;

procedure TCustomBCHexEditor.Paint;
type
  TKindOfCell = (kocData, kocRuler, kocOffset, kocEmpty);
var
  DrawInfo: TGridDrawInfo;
  LIntCurCol, LIntCurRow, LIntOldFontSize: longint;
  LRctWhere: TRect;
  LBoolOddCol: boolean;
  LBoolChanged: boolean;
  LIntDataPos, LIntDataSize: integer;
  LWStrOutput: WideString;
  LWChrOutput: WideChar;
  LColTextColor, LColTextBackColor, LColBackColor: TColor;
  LIntPenWidthSave: integer;
  LrecSize: TSize;

  LBoolDraw: Boolean;

  LBoolFocused: boolean;
  LRect2: TRect;
  LIntLastCol: integer;

  // get the width of a wide text
  function GetTextWidthW: Integer;
  begin
    GetTextExtentPoint32W(Canvas.Handle, PWideChar(LWStrOutput),
      Length(LWStrOutput), LrecSize);
    Result := LRecSize.cx;
  end;

  // render an offset/ruler/fixed cell
  procedure _TextOut;
  begin
    with Canvas, LRctWhere do
    begin
      Brush.Color := TestColor(LColBackColor);
      Font.Color := LColTextColor;
      SetBKColor(Handle, ColorToRGB(TestColor(LColTextBackColor)));
      LRect2 := LRctWhere; //Rect(Left, Top, Left + FCharWidth, Bottom);
      LRect2.Right := Left + FCharWidth;
      //SetTextColor(Handle, ColorToRGB(LColTextColor));

      LBoolDraw := True;
      if Assigned(FOnDrawCell) then
      begin
        if LIntCurCol = 0 then
          FOnDrawCell(self, Canvas, LIntCurCol, LIntCurRow, LWStrOutput,
            LRctWhere, LBoolDraw)
        else
          FOnDrawCell(self, Canvas, LIntCurCol, LIntCurRow, LWStrOutput, LRect2,
            LBoolDraw)
      end;
      if LBoolDraw then
      begin

        FillRect(LRctWhere);
        if LIntCurCol = 0 then
          ExtTextOutW(Handle, Right - GetTextWidthW - 4, Top,
            ETO_CLIPPED or ETO_OPAQUE, @LRctWhere, PWideChar(LWStrOutput),
            Length(LWStrOutput), nil)
        else
          ExtTextOutW(Handle, Left, Top,
            ETO_CLIPPED or ETO_OPAQUE, @LRect2, PWideChar(LWStrOutput),
            Length(LWStrOutput), nil);

      end
      else
        LBoolDraw := True;

    end;
  end;

  // draw an offset cell
  procedure DrawOffsetCell;
  var
    LIntLoop: integer;
  begin
    if (LIntCurRow = Row) then
    begin
      LColBackColor := FColors.CurrentOffsetBackground;
      LColTextColor := FColors.CurrentOffset;
    end
    else
    begin
      LColBackColor := FColors.OffsetBackground;
      LColTextColor := Colors.Offset;
    end;
    LColTextBackColor := LColBackColor;

    (* text ausgeben *)
    LWStrOutput := GetOffsetString((LIntCurRow - GRID_FIXED) * FBytesPerRow);
    _TextOut;

    (* auf bookmark prüfen *)
    for LIntLoop := 0 to 9 do
      with FBookmarks[lIntLoop] do
        if (mPosition > -1) and ((mPosition div FBytesPerRow) = (LIntCurRow -
          GRID_FIXED)) then
          with LRctWhere do
            FBookmarkImageList.Draw(Canvas, Left + 3, ((Bottom - Top - 10) div 2)
              + Top, lIntLoop + (10 * integer(mInCharField)));
  end;

  // draw a ruler cell
  procedure DrawRulerCell;
  begin
    if LIntCurCol <> (GRID_FIXED + FBytesPerRowDup) then
    begin
      if LIntCurCol > (GRID_FIXED + FBytesPerRowDup) then
      begin
        LIntDataPos := (LIntCurCol - (GRID_FIXED + FBytesPerRowDup + 1));
        LWStrOutput := FRulerCharString[LIntDataPos + 1];
      end
      else
        LWStrOutput := FRulerString[LIntCurCol - GRID_FIXED + 1];
    end
    else
      LWStrOutput := '  ';
    LColBackColor := FColors.OffsetBackground;
    if Col = LIntCurCol then
    begin
      LColTextBackColor := FColors.CurrentOffsetBackground;
      LColTextColor := FColors.CurrentOffset;
    end
    else
    begin
      LColTextBackColor := FColors.OffsetBackground;
      LColTextColor := FColors.Offset;
    end;
    _TextOut;
  end;

  // draw a hex/char cell
  procedure DrawDataCell(const bIsCharCell, bIsCurrentField: boolean);
  begin
    LIntDataPos := GetPosAtCursor(LIntCurCol, LIntCurRow);
    FDrawDataPosition := LIntDataPos;
    FDrawDataPositionIsHex := not FPosInCharField;

    // farbe setzen
    if bIsCurrentField and (LIntCurCol < LIntLastCol) and
      (LIntCurCol <> FIntLastHexCol) then
      LColBackColor := FColors.FActiveFieldBackground
    else
      LColBackColor := FColors.FBackground;

    // nicht zeichnen, falls keine daten
    if (LIntDataPos < LIntDataSize) then
    begin
      if not bIsCharCell then
      begin // partie hexadecimale
        if ((LIntCurCol - GRID_FIXED) mod 2) = FSwapNibbles then
          LWChrOutput := WideChar(FHexChars[Data[LIntDataPos] shr 4])
        else
          LWChrOutput := WideChar(FHexChars[Data[LIntDataPos] and 15])
      end
      else
      begin
        if FUnicodeCharacters then
        begin
          LWChrOutput := #0;
          ReadBuffer(LWChrOutput, LIntDataPos, Min(2, LIntDataSize -
            LIntDataPos));
          if FUnicodeBigEndian then
            SwapWideChar(LWChrOutput);
          if (Pos(LWChrOutput, FMaskedChars) > 0)
            then
            LWChrOutput := WideChar(FReplaceUnprintableCharsBy);
        end
        else
          LWChrOutput := TranslateToAnsiChar(Data[LIntDataPos]);
      end;

      // testen ob byte geändert
      LBoolChanged := (HasChanged(LIntDataPos)) or ((FUnicodeCharacters and
        bIsCharCell) and HasChanged(LIntDataPos + 1));
      LBoolOddCol := (((LIntCurCol - GRID_FIXED) div FBytesPerCol) mod 2) = 0;

      if LBoolChanged then
      begin
        LColTextColor := FColors.FChangedText;
        LColTextBackColor := FColors.FChangedBackground;
      end
      else
      begin
        if bIsCurrentField then
          LColTextBackColor := FColors.FActiveFieldBackground
        else
          LColTextBackColor := FColors.FBackground;

        if not FPosInCharField then
        begin
          if LBoolOddCol then
            LColTextColor := Colors.FOddColumn
          else
            LColTextColor := Colors.FEvenColumn;
        end
        else
          LColTextColor := Font.Color;
      end;

      if (FSelPosition <> -1) and IsSelected(LIntDataPos) then
      begin

        FIsDrawDataSelected := True;

        if (not FHideSelection) or LBoolFocused then
        begin
          if (LIntCurCol < LIntLastCol) and (LIntCurCol <> FIntLastHexCol)
            and (LIntDataPos <> Max(FSelStart, FSelEnd)) then
            LColBackColor := Invert(LColBackColor);
          LColTextBackColor := Invert(LColTextBackColor);
          LColTextColor := Invert(LColTextColor);

          if FGraySelOnLostFocus and (not LBoolFocused) then
          begin
            LColTextBackColor := FadeToGray(LColTextBackColor);
            LColTextColor := FadeToGray(LColTextColor);
          end;
        end;
      end

      else
        FIsDrawDataSelected := False
;

      with Canvas, LRctWhere do
      begin
        Brush.Color := TestColor(LColBackColor);
        Font.Color := LColTextColor;
        SetBKColor(Handle, ColorToRGB(TestColor(LColTextBackColor)));
        LRect2 := LRctWhere; //Rect(Left, Top, Left + FCharWidth, Bottom);
        LRect2.Right := Left + FCharWidth;
        //SetTextColor(Handle, ColorToRGB(LColTextColor));

        LBoolDraw := True;
        if Assigned(FOnDrawCell) then
        begin
          LWStrOutput := LWChrOutput;
          FOnDrawCell(self, Canvas, LIntCurCol, LIntCurRow, LWStrOutput, LRect2,
            LBoolDraw);
          LWChrOutput := (LWStrOutput+#0)[1];
        end;
        if LBoolDraw then
        begin

          FillRect(LRctWhere);
          LIntOldFontSize := Canvas.Font.Size;
          if FUnicodeCharacters then
            while (Canvas.Font.Size > 1) and GetTextExtentPoint32W(Canvas.Handle, @LWChrOutput,
              1, LrecSize) and (LRecSize.cx > (LRect2.Right - LRect2.Left)) do
              Canvas.Font.Size := Canvas.Font.Size -1;
          ExtTextOutW(Handle, Left, Top,
            ETO_CLIPPED or ETO_OPAQUE, @LRect2, @LWChrOutput,
            1, nil);
          if FUnicodeCharacters then
            Canvas.Font.Size := LIntOldFontSize;

        end
        else
          LBoolDraw := True;

        if FShowDrag and (LIntCurCol = FDropCol) and (LIntCurRow = FDropRow) then
        begin
          LIntPenWidthSave := Pen.Width;
          try
            Pen.Width := 2;
            Pen.Color := LColTextColor;
            MoveTo(Left + 1, Top + 1);
            LineTo(Left + 1, Bottom - 1)
          finally
            Pen.Width := LIntPenWidthSave;
          end;
        end
      end;
    end;

    // focus frame auf der anderen seite
    if LBoolFocused then
    begin
      if not FPosInCharField then
      begin
        if (LIntCurRow = Row) then
        begin
          if not FUnicodeCharacters then
          begin
            if GetOtherFieldColCheck(Col) = (LIntCurCol - 1) then
              with LRctWhere do
                if FFocusFrame then
                  Canvas.DrawFocusRect(Rect(
                    CellRect(LIntCurCol-1, LIntCurRow).Left,
                    Top, Left + FCharWidth, Bottom - 1))
                else
                begin
                  Canvas.Pen.Color := FColors.CursorFrame;
                  Canvas.Brush.Style := bsClear;
                  Canvas.Rectangle(CellRect(LIntCurCol-1, LIntCurRow).Left, Top,
                    Left + FCharWidth, Bottom - 1);
                end;
          end
          else if GetOtherFieldColCheck(Col) = (LIntCurCol - 3) then
            with LRctWhere do
              if FFocusFrame then
                Canvas.DrawFocusRect(Rect(
                  CellRect(LIntCurCol-3, LIntCurRow).Left, Top,
                  Left + FCharWidth, Bottom - 1))
              else
              begin
                Canvas.Pen.Color := FColors.CursorFrame;
                Canvas.Brush.Style := bsClear;
                Canvas.Rectangle(CellRect(LIntCurCol-3, LIntCurRow).Left, Top,
                  Left + FCharWidth, Bottom - 1);
              end;
        end;
      end
      else
      begin
        if (LIntCurRow = Row) and (GetOtherFieldColCheck(Col) = LIntCurCol) then
        begin
          with LRctWhere do
            if FFocusFrame then
              Canvas.DrawFocusRect(Rect(Left, Top, Left + FCharWidth, Bottom -
                1))
            else
            begin
              Canvas.Pen.Color := FColors.CursorFrame;
              Canvas.Brush.Style := bsClear;
              Canvas.Rectangle(Left, Top, Left + FCharWidth, Bottom - 1);
            end;
        end;
      end;
    end
    else
    begin
      // possibly draw a mark at the current position when not focused
      if FShowPositionIfNotFocused and (LIntCurRow = Row) and (Col = LIntCurCol)
        then
      begin
        with LRctWhere do
          if FFocusFrame then
            Canvas.DrawFocusRect(Rect(Left, Top, Left + FCharWidth, Bottom - 1))
          else
          begin
            Canvas.Pen.Color := FColors.NonFocusCursorFrame;
            Canvas.Brush.Style := bsClear;
            Canvas.Rectangle(Left, Top, Left + FCharWidth, Bottom - 1);
          end;
      end;
    end;

    if FDrawGridLines and (LIntCurCol = LIntLastCol) then
      with Canvas, LRctWhere do
      begin
        Pen.Color := FColors.FGrid;
        MoveTo(Right - 1, Top);
        LineTo(Right - 1, Bottom - 1);
      end;

  end;

  // draw
  procedure DrawCells(ACol, ARow: longint; StartX, StartY, StopX, StopY:
    integer;
    Kind: TKindOfCell);
  begin
    LIntCurRow := ARow;
    LRctWhere.Top := StartY;
    while (LRctWhere.Top < StopY) and (LIntCurRow < RowCount) do
    begin
      LIntCurCol := ACol;
      LRctWhere.Left := StartX;
      LRctWhere.Bottom := LRctWhere.Top + RowHeights[LIntCurRow];
      while (LRctWhere.Left < StopX) and (LIntCurCol <= LIntLastCol) do
      begin
        LRctWhere.Right := LRctWhere.Left + ColWidths[LIntCurCol];
        if (LRctWhere.Right > LRctWhere.Left) then
        begin
          case Kind of
            kocData:
              begin
                if LIntCurCol < (GRID_FIXED + FBytesPerRowDup) then
                  DrawDataCell(False, not FLastPosInCharField)
                else if LIntCurCol > (GRID_FIXED + FBytesPerRowDup) then
                  DrawDataCell(True, FLastPosInCharField)
                else if FDrawGridLines then
                  with Canvas do
                  begin
                    Pen.Color := FColors.FGrid;
                    MoveTo(LRctWhere.Left, LRctWhere.Top);
                    LineTo(LRctWhere.Left, LRctWhere.Bottom - 1);
                  end;

                if FDrawGridLines then
                  with Canvas do
                  begin
                    Pen.Color := FColors.FGrid;
                    MoveTo(LRctWhere.Left, LRctWhere.Bottom - 1);
                    LineTo(LRctWhere.Right, LRctWhere.Bottom - 1);
                  end;
              end;
            kocEmpty:
              begin
                FDrawDataPosition := -1;
                LColTextBackColor := FColors.OffsetBackground;
                LColTextColor := FColors.Offset;
                LWStrOutput := '';
                _TextOut;
              end;
            kocRuler:
              begin
                FDrawDataPosition := -1;
                DrawRulerCell;
              end;
            kocOffset:
              begin
                FDrawDataPosition := -1;
                if LIntCurCol = 1 then
                begin
                  if FDrawGridLines then
                    with Canvas do
                    begin
                      Pen.Color := FColors.FGrid;
                      MoveTo(LRctWhere.Left, LRctWhere.Bottom - 1);
                      LineTo(LRctWhere.Right, LRctWhere.Bottom - 1);
                    end;
                end
                else
                  DrawOffsetCell;
              end;
          end;
        end;
        LRctWhere.Left := LRctWhere.Right;
        Inc(LIntCurCol);
      end;
      LRctWhere.Top := LRctWhere.Bottom;
      Inc(LIntCurRow);
    end;
  end;
var
  LIntTop: integer;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  if UseRightToLeftAlignment then
    ChangeGridOrientation(True);
  CalcDrawInfo(DrawInfo);
  LBoolFocused := Focused;
  LIntDataSize := DataSize;
  LIntLastCol := GetLastCharCol;
  with DrawInfo do
  begin
    if FShowRuler then
    begin
      // oben links, fixed
      DrawCells(0, 0, 0, 0, Horz.FixedBoundary, Vert.FixedBoundary, kocEmpty);
      // oben, fixed
      DrawCells(LeftCol, 0, Horz.FixedBoundary, 0, Horz.GridBoundary,
        Vert.FixedBoundary, kocRuler);
    end;
    // links, fixed
    DrawCells(0, TopRow, 0, Vert.FixedBoundary, Horz.FixedBoundary,
      Vert.GridBoundary, kocOffset);
    // daten
    DrawCells(LeftCol, TopRow, Horz.FixedBoundary, Vert.FixedBoundary,
      Horz.GridBoundary, Vert.GridBoundary, kocData);

    // paint unoccupied space on the right
    if Horz.GridBoundary < Horz.GridExtent then
    begin
      Canvas.Brush.Color := TestColor(Color);
      Canvas.FillRect(Rect(Horz.GridBoundary, 0, Horz.GridExtent,
        Vert.GridBoundary));

      // fixed (ruler)
      Canvas.Brush.Color := TestColor(FColors.OffsetBackground);
      Canvas.FillRect(Rect(Horz.GridBoundary, 0, Horz.GridExtent, RowHeights[0]
        + RowHeights[1]));
    end;

    // paint unoccupied space on bottom
    if Vert.GridBoundary < Vert.GridExtent then
    begin
      // hex + chars
      Canvas.Brush.Color := TestColor(Color);
      Canvas.FillRect(Rect(ColWidths[0] + 1, Vert.GridBoundary, Horz.GridExtent,
        Vert.GridExtent));

      // fixed (position gutter)
      Canvas.Brush.Color := TestColor(FColors.OffsetBackground);
      Canvas.FillRect(Rect(0, Vert.GridBoundary, ColWidths[0],
        Vert.GridExtent));
    end;

    LIntTop := RowHeights[0] + RowHeights[1];

    // draw bevel on the right of the offset gutter
    if (ColWidths[0] <> 0) then
    begin
      if FDrawGutter3D then
      begin
        Canvas.MoveTo(ColWidths[0], LIntTop);
        Canvas.Pen.Color := TestColor(clBtnShadow);
        Canvas.LineTo(ColWidths[0], Vert.GridExtent);
        Canvas.MoveTo(ColWidths[0] - 1, LIntTop);
        Canvas.Pen.Color := TestColor(clBtnHighlight);
        Canvas.LineTo(ColWidths[0] - 1, Vert.GridExtent);
      end
      else if FDrawGridLines then
      begin
        Canvas.MoveTo(ColWidths[0] - 1, LIntTop);
        Canvas.Pen.Color := TestColor(FColors.Grid);
        Canvas.LineTo(ColWidths[0] - 1, Vert.GridExtent);
      end;
    end;

    if (FShowRuler) then
    begin
      if FDrawGutter3D then
      begin
        Canvas.MoveTo(ColWidths[0] - 1, LIntTop - 1);
        Canvas.Pen.Color := TestColor(clBtnShadow);
        Canvas.LineTo(Horz.GridExtent, LIntTop - 1);
        Canvas.MoveTo(ColWidths[0] - 1, LIntTop - 2);
        Canvas.Pen.Color := TestColor(clBtnHighlight);
        Canvas.LineTo(Horz.GridExtent, LIntTop - 2);
      end
      else if FDrawGridLines then
      begin
        Canvas.MoveTo(ColWidths[0] - 1, LIntTop - 1);
        Canvas.Pen.Color := TestColor(FColors.Grid);
        Canvas.LineTo(Horz.GridExtent, LIntTop - 1);
      end;
    end;
  end;

  if UseRightToLeftAlignment then
    ChangeGridOrientation(False);

end;

procedure TCustomBCHexEditor.SetSelectionAsHex(const s: string);
var
  LStrData: AnsiString;
  LIntAmount: integer;
begin
  if s <> '' then
  begin
    SetLength(LStrData, Length(s));
    ConvertHexToBin(@s[1], @LStrData[1], Length(s), SwapNibbles, LIntAmount);
    SetLength(LStrData, LIntAmount);
    SetSelectionAsText(LStrData);
  end;
end;

function TCustomBCHexEditor.GetSelectionAsText: AnsiString;
begin
  if (DataSize < 1) or (SelCount < 1) then
    Result := ''
  else
  begin
    SetLength(Result, SelCount);
    FDataStorage.ReadBufferAt(Result[1], Min(SelStart, SelEnd), SelCount);
  end;
end;

procedure TCustomBCHexEditor.SetSelectionAsText(const s: AnsiString);
begin
  if s <> '' then
    ReplaceSelection(@s[1], Length(s));
end;

procedure TCustomBCHexEditor.SetDrawGridLines(const Value: boolean);
begin
  if Value <> FDrawGridLines then
  begin
    FDrawGridLines := Value;
    Invalidate;
  end;
end;

function TCustomBCHexEditor.UndoBeginUpdate: integer;
begin
  Result := FUndoStorage.BeginUpdate;
end;

function TCustomBCHexEditor.UndoEndUpdate: integer;
begin
  Result := FUndoStorage.EndUpdate;
end;

function TCustomBCHexEditor.Undo: boolean;
begin
  Result := FUndoStorage.Undo;
end;

function TCustomBCHexEditor.Redo: boolean;
begin
  Result := FUndoStorage.Redo;
end;

procedure TCustomBCHexEditor.SetGutterWidth(const Value: integer);
begin
  if FGutterWidth <> Value then
  begin
    FGutterWidth := Value;
    SetOffsetDisplayWidth;
    Invalidate;
  end;
end;

procedure TCustomBCHexEditor.BookmarkBitmapChanged(Sender: TObject);
begin
  // invalidieren
  FBookmarkImageList.Clear;
  FBookmarkImageList.AddMasked(FBookmarkBitmap, FBookmarkBitmap.Canvas.Pixels[0,
    0]);
  if HandleAllocated then
    Invalidate;
end;

procedure TCustomBCHexEditor.SetBookmarkBitmap(const Value: TBitmap);
begin
  if Value = nil then
    FBookmarkBitmap.LoadFromResourceName(HINSTANCE, 'BOOKMARKICONS')
  else
  begin
    if (Value.Width <> 200) or (Value.Height <> 10) then
      raise EBCHexEditor.Create(ERR_INVALID_BOOKMARKBMP);
    FBookmarkBitmap.Assign(Value);
  end;
  FHasCustomBMP := Value <> nil;
end;

procedure TCustomBCHexEditor.SelectAll;
var
  LgrcPosition: TGridCoord;
begin
  if DataSize > 0 then
  begin
    // position auf ende stzen
    if (not InsertMode) then
      LgrcPosition := GetCursorAtPos(DataSize - 1, InCharField)
    else
      LgrcPosition := GetCursorAtPos(DataSize, InCharField);
    MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);

    // alles wählen
    NewSelection(0, Pred(DataSize));
  end;
end;

procedure TCustomBCHexEditor.FreeStorage(FreeUndo: boolean = False);
begin
  if not FreeUndo then
    FDataStorage.Size := 0
  else
    FUndoStorage.Size := 0;
end;

procedure TCustomBCHexEditor.OldCursor;
begin
  if Length(FCursorList) > 0 then
  begin
    Cursor := FCursorList[Pred(Length(FCursorList))];
    SetLength(FCursorList, PRed(Length(FCursorList)));
  end;
end;

procedure TCustomBCHexEditor.WaitCursor;
begin
  SetLength(FCursorList, Succ(Length(FCursorList)));
  FCursorList[Pred(Length(FCursorList))] := Cursor;
  Cursor := crHourGlass;
end;

function TCustomBCHexEditor.HasCustomBookmarkBitmap: boolean;
begin
  Result := FHasCustomBMP;
end;

procedure TCustomBCHexEditor.PrepareOverwriteDiskFile;
begin
  if FIsFileReadonly then
    raise EFOpenError.CreateFmt(ERR_FILE_READONLY, [FileName]);
end;

procedure TCustomBCHexEditor.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
  SelectionChanged;
end;

procedure TCustomBCHexEditor.SetDrawGutter3D(const Value: boolean);
begin
  if FDrawGutter3D <> Value then
  begin
    FDrawGutter3D := Value;
    Repaint;
  end;
end;

procedure TCustomBCHexEditor.SetShowRuler(const Value: boolean);
begin
  if (FShowRuler <> Value) or (csLoading in ComponentState) then
  begin
    FShowRuler := Value;
    AdjustMetrics;
  end;
end;

function TCustomBCHexEditor.DisplayEnd: integer;
begin
  if DataSize < 1 then
    Result := -1
  else
    Result := Min((DataSize - 1), (DisplayStart - 1) + (VisibleRowCount *
      BytesPerRow));
end;

function TCustomBCHexEditor.DisplayStart: integer;
begin
  if DataSize < 1 then
    Result := -1
  else
    Result := GetPosAtCursor(GRID_FIXED, TopRow);
end;

procedure TCustomBCHexEditor.SetBytesPerUnit(const Value: integer);
begin
  if FBytesPerUnit <> Value then
  begin
    if FUnicodeCharacters and (Value <> 2) then
      raise EBCHexEditor.Create(ERR_INVALID_BPU_U);
    if not (Value in [1, 2, 4, 8]) then
      raise EBCHexEditor.CreateFmt(ERR_INVALID_BPU, [Value]);
    FBytesPerUnit := Value;
    if FRulerBytesPerUnit = -1 then
      FUsedRulerBytesPerUnit := Value;
    with FOffsetFormat do
      if offBytesPerUnit in Flags then
        _BytesPerUnit := FUsedRulerBytesPerUnit;
    AdjustMetrics;
    SetRulerString;
    if (SelCount mod FBytesPerUnit) <> 0 then
      ResetSelection(False);
    Invalidate;
  end;
end;

procedure TCustomBCHexEditor.SetRulerString;
var
  intLoop, intLen: Integer;
  sLoop: string;
begin
  FRulerString := '';
  intLen := 2 * FUsedRulerBytesPerUnit;
  for intLoop := 0 to Pred(FBytesPerRow div FUsedRulerBytesPerUnit) do
  begin
    sLoop := IntToRadix(intLoop, FRulerNumberBase, intLen);
    if Length(sLoop) > intLen then
      Delete(sLoop, 1, Length(sLoop) - intLen);
    FRulerString := FRulerString + sLoop;
  end;
  if FHexLowerCase then
    FRulerString := LowerCase(FRulerString)
  else
    FRulerString := UpperCase(FRulerString);
  FRulerCharString := '';
  if FUnicodeCharacters then
    intLen := FUsedRulerBytesPerUnit div 2
  else
    intLen := FUsedRulerBytesPerUnit;
  for intLoop := 0 to Pred(FBytesPerRow div FUsedRulerBytesPerUnit) do
  begin
    sLoop := IntToRadix(intLoop, FRulerNumberBase);
    if Length(sLoop) > intLen then
      Delete(sLoop, 1, Length(sLoop) - intLen)
    else
      while Length(sLoop) < intLen do
        sLoop := ' ' + sLoop;
    FRulerCharString := FRulerCharString + sLoop;
  end;
  if FHexLowerCase then
    FRulerCharString := LowerCase(FRulerCharString)
  else
    FRulerCharString := UpperCase(FRulerCharString);
end;

procedure TCustomBCHexEditor.CheckSelectUnit(var AStart, AEnd: Integer);
begin
  // assure that the selection covers a whole unit
  if AStart <= AEnd then
  begin
    CheckUnit(AStart);
    CheckUnit(AEnd);
    Inc(AEnd, FBytesPerUnit - 1);
    if (AEnd >= DataSize) then
      AEnd := Pred(DataSize);
  end
  else
  begin
    CheckUnit(AEnd);
    CheckUnit(AStart);
    Inc(AStart, FBytesPerUnit - 1);
    if (AStart >= DataSize) then
      AStart := Pred(DataSize);
  end;
end;

// make sure the value is a multiple of FBytesPerUnit

procedure TCustomBCHexEditor.CheckUnit(var AValue: Integer);
begin
  AValue := AValue div FBytesPerUnit * FBytesPerUnit;
end;

procedure TCustomBCHexEditor.SelectionChanged;
begin
  if not (csLoading in ComponentState) then
  begin
    Inc(FSelectionChangedCount);
    if FSelectionChangedCount = 1 then
      PostMessage(Handle, CM_SELECTIONCHANGED, 0, 0);
  end;
end;

procedure TCustomBCHexEditor.SyncView(Source: TCustomBCHexEditor;
  SyncOffset: integer = 0);
var
  curPos, SelS, SelE: integer;
  coord: TGridCoord;
begin
  if FIsViewSyncing or Source.FIsViewSyncing then
    Exit;
  FIsViewSyncing := True;
  try
    if (Source.BytesPerRow = BytesPerRow) and (Source.BytesPerColumn =
      BytesPerColumn) and (Source.BytesPerUnit = BytesPerUnit) and
      (Source.GetCursorPos < DataSize) and (SyncOffset = 0) then
    begin
      TopRow := Source.TopRow;
      LeftCol := Source.LeftCol;
      MoveColRow(Source.Col, Source.Row, True, False);
    end
    else
    begin
      // get the current view
      curPos := Source.GetCursorPos;
      coord := Source.GetCursorAtPos(curPos, Source.InCharField);
      with Source.CellRect(coord.X, coord.Y) do
        if Left + Bottom = 0 then
        begin
          curPos := Source.GetPositionAtCursor(Source.LeftCol, Source.TopRow) +
            SyncOffset;
          if curPos >= DataSize then
            curPos := Pred(DataSize);
          if curPos < 0 then
            curPos := 0;
          coord := GetCursorAtPos(curPos, Source.InCharField);
          LeftCol := Source.LeftCol;
          TopRow := coord.Y;
          Col := coord.X;
        end
        else
        begin
          // use this value if visible, left/top otherwise (when wheeling or scrolling)
          curPos := curPos + SyncOffset;
          if curPos >= DataSize then
            curPos := Pred(DataSize);
          if curPos < 0 then
            curPos := 0;
          coord := GetCursorAtPos(curPos, Source.InCharField);
          LeftCol := Source.LeftCol;
          MoveColRow(coord.X, coord.Y, True, True);
        end;
    end;
    if (Source.SelCount = 0) then
    begin
      if (SelCount <> 0) then
        ResetSelection(True)
    end
    else
    begin
      SelS := Source.FSelStart + SyncOffset;
      SelE := Source.FSelEnd + SyncOffset;
      if SelE >= DataSize then
        SelE := DataSize - 1;
      if SelS >= DataSize then
        SelS := DataSize - 1;
      if SelE < 0 then
        SelE := 0;
      if SelS < 0 then
        SelS := 0;
      NewSelection(SelS, SelE);
    end;
  finally
    FIsViewSyncing := False;
  end;
end;

procedure TCustomBCHexEditor.CMSelectionChanged(var Msg: TMessage);
begin
  if (FSelectionChangedCount <> 0) and Assigned(FOnSelectionChanged) then
  try
    FOnSelectionChanged(self);
  finally
    FSelectionChangedCount := 0;
  end;
end;

procedure TCustomBCHexEditor.SetRulerBytesPerUnit(const Value: integer);
begin
  if FRulerBytesPerUnit <> Value then
  begin
    if (not (Value in [1, 2, 4, 8])) and (Value <> -1) then
      raise EBCHexEditor.CreateFmt(ERR_INVALID_RBPU, [Value]);
    FRulerBytesPerUnit := Value;
    if Value = -1 then
      FUsedRulerBytesPerUnit := FBytesPerUnit
    else
      FUsedRulerBytesPerUnit := Value;
    with FOffsetFormat do
      if offBytesPerUnit in Flags then
        _BytesPerUnit := FUsedRulerBytesPerUnit;
    AdjustMetrics;
    SetRulerString;
    Invalidate;
  end;
end;

procedure TCustomBCHexEditor.SetShowPositionIfNotFocused(const Value: Boolean);
begin
  if FShowPositionIfNotFocused <> Value then
  begin
    FShowPositionIfNotFocused := Value;
    Invalidate;
  end;
end;

function TCustomBCHexEditor.GetDataAt(Index: integer): Byte;
begin
{$R-}
  Result := GetFastPointer(Index,1)^;
end;

procedure TCustomBCHexEditor.SetDataAt(Index: integer; const Value: Byte);
begin
  GetFastPointer(Index, 1)^ := Value;
  Changed;
end;

procedure TCustomBCHexEditor.ReadBuffer(var Buffer; const Index, Count:
  Integer);
begin
  Move(GetFastPointer(Index, Count)^, Buffer, Count);
end;

procedure TCustomBCHexEditor.WriteBuffer(const Buffer; const Index, Count:
  Integer);
begin
  Move(Buffer, GetFastPointer(Index,Count)^, Count);
  Changed;
end;

// fire OnBookmarkChanged

procedure TCustomBCHexEditor.BookmarkChanged;
begin
  if Assigned(FOnBookmarkChanged) then
    FOnBookmarkChanged(self);
end;

procedure TCustomBCHexEditor.DoSetCellWidth(const Index: integer;
  Value: integer);
begin
  ColWidths[Index] := Value;
  CheckSetCaret;
end;

// legacy, do not use

function TCustomBCHexEditor.GetMemory(const Index: Integer): char;
begin
  Result := Char(Data[Index])
end;

// legacy, do not use

procedure TCustomBCHexEditor.SetMemory(const Index: integer; const Value: char);
begin
  Data[Index] := Ord(Value);
end;

procedure TCustomBCHexEditor.SetUnicodeCharacters(const Value: Boolean);
begin
  if FUnicodeCharacters <> Value then
  begin
    if Value then
    begin
      if (BytesPerRow mod 2) <> 0 then
        raise EBCHexEditor.Create(ERR_INVALID_BYTESPERLINE);
      if (BytesPerColumn mod 2) <> 0 then
        raise EBCHexEditor.Create(ERR_INVALID_BYTESPERCOL);
      if (DataSize mod 2) <> 0 then
        raise EBCHexEditor.Create(ERR_ODD_FILESIZE_UNICODE);
      FTranslation := tkAsIs;
    end;
    FUnicodeCharacters := Value;
    ColCount := CalcColCount;
    if Value then
      BytesPerUnit := 2
    else
      BytesPerUnit := 1;

    CalcSizes;
    SetRulerString;
    Invalidate;
  end;
end;

procedure TCustomBCHexEditor.SetUnicodeBigEndian(const Value: Boolean);
begin
  if FUnicodeBigEndian <> Value then
  begin
    FUnicodeBigEndian := Value;
    if FUnicodeCharacters then
      Invalidate;
  end;
end;

function TCustomBCHexEditor.GetPositionAtCursor(const ACol,
  ARow: integer): integer;
var
  LBoolInCharField: Boolean;
begin
  LBoolInCharField := FPosInCharField;
  try
    Result := GetPosAtCursor(ACol, ARow);
  finally
    FPosInCharField := (LBoolInCharField);
  end;
end;

function TCustomBCHexEditor.GetIsCharFieldCol(
  const ACol: integer): Boolean;
begin
  Result := ACol > (GRID_FIXED + FBytesPerRowDup);
end;

function TCustomBCHexEditor.IsColorsStored(): boolean;
begin
  Result := FColors.IsStored();
end;

function TCustomBCHexEditor.IsFileSizeFixed: boolean;
begin
  if FFixedFileSizeOverride then
    Result := False
  else
    Result := FFixedFileSize;
end;

function TCustomBCHexEditor.IsFontStored(): boolean;
begin
  Result := (Font.Name <> DefaultFontName)
    or (Font.Size <> DefaultFontSize);
end;

function TCustomBCHexEditor.IsInsertModePossible: boolean;
begin
  Result := (not IsFileSizeFixed) and FAllowInsertMode and (not FReadOnlyView)
end;

function TCustomBCHexEditor.Replace(aBuffer: PChar; aPosition, aOldCount,
  aNewCount: integer;
  const UndoDesc: string = ''; const MoveCursor: Boolean = False): integer;
var
  LBoolInCharField: boolean;
  LIntSize: integer;
begin
  //FDataStorage.CheckBounds((Abs(aPosition) + Abs(aOldCount)) - 1);
  LIntSize := DataSize;
  // auswahl berechnen
  LBoolInCharField := GetInCharField;
  if LIntSize - APosition < aOldCount then
  begin
    if aNewCount = aOldCount then
      aNewCount := LIntSize - APosition;
    aOldCount := LIntSize - APosition;
  end;
  if IsFileSizeFixed then
  begin
    if aOldCount < aNewCount then
      aNewCount := aOldCount
    else
      aOldCount := aNewCount;
  end;

  CreateUndo(ufKindReplace, APosition, aNewCount, aOldCount, UndoDesc);

  if (not MoveCursor) and (FUndoStorage.FUpdateCount = 0) then
    FUndoStorage.AddSelection(APosition, aOldCount);

  if aOldCount = aNewCount then
    WriteBuffer(aBuffer^, APosition, aOldCount)
  else
    if aOldCount > aNewCount then
    begin
      InternalDelete(APosition, APosition + (aOldCount - aNewCount), Col, Row);
      WriteBuffer(aBuffer^, APosition, aNewCount)
    end
    else
    begin
      InternalInsertBuffer(nil, aNewCount-aOldCount, APosition);
      WriteBuffer(aBuffer^, APosition, aNewCount)
    end;
  Result := aNewCount;
  if FModifiedBytes.Size >= APosition then
    FModifiedBytes.Size := Max(0, APosition);

  if MoveCursor then
  begin
    with GetCursorAtPos(APosition, LBoolInCharField) do
      MoveColRow(x, y, True, True);
  end;
  Invalidate;
  Changed;
end;

function TCustomBCHexEditor.GotoBookmark(const Index: integer): boolean;
var
  LIntRow: integer;
  LgrcPosition: TGridCoord;
begin
  Result := False;
  if FBookmarks[Index].mPosition > -1 then
  begin
    ResetSelection(True);
    LIntRow := FBookmarks[Index].mPosition;
    if (LIntRow < DataSize) or ((LIntRow = DataSize) and InsertMode) then
    begin
      LgrcPosition := GetCursorAtPos(LIntRow, FBookmarks[Index].mInCharField);
      MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True);
      Result := True;
    end
    else
      SetBookmarkVals(Index, -1, False);
  end;
end;

procedure TCustomBCHexEditor.UpdateGetOffsetText;
begin
  SetOffsetDisplayWidth;
  Invalidate;
  CheckSetCaret;
end;

function TCustomBCHexEditor.GetFastPointer(const Index, Count: integer): PByte;
begin
  Result := FDataStorage.GetAddress(Index, Count);
end;

procedure TCustomBCHexEditor.SeekToEOF;
var
  LgrcPosition: TGridCoord;
begin
  InCharField;
  if (not InsertMode) then
    LgrcPosition := GetCursorAtPos(DataSize - 1, FPosInCharField)
  else
    LgrcPosition := GetCursorAtPos(DataSize, FPosInCharField);
  MoveColRow(LgrcPosition.x, LgrcPosition.y, True, True)
end;

function TCustomBCHexEditor.CanCreateUndo(const aKind: TBCHUndoFlag; const
  aCount,
  aReplCount: integer): Boolean;
begin
  Result := False;
  if DataSize > 0 then
    Result := True;

  if not Result then
    if aKind in [ufKindInsertBuffer, ufKindAppendBuffer, ufKindAllData] then
      Result := True;

  // check for NoSizeChange
  if IsFileSizeFixed and Result then
    if (aKind in [ufKindByteRemoved, ufKindInsertBuffer, ufKindAppendBuffer,
      ufKindNibbleInsert,
        ufKindNibbleDelete]) or
      ((aKind = ufKindReplace) and (aCount <> aReplCount)) then
      Result := False;

  if (not Result) and ((aKind = ufKindCombined) and (FUndoStorage.Count >=
    aCount)) then
    Result := True;

end;

procedure TCustomBCHexEditor.SetDataSize(const Value: integer);
var
  iPos: Integer;
  iSize: integer;
begin
  iSize := DataSize;
  if Value <> iSize then
  begin
    iPos := GetCursorPos;

    // new in 12-16-2003: don't allow change of datasize if nosizechange
    // and (new datasize <> 0 and old datasize <> 0)
    if (Value <> 0) and (iSize <> 0) and IsFileSizeFixed then
      raise EBCHexEditor.Create(ERR_FIXED_FILESIZE);

    FFixedFileSizeOverride := True;
    try
      // new in 12-16-2003: generate undo
      if Value < iSize then
        // create a 'bytes deleted' undo
        CreateUndo(ufKindByteRemoved, Value, DataSize - Value, 0)
      else
        // create a 'append buffer' undo
        CreateUndo(ufKindAppendBuffer, DataSize, Value - DataSize, 0);
      FDataStorage.Size := Value;
      if Value > iSize then
        // fill the new data block
        FillChar(GetFastPointer(iSize, Value-iSize)^, Value - iSize, FSetDataSizeFillByte);
      FModified := True;
      CalcSizes;
      if iPos > DataSize then
      begin
        ResetSelection(True);
        if (DataSize = 0) and (not InsertMode) then
        begin
          with GetCursorAtPos(0, InCharField) do
            MoveColRow(X, Y, True, True);
        end
        else
          SeekToEOF;
      end;
    finally
      FFixedFileSizeOverride := False;
    end;
  end;
end;

procedure TCustomBCHexEditor.SetBlockSize(const Value: Integer);
begin
  if FBlockSize <> Value then
  begin
    FBlockSize := Value;
    AdjustMetrics;
  end;
end;

procedure TCustomBCHexEditor.SetSepCharBlocks(const Value: boolean);
begin
  if FSepCharBlocks <> Value then
  begin
    FSepCharBlocks := Value;
    if Value and (FBlockSize > 1) then
      AdjustMetrics;
  end;
end;

procedure TCustomBCHexEditor.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('MaskChar', ReadMaskChar, nil, False);
  Filer.DefineProperty('MaskChar_AsInteger', ReadMaskChar_I, WriteMaskChar_I,
    FReplaceUnprintableCharsBy <> '.');
end;

procedure TCustomBCHexEditor.ReadMaskChar(Reader: TReader);
var
  s: string;
begin
  s := Reader.ReadString;
  if Length(s) <> 1 then
    FReplaceUnprintableCharsBy := '.'
  else
  try
    FReplaceUnprintableCharsBy := s[1];
  except
    FReplaceUnprintableCharsBy := '.';
  end;
end;

procedure TCustomBCHexEditor.ReadMaskChar_I(Reader: TReader);
begin
  try
    FReplaceUnprintableCharsBy := WideChar(Byte(Reader.ReadInteger));
  except
    FReplaceUnprintableCharsBy := '.';
  end;
end;

procedure TCustomBCHexEditor.WriteMaskChar_I(Writer: TWriter);
begin
  Writer.WriteInteger(Byte(FReplaceUnprintableCharsBy));
end;

function TCustomBCHexEditor.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): boolean;
begin
  if Shift <> [] then
    Result := inherited DoMouseWheelDown(Shift, MousePos)
  else
  begin
    // scroll down one page
    TopRow := Min(Max(GRID_FIXED, RowCount - VisibleRowCount),
      TopRow + VisibleRowCount - 1);
    CheckSetCaret;
    Result := True;
  end;
end;

function TCustomBCHexEditor.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): boolean;
begin
  if Shift <> [] then
    Result := inherited DoMouseWheelUp(Shift, MousePos)
  else
  begin
    // scroll up one page
    TopRow := Max(GRID_FIXED, TopRow - VisibleRowCount + 1);
    CheckSetCaret;
    Result := True;
  end;
end;

procedure TCustomBCHexEditor.CheckSetCaret;
begin
  with CellRect(Col, Row) do
  begin
    if Left + Bottom = 0 then
      IntSetCaretPos(-50, -50, -1)
    else
      IntSetCaretPos(Left, Top, Col);
  end;
end;

function TCustomBCHexEditor.CanFocus: Boolean;
var
  Form: TCustomForm;
begin
  Result := inherited CanFocus;
  if Result and not (csDesigning in ComponentState) then
  begin
    Form := GetParentForm(Self);
    Result := (not Assigned(Form)) or (Form.Enabled and Form.Visible);
  end;
end;

procedure TCustomBCHexEditor.SetRulerNumberBase(const Value: byte);
begin
  if FRulerNumberBase <> Value then
  begin
    // force number that can be represented using '0'-'9','A'-'F'
    if not (Value in [2..16]) then
      FRulerNumberBase := 16
    else
      FRulerNumberBase := Value;
    SetRulerString;
    if FShowRuler then
      Invalidate;
  end;
end;

procedure TCustomBCHexEditor.SetMaskedChars(const Value: string);
begin
  if FMaskedChars <> Value then
  begin
    FMaskedChars := Value;
    Invalidate;
  end;
end;

procedure TCustomBCHexEditor.CenterCursorPosition;
var
  iPos: integer;
begin
  iPos := GetCursorPos;
  iPos := (iPos div FBytesPerRow) + GRID_FIXED;
  TopRow := Max(GRID_FIXED, Min(iPos - (VisibleRowCount div 2), RowCount-VisibleRowCount));
end;

{ TBCHColors }

procedure TBCHColors.Assign(Source: TPersistent);
begin
  if Source is TBCHColors then
    with Source as TBCHColors do
    begin
      self.Background := Background;
      self.ChangedText := ChangedText;
      self.CursorFrame := CursorFrame;
      self.NonFocusCursorFrame := NonFocusCursorFrame;
      self.Offset := Offset;
      self.OddColumn := OddColumn;
      self.EvenColumn := EvenColumn;
      self.ChangedBackground := ChangedBackground;
      self.CurrentOffsetBackground := CurrentOffsetBackground;
      self.CurrentOffset := CurrentOffset;
      self.OffsetBackground := OffsetBackground;
      self.ActiveFieldBackground := ActiveFieldBackground;
      self.Grid := Grid;
    end;
end;

constructor TBCHColors.Create(Parent: TControl);
begin
  inherited Create;

  FActiveFieldBackground := DefaultActiveFieldBackground;
  FBackground := DefaultBackground;
  FChangedBackground := DefaultChangedBackground;
  FChangedText := DefaultChangedText;
  FCurrentOffset := DefaultCurrentOffset;
  FCurrentOffsetBackground := DefaultCurrentOffsetBackground;
  FCursorFrame := DefaultCursorFrame;
  FEvenColumn := DefaultEvenColumn;
  FGrid := DefaultGrid;
  FNonFocusCursorFrame := DefaultNonFocusCursorFrame;
  FOddColumn := DefaultOddColumn;
  FOffset := DefaultOffset;
  FOffsetBackground := DefaultOffsetBackground;
  FParent := Parent;

end;

function TBCHColors.IsStored(): boolean;
begin
  Result := (FActiveFieldBackground <> DefaultActiveFieldBackground)
    or (FBackground <> DefaultBackground)
    or (FChangedBackground <> DefaultChangedBackground)
    or (FChangedText <> DefaultChangedText)
    or (FCurrentOffset <> DefaultCurrentOffset)
    or (FCurrentOffsetBackground <> DefaultCurrentOffsetBackground)
    or (FCursorFrame <> DefaultCursorFrame)
    or (FEvenColumn <> DefaultEvenColumn)
    or (FGrid <> DefaultGrid)
    or (FNonFocusCursorFrame <> DefaultNonFocusCursorFrame)
    or (FOddColumn <> DefaultOddColumn)
    or (FOffset <> DefaultOffset)
    or (FOffsetBackground <> DefaultOffsetBackground);
end;

procedure TBCHColors.SetBackground(const Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    if Assigned(fParent) then
    begin
      TCustomBCHexEditor(FParent).Color := Value;
      fParent.Invalidate;
    end;
  end;
end;

procedure TBCHColors.SetChangedBackground(const Value: TColor);
begin
  if FChangedBackground <> Value then
  begin
    FChangedBackground := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TBCHColors.SetCurrentOffsetBackground(const Value: TColor);
begin
  if FCurrentOffsetBackground <> Value then
  begin
    FCurrentOffsetBackground := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TBCHColors.SetNonFocusCursorFrame(const Value: TColor);
begin
  if FNonFocusCursorFrame <> Value then
  begin
    FNonFocusCursorFrame := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TBCHColors.SetChangedText(const Value: TColor);
begin
  if FChangedText <> Value then
  begin
    FChangedText := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TBCHColors.SetCursorFrame(const Value: TColor);
begin
  if FCursorFrame <> Value then
  begin
    FCursorFrame := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TBCHColors.SetEvenColumn(const Value: TColor);
begin
  if FEvenColumn <> Value then
  begin
    FEvenColumn := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TBCHColors.SetOddColumn(const Value: TColor);
begin
  if FOddColumn <> Value then
  begin
    FOddColumn := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TBCHColors.SetOffset(const Value: TColor);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TBCHColors.SetOffsetBackground(const Value: TColor);
begin
  if FOffsetBackground <> Value then
  begin
    FOffsetBackground := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TBCHColors.SetCurrentOffset(const Value: TColor);
begin
  if FCurrentOffset <> Value then
  begin
    FCurrentOffset := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TBCHColors.SetParent(const Value: TControl);
begin
  FParent := Value;
  Assign(self);
end;

procedure TBCHColors.SetGrid(const Value: TColor);
begin
  if FGrid <> Value then
  begin
    FGrid := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

procedure TBCHColors.SetActiveFieldBackground(const Value: TColor);
begin
  if FActiveFieldBackground <> Value then
  begin
    FActiveFieldBackground := Value;
    if Assigned(fParent) then
      fParent.Invalidate;
  end;
end;

{ TBCHUndoStorage }

type

  // undo storage

  PUndoSelRec = ^TUndoSelRec;
  TUndoSelRec = packed record
    SelStart,
      SelEnd,
      SelPos: integer;
  end;

constructor TBCHUndoStorage.Create(AEditor: TCustomBCHexEditor);
begin
  inherited Create;
  FEditor := AEditor;
  FRedoPointer := nil;
  FLastUndo := nil;
  FLastUndoSize := 0;
  Reset;
end;

destructor TBCHUndoStorage.Destroy;
begin
  Reset;
  inherited;
end;

function TBCHUndoStorage.BeginUpdate: integer;
begin
  Inc(FUpdateCount);
  Result := FUpdateCount;
end;

function TBCHUndoStorage.CanUndo: boolean;
begin
  Result := (FCount > 0) and (FUpdateCount < 1) and (Size > 0);
end;

procedure TBCHUndoStorage.CreateUndo(aKind: TBCHUndoFlag; APosition, ACount,
  AReplaceCount: integer; const SDescription: string);
var
  urPos: integer;

  function PUndoRec: PBCHUndoRec;
  begin
    Result := PBCHUndoRec(@(PChar(Memory)[urPos]))
  end;
  //LPurUndoRec: PMPHUndoRec;

  procedure NewFillBuffer(ASize: integer);
  var
    i: integer;
  begin
    i := Position;
    urPos := i;
    (*if FEditor.FSelPosition > -1 then
      ASize := ASize+sizeof(TUndoSelRec);*)

    Size := Size + sizeof(TBCHUndoRec) + ASize;

    FillChar(PUndoRec^, SizeOf(TBCHUndoRec) + ASize, 0);
    with PUndoRec^ do
    begin
      Flags := [aKind];
      CurPos := FEditor.GetPosAtCursor(FEditor.Col, FEditor.Row);
      if not FEditor.FPosInCharField then
        with FEditor.GetCursorAtPos(CurPos, FEditor.FPosInCharField) do
          if (FEditor.Col - x) <> 0 then
            Include(Flags, ufFlag2ndByteCol);
      if FEditor.FPosInCharField then
        Include(Flags, ufFlagInCharField);
      if FEditor.FInsertModeOn then
        Include(Flags, ufFlagInsertMode);
      Pos := aPosition;
      Count := aCount;
      ReplCount := aReplaceCount;
      CurTranslation := FEditor.FTranslation;
      if FEditor.UnicodeChars then
        Include(Flags, ufFlagIsUnicode);
      if FEditor.UnicodeBigEndian then
        Include(Flags, ufFlagIsUnicodeBigEndian);
      CurBPU := FEditor.BytesPerUnit;
      if FEditor.FModified then
        Include(Flags, ufFlagModified);
      if FEditor.FSelPosition > -1 then
        Include(Flags, ufFlagHasSelection);
      if SDescription <> '' then
        Include(Flags, ufFlagHasDescription);
    end;
  end;

  procedure DeleteOldestUndoRec;
  var
    LintRecSize: integer;
  begin
    begin
      if Size < 4 then
      begin
        Size := 0;
        FCount := 0;
      end
      else
      begin
        Seek(0, soFromBeginning);
        Read(LIntRecSize, sizeof(integer));
        if LIntRecSize < sizeof(TBCHUndoRec) then
        begin
          Size := 0;
          FCount := 0;
        end
        else
        begin
          Move(PChar(Memory)[LIntRecSize], Memory^, Size - LIntRecSize);
          Size := Size - LIntRecSize;
          if FCount > 0 then
            Dec(FCount);
        end;
      end;
    end;
  end;

  procedure UpdateUndoRecord(Length: integer = 0);
  var
    LRecSelection: TUndoSelRec;
    i: integer;
  begin
    PUndoRec^.DataLen := SizeOf(TBCHUndoRec) + Length + 4;
    if ufFlagHasSelection in PUndoRec^.Flags then
      Inc(PUndoRec^.DataLen, sizeof(TUndoSelRec));
    if ufFlagHasDescription in PUndoRec^.Flags then
      Inc(PUndoRec^.DataLen, system.Length(SDescription) + sizeof(i));

    Position := Size;
    if ufFlagHasDescription in PUndoRec^.Flags then
    begin
      write(Sdescription[1], system.Length(SDescription));
      i := system.Length(sDescription);
      write(i, sizeof(i));
      Length := Length + i + sizeof(i);
    end;

    if ufFlagHasSelection in PUndoRec^.Flags then
    begin
      with LRecSelection do
      begin
        SelStart := FEditor.FSelStart;
        SelEnd := FEditor.FSelEnd;
        SelPos := FEditor.FSelPosition;
      end;
      Write(LRecSelection, sizeof(LRecSelection));
      Length := Length + sizeof(LRecSelection);
    end;

    Length := SizeOf(TBCHUndoRec) + 4 + Length;
    Write(Length, 4);
  end;

var
  LPtrBytes: PByteArray;
  LSStDesc: shortstring;
begin
  if FUpdateCount < 1 then
  begin
    ResetRedo;

    if sDescription <> '' then
      FDescription := sDescription
    else
      FDescription := STRS_UNDODESC[aKind];

    while (FEditor.FMaxUndo > 0) and (FCount > 0) and (Size > FEditor.FMaxUndo)
      do
      DeleteOldestUndoRec;

    Position := Size;

    Inc(FCount);

    case aKind of
      ufKindBytesChanged:
        begin
          NewFillBuffer(aCount - 1);
          LPtrBytes := PByteArray(@PUndoRec.Buffer);
          FEditor.ReadBuffer(LPtrBytes^, aPosition, aCount);
          if FEditor.HasChanged(aPosition) then
            Include(PUndoRec.Flags, ufFlagByte1Changed);
          if (aCount = 2) and FEditor.HasChanged(aPosition + 1) then
            Include(PUndoRec.Flags, ufFlagByte2Changed);
          UpdateUndoRecord(aCount - 1);
        end;
      ufKindByteRemoved:
        begin
          NewFillBuffer(aCount - 1);
          LPtrBytes := PByteArray(@PUndoRec.Buffer);
          FEditor.ReadBuffer(LPtrBytes^, aPosition, aCount);
          FEditor.AdjustBookmarks(aPosition + aCount, -aCount);
          UpdateUndoRecord(aCount - 1);
        end;
      ufKindInsertBuffer:
        begin
          NewFillBuffer(0);
          FEditor.AdjustBookmarks(aPosition, aCount);
          UpdateUndoRecord;
        end;
      ufKindReplace:
        begin
          NewFillBuffer(aReplaceCount - 1);
          LPtrBytes := PByteArray(@PUndoRec.Buffer);
          FEditor.ReadBuffer(LPtrBytes^, aPosition, aReplaceCount);
          FEditor.AdjustBookmarks(aPosition + aCount, aCount - aReplaceCount);
          UpdateUndoRecord(aReplaceCount - 1);
        end;
      ufKindAppendBuffer:
        begin
          NewFillBuffer(0);
          UpdateUndoRecord;
        end;
      ufKindNibbleInsert:
        begin
          NewFillBuffer(0);
          PUndoRec.Buffer := FEditor.Data[aPosition];
          if FEditor.HasChanged(aPosition) then
            Include(PUndoRec.Flags, ufFlagByte1Changed);
          UpdateUndoRecord;
        end;
      ufKindNibbleDelete:
        begin
          NewFillBuffer(0);
          PUndoRec.Buffer := FEditor.Data[aPosition];
          if FEditor.HasChanged(aPosition) then
            Include(PUndoRec.Flags, ufFlagByte1Changed);
          UpdateUndoRecord;
        end;
      ufKindConvert:
        begin
          NewFillBuffer(aCount - 1);
          LPtrBytes := PByteArray(@PUndoRec.Buffer);
          FEditor.ReadBuffer(LPtrBytes^, aPosition, aCount);
          UpdateUndoRecord(aCount - 1);
        end;
      ufKindSelection:
        begin
          NewFillBuffer(0);
          PUndoRec^.CurPos := APosition;
          UpdateUndoRecord;
          AddSelection(APosition, ACount);
        end;
      ufKindAllData:
        begin
          aCount := FEditor.DataSize;
          if aCount = 0 then
            NewFillBuffer(0)
          else
            NewFillBuffer(aCount - 1);
          LPtrBytes := PByteArray(@PUndoRec.Buffer);
          if aCount > 0 then
            FEditor.ReadBuffer(LPtrBytes^, 0, aCount);
          if aCount = 0 then
            UpdateUndoRecord
          else
            UpdateUndoRecord(aCount - 1);
        end;
      ufKindCombined:
        begin
          LSStDesc := shortstring(sDescription);
          NewFillBuffer(Length(LSStDesc));
          PUndoRec.Buffer := aCount;
          if FEditor.HasChanged(aPosition) then
            Include(PUndoRec.Flags, ufFlagByte1Changed);
          Move(LSStDesc[0], PUndoRec^.Buffer, Length(LSStDesc) + 1);
          UpdateUndoRecord(Length(LSStDesc));
        end;
    end;
  end;
end;

function TBCHUndoStorage.EndUpdate: integer;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0;
  Result := FUpdateCount;
end;

function TBCHUndoStorage.Undo: boolean;

  procedure PopulateUndo(const aBuffer: TBCHUndoRec);
  var
    LRecSel: TUndoSelRec;
    LCoord: TGridCoord;
  begin
    LCoord := FEditor.GetCursorAtPos(aBuffer.CurPos, ufFlagInCharField in
      aBuffer.Flags);
    with LCoord do
    begin
      if not (ufFlagInCharField in aBuffer.Flags) then
        if FEditor.DataSize > 0 then
          if ufFlag2ndByteCol in aBuffer.Flags then
            x := x + 1;

      FEditor.MoveColRow(x, y, True, True);
    end;
    FEditor.FModified := ufFlagModified in aBuffer.Flags;
    FEditor.InsertMode := (ufFlagInsertMode in aBuffer.Flags);
    if ufFlagHasSelection in aBuffer.Flags then
    begin
      Position := Size - 4 - sizeof(LRecSel);
      Read(LRecSel, sizeof(LRecSel));
      with LRecSel do
      begin
        if SelEnd = -1 then
          FEditor.Seek(SelStart, FILE_BEGIN)
        else
          FEditor.SetSelection(SelPos, SelStart, SelEnd);
      end;
    end;
    FEditor.UnicodeChars := (ufFlagIsUnicode in aBuffer.Flags);
    FEditor.UnicodeBigEndian := (ufFlagIsUnicodeBigEndian in aBuffer.Flags);
    if not FEditor.UnicodeChars then
      FEditor.Translation := aBuffer.CurTranslation
    else
      FEditor.FTranslation := aBuffer.CurTranslation;
    FEditor.BytesPerUnit := aBuffer.CurBPU;
    FEditor.Invalidate;
    FEditor.Changed;
  end;

var
  LEnumUndo: TBCHUndoFlag;
  LRecUndo: TBCHUndoRec;
  LIntLoop: integer;
  s: string;
begin
  Result := False;
  if not CanUndo then
  begin
    Reset(False);
    Exit;
  end;

  if Size >= sizeof(TBCHUndoRec) then
  begin
    // letzten eintrag lesen
    LEnumUndo := ReadUndoRecord(LRecUndo, s);
    // redo erstellen
    CreateRedo(LRecUndo);
    case LEnumUndo of
      ufKindBytesChanged:
        begin
          FEditor.WriteBuffer(PChar(Memory)[Position - 1], LRecUndo.Pos,
            LRecUndo.Count);
          FEditor.SetChanged(LRecUndo.Pos, ufFlagByte1Changed in
            LRecUndo.Flags);
          if LRecUndo.Count = 2 then
            FEditor.SetChanged(LRecUndo.Pos + 1, ufFlagByte2Changed in
              LRecUndo.Flags);
          PopulateUndo(LRecUndo);
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindByteRemoved:
        begin
          FEditor.InternalInsertBuffer(Pointer(Cardinal(Memory) + Position - 1),
            LRecUndo.Count, LRecUndo.Pos);
          PopulateUndo(LRecUndo);
          FEditor.AdjustBookmarks(LRecUndo.Pos - LRecUndo.Count,
            LRecUndo.Count);
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            FEditor.FModifiedBytes.Size := LRecUndo.Pos;
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindInsertBuffer:
        begin
          FEditor.InternalDelete(LRecUndo.Pos, LRecUndo.Pos + LRecUndo.Count,
            -1, 0);
          PopulateUndo(LRecUndo);
          FEditor.AdjustBookmarks(LRecUndo.Pos, -LRecUndo.Count);
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            FEditor.FModifiedBytes.Size := LRecUndo.Pos;
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindSelection:
        begin
          PopulateUndo(LRecUndo);
          RemoveLastUndo;
        end;
      ufKindAllData:
        begin
          FEditor.FDataStorage.Size := LRecUndo.Count;
          FEditor.FDataStorage.WriteBufferAt(Pointer(Cardinal(Memory) + Position
            - 1)^, 0,
            LRecUndo.Count);
          FEditor.CalcSizes;
          PopulateUndo(LRecUndo);
          RemoveLastUndo;
        end;
      ufKindReplace:
        begin
          FEditor.InternalDelete(LRecUndo.Pos, LRecUndo.Pos + LRecUndo.Count,
            -1, 0);
          FEditor.InternalInsertBuffer(Pointer(Cardinal(Memory) + Position - 1),
            LRecUndo.ReplCount, LRecUndo.Pos);
          PopulateUndo(LRecUndo);
          FEditor.AdjustBookmarks(LRecUndo.Pos + LRecUndo.ReplCount,
            LRecUndo.ReplCount - LRecUndo.Count);
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            // was:
            // FEditor.FModifiedBytes.Size := Max(0, LRecUndo.Pos - 1);
            // line above might lead to an integer overflow
          begin
            if LRecUndo.Pos > 0 then
              FEditor.FModifiedBytes.Size := LRecUndo.Pos - 1
            else
              FEditor.FModifiedBytes.Size := 0;
          end;

          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindAppendBuffer:
        begin
          FEditor.Col := GRID_FIXED;
          FEditor.FDataStorage.Size := LRecUndo.Pos;
          FEditor.CalcSizes;
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            FEditor.FModifiedBytes.Size := LRecUndo.Pos;
          PopulateUndo(LRecUndo);
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindNibbleInsert:
        begin
          FEditor.InternalDeleteNibble(LRecUndo.Pos, False);
          FEditor.Data[LRecUndo.Pos] := LRecUndo.Buffer;
          FEditor.SetChanged(LRecUndo.Pos, ufFlagByte1Changed in
            LRecUndo.Flags);
          PopulateUndo(LRecUndo);
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            FEditor.FModifiedBytes.Size := LRecUndo.Pos;
          FEditor.FDataStorage.Size := FEditor.FDataStorage.Size - 1;
          FEditor.CalcSizes;
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindNibbleDelete:
        begin
          FEditor.InternalInsertNibble(LRecUndo.Pos, False);
          FEditor.Data[LRecUndo.Pos] := LRecUndo.Buffer;
          FEditor.SetChanged(LRecUndo.Pos, ufFlagByte1Changed in
            LRecUndo.Flags);
          PopulateUndo(LRecUndo);
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            FEditor.FModifiedBytes.Size := LRecUndo.Pos;
          FEditor.FDataStorage.Size := FEditor.FDataStorage.Size - 1;
          FEditor.CalcSizes;
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindConvert:
        begin
          FEditor.WriteBuffer(PChar(Memory)[Position - 1], LRecUndo.Pos,
            LRecUndo.Count);
          PopulateUndo(LRecUndo);
          if DWORD(FEditor.FModifiedBytes.Size) >= (LRecUndo.Pos) then
            FEditor.FModifiedBytes.Size := LRecUndo.Pos;
          FEditor.Invalidate;
          RemoveLastUndo;
        end;
      ufKindCombined:
        begin
          LIntLoop := LRecUndo.Count;
          RemoveLastUndo;
          for LIntLoop := 1 to LIntLoop do
            Undo;
          ResetRedo;
        end;
    end;
  end
  else
    Reset;
end;

procedure TBCHUndoStorage.RemoveLastUndo;
var
  LRecUndo: TBCHUndoRec;
  LSStDesc: shortstring;
  LIntRecOffs: integer;
begin
  if Size < sizeof(TBCHUndoRec) then
    Reset(False)
  else
  begin
    Position := Size - 4;
    Read(LIntRecOffs, 4);
    // restore record in case of a redo
    Seek(-LIntRecOffs, soFromCurrent);
    ReAllocMem(FLastUndo, LIntRecOffs);
    Read(FLastUndo^, LIntRecOffs);
    FLastUndoSize := LIntRecOffs;
    FLastUndoDesc := FDescription;

    // delete last undo record
    SetSize(Max(0, Size - LIntRecOffs));
    if FCount > 0 then
      Dec(FCount);
    if Size < sizeof(TBCHUndoRec) then
    begin
      Reset(False);
    end
    else
    begin
      if ReadUndoRecord(LRecUndo, FDescription) <> ufKindCombined then
      begin
        if FDescription = '' then
          FDescription := STRS_UNDODESC[GetUndoKind(LRecUndo.Flags)]
      end
      else
      begin
        if LRecUndo.Buffer = 0 then
          LSStDesc := ''
        else
        begin
          Read(LSStDesc[1], LRecUndo.Buffer);
          LSStDesc[0] := AnsiChar(LRecUndo.Buffer);
        end;
        if LSStDesc = '' then
          FDescription := STRS_UNDODESC[GetUndoKind(LRecUndo.Flags)]
        else
          FDescription := string(LSStDesc);
      end;
    end;
  end;
end;

procedure TBCHUndoStorage.SetSize(NewSize: integer);
begin
  inherited;
  if NewSize < sizeof(TBCHUndoRec) then
    FCount := 0;
end;

procedure TBCHUndoStorage.Reset(AResetRedo: boolean = True);
begin
  Size := 0;
  FCount := 0;
  FUpdateCount := 0;
  FDescription := '';
  if AResetRedo then
    ResetRedo;
end;

procedure TBCHUndoStorage.SetCount(const Value: integer);
begin
  FCount := Value;
  if FCount < 1 then
    Reset(False);
end;

function TBCHUndoStorage.CanRedo: boolean;
begin
  Result := Assigned(FRedoPointer);
end;

function TBCHUndoStorage.Redo: boolean;

  procedure SetEditorStateFromRedoRec(const _2Bytes: Boolean = False);
  var
    LCoord: TGridCoord;
  begin
    with FRedoPointer^ do
    begin
      Move(PChar(FRedoPointer)[FRedoPointer^.DataLen], FEditor.FBookmarks,
        sizeof(TBCHBookmarks));

      LCoord := FEditor.GetCursorAtPos(CurPos, ufFlagInCharField in Flags);
      with LCoord do
      begin
        if not (ufFlagInCharField in Flags) then
          if FEditor.DataSize > 0 then
            if ufFlag2ndByteCol in Flags then
              x := x + 1;

        FEditor.MoveColRow(x, y, True, True);
      end;
      FEditor.FModified := ufFlagModified in Flags;
      FEditor.InsertMode := (ufFlagInsertMode in Flags);

      with PUndoSelRec(@(PChar(FRedoPointer)[FRedoPointer^.DataLen +
        sizeof(TBCHBookmarks)]))^ do
        FEditor.SetSelection(SelPos, SelStart, SelEnd);

      FEditor.Translation := CurTranslation;
      FEditor.FTranslation := CurTranslation;
      FEditor.UnicodeChars := (ufFlagIsUnicode in Flags);
      FEditor.UnicodeBigEndian := (ufFlagIsUnicodeBigEndian in Flags);
      FEditor.BytesPerUnit := CurBPU;

      FEditor.InCharField := ufFlagInCharField in Flags;

      FEditor.SetChanged(Pos, ufFlagByte1Changed in Flags);
      if _2Bytes then
        FEditor.SetChanged(Pos + 1, ufFlagByte2Changed in Flags);

      // restore last undo record
      if Assigned(FLastUndo) then
      begin
        Seek(0, soFromEnd);
        Write(FLastUndo^, FLastUndoSize);
        Inc(FCount);
        FreeMem(FLastUndo);
        FLastUndo := nil;
        FLastUndoSize := 0;
      end;
      FDescription := FLastUndoDesc;

      FEditor.Invalidate;
      FEditor.BookmarkChanged;
    end;
  end;
begin
  Result := CanRedo;
  if Result then
  begin
    case GetUndoKind(FRedoPointer^.Flags) of
      ufKindBytesChanged:
        begin
          FEditor.WriteBuffer(FRedoPointer^.Buffer,
            FRedoPointer^.Pos, FRedoPointer^.Count);
          SetEditorStateFromRedoRec(FRedoPointer^.Count = 2);
        end;
      ufKindByteRemoved:
        begin
          FEditor.InternalDelete(FRedoPointer^.Pos,
            FRedoPointer^.Pos + FRedoPointer^.Count, -1, 0);
          SetEditorStateFromRedoRec;
        end;
      ufKindInsertBuffer:
        begin
          FEditor.InternalInsertBuffer(PAnsiChar(@(FRedoPointer^.Buffer)),
            FRedoPointer^.Count, FRedoPointer^.Pos);
          SetEditorStateFromRedoRec;
        end;
      ufKindSelection:
        begin
          SetEditorStateFromRedoRec;
        end;
      ufKindAllData:
        begin
          FEditor.FDataStorage.Size := FRedoPointer^.Count;
          FEditor.FDataStorage.WriteBufferAt(FRedoPointer^.Buffer, 0,
            FRedoPointer^.Count);
          FEditor.CalcSizes;
          SetEditorStateFromRedoRec;
        end;
      ufKindReplace:
        begin
          FEditor.InternalDelete(FRedoPointer^.Pos,
            FRedoPointer^.Pos + FRedoPointer^.ReplCount, -1, 0);
          FEditor.InternalInsertBuffer(PAnsiChar(@(FRedoPointer^.Buffer)),
            FRedoPointer^.Count, FRedoPointer^.Pos);
          SetEditorStateFromRedoRec;
        end;
      ufKindConvert:
        begin
          FEditor.InternalDelete(FRedoPointer^.Pos,
            FRedoPointer^.Pos + FRedoPointer^.Count, -1, 0);
          FEditor.InternalInsertBuffer(PAnsiChar(@(FRedoPointer^.Buffer)),
            FRedoPointer^.Count, FRedoPointer^.Pos);
          SetEditorStateFromRedoRec;
        end;
      ufKindAppendBuffer:
        begin
          FEditor.InternalAppendBuffer(PAnsiChar(@(FRedoPointer^.Buffer)),
            FRedoPointer^.Count);
          SetEditorStateFromRedoRec;
        end;
      ufKindNibbleInsert,
        ufKindNibbleDelete:
        begin
          FEditor.FDataStorage.Size := FRedoPointer^.Count;
          FEditor.FDataStorage.WriteBufferAt(FRedoPointer^.Buffer, 0,
            FRedoPointer^.Count);
          FEditor.CalcSizes;
          SetEditorStateFromRedoRec;
        end;
    end;
    ResetRedo;
    FEditor.Changed;
  end;
end;

procedure TBCHUndoStorage.ResetRedo;
begin
  if Assigned(FRedoPointer) then
    FreeMem(FRedoPointer);
  FRedoPointer := nil;
  if Assigned(FLastUndo) then
    FreeMem(FLastUndo);
  FLastUndo := nil;
  FLastUndoSize := 0;
  FLastUndoDesc := '';
end;

procedure TBCHUndoStorage.CreateRedo(const Rec: TBCHUndoRec);
var
  LIntDataSize: integer;

  procedure AllocRedoPointer;
  begin
    GetMem(FRedoPointer, sizeof(TBCHUndoRec) + sizeof(TBCHBookmarks) +
      sizeof(TUndoSelRec) + LIntDataSize);
    FRedoPointer^.Flags := [GetUndoKind(Rec.Flags)];
    FRedoPointer^.DataLen := sizeof(TBCHUndoRec) + LIntDataSize;
  end;

  procedure FinishRedoPointer;
  begin
    with FRedoPointer^ do
    begin
      CurPos := FEditor.GetPosAtCursor(FEditor.Col, FEditor.Row);
      if not FEditor.FPosInCharField then
        with FEditor.GetCursorAtPos(CurPos, FEditor.FPosInCharField) do
          if (FEditor.Col - x) <> 0 then
            Include(Flags, ufFlag2ndByteCol);
      if FEditor.FPosInCharField then
        Include(Flags, ufFlagInCharField);
      if FEditor.FInsertModeOn then
        Include(Flags, ufFlagInsertMode);
      Pos := Rec.pos;
      Count := Rec.Count;
      ReplCount := Rec.ReplCount;
      CurTranslation := FEditor.FTranslation;
      if FEditor.UnicodeChars then
        Include(Flags, ufFlagIsUnicode);
      if FEditor.UnicodeBigEndian then
        Include(Flags, ufFlagIsUnicodeBigEndian);
      CurBPU := FEditor.BytesPerUnit;
      if FEditor.FModified then
        Include(Flags, ufFlagModified);
    end;
    Move(FEditor.FBookmarks, PChar(FRedoPointer)[FRedoPointer^.DataLen],
      sizeof(TBCHBookmarks));
    with PUndoSelRec(@(PChar(FRedoPointer)[FRedoPointer^.DataLen +
      sizeof(TBCHBookmarks)]))^ do
    begin
      SelStart := FEditor.FSelStart;
      SelPos := FEditor.FSelPosition;
      SelEnd := FEditor.FSelEnd;
    end;
  end;
begin
  ResetRedo;
  // simple redo, store bookmarks, selection, insertmode, col, row, charfield...
  // and bytes to save

  case GetUndoKind(Rec.Flags) of
    ufKindBytesChanged:
      begin
        LIntDataSize := Rec.Count - 1;
        AllocRedoPointer;
        if FEditor.HasChanged(Rec.Pos) then
          Include(FRedoPointer^.Flags, ufFlagByte1Changed);
        if Rec.Count = 2 then
          if FEditor.HasChanged(Rec.Pos + 1) then
            Include(FRedoPointer^.Flags, ufFlagByte2Changed);
        FEditor.ReadBuffer(FRedoPointer^.Buffer, Rec.Pos, Rec.Count);
        FinishRedoPointer;
      end;
    ufKindByteRemoved:
      begin
        LIntDataSize := 0;
        AllocRedoPointer;
        FinishRedoPointer;
      end;
    ufKindInsertBuffer,
      ufKindReplace,
      ufKindConvert:
      begin
        LIntDataSize := Rec.Count;
        AllocRedoPointer;
        FEditor.ReadBuffer(FRedoPointer^.Buffer, Rec.Pos, Rec.Count);
        FinishRedoPointer;
      end;
    ufKindSelection:
      begin
        LIntDataSize := 0;
        AllocRedoPointer;
        FinishRedoPointer;
      end;
    ufKindAllData:
      begin
        LIntDataSize := FEditor.DataSize;
        AllocRedoPointer;
        FEditor.ReadBuffer(FRedoPointer^.Buffer, 0, FEditor.DataSize);
        FinishRedoPointer;
        FRedoPointer^.Count := FEditor.DataSize;
      end;
    ufKindAppendBuffer:
      begin
        LIntDataSize := FEditor.DataSize - integer(Rec.Pos);
        AllocRedoPointer;
        FEditor.ReadBuffer(FRedoPointer^.Buffer, Rec.Pos, FEditor.DataSize -
          integer(Rec.Pos));
        FinishRedoPointer;
      end;
    ufKindNibbleInsert,
      ufKindNibbleDelete:
      begin
        LIntDataSize := FEditor.DataSize;
        AllocRedoPointer;
        FEditor.ReadBuffer(FRedoPointer^.Buffer, 0, FEditor.DataSize);
        FinishRedoPointer;
        FRedoPointer^.Count := LIntDataSize;
      end;
  end;
  //FEditor.Changed;
end;

function TBCHUndoStorage.GetUndoKind(const Flags: TBCHUndoFlags): TBCHUndoFlag;
begin
  for Result := ufKindBytesChanged to ufKindAllData do
    if Result in Flags then
      Break;
end;

procedure TBCHUndoStorage.AddSelection(const APos, ACount: integer);
var
  P: PBCHUndoRec;
  PSel: PUndoSelRec;
  LIntRecOffset: integer;
begin
  if CanUndo then
  begin
    Position := Size - 4;
    Read(LIntRecOffset, 4);
    Seek(-LIntRecOffset, soFromCurrent);
    P := Pointer(Cardinal(Memory) + Position);
    if not (ufFlagHasSelection in P^.Flags) then
    begin
      Size := Size + SizeOf(TUndoSelRec);
      P := Pointer(Cardinal(Memory) + Position);
      Include(P^.Flags, ufFlagHasSelection);
      Inc(P^.DataLen, sizeof(TUndoSelRec));
      Inc(LIntRecOffset, sizeof(TUndoSelRec));
      Seek(-4, soFromEnd);
      WriteBuffer(LIntRecOffset, 4);
    end;
    P^.CurPos := APos;
    PSel := Pointer(Cardinal(Memory) + size - 4 - sizeof(TUndoSelRec));
    PSel^.SelStart := APos;
    if aCount = 0 then
      PSel^.SelEnd := -1
    else
      PSel^.SelEnd := APos + Acount - 1;
    PSel^.SelPos := PSel^.SelStart;
  end;
end;

function TBCHUndoStorage.ReadUndoRecord(
  var aUR: TBCHUndoRec; var SDescription: string): TBCHUndoFlag;
var
  LIntRecOffs: integer;
  LIntPos: integer;
begin
  Position := Size - 4;
  Read(LIntRecOffs, 4);
  Seek(-LIntRecOffs, soFromCurrent);
  Read(aUR, SizeOf(TBCHUndoRec));
  Result := GetUndoKind(aUr.Flags);
  if ufFlagHasDescription in aUr.Flags then
  begin
    LIntPos := Position;
    try
      Position := size - 4 - sizeof(integer);
      if ufFlagHasSelection in aUr.Flags then
        Seek(-sizeof(TUndoSelRec), soFromCurrent);
      Read(LIntRecOffs, sizeof(integer));
      Seek(-(LIntRecOffs + sizeof(integer)), soFromCurrent);
      SetLength(SDescription, LIntRecOffs);
      Read(SDescription[1], LIntRecOffs);
    finally
      Position := LIntPos;
    end;
  end
  else
    SDescription := '';
end;

function TBCHUndoStorage.GetLastUndoKind: TBCHUndoFlag;
var
  recUndo: TBCHUndoRec;
  s: string;
begin
  Result := ReadUndoRecord(recUndo, s);
end;

// initialize tkCustom translation tables

procedure InitializeCustomTables;
var
  LBytLoop: byte;
begin
  for LBytLoop := 0 to 255 do
  begin
    BCHCustomCharConv[cctFromAnsi][LBytLoop] := AnsiChar(LBytLoop);
    BCHCustomCharConv[cctToAnsi][LBytLoop] := AnsiChar(LBytLoop);
  end;
end;

{ TBCHMemoryStream }

const
  MAX_PER_BLOCK = $F000;

function TBCHMemoryStream.GetAddress(const Index, Count: integer): PByte;
begin
  if (Index < 0) or ((Index+Count) > Size) then
    raise EBCHexEditor.Create(ERR_DATA_BOUNDS);
  Result := Pointer(Cardinal(Memory)+Cardinal(Index));
end;

function TBCHMemoryStream.GetAsHex(const APosition, ACount: integer;
  const SwapNibbles: Boolean): string;
begin
  SetLength(Result, ACount * 2);
  if ACount > 0 then
    ConvertBinToHex(PointerAt(APosition, ACount), @Result[1], ACount, SwapNibbles);
end;

procedure TBCHMemoryStream.Move(const AFromPos, AToPos, ACount: Integer);
begin
  MoveMemory(PointerAt(AToPos, ACount), PointerAt(AFromPos, ACount), ACount);
end;

function TBCHMemoryStream.PointerAt(const APosition, ACount: Integer): Pointer;
begin
  Result := GetAddress(APosition, ACount);
end;

procedure TBCHMemoryStream.ReadBufferAt(var Buffer; const APosition,
  ACount: Integer);
begin
  System.Move(GetAddress(APosition, ACount)^, Buffer, ACount);
end;

procedure TBCHMemoryStream.TranslateFromAnsi(const ToTranslation:
  TBCHTranslationKind; const APosition, ACount: integer);
begin
  if ToTranslation = tkAsIs then
    Exit; // no translation needed
  if ACount > 0 then
    TranslateBufferFromAnsi(ToTranslation, PointerAt(APosition, ACount),
      PointerAt(APosition, ACount), ACount);
end;

procedure TBCHMemoryStream.TranslateToAnsi(const FromTranslation:
  TBCHTranslationKind; const APosition, ACount: integer);
begin
  if FromTranslation = tkAsIs then
    Exit; // no translation needed
  if ACount > 0 then
    TranslateBufferToAnsi(FromTranslation, PointerAt(APosition, ACount),
      PointerAt(APosition, ACount), ACount);
end;

procedure TBCHMemoryStream.WriteBufferAt(const Buffer; const APosition,
  ACount: Integer);
begin
  System.Move(Buffer, GetAddress(APosition, ACount)^, ACount);
end;

initialization

  // initialize custom tables

  InitializeCustomTables;

end.

