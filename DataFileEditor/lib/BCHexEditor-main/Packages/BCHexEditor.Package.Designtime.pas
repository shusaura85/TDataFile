unit BCHexEditor.Package.Designtime;

interface {********************************************************************}

uses
  Classes, DesignIntf, DesignEditors,
  BCHexEditor, BCHexEditorEx;

type
  TBytesPerUnitProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TRulerNumberBaseProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation {***************************************************************}

uses
  SysUtils;

procedure Register;
begin
  RegisterComponents('BCHexEditor', [TBCHexEditor, TBCHexEditorEx]);

  RegisterPropertyEditor(TypeInfo(Integer), TCustomBCHexEditor, 'BytesPerUnit',
    TBytesPerUnitProperty);
  RegisterPropertyEditor(TypeInfo(Byte), TCustomBCHexEditor, 'RulerNumberBase',
    TRulerNumberBaseProperty);

  RegisterPropertiesInCategory(sVisualCategoryName, TCustomBCHexEditor,
    ['DrawGridLines', 'Colors', 'CaretStyle', 'BytesPerRow', 'FocusFrame',
    'BytesPerColumn',
      'GraySelectionIfNotFocused', 'MaskChar', 'OffsetFormat', 'ReadOnlyView',
      'HexLowerCase', 'ZoomOnWheel', 'DrawGutter3D', 'ShowRuler',
      'GutterWidth', 'HideSelection', 'PrintOptions', 'ScrollBars',
        'Translation', 'SeparateBlocksInCharField',
      'BytesPerUnit', 'CaretKind', 'RulerBytesPerUnit', 'BytesPerBlock',
      'ShowPositionIfNotFocused', 'UnicodeChars', 'UnicodeBigEndian',
      'RulerNumberBase', 'AutoBytesPerRow']);
  RegisterPropertyInCategory(sDragNDropCategoryName, TCustomBCHexEditor,
    'OleDragDrop');
  RegisterPropertyInCategory(sInputCategoryName, TCustomBCHexEditor,
    'OnInvalidKey');
end;

{ TBytesPerUnitProperty }

type
  TInt_Hexer = class(TCustomBCHexEditor); // propagate protected properties

function TBytesPerUnitProperty.GetAttributes: TPropertyAttributes;
var
  bRO: boolean;
  i: integer;
begin
  bRo := False;
  if PropCount > 0 then
    for i := 0 to Pred(PropCount) do
    begin
      if GetComponent(i) is TCustomBCHexEditor then
        if TInt_Hexer(GetComponent(i)).UnicodeChars then
        begin
          bRO := True;
          Break;
        end;
    end;

  Result := [paValueList, paSortList, paRevertable, paMultiSelect];
  if bRO then
    Include(Result, paReadOnly);
end;

procedure TBytesPerUnitProperty.GetValues(Proc: TGetStrProc);
begin
  if not (paReadOnly in GetAttributes) then  // unicode? no.
  begin
    Proc('1');
    Proc('2');
    Proc('4');
    Proc('8');
  end
  else
    Proc('2'); // unicode
end;

function TRulerNumberBaseProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable, paMultiSelect];
end;

procedure TRulerNumberBaseProperty.GetValues(Proc: TGetStrProc);
var
  i: integer;
begin
  for i := 2 to 16
  do
    Proc(IntToStr(i));
end;

end.
