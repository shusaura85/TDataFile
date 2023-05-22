# TDataFile for Lazarus/Free Pascal

This class works in a similar way to a standard **TIniFile** with many similar methods but more powerful for stored data in binary file. The class also supports basic xor encoding to scramble the binary file further.  

Built-in data types are stored with the values and can be checked before reading them.  

Any write operations are automatically saved to the opened file.  

**IMPORTANT** The files in this folder are designed for use with Lazarus/FreePascal. Due to differences in string handling, this version is **NOT** compatible with the Delphi version, as such has a different header signature.  
Section and value names, as well as all stored strings are encoded in UTF-8. The functions *ReadANSIString* and *WriteANSIString* are not present in this version. All other functions are identical.  

## Supported data types
The following types are supported by TDataFile:

 - Streams (any TStream descendant)
 - Strings (UTF-8)
 - String List (any TStrings descendant) 
 - Booleans
 - 8-bit signed and unsigned integers
 - 16-bit signed and unsigned integers
 - 32-bit signed and unsigned integers
 - 64-bit signed and unsigned integers
 - Single and Double floats
 - Currency
 - Date Time
 - Font (name, style, size, etc)
 - Any data type (using ReadData() and WriteData())

## Constructor
The constructor creates a new instance of TDataFile.

    constructor Create(const FileName: string; readonly: boolean = false);
 
 Parameter **FileName** specifies the path and file name of the data file you open or create.  
 Parameter **readonly** specifies if the file is opened as readonly. In readonly mode the write functions will not do anything.  
  
If you want to use the built-in encoding, set the **CodeKey** property after creating the class object.  
  
    var df: TDataFile;
    df := TDataFile.Create('filename');
    df.CodeKey := 'key used to encode data'; 


## Available functions and procedures

    
|Function|Parameters|Return type|Information|
|--|--|--|--|
|GetSectionNames|List: TStrings|*none*|Returns a list of the existing sections in the file|
|GetValueNames|Section: string; List: TStrings|*none*|Returns a list of existing values in the specified section|
|GetValueNamesAndTypes|Section: string; List: TStrings|*none*|Returns a list of existing values and their data types in the specified section. Use List.Names[] and List.Values[] for the data|
|GetValueType|Section, Ident: string|TDFType|Returns the data type for the specified value|
|GetValueTypeAsString|Section, Ident: string|string|Same as **GetValueType** but returns the data type as a string|
|SectionExists|Section: string|Boolean|Check if the specified section exists|
|ValueExists|Section, Ident: string|Boolean|Check if the value exists in the section|
|ReadData|Section, Ident: string; pBuf: Pointer|Integer|Low level function to read any data stored to the specified pointer|
|ReadStream|Section, Ident: string; Stream: TStream|Integer|Read a stream from the datafile |
|ReadString|Section, Ident: string; Default: string|string|Read a unicode String from the datafile. If the identifier doesn't exist, returns the *Default* value|
|ReadInt8|Section, Ident: string; Default: Int8|Int8|Read a signed 8-bit integer|
|ReadInt16|Section, Ident: string; Default: Int16|Int16|Read a signed 16-bit integer|
|ReadUInt8|Section, Ident: string; Default: UInt8|UInt8|Read an unsigned 8-bit integer|
|ReadUInt16|Section, Ident: string; Default: UInt16|UInt16|Read an unsigned 16-bit integer|
|ReadInteger, ReadInt32|Section, Ident: string; Default: Integer|Integer|Read a signed 32-bit integer|
|ReadInt64|Section, Ident: string; Default: Int64|Int64|Read a signed 64-bit integer|
|ReadUInt32|Section, Ident: string; Default: UInt32|UInt32|Read an unsigned 32-bit integer|
|ReadUInt64|Section, Ident: string; Default: UInt64|UInt64|Read an unsigned 64-bit integer|
|ReadSingle|Section, Ident: string; Default: Single|Single|Read a 32-bit float|
|ReadDouble|Section, Ident: string; Default: Double|Double|Read a 64-bit float|
|ReadCurrency|Section, Ident: string; Default: Currency|Currency|Read a currency value|
|ReadDateTime|Section, Ident: string; Default: TDateTime|TDateTime|Read a datetime value|
|ReadBoolean|Section, Ident: string; Default: Boolean|Boolean|Read a boolean value|
|ReadStrings|Section, Ident: string; List: TStrings|*none*|Read a string list in the specified *List*|
|ReadFont|Section, Ident: string; Font: TFont|*none*|Read font settings (name, style, size, etc) in the specified *Font*|
|WriteData|Section, Ident: string; pBuf: Pointer; Count: Integer; DataType: TDFType = dfd_Unknown|Integer|Low level function to write any data stored to the specified pointer|
|WriteStream|Section, Ident: string; Stream: TStream; DataType: TDFType = dfd_Stream|Integer|Write a stream from the datafile |
|WriteString|Section, Ident, Value: string|*none*|Write a unicode String from the datafile. If the identifier doesn't exist, returns the *Default* value|
|WriteInt8|Section, Ident: string; Value: Int8|*none*|Write a signed 8-bit integer|
|WriteInt16|Section, Ident: string; Value: Int16|*none*|Write a signed 16-bit integer|
|WriteUInt8|Section, Ident: string; Value: UInt8|*none*|Write an unsigned 8-bit integer|
|WriteUInt16|Section, Ident: string; Value: UInt16|*none*|Write an unsigned 16-bit integer|
|WriteInteger, WriteInt32|Section, Ident: string; Value: Integer|*none*|Write a signed 32-bit integer|
|WriteInt64|Section, Ident: string; Value: Int64|*none*|Write a signed 64-bit integer|
|WriteUInt32|Section, Ident: string; Value: UInt32|*none*|Write an unsigned 32-bit integer|
|WriteUInt64|Section, Ident: string; Value: UInt64|*none*|Write an unsigned 64-bit integer|
|WriteSingle|Section, Ident: string; Value: Single|*none*|Write a 32-bit float|
|WriteDouble|Section, Ident: string; Value: Double|*none*|Write a 64-bit float|
|WriteCurrency|Section, Ident: string; Value: Currency|*none*|Write a currecy value|
|WriteDateTime|Section, Ident: string; Value: TDateTime|*none*|Write a datetime value|
|WriteBoolean|Section, Ident: string; Value: Boolean|*none*|Write a boolean value|
|WriteStrings|Section, Ident: string; List: TStrings|*none*|Write a string list in the specified *List*|
|WriteFont|Section, Ident: string; Font: TFont|*none*|Write font settings (name, style, size, etc) in the specified *Font*|
|DeleteSection|Section: string|*none*|Delete the specified section|
|Delete|Section, Ident: string|*none*|Delete the specified identifier|


## Requirements

TDataFile was designed to be used with Delphi XE and higher. It may work with older versions but it was not tested.  
The included editor was created using Delphi Ryo and may not compile in older versions.

## Credits

TDataFile was inspired by the original Degisy Software TDataFile.

# DataFile Editor

DataFile Editor is an application to create, view and modify files created with TDataFile. The editor will add a **META** section with a key **creation.date** in files created with it, but can be safely deleted once you add your own sections and values.  
The sections created with the editor will be presented as a tree, but the actual structure is flat, the actual section name stored is the full path shown in the upper part. The separator used when creating the tree is the slash character ("**/**").  

**IMPORTANT** DataFileEditor app was not tested in linux/mac but should compile. It requires the lazarus port of [MPHexEditor](https://github.com/michalgw/mphexeditor) component.  

# License
The code and editor are both released under the MIT license. See the **LICENSE** file for details.
