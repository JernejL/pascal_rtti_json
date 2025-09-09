{
    This file is part of the Free Component Library

    Pascal tree source file writer
    Copyright (c) 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Based on outpaswrite created jsonwrite
    Copyright (c) 2025 by
        Jernej Loknar

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}
{$inline on}

unit outjsonwrite;

interface

uses StrUtils, SysUtils, Classes, PasTree, fpjson, TypInfo;

type
  EPasWriter = Class(Exception);

  { TJsonWriter }
  TJsonWriterOption = (woNoImplementation, // Do not create implementation code.
                      woNoExternalClass,  // Do not create classes as external
                      woNoExternalVar,    // Do not declare external variables as external.
                      woNoExternalFunc,   // Do not declare external functions as external.
                      woAddLineNumber,    // Prefix line with generated line numbers in comment
                      woAddSourceLineNumber,    // Prefix line with original source line numbers (when available) in comment
                      woForwardClasses,   // Add forward definitions for all classes
                      woForceOverload,     // Force 'overload;' on overloads that are not marked as such.
                      woNoAsm,         // Do not allow asm block
                      woSkipPrivateExternals,  // Skip generation of external procedure declaration in implementation section
                      woAlwaysRecordHelper,     // Force use of record helper for type helper
                      woSkipHints          // Do not add identifier hints
                      );
  TJsonWriterOptions = Set of TJsonWriterOption;

  TOnUnitAlias = function(const UnitName : String) : String of Object;

  TJsonWriter = class
  private
    FCurrentLineNumber : Integer;
    FExtraUnits: String;
    FForwardClasses: TStrings;
    FLineEnding: String;
    FOnUnitAlias: TOnUnitAlias;
    FOPtions: TJsonWriterOptions;
    FStream: TStream;
    FIndentSize : Integer;
    IsStartOfLine: Boolean;
    FLineElement : TPasElement; // pastree
    FIndentStep,
    Indent,
    CurDeclSection: string;
    DeclSectionStack: TList;
    FInImplementation : Boolean;
    Fjson: TJSONObject;
    procedure SetForwardClasses(AValue: TStrings);
    procedure SetIndentSize(AValue: Integer);
    function CheckUnitAlias(const AUnitName : String) : String;
  protected
    procedure DisableHintsWarnings;
    procedure PrepareDeclSectionInStruct(const ADeclSection: string);
    procedure MaybeSetLineElement(AElement: TPasElement);
    procedure Add(const s: string);
    procedure Add(const Fmt: string; Args : Array of const);
    procedure AddLn(const s: string);overload;
    procedure AddLn(const Fmt: string; Args : Array of const);overload;
    procedure AddLn;overload;

    function GetExpr(E: TPasExpr): String; virtual;
    Function HasOption(aOption : TJsonWriterOption) : Boolean; inline;
    Function NotOption(aOption : TJsonWriterOption) : Boolean; inline;
    Function PostProcessLine(S : String) : String; virtual;
    Function GetLineNumberComment : String; virtual;
    Procedure ResetIndent;
    procedure IncIndent;
    procedure DecIndent;
    procedure IncDeclSectionLevel;
    procedure DecDeclSectionLevel;
    procedure PrepareDeclSection(const ADeclSection: string);

    procedure AddProcArgs(aList: TfpList); virtual;
    procedure AddProcArgsjson(aList: TfpList; var paramlist: Tjsonarray); virtual;
  public
    constructor Create(AStream: TStream); virtual;
    destructor Destroy; override;
    procedure WriteMembers(aMembers: TFPList; aDefaultVisibility: TPasMemberVisibility=visDefault); virtual;
    function getvistestrepr(whatvisibility: TPasMemberVisibility): string;
    procedure WriteMembersjson(aMembers: TFPList; ofobject: TJSONObject; aDefaultVisibility: TPasMemberVisibility=visDefault); virtual;

    procedure AddForwardClasses(aSection: TPasSection); virtual;
    procedure WriteResourceString(aStr: TPasResString); virtual;
    procedure WriteEnumType(AType: TPasEnumType); virtual;
    procedure WriteElement(AElement: TPasElement; SkipSection : Boolean = False);virtual;
    procedure WriteType(AType: TPasType; Full : Boolean = True);virtual;
    function WriteTypeJson(AType: TPasType; Full: Boolean; stortomember: Tjsonobject): Tjsonstring; virtual;

    procedure WriteProgram(aModule : TPasProgram); virtual;
    Procedure WriteLibrary(aModule : TPasLibrary); virtual;
    Procedure WriteUnit(aModule : TPasModule); virtual;
    procedure WriteModule(AModule: TPasModule); virtual;
    procedure WriteSection(ASection: TPasSection); virtual;
    procedure WriteUsesList(ASection: TPasSection); virtual;
    procedure WriteClass(AClass: TPasClassType); virtual;
    procedure WriteConst(AConst: TpasConst); virtual;
    procedure WriteVariable(aVar: TpasVariable); virtual;
    procedure WriteVariablejsonparameter(aVar: TpasVariable;  toparameterarray: TJSONArray;  existingobject: TJSONObject); virtual;
    procedure WriteArgument(aArg: TpasArgument); virtual;
    procedure WriteDummyExternalFunctions(aSection: TPasSection); virtual;
    procedure WriteOverloadedProc(aProc : TpasOverloadedProc; ForceBody: Boolean = False; NamePrefix : String = ''); virtual;
    Procedure WriteAliasType(AType : TpasAliasType); virtual;
    Procedure WriteRecordType(AType : TPasRecordType); virtual;
    Procedure WriteArrayType(AType : TPasArrayType; Full : Boolean = True); virtual;
    procedure WriteProcType(AProc: TPasProcedureType);  virtual;
    procedure WriteProcDecl(AProc: TpasProcedure; ForceBody: Boolean = False; NamePrefix : String = ''); virtual;
    procedure WriteProcDecljson(AProc: TpasProcedure; methodclass: TJSONObject; toparameterarray: TJSONArray; ForceBody: Boolean = False; NamePrefix : String = ''); virtual;
    procedure WriteProcImpl(AProc: TProcedureBody; IsAsm : Boolean = false); virtual;
    procedure WriteProcImpl(AProc: TpasProcedureImpl); virtual;
    procedure WriteProperty(AProp: TPasProperty); virtual;
    procedure WriteImplBlock(ABlock: TpasImplBlock);  virtual;
    procedure WriteImplElement(AElement: TpasImplElement; AAutoInsertBeginEnd: Boolean); virtual;
    procedure WriteImplCommand(ACommand: TpasImplCommand);virtual;
    procedure WriteImplCommands(ACommands: TpasImplCommands); virtual;
    procedure WriteImplIfElse(AIfElse: TpasImplIfElse); virtual;
    procedure WriteImplCaseOf(ACaseOf: TpasImplCaseOf); virtual;
    procedure WriteImplCaseStatement(ACaseStatement: TPasImplCaseStatement;
      AAutoInsertBeginEnd: boolean=true); virtual;
    procedure WriteImplForLoop(AForLoop: TPasImplForLoop); virtual;
    procedure WriteImplWhileDo(aWhileDo : TPasImplWhileDo); virtual;
    procedure WriteImplRepeatUntil(aRepeatUntil : TPasImplRepeatUntil); virtual;
    procedure WriteImplTryFinallyExcept(aTry: TPasImplTry); virtual;
    Procedure WriteImplRaise(aRaise : TPasImplRaise); virtual;
    Procedure WriteImplAssign(aAssign : TPasImplAssign); virtual;
    Procedure WriteImplSimple(aSimple: TPasImplSimple); virtual;
    Procedure WriteImplExceptOn(aOn : TPasImplExceptOn); virtual;
    Procedure WriteImplWithDo(aWith : TpasImplWithDo); virtual;
    //
    procedure wrt(const s: string); deprecated ;
    procedure wrtln(const s: string);overload; deprecated ;
    procedure wrtln;overload; deprecated ;
    property Stream: TStream read FStream;
  Published
    Property OnUnitAlias : TOnUnitAlias Read FOnUnitAlias Write FOnUnitAlias;
    Property Options : TJsonWriterOptions Read FOPtions Write FOptions;
    Property IndentSize : Integer Read FIndentSize Write SetIndentSize;
    Property LineEnding : String Read FLineEnding Write FLineEnding;
    Property ExtraUnits : String Read FExtraUnits Write FExtraUnits;
    Property ForwardClasses : TStrings Read FForwardClasses Write SetForwardClasses;
  end;

procedure WritePasFile(AElement: TPasElement; const AFilename: string);overload;
procedure WritePasFile(AElement: TPasElement; AStream: TStream);overload;

implementation

type
  PDeclSectionStackElement = ^TDeclSectionStackElement;
  TDeclSectionStackElement = record
    LastDeclSection, LastIndent: string;
  end;

constructor TJsonWriter.Create(AStream: TStream);
var
	classarr: TJSONArray;
begin
  FStream := AStream;
  IndentSize:=2;
  IsStartOfLine := True;
  DeclSectionStack := TList.Create;
  FForwardClasses:=TStringList.Create;
  FLineEnding:=sLineBreak;

  Fjson:= TJSONObject.create();
  classarr:= TJSONArray.Create();
  Fjson.Add('class', classarr);

end;

destructor TJsonWriter.Destroy;
var
  i: Integer;
  El: PDeclSectionStackElement;
begin
  for i := 0 to DeclSectionStack.Count - 1 do
  begin
    El := PDeclSectionStackElement(DeclSectionStack[i]);
    Dispose(El);
  end;
  DeclSectionStack.Free;
  FForwardClasses.Free;

  freeandnil(Fjson);

  inherited Destroy;
end;


procedure TJsonWriter.Add(const s: string);
begin

end;

procedure TJsonWriter.Add(const Fmt: string; Args: array of const);
begin

end;

procedure TJsonWriter.AddLn(const s: string);
begin

end;

procedure TJsonWriter.AddLn(const Fmt: string; Args: array of const);
begin

end;

procedure TJsonWriter.AddLn;
begin

end;

procedure TJsonWriter.MaybeSetLineElement(AElement : TPasElement);

begin
  If FLineElement=Nil then
    FLineElement:=AElement;
end;

procedure TJsonWriter.WriteElement(AElement: TPasElement;
	SkipSection: Boolean);

begin
  if not SkipSection then
    MaybeSetLineElement(AElement);
  if AElement.InheritsFrom(TPasModule) then
    WriteModule(TPasModule(AElement) )
  else if AElement.InheritsFrom(TPasSection) then
    WriteSection(TPasSection(AElement))
  else if AElement.ClassType.InheritsFrom(TPasProperty) then
    WriteProperty(TPasProperty(AElement))
  else if AElement.InheritsFrom(TpasConst) then
    WriteConst(TpasConst(AElement)) // Must be before variable
  else if AElement.InheritsFrom(TpasVariable) then
    WriteVariable(TpasVariable(AElement))
  else if AElement.InheritsFrom(TpasArgument) then
    WriteArgument(TpasArgument(AElement))
  else if AElement.InheritsFrom(TPasType) then
    WriteType(TPasType(AElement))
  else if AElement.InheritsFrom(TpasOverloadedProc) then
    WriteOverloadedProc(TpasOverloadedProc(AElement))
  else if AElement.InheritsFrom(TpasProcedureImpl) then // This one must come before TProcedureBody/TpasProcedure
    WriteProcImpl(TpasProcedureImpl(AElement))
  else if AElement.InheritsFrom(TpasProcedure) then
    WriteProcDecl(TpasProcedure(AElement))
  else if AElement.InheritsFrom(TProcedureBody) then
    WriteProcImpl(TProcedureBody(AElement))
  else if AElement.InheritsFrom(TpasImplCommand) or AElement.InheritsFrom(TpasImplCommands) then
    WriteImplElement(TpasImplElement(AElement),false)
  else if AElement.InheritsFrom(TPasResString) then
    WriteResourceString(TPasResString(AElement))
 else
    raise EPasWriter.CreateFmt('WriteElement Writing not implemented for %s nodes',[AElement.ElementTypeName]);
end;

procedure TJsonWriter.WriteResourceString(aStr : TPasResString);

begin
  PrepareDeclSection('resourcestring');
  AddLn(Astr.GetDeclaration(True)+';');
end;

procedure TJsonWriter.WriteEnumType(AType: TPasEnumType);

begin
  Add(Atype.GetDeclaration(true));
end;

procedure TJsonWriter.WriteType(AType: TPasType; Full : Boolean = True);

begin
  MaybeSetLineElement(AType);
  if Full and (AType.Parent is TPasSection)  then
    PrepareDeclSection('type');

  if AType = nil then
    Add('NO TYPE')
  else if AType.ClassType = TPasUnresolvedTypeRef then
    Add(AType.Name)
  else if AType.ClassType.InheritsFrom(TPasClassType) then
    WriteClass(TPasClassType(AType))
  else if AType.ClassType = TPasEnumType then
    WriteEnumType(TPasEnumType(AType))
  else if AType is TPasProcedureType then
    WriteProcType(TPasProcedureType(AType))
  else if AType is TPasArrayType then
    WriteArrayType(TPasArrayType(AType),Full)
  else if AType is TPasRecordType then
    WriteRecordType(TPasRecordType(AType))
  else if AType is TpasAliasType then
    WriteAliasType(TpasAliasType(AType))
  else if AType is TPasPointerType then
    Add(AType.GetDeclaration(true))
  else if AType is TPasSetType then
    Add(AType.GetDeclaration(true))
  else if AType is TPasRangeType then
    Add(AType.GetDeclaration(true))
  else
    raise EPasWriter.CreateFmt('WriteType Writing not implemented for %s type nodes',[aType.ElementTypeName]);
  if Full then
    AddLn(';');
end;

function TJsonWriter.WriteTypeJson(AType: TPasType; Full: Boolean;
	stortomember: Tjsonobject): Tjsonstring;
var
	arrayobject: TJSONObject;
    Resultstr: string;
begin

  result:= nil;

  if AType = nil then
    exit(Tjsonstring.Create('NO TYPE'))
  else if AType.ClassType = TPasUnresolvedTypeRef then
    exit(Tjsonstring.Create(AType.Name))
  else if AType is TpasAliasType then begin

	  If AType.Parent is TPasSection then
	    exit(Tjsonstring.Create(AType.GetDeclaration(true)))
	  else
	    exit(Tjsonstring.Create(TpasAliasType(AType).SafeName)); // alias of type


  end
  else if AType is TPasArrayType then begin

  	if assigned(stortomember) then begin

    	arrayobject:= Tjsonobject.Create();

        with TPasArrayType(AType) do begin

          	stortomember.Add('array', true);

        	If IsPacked then stortomember.Add('packed', true);
            Resultstr:= '';

	        if Full then
	          begin
	          if GenericTemplateTypes<>nil then
	            Resultstr:=SafeName+GenericTemplateTypesAsString(GenericTemplateTypes)+' = '+Resultstr
	          else
	            Resultstr:=SafeName+' = '+Resultstr;
	          end;

            If (IndexRange<>'') then stortomember.Add('IndexRange', IndexRange);

            If IsPacked then stortomember.Add('packed', true);

	        If not Assigned(Eltype) then
	          Resultstr:=Resultstr+'const';

	    	stortomember.Add('array_range', arrayobject);

            exit( Tjsonstring.Create( ElType.SafeName ) );

		end;
	end;

  	exit( Tjsonstring.Create( 'ARRIBARAJ ' + TPasArrayType(AType).GetDeclaration(Full) ) )

  end


  (*
  else if AType.ClassType.InheritsFrom(TPasClassType) then
    WriteClass(TPasClassType(AType))
  else if AType.ClassType = TPasEnumType then
    WriteEnumType(TPasEnumType(AType))
  else if AType is TPasProcedureType then
    WriteProcType(TPasProcedureType(AType))
  else if AType is TPasRecordType then
    WriteRecordType(TPasRecordType(AType))
  else if AType is TPasPointerType then
    Add(AType.GetDeclaration(true))
  else if AType is TPasSetType then
    Add(AType.GetDeclaration(true))
  else if AType is TPasRangeType then
    Add(AType.GetDeclaration(true))
  *)
  else
    raise EPasWriter.CreateFmt('WriteTypeJson Writing not implemented for %s type nodes',[aType.ElementTypeName]);

end;

procedure TJsonWriter.DisableHintsWarnings;

begin
  Addln('{$HINTS OFF}');
  Addln('{$WARNINGS OFF}');
  Addln('{$IFDEF FPC}');
  Addln('{$NOTES OFF}');
  Addln('{$ENDIF FPC}');
end;

procedure TJsonWriter.WriteProgram(aModule: TPasProgram);

Var
  S : String;

begin
  S:='';
  if aModule.Name<>'' then
    S:=Format('program %s',[aModule.SafeName]);
  if (S<>'') then
    begin
    If AModule.InputFile<>'' then
      begin
      S:=S+'('+aModule.InputFile;
      if aModule.OutPutFile<>'' then
        S:=S+','+aModule.OutPutFile;
      S:=S+')';
      end;
    AddLn(S+';');
    AddLn;
    end;

  if HasOption(woNoImplementation) then
    DisableHintsWarnings;

  if Assigned(aModule.ProgramSection) then
    WriteSection(aModule.ProgramSection);
  if Assigned(AModule.InitializationSection) then
    begin
    PrepareDeclSection('');
    AddLn;
    AddLn('begin');
    IncIndent;
    if NotOption(woNoImplementation) then
      WriteImplBlock(AModule.InitializationSection);
    DecIndent;
    end;
  Addln('end.');
end;

procedure TJsonWriter.WriteLibrary(aModule: TPasLibrary);
Var
  S : String;

begin
  S:='';
  if aModule.Name<>'' then
    S:=Format('library %s',[aModule.SafeName]);
  if (S<>'') then
    begin
    If AModule.InputFile<>'' then
      begin
      S:=S+'('+aModule.InputFile;
      if aModule.OutPutFile<>'' then
        S:=S+','+aModule.OutPutFile;
      S:=S+')';
      end;
    AddLn(S+';');
    AddLn;
    end;
  if HasOption(woNoImplementation) then
    DisableHintsWarnings;
  if Assigned(AModule.InitializationSection) then
    begin
    PrepareDeclSection('');
    AddLn;
    AddLn('begin');
    IncIndent;
    if NotOption(woNoImplementation) then
      WriteImplBlock(AModule.InitializationSection);
    DecIndent;
    end;
  Addln('end.');
end;

procedure TJsonWriter.WriteDummyExternalFunctions(aSection : TPasSection);

  Function IsExt(P : TpasProcedure; AllowConstructor : Boolean) : Boolean;

  begin
    Result:=Assigned(P.LibrarySymbolName) or Assigned(P.LibraryExpr);
    if (Not Result) Then
      Result:=(AllowConstructor and (P is TpasConstructor));
  end;

  Procedure DoCheckElement(E : TPasElement; Force : Boolean; Prefix: String);

  Var
    P : TpasProcedure;
    PP : TpasOverloadedProc;
    I : Integer;

  begin
    if (E is TpasProcedure) then
      begin
      P:=E as TpasProcedure;
      if Force or IsExt(P,False) then
        WriteProcDecl(P,True,Prefix)
      end
    else if (E is TpasOverloadedProc) then
      begin
      PP:=(E as TpasOverloadedProc);
      For I:=0 to PP.Overloads.Count-1 do
        begin
        P:=TpasProcedure(PP.Overloads[I]);
        if Force or IsExt(P,False) then
          WriteProcDecl(P,True,Prefix)
        end
      end;
  end;

  procedure DoCheckClass(C: TPasClassType; Force : Boolean; Prefix: String);
  var
    I: Integer;
    M : TPasElement;

  begin
    if (C.ExternalName<>'') then
      for I:=0 to C.Members.Count-1 do
      begin
        M:=TPasElement(C.members[I]);
        if (M is TPasClassType) then
          DoCheckClass(M as TPasClassType, Force, Prefix + C.SafeName + '.')
        else
          DoCheckElement(M, Force, Prefix + C.SafeName + '.');
      end;
  end;

Var
  I : Integer;
  E : TPasElement;
  C : TPasClassType;

begin
  Addln;
  Addln('// Dummy implementations for externals');
  Addln;
  For I:=0 to aSection.Declarations.Count-1 do
    begin
    E:=TPasElement(aSection.Declarations[i]);
    DoCheckElement(E,False,'');
    if (E is TPasClassType) then
      DoCheckClass(E as TPasClassType, True, '');
    end;
  Addln;
  Addln('// end of dummy implementations');
  Addln;
end;

procedure TJsonWriter.AddForwardClasses(aSection : TPasSection);

Var
  I : Integer;
  CN : String;

begin
  if Not Assigned(aSection.Classes) or (aSection.Classes.Count=0) then
    exit;
  PrepareDeclSection('type');
  For I:=0 to aSection.Classes.Count-1 do
    begin
    CN:=TPasElement(aSection.Classes[i]).SafeName;
    if (FForwardClasses.Count=0) or (ForwardClasses.IndexOf(CN)<>-1) then
      Addln('%s = class;',[CN]);
    end;
end;

procedure TJsonWriter.WriteUnit(aModule: TPasModule);

begin
  AddLn('unit ' + CheckUnitAlias(AModule.SafeName) + ';');
  if Assigned(AModule.GlobalDirectivesSection) then
    begin
    AddLn;
    WriteImplElement(AModule.GlobalDirectivesSection,false);
    end;
  AddLn;
  AddLn('interface');
  AddLn;
  WriteSection(AModule.InterfaceSection);
  ResetIndent;
  AddLn;
  AddLn;
  AddLn('implementation');
  FInImplementation:=True;
  if HasOption(woNoImplementation) then
    DisableHintsWarnings;
  if hasOption(woNoExternalFunc) then
    WriteDummyExternalFunctions(AModule.InterfaceSection);
  if Assigned(AModule.ImplementationSection) then
    begin
    AddLn;
    WriteSection(AModule.ImplementationSection);
    end;
  AddLn;
  if NotOption(woNoImplementation) then
    begin
    PrepareDeclSection('');
    if Assigned(AModule.InitializationSection) then
      begin
      AddLn('initialization');
      IncIndent;
      WriteImplBlock(AModule.InitializationSection);
      DecIndent;
      end;
    if Assigned(AModule.FinalizationSection) then
      begin
      AddLn('finalization');
      IncIndent;
      WriteImplBlock(AModule.FinalizationSection);
      DecIndent;
      end;
    end;
  AddLn('end.');
end;

procedure TJsonWriter.WriteModule(AModule: TPasModule);
var
	len : Integer;
    L: String;
begin

  Fjson.Clear;

  FInImplementation:=False;;
  if aModule is TPasProgram then
    WriteProgram(TPasProgram(aModule))
  else if aModule is TPasLibrary then
    WriteLibrary(TPasLibrary(aModule))
  else
    WriteUnit(aModule);

  // WITHOUT ORIGINAL OUTPUTTING.
  Stream.Seek(0, sofromcurrent);
  Stream.size:= 0;

	L:= fjson.FormatJSON([], 4);
	Len:=Length(L);

	if Len>0 then
		Stream.Write(L[1],Len);

end;

procedure TJsonWriter.WriteUsesList(ASection: TPasSection);

Const
  UnitSeps = [',',';',' '];

Var
  C : Integer;

  function AllowUnit(S : String) : Boolean;

  begin
    Result:=Not SameText(S,'System');
  end;

  Procedure AddUnit(Const aName : String; AUnitFile : TPasExpr);
  begin
    if c > 0 then
      Add(', ')
    else
      Add('uses ');
    Add(CheckUnitAlias(AName));
    if (AUnitFile<>Nil) then
      Add(' in '+GetExpr(AUnitFile));
    Inc(c);
  end;

Var
  I : integer;
  u : string;

begin
  C:=0;
  if ASection.UsesList.Count>0 then
    begin
    if not (aSection is TImplementationSection) then
      For I:=1 to WordCount(ExtraUnits,UnitSeps) do
        begin
        u:=Trim(ExtractWord(1,ExtraUnits,UnitSeps));
        if (U<>'') then
          AddUnit(U,Nil);
        end;
    if length(ASection.UsesClause)=ASection.UsesList.Count then
      begin
      for i := 0 to length(ASection.UsesClause)-1 do
        if AllowUnit(ASection.UsesClause[i].Name) then
          AddUnit(ASection.UsesClause[i].Name,ASection.UsesClause[i].InFilename);
      end
    else
      for i := 0 to ASection.UsesList.Count - 1 do
        if AllowUnit(TPasElement(ASection.UsesList[i]).Name) then
          AddUnit(TPasElement(ASection.UsesList[i]).Name,Nil);
    if C>0 then
      begin
      AddLn(';');
      AddLn;
      end;
    end;
end;

procedure TJsonWriter.WriteSection(ASection: TPasSection);

var
  i: Integer;

begin
  // WriteUsesList(aSection);

  CurDeclSection := '';
  if HasOption(woForwardClasses) then
    begin
    AddForwardClasses(ASection);
    AddLn;
    end;
  for i := 0 to ASection.Declarations.Count - 1 do
    WriteElement(TPasElement(ASection.Declarations[i]));
end;

procedure TJsonWriter.WriteClass(AClass: TPasClassType);

var
  i: Integer;
  InterfacesListPrefix: string;
  classarr: TJSONArray;
  thisobject: TJSONObject;
  interfacers: Tjsonarray;

begin
  PrepareDeclSection('type');
  Addln;
  MaybeSetLineElement(AClass);


  if (fjson.Find('class', classarr) = false) then begin

	  classarr:= TJSONArray.Create();
	  fjson.Add('class', classarr);

  end;

  // AClass.SafeName

  if AClass.IsForward = false then begin

	  thisobject:= TJSONObject.create();

	  thisobject.add('name', AClass.SafeName);
	  thisobject.add('packed', AClass.IsPacked);
	  thisobject.add('kind', ObjKindNames[ AClass.ObjKind ] );

	  if (AClass.ObjKind = okTypeHelper) then
	    if HasOption(woAlwaysRecordHelper) then
	  		thisobject.add('helper', 'record' )
	  	else
		  	thisobject.add('helper', 'type' );

	  if (AClass.ObjKind in [okTypeHelper,okRecordHelper,okClassHelper]) then
	    begin
	    if not Assigned(AClass.HelperForType) then
	      thisobject.add('helpertype', 'unknowntype' )
	    else
	      thisobject.add('helpertype', AClass.HelperForType.SafeName );
      end;



	  if (AClass.ObjKind=okClass) and (ACLass.ExternalName<>'') and NotOption(woNoExternalClass) then thisobject.add('ExternalName', AClass.ExternalName );
	  if Assigned(AClass.AncestorType) then thisobject.add('ancestor', AClass.AncestorType.SafeName );

	  if AClass.Interfaces.Count > 0 then
	  begin

          // if Assigned(AClass.AncestorType) then

        interfacers:= TJSONArray.Create();

        interfacers.Add(AClass.HelperForType.SafeName);
      	thisobject.add('interfaces', interfacers );

        // Add(InterfacesListPrefix + TPasType(AClass.Interfaces[0]).SafeName);

	    for i := 1 to AClass.Interfaces.Count - 1 do
        	interfacers.Add(TPasType(AClass.Interfaces[i]).SafeName);

	  end;

	  if AClass.ObjKind = okInterface then
	    if Assigned(AClass.GUIDExpr) then
	      thisobject.add('guid', '['+AClass.InterfaceGUID+']' );


      WriteMembersjson(AClass.Members, thisobject); // TJSONObject

  end; // not forward declaration.















  if AClass.IsPacked then
     Add('packed ');                      // 12/04/04 - Dave - Added
  case AClass.ObjKind of
    okObject: Add('object');
    okClass: Add('class');
    okInterface: Add('interface');
    okTypeHelper :
      if HasOption(woAlwaysRecordHelper) then
        Add('record helper')
      else
        Add('type helper');
    okRecordHelper: Add('record helper');
    okClassHelper: Add('class helper');
  end;
  if (AClass.ObjKind in [okTypeHelper,okRecordHelper,okClassHelper]) then
    begin
    if not Assigned(AClass.HelperForType) then
      Add(' for unknowntype')
    else
      Add(' for '+AClass.HelperForType.SafeName)
    end;

  if AClass.IsForward then
    exit;
  if (AClass.ObjKind=okClass) and (ACLass.ExternalName<>'') and NotOption(woNoExternalClass) then
    Add(' external name ''%s'' ',[AClass.ExternalName]);
  if Assigned(AClass.AncestorType) then
    Add('(' + AClass.AncestorType.SafeName);
  if AClass.Interfaces.Count > 0 then
  begin
    if Assigned(AClass.AncestorType) then
      InterfacesListPrefix:=', '
    else
      InterfacesListPrefix:='(';
    Add(InterfacesListPrefix + TPasType(AClass.Interfaces[0]).SafeName);
    for i := 1 to AClass.Interfaces.Count - 1 do
      Add(', ' + TPasType(AClass.Interfaces[i]).SafeName);
  end;
  if Assigned(AClass.AncestorType) or (AClass.Interfaces.Count > 0) then
    AddLn(')')
  else
    AddLn;
  if AClass.ObjKind = okInterface then
    if Assigned(AClass.GUIDExpr) then
      AddLn('['+AClass.InterfaceGUID+']');
  IncIndent;
  IncDeclSectionLevel;
  WriteMembers(AClass.Members);
  DecDeclSectionLevel;
  DecIndent;
  Add('end');

  classarr.Add(thisobject);

end;

procedure TJsonWriter.WriteMembers(aMembers: TFPList;
	aDefaultVisibility: TPasMemberVisibility);

Var
  Member, LastMember: TPasElement;
  LastVisibility, CurVisibility: TPasMemberVisibility;

  function ForceVisibility: boolean;
  begin
    Result := (LastMember <> nil) and
      // variables can't be declared directly after methods nor properties
      // (visibility section or var keyword is required)
      ((Member is TpasVariable) and not (Member is TPasProperty)) and not (LastMember is TpasVariable);
  end;

Var
  I : integer;

begin
  LastVisibility:=aDefaultVisibility;
  LastMember := nil;
  for i := 0 to aMembers.Count - 1 do
    begin
    Member := TPasElement(aMembers[i]);
    CurVisibility := Member.Visibility;
    if (CurVisibility <> LastVisibility) or ForceVisibility then
      begin
      DecIndent;
      case CurVisibility of
        visPrivate: AddLn('private');
        visProtected: AddLn('protected');
        visPublic: AddLn('public');
        visPublished: AddLn('published');
        visAutomated: AddLn('automated');
      end;
      IncIndent;
      LastVisibility := CurVisibility;
      CurDeclSection := '';
      end;
    WriteElement(Member);
    LastMember := Member;
    end;
end;

function TJsonWriter.getvistestrepr(whatvisibility: TPasMemberVisibility
	): string;
begin

	case whatvisibility of
      visPrivate: exit('private');
      visProtected: exit('protected');
      visPublic: exit('public');
      visPublished: exit('published');
      visAutomated: exit('automated');
      else
        exit('INVALID VISIBILITY.');
	end;

end;

procedure TJsonWriter.WriteMembersjson(aMembers: TFPList;
	ofobject: TJSONObject; aDefaultVisibility: TPasMemberVisibility);
var
	Member: TPasElement;

Var
i, k: integer;
class_method_list, class_property_list, class_variable_list, where_put_member: TJSONObject;
currentmethod: TJSONObject;
methodtypes, varprop: TJSONObject;
methodvisLabel: String;
currmethodlist: TJSONArray;
newmethodlist, paramarray: Tjsonarray;
IsImpl: TPasElement;

procedure WriteSeparateMemberProp(var currmethodlist: TJSONArray; const Member: TPasElement);
var
	j: integer;
begin

	if Member is TPasProcedure then
		where_put_member:= class_method_list
	else if Member is TPasFunction then
		where_put_member:= class_method_list
	else if member is TPasProperty then
		where_put_member:= class_property_list
	else if Member is TPasVariable then
		where_put_member:= class_variable_list;

    methodvisLabel:= getvistestrepr( Member.Visibility );

    currmethodlist:= nil;

    where_put_member.find(methodvisLabel, currmethodlist);

  if (not assigned(currmethodlist)) then begin

    currmethodlist:= TJSONArray.Create();

  	where_put_member.Add(methodvisLabel, currmethodlist);

  end;


    currentmethod:= TJSONObject.Create();

    currentmethod.Add('name', Member.SafeName);

    // todo: static variables.

    currentmethod.Add('Member.ClassName', Member.ClassName);

    // currentmethod.Add('test_decl', Member.GetDeclaration(true));


    // if (TPasProcedureImpl(aMembers[i]).IsClassMethod) then currentmethod.Add('IsClass', true);
    // if (TPasProperty(member).IsClass) then currentmethod.Add('IsClass', true);

    // vmClass in VarModifiers


    // currentmethod.add('Debug member class', Member.ClassName);


    		try
            if Member is TPasFunction then begin

            		//currentmethod.add('method_type', 'function');

            		currentmethod.add('method_type', TPasProcedure(member).TypeName);

                  if TPasFunction(member).IsVirtual       then currentmethod.add('Virtual', true);
                  if TPasFunction(member).IsDynamic       then currentmethod.add('Dynamic', true);
                  if TPasFunction(member).IsAbstract      then currentmethod.add('Abstract', true);
                  if TPasFunction(member).IsOverride      then currentmethod.add('Override', true);
                  if TPasFunction(member).IsExported      then currentmethod.add('Exported', true);
                  if TPasFunction(member).IsExternal      then currentmethod.add('External', true);
                  if TPasFunction(member).IsOverload      then currentmethod.add('Overload', true);
                  if TPasFunction(member).IsMessage       then currentmethod.add('Message', true);
                  if TPasFunction(member).IsReintroduced  then currentmethod.add('Reintroduced', true);
                  if TPasFunction(member).IsStatic        then currentmethod.add('Static', true);
                  if TPasFunction(member).IsForward       then currentmethod.add('Forward', true);
                  if TPasFunction(member).IsAssembler     then currentmethod.add('Assembler', true);
                  if TPasFunction(member).IsAsync         then currentmethod.add('Async', true);

  		  end
  		  else if Member is TPasProcedure then begin

            		currentmethod.add('method_type', TPasProcedure(member).TypeName);

                  if TPasProcedure(member).IsVirtual       then currentmethod.add('Virtual', true);
                  if TPasProcedure(member).IsDynamic       then currentmethod.add('Dynamic', true);
                  if TPasProcedure(member).IsAbstract      then currentmethod.add('Abstract', true);
                  if TPasProcedure(member).IsOverride      then currentmethod.add('Override', true);
                  if TPasProcedure(member).IsExported      then currentmethod.add('Exported', true);
                  if TPasProcedure(member).IsExternal      then currentmethod.add('External', true);
                  if TPasProcedure(member).IsOverload      then currentmethod.add('Overload', true);
                  if TPasProcedure(member).IsMessage       then currentmethod.add('Message', true);
                  if TPasProcedure(member).IsReintroduced  then currentmethod.add('Reintroduced', true);
                  if TPasProcedure(member).IsStatic        then currentmethod.add('Static', true);
                  if TPasProcedure(member).IsForward       then currentmethod.add('Forward', true);
                  if TPasProcedure(member).IsAssembler     then currentmethod.add('Assembler', true);
                  if TPasProcedure(member).IsAsync         then currentmethod.add('Async', true);

  		  end;

            //else if member is TPasProperty then
            //    currentmethod.add('member type', 'property')
            //else if Member is TPasVariable then
  		  //    currentmethod.add('member type', 'var')
            //else
            //  currentmethod.add('type', 'type ' + Member.ClassName + ' not implemented')

        except on e: Exception do

        	currentmethod.add('type', 'exception ' + e.Message);

  	  end;


        paramarray:= Tjsonarray.Create();

        if (Member is TPasProcedure) or (Member is TPasFunction) then begin
        	  currentmethod.Add('parameters', paramarray);
        	  WriteProcDecljson(TpasProcedure(Member), currentmethod, paramarray)
  	  end
  	  else if member is TPasProperty then begin

        	with TPasProperty(member) do begin

  	      	currentmethod.Add('safename', SafeName);

              if (IsClass) then currentmethod.Add('IsClass', true);

              if IndexValue <> '' then
                currentmethod.Add('Index', IndexValue);
              if ReadAccessorName <> '' then
              	currentmethod.Add('read', ReadAccessorName);
              if WriteAccessorName <> '' then
              	currentmethod.Add('write', WriteAccessorName);
              if StoredAccessorName <> '' then
              	currentmethod.Add('stored', StoredAccessorName);
              if DefaultValue <> '' then
              	currentmethod.Add('default', DefaultValue);
              if IsNodefault then
              	currentmethod.Add('nodefault', true);
              if IsDefault then
              	currentmethod.Add('default', true);

  	        if Assigned(VarType) then
  	        	currentmethod.Add('proptype', VarType.SafeName);

              if Args.Count > 0 then
              begin

              	currentmethod.Add('TODO: Args.Count is needed for properties.', true);

                for j := 0 to Args.Count - 1 do
                  begin

                  	// TpasArgument(Args[j]).SafeName;

                  end;

              end;

  		end;




          // TPasProperty(member).SafeName

  	  end
  	  else if Member is TPasVariable then begin

          WriteVariablejsonparameter(TpasVariable(Member), nil, currentmethod);


  	  end else
        	raise EPasWriter.CreateFmt('Writing not implemented for %s ( %s ) nodes',[Member.ElementTypeName, member.ClassName]);


{
    if Member.InheritsFrom(TpasVariable) then

    else if Member.InheritsFrom(TPasModule) then
      WriteModule(TPasModule(Member) )
    else if Member.InheritsFrom(TPasSection) then
      WriteSection(TPasSection(Member))
    else if Member.ClassType.InheritsFrom(TPasProperty) then
      WriteProperty(TPasProperty(Member))
    else if Member.InheritsFrom(TpasConst) then
      WriteConst(TpasConst(Member)) // Must be before variable
    else if Member.InheritsFrom(TpasArgument) then
      WriteArgument(TpasArgument(Member))
    else if Member.InheritsFrom(TPasType) then
      WriteType(TPasType(Member))
    else if Member.InheritsFrom(TpasOverloadedProc) then
      WriteOverloadedProc(TpasOverloadedProc(Member))
    else if Member.InheritsFrom(TpasProcedureImpl) then // This one must come before TProcedureBody/TpasProcedure
      WriteProcImpl(TpasProcedureImpl(Member))
    else if Member.InheritsFrom(TpasProcedure) then
      WriteProcDecljson(TpasProcedure(Member), currentmethod, paramarray)
    else if Member.InheritsFrom(TProcedureBody) then
      WriteProcImpl(TProcedureBody(Member))
    else if Member.InheritsFrom(TpasImplCommand) or Member.InheritsFrom(TpasImplCommands) then
      WriteImplElement(TpasImplElement(Member),false)
    else if Member.InheritsFrom(TPasResString) then
      WriteResourceString(TPasResString(Member))
   else
      raise EPasWriter.CreateFmt('Writing not implemented for %s nodes',[Member.ElementTypeName]);
     }





    currmethodlist.Add(currentmethod);

end;

begin

  currmethodlist:= nil;

  if (ofobject.Find('methods', class_method_list) = false) then begin

	  class_method_list:= TJSONObject.Create();
	  ofobject.Add('methods', class_method_list);

  end;

  if (ofobject.Find('properties', class_property_list) = false) then begin

	  class_property_list:= TJSONObject.Create();
	  ofobject.Add('properties', class_property_list);

  end;

    if (ofobject.Find('variables', class_variable_list) = false) then begin

	  class_variable_list:= TJSONObject.Create();
	  ofobject.Add('variables', class_variable_list);

  end;

  methodtypes:= TJSONObject.Create();

    for i := 0 to aMembers.Count - 1 do begin

    	Member := TPasElement(aMembers[i]);

        if Member.InheritsFrom(TpasOverloadedProc) then begin // from TPasProcedureBase

			// inherited functions have multiple functions in overloads prop

        	with TpasOverloadedProc(Member) do begin

	        	for k:= 0 to Overloads.Count - 1 do begin

                    WriteSeparateMemberProp(currmethodlist, TPasElement(Overloads[k]) );

				end;

            end;

			// WriteOverloadedProc(TpasOverloadedProc(Member))

		end else begin

            WriteSeparateMemberProp(currmethodlist, Member);

		end;

    end; // with member

end;

procedure TJsonWriter.WriteConst(AConst: TpasConst);

Const
  Seps : Array[Boolean] of Char = ('=',':');

Var
  Vart,Decl : String;

begin
  PrepareDeclSection('const');
  Decl:='';
  With AConst do
    begin
    If Assigned(VarType) then
      begin
      If VarType.Name='' then
        Vart:=VarType.GetDeclaration(False)
      else
        Vart:=VarType.SafeName;
      Decl:=Vart+Modifiers;
      Vart:=LowerCase(Vart);
      if (Value<>'') then
         Decl:=Decl+' = '+Value
      else if (ExportName<>Nil) or ((Parent is TPasClassType) and (TPasClassType(Parent).ExternalName<>'')) then // external name
        case VarT of
          'integer',
          'byte',
          'word',
          'smallint',
          'int64',
          'nativeint',
          'shortint',
          'longint' : Decl:=Decl+' = 0';
          'double',
          'single',
          'extended' : Decl:=Decl+' = 0.0';
          'string' : Decl:=Decl+' = ''''';
        else
          if Pos('array',Vart)>0 then
            Decl:=Decl+' = []';
        end;
      end
    else
      Decl:=Value;

    Decl:=SafeName+' '+Seps[Assigned(VarType)]+' '+Decl;
    if NotOption(woSkipHints) then
      Decl:=Decl+HintsString;
    end;
  AddLn(Decl+';');
end;

procedure TJsonWriter.WriteVariable(aVar: TpasVariable);

var
  LParentIsClassOrRecord: boolean;

begin
  LParentIsClassOrRecord:= (aVar.Parent.ClassType = TPasClassType) or
    (aVar.Parent.ClassType = TPasRecordType);
  if not LParentIsClassOrRecord then
    PrepareDeclSection('var')
  // handle variables in classes/records
  else if vmClass in aVar.VarModifiers then
    PrepareDeclSectionInStruct('class var')
  else if (CurDeclSection<>'') and not (aVar.Parent.ClassType = TPasRecordType) then
    PrepareDeclSectionInStruct('var');
  Add(aVar.SafeName + ': ');
  if Not Assigned(aVar.VarType) then
    Raise EWriteError.CreateFmt('No type for variable %s',[aVar.SafeName]);
  WriteType(aVar.VarType,False);
  if (aVar.AbsoluteExpr<>nil) then
    Add(' absolute %s',[aVar.AbsoluteExpr.ClassName])
  else if (aVar.LibraryName<>Nil) or Assigned (aVar.ExportName) then
    begin
    if LParentIsClassOrRecord then
      begin
      if NotOption(woNoExternalClass) then
        Add('; external name ''%s''',[aVar.ExportName.GetDeclaration(true)]);
      end
    else if NotOption(woNoExternalVar) then
      begin
      Add('; external ');
      if (aVar.LibraryName<>Nil) then
        Add('%s ',[aVar.LibraryName.GetDeclaration(true)]);
      Add('name %s',[aVar.ExportName.GetDeclaration(true)]);
      end;
    end;
  if Not LParentIsClassOrRecord then
    if Assigned(aVar.Expr) then
      Add(' = '+aVar.Expr.GetDeclaration(true));
  AddLn(';');
end;

procedure TJsonWriter.WriteVariablejsonparameter(aVar: TpasVariable;
	toparameterarray: TJSONArray; existingobject: TJSONObject);
var
  LParentIsClassOrRecord: boolean;
  newvariable: TJSONObject;

begin

  if existingobject = nil then
  	newvariable:= Tjsonobject.Create()
  else
  	newvariable:= existingobject;

  LParentIsClassOrRecord:= (aVar.Parent.ClassType = TPasClassType) or
    (aVar.Parent.ClassType = TPasRecordType);

  if not LParentIsClassOrRecord then
    PrepareDeclSection('var')
  // handle variables in classes/records
  else if vmClass in aVar.VarModifiers then
    PrepareDeclSectionInStruct('class var')
  else if (CurDeclSection<>'') and not (aVar.Parent.ClassType = TPasRecordType) then
    PrepareDeclSectionInStruct('var');

  if (newvariable.Find('name') = nil) then
  	newvariable.Add('name', aVar.SafeName);

  if Not Assigned(aVar.VarType) then
    Raise EWriteError.CreateFmt('No type for variable %s',[aVar.SafeName]);

  if (newvariable.Find('type') = nil) then
  	newvariable.Add('type', WriteTypejson(aVar.VarType,False, existingobject));

  if (aVar.AbsoluteExpr<>nil) then
    newvariable.Add('absolute', aVar.AbsoluteExpr.ClassName)
  else if (aVar.LibraryName<>Nil) or Assigned (aVar.ExportName) then
    begin
    if LParentIsClassOrRecord then
      begin
      if NotOption(woNoExternalClass) then
        newvariable.Add('external', aVar.ExportName.GetDeclaration(true));
      end
    else if NotOption(woNoExternalVar) then
      begin

      if (aVar.LibraryName<>Nil) then
        newvariable.Add('library', aVar.LibraryName.GetDeclaration(true) );
      	newvariable.Add('exportname', aVar.ExportName.GetDeclaration(true) );
      end;
    end;
  if Not LParentIsClassOrRecord then
    if Assigned(aVar.Expr) then
      newvariable.Add('=', aVar.Expr.GetDeclaration(true) );

  if toparameterarray <> nil then
	  toparameterarray.Add(newvariable);

end;

procedure TJsonWriter.WriteArgument(aArg: TpasArgument);

begin
  if (aArg.Access<>argDefault) then
    Add(AccessNames[aArg.Access]+' ');
  Add(aArg.SafeName+' : ');
  WriteType(aArg.ArgType,False);
end;

procedure TJsonWriter.WriteOverloadedProc(aProc: TpasOverloadedProc; ForceBody: Boolean = False; NamePrefix : String = '');

Var
  I : integer;

begin
  For I:=0 to aProc.Overloads.Count-1 do
    begin
    if HasOption(woForceOverload) then
      TpasProcedure(aProc.Overloads[i]).AddModifier(pmOverload);
    WriteProcDecl(TPasElement(aProc.Overloads[i]) as TpasProcedure,ForceBody,NamePrefix);
    end;
end;

procedure TJsonWriter.WriteAliasType(AType: TpasAliasType);

begin
  If AType.Parent is TPasSection then
    Add(AType.GetDeclaration(true))
  else
    Add(AType.Name)
end;

procedure TJsonWriter.WriteRecordType(AType: TPasRecordType);

Var
  Temp : String;

begin
  Temp:='record';
  If aType.IsPacked then
    if Atype.IsBitPacked then
      Temp:='bitpacked '+Temp
    else
      Temp:='packed '+Temp;
  If (Atype.Name<>'') then
    begin
    if AType.GenericTemplateTypes.Count>0 then
      Temp:=AType.SafeName+GenericTemplateTypesAsString(AType.GenericTemplateTypes)+' = '+Temp
    else
      Temp:=AType.SafeName+' = '+Temp;
    end;
  AddLn(Temp);
  IncIndent;
  IncDeclSectionLevel;
  WriteMembers(AType.Members,visPublic);
  DecDeclSectionLevel;
  DecIndent;
  Add('end');
end;

procedure TJsonWriter.WriteArrayType(AType: TPasArrayType; Full : Boolean = True);

begin
  Add(AType.GetDeclaration(Full));
end;

procedure TJsonWriter.WriteProcType(AProc: TPasProcedureType);

begin
  Add(TPasProcedureType(AProc).GetDeclaration(true));
  if TPasProcedureType(AProc).CallingConvention<>ccDefault then
    Add('; '+cCallingConventions[TPasProcedureType(AProc).CallingConvention]);
end;


procedure TJsonWriter.WriteProcDecl(AProc: TpasProcedure; ForceBody : Boolean = False; NamePrefix : String = '');

  Procedure EmptyBody;

  begin
    Addln('');
    Addln('begin');
    AddLn('end;');
    Addln('');
  end;
Var
  AddExternal : boolean;
  IsImpl : Boolean;

begin

  IsImpl:=AProc.Parent is TImplementationSection;
  if IsImpl then
    PrepareDeclSection('');
  if Not IsImpl then
    IsImpl:=FInImplementation;
  if FInImplementation and not forcebody and (Assigned(AProc.LibraryExpr) or Assigned(AProc.LibrarySymbolName)) and HasOption(woSkipPrivateExternals)  then
    Exit;
  Add(AProc.TypeName + ' ' + NamePrefix+AProc.SafeName);
  if Assigned(AProc.ProcType) and (AProc.ProcType.Args.Count > 0) then
    AddProcArgs(AProc.ProcType.Args) ;
  if Assigned(AProc.ProcType) and
    (AProc.ProcType.ClassType = TPasFunctionType) then
  begin
    Add(': ');
    WriteType(TPasFunctionType(AProc.ProcType).ResultEl.ResultType,False);
  end;
  Add(';');
  // delphi compatible order for example: procedure foo; reintroduce; overload; static;
  if not IsImpl and AProc.IsReintroduced then
    Add(' reintroduce;');
  // if NamePrefix is not empty, we're writing a dummy for external class methods.
  // In that case, we must not write the 'overload'.
  if AProc.IsOverload and (NamePrefix='') and not IsImpl then
    Add(' overload;');
  if not IsImpl then
    begin
    if AProc.IsVirtual then
      Add(' virtual;');
    if AProc.IsDynamic then
      Add(' dynamic;');
    if AProc.IsAbstract then
      Add(' abstract;');
    if AProc.IsOverride then
      Add(' override;');
    if AProc.IsStatic then
      Add(' static;');
    end;
  if (pmAssembler in AProc.Modifiers) and Not (woNoAsm in OPtions) then
    Add(' assembler;');
  if AProc.CallingConvention<>ccDefault then
    Add(' '+cCallingConventions[AProc.CallingConvention]+';');
  If Assigned(AProc.LibraryExpr) or Assigned(AProc.LibrarySymbolName) then
    begin
    if AProc.Parent is TPasClassType then
      AddExternal:=NotOption(woNoExternalClass)
    else
      AddExternal:=NotOption(woNoExternalFunc);
    if AddExternal then
      begin
      add('external');
      if Assigned(AProc.LibraryExpr) then
        Add(' '+GetExpr(AProc.LibraryExpr));
      if Assigned(AProc.LibrarySymbolName) then
        Add(' name '+GetExpr(AProc.LibrarySymbolName));
      Add(';');
      end;
    end;
  AddLn;

  if Assigned(AProc.Body)  then
    begin
    if (pmAssembler in AProc.Modifiers) and (woNoAsm in Options) then
      EmptyBody
    else
      WriteProcImpl(AProc.Body,pmAssembler in AProc.Modifiers)
    end
  else if ForceBody then
    EmptyBody;
end;

procedure TJsonWriter.WriteProcDecljson(AProc: TpasProcedure;
	methodclass: TJSONObject; toparameterarray: TJSONArray; ForceBody: Boolean;
	NamePrefix: String);
Var
  AddExternal : boolean;
  IsImpl : Boolean;
  tempresult: TJSONObject;
  methodreturn: TJSONArray;

begin

  //tempo:= TJSONObject.Create();

  IsImpl:=AProc.Parent is TImplementationSection;

  // if Not IsImpl then IsImpl:=FInImplementation;

  // skip implementation (only definition)
  if FInImplementation and not forcebody and (Assigned(AProc.LibraryExpr) or Assigned(AProc.LibrarySymbolName)) and HasOption(woSkipPrivateExternals)  then Exit;

  // tempo.add('method type', AProc.TypeName);
  // tempo.add('name', NamePrefix+AProc.SafeName);

  if Assigned(AProc.ProcType) and (AProc.ProcType.ClassType = TPasFunctionType) then // is a function so it has return parameter.
  begin

//  	methodclass.add('type', 'function');

	tempresult:= Tjsonobject.Create();
	tempresult.add('name', 'return');
	tempresult.add('type', WriteTypeJson(TPasFunctionType(AProc.ProcType).ResultEl.ResultType,False, tempresult) );
    tempresult.add('access', 'return');

    methodreturn:= Tjsonarray.Create();

    methodreturn.ADD(tempresult);

    methodclass.add('return', methodreturn);

	//toparameterarray.add(tempresult);

  end else begin

    // if (AProc.ProcType is TPasProcedureType) then methodclass.add('type', 'procedure');

  end;

  if Assigned(AProc.ProcType) and (AProc.ProcType.Args.Count > 0) then
    AddProcArgsjson(AProc.ProcType.Args, toparameterarray); // ADD ALL INPUT PARAMETERS


  // delphi compatible order for example: procedure foo; reintroduce; overload; static;
  if not IsImpl and AProc.IsReintroduced then
    methodclass.add('reintroduce', true);
  // if NamePrefix is not empty, we're writing a dummy for external class methods.
  // In that case, we must not write the 'overload'.
  if AProc.IsOverload and (NamePrefix='') and not IsImpl then

  	if (methodclass.Get('Overload') = nil) then
	  	methodclass.add('Overload', true);

  if not IsImpl then
    begin
    if AProc.IsVirtual then
      methodclass.add('virtual', true);
    if AProc.IsDynamic then
      methodclass.add('dynamic', true);
    if AProc.IsAbstract then
      methodclass.add('abstract', true);
    if AProc.IsOverride then
      methodclass.add('override', true);
    if AProc.IsStatic then
      methodclass.add('static', true);
    end;
  if (pmAssembler in AProc.Modifiers) and Not (woNoAsm in OPtions) then
    methodclass.add('assembler', true);
  if AProc.CallingConvention<>ccDefault then
    methodclass.add('convention', cCallingConventions[AProc.CallingConvention] );
  If Assigned(AProc.LibraryExpr) or Assigned(AProc.LibrarySymbolName) then
    begin
    if AProc.Parent is TPasClassType then
      AddExternal:=NotOption(woNoExternalClass)
    else
      AddExternal:=NotOption(woNoExternalFunc);
    if AddExternal then
      begin
      methodclass.add('external', true);
      if Assigned(AProc.LibraryExpr) then
         methodclass.add('library', GetExpr(AProc.LibraryExpr) );
      if Assigned(AProc.LibrarySymbolName) then
        methodclass.add('symbol name', GetExpr(AProc.LibrarySymbolName));

      end;
    end;


  if Assigned(AProc.Body)  then
    begin
      // WriteProcImpljson(AProc.Body,pmAssembler in AProc.Modifiers)
    end;

  //toparameterarray.Add(tempo);

end;


procedure TJsonWriter.AddProcArgs(aList : TfpList);
Var
  I : Integer;
  A : TpasArgument;

begin
  Add('(');
  If Assigned(aList) then
    for i := 0 to Alist.Count - 1 do
      begin
      A:= TpasArgument(AList[i]);
      if i > 0 then
        Add('; ');
      Add(AccessNames[A.Access]+A.Name);
      if Assigned(A.ArgType) then
        begin
        Add(': ');
        WriteType(A.ArgType,False);
        end;
      if A.Value <> '' then
        Add(' = ' + A.Value);
      end;
  Add(')');
end;

procedure TJsonWriter.AddProcArgsjson(aList: TfpList; var paramlist: Tjsonarray
	);
Var
  I : Integer;
  A : TpasArgument;
  tempresult: TJSONObject;

begin

  If Assigned(aList) then
    for i := 0 to Alist.Count - 1 do
      begin

	      A:= TpasArgument(AList[i]);

	      tempresult:= Tjsonobject.Create();

          if AccessNames[A.Access] = '' then
          	tempresult.add('access', 'default')
          else
	      	tempresult.add('access', trim(AccessNames[A.Access]));
	      tempresult.add('name', A.safename);

	      if Assigned(A.ArgType) then
	      begin

	          tempresult.add('type', A.ArgType.SafeName);

	      	  // tempresult.add('argtype', A.ArgType.ClassName);

	      end;

	      if A.Value <> '' then
	          tempresult.add('default', A.Value);

		  paramlist.Add(tempresult);

      end;


end;

// For backwards compatibility

procedure TJsonWriter.WriteProcImpl(AProc: TpasProcedureImpl);

var
  i: Integer;
  E,PE  :TPasElement;

begin
  PrepareDeclSection('');
  if AProc.IsClassMethod then
    Add('class ');
  Add(AProc.TypeName + ' ');
  if AProc.Parent.ClassType = TPasClassType then
    Add(AProc.Parent.SafeName + '.');
  Add(AProc.SafeName);
  if Assigned(AProc.ProcType) and (AProc.ProcType.Args.Count > 0) then
    AddProcArgs(AProc.ProcType.Args);
  if Assigned(AProc.ProcType) and
    (AProc.ProcType.ClassType = TPasFunctionType) then
  begin
    Add(': ');
    WriteType(TPasFunctionType(AProc.ProcType).ResultEl.ResultType,False);
  end;
  AddLn(';');
  IncDeclSectionLevel;
  PE:=nil;
  for i := 0 to AProc.Locals.Count - 1 do
    begin
    E:=TPasElement(AProc.Locals[i]);
    if E.InheritsFrom(TpasProcedureImpl) then
      begin
      IncIndent;
      if (i = 0) or not PE.InheritsFrom(TpasProcedureImpl) then
        Addln;
      end;
    WriteElement(E);
    if E.InheritsFrom(TpasProcedureImpl) then
      DecIndent;
    PE:=E;
    end;
  DecDeclSectionLevel;
  AddLn('begin');
  IncIndent;
  if Assigned(AProc.Body) then
    WriteImplBlock(AProc.Body);
  DecIndent;
  AddLn('end;');
  AddLn;
end;

procedure TJsonWriter.WriteProcImpl(AProc: TProcedureBody; IsAsm : Boolean = false);

var
  i: Integer;
  El,PEl : TPasElement;
begin
  PrepareDeclSection('');
  If NotOption(woNoImplementation) then
    begin
    IncDeclSectionLevel;
    PEl:=Nil;
    for i := 0 to aProc.Declarations.Count - 1 do
      begin
      El:=TPasElement(aProc.Declarations[i]);
      if El.InheritsFrom(TpasProcedureImpl) then
        begin
        IncIndent;
        if (PEL=Nil) or not PEL.InheritsFrom(TpasProcedureImpl) then
          AddLn;
        end;
      WriteElement(El);
      if El.InheritsFrom(TpasProcedureImpl) then
        DecIndent;
      Pel:=El;
      end;
    DecDeclSectionLevel;
    end;
  if IsAsm then
    AddLn('asm')
  else
    AddLn('begin');
  If NotOption(woNoImplementation) then
    begin
    IncIndent;
    if Assigned(AProc.Body) then
      WriteImplBlock(AProc.Body);
    DecIndent;
    end;
  AddLn('end;');
  AddLn;
end;

procedure TJsonWriter.WriteProperty(AProp: TPasProperty);
var
  i: Integer;
begin
  if AProp.IsClass then
    Add('class ');
  Add('property ' + AProp.SafeName);
  if AProp.Args.Count > 0 then
  begin
    Add('[');
    for i := 0 to AProp.Args.Count - 1 do
      begin
      if I>0 then Add(',');
      WriteArgument(TpasArgument(AProp.Args[i]));
      end;
      // !!!: Create WriteArgument method and call it here
    Add(']');
  end;
  if Assigned(AProp.VarType) then
  begin
    Add(': ');
    WriteType(AProp.VarType,False);
  end;
  if AProp.IndexValue <> '' then
    Add(' index ' + AProp.IndexValue); 
  if AProp.ReadAccessorName <> '' then
    Add(' read ' + AProp.ReadAccessorName);
  if AProp.WriteAccessorName <> '' then
    Add(' write ' + AProp.WriteAccessorName);
  if AProp.StoredAccessorName <> '' then
    Add(' stored ' + AProp.StoredAccessorName);
  if AProp.DefaultValue <> '' then
    Add(' default ' + AProp.DefaultValue);
  if AProp.IsNodefault then
    Add(' nodefault');
  if AProp.IsDefault then
    Add('; default');
  AddLn(';');
end;

procedure TJsonWriter.WriteImplBlock(ABlock: TpasImplBlock);
var
  i: Integer;
begin
  for i := 0 to ABlock.Elements.Count - 1 do
  begin
    WriteImplElement(TpasImplElement(ABlock.Elements[i]), False);
    if (TpasImplElement(ABlock.Elements[i]).ClassType = TpasImplCommand) then
    begin
      if TpasImplCommand(ABlock.Elements[i]).SemicolonAtEOL then
        AddLn(';')
      else
        AddLn;
    end;
  end;
end;

procedure TJsonWriter.WriteImplElement(AElement: TpasImplElement;  AAutoInsertBeginEnd: Boolean);

begin

  if AElement.ClassType = TpasImplCommand then
    WriteImplCommand(TpasImplCommand(AElement))
  else
  if AElement.ClassType = TpasImplCommands then
    begin
    if AAutoInsertBeginEnd then
    begin
      DecIndent;
      AddLn('begin');
      IncIndent;
    end;
    WriteImplCommands(TpasImplCommands(AElement));
    if AAutoInsertBeginEnd then
    begin
      DecIndent;
      AddLn('end;');
      IncIndent;
    end;
    end
  else if (AElement.ClassType = TpasImplBlock) or (AElement.ClassType = TPasImplBeginBlock) then
    begin
    if AAutoInsertBeginEnd or (AElement.ClassType = TPasImplBeginBlock) then
    begin
      DecIndent;
      AddLn('begin');
      IncIndent;
    end;
    WriteImplBlock(TpasImplBlock(AElement));
    if AAutoInsertBeginEnd or (AElement.ClassType = TPasImplBeginBlock) then
    begin
      DecIndent;
      AddLn('end;');
      IncIndent;
    end;
    end
  else if AElement.ClassType = TpasImplIfElse then
    WriteImplIfElse(TpasImplIfElse(AElement))
  else if AElement.InheritsFrom(TpasImplCaseOf) then
    WriteImplCaseOf(TpasImplCaseOf(aElement))
  else if AElement.ClassType = TPasImplForLoop then
    WriteImplForLoop(TPasImplForLoop(AElement))
  else if AElement.InheritsFrom(TPasImplWhileDo) then
    WriteImplWhileDo(TPasImplWhileDo(AElement))
  else if AElement.InheritsFrom(TPasImplRepeatUntil) then
    WriteImplRepeatUntil(TPasImplRepeatUntil(AElement))
  else if AElement.InheritsFrom(TPasImplTry) then
    WriteImplTryFinallyExcept(TPasImplTry(aElement))
  else if AElement.InheritsFrom(TPasImplRaise) then
    WriteImplRaise(TPasImplRaise(aElement))
  else if AElement.InheritsFrom(TPasImplAssign) then
    WriteImplAssign(TPasImplAssign(aElement))
  else if AElement.InheritsFrom(TPasImplSimple) then
    WriteImplSimple(TPasImplSimple(aElement))
  else if AElement.InheritsFrom(TPasImplExceptOn) then
    WriteImplExceptOn(TPasImplExceptOn(aElement))

  else if AElement.InheritsFrom(TpasImplWithDo) then
    WriteImplWithDo(TpasImplWithDo(aElement))

  else
    raise EPasWriter.CreateFmt('Writing not yet implemented for %s implementation elements',[AElement.ClassName]);
end;

procedure TJsonWriter.WriteImplCommand(ACommand: TpasImplCommand);
begin
  Add(ACommand.Command);
end;

procedure TJsonWriter.WriteImplCommands(ACommands: TpasImplCommands);
var
  i: Integer;
  s: string;
begin
  for i := 0 to ACommands.Commands.Count - 1 do
  begin
    s := ACommands.Commands[i];
    if Length(s) > 0 then
      if (Length(s) >= 2) and (s[1] = '/') and (s[2] = '/') then
        AddLn(s)
      else
        if ACommands.SemicolonAtEOL then
          AddLn(s + ';')
        else
          AddLn(s);
  end;
end;

procedure TJsonWriter.WriteImplIfElse(AIfElse: TpasImplIfElse);

Var
  DoBeginEnd : Boolean;

begin
  Add('if ' + AIfElse.Condition + ' then');
  if Assigned(AIfElse.IfBranch) then
    begin
    AddLn;
    DoBeginEnd:=(AIfElse.IfBranch.ClassType = TpasImplCommands) or
                (AIfElse.IfBranch.ClassType = TpasImplBlock) or
                Assigned(aIfElse.ElseBranch);
    if DoBeginEnd then
      AddLn('begin');
    IncIndent;
    if AIfElse.IfBranch is TPasImplBeginBlock then
       WriteImplBlock(TPasImplBeginBlock(AIfElse.IfBranch))
     else
       WriteImplElement(AIfElse.IfBranch, False);
    DecIndent;
    if DoBeginEnd then
      begin
      if Assigned(AIfElse.ElseBranch) then
        Add('end ')
      else
        AddLn('end;')
      end
    else
      if Assigned(AIfElse.ElseBranch) then
        AddLn;
    end
  else if not Assigned(AIfElse.ElseBranch) then
    AddLn(';')
  else
    AddLn;

  if Assigned(AIfElse.ElseBranch) then
    if AIfElse.ElseBranch.ClassType = TpasImplIfElse then
    begin
      Add('else ');
      WriteImplElement(AIfElse.ElseBranch, True);
    end else
    begin
      AddLn('else');
      IncIndent;
      WriteImplElement(AIfElse.ElseBranch, True);
{      if (not Assigned(AIfElse.Parent)) or
        (AIfElse.Parent.ClassType <> TpasImplIfElse) or
        (TpasImplIfElse(AIfElse.Parent).IfBranch <> AIfElse) then
        AddLn(';');}
      DecIndent;
    end;
end;

procedure TJsonWriter.WriteImplCaseStatement(ACaseStatement: TPasImplCaseStatement;AAutoInsertBeginEnd:boolean=true);
var
  i: Integer;
begin
  for i := 0 to ACaseStatement.Expressions.Count - 1 do
     begin
       if i>0 then add(', ');
       add(GetExpr(TPasExpr(ACaseStatement.Expressions[i])))
     end;
  add(': ');
  IncIndent;
  //JC: If no body is assigned, omit the whole block
  if assigned(ACaseStatement.Body) then
    begin
      if AAutoInsertBeginEnd then
        begin
          addLn('begin');
          IncIndent;
        end;
      //JC: if the body already is a begin-end-Block, the begin of that block is omitted
      if ACaseStatement.Body is TPasImplBeginBlock then
         WriteImplBlock(TPasImplBeginBlock(ACaseStatement.Body))
       else
         WriteImplElement(ACaseStatement.Body,false);
      if AAutoInsertBeginEnd then
        begin
          DecIndent;
          Add('end'); //JC: No semicolon or Linefeed here !
          // Otherwise there would be a problem with th else-statement.
        end;
    end;
  DecIndent;
end;

procedure TJsonWriter.WriteImplCaseOf(ACaseOf: TpasImplCaseOf);
var
  i: Integer;

begin
  Add('case %s of', [GetExpr(ACaseOf.CaseExpr)]);
  IncIndent;
  for i := 0 to ACaseOf.Elements.Count - 1 do
  begin
    if TPasElement(ACaseOf.Elements[i]) is TPasImplCaseStatement then
      begin
        if i >0 then
          AddLn(';')
        else
          AddLn;
        WriteImplCaseStatement(TPasImplCaseStatement(ACaseOf.Elements[i]),True);
      end;
  end;
  if assigned(ACaseOf.ElseBranch) then
    begin
      AddLn;
      AddLn('else');
      IncIndent;
      WriteImplBlock(ACaseOf.ElseBranch);
      DecIndent;
    end
  else
    AddLn(';');
  DecIndent;
  AddLn('end;');
end;


procedure TJsonWriter.WriteImplRepeatUntil(aRepeatUntil: TPasImplRepeatUntil);

begin
  Addln('repeat');
  with aRepeatUntil do
    begin
    IncIndent;
    WriteImplBlock(aRepeatUntil);
    DecIndent;
    AddLn('until %s;',[GetExpr(ConditionExpr)]);
    end;
end;

procedure TJsonWriter.WriteImplTryFinallyExcept(aTry: TPasImplTry);
begin
  Addln('try');
  with aTry do
    begin
    IncIndent;
    WriteImplBlock(aTry);
    DecIndent;
    if aTry.FinallyExcept is TPasImplTryFinally then
      AddLn('finally')
    else
      AddLn('except');
    IncIndent;
    WriteImplBlock(aTry.FinallyExcept);
    DecIndent;
    if Assigned(aTry.ElseBranch) then
      begin
      AddLn('else');
      IncIndent;
      WriteImplBlock(aTry.ElseBranch);
      DecIndent;
      end;
    end;
  AddLn('end;')
end;

procedure TJsonWriter.WriteImplRaise(aRaise: TPasImplRaise);
begin
  if assigned(aRaise.ExceptObject) then
    begin
    Add('raise %s',[GetExpr(aRaise.ExceptObject)]);
    if aRaise.ExceptAddr<>Nil then
      Add(' at %s',[GetExpr(aRaise.ExceptAddr)]);
    end
  else
    Add('raise');
  Addln(';');
end;

procedure TJsonWriter.WriteImplAssign(aAssign: TPasImplAssign);

begin
  AddLn('%s %s %s;',[GetExpr(aAssign.left),AssignKindNames[aAssign.Kind],GetExpr(aAssign.right)]);
end;

procedure TJsonWriter.WriteImplSimple(aSimple: TPasImplSimple);
begin
  Addln('%s;',[GetExpr(aSimple.expr)]);
end;

procedure TJsonWriter.WriteImplExceptOn(aOn: TPasImplExceptOn);
begin
  Addln('On %s : %s do',[aOn.VarEl.SafeName,aOn.TypeEl.SafeName]);
  if Assigned(aOn.Body) then
    WriteImplElement(aOn.Body,True);
end;

procedure TJsonWriter.WriteImplWithDo(aWith: TpasImplWithDo);
var
  ind : integer;
  Expr : string;
begin
  With aWith do
    begin
    for ind:=0 to Expressions.Count-1 do
      begin
        Expr:=Expr+GetExpr(TPasExpr(Expressions[ind]));
        if ind<Expressions.Count-1 then
          Expr:=Expr+',';
      end;
    Add('With %s do',[Expr]);
    if assigned(Body) then
      begin
        AddLn;
        IncIndent;
        WriteImplElement(Body, True);
        DecIndent;
        if (Body.InheritsFrom(TpasImplBlock)) and
           (Body.InheritsFrom(TpasImplCommands)) then
          AddLn(';');
      end
    else
      AddLn(';');
    end;

end;

procedure TJsonWriter.wrt(const s: string);
begin
  Add(s);
end;

procedure TJsonWriter.wrtln(const s: string);
begin
  AddLn(s);
end;

procedure TJsonWriter.wrtln;
begin
  Addln;
end;

function TJsonWriter.GetExpr(E : TPasExpr) : String;

begin
  Result:=E.GetDeclaration(True);
end;

procedure TJsonWriter.WriteImplForLoop(AForLoop: TPasImplForLoop);

Const
  ToNames : Array[Boolean] of string = ('to','downto');

begin
  With aForLoop do
    begin
    If LoopType=ltIn then
      Add('for %s in %s do',[GetExpr(VariableName),GetExpr(StartExpr)])
    else
      Add('for %s:=%s %s %s do',[GetExpr(VariableName),GetExpr(StartExpr),
                                   ToNames[Down],GetExpr(EndExpr)]);
    if assigned(Body) then
      begin
        AddLn;
        IncIndent;
        WriteImplElement(Body, True);
        DecIndent;
        if (Body is TpasImplBlock) and
           (Body is TpasImplCommands) then
          AddLn(';');
      end
    else
      AddLn(';');
    end;
end;


procedure TJsonWriter.WriteImplWhileDo(aWhileDo: TPasImplWhileDo);

begin
  With aWhileDo do
    begin
    Add('While %s do',[GetExpr(ConditionExpr)]);
    if assigned(Body) then
      begin
        AddLn;
        IncIndent;
        WriteImplElement(Body, True);
        DecIndent;
        if (Body.InheritsFrom(TpasImplBlock)) and
           (Body.InheritsFrom(TpasImplCommands)) then
          AddLn(';');
      end
    else
      AddLn(';');
    end;
end;

procedure TJsonWriter.IncIndent;
begin
  Indent := Indent + FIndentStep;
end;

procedure TJsonWriter.DecIndent;
begin
  if (Length(Indent)<FIndentSize) then
    raise EPasWriter.Create('Internal indent error');
  SetLength(Indent, Length(Indent) - FIndentSize);
end;

procedure TJsonWriter.IncDeclSectionLevel;
var
  El: PDeclSectionStackElement;
begin
  New(El);
  DeclSectionStack.Add(El);
  El^.LastDeclSection := CurDeclSection;
  El^.LastIndent := Indent;
  CurDeclSection := '';
end;

procedure TJsonWriter.DecDeclSectionLevel;
var
  El: PDeclSectionStackElement;
begin
  if DeclSectionStack.Count=0 then
    raise EPasWriter.Create('Internal section indent error');
  El := PDeclSectionStackElement(DeclSectionStack[DeclSectionStack.Count - 1]);
  DeclSectionStack.Delete(DeclSectionStack.Count - 1);
  CurDeclSection := El^.LastDeclSection;
  Indent := El^.LastIndent;
  Dispose(El);
end;

procedure TJsonWriter.PrepareDeclSection(const ADeclSection: string);
begin
  if Not SameText(ADeclSection,CurDeclSection) then
  begin
    if CurDeclsection <> '' then
      begin
      DecIndent;
      end;
    if ADeclSection <> '' then
    begin
      AddLn(ADeclSection);
      IncIndent;
    end;
    CurDeclSection := ADeclSection;
  end;
end;

procedure TJsonWriter.PrepareDeclSectionInStruct(const ADeclSection: string);

begin
  if Not SameText(ADeclSection,CurDeclSection) then
  begin
    if ADeclSection <> '' then
    begin
      DecIndent;
      AddLn(ADeclSection);
      IncIndent;
    end;
    CurDeclSection := ADeclSection;
  end;
end;

procedure TJsonWriter.SetForwardClasses(AValue: TStrings);
begin
  if FForwardClasses=AValue then Exit;
  FForwardClasses.Assign(AValue);
end;

procedure TJsonWriter.SetIndentSize(AValue: Integer);
begin
  if AValue=FIndentSize then exit;
  if AValue<0 then
    AValue:=0;
  FIndentSize:=AValue;
  FIndentStep:=StringOfChar(' ',aValue);
end;

function TJsonWriter.CheckUnitAlias(const AUnitName: String): String;
begin
  if Assigned(FOnUnitAlias) then
    Result := FOnUnitAlias(AUnitName)
  else
    Result := AUnitName;
end;

function TJsonWriter.HasOption(aOption: TJsonWriterOption): Boolean;
begin
  Result:=(aOption in FOptions)
end;

function TJsonWriter.NotOption(aOption: TJsonWriterOption): Boolean;
begin
  Result:=Not (aOption in FOptions)
end;

function TJsonWriter.PostProcessLine(S: String): String;
begin
  Result:=S;
  if HasOption(woAddLineNumber) or HasOption(woAddSourceLineNumber) then
    Result:=GetLineNumberComment+Result;
end;

function TJsonWriter.GetLineNumberComment: String;
begin

end;

procedure TJsonWriter.ResetIndent;

Var
  I : integer;
  E : PDeclSectionStackElement;

begin
  CurDeclSection:='';
  Indent:='';
  For I:=DeclSectionStack.Count-1 downto 0 do
    begin
    E:=PDeclSectionStackElement(DeclSectionStack[i]);
    Dispose(E);
    end;
  DeclSectionStack.Clear;
end;

procedure WritePasFile(AElement: TPasElement; const AFilename: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFilename, fmCreate or fmShareDenyNone);
  try
    WritePasFile(AElement, Stream);
  finally
    Stream.Free;
  end;
end;

procedure WritePasFile(AElement: TPasElement; AStream: TStream);
var
  Writer: TJsonWriter;
begin
  Writer := TJsonWriter.Create(AStream);
  try
    Writer.WriteElement(AElement);
  finally
    Writer.Free;
  end;
end;

end.
