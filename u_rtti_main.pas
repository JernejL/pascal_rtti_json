unit u_rtti_main;

{$mode Delphi}

// compile with AT LEAST lazarus 4 fpc 3.3.1

// example resources for pastree & pparser:
// example 1: https://github.com/fpc/FPCSource/blob/fd677978e7661b22a15fb3a9bce3f5c4b2ae8134/utils/fpdoc/unitdiff.pp#L61
// example 2: https://fossies.org/linux/misc/fpcbuild-3.2.2.tar.gz/fpcbuild-3.2.2/fpcsrc/packages/fcl-passrc/src/pasuseanalyzer.pas


interface

uses
	Classes, SysUtils, Pscanner, PParser, PasTree, outjsonwrite, strutils;

type

   TLogfunction = procedure(Const Msg : String);

   { TSimpleEngine }

   TSimpleEngine = class(TPasTreeContainer)
   public
     function CreateElement(AClass: TPTreeElement; const AName: String;
       AParent: TPasElement; AVisibility: TPasMemberVisibility;
       const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
       override;
     function FindElement(const AName: String): TPasElement; override;
   end;

   { TPasRewriteApplication }

   TPasRewriteApplication  = Class
   Private
     FDefines : TStrings;
     FForwardClasses,
     FExtraUnits: string;
     Procedure PasParserLogHandler(Sender : TObject; Const Msg : String);
     function GetModule(InputFileName : string): TPasModule;
     function WriteModule(M: TPasModule): string;
   Protected
     function ParseOptions : Boolean;
   Public
     onlogcallback: TLogfunction;
     Constructor Create();
     Destructor Destroy; override;
     function DoRun(InputFilename: string): string;
   end;


implementation

{ TSimpleEngine }

function TSimpleEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  { dummy implementation, see TFPDocEngine.FindElement for a real example }
  Result := nil;
end;


function TPasRewriteApplication.ParseOptions : Boolean;
begin
  Result:=False;

end;

function TPasRewriteApplication.DoRun(InputFilename: string): string;
Var
  M: TPasModule;
begin

  result:= '';

  ParseOptions();

  M:=GetModule(InputFilename);

  if M=Nil then
    exit;
  try
    result:= WriteModule(M);
  finally
    M.Free;
  end;
end;

constructor TPasRewriteApplication.Create();
begin

	inherited Create();
	FDefines:=TStringList.Create;
    onlogcallback:= nil;

end;

destructor TPasRewriteApplication.Destroy;
begin
FreeAndNil(FDefines);
inherited Destroy;
end;

{ TPasRewriteApplication }

procedure TPasRewriteApplication.PasParserLogHandler(Sender: TObject; const Msg: String);
begin

  if assigned(onlogcallback) then onlogcallback(msg);

end;

function TPasRewriteApplication.GetModule(InputFileName: string): TPasModule;

Var
  SE : TSimpleEngine;
  FileResolver: TFileResolver;
  Parser: TPasParser;
  Scanner: TPascalScanner;

var
  s: String;

begin
  try
    Result := nil;
    FileResolver := nil;
    Scanner := nil;
    Parser := nil;
    SE:=TSimpleEngine.Create;

    se.OnLog:= PasParserLogHandler;

    try
      FileResolver := TFileResolver.Create;
      FileResolver.UseStreams:=True;

      Scanner := TPascalScanner.Create(FileResolver);
      Scanner.Options:= [ po_keepclassforward, po_AsmWhole, po_CAssignments, po_IgnoreUnknownResource ]; // po_IgnoreUnknownResource ???

      SCanner.LogEvents:= SE.ScannerLogEvents;
      SCanner.OnLog:= SE.Onlog;

      Scanner.AddDefine('FPK');
      Scanner.AddDefine('FPC');

      Scanner.AddDefine('RTTI_WORKER');

      For S in FDefines do
        Scanner.AddDefine(S);

      // TargetOS
      s := UpperCase('linux');
      Scanner.AddDefine(s);

      if s = 'LINUX' then
        Scanner.AddDefine('UNIX')
      else if s = 'FREEBSD' then
      begin
        Scanner.AddDefine('BSD');
        Scanner.AddDefine('UNIX');
      end else if s = 'NETBSD' then
      begin
        Scanner.AddDefine('BSD');
        Scanner.AddDefine('UNIX');
      end else if s = 'SUNOS' then
      begin
        Scanner.AddDefine('SOLARIS');
        Scanner.AddDefine('UNIX');
      end else if s = 'GO32V2' then
        Scanner.AddDefine('DPMI')
      else if s = 'BEOS' then
        Scanner.AddDefine('UNIX')
      else if s = 'QNX' then
        Scanner.AddDefine('UNIX')
      else if s = 'AROS' then
        Scanner.AddDefine('HASAMIGA')
      else if s = 'MORPHOS' then
        Scanner.AddDefine('HASAMIGA')
      else if s = 'AMIGA' then
        Scanner.AddDefine('HASAMIGA');

      // TargetCPU
      s := UpperCase('i386');
      Scanner.AddDefine('CPU'+s);
      if (s='X86_64') then
        Scanner.AddDefine('CPU64')
      else
        Scanner.AddDefine('CPU32');

      Parser := TPasParser.Create(Scanner, FileResolver, SE);
      // InputFilename := '';

      Parser.LogEvents:=SE.ParserLogEvents;
      Parser.OnLog:=SE.Onlog;

      Scanner.SetCompilerMode('OBJFPC');
      Scanner.SetCompilerMode('DELPHI');

      if InputFilename = '' then
        raise Exception.Create(SErrNoSourceGiven);

      FileResolver.AddIncludePath(ExtractFilePath(InputFileName));

      scanner.CurrentModeSwitches:= scanner.CurrentModeSwitches + [ msAdvancedRecords, msTypeHelpers ]; // msAdvancedRecords
      Scanner.OpenFile(InputFilename);

      Parser.Options:= Parser.Options + [ po_AsmWhole, po_KeepClassForward, po_CAssignments];
      Parser.ParseMain(Result);

    finally
      Parser.Free;
      Scanner.Free;
      FileResolver.Free;
      SE.Free;
    end;
  except
    on E : EParserError do
      begin

        se.OnLog(self, format('%s on line: %d column %d file %s', [ E.message, E.row, E.column, E.filename ]));
      end;
    on Ex : Exception do
      begin
      	se.OnLog(self, 'exception: ' + Ex.Message);
      end;
  end;
end;

function TPasRewriteApplication.WriteModule(M: TPasModule): string;
Var
  OutStream : TStringStream;
  PasWriter : TJsonWriter;
begin
  PasWriter:= Nil;

  OutStream:= TStringStream.Create();

  try

     PasWriter:=TJsonWriter.Create(OutStream);
     PasWriter.Options:= [];
     PasWriter.ExtraUnits:= FExtraUnits;

     PasWriter.IndentSize:= 0;

     PasWriter.ForwardClasses.CommaText:=FForwardClasses;

     PasWriter.WriteModule(M);

  except
    on E: exception do begin
	    // form1.lst_errors.lines.Add('Error in writing: ' + e.Message);

        if assigned(onlogcallback) then onlogcallback('Error in writing: ' + e.Message);

	  end;
  end;

  OutStream.seek(0, soFromBeginning);

  result:= OutStream.DataString;

  PasWriter.Free;
  OutStream.Free;

end;

end.

