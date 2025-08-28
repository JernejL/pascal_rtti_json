program rttiprocessor;

{$apptype console}
{$mode objfpc}{$H+}

uses
	{$IFDEF UNIX}{$IFDEF UseCThreads}
	cthreads,
	{$ENDIF}{$ENDIF}
	Interfaces, // this includes the LCL widgetset
	Forms, windows, dialogs, u_rtti_main, classes, sysutils;

{$R *.res}

procedure logfunction_cmdl(const Msg: String);
begin

	Writeln(StdErr, 'PasParserLogHandler: ' + msg);

end;

var
	pr: TPasRewriteApplication;
	newtxt, InputFilename: String;
    outstream: TStringStream;
begin

    if ParamCount = 2 then begin

		pr:= TPasRewriteApplication.create();
		pr.onlogcallback:= @logfunction_cmdl;

        InputFilename:= ParamStr(1);

		newtxt:= pr.DoRun( ParamStr(1) );

        if ParamStr(2) = '-o' then begin

            Writeln(StdOut, newtxt);

		end else begin

	        outstream:= TStringStream.create();

	        outstream.WriteString(newtxt);

	        outstream.SaveToFile( ParamStr(2) );

	        freeandnil(outstream);

		end;

        exit;

	end;

    Writeln(StdOut, 'rttiprocessor by Jernej L.');
    Writeln(StdOut, '');
    Writeln(StdOut, '<pasfile> <outputjsonfile>');
    Writeln(StdOut, '<outputjsonfile> can be also -o to output into stdout.');
    Writeln(StdOut, 'errors go to stderr.');

end.

