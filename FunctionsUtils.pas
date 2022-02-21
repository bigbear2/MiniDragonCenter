unit FunctionsUtils;

interface

uses
  Windows, Classes, SysUtils, Controls, Forms, Graphics, Dialogs, StrUtils, dbugintf, Variants, Registry;

function RoundEx(aValue: double): double;

function MsgBox(Text: string; const TypeMsg: TMsgDlgType = mtError; const Confirm: boolean = False): boolean;

function GetLocalComputerName: string;

function LoadString(Filename: string): string;

function SaveString(Filename: string; Value: string): boolean;

function ReplaceInvalidFileNameChars(const aFileName: string; const aReplaceWith: char = '_'): string;

function AlphaBase(Num: integer): string; overload;

function WinExecAndWait32(Filename: string; Visibility: integer; UseFilenamePath: boolean): cardinal;

procedure WaitDelay(dwMilliseconds: longint);

procedure Debug(Value: variant; const Level: TDebugLevel = dlInformation);

function InDebugMode(): boolean;

function IIF(Condition: boolean; IFTrue: variant; IFFalse: variant): variant;

function Explode(Delimiter: char; Value: string; const TrimSpace: boolean = False): TStringList;

function Implode(Delimiter: char; StringList: TStrings): string;

function ReplaceWords(const Text, SearchText, ReplaceText: string; caseSensitive: boolean = False): string;

function FindWord(pattern, Text: string; caseSensitive: boolean = False; startAt: integer = 1): integer;

function TerminateProcessByID(ProcessID: cardinal): boolean;

function GetDosOutput(CommandLine: string; Work: string = 'C:\'; ShowConsole: boolean = False): string;

function RunOnStartup(Title: string; Filename: string; RemoveKey: boolean; DefaultRootKey: HKEY = HKEY_CURRENT_USER): boolean;

var
  ApplicationDir: string;

implementation

var
  FPid: cardinal;

const
  separators: set of char = [' ', '.', ',', '?', '!', #13, #10, #09, '(', ')'];

function InDebugMode(): boolean;
begin
  Result :=
{$ifopt D+}true{$else}
    False
{$endif}
  ;
end;

function Explode(Delimiter: char; Value: string; const TrimSpace: boolean = False): TStringList;
begin

  Result := TStringList.Create;
  Result.Delimiter := Delimiter;
  Result.StrictDelimiter := True;
  Result.DelimitedText := Value;
  if TrimSpace then
    Result.Text := Trim(Result.Text);
end;

function Implode(Delimiter: char; StringList: TStrings): string;
begin

  StringList.Delimiter := Delimiter;
  StringList.QuoteChar := #0;
  Result := StringList.DelimitedText;
end;


procedure Debug(Value: variant; const Level: TDebugLevel = dlInformation);
var
  S: string;
begin
  S := VarToStr(Value);
  SendDebugEx(s, Level);
end;

function IIF(Condition: boolean; IFTrue: variant; IFFalse: variant): variant;
begin
  if Condition then
    Result := IFTrue
  else
    Result := IFFalse;
end;

procedure WaitDelay(dwMilliseconds: longint);
var
  iStart, iStop: DWORD;
begin
  iStart := GetTickCount;
  repeat
    iStop := GetTickCount;
    Application.ProcessMessages;
    Sleep(1);
  until (iStop - iStart) >= dwMilliseconds;
end;

function AlphaBase(Num: integer): string; overload;
begin
  Result := '';
  if Num < 1 then
    Exit;
  Result := chr(((Num - 1) mod 26) + 65);
  if Num > 26 then
    Result := AlphaBase((Num - 1) div 26) + Result;
end;

function AlphaBase(s: string): integer; overload;
var
  i: integer;
begin
  Result := 0;
  i := Length(s);
  if i < 1 then
    exit;
  Result := Ord(s[i]) - 64;
  if i > 1 then
    Result := AlphaBase(LeftStr(s, i - 1)) * 26 + Result;
end;

function SaveString(Filename: string; Value: string): boolean;
begin
  with TStringList.Create do
  begin
    Text := Value;
    SaveToFile(Filename);
    Free;
  end;
  Result := FileExists(Filename);
end;

function LoadString(Filename: string): string;
begin
  with TStringList.Create do
  begin
    if FileExists(Filename) then
      LoadFromFile(Filename);
    Result := Text;
    Free;
  end;
end;

function GetLocalComputerName: string;
var
  c1: DWORD;
  arrCh: array[0..MAX_PATH] of char;
begin
  c1 := MAX_PATH;
  GetComputerName(arrCh, c1);
  if c1 > 0 then
    Result := arrCh
  else
    Result := '';
end;

function MsgBox(Text: string; const TypeMsg: TMsgDlgType = mtError; const Confirm: boolean = False): boolean;
var
  Value: boolean;
begin
  Value := Confirm;
  if TypeMsg = mtConfirmation then
    Value := True;

  if Value then
    Result := MessageDlg(PChar(Text), TypeMsg, [mbYes, mbNo], 0) = mrYes
  else
    Result := MessageDlg(PChar(Text), TypeMsg, [mbOK], 0) = mrOk;

end;

function RoundEx(aValue: double): double;
begin
  Result := Round(aValue * 100) / 100;
end;


function ReplaceInvalidFileNameChars(const aFileName: string; const aReplaceWith: char = '_'): string;
var
  i: integer;
begin
  Result := aFileName;
  for i := Low(Result) to High(Result) do
  begin
    //  if not IsValidFileNameChar(Result[i]) then
    Result[i] := aReplaceWith;
  end;
end;

function WinExecAndWait32(Filename: string; Visibility: integer; UseFilenamePath: boolean): cardinal;
var
  zAppName: array [0 .. 1024] of char;
  zCurDir: array [0 .. 255] of char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPCopy(zAppName, Filename);

  if not UseFilenamePath then
    GetDir(0, WorkDir)
  else
    WorkDir := ExtractFilePath(Filename);

  StrPCopy(zCurDir, WorkDir);
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);

  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(nil, zAppName, { pointer to command line string }
    nil, { pointer to process security attributes }
    nil, { pointer to thread security attributes }
    False, { handle inheritance flag }
    CREATE_NEW_CONSOLE or { creation flags }
    NORMAL_PRIORITY_CLASS, nil, { pointer to new environment block }
    nil, { pointer to current directory name }
    StartupInfo, { pointer to STARTUPINFO }
    ProcessInfo) then
    Result := 0 { pointer to PROCESS_INF }
  else
  begin

    WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end;
end;


procedure ProcessVariant(v: variant);
begin

  // Example of how to determine the contents of a Variant type:
  case varType(v) of
    varEmpty:
      Writeln('Empty');
    varNull:
      Writeln('Null');
    varSingle:
      Writeln('Datatype: Single');
    varDouble:
      Writeln('Datatype: Double');
    varDecimal:
      Writeln('Datatype: Decimal');
    varCurrency:
      Writeln('Datatype: Currency');
    varDate:
      Writeln('Datatype: Date');
    varOleStr:
      Writeln('Datatype: UnicodeString');
    varStrArg:
      Writeln('Datatype: COM-compatible string');
    varString:
      Writeln('Datatype: Pointer to a dynamic string');
    varDispatch:
      Writeln('Datatype: Pointer to an Automation object');
    varBoolean:
      Writeln('Datatype: Wordbool');
    varVariant:
      Writeln('Datatype: Variant');
    varUnknown:
      Writeln('Datatype: unknown');
    varShortInt:
      Writeln('Datatype: ShortInt');
    varSmallint:
      Writeln('Datatype: Smallint');
    varInteger:
      Writeln('Datatype: Integer');
    varInt64:
      Writeln('Datatype: Int64');
    varByte:
      Writeln('Datatype: Byte');
    varWord:
      Writeln('Datatype: Word');
    varLongWord:
      Writeln('Datatype: LongWord');
    varQWord:
      Writeln('Datatype: QWord');
    varError:
      Writeln('ERROR determining variant type');
    else
      Writeln('Unable to determine variant type');
  end;
end;

function FindWord(pattern, Text: string; caseSensitive: boolean = False; startAt: integer = 1): integer;
var
  offset, endOfPattern: integer;
begin
  Result := 0;
  if Length(Text) = 0 then Exit;
  if Length(pattern) = 0 then
  begin
    { By definition a pattern of length 0 is always found. }
    Result := 1;
    Exit;
  end;
  if not caseSensitive then
  begin
    pattern := AnsiLowerCase(pattern);
    Text := AnsiLowerCase(Text);
  end; { If }
  endOfPattern := startAt + Length(pattern);
  for offset := startAt to Length(Text) - Length(pattern) + 1 do
  begin
    if pattern[1] = Text[offset] then
    begin
      if ((offset = 1) or not IsCharAlphaNumeric(Text[offset - 1])) and ((endOfPattern > Length(Text)) or not
        IsCharAlphaNumeric(Text[endOfPattern])) and (StrLComp(@Text[offset], @pattern[1], Length(pattern)) = 0) then
      begin
        Result := offset;
        Exit;
      end;
    end; { If }
    Inc(endOfPattern);
  end; { for }
end; { FindWord }

function ReplaceWords(const Text, SearchText, ReplaceText: string; caseSensitive: boolean = False): string;
var
  Offset: integer;
begin
  Result := Text;
  Offset := FindWord(SearchText, Result, caseSensitive);
  while Offset > 0 do
  begin
    Delete(Result, Offset, SearchText.Length);
    Inc(Offset, -1);
    Result.Insert(Offset, ReplaceText);
    Inc(Offset, ReplaceText.Length);
    Offset := FindWord(SearchText, Result, True, Offset);
  end;
end;


function TerminateProcessByID(ProcessID: cardinal): boolean;
var
  hProcess: THandle;
begin
  Result := False;
  hProcess := OpenProcess(PROCESS_TERMINATE, False, ProcessID);
  if hProcess > 0 then
    try
      Result := Win32Check(Windows.TerminateProcess(hProcess, 0));
    finally
      CloseHandle(hProcess);
    end;
end;


function GetDosOutput(CommandLine: string; Work: string = 'C:\'; ShowConsole: boolean = False): string;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: boolean;
  Buffer: array[0..255] of ansichar;
  BytesRead: cardinal;
  WorkDir: string;
  Handle: boolean;
begin
  if FPid > 0 then TerminateProcessByID(FPid);

  Screen.Cursor := crHourGlass;

  with SA do
  begin
    nLength := SizeOf(SA);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    with SI do
    begin
      FillChar(SI, SizeOf(SI), 0);
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      if ShowConsole then  wShowWindow := SW_SHOWNORMAL;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;
    WorkDir := Work;
    Handle := CreateProcess(nil, PChar('cmd.exe /C ' + CommandLine), nil, nil, True, 0, nil, PChar(WorkDir), SI, PI);
    CloseHandle(StdOutPipeWrite);

    FPid := PI.hProcess;

    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            Result := Result + Buffer;
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(PI.hProcess, INFINITE);
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;

  Screen.Cursor := crDefault;
end;

function RunOnStartup(Title: string; Filename: string; RemoveKey: boolean; DefaultRootKey: HKEY = HKEY_CURRENT_USER): boolean;
var
  Reg: TRegistry;
  Exist: boolean;
begin
  Title := Title.Replace(' ', '', [rfReplaceAll]);
  //HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Run
  //HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Run
  Result := False;
  try
    reg := TRegistry.Create(KEY_READ or KEY_WRITE);
    with Reg do
    begin
      try
        RootKey := DefaultRootKey;

        if OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', False) then
        begin
          Exist := ValueExists(Title);

          if RemoveKey then
          begin
            if Exist then Result := Reg.DeleteValue(Title);
          end
          else
          begin
            if not Exist then Reg.WriteString(Title, Filename);
            Result := ValueExists(Title);
          end;

        end;

      finally
        CloseKey;
        Free;
      end;
    end;
  finally

  end;

end;

initialization
  FPid := 0;
  ApplicationDir := ExtractFilePath(ParamStr(0));

end.
