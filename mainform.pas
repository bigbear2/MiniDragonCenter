unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Spin, FunctionsUtils, ShellApi, jwatlhelp32, jwapsapi, IniFiles;

type
  DWORDLONG = uint64;

  PMemoryStatusEx = ^TMemoryStatusEx;

  TMemoryStatusEx = packed record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: DWORDLONG;
    ullAvailPhys: DWORDLONG;
    ullTotalPageFile: DWORDLONG;
    ullAvailPageFile: DWORDLONG;
    ullTotalVirtual: DWORDLONG;
    ullAvailVirtual: DWORDLONG;
    ullAvailExtendedVirtual: DWORDLONG;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    btnApply: TButton;
    btnStatus: TButton;
    btnTrimMemory: TButton;
    Button1: TButton;
    cbbMode: TComboBox;
    chkTrimMemoryEvery: TCheckBox;
    chkTrimMemoryWhenOver: TCheckBox;
    chkTrimMemoryWhenProgramStart: TCheckBox;
    edtAdvanced1: TEdit;
    edtAdvanced10: TEdit;
    edtAdvanced11: TEdit;
    edtAdvanced12: TEdit;
    edtAdvanced2: TEdit;
    edtAdvanced3: TEdit;
    edtAdvanced4: TEdit;
    edtAdvanced5: TEdit;
    edtAdvanced6: TEdit;
    edtAdvanced7: TEdit;
    edtAdvanced8: TEdit;
    edtAdvanced9: TEdit;
    gbMemoryOptions: TGroupBox;
    ilMemory: TImageList;
    Image1: TImage;
    Image2: TImage;
    lblTotalMem: TLabel;
    lblAdvanced1: TLabel;
    lblAdvanced10: TLabel;
    lblAdvanced11: TLabel;
    lblAdvanced12: TLabel;
    lblAdvanced2: TLabel;
    lblAdvanced3: TLabel;
    lblAdvanced4: TLabel;
    lblAdvanced5: TLabel;
    lblAdvanced6: TLabel;
    lblAdvanced7: TLabel;
    lblAdvanced8: TLabel;
    lblAdvanced9: TLabel;
    lblUsageMem: TLabel;
    lblPercentMem: TLabel;
    mmoLog: TMemo;
    pcAdvanced: TPageControl;
    pcMain: TPageControl;
    pbMemory: TProgressBar;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    tsAbout: TTabSheet;
    tmrMemoryStatus: TTimer;
    tsMemory: TTabSheet;
    tsOptions: TTabSheet;
    tbAdvanced1: TTrackBar;
    tbAdvanced10: TTrackBar;
    tbAdvanced11: TTrackBar;
    tbAdvanced12: TTrackBar;
    tbAdvanced2: TTrackBar;
    tbAdvanced3: TTrackBar;
    tbAdvanced4: TTrackBar;
    tbAdvanced5: TTrackBar;
    tbAdvanced6: TTrackBar;
    tbAdvanced7: TTrackBar;
    tbAdvanced8: TTrackBar;
    tbAdvanced9: TTrackBar;
    tsGPU: TTabSheet;
    tsCPU: TTabSheet;
    tbValue: TTrackBar;
    tsLog: TTabSheet;
    tsGeneral: TTabSheet;
    tiTray: TTrayIcon;
    procedure btnApplyClick(Sender: TObject);
    procedure btnStatusClick(Sender: TObject);
    procedure btnTrimMemoryClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbbModeChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tbAdvanced1Change(Sender: TObject);
    procedure tmrMemoryStatusTimer(Sender: TObject);
  private
    FActivated: boolean;
  public
    procedure LoadOptions();
    procedure SaveOptions();
    procedure Status();
    procedure Basic();
    procedure Auto();
    procedure Advanced();
    procedure MemoryUsage();
    procedure TrimMemory(const ShowMessage: boolean = False);
  end;

var
  frmMain: TfrmMain;
  IniFilename: string;
  AppDir: string;
  MSIApp: string;
  DriveLetter: string;
  FPid: cardinal;

const
  APP_TITLE = 'MiniDragonCenter v2022.1';

function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL; stdcall; external kernel32;

implementation

{$R *.lfm}

{ TfrmMain }

procedure Debug(Value: string);
begin
  OutputDebugString(PChar(Value));
  frmMain.mmoLog.Lines.Add(Value);
  SendMessage(frmMain.mmoLog.Handle, EM_LINESCROLL, 0, frmMain.mmoLog.Lines.Count);
end;

function DrawTextCentered(Canvas: TCanvas; const R: TRect; S: string): integer;
var
  DrawRect: TRect;
  DrawFlags: cardinal;
  DrawParams: TDrawTextParams;
begin
  DrawRect := R;
  DrawFlags := DT_END_ELLIPSIS or DT_NOPREFIX or DT_WORDBREAK or DT_EDITCONTROL or DT_CENTER;
  DrawText(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags or DT_CALCRECT);
  DrawRect.Right := R.Right;
  if DrawRect.Bottom < R.Bottom then
    OffsetRect(DrawRect, 0, (R.Bottom - DrawRect.Bottom) div 2)
  else
    DrawRect.Bottom := R.Bottom;
  ZeroMemory(@DrawParams, SizeOf(DrawParams));
  DrawParams.cbSize := SizeOf(DrawParams);
  DrawTextEx(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags, @DrawParams);
  Result := DrawParams.uiLengthDrawn;
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

function CalcolatePercent(N1: integer; N2: integer): double;

begin
  Result := N1 - N2;
  Result := Result / N1;
  Result := 100 - Round(Result * 100);
end;

procedure TrimAppMemorySize(PName: string; IDP: DWORD);
var
  MainHandle: THandle;
begin
  try
    MainHandle := OpenProcess(PROCESS_ALL_ACCESS, False, IDP);
    SetProcessWorkingSetSize(MainHandle, $FFFFFFFF, $FFFFFFFF);
    CloseHandle(MainHandle);
    Debug(PName + ' Trimmed Memory Successfull!');
  except
    Debug(PName + ' Failed to trim Memory!');
  end;
  Application.ProcessMessages;
end;



function GetDosOutput(CommandLine: string; Work: string = 'C:\'): string;
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
  Result := '';
  if not FileExists(MSIApp) then
  begin
    Messaggio('MsiFanControl.exe non found!');
    Exit;
  end;

  if FPid > 0 then TerminateProcessByID(FPid);

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
end;

procedure TfrmMain.MemoryUsage();
var
  MemStatus: TMemoryStatusEx;

  MemTot, MemUsg, MemAvi: cardinal;
begin

  // initialize the structure
  FillChar(MemStatus, SizeOf(MemStatus), 0);
  MemStatus.dwLength := SizeOf(MemStatus);
  // check return code for errors
  Win32Check(GlobalMemoryStatusEx(MemStatus));

  //Debug(Format('dwLength: %d', [MemStatus.dwLength]));
  //Debug(Format('dwMemoryLoad: %d', [MemStatus.dwMemoryLoad]));
  //Debug(Format('ullTotalPhys: %d', [MemStatus.ullTotalPhys]));
  //Debug(Format('ullAvailPhys: %d', [MemStatus.ullAvailPhys]));
  //Debug(Format('ullTotalPageFile: %d', [MemStatus.ullTotalPageFile]));
  //Debug(Format('ullAvailPageFile: %d', [MemStatus.ullAvailPageFile]));
  //Debug(Format('ullTotalVirtual: %d', [MemStatus.ullTotalVirtual]));
  //Debug(Format('ullAvailVirtual: %d', [MemStatus.ullAvailVirtual]));
  //Debug(Format('ullAvailExtendedVirtual: %d', [MemStatus.ullAvailExtendedVirtual]));

  MemTot := MemStatus.ullTotalPhys div (1024 * 1024);
  MemAvi := MemStatus.ullAvailPhys div (1024 * 1024);
  MemUsg := MemTot - MemAvi;

  pbMemory.Max := MemTot;
  pbMemory.Position := MemUsg;

  lblTotalMem.Caption := 'Total Memory (MB): ' + MemTot.ToString;
  lblUsageMem.Caption := 'Usage Memory (MB): ' + (MemTot - MemUsg).ToString;
  lblPercentMem.Caption := 'Percent Used Memory (%): ' + (CalcolatePercent(MemTot, MemUsg)).ToString;

end;



procedure TfrmMain.TrimMemory(const ShowMessage: boolean = False);

var
  MyHandle: THandle;
  Struct: TProcessEntry32;
  BeforeTrim, AfterTrim: integer;

begin

  BeforeTrim := pbMemory.Position;
  try
    MyHandle := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
    Struct.dwSize := Sizeof(TProcessEntry32);

    if Process32First(MyHandle, Struct) then
      TrimAppMemorySize(struct.szExeFile, struct.th32ProcessID);

    while Process32Next(MyHandle, Struct) do
    begin
      TrimAppMemorySize(struct.szExeFile, struct.th32ProcessID);
    end;

  except
    on Exception do
      Debug('Error showing process list');
  end;

  MemoryUsage();
  AfterTrim := pbMemory.Position;

  AfterTrim := BeforeTrim - AfterTrim;
  if ShowMessage then
    Messaggio('Memory Trim (MB): ' + AfterTrim.ToString, mtInformation)
  else
    Debug('Error showing process list');
end;

procedure TfrmMain.LoadOptions();
begin
  with TIniFile.Create(IniFilename) do
  begin
    //FAN
    cbbMode.ItemIndex := ReadInteger('Fans', 'Mode', 0);
    tbValue.Position := ReadInteger('Fans', 'BasicMode', 50);
    //CPU
    tbAdvanced1.Position := ReadInteger('Fans', 'AdvancedCPU1', 20);
    tbAdvanced2.Position := ReadInteger('Fans', 'AdvancedCPU2', 45);
    tbAdvanced3.Position := ReadInteger('Fans', 'AdvancedCPU3', 55);
    tbAdvanced4.Position := ReadInteger('Fans', 'AdvancedCPU4', 65);
    tbAdvanced5.Position := ReadInteger('Fans', 'AdvancedCPU5', 70);
    tbAdvanced6.Position := ReadInteger('Fans', 'AdvancedCPU6', 75);
    //GPU
    tbAdvanced7.Position := ReadInteger('Fans', 'AdvancedGPU1', 0);
    tbAdvanced8.Position := ReadInteger('Fans', 'AdvancedGPU2', 20);
    tbAdvanced9.Position := ReadInteger('Fans', 'AdvancedGPU3', 40);
    tbAdvanced10.Position := ReadInteger('Fans', 'AdvancedGPU4', 60);
    tbAdvanced11.Position := ReadInteger('Fans', 'AdvancedGPU5', 80);
    tbAdvanced12.Position := ReadInteger('Fans', 'AdvancedGPU6', 80);
    //MEMORY
    chkTrimMemoryEvery.Checked := ReadBool('Memory', 'TrimMemoryEvery', False);
    chkTrimMemoryWhenOver.Checked := ReadBool('Memory', 'TrimMemoryWhenOver', False);
    chkTrimMemoryWhenProgramStart.Checked := ReadBool('Memory', 'TrimMemoryWhenProgramStart', False);
    Free;
  end;

  //btnApply.Click;
end;

procedure TfrmMain.SaveOptions();
begin
  with TIniFile.Create(IniFilename) do
  begin
    WriteInteger('Fans', 'Mode', cbbMode.ItemIndex);
    WriteInteger('Fans', 'BasicMode', tbValue.Position);
    //CPU
    WriteInteger('Fans', 'AdvancedCPU1', tbAdvanced1.Position);
    WriteInteger('Fans', 'AdvancedCPU2', tbAdvanced2.Position);
    WriteInteger('Fans', 'AdvancedCPU3', tbAdvanced3.Position);
    WriteInteger('Fans', 'AdvancedCPU4', tbAdvanced4.Position);
    WriteInteger('Fans', 'AdvancedCPU5', tbAdvanced5.Position);
    WriteInteger('Fans', 'AdvancedCPU6', tbAdvanced6.Position);
    //GPU
    WriteInteger('Fans', 'AdvancedGPU1', tbAdvanced7.Position);
    WriteInteger('Fans', 'AdvancedGPU2', tbAdvanced8.Position);
    WriteInteger('Fans', 'AdvancedGPU3', tbAdvanced9.Position);
    WriteInteger('Fans', 'AdvancedGPU4', tbAdvanced10.Position);
    WriteInteger('Fans', 'AdvancedGPU5', tbAdvanced11.Position);
    WriteInteger('Fans', 'AdvancedGPU6', tbAdvanced12.Position);


    WriteBool('Memory', 'TrimMemoryEvery', chkTrimMemoryEvery.Checked);
    WriteBool('Memory', 'TrimMemoryWhenOver', chkTrimMemoryWhenOver.Checked);
    WriteBool('Memory', 'TrimMemoryWhenProgramStart', chkTrimMemoryWhenProgramStart.Checked);
    Free;
  end;
end;

procedure TfrmMain.Advanced();
var
  DosOut: TStringList;
  sCPU, sGPU: string;
  Temp: TStringList;
  TB: TTrackBar;
  I: integer;
begin

  sCPU := 'A';
  for I := 1 to 6 do
  begin
    tb := TTrackBar(FindComponent('tbAdvanced' + I.ToString));
    sCPU := sCPU + ',' + TB.Position.ToString;
  end;
  sCPU := StringReplace(sCPU, 'A,', '/cpu:', []);


  sGPU := 'A';
  for I := 7 to 12 do
  begin
    tb := TTrackBar(FindComponent('tbAdvanced' + I.ToString));
    sGPU := sGPU + ',' + TB.Position.ToString;
  end;
  sGPU := StringReplace(sGPU, 'A,', '/gpu:', []);

  Debug(sCPU);
  Debug(sGPU);



  DosOut := TStringList.Create;
  //DosOut.Text := GetDosOutput(MSIApp + ' advanced /cpu:20,45,55,65,70,75 /gpu:0,20,40,60,80,80', DriveLetter);
  DosOut.Text := GetDosOutput(MSIApp + ' advanced ' + sCPU + ' ' + sGPU, DriveLetter);

  Debug('COMMAND: ADVANCED');
  Debug(DosOut.Text);

  if Pos('All done', DosOut.Text) = 0 then
  begin
    Messaggio('Error: Read Logs');
  end;

  DosOut.Free;
  Status();

end;

procedure TfrmMain.Auto();
var
  DosOut: TStringList;

begin

  DosOut := TStringList.Create;
  DosOut.Text := GetDosOutput(MSIApp + ' auto', DriveLetter);

  Debug('COMMAND: BASIC');
  Debug(DosOut.Text);

  if Pos('All done', DosOut.Text) = 0 then
  begin
    Messaggio('Error: Read Logs');
  end;

  DosOut.Free;
  Status();

end;

procedure TfrmMain.Basic();
var
  DosOut: TStringList;

begin

  DosOut := TStringList.Create;
  DosOut.Text := GetDosOutput(MSIApp + ' basic /value:' + tbValue.Position.ToString, DriveLetter);

  Debug('COMMAND: BASIC');
  Debug(DosOut.Text);

  if Pos('All done', DosOut.Text) = 0 then
  begin
    Messaggio('Error: Read Logs');
  end;

  DosOut.Free;
  Status();

end;

procedure TfrmMain.Status();
var
  DosOut: TStringList;
  Value: string;
  sCPU, sGPU: string;
  Temp: TStringList;
  I: integer;
  TB: TTrackBar;
begin
  DosOut := TStringList.Create;
  DosOut.Text := GetDosOutput(MSIApp + ' status', DriveLetter);

  Debug('COMMAND: STATUS');
  Debug(DosOut.Text);

  if DosOut.Count > 0 then
    DosOut[0] := StringReplace(DosOut[0], 'Current control mode: ', '', [rfIgnoreCase]).ToLower;

  tbValue.Position := 0;
  tbValue.Enabled := False;
  pcAdvanced.ActivePageIndex := 0;
  pcAdvanced.Enabled := False;

  if DosOut[0] = 'auto' then
  begin
    cbbMode.ItemIndex := 0;
    Exit;
  end;

  if DosOut[0] = 'basic' then
  begin
    cbbMode.ItemIndex := 1;
    Value := StringReplace(DosOut[1], 'Current control value:', '', [rfIgnoreCase]);
    tbValue.Enabled := True;
    tbValue.Position := StrToInt(Value);
    Exit;
  end;

  if DosOut[0] = 'advanced' then
  begin
    //CPU curve: [20,45,55,65,70,75]
    //GPU curve: [0,50,40,60,80,80]

    cbbMode.ItemIndex := 2;
    pcAdvanced.ActivePageIndex := 0;
    pcAdvanced.Enabled := True;
    sCPU := StringReplace(DosOut[1], 'CPU curve: [', '', [rfIgnoreCase]).Replace(']', '');
    sGPU := StringReplace(DosOut[2], 'GPU curve: [', '', [rfIgnoreCase]).Replace(']', '');


    Temp := Explode(',', sCPU, True);
    for I := 0 to Temp.Count - 1 do
    begin
      tb := TTrackBar(FindComponent('tbAdvanced' + (I + 1).ToString));
      TB.Position := StrToInt(Temp[I]);
      tbAdvanced1Change(tb);
    end;

    Temp := Explode(',', sGPU, True);
    for I := 0 to Temp.Count - 1 do
    begin
      tb := TTrackBar(FindComponent('tbAdvanced' + (I + 7).ToString));
      TB.Position := StrToInt(Temp[I]);
      tbAdvanced1Change(tb);
    end;

    Temp.Free;
  end;

  DosOut.Free;
end;


procedure TfrmMain.btnStatusClick(Sender: TObject);

begin
  Status();
end;

procedure TfrmMain.btnTrimMemoryClick(Sender: TObject);
begin
  TrimMemory(True);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  bmp: TBitmap;
  R: TRect;
begin
  SetRect(R, 0, 0, 16, 16);
  bmp := TBitmap.Create;
  bmp.SetSize(16, 16);

  with bmp.Canvas do
  begin
    bmp.Canvas.Brush.Color := $81F682;
    bmp.Canvas.FillRect(r);


    bmp.Canvas.Pen.Color := $1EB100;
    Rectangle(r);
    //Image2.Picture.Assign(bmp);
    //Font.Color := clBlack;
    //DrawTextCentered(bmp.Canvas, r, '10');
  end;
  Image1.Picture.Assign(bmp);
end;

procedure TfrmMain.Button2Click(Sender: TObject);

begin

end;

procedure TfrmMain.cbbModeChange(Sender: TObject);
begin
  tbValue.Enabled := cbbMode.ItemIndex = 1;
  pcAdvanced.Enabled := cbbMode.ItemIndex = 2;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := True;
    Caption := APP_TITLE + ' - Please Wait...';
    Status();
    LoadOptions();
    btnApply.Click;
    Caption := APP_TITLE;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveOptions();
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FPid := 0;
  FActivated := False;
  AppDir := ExtractFilePath(Application.ExeName);
  MSIApp := AppDir + 'MsiFanControl.exe';
  IniFilename := ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
  DriveLetter := AppDir.Substring(0, 3);

  pcMain.ActivePageIndex := 0;
  pcAdvanced.ActivePageIndex := 0;



  LoadOptions();
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin

end;

procedure TfrmMain.tbAdvanced1Change(Sender: TObject);
var
  TB: TTrackBar;
  TxT: TEdit;
  ID: integer;
begin
  TB := TTrackBar(Sender);
  ID := TB.Tag;
  TxT := TEdit(FindComponent('edtAdvanced' + ID.ToString));
  if TxT <> nil then
    TxT.Text := TB.Position.ToString;
end;

procedure TfrmMain.tmrMemoryStatusTimer(Sender: TObject);
begin
  try
    MemoryUsage();
  except
    pbMemory.Position := 0;
    lblTotalMem.Caption := 'Total Memory (MB): 0';
    lblUsageMem.Caption := 'Usage Memory (MB): 0';
    lblPercentMem.Caption := 'Percent Used Memory (%): 0';

  end;
end;

procedure TfrmMain.btnApplyClick(Sender: TObject);
begin
  case cbbMode.ItemIndex of
    0: Auto();
    1: Basic();
    2: Advanced()
    else;
  end;

  SaveOptions();
end;

end.