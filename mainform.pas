unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Spin, ValEdit, PopupNotifier, Menus, FunctionsUtils,
  ShellApi, jwatlhelp32, jwapsapi, IniFiles, Registry;

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
    apApp: TApplicationProperties;
    btnApply: TButton;
    btnHide: TButton;
    btnExit: TButton;
    btnTrimMemory: TButton;
    btnCreateScheduleTask: TButton;
    cbbMode: TComboBox;
    chkStartMinimized: TCheckBox;
    chkMinimizedWhenClose: TCheckBox;
    chkTrimMemoryEvery: TCheckBox;
    chkStartUpWindows: TCheckBox;
    chkTrimMemoryWhenOver: TCheckBox;
    chkRefreshMemoryStatus: TCheckBox;
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
    Separator1: TMenuItem;
    mniShow: TMenuItem;
    mniClose: TMenuItem;
    mmoLog: TMemo;
    pcAdvanced: TPageControl;
    pcMain: TPageControl;
    pbMemory: TProgressBar;
    pnNotifier: TPopupNotifier;
    pmTrayIcon: TPopupMenu;
    seTrimMemoryEvery: TSpinEdit;
    seTrimMemoryWhenOver: TSpinEdit;
    seRefreshMemoryStatus: TSpinEdit;
    tsDeveloped: TTabSheet;
    tmrNotifier: TTimer;
    tmrTrimMemoryOver: TTimer;
    tmrTrimMemoryEvery: TTimer;
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
    lstAbout: TValueListEditor;
    procedure apAppMinimize(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnCreateScheduleTaskClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnHideClick(Sender: TObject);
    procedure btnTrimMemoryClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbbModeChange(Sender: TObject);
    procedure chkTrimMemoryEveryChange(Sender: TObject);
    procedure chkTrimMemoryEveryClick(Sender: TObject);
    procedure chkTrimMemoryWhenOverClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure mniShowClick(Sender: TObject);
    procedure tbAdvanced1Change(Sender: TObject);
    procedure tiTrayDblClick(Sender: TObject);
    procedure tmrMemoryStatusTimer(Sender: TObject);
    procedure tmrNotifierTimer(Sender: TObject);
    procedure tmrTrimMemoryEveryTimer(Sender: TObject);
    procedure tmrTrimMemoryOverTimer(Sender: TObject);
    procedure lstAboutDblClick(Sender: TObject);
  private
    FSeconds: integer;
    FPercent: integer;
    FActivated: boolean;
    procedure GenerateIconsNumber();
  public
    procedure LoadOptions();
    procedure SaveOptions();
    procedure Status();
    procedure Basic();
    procedure Auto();
    procedure Advanced();
    procedure Apply();
    procedure TrimMemoryWhenOver();
    procedure MemoryUsage();
    procedure TrimMemory(const ShowMessage: boolean = False);
    procedure Notifier(aText: string);
    procedure RunOnStartup(bRemove: boolean = False);
  end;

var
  frmMain: TfrmMain;
  IniFilename: string;
  AppDir: string;
  MSIApp: string;
  DriveLetter: string;


const
  APP_TITLE = 'MiniDragonCenter v2022.1';

function GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx): BOOL; stdcall; external kernel32;

implementation

uses ShlObj, ActiveX, ComObj;

{$R *.lfm}

{ TfrmMain }

procedure Debug(Value: string);
begin
  OutputDebugString(PChar(Value));

  frmMain.mmoLog.Lines.Add(Value);
  if frmMain.mmoLog.Lines.Count > 1000 then frmMain.mmoLog.Lines.Delete(0);
  SendMessage(frmMain.mmoLog.Handle, EM_LINESCROLL, 0, frmMain.mmoLog.Lines.Count);
end;

procedure OpenLink(Value: string);
begin
  ShellExecute(GetForegroundWindow, 'open', PChar(Value), '', '', SW_SHOWNORMAL);
end;

function RunOnStartupRegistry(RemoveKey: boolean): boolean;
const
  REG_KEY = 'MiniDragonCenter';
begin

  Result := RunOnStartup(REG_KEY, Application.ExeName, RemoveKey);
  Debug('RunOnStartupRegistry: ' + BoolToStr(Result, True));

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
  // if DrawRect.Bottom < R.Bottom then
  //   OffsetRect(DrawRect, 0, (R.Bottom - DrawRect.Bottom) div 2)
  // else
  DrawRect.Bottom := R.Bottom;
  ZeroMemory(@DrawParams, SizeOf(DrawParams));
  DrawParams.cbSize := SizeOf(DrawParams);
  DrawTextEx(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags, @DrawParams);
  Result := DrawParams.uiLengthDrawn;
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



procedure CreateShortCut();
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  PIDL: PItemIDList;
  InFolder: array[0..MAX_PATH] of char;
  TargetName: string;
  LinkName: WideString;

begin

  TargetName := Application.ExeName;

  IObject := CreateComObject(CLSID_ShellLink);
  ISLink := IObject as IShellLink;
  IPFile := IObject as IPersistFile;

  with ISLink do
  begin
    SetPath(PChar(TargetName));
    SetWorkingDirectory(PChar(ExtractFilePath(TargetName)));
  end;

  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDList(PIDL, InFolder);

  LinkName := ChangeFileExt(Application.ExeName, '.lnk');
  IPFile.Save(PWChar(LinkName), False);
end;

procedure TfrmMain.RunOnStartup(bRemove: boolean = False);
var
  A, B, User, STARTUP: string;
  //const
  //STARTUP = 'C:\ProgramData\Microsoft\Windows\Start Menu\Programs\StartUp\';
begin

  User := GetEnvironmentVariable('USERNAME');
  STARTUP := 'C:\Users\' + User + '\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup\';

  //CREATE LNK IN APP FOLDER
  A := ChangeFileExt(Application.ExeName, '.lnk');
  B := STARTUP + ExtractFileName(A);

  if not FileExists(A) then CreateShortCut;

  ForceDirectories(STARTUP);

  DeleteFile(b);
  if not bRemove then CopyFile(PChar(A), PChar(B), False);

  Debug('RunOnStartup Remove: ' + BoolToStr(bRemove, True));
end;

procedure TfrmMain.Notifier(aText: string);

var
  X, Y: integer;
begin
  if tmrNotifier.Enabled then
  begin
    tmrNotifier.Enabled := False;
    pnNotifier.Hide;
  end;
  pnNotifier.Title := Application.Title;
  pnNotifier.Text := aText;

  x := Screen.PrimaryMonitor.Left + Screen.PrimaryMonitor.Width - pnNotifier.vNotifierForm.Width;
  y := Screen.PrimaryMonitor.top + Screen.PrimaryMonitor.Height - pnNotifier.vNotifierForm.Height - 50;
  pnNotifier.ShowAtPos(x, y);
  tmrNotifier.Enabled := True;
end;

procedure TfrmMain.GenerateIconsNumber();
var
  ImageNumber: TBitmap;
  RImage, RText: TRect;
  I: integer;
  IconNumber: TIcon;
begin
  SetRect(RImage, 0, 0, 16, 16);
  SetRect(RText, 1, 1, 15, 15);
  ImageNumber := TBitmap.Create;
  ImageNumber.SetSize(16, 16);
  IconNumber := TIcon.Create;

  with ImageNumber.Canvas do
  begin
    Font.Color := clBlack;
    Font.Name := 'Consolas';
    Font.Size := 8;
  end;

  for I := 1 to 100 do
  begin

    with ImageNumber.Canvas do
    begin

      if I < 65 then
      begin
        ImageNumber.Canvas.Brush.Color := $81F682;
        ImageNumber.Canvas.Pen.Color := $1EB100;
      end
      else
      if I < 70 then
      begin
        ImageNumber.Canvas.Brush.Color := $93F2F6;
        ImageNumber.Canvas.Pen.Color := $00BAEF;
      end
      else
      begin
        ImageNumber.Canvas.Brush.Color := $5E7EFF;
        ImageNumber.Canvas.Pen.Color := $0000D6;
      end;

      FillRect(RImage);
      Rectangle(RImage);
      DrawTextCentered(ImageNumber.Canvas, RText, I.ToString);

      IconNumber.Assign(ImageNumber);
      ilMemory.AddIcon(IconNumber);
    end;
  end;
  IconNumber.Free;
  ImageNumber.Free;
end;

procedure TfrmMain.TrimMemoryWhenOver();
begin
  if not chkTrimMemoryWhenOver.Checked then exit;

end;

procedure TfrmMain.MemoryUsage();
var
  MemStatus: TMemoryStatusEx;

  MemTot, MemUsg, MemAvi, Percent: cardinal;
  IconImage: TIcon;
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
  Percent := Trunc(CalcolatePercent(MemTot, MemUsg));
  FPercent := Percent;

  pbMemory.Max := MemTot;
  pbMemory.Position := MemUsg;

  lblTotalMem.Caption := 'Total Memory (MB): ' + MemTot.ToString;
  lblUsageMem.Caption := 'Usage Memory (MB): ' + (MemTot - MemUsg).ToString;
  lblPercentMem.Caption := 'Percent Used Memory (%): ' + (Percent).ToString;




  IconImage := TIcon.Create;
  ilMemory.GetIcon(Percent - 1, IconImage);
  tiTray.Icon := IconImage;
end;



procedure TfrmMain.TrimMemory(const ShowMessage: boolean = False);

var
  MyHandle: THandle;
  Struct: TProcessEntry32;
  BeforeTrim, AfterTrim: integer;
  Error: boolean;
begin
  Error := False;
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
    begin
      Error := True;
      Debug('Error trim memory!');
    end;
  end;

  MemoryUsage();
  AfterTrim := pbMemory.Position;

  AfterTrim := AfterTrim - BeforeTrim;
  if AfterTrim < 0 then AfterTrim := 0;

  if not ShowMessage then exit;

  if Error then
    Notifier('Error trim memory!')
  else
    Notifier('Memory Trim: ' + AfterTrim.ToString + ' MB Free');

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
    seTrimMemoryEvery.Value := ReadInteger('Memory', 'TrimMemoryEveryValue', 10);
    chkTrimMemoryWhenOver.Checked := ReadBool('Memory', 'TrimMemoryWhenOver', False);
    seTrimMemoryWhenOver.Value := ReadInteger('Memory', 'TrimMemoryWhenOverValue', 65);
    chkRefreshMemoryStatus.Checked := ReadBool('Memory', 'RefreshMemoryStatus', True);
    seRefreshMemoryStatus.Value := ReadInteger('Memory', 'RefreshMemoryStatusValue', 1);


    chkTrimMemoryWhenProgramStart.Checked := ReadBool('Memory', 'TrimMemoryWhenProgramStart', False);
    //OPTIONS
    chkStartUpWindows.Checked := ReadBool('Options', 'StartUpWindows', False);
    chkStartMinimized.Checked := ReadBool('Options', 'StartMinimized', True);
    chkMinimizedWhenClose.Checked := ReadBool('Options', 'MinimizedWhenClose', True);
    Free;
  end;

  tmrMemoryStatus.Enabled := chkRefreshMemoryStatus.Checked;
  tmrMemoryStatus.Interval := seRefreshMemoryStatus.Value * 1000;

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
    WriteInteger('Memory', 'TrimMemoryEveryValue', seTrimMemoryEvery.Value);
    WriteBool('Memory', 'TrimMemoryWhenOver', chkTrimMemoryWhenOver.Checked);
    WriteInteger('Memory', 'TrimMemoryWhenOverValue', seTrimMemoryWhenOver.Value);
    WriteBool('Memory', 'RefreshMemoryStatus', chkRefreshMemoryStatus.Checked);
    WriteInteger('Memory', 'RefreshMemoryStatusValue', seRefreshMemoryStatus.Value);

    WriteBool('Memory', 'TrimMemoryWhenProgramStart', chkTrimMemoryWhenProgramStart.Checked);
    //OPTIONS
    WriteBool('Options', 'StartUpWindows', chkStartUpWindows.Checked);
    WriteBool('Options', 'StartMinimized', chkStartMinimized.Checked);
    WriteBool('Options', 'MinimizedWhenClose', chkMinimizedWhenClose.Checked);
    Free;
  end;

  RunOnStartupRegistry(not chkStartUpWindows.Checked);

end;

procedure TfrmMain.Apply();
begin
  if not FileExists(MSIApp) then
  begin
    MsgBox('MsiFanControl.exe non found!');
    Exit;
  end;

  Caption := APP_TITLE + ' - Please Wait...';
  Application.ProcessMessages;

  case cbbMode.ItemIndex of
    0: Auto();
    1: Basic();
    2: Advanced()
    else;
  end;

  Status();

  Caption := APP_TITLE;

  tmrTrimMemoryOver.Interval := 60000; //seTrimMemoryWhenOver.Value * 60000;
  tmrTrimMemoryOver.Enabled := chkTrimMemoryWhenOver.Checked;
  Debug('Trim Memory When Over (%) Enable: ' + BoolToStr(chkTrimMemoryWhenOver.Checked, True));

  tmrTrimMemoryEvery.Interval := seTrimMemoryEvery.Value * 60000;
  tmrTrimMemoryEvery.Enabled := chkTrimMemoryEvery.Checked;
  Debug('Trim Memory Every (Minutes) Enable: ' + BoolToStr(chkTrimMemoryEvery.Checked, True));
end;

procedure TfrmMain.Advanced();
var
  DosOut: TStringList;
  sCPU, sGPU: string;
  Temp: TStringList;
  TB: TTrackBar;
  I: integer;
begin
  tiTray.Hint := 'Fan Mode: Advanced';
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
    MsgBox('Error: Read Logs');
  end;

  DosOut.Free;
  Status();

end;

procedure TfrmMain.Auto();
var
  DosOut: TStringList;

begin
  tiTray.Hint := 'Fan Mode: Auto';
  DosOut := TStringList.Create;
  DosOut.Text := GetDosOutput(MSIApp + ' auto', DriveLetter);

  Debug('COMMAND: BASIC');
  Debug(DosOut.Text);

  if Pos('All done', DosOut.Text) = 0 then
  begin
    MsgBox('Error: Read Logs');
  end;

  DosOut.Free;
  Status();

end;

procedure TfrmMain.Basic();
var
  DosOut: TStringList;

begin
  tiTray.Hint := 'Fan Mode: Basic';
  DosOut := TStringList.Create;
  DosOut.Text := GetDosOutput(MSIApp + ' basic /value:' + tbValue.Position.ToString, DriveLetter);

  Debug('COMMAND: BASIC');
  Debug(DosOut.Text);

  if Pos('All done', DosOut.Text) = 0 then
  begin
    MsgBox('Error: Read Logs');
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


procedure TfrmMain.btnHideClick(Sender: TObject);
begin
  Application.Minimize;
end;

procedure TfrmMain.btnTrimMemoryClick(Sender: TObject);
begin
  TrimMemory(True);
end;

procedure TfrmMain.Button1Click(Sender: TObject);

begin

end;


procedure TfrmMain.cbbModeChange(Sender: TObject);
begin
  tbValue.Enabled := cbbMode.ItemIndex = 1;
  pcAdvanced.Enabled := cbbMode.ItemIndex = 2;
end;

procedure TfrmMain.chkTrimMemoryEveryChange(Sender: TObject);
begin

end;

procedure TfrmMain.chkTrimMemoryEveryClick(Sender: TObject);
begin

end;

procedure TfrmMain.chkTrimMemoryWhenOverClick(Sender: TObject);
begin

end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := True;

  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if chkMinimizedWhenClose.Checked then
  begin
    CloseAction := caNone;
    Application.Minimize;
    exit;
  end;

  if MsgBox('Save all options?', mtConfirmation) then
    SaveOptions();

  CloseAction := caFree;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin

  FActivated := False;
  AppDir := ExtractFilePath(Application.ExeName);
  MSIApp := AppDir + 'MsiFanControl.exe';
  IniFilename := ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
  DriveLetter := AppDir.Substring(0, 3);

  tiTray.BalloonTitle := APP_TITLE;
  pcMain.ActivePageIndex := 0;
  pcAdvanced.ActivePageIndex := 0;
  tsDeveloped.TabVisible := InDebugMode();
  lstAbout.Row := 0;

  GenerateIconsNumber();
  LoadOptions();
  Apply();

  if not chkStartMinimized.Checked then
    tiTrayDblClick(tiTray);

  tmrTrimMemoryEvery.Interval := seTrimMemoryEvery.Value * 60000;
  tmrTrimMemoryEvery.Enabled := chkTrimMemoryEvery.Checked;
  Debug('Trim Memory Every (Minutes) Enable: ' + BoolToStr(chkTrimMemoryEvery.Checked, True));

  tmrTrimMemoryOver.Interval := 60000; //seTrimMemoryWhenOver.Value * 60000;
  tmrTrimMemoryOver.Enabled := chkTrimMemoryWhenOver.Checked;
  Debug('Trim Memory When Over (%) Enable: ' + BoolToStr(chkTrimMemoryWhenOver.Checked, True));

  if chkTrimMemoryWhenProgramStart.Checked then TrimMemory(True);
end;


procedure TfrmMain.mniShowClick(Sender: TObject);
begin
  if Showing then exit;

  Show();
  WindowState := wsNormal;
  Application.BringToFront();
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



procedure TfrmMain.tiTrayDblClick(Sender: TObject);
begin
  if Showing then
    Application.Minimize
  else
  begin
    Show();
    WindowState := wsNormal;
    Application.BringToFront();
  end;
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

procedure TfrmMain.tmrNotifierTimer(Sender: TObject);
begin
  tmrNotifier.Enabled := False;
  pnNotifier.Hide;

end;

procedure TfrmMain.tmrTrimMemoryEveryTimer(Sender: TObject);
begin
  Debug('Call Trim Memory Every');
  TrimMemory();
end;

procedure TfrmMain.tmrTrimMemoryOverTimer(Sender: TObject);
begin
  if FPercent > seTrimMemoryWhenOver.Value then
  begin
    Debug('Call Trim Memory Over');
    TrimMemory(True);
  end;
end;

procedure TfrmMain.lstAboutDblClick(Sender: TObject);
var
  Value: string;
begin
  Value := lstAbout.Cells[lstAbout.Col, lstAbout.Row];
  OpenLink(Value);
end;


procedure TfrmMain.btnApplyClick(Sender: TObject);
begin
  Apply();
  SaveOptions();
end;

procedure TfrmMain.btnCreateScheduleTaskClick(Sender: TObject);
var
  Bat: TStringList;
  Filename: string;
begin
  pcMain.ActivePage := tsLog;
  Filename := ChangeFileExt(Application.ExeName, '.bat');
  Bat := TStringList.Create;
  Bat.Add('SCHTASKS /DELETE /TN "\MiniDragonCenter"');
  if MsgBox('Do you wont add schedule task?', mtConfirmation) then
    Bat.Add('SCHTASKS /CREATE /sc ONLOGON /tn "\MiniDragonCenter" /tr "d:\Sviluppo\Lazarus\Progetti\MiniDragonCenter\Bin\MiniDragonCenter.exe"');
  Bat.Add('EXIT');
  Bat.SaveToFile(Filename);
  Bat.Free;

  Debug('START CreateScheduleTask');

  GetDosOutput(Filename, AppDir, True);

  Debug('END CreateScheduleTask');
end;

procedure TfrmMain.btnExitClick(Sender: TObject);
begin
  if MsgBox('Save all options?', mtConfirmation) then
    SaveOptions();

  Application.Terminate;
end;

procedure TfrmMain.apAppMinimize(Sender: TObject);
begin
  Hide();
  WindowState := wsMinimized;

end;

end.
