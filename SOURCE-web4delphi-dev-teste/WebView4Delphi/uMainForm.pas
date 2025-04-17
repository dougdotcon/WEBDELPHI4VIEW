unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Buttons,
  uWVBrowser, uWVWindowParent, uWVTypes, uWVConstants, 
  uWVTypeLibrary, uWVBrowserBase, uWVLoader, uWVLoaderInternal,
  uPopularCaptchaSolver, System.JSON, uWVWinControl,
  uWVLibFunctions, uWVInterfaces, uWVEvents, uWVCoreWebView2Args,
  uWVCoreWebView2, uWVCoreWebView2Controller, uWVCoreWebView2Environment,
  uWVCoreWebView2EnvironmentOptions, uWVCoreWebView2CustomSchemeRegistration, 
  uWVCoreWebView2Delegates, uWVCoreWebView2Frame;

type
  TLogLevel = (llDebug, llInfo, llWarn, llError);

  TMainForm = class(TForm)
    pnlHeader: TPanel;
    pnlControls: TPanel;
    pnlMain: TPanel;
    pnlLog: TPanel;
    pnlSolverConfig: TPanel;
    edURL: TEdit;
    btnNavigate: TButton;
    txtLog: TMemo;
    btnSolve: TButton;
    btnStop: TButton;
    lblApiKey: TLabel;
    edApiKey: TEdit;
    chkAutoSolve: TCheckBox;
    chkAutoOpen: TCheckBox;
    chkEnglish: TCheckBox;
    chkAlwaysSolve: TCheckBox;
    btnSettings: TButton;
    btnRefreshIframe: TButton;
    WVWindowParent1: TWVWindowParent;
    StatusBar1: TStatusBar;
    cbDetectMethod: TComboBox;
    lblStatus: TLabel;
    btnLoadCaptcha: TButton;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNavigateClick(Sender: TObject);
    procedure btnSolveClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnRefreshIframeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbDetectMethodChange(Sender: TObject);
    procedure btnLoadCaptchaClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
  private
    FWVBrowser: TWVBrowser;
    FSolver: TPopularCaptchaSolver;
    FIsPageLoaded: Boolean;

    procedure WVBrowserInitializedHandler(Sender: TObject);
    procedure WVBrowserNavigationCompletedHandler(Sender: TObject;
      const aWebView: ICoreWebView2;
      const aArgs: ICoreWebView2NavigationCompletedEventArgs);
    procedure WVBrowserSourceChangedHandler(Sender: TObject;
      const aWebView: ICoreWebView2;
      const aArgs: ICoreWebView2SourceChangedEventArgs);
    procedure WVBrowserAfterCreatedHandler(Sender: TObject);
    procedure HandleLogEvent(Msg: string; Level:string);
    procedure SolveOnUIThread;


    procedure InitializeWebView;
    procedure InitializeSolver;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure LoadSampleCaptcha;
    function GetSolverDetectionMethod: Integer;
    procedure SetSolverDetectionMethod(Method: Integer);
    procedure AddLog(const Msg, Level: string);
    procedure ClickCheckboxNatively;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
  // Sample CAPTCHA URLs for testing
  SAMPLE_URLS: array [0 .. 3] of string = ('https://accounts.hcaptcha.com/demo',
    'https://democaptcha.com/demo-form-eng/hcaptcha.html',
    'https://www.hcaptcha.com/showcase',
    'https://www.hcaptcha.com/content-guidelines');

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialize form and components
  txtLog.Clear;
  cbDetectMethod.Items.Add('Automatique');
  cbDetectMethod.Items.Add('Widget');
  cbDetectMethod.Items.Add('MultiSelect');
  cbDetectMethod.Items.Add('Grid');
  cbDetectMethod.Items.Add('Bbox');
  cbDetectMethod.Items.Add('BboxDD');
  cbDetectMethod.ItemIndex := 0;

  // Default URL
  edURL.Text := SAMPLE_URLS[0];

  // Load settings
  LoadSettings;
end;

procedure TMainForm.InitializeWebView;
begin
  // Create and initialize WebView browser
  FWVBrowser := TWVBrowser.Create(nil);
  FIsPageLoaded := False;
  // Connect event handlers
  WVBrowserInitializedHandler(Self);
  FWVBrowser.OnNavigationCompleted := WVBrowserNavigationCompletedHandler;
  FWVBrowser.OnSourceChanged := WVBrowserSourceChangedHandler;
  FWVBrowser.OnAfterCreated := WVBrowserAfterCreatedHandler;

  WVWindowParent1.CreateHandle;
end;

procedure TMainForm.InitializeSolver;
begin
  if Assigned(FSolver) then
    FSolver.Free;

  FSolver := TPopularCaptchaSolver.Create(FWVBrowser);
  FSolver.OnLog := HandleLogEvent;

  // Configure from UI
  FSolver.ApiKey := edApiKey.Text;
  FSolver.AutoSolve := chkAutoSolve.Checked;
  FSolver.AutoOpen := chkAutoOpen.Checked;
  FSolver.EnglishEnabled := chkEnglish.Checked;
  FSolver.AlwaysSolve := chkAlwaysSolve.Checked;

  // Set detection method if not automatic
  if cbDetectMethod.ItemIndex > 0 then
    SetSolverDetectionMethod(cbDetectMethod.ItemIndex - 1);

  AddLog('Solveur initialisé', 'INFO');
end;

procedure TMainForm.WVBrowserInitializedHandler(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := 'WebView2 initialisé';
  InitializeSolver;
end;

procedure TMainForm.WVBrowserNavigationCompletedHandler(Sender: TObject;
  const aWebView: ICoreWebView2;
  const aArgs: ICoreWebView2NavigationCompletedEventArgs);
var
  IsSuccess: Boolean;
  NavID: UINT64;
  StatusCode: Integer;
  TempArgs: TCoreWebView2NavigationCompletedEventArgs;
begin
  // Créer une instance temporaire pour accéder aux méthodes
  TempArgs := TCoreWebView2NavigationCompletedEventArgs.Create(aArgs);
  try
    IsSuccess := TempArgs.IsSuccess;
    NavID := TempArgs.NavigationID;

    if IsSuccess then
    begin
      FIsPageLoaded := True;
      AddLog(Format('Navigation complétée avec succès. Navigation ID: %d', [NavID]), 'INFO');

      // Si vous voulez obtenir le code HTTP
      StatusCode := TempArgs.HttpStatusCode;
      AddLog(Format('Code de statut HTTP: %d', [StatusCode]), 'INFO');

      // Ici vous pouvez lancer l'extraction des données ou la résolution du captcha
//      if Assigned(FSolver) then
//        TThread.Queue(nil, SolveOnUIThread);
    end
    else
    begin
      FIsPageLoaded := False;
      AddLog('Navigation échouée', 'ERROR');
    end;
  finally
    TempArgs.Free;
  end;
end;

procedure TMainForm.WVBrowserSourceChangedHandler(Sender: TObject;
const aWebView: ICoreWebView2;
const aArgs: ICoreWebView2SourceChangedEventArgs);
begin
  edURL.Text := FWVBrowser.Source;
end;

procedure TMainForm.WVBrowserAfterCreatedHandler(Sender: TObject);
begin
  WVWindowParent1.UpdateSize;
  StatusBar1.Panels[0].Text := 'WebView2 créé';

  // Add resource filters for capturing images
  FWVBrowser.AddWebResourceRequestedFilter('*',
    COREWEBVIEW2_WEB_RESOURCE_CONTEXT_IMAGE);
end;

procedure TMainForm.HandleLogEvent( Msg: string; Level:string);
var
  Prefix: string;
begin
  if Level='Debug' then
      Prefix := '[DEBUG] '
   else if Level='Info' then
      Prefix := '[INFO] ' else
   if Level='Warm' then
      Prefix := '[WARN] ' else
   if Level='Error' then
      Prefix := '[ERROR] '
  else
    Prefix := '';


  TThread.Queue(nil,
    procedure
    begin
      txtLog.Lines.Add(Prefix + Msg);
      // Auto-scroll to bottom
      SendMessage(txtLog.Handle, EM_LINESCROLL, 0, txtLog.Lines.Count);

      // Update status
      lblStatus.Caption := 'État: ' + Msg;
    end);
end;

procedure TMainForm.SolveOnUIThread;
begin
  if Assigned(FSolver) then
    FSolver.Solve;
end;

procedure TMainForm.LoadSettings;
var
  SettingsFile: string;
  JsonObj: TJSONObject;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  SettingsFile := ExtractFilePath(Application.ExeName) + 'captchasolver.json';

  if FileExists(SettingsFile) then
  begin
    try
      FileStream := TFileStream.Create(SettingsFile, fmOpenRead or
        fmShareDenyWrite);
      StringStream := TStringStream.Create('', TEncoding.UTF8);
      try
        StringStream.CopyFrom(FileStream, 0);

        JsonObj := TJSONObject.ParseJSONValue(StringStream.DataString)
          as TJSONObject;
        if Assigned(JsonObj) then
          try
            // Load settings
            edApiKey.Text := JsonObj.GetValue<string>('apiKey', '');
            chkAutoSolve.Checked := JsonObj.GetValue<Boolean>
              ('autoSolve', True);
            chkAutoOpen.Checked := JsonObj.GetValue<Boolean>('autoOpen', True);
            chkEnglish.Checked := JsonObj.GetValue<Boolean>
              ('englishEnabled', True);
            chkAlwaysSolve.Checked := JsonObj.GetValue<Boolean>
              ('alwaysSolve', False);
            cbDetectMethod.ItemIndex := JsonObj.GetValue<Integer>
              ('detectMethod', 0);

            AddLog('Paramètres chargés depuis: ' + SettingsFile, 'INFO');
          finally
            JsonObj.Free;
          end;
      finally
        StringStream.Free;
        FileStream.Free;
      end;
    except
      on E: Exception do
        AddLog('Erreur lors du chargement des paramètres: ' +
          E.Message, 'ERROR');
    end;
  end
  else
    AddLog('Fichier de paramètres non trouvé: ' + SettingsFile, 'INFO');
end;

procedure TMainForm.SaveSettings;
var
  SettingsFile: string;
  JsonObj: TJSONObject;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  SettingsFile := ExtractFilePath(Application.ExeName) + 'captchasolver.json';

  JsonObj := TJSONObject.Create;
  try
    // Save settings
    JsonObj.AddPair('apiKey', edApiKey.Text);
    JsonObj.AddPair('autoSolve', TJSONBool.Create(chkAutoSolve.Checked));
    JsonObj.AddPair('autoOpen', TJSONBool.Create(chkAutoOpen.Checked));
    JsonObj.AddPair('englishEnabled', TJSONBool.Create(chkEnglish.Checked));
    JsonObj.AddPair('alwaysSolve', TJSONBool.Create(chkAlwaysSolve.Checked));
    JsonObj.AddPair('detectMethod',
      TJSONNumber.Create(cbDetectMethod.ItemIndex));

    StringStream := TStringStream.Create(JsonObj.ToJSON, TEncoding.UTF8);
    try
      FileStream := TFileStream.Create(SettingsFile, fmCreate);
      try
        FileStream.CopyFrom(StringStream, 0);
        AddLog('Paramètres enregistrés dans: ' + SettingsFile, 'INFO');
      finally
        FileStream.Free;
      end;
    finally
      StringStream.Free;
    end;
  finally
    JsonObj.Free;
  end;
end;

procedure TMainForm.AddLog(const Msg, Level: string);
var
  LogLevel: TLogLevel;
begin
//  if Level = 'DEBUG' then
//    LogLevel := llDebug
//  else if Level = 'INFO' then
//    LogLevel := llInfo
//  else if Level = 'WARN' then
//    LogLevel := llWarn
//  else if Level = 'ERROR' then
//    LogLevel := llError
//  else
//    LogLevel := llInfo;

  HandleLogEvent(Msg, Level);
end;

function TMainForm.GetSolverDetectionMethod: Integer;
begin
  Result := cbDetectMethod.ItemIndex - 1;
end;

procedure TMainForm.SetSolverDetectionMethod(Method: Integer);
begin
  if not Assigned(FSolver) then
    Exit;

  // Force a specific captcha type
  if Method >= 0 then
  begin
    case Method of
      0:
        FSolver.DetectionMethod := 0; // Widget
      1:
        FSolver.DetectionMethod := 1; // MultiSelect
      2:
        FSolver.DetectionMethod := 2; // Grid
      3:
        FSolver.DetectionMethod := 3; // Bbox
      4:
        FSolver.DetectionMethod := 4; // BboxDD
    end;
    AddLog(Format('Type de CAPTCHA forcé: %s', [cbDetectMethod.Items[Method + 1]
      ]), 'INFO');
  end
  else
    FSolver.DetectionMethod := -1; // Auto
end;

procedure TMainForm.LoadSampleCaptcha;
begin
  // Select a random sample captcha
  edURL.Text := SAMPLE_URLS[Random(Length(SAMPLE_URLS))];
  btnNavigateClick(nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveSettings;

  if Assigned(FSolver) then
    FreeAndNil(FSolver);

  if Assigned(FWVBrowser) then
    FreeAndNil(FWVBrowser);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // Adjust window parent size to panel
  WVWindowParent1.Top := 0;
  WVWindowParent1.Left := 0;
  WVWindowParent1.Width := pnlMain.ClientWidth;
  WVWindowParent1.Height := pnlMain.ClientHeight;

  // Initialize WebView
  InitializeWebView;

  if GlobalWebView2Loader.InitializationError then
    ShowMessage(GlobalWebView2Loader.ErrorMessage)
  else if GlobalWebView2Loader.Initialized then
    FWVBrowser.CreateBrowser(WVWindowParent1.Handle)
  else
    AddLog('Attente de l''initialisation de WebView2...', 'INFO');
end;

procedure TMainForm.btnNavigateClick(Sender: TObject);
begin
  if Assigned(FWVBrowser) then
  begin
    FWVBrowser.Navigate(edURL.Text);
    StatusBar1.Panels[0].Text := 'Navigation en cours...';
  end;
end;

procedure TMainForm.btnSolveClick(Sender: TObject);
begin
  if Assigned(FSolver) then
  begin
    // Update solver settings from UI
    FSolver.ApiKey := edApiKey.Text;
    FSolver.AutoSolve := chkAutoSolve.Checked;
    FSolver.AutoOpen := chkAutoOpen.Checked;
    FSolver.EnglishEnabled := chkEnglish.Checked;
    FSolver.AlwaysSolve := chkAlwaysSolve.Checked;

    // Set detection method if not automatic
    if cbDetectMethod.ItemIndex > 0 then
      SetSolverDetectionMethod(cbDetectMethod.ItemIndex - 1)
    else
      FSolver.DetectionMethod := -1; // Auto

    // Start solving
    FSolver.Solve;
  end;
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  if Assigned(FSolver) then
    FSolver.Stop;
end;

procedure TMainForm.btnRefreshIframeClick(Sender: TObject);
begin
  if Assigned(FSolver) then
    FSolver.RefreshIframes;
end;

procedure TMainForm.btnLoadCaptchaClick(Sender: TObject);
begin
  LoadSampleCaptcha;
end;

procedure TMainForm.btnSettingsClick(Sender: TObject);
begin
  SaveSettings;
  AddLog('Paramètres sauvegardés', 'INFO');
end;

procedure TMainForm.cbDetectMethodChange(Sender: TObject);
begin
  if Assigned(FSolver) and (cbDetectMethod.ItemIndex > 0) then
    SetSolverDetectionMethod(cbDetectMethod.ItemIndex - 1)
  else if Assigned(FSolver) then
    FSolver.DetectionMethod := -1; // Auto
end;

procedure TMainForm.ClickCheckboxNatively;
var
  Rect: TRect;
  CheckboxX, CheckboxY: Integer;
begin
//  // Obtenez la position du checkbox via JavaScript
//  FSolver.(
//    'const checkbox = document.querySelector("#checkbox");' +
//    'if (checkbox) {' +
//    '  const rect = checkbox.getBoundingClientRect();' +
//    '  window.checkboxX = rect.left + rect.width/2;' +
//    '  window.checkboxY = rect.top + rect.height/2;' +
//    '}', 0);
//
//  // Attendez un peu
//  Sleep(500);
//
//  // Récupérez les coordonnées
//  CheckboxX := StrToIntDef(FWVBrowser.ExecuteScriptSync('return window.checkboxX || 0;'), 0);
//  CheckboxY := StrToIntDef(FWVBrowser.ExecuteScriptSync('return window.checkboxY || 0;'), 0);
//
//  if (CheckboxX > 0) and (CheckboxY > 0) then
//  begin
//    // Simulez un clic
//    Rect := FWVBrowser.BoundsRect;
//    mouse_event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_MOVE,
//                Rect.Left + CheckboxX, Rect.Top + CheckboxY, 0, 0);
//    Sleep(100);
//    mouse_event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN,
//                Rect.Left + CheckboxX, Rect.Top + CheckboxY, 0, 0);
//    Sleep(50);
//    mouse_event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP,
//                Rect.Left + CheckboxX, Rect.Top + CheckboxY, 0, 0);
//  end;
end;


initialization

// Initialize WebView2Loader
GlobalWebView2Loader := TWVLoader.Create(nil);
GlobalWebView2Loader.UserDataFolder := ExtractFileDir(Application.ExeName) +
  '\CustomCache';
GlobalWebView2Loader.RemoteDebuggingPort := 9999;
GlobalWebView2Loader.RemoteAllowOrigins := '*';
GlobalWebView2Loader.AreBrowserExtensionsEnabled := True;
GlobalWebView2Loader.StartWebView2;

finalization

if Assigned(GlobalWebView2Loader) then
  FreeAndNil(GlobalWebView2Loader);

end.
