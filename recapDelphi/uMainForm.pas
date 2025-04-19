unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Buttons, WebView4Delphi, uPopularCaptchaSolver, System.JSON,
  System.Net.HttpClient, System.Net.URLClient, System.Generics.Collections;

type
  TMainForm = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlCenter: TPanel;
    pnlWebView: TPanel;
    pnlControls: TPanel;
    pnlLog: TPanel;
    edtUrl: TEdit;
    btnNavigate: TButton;
    btnSolve: TButton;
    btnClear: TButton;
    mmoLog: TMemo;
    lblUrl: TLabel;
    lblApiKey: TLabel;
    edtApiKey: TEdit;
    lblApiEndpoint: TLabel;
    edtApiEndpoint: TEdit;
    chkAutoSolve: TCheckBox;
    chkAutoOpen: TCheckBox;
    cmbDetectionMethod: TComboBox;
    lblDetectionMethod: TLabel;
    btnSettings: TButton;
    btnRefresh: TButton;
    btnBack: TButton;
    btnForward: TButton;
    btnStop: TButton;
    lblStatus: TLabel;
    pnlStatus: TPanel;
    chkEnglish: TCheckBox;
    btnClearLog: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnNavigateClick(Sender: TObject);
    procedure btnSolveClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnForwardClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure chkEnglishClick(Sender: TObject);
    procedure cmbDetectionMethodChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

  protected
    procedure WndProc(var Message: TMessage); override;

  private
    FWebView: TWebView;
    FCaptchaSolver: TPopularCaptchaSolver;
    FScriptQueue: TQueue<string>;
    FIsProcessing: Boolean;
    FLanguageIsEnglish: Boolean;

    procedure InitializeWebView;
    procedure InitializeControls;
    procedure Log(const AMessage: string);
    procedure UpdateStatus(const AStatus: string);
    procedure ExecuteNextScript;
    procedure ProcessCaptchaResult(Success: Boolean; const Message: string);
    procedure WebViewNavigationCompleted(Sender: TObject; IsSuccess: Boolean; WebErrorStatus: HRESULT);
    // Métodos declarados mas não utilizados - comentados para evitar warnings
    // procedure WebViewNavigationStarting(Sender: TObject);
    // procedure WebViewWebMessageReceived(Sender: TObject; const AWebMessageAsJson: string);
    procedure WebViewAfterCreated(Sender: TObject);
    // procedure WebViewInitializationError(Sender: TObject; aErrorCode: HRESULT; const aErrorMessage: string);
    procedure ApplyLanguage;
    function GetDetectionMethod: uPopularCaptchaSolver.TCaptchaDetectionMethod;
    procedure SaveSettings;
    procedure LoadSettings;

  public
    procedure SolveCaptcha;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  System.IniFiles, Vcl.FileCtrl;

// Mensagens do Windows para tratamento de movimentação da janela
const
  WM_MOVE   = $0003;
  WM_MOVING = $0216;

type
  TWMMove = packed record
    Msg: Cardinal;
    Unused: Integer;
    Result: Longint;
  end;

// Implementação do método WndProc
procedure TMainForm.WndProc(var Message: TMessage);
begin
  inherited;

  case Message.Msg of
    WM_MOVE, WM_MOVING:
      if Assigned(FWebView) then
        ; // Não fazemos nada, o TWebView simplificado não tem NotifyParentWindowPositionChanged
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FScriptQueue := TQueue<string>.Create;
  FIsProcessing := False;
  FLanguageIsEnglish := False;

  LoadSettings;
  InitializeControls;
  InitializeWebView;
  ApplyLanguage;

  FCaptchaSolver := TPopularCaptchaSolver.Create(FWebView);
  FCaptchaSolver.ApiKey := edtApiKey.Text;
  FCaptchaSolver.ApiEndpoint := edtApiEndpoint.Text;
  FCaptchaSolver.OnLog := Log;

  // A navegação será feita após o WebView ser criado
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  FCaptchaSolver.Free;
  FScriptQueue.Free;
  FWebView.Free;
end;

procedure TMainForm.InitializeWebView;
begin
  // Cria o componente WebView
  FWebView := TWebView.Create;
  FWebView.DefaultURL := 'about:blank';
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if Assigned(FWebView) then
    FWebView.CreateWebView;
  
  // Simulamos o evento AfterCreated após um pequeno atraso
  Timer1.Interval := 1000;
  Timer1.Enabled := True;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  WebViewAfterCreated(nil);
end;

procedure TMainForm.WebViewAfterCreated(Sender: TObject);
begin
  Log('WebView criado com sucesso (simulado)');
  UpdateStatus('Pronto');

  if edtUrl.Text <> '' then
    btnNavigateClick(nil);
end;

procedure TMainForm.btnNavigateClick(Sender: TObject);
var
  NavCompletedProc: TThreadProcedure;
begin
  if edtUrl.Text <> '' then
  begin
    UpdateStatus('Navegando...');
    FWebView.Navigate(edtUrl.Text);
    
    // Simulamos o evento NavigationCompleted após um pequeno atraso
    NavCompletedProc := procedure
    begin
      WebViewNavigationCompleted(nil, True, 0);
    end;
    
    TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(2000);
        TThread.Synchronize(TThread.Current, NavCompletedProc);
      end
    ).Start;
  end;
end;

procedure TMainForm.btnSolveClick(Sender: TObject);
begin
  SolveCaptcha;
end;

procedure TMainForm.SolveCaptcha;
begin
  if FIsProcessing then
  begin
    Log('Já existe um processo de solução em andamento.');
    Exit;
  end;

  FCaptchaSolver.ApiKey := edtApiKey.Text;
  FCaptchaSolver.ApiEndpoint := edtApiEndpoint.Text;
  FCaptchaSolver.DetectionMethod := GetDetectionMethod;

  UpdateStatus('Solucionando captcha...');
  FIsProcessing := True;

  try
    FCaptchaSolver.Solve(ProcessCaptchaResult);
  except
    on E: Exception do
    begin
      Log('Erro ao iniciar solução: ' + E.Message);
      FIsProcessing := False;
      UpdateStatus('Erro');
    end;
  end;
end;

procedure TMainForm.ProcessCaptchaResult(Success: Boolean; const Message: string);
begin
  FIsProcessing := False;

  if Success then
  begin
    Log('Captcha resolvido com sucesso: ' + Message);
    UpdateStatus('Resolvido');
  end
  else
  begin
    Log('Falha ao resolver captcha: ' + Message);
    UpdateStatus('Falha');
  end;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
var
  NavCompletedProc: TThreadProcedure;
begin
  edtUrl.Clear;
  FWebView.Navigate('about:blank');
  
  NavCompletedProc := procedure
  begin
    WebViewNavigationCompleted(nil, True, 0);
  end;
  
  // Simulamos o evento NavigationCompleted após um pequeno atraso
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(500);
      TThread.Synchronize(TThread.Current, NavCompletedProc);
    end
  ).Start;
end;

procedure TMainForm.btnRefreshClick(Sender: TObject);
var
  NavCompletedProc: TThreadProcedure;
begin
  FWebView.Refresh;
  
  NavCompletedProc := procedure
  begin
    WebViewNavigationCompleted(nil, True, 0);
  end;
  
  // Simulamos o evento NavigationCompleted após um pequeno atraso
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(1000);
      TThread.Synchronize(TThread.Current, NavCompletedProc);
    end
  ).Start;
end;

procedure TMainForm.btnBackClick(Sender: TObject);
var
  NavCompletedProc: TThreadProcedure;
begin
  // Versão simplificada não implementa CanGoBack
  FWebView.GoBack;
  
  NavCompletedProc := procedure
  begin
    WebViewNavigationCompleted(nil, True, 0);
  end;
  
  // Simulamos o evento NavigationCompleted após um pequeno atraso
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(1000);
      TThread.Synchronize(TThread.Current, NavCompletedProc);
    end
  ).Start;
end;

procedure TMainForm.btnForwardClick(Sender: TObject);
var
  NavCompletedProc: TThreadProcedure;
begin
  // Versão simplificada não implementa CanGoForward
  FWebView.GoForward;
  
  NavCompletedProc := procedure
  begin
    WebViewNavigationCompleted(nil, True, 0);
  end;
  
  // Simulamos o evento NavigationCompleted após um pequeno atraso
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(1000);
      TThread.Synchronize(TThread.Current, NavCompletedProc);
    end
  ).Start;
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  FWebView.Stop;
  FIsProcessing := False;
  UpdateStatus('Parado');
end;

procedure TMainForm.btnClearLogClick(Sender: TObject);
begin
  mmoLog.Clear;
end;

procedure TMainForm.btnSettingsClick(Sender: TObject);
begin
  // Implementar tela de configurações avançadas
  ShowMessage('Configurações avançadas não implementadas.');
end;

procedure TMainForm.chkEnglishClick(Sender: TObject);
begin
  FLanguageIsEnglish := chkEnglish.Checked;
  ApplyLanguage;
end;

procedure TMainForm.cmbDetectionMethodChange(Sender: TObject);
begin
  FCaptchaSolver.DetectionMethod := GetDetectionMethod;
end;

procedure TMainForm.WebViewNavigationCompleted(Sender: TObject; IsSuccess: Boolean; WebErrorStatus: HRESULT);
begin
  if IsSuccess then
  begin
    UpdateStatus('Carregado');
    Log('Página carregada: ' + FWebView.Source);

    if chkAutoOpen.Checked then
    begin
      // Implementar detecção e abertura automática do captcha
    end;

    if chkAutoSolve.Checked then
    begin
      SolveCaptcha;
    end;
  end
  else
  begin
    UpdateStatus('Erro: ' + IntToStr(WebErrorStatus));
    Log('Erro ao carregar página: ' + IntToStr(WebErrorStatus));
  end;
end;

procedure TMainForm.Log(const AMessage: string);
var
  LogProc: TThreadProcedure;
begin
  LogProc := procedure
  begin
    mmoLog.Lines.Add(FormatDateTime('[dd/mm/yyyy hh:nn:ss] ', Now) + AMessage);
    mmoLog.SelStart := Length(mmoLog.Text);
    SendMessage(mmoLog.Handle, EM_SCROLLCARET, 0, 0);
  end;
  
  TThread.Synchronize(TThread.Current, LogProc);
end;

procedure TMainForm.UpdateStatus(const AStatus: string);
var
  StatusProc: TThreadProcedure;
begin
  StatusProc := procedure
  begin
    lblStatus.Caption := AStatus;
  end;
  
  TThread.Synchronize(TThread.Current, StatusProc);
end;

procedure TMainForm.ExecuteNextScript;
var
  Script: string;
  ExecuteScriptProc: TThreadProcedure;
begin
  if FScriptQueue.Count > 0 then
  begin
    Script := FScriptQueue.Dequeue;
    
    // Em nossa versão simplificada, simulamos a execução do script
    Log('Script executado (simulado)');
    ExecuteNextScript;
    
    // Versão real (comentada)
    {
    FWebView.ExecuteScript(Script,
      procedure(ResultObjectAsJson: string; Success: Boolean)
      begin
        if Success then
          Log('Script executado com sucesso')
        else
          Log('Erro ao executar script: ' + ResultObjectAsJson);

        ExecuteNextScript;
      end
    );
    }
  end
  else
  begin
    FIsProcessing := False;
  end;
end;

function TMainForm.GetDetectionMethod: uPopularCaptchaSolver.TCaptchaDetectionMethod;
begin
  Result := uPopularCaptchaSolver.TCaptchaDetectionMethod(cmbDetectionMethod.ItemIndex);
end;

procedure TMainForm.ApplyLanguage;
begin
  if FLanguageIsEnglish then
  begin
    Caption := 'Popular Captcha Tester';
    lblUrl.Caption := 'URL:';
    lblApiKey.Caption := 'API Key:';
    lblApiEndpoint.Caption := 'API Endpoint:';
    lblDetectionMethod.Caption := 'Detection Method:';
    btnNavigate.Caption := 'Navigate';
    btnSolve.Caption := 'Solve';
    btnClear.Caption := 'Clear';
    btnRefresh.Caption := 'Refresh';
    btnBack.Caption := 'Back';
    btnForward.Caption := 'Forward';
    btnStop.Caption := 'Stop';
    btnSettings.Caption := 'Settings';
    btnClearLog.Caption := 'Clear Log';
    chkAutoSolve.Caption := 'Auto Solve';
    chkAutoOpen.Caption := 'Auto Open';
    chkEnglish.Caption := 'English';
  end
  else
  begin
    Caption := 'Testador de Captcha Popular';
    lblUrl.Caption := 'URL:';
    lblApiKey.Caption := 'Chave API:';
    lblApiEndpoint.Caption := 'Endpoint API:';
    lblDetectionMethod.Caption := 'Método de Detecção:';
    btnNavigate.Caption := 'Navegar';
    btnSolve.Caption := 'Resolver';
    btnClear.Caption := 'Limpar';
    btnRefresh.Caption := 'Atualizar';
    btnBack.Caption := 'Voltar';
    btnForward.Caption := 'Avançar';
    btnStop.Caption := 'Parar';
    btnSettings.Caption := 'Configurações';
    btnClearLog.Caption := 'Limpar Log';
    chkAutoSolve.Caption := 'Auto Resolver';
    chkAutoOpen.Caption := 'Auto Abrir';
    chkEnglish.Caption := 'Inglês';
  end;
end;

procedure TMainForm.SaveSettings;
var
  IniFile: TIniFile;
  SettingsDir: string;
begin
  SettingsDir := ExtractFilePath(Application.ExeName);
  IniFile := TIniFile.Create(SettingsDir + 'settings.ini');
  try
    IniFile.WriteString('General', 'URL', edtUrl.Text);
    IniFile.WriteString('API', 'Key', edtApiKey.Text);
    IniFile.WriteString('API', 'Endpoint', edtApiEndpoint.Text);
    IniFile.WriteBool('Options', 'AutoSolve', chkAutoSolve.Checked);
    IniFile.WriteBool('Options', 'AutoOpen', chkAutoOpen.Checked);
    IniFile.WriteBool('Options', 'English', chkEnglish.Checked);
    IniFile.WriteInteger('Options', 'DetectionMethod', cmbDetectionMethod.ItemIndex);
  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.LoadSettings;
var
  IniFile: TIniFile;
  SettingsDir: string;
begin
  SettingsDir := ExtractFilePath(Application.ExeName);
  if not FileExists(SettingsDir + 'settings.ini') then
  begin
    // Valores padrão
    edtUrl.Text := '';
    edtApiKey.Text := '';
    edtApiEndpoint.Text := 'https://api.captchasolver.com/solve';
    chkAutoSolve.Checked := False;
    chkAutoOpen.Checked := False;
    chkEnglish.Checked := False;
    Exit;
  end;

  IniFile := TIniFile.Create(SettingsDir + 'settings.ini');
  try
    edtUrl.Text := IniFile.ReadString('General', 'URL', '');
    edtApiKey.Text := IniFile.ReadString('API', 'Key', '');
    edtApiEndpoint.Text := IniFile.ReadString('API', 'Endpoint', 'https://api.captchasolver.com/solve');
    chkAutoSolve.Checked := IniFile.ReadBool('Options', 'AutoSolve', False);
    chkAutoOpen.Checked := IniFile.ReadBool('Options', 'AutoOpen', False);
    chkEnglish.Checked := IniFile.ReadBool('Options', 'English', False);
    cmbDetectionMethod.ItemIndex := IniFile.ReadInteger('Options', 'DetectionMethod', 0);
    FLanguageIsEnglish := chkEnglish.Checked;
  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  // Notifica o WebView sobre mudanças no tamanho da janela
  // Na versão simplificada, não fazemos nada
end;

procedure TMainForm.InitializeControls;
begin
  cmbDetectionMethod.Items.Clear;
  cmbDetectionMethod.Items.Add('Auto');
  cmbDetectionMethod.Items.Add('Widget');
  cmbDetectionMethod.Items.Add('MultiSelect');
  cmbDetectionMethod.Items.Add('Grid');
  cmbDetectionMethod.Items.Add('Bbox');
  cmbDetectionMethod.Items.Add('BboxDD');
  cmbDetectionMethod.ItemIndex := 0;
end;

end.