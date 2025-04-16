unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.JSON,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls,
  uWVBrowser, uWVWinControl, uWVWindowParent, uWVTypes, uWVConstants,
  uWVTypeLibrary, uWVLibFunctions, uWVLoader, uWVInterfaces,
  uCaptchaTypes, uCaptchaSonicClient, uHCaptchaAutomation,
  uImageProcessor, uLogger, uConfig;

type
  TMainForm = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlMain: TPanel;
    lblURL: TLabel;
    edtURL: TEdit;
    btnStart: TButton;
    mmLog: TMemo;
    StatusBar: TStatusBar;
    WVBrowser1: TWVBrowser;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure WVBrowser1AfterCreated(Sender: TObject);
    procedure WVBrowser1DocumentTitleChanged(Sender: TObject);
    procedure WVBrowser1WebMessageReceived(Sender: TObject; const aWebView: ICoreWebView2; const aArgs: ICoreWebView2WebMessageReceivedEventArgs);
  private
    FConfig: TConfig;
    FCaptchaSonic: TCaptchaSonicClient;
    FImageProcessor: TImageProcessor;
    FAutomation: THCaptchaAutomation;
    FRunning: Boolean;
    FCurrentWebsiteKey: string;
    
    procedure InitializeComponents;
    procedure UpdateStatus(const Status: string);
    procedure LogMessage(const Msg: string);
    function ExtractWebsiteKey(const JsonStr: string): string;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FConfig := TConfig.Create('config.ini');
  
  TLogger.Initialize(FConfig.LogFile, FConfig.LogLevel);
  TLogger.GetInstance.LogInfo('Application started');
  
  InitializeComponents;
  UpdateStatus('Ready');

  if GlobalWebView2Loader.InitializationError then
    ShowMessage(GlobalWebView2Loader.ErrorMessage)
  else
    WVBrowser1.CreateBrowser(pnlMain.Handle);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FAutomation.Free;
  FImageProcessor.Free;
  FCaptchaSonic.Free;
  FConfig.Free;
  
  TLogger.GetInstance.LogInfo('Application terminated');
  TLogger.Finalize;
end;

procedure TMainForm.InitializeComponents;
begin
  // Create components
  FCaptchaSonic := TCaptchaSonicClient.Create(FConfig.CaptchaConfig);
  FImageProcessor := TImageProcessor.Create;
  FAutomation := THCaptchaAutomation.Create(FConfig.CaptchaConfig, WVBrowser1);
  
  // Initialize UI
  FRunning := False;
  FCurrentWebsiteKey := '';
end;

procedure TMainForm.UpdateStatus(const Status: string);
begin
  StatusBar.SimpleText := Status;
  Application.ProcessMessages;
end;

procedure TMainForm.LogMessage(const Msg: string);
begin
  mmLog.Lines.Add(Format('[%s] %s', [FormatDateTime('hh:nn:ss', Now), Msg]));
  mmLog.SelStart := mmLog.GetTextLen;
  mmLog.Perform(EM_SCROLLCARET, 0, 0);
end;

function TMainForm.ExtractWebsiteKey(const JsonStr: string): string;
var
  JsonObj: TJSONObject;
begin
  Result := '';
  try
    JsonObj := TJSONObject.ParseJSONValue(JsonStr) as TJSONObject;
    try
      if JsonObj.GetValue('type').Value = 'hcaptcha_found' then
        Result := JsonObj.GetValue('sitekey').Value;
    finally
      JsonObj.Free;
    end;
  except
    on E: Exception do
      TLogger.GetInstance.LogError('Error parsing JSON: ' + E.Message);
  end;
end;

procedure TMainForm.WVBrowser1AfterCreated(Sender: TObject);
begin
  Caption := 'CaptchaSolver';
  pnlTop.Enabled := True;
end;

procedure TMainForm.WVBrowser1DocumentTitleChanged(Sender: TObject);
begin
  Caption := 'CaptchaSolver - ' + WVBrowser1.DocumentTitle;
end;

procedure TMainForm.WVBrowser1WebMessageReceived(Sender: TObject;
  const aWebView: ICoreWebView2;
  const aArgs: ICoreWebView2WebMessageReceivedEventArgs);
var
  WebsiteKey: string;
  TempArgs: TCoreWebView2WebMessageReceivedEventArgs;
begin
  TempArgs := TCoreWebView2WebMessageReceivedEventArgs.Create(aArgs);
  try
    WebsiteKey := ExtractWebsiteKey(TempArgs.WebMessageAsJson);
    
    if WebsiteKey <> '' then
    begin
      FCurrentWebsiteKey := WebsiteKey;
      TLogger.GetInstance.LogInfo('hCaptcha found with key: ' + WebsiteKey);
      LogMessage('hCaptcha detectado, iniciando solução');
      UpdateStatus('Resolvendo captcha...');
      
      if FAutomation.Solve(edtURL.Text, WebsiteKey) then
      begin
        LogMessage('Captcha resolvido com sucesso');
        UpdateStatus('Captcha resolvido');
      end
      else
      begin
        LogMessage('Falha ao resolver captcha: ' + FAutomation.LastError);
        UpdateStatus('Erro ao resolver captcha');
      end;
    end;
  finally
    TempArgs.Free;
  end;
end;

procedure TMainForm.btnStartClick(Sender: TObject);
begin
  if Trim(edtURL.Text) = '' then
  begin
    ShowMessage('Por favor, insira uma URL válida.');
    Exit;
  end;
  
  btnStart.Enabled := False;
  FRunning := True;
  FCurrentWebsiteKey := '';
  
  LogMessage('Iniciando navegação para: ' + edtURL.Text);
  UpdateStatus('Carregando página...');
  
  WVBrowser1.Navigate(edtURL.Text);
end;

initialization
  GlobalWebView2Loader := TWVLoader.Create(nil);
  GlobalWebView2Loader.UserDataFolder := ExtractFileDir(Application.ExeName) + '\Cache';
  GlobalWebView2Loader.StartWebView2;

end. 