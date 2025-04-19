unit uPopularCaptchaSolver;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Net.URLClient, WebView4Delphi;

type
  // Tipo do método de callback para log
  TLogCallback = procedure(const AMessage: string) of object;
  
  // Tipo do método de callback para resultado da solução
  TResultCallback = procedure(Success: Boolean; const Message: string) of object;

  // Tipo do método de detecção de captcha
  TCaptchaDetectionMethod = (cdmAuto, cdmWidget, cdmMultiSelect, cdmGrid, cdmBbox, cdmBboxDD);

  // Classe principal para resolver captchas
  TPopularCaptchaSolver = class
  private
    FWebView: TWebView;
    FApiKey: string;
    FApiEndpoint: string;
    FDetectionMethod: TCaptchaDetectionMethod;
    FOnLog: TLogCallback;
    
    procedure Log(const AMessage: string);
    
  public
    constructor Create(AWebView: TWebView);
    destructor Destroy; override;
    
    // Método principal para resolver captcha com callback
    procedure Solve(ACallback: TResultCallback);
    
    // Propriedades
    property ApiKey: string read FApiKey write FApiKey;
    property ApiEndpoint: string read FApiEndpoint write FApiEndpoint;
    property DetectionMethod: TCaptchaDetectionMethod read FDetectionMethod write FDetectionMethod;
    property OnLog: TLogCallback read FOnLog write FOnLog;
  end;

implementation

{ TPopularCaptchaSolver }

constructor TPopularCaptchaSolver.Create(AWebView: TWebView);
begin
  FWebView := AWebView;
  FApiKey := '';
  FApiEndpoint := 'https://api.captchasolver.com/solve';
  FDetectionMethod := cdmAuto;
end;

destructor TPopularCaptchaSolver.Destroy;
begin
  inherited;
end;

procedure TPopularCaptchaSolver.Log(const AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(AMessage);
end;

procedure TPopularCaptchaSolver.Solve(ACallback: TResultCallback);
var
  CallbackProc: TThreadProcedure;
begin
  // Verificar se temos uma chave API
  if FApiKey = '' then
  begin
    Log('Erro: Chave API não definida');
    if Assigned(ACallback) then
      ACallback(False, 'Chave API não definida');
    Exit;
  end;

  // Verificar se temos um endpoint
  if FApiEndpoint = '' then
  begin
    Log('Erro: Endpoint API não definido');
    if Assigned(ACallback) then
      ACallback(False, 'Endpoint API não definido');
    Exit;
  end;

  // Verificar se o WebView está inicializado
  if not Assigned(FWebView) then
  begin
    Log('Erro: WebView não inicializado');
    if Assigned(ACallback) then
      ACallback(False, 'WebView não inicializado');
    Exit;
  end;

  Log('Iniciando processo de solução de captcha...');
  Log('Método de detecção: ' + Ord(FDetectionMethod).ToString);

  // Capturamos o callback em uma variável
  CallbackProc := procedure
  begin
    Log('Captcha detectado e resolvido com sucesso (simulação)');
          
    if Assigned(ACallback) then
      ACallback(True, 'Captcha resolvido com sucesso');
  end;

  // Simulamos uma chamada API com sucesso após um atraso
  TThread.CreateAnonymousThread(
    procedure
    begin
      // Simulando processamento
      Sleep(3000);
      
      TThread.Synchronize(TThread.Current, CallbackProc);
    end
  ).Start;
end;

end.
