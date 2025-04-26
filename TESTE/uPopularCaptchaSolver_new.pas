unit uPopularCaptchaSolver;

{$I source\webview2.inc}

interface

uses
  System.SysUtils, System.Classes, System.JSON, 
  System.Variants, System.RegularExpressions, System.Generics.Collections,
  System.SyncObjs, System.IOUtils, Winapi.WinInet, System.TypInfo, Vcl.Forms,
  Vcl.Dialogs,
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.NetEncoding, uWVBrowser, uWVTypes,
  uWVConstants, uWVTypeLibrary, uWVLibFunctions, uWVLoader, uWVInterfaces,
  uWVCoreWebView2Args, uWVBrowserBase, uWVWindowParent, uWVWinControl;

type
  TLogLevel = (llDebug, llInfo, llWarn, llError);
  TLogEvent = TProc<string, string>;
  TCaptchaType = (ctUnknown, ctWidget, ctMultiSelect, ctGrid, ctBbox, ctBboxDD);

  TPopularCaptchaSolver = class
  private
    FWebBrowser: TWVBrowser;
    FApiKey: string;
    FApiEndpoint: string;
    FOnLog: TLogEvent;
    FIsProcessing: Boolean;
    FScriptResult: wvstring;
    FScriptCompleted: Boolean;
    FAutoSolve: Boolean;
    FAutoOpen: Boolean;
    FEnglishEnabled: Boolean;
    FAlwaysSolve: Boolean;
    FMaxRetries: Integer;
    FCurrentRetryCount: Integer;
    FDetectionMethod: Integer;
    FMHTMLContent: TStringList;
    FCaptchaType: TCaptchaType;

    procedure OnRetrieveMHTMLCompleted(Sender: TObject;
      aResult: Boolean; const aMHTML: wvstring);
    function ExecScriptSync(const Script: string): string;
    procedure Log(const Msg: string; Level: string);

  public
    constructor Create(WebBrowser: TWVBrowser);
    destructor Destroy; override;
    procedure RefreshIframes;
    procedure Solve;
    procedure Stop;

    property ApiKey: string read FApiKey write FApiKey;
    property AutoSolve: Boolean read FAutoSolve write FAutoSolve;
    property AutoOpen: Boolean read FAutoOpen write FAutoOpen;
    property EnglishEnabled: Boolean read FEnglishEnabled write FEnglishEnabled;
    property AlwaysSolve: Boolean read FAlwaysSolve write FAlwaysSolve;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    property DetectionMethod: Integer read FDetectionMethod
      write FDetectionMethod;
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

implementation

uses
  System.Math, System.StrUtils;

{ TPopularCaptchaSolver }

constructor TPopularCaptchaSolver.Create(WebBrowser: TWVBrowser);
begin
  inherited Create;
  FWebBrowser := WebBrowser;
  FApiEndpoint := 'https://api.captchasonic.com/createTask';
  FIsProcessing := False;
  FMaxRetries := 3;
  FCurrentRetryCount := 0;
  FAutoSolve := True;
  FAutoOpen := True;
  FEnglishEnabled := True;
  FAlwaysSolve := False;
  FDetectionMethod := -1; // Auto detection
  FMHTMLContent := TStringList.Create;
end;

destructor TPopularCaptchaSolver.Destroy;
begin
  FWebBrowser := nil;
  FOnLog := nil;
  FMHTMLContent.Free;
  inherited;
end;

procedure TPopularCaptchaSolver.Log(const Msg: string; Level: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Msg, Level);
end;

function TPopularCaptchaSolver.ExecScriptSync(const Script: string): string;
begin
  Result := '';
  // Implementação simplificada
  if Assigned(FWebBrowser) then
  begin
    try
      Log('Executando script: ' + Script, 'Debug');
    except
      on E: Exception do
        Log('Erro ao executar script: ' + E.Message, 'Error');
    end;
  end;
end;

procedure TPopularCaptchaSolver.OnRetrieveMHTMLCompleted(Sender: TObject;
  aResult: Boolean; const aMHTML: wvstring);
begin
  if aResult then
  begin
    FMHTMLContent.Text := aMHTML;
    Log('MHTML capturado com sucesso: ' + IntToStr(Length(aMHTML)) + ' bytes', 'Info');
  end
  else
    Log('Falha ao capturar MHTML', 'Error');
end;

procedure TPopularCaptchaSolver.RefreshIframes;
begin
  Log('RefreshIframes chamado', 'Info');
  // Implementação simplificada
end;

procedure TPopularCaptchaSolver.Solve;
begin
  Log('Iniciando solução do CAPTCHA...', 'Info');
  // Implementação simplificada
end;

procedure TPopularCaptchaSolver.Stop;
begin
  Log('Interrompendo operação...', 'Info');
  // Implementação simplificada
end;

end. 