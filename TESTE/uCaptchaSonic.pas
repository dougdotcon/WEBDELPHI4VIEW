unit uCaptchaSonic;

{$I source\webview2.inc}

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.NetEncoding, Vcl.Graphics, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage, Vcl.Forms,
  uWVBrowser, uWVTypes, uWVConstants, uWVTypeLibrary, uWVLibFunctions, 
  uWVLoader, uWVInterfaces, uWVBrowserBase;

type
  TCaptchaSonicSolverMode = (smObjectClick, smObjectDrag, smObjectClassify);

  TCaptchaSonicResultEvent = procedure(Sender: TObject; Success: Boolean; const Token: string) of object;

  TCaptchaSonicSolver = class
  private
    FWebBrowser: TWVBrowser;
    FApiKey: string;
    FApiEndpoint: string;
    FSiteURL: string;
    FSiteKey: string;
    FMHTMLContent: TStringList;
    FMode: TCaptchaSonicSolverMode;
    FOnResult: TCaptchaSonicResultEvent;
    FOnLog: TProc<string>;
    
    procedure Log(const Msg: string);
    
  public
    constructor Create(WebBrowser: TWVBrowser);
    destructor Destroy; override;

    procedure Solve;
    procedure SetMHTMLContent(const aMHTML: string);

    property ApiKey: string read FApiKey write FApiKey;
    property ApiEndpoint: string read FApiEndpoint write FApiEndpoint;
    property Mode: TCaptchaSonicSolverMode read FMode write FMode;
    property SiteURL: string read FSiteURL write FSiteURL;
    property SiteKey: string read FSiteKey write FSiteKey;
    property OnResult: TCaptchaSonicResultEvent read FOnResult write FOnResult;
    property OnLog: TProc<string> read FOnLog write FOnLog;
  end;

implementation

uses
  System.StrUtils, System.Math, System.RegularExpressions;

{ TCaptchaSonicSolver }

constructor TCaptchaSonicSolver.Create(WebBrowser: TWVBrowser);
begin
  inherited Create;
  FWebBrowser := WebBrowser;
  FApiEndpoint := 'https://api.captchasonic.com/createTask';
  FMHTMLContent := TStringList.Create;
  FMode := smObjectClick;
end;

destructor TCaptchaSonicSolver.Destroy;
begin
  FMHTMLContent.Free;
  inherited;
end;

procedure TCaptchaSonicSolver.Log(const Msg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Msg);
end;

procedure TCaptchaSonicSolver.Solve;
begin
  Log('Iniciando solução do CAPTCHA...');
  
  // Implementação simplificada
  if Assigned(FOnResult) then
    FOnResult(Self, False, 'Funcionalidade não implementada');
end;

procedure TCaptchaSonicSolver.SetMHTMLContent(const aMHTML: string);
begin
  FMHTMLContent.Text := aMHTML;
  Log('MHTML definido manualmente: ' + IntToStr(Length(aMHTML)) + ' bytes');
end;

end. 