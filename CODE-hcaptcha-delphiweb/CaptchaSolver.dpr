program CaptchaSolver;

uses
  Vcl.Forms,
  uWindowlessBrowser in 'WindowlessBrowser\uWindowlessBrowser.pas' {MainForm},
  uCaptchaSonicClient in 'src\api\uCaptchaSonicClient.pas',
  uCaptchaTypes in 'src\types\uCaptchaTypes.pas',
  uHCaptchaAutomation in 'src\automation\uHCaptchaAutomation.pas',
  uImageProcessor in 'src\utils\uImageProcessor.pas',
  uLogger in 'src\utils\uLogger.pas',
  uConfig in 'src\utils\uConfig.pas',
  uDirectCompositionHost in 'WindowlessBrowser\uDirectCompositionHost.pas',
  WebView2 in 'WebView2.pas',
  WebView2.Loader in 'WebView2.Loader.pas',
  uWVBrowser in 'BIN-WebView4Delphi\source\uWVBrowser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.