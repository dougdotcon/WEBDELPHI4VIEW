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
  uWebView2Helper in 'src\utils\uWebView2Helper.pas',
  uDirectCompositionHost in 'WindowlessBrowser\uDirectCompositionHost.pas',
  WebView2 in 'WebView2.pas',
  WebView2.Loader in 'WebView2.Loader.pas',
  uWVTypes in 'source\uWVTypes.pas',
  uWVConstants in 'source\uWVConstants.pas',
  uWVTypeLibrary in 'source\uWVTypeLibrary.pas',
  uWVLibFunctions in 'source\uWVLibFunctions.pas',
  uWVLoader in 'source\uWVLoader.pas',
  uWVInterfaces in 'source\uWVInterfaces.pas',
  uWVEvents in 'source\uWVEvents.pas',
  uWVCoreWebView2 in 'source\uWVCoreWebView2.pas',
  uWVCoreWebView2Settings in 'source\uWVCoreWebView2Settings.pas',
  uWVCoreWebView2Environment in 'source\uWVCoreWebView2Environment.pas',
  uWVCoreWebView2Controller in 'source\uWVCoreWebView2Controller.pas',
  uWVCoreWebView2PrintSettings in 'source\uWVCoreWebView2PrintSettings.pas',
  uWVCoreWebView2CompositionController in 'source\uWVCoreWebView2CompositionController.pas',
  uWVCoreWebView2CookieManager in 'source\uWVCoreWebView2CookieManager.pas',
  uWVCoreWebView2Delegates in 'source\uWVCoreWebView2Delegates.pas',
  uWVCoreWebView2Profile in 'source\uWVCoreWebView2Profile.pas',
  uWVMiscFunctions in 'source\uWVMiscFunctions.pas',
  uWVCoreWebView2EnvironmentOptions in 'source\uWVCoreWebView2EnvironmentOptions.pas',
  uWVCoreWebView2ControllerOptions in 'source\uWVCoreWebView2ControllerOptions.pas',
  uWVCoreWebView2CustomSchemeRegistration in 'source\uWVCoreWebView2CustomSchemeRegistration.pas',
  uWVBrowserBase in 'source\uWVBrowserBase.pas',
  uWVBrowser in 'source\uWVBrowser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.