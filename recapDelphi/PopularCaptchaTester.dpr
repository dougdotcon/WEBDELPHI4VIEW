program PopularCaptchaTester;

{$DEFINE STARTUPMODE}

{$R *.res}

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uPopularCaptchaSolver in 'uPopularCaptchaSolver.pas',
  WebView4Delphi in 'WebView4Delphi.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.