program PopularCaptchaTester;

{$I source\webview2.inc}

uses
  Vcl.Forms,
  uPopularCaptchaSolver in 'uPopularCaptchaSolver.pas',
  uMainForm in 'uMainForm.pas' {MainForm},
  uCaptchaSonic in 'uCaptchaSonic.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.
