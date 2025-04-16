program PopularCaptchaTester;

uses
  Vcl.Forms,
  uPopularCaptchaSolver in 'uPopularCaptchaSolver.pas',
  uMainForm in 'uMainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.
