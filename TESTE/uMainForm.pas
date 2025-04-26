unit uMainForm;

{$I source\webview2.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, uWVBrowser,
  uWVWinControl, uWVWindowParent, uWVTypes, uWVConstants, uWVTypeLibrary, 
  uWVCoreWebView2Args, uWVBrowserBase, uWVCoreWebView2Deferral, uPopularCaptchaSolver,
  uCaptchaSonic, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    WVWindowParent1: TWVWindowParent;
    WVBrowser1: TWVBrowser;
    Panel1: TPanel;
    edtURL: TEdit;
    btnGo: TButton;
    btnGetMHTML: TButton;
    Panel2: TPanel;
    lblStatus: TLabel;
    btnSolve: TButton;
    cbxSolver: TComboBox;
    lblCaptchaSolver: TLabel;
    edtApiKey: TEdit;
    lblApiKey: TLabel;
    lstLogs: TMemo;
    PageControl1: TPageControl;
    tabLogs: TTabSheet;
    tabRaw: TTabSheet;
    memRaw: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure WVBrowser1InitializationError(Sender: TObject; aErrorCode: HRESULT; const aErrorMessage: wvstring);
    procedure WVBrowser1AfterCreated(Sender: TObject);
    procedure WVBrowser1DocumentTitleChanged(Sender: TObject);
    procedure btnGetMHTMLClick(Sender: TObject);
    procedure WVBrowser1RetrieveMHTMLCompleted(Sender: TObject; aResult: Boolean; const aMHTML: wvstring);
    procedure btnSolveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMHTMLContent: TStringList;
    FCaptchaSolver: TPopularCaptchaSolver;
    FCaptchaSonic: TCaptchaSonicSolver;
    procedure Log(const Msg: string; Level: string = 'Info');
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FMHTMLContent := TStringList.Create;
  FCaptchaSolver := nil;
  FCaptchaSonic := nil;
  
  cbxSolver.Items.Add('Popular Captcha Solver');
  cbxSolver.Items.Add('CaptchaSonic');
  cbxSolver.ItemIndex := 0;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FCaptchaSolver) then
    FCaptchaSolver.Free;
    
  if Assigned(FCaptchaSonic) then
    FCaptchaSonic.Free;
    
  FMHTMLContent.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  WVBrowser1.CreateBrowser(WVWindowParent1.Handle);
end;

procedure TMainForm.Log(const Msg: string; Level: string);
var
  LogMsg: string;
begin
  LogMsg := FormatDateTime('[hh:nn:ss.zzz]', Now) + ' [' + Level + '] ' + Msg;
  lstLogs.Lines.Add(LogMsg);
  
  // Auto-scroll to bottom
  lstLogs.SelStart := Length(lstLogs.Text);
  lstLogs.SelLength := 0;
  SendMessage(lstLogs.Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TMainForm.btnGetMHTMLClick(Sender: TObject);
begin
  FMHTMLContent.Clear;
  memRaw.Clear;
  WVBrowser1.RetrieveMHTML;
end;

procedure TMainForm.btnGoClick(Sender: TObject);
begin
  WVBrowser1.Navigate(edtURL.Text);
end;

procedure TMainForm.btnSolveClick(Sender: TObject);
begin
  if cbxSolver.ItemIndex = 0 then
  begin
    // Popular Captcha Solver
    if not Assigned(FCaptchaSolver) then
    begin
      FCaptchaSolver := TPopularCaptchaSolver.Create(WVBrowser1);
      FCaptchaSolver.OnLog := procedure(Msg: string; Level: string)
      begin
        Log(Msg, Level);
      end;
    end;
    
    FCaptchaSolver.ApiKey := edtApiKey.Text;
    Log('Iniciando solução com Popular Captcha Solver');
    FCaptchaSolver.Solve;
  end
  else
  begin
    // CaptchaSonic Solver
    if not Assigned(FCaptchaSonic) then
    begin
      FCaptchaSonic := TCaptchaSonicSolver.Create(WVBrowser1);
      FCaptchaSonic.OnLog := procedure(const Msg: string)
      begin
        Log(Msg);
      end;
      FCaptchaSonic.OnResult := procedure(Sender: TObject; Success: Boolean; const Token: string)
      begin
        if Success then
          Log('CaptchaSonic: Captcha resolvido com sucesso, token: ' + Token, 'Success')
        else
          Log('CaptchaSonic: Falha ao resolver captcha: ' + Token, 'Error');
      end;
    end;
    
    FCaptchaSonic.ApiKey := edtApiKey.Text;
    Log('Iniciando solução com CaptchaSonic');
    FCaptchaSonic.Solve;
  end;
end;

procedure TMainForm.WVBrowser1AfterCreated(Sender: TObject);
begin
  WVWindowParent1.UpdateSize;
  lblStatus.Caption := 'Navegador pronto';
  
  // Navegar para URL inicial se o campo estiver preenchido
  if edtURL.Text <> '' then
    WVBrowser1.Navigate(edtURL.Text)
  else
    WVBrowser1.Navigate('https://www.google.com');
end;

procedure TMainForm.WVBrowser1DocumentTitleChanged(Sender: TObject);
begin
  Caption := 'Testador de CAPTCHAs - ' + WVBrowser1.DocumentTitle;
end;

procedure TMainForm.WVBrowser1InitializationError(Sender: TObject; aErrorCode: HRESULT; const aErrorMessage: wvstring);
begin
  lblStatus.Caption := 'Erro: ' + aErrorMessage;
end;

procedure TMainForm.WVBrowser1RetrieveMHTMLCompleted(Sender: TObject; aResult: Boolean; const aMHTML: wvstring);
begin
  if aResult then
  begin
    FMHTMLContent.Text := aMHTML;
    memRaw.Text := aMHTML;
    Log('MHTML capturado com sucesso: ' + IntToStr(Length(aMHTML)) + ' bytes', 'Success');
  end
  else
    Log('Falha ao capturar MHTML', 'Error');
end;

end.
