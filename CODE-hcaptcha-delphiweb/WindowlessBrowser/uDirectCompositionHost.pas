unit uDirectCompositionHost;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Graphics,
  uWebView2Helper;

type
  TNavigationCompletedEvent = procedure(Sender: TObject; IsSuccess: Boolean) of object;
  TScriptCompletedEvent = procedure(Sender: TObject; const Result: string) of object;

  TDirectCompositionHost = class(TCustomControl)
  private
    FWebView: TWebView2Helper;
    FOnNavigationCompleted: TNavigationCompletedEvent;
    FOnScriptCompleted: TScriptCompletedEvent;
    
    procedure HandleNavigationCompleted(Sender: TObject; IsSuccess: Boolean);
    procedure HandleScriptCompleted(Sender: TObject; const Result: string);
    procedure HandleResize;
    
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure Navigate(const URL: string);
    procedure ExecuteScript(const Script: string);
    function EvaluateScript(const Script: string): string;
    procedure Stop;
    
    property OnNavigationCompleted: TNavigationCompletedEvent read FOnNavigationCompleted write FOnNavigationCompleted;
    property OnScriptCompleted: TScriptCompletedEvent read FOnScriptCompleted write FOnScriptCompleted;
  end;

implementation

constructor TDirectCompositionHost.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 800;
  Height := 600;
  ControlStyle := ControlStyle + [csOpaque];
end;

destructor TDirectCompositionHost.Destroy;
begin
  FWebView.Free;
  inherited;
end;

procedure TDirectCompositionHost.CreateWnd;
begin
  inherited;
  
  FWebView := TWebView2Helper.Create(Self);
  FWebView.OnNavigationCompleted := HandleNavigationCompleted;
  FWebView.OnScriptCompleted := HandleScriptCompleted;
  
  if not FWebView.Initialize then
    raise Exception.Create('Failed to initialize WebView2');
end;

procedure TDirectCompositionHost.DestroyWnd;
begin
  FWebView.Free;
  FWebView := nil;
  inherited;
end;

procedure TDirectCompositionHost.HandleNavigationCompleted(Sender: TObject; IsSuccess: Boolean);
begin
  if Assigned(FOnNavigationCompleted) then
    FOnNavigationCompleted(Self, IsSuccess);
end;

procedure TDirectCompositionHost.HandleScriptCompleted(Sender: TObject; const Result: string);
begin
  if Assigned(FOnScriptCompleted) then
    FOnScriptCompleted(Self, Result);
end;

procedure TDirectCompositionHost.HandleResize;
begin
  if Assigned(FWebView) then
    FWebView.SetBounds(ClientRect);
end;

procedure TDirectCompositionHost.WMSize(var Message: TWMSize);
begin
  inherited;
  HandleResize;
end;

procedure TDirectCompositionHost.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TDirectCompositionHost.Paint;
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

procedure TDirectCompositionHost.Navigate(const URL: string);
begin
  if Assigned(FWebView) then
    FWebView.Navigate(URL);
end;

procedure TDirectCompositionHost.ExecuteScript(const Script: string);
begin
  if Assigned(FWebView) then
    FWebView.ExecuteScript(Script);
end;

function TDirectCompositionHost.EvaluateScript(const Script: string): string;
begin
  if Assigned(FWebView) then
    Result := FWebView.EvaluateScript(Script)
  else
    Result := '';
end;

procedure TDirectCompositionHost.Stop;
begin
  if Assigned(FWebView) then
    FWebView.Stop;
end;

end.
