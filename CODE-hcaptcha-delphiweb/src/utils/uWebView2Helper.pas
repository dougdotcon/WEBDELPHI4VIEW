unit uWebView2Helper;

interface

uses
  Winapi.Windows, Winapi.ActiveX,
  System.SysUtils, System.Classes,
  Vcl.Controls,
  WebView2, WebView2.Loader;

type
  TWebViewNavigationCompletedEvent = procedure(Sender: TObject; IsSuccess: Boolean) of object;
  TWebViewScriptCompletedEvent = procedure(Sender: TObject; const Result: string) of object;

  TWebView2Helper = class
  private
    FWebView: ICoreWebView2;
    FController: ICoreWebView2Controller;
    FEnvironment: ICoreWebView2Environment;
    FParent: TWinControl;
    FOnNavigationCompleted: TWebViewNavigationCompletedEvent;
    FOnScriptCompleted: TWebViewScriptCompletedEvent;
    
    procedure HandleNavigationCompleted(Sender: TObject; const Args: ICoreWebView2NavigationCompletedEventArgs);
    procedure HandleWebMessageReceived(Sender: TObject; const Args: ICoreWebView2WebMessageReceivedEventArgs);
  public
    constructor Create(AParent: TWinControl);
    destructor Destroy; override;
    
    function Initialize: Boolean;
    procedure Navigate(const URL: string);
    procedure ExecuteScript(const Script: string);
    function EvaluateScript(const Script: string): string;
    procedure Stop;
    procedure SetBounds(const Bounds: TRect);
    
    property OnNavigationCompleted: TWebViewNavigationCompletedEvent read FOnNavigationCompleted write FOnNavigationCompleted;
    property OnScriptCompleted: TWebViewScriptCompletedEvent read FOnScriptCompleted write FOnScriptCompleted;
  end;

implementation

constructor TWebView2Helper.Create(AParent: TWinControl);
begin
  inherited Create;
  FParent := AParent;
end;

destructor TWebView2Helper.Destroy;
begin
  if Assigned(FController) then
    FController.Close;
  inherited;
end;

function TWebView2Helper.Initialize: Boolean;
var
  UserDataFolder: string;
begin
  Result := False;
  
  try
    // Initialize COM
    CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
    
    // Create user data folder
    UserDataFolder := IncludeTrailingPathDelimiter(GetEnvironmentVariable('LOCALAPPDATA')) +
      'CaptchaSolver\WebView2';
    ForceDirectories(UserDataFolder);
    
    // Create WebView2 environment
    if Failed(CreateCoreWebView2EnvironmentWithOptions(nil,
      PWideChar(UserDataFolder), nil,
      TCoreWebView2EnvironmentCompletedHandler.Create(
        procedure(ErrorCode: HResult; const CreatedEnvironment: ICoreWebView2Environment)
        begin
          if Succeeded(ErrorCode) then
          begin
            FEnvironment := CreatedEnvironment;
            
            // Create WebView2
            FEnvironment.CreateCoreWebView2Controller(FParent.Handle,
              TCoreWebView2CreateCoreWebView2ControllerCompletedHandler.Create(
                procedure(ErrorCode: HResult; const CreatedController: ICoreWebView2Controller)
                begin
                  if Succeeded(ErrorCode) then
                  begin
                    FController := CreatedController;
                    FWebView := FController.CoreWebView2;
                    
                    // Set up event handlers
                    FWebView.add_NavigationCompleted(
                      TCoreWebView2NavigationCompletedEventHandler.Create(HandleNavigationCompleted),
                      nil);
                      
                    FWebView.add_WebMessageReceived(
                      TCoreWebView2WebMessageReceivedEventHandler.Create(HandleWebMessageReceived),
                      nil);
                      
                    // Set initial bounds
                    SetBounds(FParent.ClientRect);
                    
                    // Show the WebView
                    FController.put_IsVisible(True);
                    Result := True;
                  end;
                end));
          end;
        end))) then
      Exit;
      
    Result := True;
  except
    Result := False;
  end;
end;

procedure TWebView2Helper.Navigate(const URL: string);
begin
  if Assigned(FWebView) then
    FWebView.Navigate(URL);
end;

procedure TWebView2Helper.ExecuteScript(const Script: string);
begin
  if Assigned(FWebView) then
    FWebView.ExecuteScript(Script, nil);
end;

function TWebView2Helper.EvaluateScript(const Script: string): string;
var
  ExecuteResult: string;
begin
  Result := '';
  if Assigned(FWebView) then
  begin
    FWebView.ExecuteScript(Script,
      TCoreWebView2ExecuteScriptCompletedHandler.Create(
        procedure(ErrorCode: HResult; const ResultObjectAsJson: PWideChar)
        begin
          if Succeeded(ErrorCode) then
            ExecuteResult := ResultObjectAsJson;
        end));
    Result := ExecuteResult;
  end;
end;

procedure TWebView2Helper.Stop;
begin
  if Assigned(FWebView) then
    FWebView.Stop;
end;

procedure TWebView2Helper.SetBounds(const Bounds: TRect);
begin
  if Assigned(FController) then
    FController.put_Bounds(Bounds);
end;

procedure TWebView2Helper.HandleNavigationCompleted(Sender: TObject;
  const Args: ICoreWebView2NavigationCompletedEventArgs);
var
  IsSuccess: Boolean;
begin
  if Assigned(FOnNavigationCompleted) then
  begin
    Args.get_IsSuccess(IsSuccess);
    FOnNavigationCompleted(Self, IsSuccess);
  end;
end;

procedure TWebView2Helper.HandleWebMessageReceived(Sender: TObject;
  const Args: ICoreWebView2WebMessageReceivedEventArgs);
var
  Message: PWideChar;
begin
  if Assigned(FOnScriptCompleted) and Assigned(Args) then
  begin
    Args.TryGetWebMessageAsString(Message);
    FOnScriptCompleted(Self, Message);
  end;
end;

end. 