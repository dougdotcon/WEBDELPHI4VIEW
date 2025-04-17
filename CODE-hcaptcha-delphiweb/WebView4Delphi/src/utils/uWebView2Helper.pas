unit uWebView2Helper;

interface

uses
  Winapi.Windows, Winapi.ActiveX,
  System.SysUtils, System.Classes, System.Variants,
  Vcl.Controls,
  WebView2, WebView2.Loader,
  uWVTypes, uWVInterfaces, uWVTypeLibrary, uWVCoreWebView2Delegates, uWVCoreWebView2Environment,
  uWVCoreWebView2Controller, uWVCoreWebView2;

type
  TWebViewNavigationCompletedEvent = procedure(Sender: TObject; IsSuccess: Boolean) of object;
  TWebViewScriptCompletedEvent = procedure(Sender: TObject; const Result: string) of object;
  TWebViewEnvironmentCompletedProc = procedure(errorCode: HRESULT; const createdEnvironment: ICoreWebView2Environment) of object;
  TWebViewControllerCompletedProc = procedure(errorCode: HRESULT; const createdController: ICoreWebView2Controller) of object;
  TWebViewScriptCompletedProc = procedure(errorCode: HRESULT; resultObjectAsJson: PWideChar) of object;
  
  // Delegado personalizado para usar em vez do TCoreWebView2NavigationCompletedEventHandler
  TWebView2NavigationCompletedHandler = class(TInterfacedObject, ICoreWebView2NavigationCompletedEventHandler)
  private
    FOwner: TObject;
    FCallback: TWebViewNavigationCompletedEvent;
  public
    constructor Create(AOwner: TObject; ACallback: TWebViewNavigationCompletedEvent);
    function Invoke(const sender: ICoreWebView2; const args: ICoreWebView2NavigationCompletedEventArgs): HRESULT; stdcall;
  end;

  // Delegado personalizado para usar em vez do TCoreWebView2WebMessageReceivedEventHandler
  TWebView2WebMessageReceivedHandler = class(TInterfacedObject, ICoreWebView2WebMessageReceivedEventHandler)
  private
    FOwner: TObject;
    FCallback: TWebViewScriptCompletedEvent;
  public
    constructor Create(AOwner: TObject; ACallback: TWebViewScriptCompletedEvent);
    function Invoke(const sender: ICoreWebView2; const args: ICoreWebView2WebMessageReceivedEventArgs): HRESULT; stdcall;
  end;

  // Delegado personalizado para o TCoreWebView2EnvironmentCompletedHandler
  TWebView2EnvironmentCompletedHandler = class(TInterfacedObject, ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler)
  private
    FHelper: TObject;
    FCallback: TMethod;
  public
    constructor Create(AHelper: TObject; ACallback: TWebViewEnvironmentCompletedProc);
    function Invoke(errorCode: HRESULT; const createdEnvironment: ICoreWebView2Environment): HRESULT; stdcall;
  end;

  // Delegado personalizado para o TCoreWebView2ControllerCompletedHandler
  TWebView2ControllerCompletedHandler = class(TInterfacedObject, ICoreWebView2CreateCoreWebView2ControllerCompletedHandler)
  private
    FHelper: TObject;
    FCallback: TMethod;
  public
    constructor Create(AHelper: TObject; ACallback: TWebViewControllerCompletedProc);
    function Invoke(errorCode: HRESULT; const createdController: ICoreWebView2Controller): HRESULT; stdcall;
  end;

  // Delegado personalizado para o TCoreWebView2ExecuteScriptCompletedHandler
  TWebView2ExecuteScriptCompletedHandler = class(TInterfacedObject, ICoreWebView2ExecuteScriptCompletedHandler)
  private
    FHelper: TObject;
    FCallback: TMethod;
  public
    constructor Create(AHelper: TObject; ACallback: TWebViewScriptCompletedProc);
    function Invoke(errorCode: HRESULT; resultObjectAsJson: PWideChar): HRESULT; stdcall;
  end;

  TWebView2Helper = class
  private
    FWebView: ICoreWebView2;
    FController: TCoreWebView2Controller;
    FEnvironment: ICoreWebView2Environment;
    FParent: TWinControl;
    FOnNavigationCompleted: TWebViewNavigationCompletedEvent;
    FOnScriptCompleted: TWebViewScriptCompletedEvent;
    FScriptResult: string;
    FScriptEvent: THandle;

    procedure HandleNavigationCompleted(Sender: TObject; IsSuccess: Boolean);
    procedure HandleWebMessageReceived(Sender: TObject; const Result: string);
    procedure HandleEnvironmentCompleted(errorCode: HRESULT; const createdEnvironment: ICoreWebView2Environment);
    procedure HandleControllerCompleted(errorCode: HRESULT; const createdController: ICoreWebView2Controller);
    procedure HandleScriptCompleted(errorCode: HRESULT; resultObjectAsJson: PWideChar);
    procedure HandleEmptyScriptCompleted(errorCode: HRESULT; resultObjectAsJson: PWideChar);
  public
    constructor Create(AParent: TWinControl);
    destructor Destroy; override;

    function Initialize: Boolean;
    procedure Navigate(const URL: string);
    procedure ExecuteScript(const Script: string);
    function EvaluateScript(const Script: string): string;
    procedure Stop;
    procedure SetBoundsRect(const Bounds: TRect);

    property OnNavigationCompleted: TWebViewNavigationCompletedEvent read FOnNavigationCompleted write FOnNavigationCompleted;
    property OnScriptCompleted: TWebViewScriptCompletedEvent read FOnScriptCompleted write FOnScriptCompleted;
  end;

implementation

{ TWebView2NavigationCompletedHandler }

constructor TWebView2NavigationCompletedHandler.Create(AOwner: TObject; ACallback: TWebViewNavigationCompletedEvent);
begin
  inherited Create;
  FOwner := AOwner;
  FCallback := ACallback;
end;

function TWebView2NavigationCompletedHandler.Invoke(const sender: ICoreWebView2; 
  const args: ICoreWebView2NavigationCompletedEventArgs): HRESULT;
var
  LSuccess: Integer;
begin
  if Assigned(FCallback) and Assigned(args) then
  begin
    args.get_IsSuccess(LSuccess);
    FCallback(FOwner, LSuccess <> 0);
  end;
  Result := S_OK;
end;

{ TWebView2WebMessageReceivedHandler }

constructor TWebView2WebMessageReceivedHandler.Create(AOwner: TObject; ACallback: TWebViewScriptCompletedEvent);
begin
  inherited Create;
  FOwner := AOwner;
  FCallback := ACallback;
end;

function TWebView2WebMessageReceivedHandler.Invoke(const sender: ICoreWebView2; 
  const args: ICoreWebView2WebMessageReceivedEventArgs): HRESULT;
var
  Message: PWideChar;
  TempResult: HRESULT;
  MsgStr: string;
begin
  Result := S_OK;
  if Assigned(FCallback) and Assigned(args) then
  begin
    TempResult := args.TryGetWebMessageAsString(Message);
    if Succeeded(TempResult) and (Message <> nil) then
    begin
      MsgStr := string(Message);
      FCallback(FOwner, MsgStr);
    end
    else
    begin
      // Opcional: Lidar com falha ou mensagem nula
    end;
  end;
end;

{ TWebView2EnvironmentCompletedHandler }

constructor TWebView2EnvironmentCompletedHandler.Create(AHelper: TObject; 
  ACallback: TWebViewEnvironmentCompletedProc);
begin
  inherited Create;
  FHelper := AHelper;
  FCallback := TMethod(ACallback);
end;

function TWebView2EnvironmentCompletedHandler.Invoke(errorCode: HRESULT;
  const createdEnvironment: ICoreWebView2Environment): HRESULT;
type
  TCallbackProc = procedure(errorCode: HRESULT; const createdEnvironment: ICoreWebView2Environment) of object;
var
  Callback: TCallbackProc;
begin
  if Assigned(TMethod(FCallback).Code) then
  begin
    Callback := TCallbackProc(FCallback);
    Callback(errorCode, createdEnvironment);
  end;
  Result := S_OK;
end;

{ TWebView2ControllerCompletedHandler }

constructor TWebView2ControllerCompletedHandler.Create(AHelper: TObject;
  ACallback: TWebViewControllerCompletedProc);
begin
  inherited Create;
  FHelper := AHelper;
  FCallback := TMethod(ACallback);
end;

function TWebView2ControllerCompletedHandler.Invoke(errorCode: HRESULT;
  const createdController: ICoreWebView2Controller): HRESULT; 
type
  TCallbackProc = procedure(errorCode: HRESULT; const createdController: ICoreWebView2Controller) of object;
var
  Callback: TCallbackProc;
begin
  if Assigned(TMethod(FCallback).Code) then
  begin
    Callback := TCallbackProc(FCallback);
    Callback(errorCode, createdController);
  end;
  Result := S_OK;
end;

{ TWebView2ExecuteScriptCompletedHandler }

constructor TWebView2ExecuteScriptCompletedHandler.Create(AHelper: TObject; ACallback: TWebViewScriptCompletedProc);
begin
  inherited Create;
  FHelper := AHelper;
  FCallback := TMethod(ACallback);
end;

function TWebView2ExecuteScriptCompletedHandler.Invoke(errorCode: HRESULT; 
  resultObjectAsJson: PWideChar): HRESULT;
type
  TCallbackProc = procedure(errorCode: HRESULT; resultObjectAsJson: PWideChar) of object;
var
  Callback: TCallbackProc;
begin
  if Assigned(TMethod(FCallback).Code) then
  begin
    Callback := TCallbackProc(FCallback);
    Callback(errorCode, resultObjectAsJson);
  end;
  Result := S_OK;
end;

{ TWebView2Helper }

constructor TWebView2Helper.Create(AParent: TWinControl);
begin
  inherited Create;
  FParent := AParent;
end;

destructor TWebView2Helper.Destroy;
begin
  FController.Free;
  inherited;
end;

procedure TWebView2Helper.HandleEmptyScriptCompleted(errorCode: HRESULT; resultObjectAsJson: PWideChar);
begin
  // Nada a fazer aqui
end;

procedure TWebView2Helper.HandleScriptCompleted(errorCode: HRESULT; resultObjectAsJson: PWideChar);
begin
  if Succeeded(errorCode) and (resultObjectAsJson <> nil) then
    FScriptResult := string(resultObjectAsJson);
  SetEvent(FScriptEvent);
end;

procedure TWebView2Helper.HandleEnvironmentCompleted(errorCode: HRESULT; const createdEnvironment: ICoreWebView2Environment);
var
  TempControllerHandler: TWebView2ControllerCompletedHandler;
  TempResult: HRESULT;
begin
  if Succeeded(errorCode) then
  begin
    FEnvironment := createdEnvironment;

    // Create WebView2 Controller
    TempControllerHandler := TWebView2ControllerCompletedHandler.Create(Self, HandleControllerCompleted);
    TempResult := FEnvironment.CreateCoreWebView2Controller(FParent.Handle, TempControllerHandler);
  end;
end;

procedure TWebView2Helper.HandleControllerCompleted(errorCode: HRESULT; const createdController: ICoreWebView2Controller);
var
  TempNavHandler: TWebView2NavigationCompletedHandler;
  TempMsgHandler: TWebView2WebMessageReceivedHandler;
  TempToken: EventRegistrationToken;
begin
  if Succeeded(errorCode) then
  begin
    // Criar o wrapper a partir da interface
    FController := TCoreWebView2Controller.Create(createdController);
    
    // Obter referência ao WebView2 diretamente da propriedade
    FWebView := FController.CoreWebView2;

    // Set up event handlers
    TempNavHandler := TWebView2NavigationCompletedHandler.Create(
      Self,
      HandleNavigationCompleted
    );
    FWebView.add_NavigationCompleted(TempNavHandler, TempToken);

    TempMsgHandler := TWebView2WebMessageReceivedHandler.Create(
      Self,
      HandleWebMessageReceived
    );
    FWebView.add_WebMessageReceived(TempMsgHandler, TempToken);

    // Set initial bounds
    SetBoundsRect(FParent.ClientRect);

    // Show the WebView - usando a propriedade pública
    FController.IsVisible := True;
  end;
end;

function TWebView2Helper.Initialize: Boolean;
var
  UserDataFolder: string;
  TempHandler: TWebView2EnvironmentCompletedHandler;
  TempResult: HRESULT;
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
    TempHandler := TWebView2EnvironmentCompletedHandler.Create(Self, HandleEnvironmentCompleted);
    TempResult := CreateCoreWebView2EnvironmentWithOptions(nil, PWideChar(WideString(UserDataFolder)), nil, TempHandler);
    if Failed(TempResult) then
      Exit;

    Result := True;
  except
    Result := False;
  end;
end;

procedure TWebView2Helper.Navigate(const URL: string);
begin
  if Assigned(FWebView) then
    FWebView.Navigate(PWideChar(URL));
end;

procedure TWebView2Helper.ExecuteScript(const Script: string);
var
  TempHandler: TWebView2ExecuteScriptCompletedHandler;
begin
  if Assigned(FWebView) then
  begin
    TempHandler := TWebView2ExecuteScriptCompletedHandler.Create(Self, HandleEmptyScriptCompleted);
    FWebView.ExecuteScript(PWideChar(Script), TempHandler);
  end;
end;

function TWebView2Helper.EvaluateScript(const Script: string): string;
var
  TempHandler: TWebView2ExecuteScriptCompletedHandler;
begin
  Result := '';
  if Assigned(FWebView) then
  begin
    FScriptResult := '';
    FScriptEvent := CreateEvent(nil, True, False, nil);
    try
      TempHandler := TWebView2ExecuteScriptCompletedHandler.Create(Self, HandleScriptCompleted);
      FWebView.ExecuteScript(PWideChar(Script), TempHandler);

      // Espera pelo resultado (com timeout)
      if WaitForSingleObject(FScriptEvent, 5000) = WAIT_OBJECT_0 then
        Result := FScriptResult;
    finally
      CloseHandle(FScriptEvent);
    end;
  end;
end;

procedure TWebView2Helper.Stop;
begin
  if Assigned(FWebView) then
    FWebView.Stop;
end;

procedure TWebView2Helper.SetBoundsRect(const Bounds: TRect);
begin
  if Assigned(FController) then
    FController.Bounds := Bounds; // Usando a propriedade pública
end;

procedure TWebView2Helper.HandleNavigationCompleted(Sender: TObject; IsSuccess: Boolean);
begin
  if Assigned(FOnNavigationCompleted) then
    FOnNavigationCompleted(Self, IsSuccess);
end;

procedure TWebView2Helper.HandleWebMessageReceived(Sender: TObject; const Result: string);
begin
  if Assigned(FOnScriptCompleted) then
    FOnScriptCompleted(Self, Result);
end;

end. 