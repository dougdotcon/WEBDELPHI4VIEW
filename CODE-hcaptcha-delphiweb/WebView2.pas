unit WebView2;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  {$IFDEF VER330}
  Winapi.Windows, System.Classes, System.SysUtils, Winapi.ActiveX,
  {$ELSE}
  Windows, Classes, SysUtils, ActiveX,
  {$ENDIF}
  uWVTypes, uWVInterfaces, uWVTypeLibrary;

// Apenas redireciona para as definições no uWVTypeLibrary e uWVTypes
// Este arquivo serve como ponte para o código existente

type
  // Redirecionamentos para os tipos existentes
  ICoreWebView2                                          = uWVTypeLibrary.ICoreWebView2;
  ICoreWebView2Environment                               = uWVTypeLibrary.ICoreWebView2Environment;
  ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler = uWVTypeLibrary.ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler;
  ICoreWebView2EnvironmentOptions                        = uWVTypeLibrary.ICoreWebView2EnvironmentOptions;
  ICoreWebView2Controller                                = uWVTypeLibrary.ICoreWebView2Controller;
  ICoreWebView2CreateCoreWebView2ControllerCompletedHandler = uWVTypeLibrary.ICoreWebView2CreateCoreWebView2ControllerCompletedHandler;
  ICoreWebView2WebMessageReceivedEventHandler           = uWVTypeLibrary.ICoreWebView2WebMessageReceivedEventHandler;
  ICoreWebView2WebMessageReceivedEventArgs              = uWVTypeLibrary.ICoreWebView2WebMessageReceivedEventArgs;
  ICoreWebView2NavigationCompletedEventHandler          = uWVTypeLibrary.ICoreWebView2NavigationCompletedEventHandler;
  ICoreWebView2NavigationCompletedEventArgs             = uWVTypeLibrary.ICoreWebView2NavigationCompletedEventArgs;
  ICoreWebView2ExecuteScriptCompletedHandler            = uWVTypeLibrary.ICoreWebView2ExecuteScriptCompletedHandler;

  // Constantes úteis
  COREWEBVIEW2_KEY_EVENT_KIND                           = type uWVTypes.COREWEBVIEW2_KEY_EVENT_KIND;
  COREWEBVIEW2_MOVE_FOCUS_REASON                        = type uWVTypes.COREWEBVIEW2_MOVE_FOCUS_REASON;
  COREWEBVIEW2_WEB_ERROR_STATUS                         = type uWVTypes.COREWEBVIEW2_WEB_ERROR_STATUS;
  COREWEBVIEW2_SCRIPT_DIALOG_KIND                       = type uWVTypes.COREWEBVIEW2_SCRIPT_DIALOG_KIND;
  COREWEBVIEW2_PERMISSION_STATE                         = type uWVTypes.COREWEBVIEW2_PERMISSION_STATE;
  COREWEBVIEW2_PERMISSION_KIND                          = type uWVTypes.COREWEBVIEW2_PERMISSION_KIND;
  COREWEBVIEW2_PROCESS_FAILED_KIND                      = type uWVTypes.COREWEBVIEW2_PROCESS_FAILED_KIND;
  COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT             = type uWVTypes.COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT                     = type uWVTypes.COREWEBVIEW2_WEB_RESOURCE_CONTEXT;
  COREWEBVIEW2_COOKIE_SAME_SITE_KIND                    = type uWVTypes.COREWEBVIEW2_COOKIE_SAME_SITE_KIND;
  COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND                = type uWVTypes.COREWEBVIEW2_HOST_RESOURCE_ACCESS_KIND;

implementation

end. 