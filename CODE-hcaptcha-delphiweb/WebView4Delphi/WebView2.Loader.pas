unit WebView2.Loader;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  {$IFDEF VER330}
  Winapi.Windows, System.Classes, System.SysUtils, Winapi.ActiveX,
  {$ELSE}
  Windows, Classes, SysUtils, ActiveX,
  {$ENDIF}
  uWVTypes, uWVInterfaces, uWVTypeLibrary, uWVLoader;

function CreateCoreWebView2EnvironmentWithOptions(
  browserExecutableFolder: PWideChar;
  userDataFolder: PWideChar;
  environmentOptions: ICoreWebView2EnvironmentOptions;
  const environmentCreatedHandler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT; stdcall;

function CreateCoreWebView2Environment(
  const environmentCreatedHandler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT; stdcall;

function GetAvailableCoreWebView2BrowserVersionString(
  browserExecutableFolder: PWideChar;
  var versionInfo: PWideChar): HRESULT; stdcall;

function CompareBrowserVersions(
  version1: PWideChar;
  version2: PWideChar;
  var comparisonResult: Integer): HRESULT; stdcall;

var
  WebView2Loader: TWVLoader;

implementation

function CreateCoreWebView2EnvironmentWithOptions(
  browserExecutableFolder: PWideChar;
  userDataFolder: PWideChar;
  environmentOptions: ICoreWebView2EnvironmentOptions;
  const environmentCreatedHandler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT;
begin
  try
    if not Assigned(WebView2Loader) then
      WebView2Loader := TWVLoader.Create(nil);

    if Assigned(browserExecutableFolder) and (browserExecutableFolder <> '') then
      WebView2Loader.BrowserExecPath := browserExecutableFolder;

    if Assigned(userDataFolder) and (userDataFolder <> '') then
      WebView2Loader.UserDataFolder := userDataFolder;

    if WebView2Loader.StartWebView2 then
    begin
      if Assigned(environmentCreatedHandler) then
        environmentCreatedHandler.Invoke(S_OK, WebView2Loader.Environment);
      Result := S_OK;
    end
    else
    begin
      if Assigned(environmentCreatedHandler) then
        environmentCreatedHandler.Invoke(WebView2Loader.ErrorCode, nil);
      Result := WebView2Loader.ErrorCode;
    end;
  except
    on E: Exception do
      Result := E_FAIL;
  end;
end;

function CreateCoreWebView2Environment(
  const environmentCreatedHandler: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler): HRESULT;
begin
  Result := CreateCoreWebView2EnvironmentWithOptions(nil, nil, nil, environmentCreatedHandler);
end;

function GetAvailableCoreWebView2BrowserVersionString(
  browserExecutableFolder: PWideChar;
  var versionInfo: PWideChar): HRESULT;
var
  TempVersion: wvstring;
begin
  try
    if not Assigned(WebView2Loader) then
      WebView2Loader := TWVLoader.Create(nil);

    if Assigned(browserExecutableFolder) and (browserExecutableFolder <> '') then
      WebView2Loader.BrowserExecPath := browserExecutableFolder;

    TempVersion := WebView2Loader.AvailableBrowserVersion;

    if TempVersion <> '' then
    begin
      versionInfo := CoTaskMemAlloc((Length(TempVersion) + 1) * SizeOf(WideChar));
      if Assigned(versionInfo) then
      begin
        StringToWideChar(TempVersion, versionInfo, Length(TempVersion) + 1);
        Result := S_OK;
      end
      else
        Result := E_OUTOFMEMORY;
    end
    else
      Result := E_FAIL;
  except
    on E: Exception do
      Result := E_FAIL;
  end;
end;

function CompareBrowserVersions(
  version1: PWideChar;
  version2: PWideChar;
  var comparisonResult: Integer): HRESULT;
begin
  try
    if not Assigned(WebView2Loader) then
      WebView2Loader := TWVLoader.Create(nil);

    if Assigned(version1) and Assigned(version2) and
       WebView2Loader.CompareVersions(version1, version2, comparisonResult) then
      Result := S_OK
    else
      Result := E_INVALIDARG;
  except
    on E: Exception do
      Result := E_FAIL;
  end;
end;

initialization
  WebView2Loader := nil;

finalization
  if Assigned(WebView2Loader) then
    WebView2Loader.Free;

end.