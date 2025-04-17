unit uCaptchaSonicClient;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Net.HttpClient,
  System.Net.URLClient,
  uCaptchaTypes,
  uLogger;

type
  TCaptchaSonicClient = class
  private
    FConfig: TCaptchaConfig;
    FHttpClient: THTTPClient;
    
    function CreateTask(const Task: TCaptchaTask): TCaptchaResponse;
    function GetTaskResult(const TaskId: string): TCaptchaResponse;
    function ParseResponse(const Response: string): TCaptchaResponse;
  public
    constructor Create(const AConfig: TCaptchaConfig);
    destructor Destroy; override;
    
    function SolveHCaptcha(const WebsiteURL, WebsiteKey: string): TCaptchaResponse;
  end;

implementation

constructor TCaptchaSonicClient.Create(const AConfig: TCaptchaConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FHttpClient := THTTPClient.Create;
  FHttpClient.ConnectionTimeout := FConfig.TimeoutMS;
  FHttpClient.ResponseTimeout := FConfig.TimeoutMS;
end;

destructor TCaptchaSonicClient.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

function TCaptchaSonicClient.CreateTask(const Task: TCaptchaTask): TCaptchaResponse;
var
  RequestBody: TJSONObject;
  Response: IHTTPResponse;
begin
  RequestBody := TJSONObject.Create;
  try
    RequestBody.AddPair('type', Task.TaskType);
    RequestBody.AddPair('websiteURL', Task.WebsiteURL);
    RequestBody.AddPair('websiteKey', Task.WebsiteKey);
    
    if Task.Data <> '' then
      RequestBody.AddPair('data', Task.Data);
    if Task.Proxy <> '' then
      RequestBody.AddPair('proxy', Task.Proxy);
    if Task.UserAgent <> '' then
      RequestBody.AddPair('userAgent', Task.UserAgent);
      
    Response := FHttpClient.Post(
      FConfig.BaseURL + '/createTask',
      TStringStream.Create(RequestBody.ToString),
      nil,
      [TNameValuePair.Create('Authorization', 'Bearer ' + FConfig.APIKey)]
    );
    
    Result := ParseResponse(Response.ContentAsString);
  finally
    RequestBody.Free;
  end;
end;

function TCaptchaSonicClient.GetTaskResult(const TaskId: string): TCaptchaResponse;
var
  Response: IHTTPResponse;
begin
  Response := FHttpClient.Get(
    Format('%s/getTaskResult/%s', [FConfig.BaseURL, TaskId]),
    nil,
    [TNameValuePair.Create('Authorization', 'Bearer ' + FConfig.APIKey)]
  );
  
  Result := ParseResponse(Response.ContentAsString);
end;

function TCaptchaSonicClient.ParseResponse(const Response: string): TCaptchaResponse;
var
  JsonObj: TJSONObject;
begin
  JsonObj := TJSONObject.ParseJSONValue(Response) as TJSONObject;
  try
    Result.Success := JsonObj.GetValue<Boolean>('success');
    if Result.Success then
    begin
      if JsonObj.TryGetValue<string>('solution', Result.Solution) then;
      if JsonObj.TryGetValue<string>('taskId', Result.TaskId) then;
      if JsonObj.TryGetValue<Double>('balance', Result.Balance) then;
    end
    else
    begin
      Result.ErrorMessage := JsonObj.GetValue<string>('error');
    end;
  finally
    JsonObj.Free;
  end;
end;

function TCaptchaSonicClient.SolveHCaptcha(const WebsiteURL, WebsiteKey: string): TCaptchaResponse;
var
  Task: TCaptchaTask;
  RetryCount: Integer;
begin
  Task.TaskType := 'HCaptchaTask';
  Task.WebsiteURL := WebsiteURL;
  Task.WebsiteKey := WebsiteKey;
  Task.Data := '';
  Task.Proxy := '';
  Task.UserAgent := '';
  
  Result := CreateTask(Task);
  if not Result.Success then
    Exit;
    
  RetryCount := 0;
  while RetryCount < FConfig.RetryCount do
  begin
    Sleep(FConfig.DelayBetweenActionsMS + Random(FConfig.RandomDelayRangeMS));
    Result := GetTaskResult(Result.TaskId);
    
    if Result.Success and (Result.Solution <> '') then
      Break;
      
    Inc(RetryCount);
  end;
end;

end. 