unit uPopularCaptchaSolver;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Threading,
  System.Variants, System.RegularExpressions, system.Generics.Collections,
  System.SyncObjs, System.IOUtils, Winapi.WinInet, System.TypInfo, Vcl.Forms,
  Vcl.Dialogs,
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.NetEncoding, uWVBrowser, uWVTypes,
  uWVConstants, uWVTypeLibrary, uWVLibFunctions, uWVLoader, uWVInterfaces,
  uWVCoreWebView2Args, uWVBrowserBase, uWVWindowParent, uWVWinControl;

type
  TLogLevel = (llDebug, llInfo, llWarn, llError);
  TLogEvent = TProc<string, string>;
  TCaptchaType = (ctUnknown, ctWidget, ctMultiSelect, ctGrid, ctBbox, ctBboxDD);
  // Structure for script execution
    type
      TScriptExecution = record
        Script: string;
        ExecutionID: Integer;
        TimeoutMs: Cardinal;
        Callback: TProc<string>;
      end;

  TPopularCaptchaSolver = class
  private
    FWebBrowser: TWVBrowser;
    FApiKey: string;
    FApiEndpoint: string;
    FOnLog: TLogEvent;
    FIsProcessing: Boolean;
    FScriptResult: wvstring;
    FScriptCompleted: Boolean;
    FExecutionID: Integer;
    FStop: Boolean;
    FAutoSolve: Boolean;
    FAutoOpen: Boolean;
    FEnglishEnabled: Boolean;
    FAlwaysSolve: Boolean;
    FMaxRetries: Integer;
    FCurrentRetryCount: Integer;
    FDetectionMethod: Integer;
    FMHTMLContent: TStringList;
    FCaptchaType: TCaptchaType;
    FCaptchaFrameFound: Boolean;
    FCaptchaFrame: ICoreWebView2Frame;
    FOnFrameCreatedToken: EventRegistrationToken;
    FFrameScriptExecutionQueue: TQueue<TScriptExecution>;



    procedure OnRetrieveMHTMLCompleted(Sender: TObject;
      aResult: Boolean; const aMHTML: wvstring);
    function ExtractCaptchaDataFromMHTML(const MHTMLContent: string): TJSONObject;
    procedure ProcessCaptchaFromMHTML(const MHTMLContent: string);
    procedure SolveCaptchaWithMHTML;
    function CreateApiRequestFromMHTML(const CaptchaData: TJSONObject): TJSONObject;

    // Frame handling
    procedure OnFrameCreated(Sender: TObject; const aWebView: ICoreWebView2;
                             const aArgs: ICoreWebView2FrameCreatedEventArgs);
    procedure OnFrameNavigationCompleted(Sender: TObject; const aArgs: TCoreWebView2NavigationCompletedEventArgs);
    procedure InjectScriptIntoFrame(const Frame: ICoreWebView2Frame; const Script: string; ExecutionID: Integer);
    function IsFrameContainingCaptcha(const Frame: ICoreWebView2Frame): Boolean;
    function ExecuteScriptInCaptchaFrame(const Script: string; ExecutionID: Integer; TimeoutMs: Cardinal): wvstring;
    procedure RefreshAndFindCaptchaFrames;
    procedure ExtractMHTMLFromFrame;
    procedure ProcessFrameHTML(const HtmlContent: string);
    procedure SolveWithTraditionalMethod;
    function ExtractCaptchaDataFromHTML(const HtmlContent: string): TJSONObject;
    procedure ProcessCaptchaData(const CaptchaData: TJSONObject);
    function CreateApiRequestFromExtractedData(const CaptchaData: TJSONObject): TJSONObject;

    // Script Execution
    procedure ScriptCompletedHandler(Sender: TObject; aErrorCode: HRESULT;
      const aResultObjectAsJson: wvstring; aExecutionID: Integer);
    function ExecuteScriptAndWait(const Script: string; ExecutionID: Integer;
      TimeoutMs: Cardinal): wvstring;
    function ExecScriptSync(const Script: string): string;

    // Captcha Detection
    function IsWidget: Boolean;
    function IsSolved: Boolean;
    function IsMultiSelect: Boolean;
    function IsGrid: Boolean;
    function IsBbox: Boolean;
    function IsBboxDD: Boolean;
    function IsOnline: Boolean;
    function DetectCaptchaType: TCaptchaType;

    // UI Interaction
    function ClickElement(const Selector: string): Boolean;
    function ClickGridCell(Index: Integer): Boolean;
    function ClickCanvas(X, Y: Integer): Boolean;
    function ClickMatchingElement(const Text: string): Boolean;
    procedure SimulateMouseAction(const Action, Selector: string; X, Y: Integer);

    // Data Extraction
    function ExtractCaptchaData: TJSONObject;
    function GetExamples(const Selector: string): TJSONArray;
    function GetBase64FromUrl(const Url: string): string;
    function SliceCanvas: string;
    function ExtractWebsiteURLFromPage: string;
    function ExtractWebsiteKeyFromPage: string;

    // API Communication
    function CreateApiRequest(CaptchaType: TCaptchaType): TJSONObject;
    function SendApiRequest(const Request: TJSONObject): TJSONObject;
    procedure HandleApiError(Response: TJSONObject);

    // Solution Application
    procedure ApplySolution(const Response: TJSONObject);
    procedure ApplyMultiSelect(const Answers: TJSONArray);
    procedure ApplyGrid(const Answers: TJSONArray);
    procedure ApplyBbox(const Answers: TJSONArray);
    procedure ApplyBboxDD(const Answers: TJSONArray);
    procedure HandleCanvasDD(StartX, StartY, EndX, EndY: Integer; Delay: Integer);

    // UI Adjustment
    procedure SetLanguageToEnglish;

    // Logging
    procedure Log(const Msg: string; Level: string);

    // MHTML processing
    function ExtractMhtmlBoundary(const MHTMLContent: string): string;
    function SplitMhtmlParts(const MHTMLContent, Boundary: string): TArray<string>;
    function ExtractBase64FromMimePart(const MimePart: string): string;
    function DecodeQuotedPrintable(const Input: string): string;
    function ExtractQuestionText(const HtmlContent: string): string;
    function ExtractImageUrlsFromCSS(const HtmlContent: string): TStringList;
    function ExtractChoices(const HtmlContent: string): TStringList;

  public
    constructor Create(WebBrowser: TWVBrowser);
    destructor Destroy; override;
    procedure RefreshIframes;
    procedure Solve;
    procedure Stop;

    property ApiKey: string read FApiKey write FApiKey;
    property AutoSolve: Boolean read FAutoSolve write FAutoSolve;
    property AutoOpen: Boolean read FAutoOpen write FAutoOpen;
    property EnglishEnabled: Boolean read FEnglishEnabled write FEnglishEnabled;
    property AlwaysSolve: Boolean read FAlwaysSolve write FAlwaysSolve;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    property DetectionMethod: Integer read FDetectionMethod
      write FDetectionMethod;
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

implementation

uses
  System.Math, System.StrUtils;

// Helper functions
// Remove HTML tags from text
function StripHtmlTags(const HtmlText: string): string;
var
  regex: TRegEx;
  aresult: string;
begin
  // Remove all HTML tags
  regex := TRegEx.Create('<[^>]+>', [roIgnoreCase]);
  aresult := regex.Replace(HtmlText, ' ');

  // Convert common HTML entities
  aresult := StringReplace(aresult, '&nbsp;', ' ', [rfReplaceAll]);
  aresult := StringReplace(aresult, '&amp;', '&', [rfReplaceAll]);
  aresult := StringReplace(aresult, '&lt;', '<', [rfReplaceAll]);
  aresult := StringReplace(aresult, '&gt;', '>', [rfReplaceAll]);
  aresult := StringReplace(aresult, '&quot;', '"', [rfReplaceAll]);

  // Normalize multiple spaces
  regex := TRegEx.Create('\s+');
  aresult := regex.Replace(aresult, ' ');

  Result := Trim(aresult);
end;

{ TPopularCaptchaSolver }

constructor TPopularCaptchaSolver.Create(WebBrowser: TWVBrowser);
begin
  inherited Create;
  FWebBrowser := WebBrowser;
  FApiEndpoint := 'https://api.captchasonic.com/createTask';
  FIsProcessing := False;
  FStop := False;
  FMaxRetries := 3;
  FCurrentRetryCount := 0;
  FAutoSolve := True;
  FAutoOpen := True;
  FEnglishEnabled := True;
  FAlwaysSolve := False;
  FDetectionMethod := -1; // Auto detection
  FMHTMLContent := TStringList.Create;
  FCaptchaFrameFound := False;
  FCaptchaFrame := nil;
  FFrameScriptExecutionQueue := TQueue<TScriptExecution>.Create;

  if Assigned(FWebBrowser) then
  begin
    FWebBrowser.OnExecuteScriptCompleted := ScriptCompletedHandler;

    // Add OnFrameCreated event listener
    if Assigned(FWebBrowser.CoreWebView2) then
    begin
      FWebBrowser.CoreWebView2.AddFrameCreatedEvent(OnFrameCreated, FOnFrameCreatedToken);
    end
    else
    begin
      FWebBrowser.OnCoreWebView2InitializationCompleted := procedure(Sender: TObject; aSuccess: Boolean)
      begin
        if aSuccess and Assigned(FWebBrowser.CoreWebView2) then
          FWebBrowser.CoreWebView2.AddFrameCreatedEvent(OnFrameCreated, FOnFrameCreatedToken);
      end;
    end;
  end;
end;

destructor TPopularCaptchaSolver.Destroy;
begin
  // Release event token if registered
  if (FOnFrameCreatedToken.value <> 0) and Assigned(FWebBrowser) and Assigned(FWebBrowser.CoreWebView2) then
    FWebBrowser.CoreWebView2.RemoveFrameCreatedEvent(FOnFrameCreatedToken);

  FFrameScriptExecutionQueue.Free;
  FWebBrowser := nil;
  FCaptchaFrame := nil;
  FOnLog := nil;
  FMHTMLContent.Free;
  inherited;
end;

procedure TPopularCaptchaSolver.Log(const Msg: string; Level: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Msg, Level);
end;

procedure TPopularCaptchaSolver.OnFrameCreated(Sender: TObject;
  const aWebView: ICoreWebView2;
  const aArgs: ICoreWebView2FrameCreatedEventArgs);
var
  Frame: ICoreWebView2Frame;
  NavigationToken: EventRegistrationToken;
begin
  if Assigned(aArgs) and (aArgs.GetFrame(Frame) = S_OK) then
  begin
    // Check if this frame contains a captcha
    if IsFrameContainingCaptcha(Frame) then
    begin
      Log('Captcha frame detected!', 'Info');
      FCaptchaFrameFound := True;
      FCaptchaFrame := Frame;

      // Subscribe to navigation completed event for this frame
      Frame.add_NavigationCompleted(
        function(aSender: TObject; const args: ICoreWebView2NavigationCompletedEventArgs): HRESULT
        var
          Success: Boolean;
          NavErrorCode: HRESULT;
        begin
          args.IsSuccess(Success);
          args.GetWebErrorStatus(NavErrorCode);

          if Success then
          begin
            Log('Captcha frame navigation completed successfully', 'Debug');

            // Inject scripts needed for detection and solving
            if FAutoSolve then
            begin
              // Inject script to detect captcha type
              InjectScriptIntoFrame(FCaptchaFrame,
                'try {' +
                '  const isMulti = document.querySelector(".task-answers") !== null;' +
                '  const isGrid = document.querySelectorAll(".task-image .image").length === 9;' +
                '  const isBbox = document.querySelector(".bounding-box-example") !== null;' +
                '  const isBboxDD = document.querySelector(".challenge-header") !== null;' +
                '  return {isMulti, isGrid, isBbox, isBboxDD};' +
                '} catch(e) { return {error: e.message}; }',
                1000);

              // If English language is enabled, set the language
              if FEnglishEnabled then
              begin
                SetLanguageToEnglish;
              end;
            end;
          end
          else
            Log(Format('Captcha frame navigation failed with code: %d', [NavErrorCode]), 'Error');

          Result := S_OK;
        end,
        NavigationToken
      );
    end;
  end;
end;

function TPopularCaptchaSolver.IsFrameContainingCaptcha(const Frame: ICoreWebView2Frame): Boolean;
var
  FrameName: PWideChar;
  FrameSource: PWideChar;
begin
  // Check if frame likely contains captcha based on name and source URL
  if Frame.GetName(FrameName) = S_OK then
  begin
    Result := (Pos('captcha', LowerCase(String(FrameName))) > 0) or
              (Pos('hcaptcha', LowerCase(String(FrameName))) > 0);

    CoTaskMemFree(FrameName);
    if Result then
      Exit;
  end;

  if Frame.GetSource(FrameSource) = S_OK then
  begin
    Result := (Pos('captcha', LowerCase(String(FrameSource))) > 0) or
              (Pos('hcaptcha', LowerCase(String(FrameSource))) > 0) or
              (Pos('newassets', LowerCase(String(FrameSource))) > 0);

    CoTaskMemFree(FrameSource);
    Exit;
  end;

  // If can't determine by attributes, return false
  Result := False;
end;

procedure TPopularCaptchaSolver.InjectScriptIntoFrame(const Frame: ICoreWebView2Frame;
                                                     const Script: string;
                                                     ExecutionID: Integer);
begin
  if not Assigned(Frame) then
    Exit;

  // Create handler to receive the result
  var CompletedHandler := TCoreWebView2ExecuteScriptCompletedHandler.Create(
    function(errorCode: HRESULT; resultObjectAsJson: PWideChar): HRESULT
    begin
      if errorCode = S_OK then
      begin
        FScriptResult := resultObjectAsJson;
        FScriptCompleted := True;
        Log(Format('Script in iframe (ID %d) completed successfully: %s',
          [ExecutionID, FScriptResult]), 'Debug');
      end
      else
      begin
        FScriptResult := '';
        FScriptCompleted := True;
        Log(Format('Script in iframe (ID %d) failed with code: %X',
          [ExecutionID, errorCode]), 'Error');
      end;

      CoTaskMemFree(resultObjectAsJson);
      Result := S_OK;
    end
  );

  try
    // Execute script in the frame
    Frame.ExecuteScript(PWideChar(Script), CompletedHandler);
    Log(Format('Script injected into frame (ID %d)', [ExecutionID]), 'Debug');
  except
    on E: Exception do
    begin
      Log('Error injecting script into frame: ' + E.Message, 'Error');
      CompletedHandler := nil;
    end;
  end;
end;

function TPopularCaptchaSolver.ExecuteScriptInCaptchaFrame(const Script: string;
                                                         ExecutionID: Integer;
                                                         TimeoutMs: Cardinal): wvstring;
var
  StartTime: Cardinal;
  TimeoutOccurred: Boolean;
  ScriptExec: TScriptExecution;
begin
  Log('Attempting to execute script in captcha frame: ' + Script, 'Debug');

  if not FCaptchaFrameFound or not Assigned(FCaptchaFrame) then
  begin
    // If captcha frame not yet found, queue the script
    Log('Captcha frame not found, queueing script', 'Debug');
    ScriptExec.Script := Script;
    ScriptExec.ExecutionID := ExecutionID;
    ScriptExec.TimeoutMs := TimeoutMs;
    FFrameScriptExecutionQueue.Enqueue(ScriptExec);
    Result := '';
    Exit;
  end;

  FScriptResult := '';
  FScriptCompleted := False;
  TimeoutOccurred := False;

  // Execute script in captcha frame
  InjectScriptIntoFrame(FCaptchaFrame, Script, ExecutionID);

  // Wait for script to complete or timeout
  StartTime := GetTickCount;
  while (not FScriptCompleted) and (not TimeoutOccurred) do
  begin
    TimeoutOccurred := (GetTickCount - StartTime) >= TimeoutMs;
    if not TimeoutOccurred then
    begin
      Application.ProcessMessages;
      Sleep(10);
    end;
  end;

  if TimeoutOccurred then
  begin
    Log(Format('Script in captcha frame timed out after %d ms (ID: %d)',
      [TimeoutMs, ExecutionID]), 'Error');
    Result := '';
  end
  else
    Result := FScriptResult;
end;

procedure TPopularCaptchaSolver.RefreshIframes;
const
  Script = 'try {' +
           '  const frames = document.querySelectorAll("iframe");' +
           '  for (const frame of frames) {' +
           '    try {' +
           '      if (frame.src.includes("captcha") || frame.src.includes("newassets")) {' +
           '        const src = frame.src;' +
           '        frame.src = "about:blank";' +
           '        setTimeout(() => { frame.src = src; }, 100);' +
           '      }' +
           '    } catch(e) { console.error(e); }' +
           '  }' +
           '  return true;' +
           '} catch(e) { return false; }';
begin
  ExecScriptSync(Script);
  Log('Refreshed CAPTCHA iframes', 'Info');
  Sleep(1000); // Wait for iframes to reload

  // Reset captcha frame reference
  FCaptchaFrameFound := False;
  FCaptchaFrame := nil;

  // Attempt to find captcha frames again
  if Assigned(FWebBrowser) and Assigned(FWebBrowser.CoreWebView2) then
  begin
    FWebBrowser.CoreWebView2.GetFrames(
      function(const frameCollection: ICoreWebView2FrameCollection): HRESULT
      var
        FrameCount: UINT;
        I: Integer;
        Frame: ICoreWebView2Frame;
      begin
        if Succeeded(frameCollection.get_Count(FrameCount)) and (FrameCount > 0) then
        begin
          Log(Format('Found %d frames after refresh', [FrameCount]), 'Debug');

          for I := 0 to Integer(FrameCount) - 1 do
          begin
            if Succeeded(frameCollection.GetValueAtIndex(I, Frame)) then
            begin
              if IsFrameContainingCaptcha(Frame) then
              begin
                FCaptchaFrameFound := True;
                FCaptchaFrame := Frame;
                Log('Captcha frame found after refresh', 'Info');
                Break;
              end;
            end;
          end;
        end;

        Result := S_OK;
      end);
  end;
end;

procedure TPopularCaptchaSolver.ExtractMHTMLFromFrame;
begin
  if FCaptchaFrameFound and Assigned(FCaptchaFrame) then
  begin
    Log('Attempting to extract MHTML from captcha iframe...', 'Info');

    // Execute script to get frame HTML
    var CompletedHandler := TCoreWebView2ExecuteScriptCompletedHandler.Create(
      function(errorCode: HRESULT; resultObjectAsJson: PWideChar): HRESULT
      begin
        if errorCode = S_OK then
        begin
          // Process the frame HTML
          ProcessFrameHTML(resultObjectAsJson);
        end
        else
        begin
          Log(Format('Failed to retrieve frame HTML: %X', [errorCode]), 'Error');
        end;

        CoTaskMemFree(resultObjectAsJson);
        Result := S_OK;
      end
    );

    FCaptchaFrame.ExecuteScript(
      'document.documentElement.outerHTML;',
      CompletedHandler
    );
  end
  else
  begin
    Log('No captcha iframe found to extract MHTML', 'Warn');
  end;
end;

procedure TPopularCaptchaSolver.ProcessFrameHTML(const HtmlContent: string);
var
  CaptchaData: TJSONObject;
begin
  if (HtmlContent = '') or (HtmlContent = 'null') then
  begin
    Log('Empty or invalid frame HTML', 'Error');
    Exit;
  end;

  Log('Captcha frame HTML retrieved (size: ' + IntToStr(Length(HtmlContent)) + ')', 'Info');

  // Extract captcha data directly from HTML
  try
    CaptchaData := ExtractCaptchaDataFromHTML(HtmlContent);

    if (CaptchaData <> nil) and (CaptchaData.Count > 0) then
    begin
      ProcessCaptchaData(CaptchaData);
    end
    else
    begin
      Log('Failed to extract captcha data from frame HTML', 'Error');
    end;
  finally
    if Assigned(CaptchaData) then
      CaptchaData.Free;
  end;
end;

function TPopularCaptchaSolver.ExtractCaptchaDataFromHTML(const HtmlContent: string): TJSONObject;
var
  QuestionMatch, ImagesMatch, ExamplesMatch, ChoicesMatch: TMatch;
  Questions, Images, Examples, Choices: TStringList;
  ImagesArray, ExamplesArray, ChoicesArray: TJSONArray;
  i: Integer;
begin
  Result := TJSONObject.Create;
  Questions := TStringList.Create;
  Images := TStringList.Create;
  Examples := TStringList.Create;
  Choices := TStringList.Create;

  try
    // Extract question (prompt)
    QuestionMatch := TRegEx.Match(HtmlContent,
      '<div[^>]*class="[^"]*prompt-text[^"]*"[^>]*>(.*?)</div>',
      [roSingleLine, roIgnoreCase]);

    if QuestionMatch.Success and (QuestionMatch.Groups.Count > 1) then
    begin
      Questions.Add(Trim(QuestionMatch.Groups[1].Value));
    end;

    // Extract main images
    for ImagesMatch in TRegEx.Matches(HtmlContent,
      'background-image\s*:\s*url\([''"]?(.*?)[''"]?\)',
      [roSingleLine, roIgnoreCase]) do
    begin
      if ImagesMatch.Groups.Count > 1 then
        Images.Add(ImagesMatch.Groups[1].Value);
    end;

    // Extract examples
    for ExamplesMatch in TRegEx.Matches(HtmlContent,
      'class="[^"]*example[^"]*".*?background-image\s*:\s*url\([''"]?(.*?)[''"]?\)',
      [roSingleLine, roIgnoreCase]) do
    begin
      if ExamplesMatch.Groups.Count > 1 then
        Examples.Add(ExamplesMatch.Groups[1].Value);
    end;

    // Extract choices
    for ChoicesMatch in TRegEx.Matches(HtmlContent,
      '<div[^>]*class="[^"]*answer-text[^"]*"[^>]*>(.*?)</div>',
      [roSingleLine, roIgnoreCase]) do
    begin
      if ChoicesMatch.Groups.Count > 1 then
        Choices.Add(Trim(ChoicesMatch.Groups[1].Value));
    end;

    // Build JSON arrays
    ImagesArray := TJSONArray.Create;
    ExamplesArray := TJSONArray.Create;
    ChoicesArray := TJSONArray.Create;

    // Add images
    for i := 0 to Images.Count - 1 do
      ImagesArray.Add(Images[i]);

    // Add examples
    for i := 0 to Examples.Count - 1 do
      ExamplesArray.Add(Examples[i]);

    // Add choices
    for i := 0 to Choices.Count - 1 do
      ChoicesArray.Add(Choices[i]);

    // Build result object
    if Questions.Count > 0 then
      Result.AddPair('target', Questions[0])
    else
      Result.AddPair('target', '');

    Result.AddPair('images', ImagesArray);
    Result.AddPair('examples', ExamplesArray);
    Result.AddPair('choices', ChoicesArray);

    // Determine captcha type based on extracted data
    if Choices.Count > 0 then
      FCaptchaType := ctMultiSelect
    else if Images.Count = 9 then
      FCaptchaType := ctGrid
    else if HtmlContent.Contains('bounding-box-example') then
      FCaptchaType := ctBbox
    else if HtmlContent.Contains('challenge-header') then
      FCaptchaType := ctBboxDD
    else
      FCaptchaType := ctUnknown;

    Log(Format('Data extracted from HTML: Question="%s", Images=%d, Examples=%d, Choices=%d, Type=%s',
        [Questions.Count > 0 ? Questions[0] : '', ImagesArray.Count, ExamplesArray.Count,
        ChoicesArray.Count, GetEnumName(TypeInfo(TCaptchaType), Ord(FCaptchaType))]), 'Debug');

  finally
    Questions.Free;
    Images.Free;
    Examples.Free;
    Choices.Free;
  end;
end;

// Process extracted captcha data
procedure TPopularCaptchaSolver.ProcessCaptchaData(const CaptchaData: TJSONObject);
var
  Request: TJSONObject;
  Response: TJSONObject;
begin
  if not Assigned(CaptchaData) or (CaptchaData.Count = 0) then
  begin
    Log('No valid captcha data to process', 'Error');
    Exit;
  end;

  Request := nil;
  Response := nil;

  try
    // Create API request
    Request := CreateApiRequestFromExtractedData(CaptchaData);

    if not Assigned(Request) or (Request.Count = 0) then
    begin
      Log('Failed to create API request', 'Error');
      Exit;
    end;

    // Send request to API
    Response := SendApiRequest(Request);

    if not Assigned(Response) then
    begin
      Log('No response received from API', 'Error');
      Exit;
    end;

    // Process response
    if Response.GetValue<Boolean>('success', False) then
    begin
      Log('API response received successfully, applying solution', 'Info');
      ApplySolution(Response);
    end
    else
    begin
      HandleApiError(Response);
    end;
  finally
    FreeAndNil(Request);
    FreeAndNil(Response);
  end;
end;

function TPopularCaptchaSolver.CreateApiRequestFromExtractedData(const CaptchaData: TJSONObject): TJSONObject;
var
  Task: TJSONObject;
  WebsiteURL, WebsiteKey: string;
  CaptchaType: string;
  ImagesArray, ExamplesArray, ChoicesArray: TJSONArray;
begin
  Result := TJSONObject.Create;
  Task := TJSONObject.Create;

  try
    // Determine captcha type based on extracted data
    case FCaptchaType of
      ctMultiSelect: CaptchaType := 'objectTag';
      ctBbox: CaptchaType := 'objectClick';
      ctBboxDD: CaptchaType := 'objectDrag';
      else CaptchaType := 'objectClassify';
    end;

    // Get website URL and key
    WebsiteURL := ExtractWebsiteURLFromPage;
    WebsiteKey := ExtractWebsiteKeyFromPage;

    // Clone the arrays from CaptchaData
    ImagesArray := CaptchaData.GetValue<TJSONArray>('images').Clone as TJSONArray;
    ExamplesArray := CaptchaData.GetValue<TJSONArray>('examples').Clone as TJSONArray;
    ChoicesArray := CaptchaData.GetValue<TJSONArray>('choices').Clone as TJSONArray;

    // Add basic information to the request
    Result.AddPair('apiKey', FApiKey);
    Result.AddPair('source', 'delphi');
    Result.AddPair('version', '1.0.0');
    Result.AddPair('appID', TJSONNumber.Create(0));

    // Build task object
    Task.AddPair('type', 'PopularCaptchaImage');
    Task.AddPair('queries', ImagesArray);
    Task.AddPair('examples', ExamplesArray);
    Task.AddPair('question', CaptchaData.GetValue<string>('target', ''));
    Task.AddPair('questionType', CaptchaType);
    Task.AddPair('websiteURL', WebsiteURL);
    Task.AddPair('websiteKEY', WebsiteKey);

    // For multiselect captchas, include choices
    if FCaptchaType = ctMultiSelect then
      Task.AddPair('choices', ChoicesArray)
    else
      ChoicesArray.Free; // Free if not used

    // Add screenshot=true for BboxDD
    if FCaptchaType = ctBboxDD then
      Task.AddPair('screenshot', TJSONTrue.Create);

    // Add task to main request
    Result.AddPair('task', Task);

    Log('API request created: ' + Result.ToJSON, 'Debug');
  except
    on E: Exception do
    begin
      Log('Error creating API request: ' + E.Message, 'Error');
      FreeAndNil(Result);
      FreeAndNil(Task);
      raise;
    end;
  end;
end;

function TPopularCaptchaSolver.ExtractWebsiteURLFromPage: string;
const
  Script =
    'try {' +
    '  // Try to get from URL parameters' +
    '  const urlParams = new URLSearchParams(window.location.hash);' +
    '  const host = urlParams.get("host");' +
    '  if (host) return host;' +
    '  ' +
    '  // Otherwise return current host' +
    '  return window.location.host;' +
    '} catch(e) { return ""; }';
begin
  Result := ExecScriptSync(Script);
  if Result = '' then
    Result := 'unknown.com';

  Log('Website URL extracted: ' + Result, 'Debug');
end;

function TPopularCaptchaSolver.ExtractWebsiteKeyFromPage: string;
const
  Script =
    'try {' +
    '  // Try to get from URL parameters' +
    '  const urlParams = new URLSearchParams(window.location.hash);' +
    '  const sitekey = urlParams.get("sitekey");' +
    '  if (sitekey) return sitekey;' +
    '  ' +
    '  // Try to get from page' +
    '  const dataKeys = document.querySelectorAll("[data-sitekey]");' +
    '  if (dataKeys.length > 0) return dataKeys[0].getAttribute("data-sitekey");' +
    '  ' +
    '  return "";' +
    '} catch(e) { return ""; }';
begin
  Result := ExecScriptSync(Script);
  if Result = '' then
    Result := 'unknown';

  Log('Website key extracted: ' + Result, 'Debug');
end;

procedure TPopularCaptchaSolver.ScriptCompletedHandler(Sender: TObject; aErrorCode: HRESULT;
  const aResultObjectAsJson: wvstring; aExecutionID: Integer);
begin
  if aErrorCode = S_OK then
    FScriptResult := aResultObjectAsJson
  else
    FScriptResult := '';

  FScriptCompleted := True;
  Log(Format('Script (ID %d) completed with result: %s (Error: %X)',
    [aExecutionID, FScriptResult, aErrorCode]), 'Debug');
end;

function TPopularCaptchaSolver.ExecuteScriptAndWait(const Script: string;
  ExecutionID: Integer; TimeoutMs: Cardinal): wvstring;
var
  StartTime: Cardinal;
  TimeoutOccurred: Boolean;
begin
  Log('Executing script: ' + Script, 'Debug');

  FScriptResult := '';
  FScriptCompleted := False;
  TimeoutOccurred := False;

  if not FWebBrowser.ExecuteScript(Script, ExecutionID) then
  begin
    Log('Script execution failed (ID: ' + IntToStr(ExecutionID) + ')', 'Error');
    Exit('');
  end;

  StartTime := GetTickCount;
  while (not FScriptCompleted) and (not TimeoutOccurred) do
  begin
    TimeoutOccurred := (GetTickCount - StartTime) >= TimeoutMs;
    if not TimeoutOccurred then
    begin
      Application.ProcessMessages;
      Sleep(10);
    end;
  end;

  if TimeoutOccurred then
  begin
    Log(Format('Script execution timed out after %d ms (ID: %d)',
      [TimeoutMs, ExecutionID]), 'Error');
    Result := '';
  end
  else
    Result := FScriptResult;
end;

function TPopularCaptchaSolver.ExecScriptSync(const Script: string): string;
var
  ExecutionID: Integer;
  Result1: string;
begin
  ExecutionID := Random(10000);

  Result1 := ExecuteScriptAndWait(Script, ExecutionID, 5000);

  if Result1 = 'true' then
    Result := 'true'
  else if Result1 = 'false' then
    Result := 'false'
  else if (Result1 = 'null') or (Result1 = '') then
    Result := ''
  else
    Result := Result1;

  Log(Format('Script execution (ID:%d) result: %s',
    [ExecutionID, IfThen(Result = '', 'empty/null', Result)]), 'Debug');
end;

function TPopularCaptchaSolver.IsWidget: Boolean;
const
  Script = 'try {' + '  const e = document.body.getBoundingClientRect();' +
    '  return (e?.width === 0 || e?.height === 0) ? false : (document.querySelector("div.check") !== null);'
    + '} catch(e) { return false; }';
begin
  Result := ExecScriptSync(Script) = 'true';
  Log(Format('Widget detection: %s', [BoolToStr(Result, True)]), 'Debug');
end;

function TPopularCaptchaSolver.IsSolved: Boolean;
const
  Script = 'try { return document.querySelector("#checkbox")?.getAttribute("aria-checked") === "true"; } catch(e) { return false; }';
var
  Result1: string;
begin
  // Check in captcha iframe first if available
  if FCaptchaFrameFound and Assigned(FCaptchaFrame) then
    Result1 := ExecuteScriptInCaptchaFrame(Script, Random(10000), 1000)
  else
    Result1 := ExecScriptSync(Script);

  Result := Result1 = 'true';
  Log(Format('Solved status: %s', [BoolToStr(Result, True)]), 'Debug');
end;

function TPopularCaptchaSolver.IsMultiSelect: Boolean;
const
  Script = 'try { return (document.querySelector(".task-answers") !== null) && (document.querySelector(".task-image") !== null); } catch(e) { return false; }';
begin
  Result := ExecScriptSync(Script) = 'true';
  Log(Format('MultiSelect detection: %s', [BoolToStr(Result, True)]), 'Debug');
end;

function TPopularCaptchaSolver.IsGrid: Boolean;
const
  Script = 'try { return document.querySelectorAll(".task-image .image").length === 9; } catch(e) { return false; }';
begin
  Result := ExecScriptSync(Script) = 'true';
  Log(Format('Grid detection: %s', [BoolToStr(Result, True)]), 'Debug');
end;

function TPopularCaptchaSolver.IsBbox: Boolean;
const
  Script = 'try { return document.querySelector(".bounding-box-example") !== null; } catch(e) { return false; }';
begin
  Result := ExecScriptSync(Script) = 'true';
  Log(Format('Bbox detection: %s', [BoolToStr(Result, True)]), 'Debug');
end;

function TPopularCaptchaSolver.IsBboxDD: Boolean;
const
  Script = 'try { return document.querySelector(".challenge-header") !== null; } catch(e) { return false; }';
begin
  Result := ExecScriptSync(Script) = 'true';
  Log(Format('BboxDD detection: %s', [BoolToStr(Result, True)]), 'Debug');
end;

function TPopularCaptchaSolver.IsOnline: Boolean;
var
  Flags: DWORD;
begin
  Result := InternetGetConnectedState(@Flags, 0);
  Log(Format('Online status: %s', [BoolToStr(Result, True)]), 'Debug');
end;

function TPopularCaptchaSolver.DetectCaptchaType: TCaptchaType;
begin
  // If a specific detection method is forced, use that
  if FDetectionMethod >= 0 then
  begin
    case FDetectionMethod of
      0:
        Result := ctWidget;
      1:
        Result := ctMultiSelect;
      2:
        Result := ctGrid;
      3:
        Result := ctBbox;
      4:
        Result := ctBboxDD;
    else
      Result := ctUnknown;
    end;

    Log(Format('Using forced CAPTCHA type: %s',
      [GetEnumName(TypeInfo(TCaptchaType), Ord(Result))]), 'Info');
    Exit;
  end;

  // Auto detection
  if IsWidget then
    Result := ctWidget
  else if IsMultiSelect then
    Result := ctMultiSelect
  else if IsGrid then
    Result := ctGrid
  else if IsBbox then
    Result := ctBbox
  else if IsBboxDD then
    Result := ctBboxDD
  else
    Result := ctUnknown;

  Log(Format('Detected CAPTCHA type: %s', [GetEnumName(TypeInfo(TCaptchaType),
    Ord(Result))]), 'Info');
end;

function TPopularCaptchaSolver.ClickElement(const Selector: string): Boolean;
const
  Script = 'try {' + '  const el = document.querySelector("%s");' +
    '  if(el) { el.click(); return true; }' + '  return false;' +
    '} catch(e) { return false; }';
var
  FormattedScript: string;
  Result1: string;
begin
  FormattedScript := Format(Script, [Selector]);

  // Try clicking in captcha iframe first if available
  if FCaptchaFrameFound and Assigned(FCaptchaFrame) then
    Result1 := ExecuteScriptInCaptchaFrame(FormattedScript, Random(10000), 5000)
  else
    Result1 := ExecScriptSync(FormattedScript);

  Result := Result1 = 'true';
  Sleep(100);
  Log(Format('Click result for %s: %s', [Selector, BoolToStr(Result, True)]), 'Debug');
end;

function TPopularCaptchaSolver.ClickGridCell(Index: Integer): Boolean;
const
  Script = 'try {' +
    '  const cells = document.querySelectorAll(".task-image .image");' +
    '  if(cells[%d]) { cells[%d].click(); return true; }' + '  return false;' +
    '} catch(e) { return false; }';
begin
  Result := ExecScriptSync(Format(Script, [Index, Index])) = 'true';
  Log(Format('Grid cell click at index %d: %s', [Index, BoolToStr(Result, True)]), 'Debug');
end;

function TPopularCaptchaSolver.ClickCanvas(X, Y: Integer): Boolean;
const
  Script = 'try {' + '  const canvas = document.querySelector("canvas");' +
    '  if(canvas) {' + '    const rect = canvas.getBoundingClientRect();' +
    '    const evt = new MouseEvent("click", {' +
    '      bubbles: true, cancelable: true, view: window,' +
    '      clientX: rect.left + %d, clientY: rect.top + %d' + '    });' +
    '    canvas.dispatchEvent(evt); return true;' + '  }' + '  return false;' +
    '} catch(e) { return false; }';
var
  FormattedScript: string;
  Result1: string;
begin
  FormattedScript := Format(Script, [X, Y]);

  // Try clicking in captcha iframe first if available
  if FCaptchaFrameFound and Assigned(FCaptchaFrame) then
    Result1 := ExecuteScriptInCaptchaFrame(FormattedScript, Random(10000), 5000)
  else
    Result1 := ExecScriptSync(FormattedScript);

  Result := Result1 = 'true';
  Log(Format('Canvas click at (%d,%d): %s', [X, Y, BoolToStr(Result, True)]), 'Debug');
end;

function TPopularCaptchaSolver.ClickMatchingElement(const Text: string): Boolean;
const
  Script = 'try {' +
    '  const elements = document.querySelectorAll(".answer-text");' +
    '  for(const el of elements) {' + '    if(el.textContent.trim() === "%s") {'
    + '      el.click(); return true;' + '    }' + '  }' + '  return false;' +
    '} catch(e) { return false; }';
var
  FormattedScript: string;
  Result1: string;
begin
  FormattedScript := Format(Script, [Text]);

  // Try clicking in captcha iframe first if available
  if FCaptchaFrameFound and Assigned(FCaptchaFrame) then
    Result1 := ExecuteScriptInCaptchaFrame(FormattedScript, Random(10000), 5000)
  else
    Result1 := ExecScriptSync(FormattedScript);

  Result := Result1 = 'true';
  Log(Format('Matching element click for "%s": %s', [Text, BoolToStr(Result, True)]), 'Debug');
end;

procedure TPopularCaptchaSolver.SimulateMouseAction(const Action, Selector: string; X, Y: Integer);
const
  Script = 'try {' + '  const el = document.querySelector("%s");' + '  if(el) {'
    + '    const rect = el.getBoundingClientRect();' +
    '    const evt = new MouseEvent("%s", {' +
    '      bubbles: true, cancelable: true,' +
    '      clientX: rect.left + %d, clientY: rect.top + %d' + '    });' +
    '    el.dispatchEvent(evt);' + '    return true;' + '  }' +
    '  return false;' + '} catch(e) { return false; }';
var
  FormattedScript: string;
begin
  FormattedScript := Format(Script, [Selector, Action, X, Y]);

  // Execute in correct context
  if FCaptchaFrameFound and Assigned(FCaptchaFrame) then
    ExecuteScriptInCaptchaFrame(FormattedScript, Random(10000), 5000)
  else
    ExecScriptSync(FormattedScript);

  Log(Format('Mouse %s simulated at (%d,%d) on %s', [Action, X, Y, Selector]), 'Debug');
end;

function TPopularCaptchaSolver.ExtractCaptchaData: TJSONObject;
const
  Script = 'try {' + '  const data = {' +
    '    target: document.querySelector(".prompt-text")?.textContent || "",' +
    '    images: Array.from(document.querySelectorAll(".task-image .image"))' +
    '      .map(el => el.style.backgroundImage.replace(/url\\("(.+)"\\)/, "$1")),'
    + '    examples: Array.from(document.querySelectorAll(".example-image .image, .challenge-example .image .image"))'
    + '      .map(el => el.style.backgroundImage.replace(/url\\("(.+)"\\)/, "$1")),'
    + '    choices: Array.from(document.querySelectorAll(".answer-text"))' +
    '      .map(el => el.outerText)' + '  };' + '  return JSON.stringify(data);'
    + '} catch(e) { return ""; }';
var
  JsonStr: string;
  JsonValue: TJSONValue;
begin
  // Execute in correct context
  if FCaptchaFrameFound and Assigned(FCaptchaFrame) then
    JsonStr := ExecuteScriptInCaptchaFrame(Script, Random(10000), 5000)
  else
    JsonStr := ExecScriptSync(Script);

  if (JsonStr <> '') and (JsonStr <> 'null') then
  begin
    JsonValue := TJSONObject.ParseJSONValue(JsonStr);
    if JsonValue is TJSONObject then
    begin
      Result := TJSONObject(JsonValue);
      Log('Successfully extracted CAPTCHA data', 'Debug');
    end
    else
    begin
      FreeAndNil(JsonValue);
      Result := TJSONObject.Create;
      Log('Failed to parse JSON response as object', 'Error');
    end;
  end
  else
  begin
    Result := TJSONObject.Create;
    Log('No data extracted from CAPTCHA', 'Warn');
  end;
end;

function TPopularCaptchaSolver.GetExamples(const Selector: string): TJSONArray;
const
  Script = 'try {' +
    '  return JSON.stringify(Array.from(document.querySelectorAll("%s"))' +
    '    .map(el => el.style.backgroundImage.replace(/url\\("(.+)"\\)/, "$1")));'
    + '} catch(e) { return "[]"; }';
var
  JsonStr: string;
  JsonValue: TJSONValue;
begin
  // Execute in correct context
  if FCaptchaFrameFound and Assigned(FCaptchaFrame) then
    JsonStr := ExecuteScriptInCaptchaFrame(Format(Script, [Selector]), Random(10000), 5000)
  else
    JsonStr := ExecScriptSync(Format(Script, [Selector]));

  if (JsonStr <> '') and (JsonStr <> 'null') then
  begin
    JsonValue := TJSONObject.ParseJSONValue(JsonStr);
    if JsonValue is TJSONArray then
    begin
      Result := TJSONArray(JsonValue);
      Log(Format('Retrieved %d examples', [Result.Count]), 'Debug');
    end
    else
    begin
      FreeAndNil(JsonValue);
      Result := TJSONArray.Create;
      Log('Failed to parse examples as JSON array', 'Error');
    end;
  end
  else
  begin
    Result := TJSONArray.Create;
    Log('No examples found', 'Warn');
  end;
end;

function TPopularCaptchaSolver.GetBase64FromUrl(const Url: string): string;
var
  Http: TNetHTTPClient;
  Response: IHTTPResponse;
  Stream: TMemoryStream;
begin
  Result := '';
  if Url = '' then
    Exit;

  Http := TNetHTTPClient.Create(nil);
  Stream := TMemoryStream.Create;
  try
    try
      Response := Http.Get(Url, Stream);
      if Response.StatusCode = 200 then
      begin
        Stream.Position := 0;
        Result := TNetEncoding.Base64.EncodeBytesToString(Stream.Memory, Stream.Size);
        Log('Successfully converted URL to base64', 'Debug');
      end
      else
        Log(Format('Failed to fetch URL: %s, Status: %d', [Url, Response.StatusCode]), 'Error');
    except
      on E: Exception do
        Log('Error in GetBase64FromUrl: ' + E.Message, 'Error');
    end;
  finally
    Stream.Free;
    Http.Free;
  end;
end;

function TPopularCaptchaSolver.SliceCanvas: string;
const
  Script = 'try {' + '  const canvas = document.querySelector("canvas");' +
    '  if (!canvas) return "";' +
    '  return canvas.toDataURL("image/jpeg").replace(/^data:image\\/jpeg;base64,/, "");'
    + '} catch(e) { return ""; }';
var
  Result1: string;
begin
  // Execute in correct context
  if FCaptchaFrameFound and Assigned(FCaptchaFrame) then
    Result1 := ExecuteScriptInCaptchaFrame(Script, Random(10000), 5000)
  else
    Result1 := ExecScriptSync(Script);

  Result := Result1;
  Log('Canvas image extracted: ' + BoolToStr(Result <> '', True), 'Debug');
end;

function TPopularCaptchaSolver.CreateApiRequest(CaptchaType: TCaptchaType): TJSONObject;
var
  CaptchaData: TJSONObject;
  Task: TJSONObject;
  CanvasImage: string;
  WebsiteURL, WebsiteKey: string;
begin
  Result := TJSONObject.Create;
  Task := TJSONObject.Create;

  // Extract website information
  WebsiteURL := ExtractWebsiteURLFromPage;
  WebsiteKey := ExtractWebsiteKeyFromPage;

  CaptchaData := ExtractCaptchaData;

  Result.AddPair('apiKey', TJSONString.Create(FApiKey));
  Result.AddPair('source', TJSONString.Create('desktopApp'));
  Result.AddPair('version', TJSONString.Create('1.0.0'));

  Task.AddPair('type', TJSONString.Create('PopularCaptchaImage'));
  Task.AddPair('websiteURL', TJSONString.Create(WebsiteURL));
  Task.AddPair('websiteKEY', TJSONString.Create(WebsiteKey));

  case CaptchaType of
    ctMultiSelect:
      begin
        Task.AddPair('questionType', TJSONString.Create('objectTag'));
        Task.AddPair('question',
          TJSONString.Create(CaptchaData.GetValue<string>('target', '')));
        Task.AddPair('queries', CaptchaData.GetValue<TJSONArray>('images')
          .Clone as TJSONArray);
        Task.AddPair('examples', CaptchaData.GetValue<TJSONArray>('examples')
          .Clone as TJSONArray);
        Task.AddPair('choices', CaptchaData.GetValue<TJSONArray>('choices')
          .Clone as TJSONArray);
      end;

    ctGrid:
      begin
        Task.AddPair('questionType', TJSONString.Create('objectClassify'));
        Task.AddPair('question',
          TJSONString.Create(CaptchaData.GetValue<string>('target', '')));
        Task.AddPair('queries', CaptchaData.GetValue<TJSONArray>('images')
          .Clone as TJSONArray);
        Task.AddPair('examples', CaptchaData.GetValue<TJSONArray>('examples')
          .Clone as TJSONArray);
      end;

    ctBbox:
      begin
        Task.AddPair('questionType', TJSONString.Create('objectClick'));
        Task.AddPair('question',
          TJSONString.Create(CaptchaData.GetValue<string>('target', '')));
        CanvasImage := SliceCanvas;
        if CanvasImage <> '' then
        begin
          Task.AddPair('queries',
            TJSONArray.Create(TJSONString.Create(CanvasImage)));
          Task.AddPair('examples', CaptchaData.GetValue<TJSONArray>('examples')
            .Clone as TJSONArray);
        end;
      end;

    ctBboxDD:
      begin
        Task.AddPair('questionType', TJSONString.Create('objectDrag'));
        Task.AddPair('question',
          TJSONString.Create(CaptchaData.GetValue<string>('target', '')));
        CanvasImage := SliceCanvas;
        if CanvasImage <> '' then
        begin
          Task.AddPair('queries',
            TJSONArray.Create(TJSONString.Create(CanvasImage)));
          Task.AddPair('screenshot', TJSONTrue.Create);
        end;
      end;
  end;

  Result.AddPair('task', Task);
  FreeAndNil(CaptchaData);
  Log('API request created for CAPTCHA type: ' +
    GetEnumName(TypeInfo(TCaptchaType), Ord(CaptchaType)), 'Debug');
end;

procedure TPopularCaptchaSolver.HandleApiError(Response: TJSONObject);
var
  ErrorMsg: string;
  ErrorCode: Integer;
begin
  ErrorCode := Response.GetValue<Integer>('code', 0);
  ErrorMsg := Response.GetValue<string>('msg', 'Unknown error');

  Log(Format('API error (code %d): %s', [ErrorCode, ErrorMsg]), 'Error');

  // Handle specific error codes
  case ErrorCode of
    1, 10, 18, 110:
      begin
        Log('Authentication error: ' + ErrorMsg, 'Error');
        // Invalid API key or auth issues
      end;

    17, 19, 429:
      begin
        Log('Rate limit error: ' + ErrorMsg, 'Error');
        Sleep(3000); // Wait before retry
      end;

    4, 3, 12, 13, 14, 115:
      begin
        Log('CAPTCHA processing error: ' + ErrorMsg, 'Error');
        RefreshIframes;
      end;
  end;
end;

procedure TPopularCaptchaSolver.ApplySolution(const Response: TJSONObject);
var
  Answers: TJSONArray;
  CaptchaType: TCaptchaType;
begin
  if not Response.GetValue<Boolean>('success', False) then
  begin
    HandleApiError(Response);
    Exit;
  end;

  Answers := Response.GetValue<TJSONArray>('answers', nil);
  if not Assigned(Answers) or (Answers.Count = 0) then
  begin
    Log('No answers in response', 'Warn');
    Exit;
  end;

  CaptchaType := DetectCaptchaType;
  Log(Format('Applying solution for %s with %d answers',
    [GetEnumName(TypeInfo(TCaptchaType), Ord(CaptchaType)), Answers.Count]), 'Info');

  case CaptchaType of
    ctMultiSelect:
      ApplyMultiSelect(Answers);
    ctGrid:
      ApplyGrid(Answers);
    ctBbox:
      ApplyBbox(Answers);
    ctBboxDD:
      ApplyBboxDD(Answers);
  else
    Log('Unknown CAPTCHA type, cannot apply solution', 'Error');
  end;

  // Submit after applying solution
  if CaptchaType <> ctUnknown then
    ClickElement('.button-submit');
end;

procedure TPopularCaptchaSolver.ApplyMultiSelect(const Answers: TJSONArray);
var
  I: Integer;
  Answer: string;
begin
  for I := 0 to Answers.Count - 1 do
  begin
    try
      if Answers.Items[I] is TJSONString then
        Answer := TJSONString(Answers.Items[I]).Value
      else
        Continue;

      Log('Clicking matching element: ' + Answer, 'Debug');
      ClickMatchingElement(Answer);
      Sleep(Random(300) + 200); // Random delay between clicks
    except
      on E: Exception do
        Log('Error applying multi-select answer: ' + E.Message, 'Error');
    end;
  end;
end;

procedure TPopularCaptchaSolver.ApplyGrid(const Answers: TJSONArray);
var
  I: Integer;
begin
  for I := 0 to Answers.Count - 1 do
  begin
    try
      if (Answers.Items[I] is TJSONTrue) or
        ((Answers.Items[I] is TJSONString) and
        (TJSONString(Answers.Items[I]).Value = '1')) then
      begin
        Log(Format('Clicking grid cell %d', [I]), 'Debug');
        ClickGridCell(I);
        Sleep(Random(300) + 200); // Random delay between clicks
      end;
    except
      on E: Exception do
        Log('Error applying grid answer: ' + E.Message, 'Error');
    end;
  end;
end;

procedure TPopularCaptchaSolver.ApplyBbox(const Answers: TJSONArray);
var
  I: Integer;
  Coord: TJSONObject;
  X, Y: Integer;
begin
  for I := 0 to Answers.Count - 1 do
  begin
    try
      if Answers.Items[I] is TJSONObject then
      begin
        Coord := TJSONObject(Answers.Items[I]);
        X := Coord.GetValue<Integer>('x', 0);
        Y := Coord.GetValue<Integer>('y', 0);

        Log(Format('Clicking canvas at (%d,%d)', [X, Y]), 'Debug');
        ClickCanvas(X, Y);
        Sleep(Random(300) + 200); // Random delay between clicks
      end;
    except
      on E: Exception do
        Log('Error applying bbox answer: ' + E.Message, 'Error');
    end;
  end;
end;

procedure TPopularCaptchaSolver.ApplyBboxDD(const Answers: TJSONArray);
var
  I: Integer;
  Coord: TJSONObject;
  StartX, StartY, EndX, EndY: Integer;
  StartValue: TJSONValue;
begin
  for I := 0 to Answers.Count - 1 do
  begin
    try
      if Answers.Items[I] is TJSONObject then
      begin
        Coord := TJSONObject(Answers.Items[I]);
        StartValue := Coord.GetValue('start');
        if (StartValue <> nil) and (StartValue is TJSONArray) then
        begin
          StartX := TJSONArray(StartValue).Items[0].GetValue<Integer>;
          StartY := TJSONArray(StartValue).Items[1].GetValue<Integer>;
        end;
        StartValue := Coord.GetValue('end');
        if (StartValue <> nil) and (StartValue is TJSONArray) then
        begin
          EndX := TJSONArray(StartValue).Items[0].GetValue<Integer>;
          EndY := TJSONArray(StartValue).Items[1].GetValue<Integer>;
        end;

        Log(Format('Drag from (%d,%d) to (%d,%d)', [StartX, StartY, EndX, EndY]), 'Debug');
        HandleCanvasDD(StartX, StartY, EndX, EndY, Random(500) + 500);
        Sleep(Random(300) + 200); // Random delay between operations
      end;
    except
      on E: Exception do
        Log('Error applying bbox drag-drop answer: ' + E.Message, 'Error');
    end;
  end;
end;

procedure TPopularCaptchaSolver.HandleCanvasDD(StartX, StartY, EndX, EndY: Integer; Delay: Integer);
const
  Script = 'try {' + '  const canvas = document.querySelector("canvas");' +
    '  if (!canvas) return false;' +
    '  const rect = canvas.getBoundingClientRect();' +
    '  const startX = rect.left + %d;' + '  const startY = rect.top + %d;' +
    '  const endX = rect.left + %d;' + '  const endY = rect.top + %d;' +

    '  function simulateEvent(type, x, y) {' +
    '    const event = new MouseEvent(type, {' + '      view: window,' +
    '      bubbles: true,' + '      cancelable: true,' + '      clientX: x,' +
    '      clientY: y' + '    });' + '    canvas.dispatchEvent(event);'
    + '  }' +

    '  // Mouse down at start position' +
    '  simulateEvent("mousedown", startX, startY);' + '  return true;' +
    '} catch(e) { return false; }';

  MOVE_SCRIPT = 'try {' + '  const canvas = document.querySelector("canvas");' +
    '  if (!canvas) return false;' +
    '  const rect = canvas.getBoundingClientRect();' +
    '  const x = rect.left + %d;' + '  const y = rect.top + %d;' +

    '  const event = new MouseEvent("mousemove", {' + '    view: window,' +
    '    bubbles: true,' + '    cancelable: true,' + '    clientX: x,' +
    '    clientY: y' + '  });' + '  canvas.dispatchEvent(event);' +
    '  return true;' + '} catch(e) { return false; }';

  UP_SCRIPT = 'try {' + '  const canvas = document.querySelector("canvas");' +
    '  if (!canvas) return false;' +
    '  const rect = canvas.getBoundingClientRect();' +
    '  const x = rect.left + %d;' + '  const y = rect.top + %d;' +

    '  const event = new MouseEvent("mouseup", {' + '    view: window,' +
    '    bubbles: true,' + '    cancelable: true,' + '    clientX: x,' +
    '    clientY: y' + '  });' + '  canvas.dispatchEvent(event);' +
    '  return true;' + '} catch(e) { return false; }';

var
  DragSteps: Integer;
  I: Integer;
  CurrentX, CurrentY: Integer;
  StepX, StepY: Double;
  Context: string;
begin
  // Determine if we should execute in iframe or main document
  if FCaptchaFrameFound and Assigned(FCaptchaFrame) then
    Context := 'iframe'
  else
    Context := 'main document';

  Log(Format('Starting canvas drag-drop in %s', [Context]), 'Debug');

  // Start drag
  if Context = 'iframe' then
    ExecuteScriptInCaptchaFrame(Format(Script, [StartX, StartY, EndX, EndY]), Random(10000), 5000)
  else
    ExecScriptSync(Format(Script, [StartX, StartY, EndX, EndY]));

  Sleep(Delay div 4);

  // Calculate intermediate points for move
  DragSteps := 8; // Number of intermediate steps
  StepX := (EndX - StartX) / DragSteps;
  StepY := (EndY - StartY) / DragSteps;

  // Perform mousemove events for each step
  for I := 1 to DragSteps - 1 do
  begin
    CurrentX := Round(StartX + StepX * I);
    CurrentY := Round(StartY + StepY * I);

    if Context = 'iframe' then
      ExecuteScriptInCaptchaFrame(Format(MOVE_SCRIPT, [CurrentX, CurrentY]), Random(10000), 5000)
    else
      ExecScriptSync(Format(MOVE_SCRIPT, [CurrentX, CurrentY]));

    Sleep(Delay div DragSteps);
  end;

  // End drag
  if Context = 'iframe' then
    ExecuteScriptInCaptchaFrame(Format(UP_SCRIPT, [EndX, EndY]), Random(10000), 5000)
  else
    ExecScriptSync(Format(UP_SCRIPT, [EndX, EndY]));

  Sleep(Delay div 4);

  Log(Format('Drag complete from (%d,%d) to (%d,%d)', [StartX, StartY, EndX, EndY]), 'Debug');
end;

procedure TPopularCaptchaSolver.RefreshIframes;
const
  Script = 'try {' + '  const frames = document.querySelectorAll("iframe");' +
    '  for (const frame of frames) {' + '    try {' +
    '      const src = frame.src;' + '      frame.src = "about:blank";' +
    '      setTimeout(() => { frame.src = src; }, 100);' +
    '    } catch(e) { console.error(e); }' + '  }' + '  return true;' +
    '} catch(e) { return false; }';
begin
  ExecScriptSync(Script);
  Log('Refreshed CAPTCHA iframes', 'Info');
  Sleep(1000); // Wait for iframes to reload

  // Reset captcha frame reference
  FCaptchaFrameFound := False;
  FCaptchaFrame := nil;
end;

function TPopularCaptchaSolver.SendApiRequest(const Request: TJSONObject): TJSONObject;
var
  Http: TNetHTTPClient;
  Response: IHTTPResponse;
  ResponseStr: string;
  JsonValue: TJSONValue;
begin
  Http := TNetHTTPClient.Create(nil);
  try
    Http.ContentType := 'application/json';
    Http.Accept := 'application/json';

    // Configure authentication header
    Http.CustomHeaders['apikey'] := FApiKey;

    try
      Log('Sending request to API: ' + FApiEndpoint, 'Info');

      // Convert JSON object to string
      ResponseStr := Request.ToString;
      Log('Request payload: ' + ResponseStr, 'Debug');

      // Send POST request
      Response := Http.Post(FApiEndpoint, TStringStream.Create(ResponseStr, TEncoding.UTF8));

      // Get response
      ResponseStr := Response.ContentAsString;
      Log('API response received: ' + ResponseStr, 'Debug');

      // Parse JSON response
      JsonValue := TJSONObject.ParseJSONValue(ResponseStr);
      if JsonValue is TJSONObject then
      begin
        Result := TJSONObject(JsonValue);
        Log('API response parsed successfully', 'Info');
      end
      else
      begin
        // Free memory if not a JSON object
        if Assigned(JsonValue) then
          FreeAndNil(JsonValue);

        // Create error object
        Result := TJSONObject.Create;
        Result.AddPair('success', TJSONBool.Create(False));
        Result.AddPair('error', 'Invalid JSON response');
        Log('Failed to parse API response as JSON object', 'Error');
      end;
    except
      on E: Exception do
      begin
        Log('API request failed: ' + E.Message, 'Error');
        Result := TJSONObject.Create;
        Result.AddPair('success', TJSONBool.Create(False));
        Result.AddPair('error', E.Message);
      end;
    end;
  finally
    Http.Free;
  end;
end;

procedure TPopularCaptchaSolver.SetLanguageToEnglish;
const
  SCRIPT_MULTI = 'try {' +
    '  const langBtn = document.querySelector(".display-language.button");' +
    '  if (langBtn) {' + '    langBtn.click();' + '    setTimeout(() => {' +
    '      const engOption = document.querySelector(".language-selector .option:nth-child(23)");'
    + '      if (engOption) engOption.click();' + '    }, 200);' +
    '    return true;' + '  }' + '  return false;' +
    '} catch(e) { return false; }';
begin
  if FEnglishEnabled then
  begin
    if FCaptchaFrameFound and Assigned(FCaptchaFrame) then
    begin
      if ExecuteScriptInCaptchaFrame(SCRIPT_MULTI, Random(10000), 5000) = 'true' then
        Log('Language set to English in iframe', 'Info')
      else
        Log('Failed to set language to English in iframe', 'Warn');
    end
    else
    begin
      if ExecScriptSync(SCRIPT_MULTI) = 'true' then
        Log('Language set to English', 'Info')
      else
        Log('Failed to set language to English', 'Warn');
    end;

    Sleep(500); // Wait for language change to apply
  end;
end;

procedure TPopularCaptchaSolver.OnRetrieveMHTMLCompleted(Sender: TObject;
  aResult: Boolean; const aMHTML: wvstring);
begin
  if aResult and (aMHTML <> '') then
  begin
    Log('MHTML retrieved successfully. Size: ' + IntToStr(Length(aMHTML)), 'Info');

    // Store MHTML content
    if not Assigned(FMHTMLContent) then
      FMHTMLContent := TStringList.Create;

    FMHTMLContent.Text := aMHTML;

    // Extract data and process
    ProcessCaptchaFromMHTML(aMHTML);
  end
  else
    Log('Failed to retrieve MHTML', 'Error');
end;

function TPopularCaptchaSolver.ExtractMhtmlBoundary(const MHTMLContent: string): string;
var
  Match: TMatch;
begin
  Match := TRegEx.Match(MHTMLContent, 'boundary="(.*?)"', [roIgnoreCase]);
  if Match.Success then
    Result := Match.Groups[1].Value
  else
    Result := '';

  Log('MHTML boundary: ' + Result, 'Debug');
end;

function TPopularCaptchaSolver.SplitMhtmlParts(const MHTMLContent, Boundary: string): TArray<string>;
var
  Regex: TRegEx;
  Matches: TMatchCollection;
  I: Integer;
begin
  if Boundary = '' then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  Regex := TRegEx.Create('--' + Boundary + '[\r\n]+' +
                         'Content-Type: (.*?)[\r\n]+' +
                         '(.*?)' +
                         '(--' + Boundary + '|$)',
                         [roSingleLine]);

  Matches := Regex.Matches(MHTMLContent);
  SetLength(Result, Matches.Count);

  for I := 0 to Matches.Count - 1 do
    Result[I] := Matches[I].Value;

  Log(Format('Split MHTML into %d parts', [Length(Result)]), 'Debug');
end;

function TPopularCaptchaSolver.FindHtmlPartInMhtml(const MhtmlParts: TArray<string>): string;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to High(MhtmlParts) do
  begin
    if (Pos('Content-Type: text/html', MhtmlParts[I]) > 0) or
       (Pos('Content-Type: application/xhtml+xml', MhtmlParts[I]) > 0) then
    begin
      Result := MhtmlParts[I];
      Break;
    end;
  end;

  if Result <> '' then
    Log('Found HTML part in MHTML', 'Debug')
  else
    Log('No HTML part found in MHTML', 'Warn');
end;

procedure TPopularCaptchaSolver.ProcessCaptchaFromMHTML(const MHTMLContent: string);
var
  CaptchaData: TJSONObject;
  ApiRequest: TJSONObject;
  ApiResponse: TJSONObject;
  ImagesArray: TJSONArray;
begin
  // Extract captcha data from MHTML
  CaptchaData := ExtractCaptchaDataFromMHTML(MHTMLContent);

  try
    // Verify there is data to process
    ImagesArray := CaptchaData.GetValue<TJSONArray>('images');
    if (ImagesArray = nil) or (ImagesArray.Count = 0) then
    begin
      Log('No images found in MHTML - cannot solve', 'Error');

      // Try traditional method
      Log('Attempting traditional method...', 'Info');
      SolveWithTraditionalMethod;
      Exit;
    end;

    // Create and send API request
    ApiRequest := CreateApiRequestFromMHTML(CaptchaData);
    ApiResponse := SendApiRequest(ApiRequest);

    // Handle response
    if ApiResponse <> nil then
    begin
      if ApiResponse.GetValue<Boolean>('success', False) then
        ApplySolution(ApiResponse)
      else
      begin
        Log('API Error: ' + ApiResponse.GetValue<string>('msg', 'Unknown error'), 'Error');
        SolveWithTraditionalMethod;
      end;
    end;
  finally
    FreeAndNil(CaptchaData);
    FreeAndNil(ApiRequest);
    FreeAndNil(ApiResponse);
  end;
end;

function TPopularCaptchaSolver.ExtractCaptchaDataFromMHTML(const MHTMLContent: string): TJSONObject;
var
  TargetMatch, Match: TMatch;
  ImagesJSON, ExamplesJSON, ChoicesJSON: TJSONArray;
  TargetRegex, ImagesRegex, ExamplesRegex, ChoicesRegex: string;
  ImagesFound, ExamplesFound, ChoicesFound: Boolean;
begin
  Result := TJSONObject.Create;
  ImagesJSON := TJSONArray.Create;
  ExamplesJSON := TJSONArray.Create;
  ChoicesJSON := TJSONArray.Create;

  Log('Analyzing MHTML content (length: ' + IntToStr(Length(MHTMLContent)) + ')', 'Debug');

  try
    // Save MHTML for debugging if needed
    TFile.WriteAllText('debug_mhtml.txt', MHTMLContent);

    // 1. Extract question text
    TargetRegex := '<div[^>]*class="[^"]*prompt-text[^"]*"[^>]*>(.*?)</div>';
    TargetMatch := TRegEx.Match(MHTMLContent, TargetRegex, [roSingleLine, roIgnoreCase]);

    if TargetMatch.Success then
    begin
      Result.AddPair('target', TargetMatch.Groups[1].Value.Trim);
      Log('Extracted question: ' + TargetMatch.Groups[1].Value.Trim, 'Debug');
    end
    else
      Result.AddPair('target', '');

    // 2. Extract images
    ImagesRegex := 'background-image:\s*url\([''"]?(.*?)[''"]?\)';
    Match := TRegEx.Match(MHTMLContent, ImagesRegex, [roSingleLine, roIgnoreCase]);

    ImagesFound := False;
    while Match.Success do
    begin
      ImagesJSON.Add(Match.Groups[1].Value);
      ImagesFound := True;
      Log('Image found: ' + Match.Groups[1].Value, 'Debug');
      Match := Match.NextMatch;
    end;

    // 3. Extract examples (images in examples)
    ExamplesRegex := 'class="[^"]*example[^"]*".*?background-image:\s*url\([''"]?(.*?)[''"]?\)';
    Match := TRegEx.Match(MHTMLContent, ExamplesRegex, [roSingleLine, roIgnoreCase]);

    ExamplesFound := False;
    while Match.Success do
    begin
      ExamplesJSON.Add(Match.Groups[1].Value);
      ExamplesFound := True;
      Log('Example found: ' + Match.Groups[1].Value, 'Debug');
      Match := Match.NextMatch;
    end;

    // 4. Extract text choices
    ChoicesRegex := '<div[^>]*class="[^"]*answer-text[^"]*"[^>]*>(.*?)</div>';
    Match := TRegEx.Match(MHTMLContent, ChoicesRegex, [roSingleLine, roIgnoreCase]);

    ChoicesFound := False;
    while Match.Success do
    begin
      ChoicesJSON.Add(Match.Groups[1].Value.Trim);
      ChoicesFound := True;
      Log('Choice found: ' + Match.Groups[1].Value.Trim, 'Debug');
      Match := Match.NextMatch;
    end;

    // 5. Add all results to JSON
    Result.AddPair('images', ImagesJSON);
    Result.AddPair('examples', ExamplesJSON);
    Result.AddPair('choices', ChoicesJSON);

    // 6. Check if extracted data is sufficient
    if (not ImagesFound) and (not ExamplesFound) and (not ChoicesFound) then
    begin
      Log('No data found in MHTML - check regular expressions', 'Warn');

      // Identify captcha type approximately
      if MHTMLContent.Contains('task-answers') or MHTMLContent.Contains('answer-text') then
        Log('MHTML contains MultiSelect elements', 'Debug');

      if MHTMLContent.Contains('task-image') and
         (TRegEx.Matches(MHTMLContent, 'class="[^"]*task-image[^"]*"').Count >= 9) then
        Log('MHTML contains Grid elements (9 images)', 'Debug');

      if MHTMLContent.Contains('bounding-box') then
        Log('MHTML contains BBox elements', 'Debug');

      // For debugging, check if MHTML contains hCaptcha content
      if MHTMLContent.Contains('hcaptcha') then
        Log('MHTML contains references to hCaptcha', 'Debug')
      else
        Log('WARNING: MHTML does NOT contain references to hCaptcha!', 'Warn');
    end;

    Log('Data extracted from MHTML: ' + Result.ToJSON, 'Debug');
  except
    on E: Exception do
    begin
      Log('Error extracting from MHTML: ' + E.Message, 'Error');
      Result.Free;
      ImagesJSON.Free;
      ExamplesJSON.Free;
      ChoicesJSON.Free;
      Result := TJSONObject.Create;
      Result.AddPair('images', TJSONArray.Create);
      Result.AddPair('examples', TJSONArray.Create);
      Result.AddPair('choices', TJSONArray.Create);
      Result.AddPair('target', '');
    end;
  end;
end;

function TPopularCaptchaSolver.CreateApiRequestFromMHTML(const CaptchaData: TJSONObject): TJSONObject;
var
  Task: TJSONObject;
  WebsiteURL, WebsiteKey: string;
  CaptchaType: string;
begin
  Result := TJSONObject.Create;
  Task := TJSONObject.Create;

  try
    // Determine captcha type based on extracted data
    if CaptchaData.GetValue<TJSONArray>('choices').Count > 0 then
      CaptchaType := 'objectTag'
    else if CaptchaData.GetValue<TJSONArray>('examples').Count > 0 then
    begin
      // If examples are present, it's likely a bbox
      if FCaptchaType = ctBboxDD then
        CaptchaType := 'objectDrag'
      else
        CaptchaType := 'objectClick';
    end
    else
      CaptchaType := 'objectClassify';

    // Get website URL and key
    WebsiteURL := ExtractWebsiteURLFromPage;
    WebsiteKey := ExtractWebsiteKeyFromPage;

    // Add basic information to request
    Result.AddPair('apiKey', FApiKey);
    Result.AddPair('source', 'delphi');
    Result.AddPair('version', '1.0.0');
    Result.AddPair('appID', TJSONNumber.Create(0));

    // Build task object
    Task.AddPair('type', 'PopularCaptchaImage');
    Task.AddPair('queries', CaptchaData.GetValue<TJSONArray>('images').Clone as TJSONArray);
    Task.AddPair('examples', CaptchaData.GetValue<TJSONArray>('examples').Clone as TJSONArray);
    Task.AddPair('question', CaptchaData.GetValue<string>('target', ''));
    Task.AddPair('questionType', CaptchaType);
    Task.AddPair('websiteURL', WebsiteURL);
    Task.AddPair('websiteKEY', WebsiteKey);

    // For multiselect captchas, include choices
    if CaptchaType = 'objectTag' then
      Task.AddPair('choices', CaptchaData.GetValue<TJSONArray>('choices').Clone as TJSONArray);

    // Add screenshot=true for BboxDD
    if CaptchaType = 'objectDrag' then
      Task.AddPair('screenshot', TJSONTrue.Create);

    // Add task to main request
    Result.AddPair('task', Task);

    Log('API request created: ' + Result.ToJSON, 'Debug');
  except
    on E: Exception do
    begin
      Log('Error creating API request: ' + E.Message, 'Error');
      FreeAndNil(Result);
      FreeAndNil(Task);
      raise;
    end;
  end;
end;

procedure TPopularCaptchaSolver.SolveWithTraditionalMethod;
var
  CaptchaType: TCaptchaType;
  ApiRequest: TJSONObject;
  ApiResponse: TJSONObject;
begin
  Log('Trying traditional solving method...', 'Info');

  // Detect captcha type
  CaptchaType := DetectCaptchaType;

  if CaptchaType = ctUnknown then
  begin
    Log('Could not detect captcha type', 'Error');
    Exit;
  end;

  try
    // Create API request for detected type
    ApiRequest := CreateApiRequest(CaptchaType);
    if not Assigned(ApiRequest) then
    begin
      Log('Failed to create API request', 'Error');
      Exit;
    end;

    // Send request to API
    ApiResponse := SendApiRequest(ApiRequest);
    if not Assigned(ApiResponse) then
    begin
      Log('No response received from API', 'Error');
      Exit;
    end;

    // Process response
    if ApiResponse.GetValue<Boolean>('success', False) then
    begin
      Log('API response received successfully, applying solution', 'Info');
      ApplySolution(ApiResponse);
    end
    else
    begin
      HandleApiError(ApiResponse);
    end;
  finally
    FreeAndNil(ApiRequest);
    FreeAndNil(ApiResponse);
  end;
end;

procedure TPopularCaptchaSolver.Solve;
begin
  if not IsOnline then
  begin
    Log('No internet connection', 'Error');
    Exit;
  end;

  if FApiKey = '' then
  begin
    Log('API key not defined', 'Error');
    Exit;
  end;

  if FIsProcessing then
  begin
    Log('Solver is already processing a CAPTCHA', 'Warn');
    Exit;
  end;

  FIsProcessing := True;
  FStop := False;

  try
    // Check if checkbox should be clicked
    if IsWidget and not IsSolved and FAutoOpen then
    begin
      Log('Clicking CAPTCHA checkbox', 'Info');
      ClickElement('#checkbox');
      Sleep(1000); // Wait for CAPTCHA to load
    end;

    // If already solved and not forced to solve again, exit
    if IsSolved and not FAlwaysSolve then
    begin
      Log('CAPTCHA already solved, ignoring', 'Info');
      Exit;
    end;

    // Set English if enabled
    if FEnglishEnabled then
      SetLanguageToEnglish;

    // Try to solve using frame approach first if a captcha frame is found
    if FCaptchaFrameFound and Assigned(FCaptchaFrame) then
    begin
      Log('Using frame-based approach for captcha solving', 'Info');

      // Extract captcha data directly from the frame
      var CaptchaData := ExtractCaptchaData;
      if Assigned(CaptchaData) and (CaptchaData.Count > 0) then
      begin
        var ApiRequest := CreateApiRequest(DetectCaptchaType);
        var ApiResponse := SendApiRequest(ApiRequest);

        try
          if Assigned(ApiResponse) and ApiResponse.GetValue<Boolean>('success', False) then
          begin
            Log('Successfully obtained solution, applying to frame', 'Info');
            ApplySolution(ApiResponse);
          end
          else
          begin
            Log('API response error, falling back to MHTML approach', 'Warn');
            FWebBrowser.OnRetrieveMHTMLCompleted := OnRetrieveMHTMLCompleted;
            FWebBrowser.RetrieveMHTML;
          end;
        finally
          FreeAndNil(CaptchaData);
          FreeAndNil(ApiRequest);
          FreeAndNil(ApiResponse);
        end;
      end
      else
      begin
        Log('Failed to extract captcha data from frame, trying MHTML approach', 'Warn');
        FWebBrowser.OnRetrieveMHTMLCompleted := OnRetrieveMHTMLCompleted;
        FWebBrowser.RetrieveMHTML;
      end;
    end
    else
    begin
      // No captcha frame found, use MHTML approach
      Log('No captcha frame found, using MHTML approach', 'Info');
      FWebBrowser.OnRetrieveMHTMLCompleted := OnRetrieveMHTMLCompleted;
      FWebBrowser.RetrieveMHTML;
    end;
  except
    on E: Exception do
    begin
      Log('Error during solving: ' + E.Message, 'Error');
      Inc(FCurrentRetryCount);

      if FCurrentRetryCount < FMaxRetries then
      begin
        Log(Format('Retrying (%d/%d)...', [FCurrentRetryCount, FMaxRetries]), 'Info');
        Sleep(1000);
        FIsProcessing := False; // Reset to allow retry
        Solve; // Recursive retry
      end
      else
      begin
        Log('Maximum retry count reached, aborting', 'Error');
        FCurrentRetryCount := 0;
        FIsProcessing := False;
      end;
    end;
  end;
end;

procedure TPopularCaptchaSolver.Stop;
begin
  if FIsProcessing then
  begin
    Log('Stopping solver...', 'Info');
    FStop := True;
  end;
end;

procedure TPopularCaptchaSolver.RefreshAndFindCaptchaFrames;
const
  FRAME_DETECTION_SCRIPT = 'try {' +
    '  const frames = document.querySelectorAll("iframe");' +
    '  const captchaFrames = [];' +
    '  for (let i = 0; i < frames.length; i++) {' +
    '    const frame = frames[i];' +
    '    try {' +
    '      if (frame.src.includes("hcaptcha") || frame.src.includes("newassets")) {' +
    '        captchaFrames.push({' +
    '          index: i,' +
    '          src: frame.src,' +
    '          id: frame.id || "",' +
    '          name: frame.name || ""' +
    '        });' +
    '      }' +
    '    } catch (e) { /* Cross-origin access may fail */ }' +
    '  }' +
    '  return JSON.stringify(captchaFrames);' +
    '} catch(e) { return "[]"; }';
var
  FramesJson: string;
  JsonValue: TJSONValue;
  FramesArray: TJSONArray;
  I: Integer;
  FrameObj: TJSONObject;
begin
  FCaptchaFrameFound := False;
  FCaptchaFrame := nil;

  Log('Searching for captcha frames...', 'Info');

  FramesJson := ExecScriptSync(FRAME_DETECTION_SCRIPT);

  if (FramesJson = '') or (FramesJson = '[]') then
  begin
    Log('No captcha frames found', 'Info');
    Exit;
  end;

  JsonValue := TJSONObject.ParseJSONValue(FramesJson);
  if not (JsonValue is TJSONArray) then
  begin
    Log('Invalid frame detection response', 'Error');
    FreeAndNil(JsonValue);
    Exit;
  end;

  FramesArray := TJSONArray(JsonValue);
  try
    Log(Format('Found %d potential captcha frames', [FramesArray.Count]), 'Info');

    // For now, just use the first captcha frame
    if FramesArray.Count > 0 then
    begin
      FrameObj := FramesArray.Items[0] as TJSONObject;
      FCaptchaFrameIndex := FrameObj.GetValue<Integer>('index', -1);
      FCaptchaFrameSrc := FrameObj.GetValue<string>('src', '');
      FCaptchaFrameId := FrameObj.GetValue<string>('id', '');
      FCaptchaFrameName := FrameObj.GetValue<string>('name', '');

      FCaptchaFrameFound := True;

      Log(Format('Selected captcha frame: index=%d, src=%s, id=%s, name=%s',
        [FCaptchaFrameIndex, FCaptchaFrameSrc, FCaptchaFrameId, FCaptchaFrameName]), 'Info');
    end;
  finally
    FreeAndNil(JsonValue);
  end;
end;

function TPopularCaptchaSolver.ExecuteScriptInCaptchaFrame(const Script: string;
  ExecutionID: Integer; TimeoutMs: Cardinal): wvstring;
const
  FRAME_SCRIPT_WRAPPER = 'try {' +
    '  const frames = document.querySelectorAll("iframe");' +
    '  const frame = frames[%d];' +
    '  if (!frame) return "Frame not found";' +
    '  try {' +
    '    const result = frame.contentWindow.eval(`%s`);' +
    '    return JSON.stringify(result);' +
    '  } catch (e) {' +
    '    return "Error: " + e.message;' +
    '  }' +
    '} catch(e) { return "Outer error: " + e.message; }';
var
  EscapedScript: string;
  WrappedScript: string;
begin
  if not FCaptchaFrameFound or (FCaptchaFrameIndex < 0) then
  begin
    Log('No captcha frame available for script execution', 'Error');
    Result := '';
    Exit;
  end;

  // Escape single quotes and backslashes for proper embedding in the wrapper
  EscapedScript := StringReplace(Script, '\', '\\', [rfReplaceAll]);
  EscapedScript := StringReplace(EscapedScript, '''', '\''', [rfReplaceAll]);

  // Create the wrapped script
  WrappedScript := Format(FRAME_SCRIPT_WRAPPER, [FCaptchaFrameIndex, EscapedScript]);

  Log('Executing script in captcha frame: ' + Script, 'Debug');

  // Execute wrapped script in the main document
  Result := ExecuteScriptAndWait(WrappedScript, ExecutionID, TimeoutMs);

  // Handle potential error responses
  if Pos('Error:', Result) = 1 then
  begin
    Log('Frame script execution error: ' + Result, 'Error');
    Result := '';
  end
  else if Result = 'Frame not found' then
  begin
    Log('Captcha frame not found during script execution', 'Error');
    FCaptchaFrameFound := False;
    Result := '';
  end;
end;

function TPopularCaptchaSolver.GetCaptchaFrameContent: string;
const
  FRAME_CONTENT_SCRIPT = 'try {' +
    '  const frames = document.querySelectorAll("iframe");' +
    '  const frame = frames[%d];' +
    '  if (!frame) return "";' +
    '  try {' +
    '    const doc = frame.contentDocument || frame.contentWindow.document;' +
    '    return doc.documentElement.outerHTML;' +
    '  } catch (e) {' +
    '    return "Error: " + e.message;' +
    '  }' +
    '} catch(e) { return ""; }';
begin
  if not FCaptchaFrameFound or (FCaptchaFrameIndex < 0) then
  begin
    Log('No captcha frame available to get content', 'Error');
    Result := '';
    Exit;
  end;

  Result := ExecScriptSync(Format(FRAME_CONTENT_SCRIPT, [FCaptchaFrameIndex]));

  if Pos('Error:', Result) = 1 then
  begin
    Log('Error getting frame content: ' + Result, 'Error');
    Result := '';
  end
  else if Result <> '' then
    Log('Successfully retrieved captcha frame content', 'Info');
end;

constructor TPopularCaptchaSolver.Create(WebBrowser: TWVBrowser);
begin
  inherited Create;
  FWebBrowser := WebBrowser;
  FApiEndpoint := 'https://api.captchasonic.com/createTask';
  FIsProcessing := False;
  FStop := False;
  FMaxRetries := 3;
  FCurrentRetryCount := 0;
  FAutoSolve := True;
  FAutoOpen := True;
  FEnglishEnabled := True;
  FAlwaysSolve := False;
  FDetectionMethod := -1; // Auto detection
  FMHTMLContent := TStringList.Create;
  FCaptchaFrameFound := False;
  FCaptchaFrameIndex := -1;

  if Assigned(FWebBrowser) then
    FWebBrowser.OnExecuteScriptCompleted := ScriptCompletedHandler;
end;

destructor TPopularCaptchaSolver.Destroy;
begin
  FreeAndNil(FMHTMLContent);
  FOnLog := nil;
  inherited;
end;
function TPopularCaptchaSolver.ExtractWebsiteURLFromPage: string;
const
  SCRIPT = 'try {' +
    '  const urlParams = new URLSearchParams(location.hash || location.search);' +
    '  return urlParams.get("host") || document.location.origin;' +
    '} catch(e) { return document.location.origin; }';
begin
  Result := ExecScriptSync(SCRIPT);
  if Result = '' then
    Result := 'unknown';
  Log('Extracted website URL: ' + Result, 'Debug');
end;

function TPopularCaptchaSolver.ExtractWebsiteKeyFromPage: string;
const
  SCRIPT = 'try {' +
    '  const urlParams = new URLSearchParams(location.hash || location.search);' +
    '  return urlParams.get("sitekey") || "";' +
    '} catch(e) { return ""; }';
begin
  Result := ExecScriptSync(SCRIPT);
  Log('Extracted website key: ' + Result, 'Debug');
end;

function TPopularCaptchaSolver.ExtractMhtmlBoundary(const MHTMLContent: string): string;
var
  Match: TMatch;
begin
  Match := TRegEx.Match(MHTMLContent, 'boundary="(.*?)"', [roIgnoreCase]);
  if Match.Success then
    Result := Match.Groups[1].Value
  else
    Result := '';

  Log('MHTML boundary: ' + Result, 'Debug');
end;

function TPopularCaptchaSolver.SplitMhtmlParts(const MHTMLContent, Boundary: string): TArray<string>;
var
  Regex: TRegEx;
  Matches: TMatchCollection;
  I: Integer;
begin
  if Boundary = '' then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  Regex := TRegEx.Create('--' + Boundary + '[\r\n]+' +
                         'Content-Type: (.*?)[\r\n]+' +
                         '(.*?)' +
                         '(--' + Boundary + '|$)',
                         [roSingleLine]);

  Matches := Regex.Matches(MHTMLContent);
  SetLength(Result, Matches.Count);

  for I := 0 to Matches.Count - 1 do
    Result[I] := Matches[I].Value;

  Log(Format('Split MHTML into %d parts', [Length(Result)]), 'Debug');
end;

function TPopularCaptchaSolver.FindHtmlPartInMhtml(const MhtmlParts: TArray<string>): string;
var
  I: Integer;
  ContentBase64: string;
  DecodedContent: string;
  Match: TMatch;
begin
  Result := '';

  for I := 0 to High(MhtmlParts) do
  begin
    if (Pos('Content-Type: text/html', MhtmlParts[I]) > 0) or
       (Pos('Content-Type: application/xhtml+xml', MhtmlParts[I]) > 0) then
    begin
      // Extract the base64 content
      Match := TRegEx.Match(MhtmlParts[I], 'Content-Transfer-Encoding: base64[\r\n]+(.*?)$', [roSingleLine]);
      if Match.Success then
      begin
        ContentBase64 := Match.Groups[1].Value.Trim;
        try
          // Decode the base64 content
          DecodedContent := TNetEncoding.Base64.Decode(ContentBase64);
          Result := DecodedContent;
          Log('Successfully decoded HTML part from MHTML', 'Debug');
          Break;
        except
          on E: Exception do
            Log('Error decoding base64 content: ' + E.Message, 'Error');
        end;
      end
      else
      begin
        // If not base64 encoded, just return the part
        Result := MhtmlParts[I];
        Log('Found non-base64 HTML part in MHTML', 'Debug');
        Break;
      end;
    end;
  end;

  if Result <> '' then
    Log('Found HTML part in MHTML', 'Debug')
  else
    Log('No HTML part found in MHTML', 'Warn');
end;

// Also add a method to extract images from MHTML
function TPopularCaptchaSolver.ExtractImagesFromMHTML(const MHTMLContent: string): TArray<string>;
var
  Boundary: string;
  Parts: TArray<string>;
  ImageParts: TList<string>;
  I: Integer;
  ContentType, ContentBase64: string;
  Match: TMatch;
begin
  ImageParts := TList<string>.Create;
  try
    Boundary := ExtractMhtmlBoundary(MHTMLContent);
    if Boundary = '' then
    begin
      Log('Could not determine MHTML boundary', 'Error');
      SetLength(Result, 0);
      Exit;
    end;

    Parts := SplitMhtmlParts(MHTMLContent, Boundary);

    for I := 0 to High(Parts) do
    begin
      // Check if this part is an image
      Match := TRegEx.Match(Parts[I], 'Content-Type: (image/.*?)[\r\n]+', [roSingleLine]);
      if Match.Success then
      begin
        ContentType := Match.Groups[1].Value.Trim;

        // Extract base64 content
        Match := TRegEx.Match(Parts[I], 'Content-Transfer-Encoding: base64[\r\n]+(.*?)$', [roSingleLine]);
        if Match.Success then
        begin
          ContentBase64 := Match.Groups[1].Value.Trim;
          ImageParts.Add(ContentBase64);
          Log('Found ' + ContentType + ' in MHTML', 'Debug');
        end;
      end;
    end;

    // Convert list to array
    SetLength(Result, ImageParts.Count);
    for I := 0 to ImageParts.Count - 1 do
      Result[I] := ImageParts[I];

    Log(Format('Extracted %d images from MHTML', [Length(Result)]), 'Debug');
  finally
    ImageParts.Free;
  end;
end;

// Enhanced ProcessCaptchaFromMHTML to better parse and extract data
procedure TPopularCaptchaSolver.ProcessCaptchaFromMHTML(const MHTMLContent: string);
var
  CaptchaData: TJSONObject;
  ApiRequest: TJSONObject;
  ApiResponse: TJSONObject;
  ImagesArray: TJSONArray;
  Boundary: string;
  MHTMLParts: TArray<string>;
  HtmlContent: string;
  ImageContents: TArray<string>;
begin
  try
    // Parse the MHTML structure
    Boundary := ExtractMhtmlBoundary(MHTMLContent);
    if Boundary <> '' then
    begin
      MHTMLParts := SplitMhtmlParts(MHTMLContent, Boundary);
      HtmlContent := FindHtmlPartInMhtml(MHTMLParts);
      ImageContents := ExtractImagesFromMHTML(MHTMLContent);

      Log(Format('MHTML analysis: %d parts, HTML content length: %d, %d images',
        [Length(MHTMLParts), Length(HtmlContent), Length(ImageContents)]), 'Debug');
    end;

    // Extract captcha data from MHTML
    CaptchaData := ExtractCaptchaDataFromMHTML(MHTMLContent);

    // Verify there is data to process
    ImagesArray := CaptchaData.GetValue<TJSONArray>('images');
    if (ImagesArray = nil) or (ImagesArray.Count = 0) then
    begin
      // Try to enhance image data with extracted images if available
      if Length(ImageContents) > 0 then
      begin
        if ImagesArray = nil then
          ImagesArray := TJSONArray.Create
        else
          ImagesArray.Clear;

        for var ImageContent in ImageContents do
          ImagesArray.Add(ImageContent);

        CaptchaData.RemovePair('images');
        CaptchaData.AddPair('images', ImagesArray);

        Log('Enhanced captcha data with ' + IntToStr(ImagesArray.Count) + ' images from MHTML', 'Info');
      end
      else
      begin
        Log('No images found in MHTML - cannot solve', 'Error');
        SolveWithTraditionalMethod;
        Exit;
      end;
    end;

    // Create and send API request
    ApiRequest := CreateApiRequestFromMHTML(CaptchaData);
    ApiResponse := SendApiRequest(ApiRequest);

    // Handle response
    if ApiResponse <> nil then
    begin
      if ApiResponse.GetValue<Boolean>('success', False) then
        ApplySolution(ApiResponse)
      else
      begin
        Log('API Error: ' + ApiResponse.GetValue<string>('msg', 'Unknown error'), 'Error');
        SolveWithTraditionalMethod;
      end;
    end;
  finally
    FreeAndNil(CaptchaData);
    FreeAndNil(ApiRequest);
    FreeAndNil(ApiResponse);
  end;
end;

end.


