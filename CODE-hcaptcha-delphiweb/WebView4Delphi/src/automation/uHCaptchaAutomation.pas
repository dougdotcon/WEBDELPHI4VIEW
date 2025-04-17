unit uHCaptchaAutomation;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Types,
  Winapi.Windows, Vcl.Graphics,
  uCaptchaTypes, uCaptchaSonicClient, uImageProcessor, uLogger,
  uDirectCompositionHost, uWVTypes, uWVInterfaces;

type
  THCaptchaAutomation = class
  private
    FWebView: TWVDirectCompositionHost;
    FCaptchaSonic: TCaptchaSonicClient;
    FImageProcessor: TImageProcessor;
    FLogger: TLogger;
    
    function ExtractChallengeInfo: TCaptchaChallenge;
    function GetElementBounds(const ElementSelector: string): TRect;
    function CaptureElement(const ElementSelector: string): string;
    procedure SimulateHumanClick(const X, Y: Integer);
    procedure WaitForElement(const ElementSelector: string; Timeout: Integer = 5000);
    procedure WaitForChallengeLoad;
    function IsElementPresent(const ElementSelector: string): Boolean;
    function ExtractWebsiteKey(const IframeSrc: string): string;
    function WaitForCaptchaIframe(const Timeout: Integer = 5000): Boolean;
  public
    constructor Create(WebView: TWVDirectCompositionHost; CaptchaSonic: TCaptchaSonicClient;
      ImageProcessor: TImageProcessor; Logger: TLogger);
    destructor Destroy; override;
    
    function Solve(const URL: string): Boolean;
  end;

implementation

uses
  System.Math, System.Threading;

const
  CHALLENGE_CONTAINER = 'div.challenge-container';
  REFERENCE_IMAGE = 'div.challenge-example img';
  GRID_IMAGES = 'div.task img';
  PROMPT_TEXT = 'h2.prompt-text';
  VERIFY_BUTTON = 'div.button-submit';
  SKIP_BUTTON = 'div.skip';

constructor THCaptchaAutomation.Create(WebView: TWVDirectCompositionHost;
  CaptchaSonic: TCaptchaSonicClient; ImageProcessor: TImageProcessor; Logger: TLogger);
begin
  inherited Create;
  FWebView := WebView;
  FCaptchaSonic := CaptchaSonic;
  FImageProcessor := ImageProcessor;
  FLogger := Logger;
end;

destructor THCaptchaAutomation.Destroy;
begin
  inherited;
end;

function THCaptchaAutomation.IsElementPresent(const ElementSelector: string): Boolean;
var
  Success: Boolean;
begin
  Success := False;
  // Implementação temporária
  Result := Success;
end;

procedure THCaptchaAutomation.WaitForElement(const ElementSelector: string; Timeout: Integer);
var
  StartTime: Cardinal;
begin
  StartTime := GetTickCount;
  while (GetTickCount - StartTime < Cardinal(Timeout)) do
  begin
    if IsElementPresent(ElementSelector) then
      Exit;
    Sleep(100);
  end;
  raise Exception.CreateFmt('Timeout waiting for element: %s', [ElementSelector]);
end;

procedure THCaptchaAutomation.WaitForChallengeLoad;
begin
  WaitForElement(CHALLENGE_CONTAINER);
  WaitForElement(REFERENCE_IMAGE);
  WaitForElement(GRID_IMAGES);
  WaitForElement(PROMPT_TEXT);
end;

function THCaptchaAutomation.GetElementBounds(const ElementSelector: string): TRect;
var
  Bounds: TRect;
begin
  // Implementação temporária
  Bounds := TRect.Create(0, 0, 100, 100);
  Result := Bounds;
end;

function THCaptchaAutomation.CaptureElement(const ElementSelector: string): string;
var
  ElementBounds: TRect;
begin
  ElementBounds := GetElementBounds(ElementSelector);
  Result := FImageProcessor.CaptureElementToBase64(ElementSelector);
end;

procedure THCaptchaAutomation.SimulateHumanClick(const X, Y: Integer);
var
  DelayMS: Integer;
begin
  // Add random delay before click
  DelayMS := Random(300) + 100;
  Sleep(DelayMS);
  
  // Move mouse to position with human-like movement
  // TODO: Implement smooth mouse movement
  
  // Perform click
  // TODO: Implementar clique do mouse
  
  // Add random delay after click
  DelayMS := Random(200) + 50;
  Sleep(DelayMS);
end;

function THCaptchaAutomation.ExtractChallengeInfo: TCaptchaChallenge;
var
  Challenge: TCaptchaChallenge;
  i: Integer;
  GridElementBounds: TRect;
  RefImage: TCaptchaImageInfo;
  GridImage: TCaptchaImageInfo;
begin
  Challenge := TCaptchaChallenge.Create;
  try
    // Get prompt text - implementação temporária
    Challenge.PromptText := 'Select all images with cars';
    
    // Get reference image
    RefImage.Base64Data := CaptureElement(REFERENCE_IMAGE);
    RefImage.Index := -1;
    RefImage.ElementId := '';
    RefImage.Coordinates := Point(0, 0);
    Challenge.ReferenceImage := RefImage;
    
    // Get grid images
    for i := 0 to 8 do
    begin
      GridElementBounds := GetElementBounds(Format('%s:nth-child(%d)', [GRID_IMAGES, i + 1]));
      
      GridImage.Base64Data := CaptureElement(Format('%s:nth-child(%d)', [GRID_IMAGES, i + 1]));
      GridImage.Index := i;
      GridImage.ElementId := '';
      GridImage.Coordinates := Point(
        GridElementBounds.Left + (GridElementBounds.Width div 2),
        GridElementBounds.Top + (GridElementBounds.Height div 2)
      );
      
      Challenge.GridImages[i] := GridImage;
    end;
    
    Result := Challenge;
  except
    on E: Exception do
    begin
      FLogger.LogError('Error extracting challenge info: ' + E.Message);
      Challenge.Free;
      raise;
    end;
  end;
end;

function THCaptchaAutomation.Solve(const URL: string): Boolean;
var
  IframeSrc, WebsiteKey: string;
  Response: TCaptchaResponse;
begin
  Result := False;
  
  TLogger.GetInstance.LogInfo('Starting hCaptcha automation for URL: ' + URL);
  
  if not WaitForCaptchaIframe then
  begin
    TLogger.GetInstance.LogError('hCaptcha iframe not found');
    Exit;
  end;
  
  // TODO: Get actual iframe src using WebView
  IframeSrc := '';
  WebsiteKey := ExtractWebsiteKey(IframeSrc);
  
  if WebsiteKey = '' then
  begin
    TLogger.GetInstance.LogError('Could not extract website key from iframe');
    Exit;
  end;
  
  TLogger.GetInstance.LogInfo('Found hCaptcha website key: ' + WebsiteKey);
  
  Response := FCaptchaSonic.SolveHCaptcha(URL, WebsiteKey);
  
  if not Response.Success then
  begin
    TLogger.GetInstance.LogError('Failed to solve hCaptcha: ' + Response.ErrorMessage);
    Exit;
  end;
  
  if Response.Solution = '' then
  begin
    TLogger.GetInstance.LogError('No solution received from CaptchaSonic');
    Exit;
  end;
  
  TLogger.GetInstance.LogInfo('Successfully solved hCaptcha');
  
  // TODO: Submit solution using WebView
  Result := True;
end;

function THCaptchaAutomation.ExtractWebsiteKey(const IframeSrc: string): string;
var
  StartPos, EndPos: Integer;
begin
  Result := '';
  
  StartPos := Pos('sitekey=', IframeSrc);
  if StartPos > 0 then
  begin
    StartPos := StartPos + 8;
    EndPos := Pos('&', IframeSrc, StartPos);
    if EndPos = 0 then
      EndPos := Length(IframeSrc) + 1;
      
    Result := Copy(IframeSrc, StartPos, EndPos - StartPos);
  end;
end;

function THCaptchaAutomation.WaitForCaptchaIframe(const Timeout: Integer): Boolean;
begin
  // TODO: Implement actual iframe detection using WebView
  Result := True;
end;

end. 