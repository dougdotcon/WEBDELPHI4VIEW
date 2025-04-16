unit uCaptchaTypes;

interface

uses
  System.Classes, System.SysUtils, System.JSON;

type
  TCaptchaStatus = (csNone, csDetected, csSolving, csSolved, csError);
  
  TCaptchaConfig = record
    APIKey: string;
    BaseURL: string;
    TimeoutMS: Integer;
    RetryCount: Integer;
    DelayBetweenActionsMS: Integer;
    RandomDelayRangeMS: Integer;
  end;

  TCaptchaResponse = record
    Success: Boolean;
    ErrorMessage: string;
    Solution: string;
    TaskId: string;
    Balance: Double;
  end;

  TCaptchaTask = record
    Type: string;
    WebsiteURL: string;
    WebsiteKey: string;
    Data: string;
    Proxy: string;
    UserAgent: string;
  end;

  TCaptchaSonicResponse = class
  private
    FSuccess: Boolean;
    FMessage: string;
    FData: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Success: Boolean read FSuccess write FSuccess;
    property Message: string read FMessage write FMessage;
    property Data: TJSONObject read FData write FData;
  end;

  TCaptchaImageInfo = record
    Base64Data: string;
    Index: Integer;
    ElementId: string;
    Coordinates: TPoint;
  end;

  TCaptchaChallenge = class
  private
    FReferenceImage: TCaptchaImageInfo;
    FGridImages: TArray<TCaptchaImageInfo>;
    FPromptText: string;
  public
    constructor Create;
    destructor Destroy; override;
    
    property ReferenceImage: TCaptchaImageInfo read FReferenceImage write FReferenceImage;
    property GridImages: TArray<TCaptchaImageInfo> read FGridImages write FGridImages;
    property PromptText: string read FPromptText write FPromptText;
  end;

implementation

{ TCaptchaSonicResponse }

constructor TCaptchaSonicResponse.Create;
begin
  inherited;
  FData := TJSONObject.Create;
end;

destructor TCaptchaSonicResponse.Destroy;
begin
  FData.Free;
  inherited;
end;

{ TCaptchaChallenge }

constructor TCaptchaChallenge.Create;
begin
  inherited;
  SetLength(FGridImages, 9); // hCaptcha typically uses a 3x3 grid
end;

destructor TCaptchaChallenge.Destroy;
begin
  SetLength(FGridImages, 0);
  inherited;
end;

end. 