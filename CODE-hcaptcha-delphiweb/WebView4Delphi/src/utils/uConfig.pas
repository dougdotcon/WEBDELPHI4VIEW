unit uConfig;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles,
  System.IOUtils, uCaptchaTypes;

type
  TConfig = class
  private
    FIniFile: TIniFile;
    FCaptchaConfig: TCaptchaConfig;
    FLogFile: string;
    FLogLevel: Integer;
    
    procedure LoadCaptchaConfig;
    procedure SaveCaptchaConfig;
    procedure LoadLogConfig;
    procedure SaveLogConfig;
    
    function GetConfigFileName: string;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Load;
    procedure Save;
    
    property CaptchaConfig: TCaptchaConfig read FCaptchaConfig write FCaptchaConfig;
    property LogFile: string read FLogFile write FLogFile;
    property LogLevel: Integer read FLogLevel write FLogLevel;
  end;

implementation

const
  DEFAULT_CONFIG_FILE = 'config.ini';
  DEFAULT_LOG_FILE = 'logs\captcha_solver.log';
  DEFAULT_LOG_LEVEL = 1; // Info
  
  SECTION_CAPTCHA = 'CaptchaSonic';
  SECTION_LOGGING = 'Logging';

constructor TConfig.Create;
begin
  inherited;
  FIniFile := TIniFile.Create(GetConfigFileName);
  
  // Set defaults
  FCaptchaConfig.BaseURL := 'https://my.captchasonic.com/api/v1';
  FCaptchaConfig.TimeoutMS := 30000;
  FCaptchaConfig.RetryCount := 3;
  FCaptchaConfig.DelayBetweenActionsMS := 200;
  FCaptchaConfig.RandomDelayRangeMS := 300;
  
  FLogFile := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), DEFAULT_LOG_FILE);
  FLogLevel := DEFAULT_LOG_LEVEL;
end;

destructor TConfig.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

function TConfig.GetConfigFileName: string;
begin
  Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), DEFAULT_CONFIG_FILE);
end;

procedure TConfig.LoadCaptchaConfig;
begin
  with FIniFile do
  begin
    FCaptchaConfig.APIKey := ReadString(SECTION_CAPTCHA, 'APIKey', '');
    FCaptchaConfig.BaseURL := ReadString(SECTION_CAPTCHA, 'BaseURL', FCaptchaConfig.BaseURL);
    FCaptchaConfig.TimeoutMS := ReadInteger(SECTION_CAPTCHA, 'TimeoutMS', FCaptchaConfig.TimeoutMS);
    FCaptchaConfig.RetryCount := ReadInteger(SECTION_CAPTCHA, 'RetryCount', FCaptchaConfig.RetryCount);
    FCaptchaConfig.DelayBetweenActionsMS := ReadInteger(SECTION_CAPTCHA, 'DelayBetweenActionsMS', FCaptchaConfig.DelayBetweenActionsMS);
    FCaptchaConfig.RandomDelayRangeMS := ReadInteger(SECTION_CAPTCHA, 'RandomDelayRangeMS', FCaptchaConfig.RandomDelayRangeMS);
  end;
end;

procedure TConfig.SaveCaptchaConfig;
begin
  with FIniFile do
  begin
    WriteString(SECTION_CAPTCHA, 'APIKey', FCaptchaConfig.APIKey);
    WriteString(SECTION_CAPTCHA, 'BaseURL', FCaptchaConfig.BaseURL);
    WriteInteger(SECTION_CAPTCHA, 'TimeoutMS', FCaptchaConfig.TimeoutMS);
    WriteInteger(SECTION_CAPTCHA, 'RetryCount', FCaptchaConfig.RetryCount);
    WriteInteger(SECTION_CAPTCHA, 'DelayBetweenActionsMS', FCaptchaConfig.DelayBetweenActionsMS);
    WriteInteger(SECTION_CAPTCHA, 'RandomDelayRangeMS', FCaptchaConfig.RandomDelayRangeMS);
  end;
end;

procedure TConfig.LoadLogConfig;
begin
  with FIniFile do
  begin
    FLogFile := ReadString(SECTION_LOGGING, 'LogFile', FLogFile);
    FLogLevel := ReadInteger(SECTION_LOGGING, 'LogLevel', FLogLevel);
  end;
end;

procedure TConfig.SaveLogConfig;
begin
  with FIniFile do
  begin
    WriteString(SECTION_LOGGING, 'LogFile', FLogFile);
    WriteInteger(SECTION_LOGGING, 'LogLevel', FLogLevel);
  end;
end;

procedure TConfig.Load;
begin
  LoadCaptchaConfig;
  LoadLogConfig;
end;

procedure TConfig.Save;
begin
  SaveCaptchaConfig;
  SaveLogConfig;
  FIniFile.UpdateFile;
end;

end. 