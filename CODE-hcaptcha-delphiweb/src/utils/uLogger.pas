unit uLogger;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs;

type
  TLogLevel = (llError = 0, llInfo = 1, llDebug = 2);

  TLogger = class
  private
    FLogFile: string;
    FLogLevel: TLogLevel;
    FLock: TCriticalSection;
    FLogStream: TStreamWriter;
    
    class var FInstance: TLogger;
    
    constructor Create(const ALogFile: string; ALogLevel: Integer);
  public
    destructor Destroy; override;
    
    procedure Log(const AMessage: string; ALevel: TLogLevel = llInfo);
    procedure LogError(const AMessage: string);
    procedure LogInfo(const AMessage: string);
    procedure LogDebug(const AMessage: string);
    
    class function GetInstance: TLogger;
    class procedure Initialize(const ALogFile: string; ALogLevel: Integer);
    class procedure Finalize;
  end;

implementation

constructor TLogger.Create(const ALogFile: string; ALogLevel: Integer);
begin
  inherited Create;
  FLogFile := ALogFile;
  FLogLevel := TLogLevel(ALogLevel);
  FLock := TCriticalSection.Create;
  
  ForceDirectories(ExtractFilePath(FLogFile));
  FLogStream := TStreamWriter.Create(FLogFile, True, TEncoding.UTF8);
end;

destructor TLogger.Destroy;
begin
  FLogStream.Free;
  FLock.Free;
  inherited;
end;

procedure TLogger.Log(const AMessage: string; ALevel: TLogLevel);
begin
  if Ord(ALevel) > Ord(FLogLevel) then
    Exit;
    
  FLock.Enter;
  try
    FLogStream.WriteLine(Format('[%s] [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
       GetEnumName(TypeInfo(TLogLevel), Ord(ALevel)),
       AMessage]));
    FLogStream.Flush;
  finally
    FLock.Leave;
  end;
end;

procedure TLogger.LogError(const AMessage: string);
begin
  Log(AMessage, llError);
end;

procedure TLogger.LogInfo(const AMessage: string);
begin
  Log(AMessage, llInfo);
end;

procedure TLogger.LogDebug(const AMessage: string);
begin
  Log(AMessage, llDebug);
end;

class function TLogger.GetInstance: TLogger;
begin
  if not Assigned(FInstance) then
    raise Exception.Create('Logger not initialized. Call Initialize first.');
  Result := FInstance;
end;

class procedure TLogger.Initialize(const ALogFile: string; ALogLevel: Integer);
begin
  if Assigned(FInstance) then
    FInstance.Free;
  FInstance := TLogger.Create(ALogFile, ALogLevel);
end;

class procedure TLogger.Finalize;
begin
  if Assigned(FInstance) then
  begin
    FInstance.Free;
    FInstance := nil;
  end;
end;

end. 