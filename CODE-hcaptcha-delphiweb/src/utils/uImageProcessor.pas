unit uImageProcessor;

interface

uses
  System.Classes, System.SysUtils, System.NetEncoding,
  Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg,
  uLogger;

type
  TImageProcessor = class
  private
    FLogger: TLogger;
    
    function StreamToBase64(Stream: TStream): string;
    function Base64ToStream(const Base64: string): TMemoryStream;
    function GetImageFormat(Stream: TStream): string;
  public
    constructor Create(Logger: TLogger);
    
    function CaptureElementToBase64(const ElementHandle: string): string;
    function CropImage(const Base64Image: string; const X, Y, Width, Height: Integer): string;
    function ResizeImage(const Base64Image: string; const NewWidth, NewHeight: Integer): string;
    function ConvertToFormat(const Base64Image, TargetFormat: string): string;
    function SaveBase64ToFile(const Base64: string; const FileName: string): Boolean;
    function LoadImageFromFile(const FileName: string): string;
  end;

implementation

uses
  System.Types;

constructor TImageProcessor.Create(Logger: TLogger);
begin
  inherited Create;
  FLogger := Logger;
end;

function TImageProcessor.StreamToBase64(Stream: TStream): string;
var
  Bytes: TBytes;
  Encoding: TBase64Encoding;
begin
  SetLength(Bytes, Stream.Size);
  Stream.Position := 0;
  Stream.ReadBuffer(Bytes[0], Stream.Size);
  
  Encoding := TBase64Encoding.Create;
  try
    Result := Encoding.EncodeBytesToString(Bytes);
  finally
    Encoding.Free;
  end;
end;

function TImageProcessor.Base64ToStream(const Base64: string): TMemoryStream;
var
  Bytes: TBytes;
  Encoding: TBase64Encoding;
begin
  Result := TMemoryStream.Create;
  Encoding := TBase64Encoding.Create;
  try
    Bytes := Encoding.DecodeStringToBytes(Base64);
    Result.WriteBuffer(Bytes[0], Length(Bytes));
    Result.Position := 0;
  finally
    Encoding.Free;
  end;
end;

function TImageProcessor.GetImageFormat(Stream: TStream): string;
var
  Signature: array[0..3] of Byte;
  OrigPos: Int64;
begin
  Result := 'unknown';
  OrigPos := Stream.Position;
  try
    Stream.Position := 0;
    Stream.Read(Signature, 4);
    
    // Check PNG signature
    if (Signature[0] = $89) and (Signature[1] = $50) and
       (Signature[2] = $4E) and (Signature[3] = $47) then
      Result := 'png'
    // Check JPEG signature
    else if (Signature[0] = $FF) and (Signature[1] = $D8) then
      Result := 'jpeg';
  finally
    Stream.Position := OrigPos;
  end;
end;

function TImageProcessor.CaptureElementToBase64(const ElementHandle: string): string;
var
  Stream: TMemoryStream;
begin
  Result := '';
  Stream := TMemoryStream.Create;
  try
    // TODO: Implement actual element capture using WebView
    Result := StreamToBase64(Stream);
  finally
    Stream.Free;
  end;
end;

function TImageProcessor.CropImage(const Base64Image: string; const X, Y, Width, Height: Integer): string;
var
  SourceStream: TMemoryStream;
  SourceBitmap, CroppedBitmap: TBitmap;
  PngImage: TPngImage;
  ResultStream: TMemoryStream;
  R: TRect;
begin
  SourceStream := Base64ToStream(Base64Image);
  SourceBitmap := TBitmap.Create;
  CroppedBitmap := TBitmap.Create;
  try
    // Load source image
    case GetImageFormat(SourceStream) of
      'png':
        begin
          PngImage := TPngImage.Create;
          try
            PngImage.LoadFromStream(SourceStream);
            SourceBitmap.Assign(PngImage);
          finally
            PngImage.Free;
          end;
        end;
      'jpeg':
        begin
          SourceBitmap.LoadFromStream(SourceStream);
        end;
      else
        raise Exception.Create('Unsupported image format');
    end;
    
    // Setup cropped bitmap
    CroppedBitmap.SetSize(Width, Height);
    CroppedBitmap.PixelFormat := SourceBitmap.PixelFormat;
    
    // Copy region
    R := TRect.Create(X, Y, X + Width, Y + Height);
    CroppedBitmap.Canvas.CopyRect(
      TRect.Create(0, 0, Width, Height),
      SourceBitmap.Canvas,
      R
    );
    
    // Convert to PNG and return as Base64
    ResultStream := TMemoryStream.Create;
    PngImage := TPngImage.Create;
    try
      PngImage.Assign(CroppedBitmap);
      PngImage.SaveToStream(ResultStream);
      Result := StreamToBase64(ResultStream);
    finally
      PngImage.Free;
      ResultStream.Free;
    end;
  finally
    SourceStream.Free;
    SourceBitmap.Free;
    CroppedBitmap.Free;
  end;
end;

function TImageProcessor.ResizeImage(const Base64Image: string; const NewWidth, NewHeight: Integer): string;
var
  SourceStream: TMemoryStream;
  SourceBitmap, ResizedBitmap: TBitmap;
  PngImage: TPngImage;
  ResultStream: TMemoryStream;
begin
  SourceStream := Base64ToStream(Base64Image);
  SourceBitmap := TBitmap.Create;
  ResizedBitmap := TBitmap.Create;
  try
    // Load source image
    case GetImageFormat(SourceStream) of
      'png':
        begin
          PngImage := TPngImage.Create;
          try
            PngImage.LoadFromStream(SourceStream);
            SourceBitmap.Assign(PngImage);
          finally
            PngImage.Free;
          end;
        end;
      'jpeg':
        begin
          SourceBitmap.LoadFromStream(SourceStream);
        end;
      else
        raise Exception.Create('Unsupported image format');
    end;
    
    // Setup resized bitmap
    ResizedBitmap.SetSize(NewWidth, NewHeight);
    ResizedBitmap.PixelFormat := SourceBitmap.PixelFormat;
    
    // Perform resize
    ResizedBitmap.Canvas.StretchDraw(
      TRect.Create(0, 0, NewWidth, NewHeight),
      SourceBitmap
    );
    
    // Convert to PNG and return as Base64
    ResultStream := TMemoryStream.Create;
    PngImage := TPngImage.Create;
    try
      PngImage.Assign(ResizedBitmap);
      PngImage.SaveToStream(ResultStream);
      Result := StreamToBase64(ResultStream);
    finally
      PngImage.Free;
      ResultStream.Free;
    end;
  finally
    SourceStream.Free;
    SourceBitmap.Free;
    ResizedBitmap.Free;
  end;
end;

function TImageProcessor.ConvertToFormat(const Base64Image, TargetFormat: string): string;
var
  SourceStream: TMemoryStream;
  SourceBitmap: TBitmap;
  ResultStream: TMemoryStream;
  PngImage: TPngImage;
  JpegImage: TJPEGImage;
begin
  SourceStream := Base64ToStream(Base64Image);
  SourceBitmap := TBitmap.Create;
  ResultStream := TMemoryStream.Create;
  try
    // Load source image
    case GetImageFormat(SourceStream) of
      'png':
        begin
          PngImage := TPngImage.Create;
          try
            PngImage.LoadFromStream(SourceStream);
            SourceBitmap.Assign(PngImage);
          finally
            PngImage.Free;
          end;
        end;
      'jpeg':
        begin
          SourceBitmap.LoadFromStream(SourceStream);
        end;
      else
        raise Exception.Create('Unsupported source image format');
    end;
    
    // Convert to target format
    case LowerCase(TargetFormat) of
      'png':
        begin
          PngImage := TPngImage.Create;
          try
            PngImage.Assign(SourceBitmap);
            PngImage.SaveToStream(ResultStream);
          finally
            PngImage.Free;
          end;
        end;
      'jpeg', 'jpg':
        begin
          JpegImage := TJPEGImage.Create;
          try
            JpegImage.Assign(SourceBitmap);
            JpegImage.SaveToStream(ResultStream);
          finally
            JpegImage.Free;
          end;
        end;
      else
        raise Exception.Create('Unsupported target format: ' + TargetFormat);
    end;
    
    Result := StreamToBase64(ResultStream);
  finally
    SourceStream.Free;
    SourceBitmap.Free;
    ResultStream.Free;
  end;
end;

function TImageProcessor.SaveBase64ToFile(const Base64: string; const FileName: string): Boolean;
var
  Stream: TMemoryStream;
begin
  Result := False;
  if Base64 = '' then
    Exit;
    
  Stream := Base64ToStream(Base64);
  try
    try
      Stream.SaveToFile(FileName);
      Result := True;
    except
      on E: Exception do
        Result := False;
    end;
  finally
    Stream.Free;
  end;
end;

function TImageProcessor.LoadImageFromFile(const FileName: string): string;
var
  Stream: TMemoryStream;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;
    
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FileName);
    Result := StreamToBase64(Stream);
  finally
    Stream.Free;
  end;
end;

end. 