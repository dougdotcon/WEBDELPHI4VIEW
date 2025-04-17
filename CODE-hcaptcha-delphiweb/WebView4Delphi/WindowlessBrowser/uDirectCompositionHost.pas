unit uDirectCompositionHost;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Graphics,
  uWVBrowser, uWVTypes, uWVConstants;

type
  TWVDirectCompositionHost = class(TCustomControl)
  private
    FBrowser: TObject;
    FDCompDevice: TObject;
    FWebViewVisual: IInterface;
    
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    function BuildDCompTreeUsingVisual: Boolean;
    procedure DestroyDCompVisualTree;
    procedure UpdateSize;
    function ScreenToClient(const aPoint: TPoint): TPoint;
    procedure HandleNeeded;
    
    property DCompDevice: TObject read FDCompDevice;
    property WebViewVisual: IInterface read FWebViewVisual;
    property Browser: TObject read FBrowser write FBrowser;
  end;

implementation

constructor TWVDirectCompositionHost.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 800;
  Height := 600;
  ControlStyle := ControlStyle + [csOpaque];
  
  FBrowser := nil;
  FDCompDevice := nil;
  FWebViewVisual := nil;
end;

destructor TWVDirectCompositionHost.Destroy;
begin
  DestroyDCompVisualTree;
  inherited;
end;

procedure TWVDirectCompositionHost.CreateWnd;
begin
  inherited;
  // Initialize DirectComposition if needed
end;

procedure TWVDirectCompositionHost.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateSize;
end;

procedure TWVDirectCompositionHost.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TWVDirectCompositionHost.Paint;
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

function TWVDirectCompositionHost.BuildDCompTreeUsingVisual: Boolean;
begin
  // Implementação simplificada
  Result := True;
end;

procedure TWVDirectCompositionHost.DestroyDCompVisualTree;
begin
  // Limpar recursos
  FWebViewVisual := nil;
  FDCompDevice := nil;
end;

procedure TWVDirectCompositionHost.UpdateSize;
begin
  // Atualizar tamanho do conteúdo
  if Assigned(FBrowser) and (FBrowser is TWVBrowser) then
  begin
    TWVBrowser(FBrowser).NotifyParentWindowPositionChanged;
  end;
end;

function TWVDirectCompositionHost.ScreenToClient(const aPoint: TPoint): TPoint;
begin
  Result := inherited ScreenToClient(aPoint);
end;

procedure TWVDirectCompositionHost.HandleNeeded;
begin
  // Isso garante que o controle tem um handle válido
  if not HandleAllocated then
    CreateHandle;
end;

end.
