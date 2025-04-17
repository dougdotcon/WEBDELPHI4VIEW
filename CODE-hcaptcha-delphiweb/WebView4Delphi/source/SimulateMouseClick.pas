unit SimulateMouseClick;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes;

type
  TMouseClickType = (mcLeft, mcRight, mcMiddle);

procedure SimulateMouseClick(const X, Y: Integer; const ClickType: TMouseClickType);

implementation

procedure SimulateMouseClick(const X, Y: Integer; const ClickType: TMouseClickType);
var
  Flags: DWORD;
begin
  case ClickType of
    mcLeft:   Flags := MOUSEEVENTF_LEFTDOWN or MOUSEEVENTF_LEFTUP;
    mcRight:  Flags := MOUSEEVENTF_RIGHTDOWN or MOUSEEVENTF_RIGHTUP;
    mcMiddle: Flags := MOUSEEVENTF_MIDDLEDOWN or MOUSEEVENTF_MIDDLEUP;
  else
    Flags := MOUSEEVENTF_LEFTDOWN or MOUSEEVENTF_LEFTUP;
  end;
  
  mouse_event(Flags, X, Y, 0, 0);
end;

end. 