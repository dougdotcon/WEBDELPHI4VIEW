object WebpageSnapshotFrm: TWebpageSnapshotFrm
  Left = 0
  Top = 0
  Caption = 'Web page snapshot'
  ClientHeight = 736
  ClientWidth = 1028
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 25
    Width = 1028
    Height = 540
    Align = alClient
    AutoSize = True
    Center = True
    Proportional = True
    ExplicitLeft = 104
    ExplicitTop = 112
    ExplicitWidth = 777
    ExplicitHeight = 329
  end
  object Splitter1: TSplitter
    Left = 0
    Top = 565
    Width = 1028
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 25
    ExplicitWidth = 603
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 717
    Width = 1028
    Height = 19
    Panels = <
      item
        Width = 1000
      end>
    ExplicitTop = 716
    ExplicitWidth = 1024
  end
  object NavigationPnl: TPanel
    Left = 0
    Top = 0
    Width = 1028
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 2
    Padding.Top = 2
    Padding.Right = 2
    Padding.Bottom = 2
    TabOrder = 1
    ExplicitWidth = 1024
    object GoBtn: TButton
      Left = 951
      Top = 2
      Width = 75
      Height = 21
      Align = alRight
      Caption = 'Go'
      TabOrder = 0
      OnClick = GoBtnClick
      ExplicitLeft = 947
    end
    object AddressEdt: TEdit
      Left = 2
      Top = 2
      Width = 949
      Height = 21
      Align = alClient
      TabOrder = 1
      Text = 'https://www.bing.com'
      ExplicitWidth = 945
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 568
    Width = 1028
    Height = 149
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    ExplicitTop = 567
    ExplicitWidth = 1024
  end
  object WVBrowser1: TWVBrowser
    TargetCompatibleBrowserVersion = '121.0.2277.86'
    AllowSingleSignOnUsingOSPrimaryAccount = False
    OnCapturePreviewCompleted = WVBrowser1CapturePreviewCompleted
    OnNavigationCompleted = WVBrowser1NavigationCompleted
    OnRetrieveHTMLCompleted = WVBrowser1RetrieveHTMLCompleted
    Left = 400
    Top = 360
  end
end
