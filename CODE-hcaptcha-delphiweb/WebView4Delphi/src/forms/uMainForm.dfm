object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'CaptchaSolver'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 41
    Align = alTop
    TabOrder = 0
    object lblURL: TLabel
      Left = 8
      Top = 14
      Width = 22
      Height = 13
      Caption = 'URL:'
    end
    object edtURL: TEdit
      Left = 36
      Top = 11
      Width = 673
      Height = 21
      TabOrder = 0
      Text = 'https://accounts.hcaptcha.com/demo'
    end
    object btnStart: TButton
      Left = 715
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Iniciar'
      TabOrder = 1
      OnClick = btnStartClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 511
    Width = 800
    Height = 70
    Align = alBottom
    TabOrder = 1
    object mmLog: TMemo
      Left = 1
      Top = 1
      Width = 798
      Height = 68
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 41
    Width = 800
    Height = 470
    Align = alClient
    TabOrder = 2
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 581
    Width = 800
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object WVBrowser1: TWVBrowser
    DefaultURL = 'about:blank'
    TargetCompatibleBrowserVersion = '95.0.1020.44'
    AllowSingleSignOnUsingOSPrimaryAccount = False
    OnAfterCreated = WVBrowser1AfterCreated
    OnDocumentTitleChanged = WVBrowser1DocumentTitleChanged
    OnWebMessageReceived = WVBrowser1WebMessageReceived
    Left = 40
    Top = 88
  end
end 