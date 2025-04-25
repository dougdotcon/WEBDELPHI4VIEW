object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'WindowlessBrowser - Initializing...'
  ClientHeight = 879
  ClientWidth = 995
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object AddressPnl: TPanel
    Left = 0
    Top = 0
    Width = 995
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    Enabled = False
    TabOrder = 0
    DesignSize = (
      995
      24)
    object AddressCb: TComboBox
      Left = 2
      Top = 0
      Width = 943
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      ItemIndex = 0
      TabOrder = 0
      Text = 'https://accounts.hcaptcha.com/demo'
      Items.Strings = (
        'https://accounts.hcaptcha.com/demo'
        'https://smartapi.tech/token/mousemov.php'
        'https://smartapi.tech/token/click.php')
    end
    object GoBtn: TButton
      Left = 946
      Top = 0
      Width = 49
      Height = 24
      Align = alRight
      Caption = 'Go'
      TabOrder = 1
      OnClick = GoBtnClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 678
    Width = 995
    Height = 201
    Align = alBottom
    TabOrder = 1
    object MMHTML: TMemo
      Left = 13
      Top = 35
      Width = 292
      Height = 156
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object BtnMHTML: TButton
      Left = 13
      Top = 8
      Width = 90
      Height = 25
      Caption = 'Obter MHTML'
      TabOrder = 1
      OnClick = BtnMHTMLClick
    end
    object BtnTesteClick: TButton
      Left = 328
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Clicar'
      TabOrder = 2
      OnClick = BtnTesteClickClick
    end
    object BtnTesteMover: TButton
      Left = 411
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Mover'
      TabOrder = 3
      OnClick = BtnTesteMoverClick
    end
    object BtnClickHCJs: TButton
      Left = 496
      Top = 8
      Width = 137
      Height = 25
      Caption = 'Click hcaptcha with js'
      TabOrder = 4
      OnClick = BtnClickHCJsClick
    end
    object BtnClickHCWebview: TButton
      Left = 636
      Top = 8
      Width = 173
      Height = 25
      Caption = 'Click hcaptcha with wv'
      TabOrder = 5
      OnClick = BtnClickHCWebviewClick
    end
    object SpinEdit1: TSpinEdit
      Left = 496
      Top = 39
      Width = 75
      Height = 24
      MaxValue = 1000
      MinValue = 0
      TabOrder = 6
      Value = 310
    end
    object SpinEdit2: TSpinEdit
      Left = 580
      Top = 39
      Width = 75
      Height = 24
      MaxValue = 1000
      MinValue = 0
      TabOrder = 7
      Value = 500
    end
    object Button1: TButton
      Left = 661
      Top = 39
      Width = 80
      Height = 24
      Caption = 'Click XY'
      TabOrder = 8
      OnClick = Button1Click
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 312
    Top = 160
  end
  object WVBrowser1: TWVBrowser
    DefaultURL = 'https://accounts.hcaptcha.com/demo'
    TargetCompatibleBrowserVersion = '121.0.2277.86'
    AllowSingleSignOnUsingOSPrimaryAccount = False
    OnAfterCreated = WVBrowser1AfterCreated
    OnNavigationCompleted = WVBrowser1NavigationCompleted
    OnSourceChanged = WVBrowser1SourceChanged
    Left = 200
    Top = 160
  end
  object ApplicationEvents1: TApplicationEvents
    OnMessage = ApplicationEvents1Message
    Left = 424
    Top = 160
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 656
    Width = 995
    Height = 22
    Panels = <>
    SimplePanel = True
    SimpleText = ''
  end
end
