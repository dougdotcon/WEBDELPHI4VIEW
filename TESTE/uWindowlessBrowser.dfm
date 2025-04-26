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
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
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
    ExplicitTop = 24
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
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 312
    Top = 160
  end
  object WVBrowser1: TWVBrowser
    TargetCompatibleBrowserVersion = '95.0.1020.44'
    AllowSingleSignOnUsingOSPrimaryAccount = False
    OnInitializationError = WVBrowser1InitializationError
    OnAfterCreated = WVBrowser1AfterCreated
    OnExecuteScriptCompleted = WVBrowser1ExecuteScriptCompleted
    OnDocumentTitleChanged = WVBrowser1DocumentTitleChanged
    OnWebMessageReceived = WVBrowser1WebMessageReceived
    OnCursorChanged = WVBrowser1CursorChanged
    OnRetrieveMHTMLCompleted = WVBrowser1RetrieveMHTMLCompleted
    OnContextMenuRequested = WVBrowser1ContextMenuRequested
    OnCustomItemSelected = WVBrowser1CustomItemSelected
    Left = 200
    Top = 160
  end
  object ApplicationEvents1: TApplicationEvents
    OnMessage = ApplicationEvents1Message
    Left = 424
    Top = 160
  end
end
