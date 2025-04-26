object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Testador de CAPTCHAs'
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 41
    Align = alTop
    TabOrder = 0
    object lblApiKey: TLabel
      Left = 520
      Top = 14
      Width = 44
      Height = 13
      Caption = 'API Key:'
    end
    object edtURL: TEdit
      Left = 8
      Top = 10
      Width = 401
      Height = 21
      TabOrder = 0
      Text = 'https://accounts.hcaptcha.com/demo'
    end
    object btnGo: TButton
      Left = 415
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Navegar'
      TabOrder = 1
      OnClick = btnGoClick
    end
    object edtApiKey: TEdit
      Left = 568
      Top = 10
      Width = 233
      Height = 21
      TabOrder = 2
      Text = 'sonic_144b3b7c412414e2ce4ebfaa6a8f71d4657c'
    end
    object btnGetMHTML: TButton
      Left = 807
      Top = 9
      Width = 85
      Height = 25
      Caption = 'Obter MHTML'
      TabOrder = 3
      OnClick = btnGetMHTMLClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 900
    Height = 40
    Align = alTop
    TabOrder = 1
    object lblStatus: TLabel
      Left = 8
      Top = 14
      Width = 105
      Height = 13
      Caption = 'Aguardando browser...'
    end
    object lblCaptchaSolver: TLabel
      Left = 608
      Top = 14
      Width = 42
      Height = 13
      Caption = 'Solvedor:'
    end
    object btnSolve: TButton
      Left = 808
      Top = 8
      Width = 84
      Height = 25
      Caption = 'Resolver'
      TabOrder = 0
      OnClick = btnSolveClick
    end
    object cbxSolver: TComboBox
      Left = 656
      Top = 10
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 400
    Width = 900
    Height = 200
    Align = alBottom
    TabOrder = 2
    object tabLogs: TTabSheet
      Caption = 'Logs'
      object lstLogs: TMemo
        Left = 0
        Top = 0
        Width = 892
        Height = 172
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object tabRaw: TTabSheet
      Caption = 'MHTML'
      ImageIndex = 1
      object memRaw: TMemo
        Left = 0
        Top = 0
        Width = 892
        Height = 172
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object WVWindowParent1: TWVWindowParent
    Left = 0
    Top = 81
    Width = 900
    Height = 319
    Align = alClient
    TabOrder = 3
  end
  object WVBrowser1: TWVBrowser
    TargetCompatibleBrowserVersion = '109.0.1518.78'
    DefaultURL = 'about:blank'
    OnInitializationError = WVBrowser1InitializationError
    OnAfterCreated = WVBrowser1AfterCreated
    OnDocumentTitleChanged = WVBrowser1DocumentTitleChanged
    OnRetrieveMHTMLCompleted = WVBrowser1RetrieveMHTMLCompleted
    Left = 32
    Top = 120
  end
end
