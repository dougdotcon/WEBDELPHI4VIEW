object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Testador de Captcha Popular'
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
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblUrl: TLabel
      Left = 12
      Top = 12
      Width = 23
      Height = 13
      Caption = 'URL:'
    end
    object lblApiKey: TLabel
      Left = 12
      Top = 39
      Width = 55
      Height = 13
      Caption = 'Chave API:'
    end
    object lblApiEndpoint: TLabel
      Left = 400
      Top = 39
      Width = 66
      Height = 13
      Caption = 'Endpoint API:'
    end
    object edtUrl: TEdit
      Left = 41
      Top = 9
      Width = 650
      Height = 21
      TabOrder = 0
    end
    object btnNavigate: TButton
      Left = 697
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Navegar'
      TabOrder = 1
      OnClick = btnNavigateClick
    end
    object btnRefresh: TButton
      Left = 778
      Top = 8
      Width = 25
      Height = 25
      Caption = 'R'
      TabOrder = 2
      OnClick = btnRefreshClick
    end
    object btnBack: TButton
      Left = 809
      Top = 8
      Width = 25
      Height = 25
      Caption = '<'
      TabOrder = 3
      OnClick = btnBackClick
    end
    object btnForward: TButton
      Left = 840
      Top = 8
      Width = 25
      Height = 25
      Caption = '>'
      TabOrder = 4
      OnClick = btnForwardClick
    end
    object btnStop: TButton
      Left = 871
      Top = 8
      Width = 25
      Height = 25
      Caption = 'X'
      TabOrder = 5
      OnClick = btnStopClick
    end
    object edtApiKey: TEdit
      Left = 74
      Top = 36
      Width = 320
      Height = 21
      PasswordChar = '*'
      TabOrder = 6
    end
    object edtApiEndpoint: TEdit
      Left = 477
      Top = 36
      Width = 320
      Height = 21
      TabOrder = 7
      Text = 'https://api.captchasolver.com/solve'
    end
    object btnSettings: TButton
      Left = 803
      Top = 35
      Width = 93
      Height = 25
      Caption = 'Configura'#231#245'es'
      TabOrder = 8
      OnClick = btnSettingsClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 560
    Width = 900
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlStatus: TPanel
      Left = 0
      Top = 0
      Width = 900
      Height = 20
      Align = alTop
      BevelOuter = bvLowered
      TabOrder = 0
      object lblStatus: TLabel
        Left = 8
        Top = 3
        Width = 36
        Height = 13
        Caption = 'Pronto.'
      end
    end
    object chkEnglish: TCheckBox
      Left = 824
      Top = 21
      Width = 73
      Height = 17
      Caption = 'Ingl'#234's'
      TabOrder = 1
      OnClick = chkEnglishClick
    end
  end
  object pnlCenter: TPanel
    Left = 0
    Top = 65
    Width = 900
    Height = 495
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object pnlWebView: TPanel
      Left = 0
      Top = 0
      Width = 400
      Height = 495
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 2
      ExplicitTop = -2
    end
    object pnlControls: TPanel
      Left = 400
      Top = 0
      Width = 250
      Height = 495
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 406
      ExplicitTop = -2
      object lblDetectionMethod: TLabel
        Left = 8
        Top = 8
        Width = 103
        Height = 13
        Caption = 'M'#233'todo de Detec'#231#227'o:'
      end
      object cmbDetectionMethod: TComboBox
        Left = 8
        Top = 27
        Width = 234
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnChange = cmbDetectionMethodChange
      end
      object chkAutoSolve: TCheckBox
        Left = 8
        Top = 54
        Width = 97
        Height = 17
        Caption = 'Auto Resolver'
        TabOrder = 1
      end
      object chkAutoOpen: TCheckBox
        Left = 8
        Top = 77
        Width = 97
        Height = 17
        Caption = 'Auto Abrir'
        TabOrder = 2
      end
      object btnSolve: TButton
        Left = 8
        Top = 100
        Width = 234
        Height = 40
        Caption = 'Resolver Captcha'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnClick = btnSolveClick
      end
      object btnClear: TButton
        Left = 8
        Top = 146
        Width = 234
        Height = 25
        Caption = 'Limpar'
        TabOrder = 4
        OnClick = btnClearClick
      end
    end
    object pnlLog: TPanel
      Left = 650
      Top = 0
      Width = 250
      Height = 495
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object mmoLog: TMemo
        Left = 0
        Top = 0
        Width = 250
        Height = 470
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object btnClearLog: TButton
        Left = 0
        Top = 470
        Width = 250
        Height = 25
        Align = alBottom
        Caption = 'Limpar Log'
        TabOrder = 1
        OnClick = btnClearLogClick
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 56
    Top = 137
  end
end
