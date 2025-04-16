object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'PopularCaptcha Solver - Test Application'
  ClientHeight = 679
  ClientWidth = 1196
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
  object Splitter1: TSplitter
    Left = 478
    Top = 77
    Width = 4
    Height = 583
    Align = alRight
    ExplicitLeft = 599
    ExplicitHeight = 492
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 1196
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 1192
    object btnNavigate: TButton
      Left = 406
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Navigate'
      TabOrder = 0
      OnClick = btnNavigateClick
    end
    object edURL: TEdit
      Left = 8
      Top = 10
      Width = 392
      Height = 21
      TabOrder = 1
      Text = 'https://accounts.hcaptcha.com/demo'
    end
    object btnLoadCaptcha: TButton
      Left = 487
      Top = 9
      Width = 152
      Height = 25
      Caption = 'Load Random Captcha'
      TabOrder = 2
      OnClick = btnLoadCaptchaClick
    end
  end
  object pnlControls: TPanel
    Left = 0
    Top = 41
    Width = 1196
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 1192
    object lblStatus: TLabel
      Left = 664
      Top = 10
      Width = 35
      Height = 13
      Caption = 'Status:'
    end
    object btnSolve: TButton
      Left = 8
      Top = 6
      Width = 121
      Height = 25
      Caption = 'Solve Captcha'
      TabOrder = 0
      OnClick = btnSolveClick
    end
    object btnStop: TButton
      Left = 135
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 1
      OnClick = btnStopClick
    end
    object btnRefreshIframe: TButton
      Left = 216
      Top = 6
      Width = 116
      Height = 25
      Caption = 'Refresh Iframes'
      TabOrder = 2
      OnClick = btnRefreshIframeClick
    end
    object cbDetectMethod: TComboBox
      Left = 338
      Top = 8
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnChange = cbDetectMethodChange
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 77
    Width = 478
    Height = 583
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 474
    ExplicitHeight = 582
    object WVWindowParent1: TWVWindowParent
      Left = 0
      Top = 0
      Width = 478
      Height = 583
      Align = alClient
      Color = clWhite
      TabOrder = 0
      ExplicitWidth = 474
      ExplicitHeight = 582
    end
  end
  object pnlLog: TPanel
    Left = 482
    Top = 77
    Width = 357
    Height = 583
    Align = alRight
    BevelOuter = bvNone
    Caption = 'pnlLog'
    TabOrder = 3
    ExplicitLeft = 478
    ExplicitHeight = 582
    object txtLog: TMemo
      Left = 0
      Top = 0
      Width = 357
      Height = 583
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
      ExplicitHeight = 582
    end
  end
  object pnlSolverConfig: TPanel
    Left = 839
    Top = 77
    Width = 357
    Height = 583
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 4
    ExplicitLeft = 835
    ExplicitHeight = 582
    object lblApiKey: TLabel
      Left = 24
      Top = 24
      Width = 42
      Height = 13
      Caption = 'API Key:'
    end
    object edApiKey: TEdit
      Left = 24
      Top = 43
      Width = 305
      Height = 21
      TabOrder = 0
    end
    object chkAutoSolve: TCheckBox
      Left = 24
      Top = 88
      Width = 137
      Height = 17
      Caption = 'Auto Solve'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkAutoOpen: TCheckBox
      Left = 24
      Top = 111
      Width = 153
      Height = 17
      Caption = 'Auto Open'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkEnglish: TCheckBox
      Left = 24
      Top = 134
      Width = 153
      Height = 17
      Caption = 'English Language'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object chkAlwaysSolve: TCheckBox
      Left = 24
      Top = 157
      Width = 153
      Height = 17
      Caption = 'Always Solve'
      TabOrder = 4
    end
    object btnSettings: TButton
      Left = 24
      Top = 200
      Width = 305
      Height = 25
      Caption = 'Save Settings'
      TabOrder = 5
      OnClick = btnSettingsClick
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 660
    Width = 1196
    Height = 19
    Panels = <
      item
        Width = 400
      end>
    ExplicitTop = 659
    ExplicitWidth = 1192
  end
end
