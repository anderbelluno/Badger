object FormMain: TFormMain
  Left = 523
  Top = 250
  Width = 576
  Height = 609
  Caption = 'Badger IOCP'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 560
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblPort: TLabel
      Left = 128
      Top = 16
      Width = 25
      Height = 13
      Caption = 'Porta'
    end
    object btnStartStop: TButton
      Left = 8
      Top = 12
      Width = 105
      Height = 25
      Caption = 'Iniciar'
      TabOrder = 0
      OnClick = btnStartStopClick
    end
    object edtPort: TEdit
      Left = 168
      Top = 12
      Width = 57
      Height = 21
      TabOrder = 1
      Text = '8081'
    end
    object btnPing: TButton
      Left = 248
      Top = 12
      Width = 97
      Height = 25
      Caption = 'GET /ping'
      Enabled = False
      TabOrder = 2
      OnClick = btnPingClick
    end
    object btnOther: TButton
      Left = 351
      Top = 12
      Width = 97
      Height = 25
      Caption = 'GET /other'
      Enabled = False
      TabOrder = 3
      OnClick = btnOtherClick
    end
  end
  object MemoLog: TMemo
    Left = 0
    Top = 49
    Width = 560
    Height = 321
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object PanelWs: TPanel
    Left = 0
    Top = 370
    Width = 560
    Height = 200
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lblWs: TLabel
      Left = 0
      Top = 0
      Width = 560
      Height = 13
      Align = alTop
      Caption = '  WebSocket  /chat'
    end
    object MemoWs: TMemo
      Left = 0
      Top = 13
      Width = 560
      Height = 155
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object PanelWsSend: TPanel
      Left = 0
      Top = 168
      Width = 560
      Height = 32
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnWsSend: TButton
        Left = 480
        Top = 0
        Width = 80
        Height = 28
        Caption = 'Enviar'
        Enabled = False
        TabOrder = 1
        OnClick = btnWsSendClick
      end
      object edtWs: TEdit
        Left = 0
        Top = 0
        Width = 480
        Height = 21
        Enabled = False
        TabOrder = 0
        OnKeyPress = edtWsKeyPress
      end
    end
  end
  object tmrLog: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrLogTimer
    Left = 520
    Top = 8
  end
end
