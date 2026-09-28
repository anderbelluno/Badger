object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Badger IOCP'
  ClientHeight = 529
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnDestroy = FormDestroy
  TextHeight = 15
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 563
    Height = 121
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblPort: TLabel
      Left = 134
      Top = 15
      Width = 28
      Height = 15
      Caption = 'Porta'
    end
    object Label1: TLabel
      Left = 8
      Top = 44
      Width = 149
      Height = 15
      Caption = 'MaxConcurrentConnections'
    end
    object btnStartStop: TButton
      Left = 0
      Top = 90
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
      Height = 23
      TabOrder = 1
      Text = '8080'
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
    object checkParallel: TCheckBox
      Left = 8
      Top = 21
      Width = 114
      Height = 17
      Caption = 'ParallelProcessing'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object edtMax: TEdit
      Left = 163
      Top = 41
      Width = 121
      Height = 23
      TabOrder = 5
      Text = '5000'
    end
  end
  object MemoLog: TMemo
    Left = 0
    Top = 121
    Width = 563
    Height = 208
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object PanelWs: TPanel
    Left = 0
    Top = 329
    Width = 563
    Height = 200
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lblWs: TLabel
      Left = 0
      Top = 0
      Width = 563
      Height = 15
      Align = alTop
      Caption = '  WebSocket  /chat'
      ExplicitWidth = 99
    end
    object MemoWs: TMemo
      Left = 0
      Top = 15
      Width = 563
      Height = 151
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object PanelWsSend: TPanel
      Left = 0
      Top = 166
      Width = 563
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnWsSend: TButton
        Left = 483
        Top = 0
        Width = 80
        Height = 34
        Align = alRight
        Caption = 'Enviar'
        Enabled = False
        TabOrder = 1
        OnClick = btnWsSendClick
      end
      object edtWs: TEdit
        Left = 0
        Top = 0
        Width = 483
        Height = 34
        Align = alClient
        Enabled = False
        TabOrder = 0
        OnKeyPress = edtWsKeyPress
        ExplicitHeight = 23
      end
    end
  end
end
