object Form1: TForm1
  Left = 192
  Top = 125
  Width = 690
  Height = 675
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 185
    Top = 73
    Width = 489
    Height = 563
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 674
    Height = 73
    Align = alTop
    TabOrder = 1
    object Label2: TLabel
      Left = 408
      Top = 28
      Width = 57
      Height = 13
      Caption = 'Timeout(ms)'
    end
    object Label1: TLabel
      Left = 216
      Top = 28
      Width = 31
      Height = 13
      Caption = 'Porta: '
    end
    object btnSyna: TButton
      Left = 24
      Top = 28
      Width = 105
      Height = 25
      Caption = 'Iniciar Servidor'
      TabOrder = 0
      OnClick = btnSynaClick
    end
    object edtPorta: TEdit
      Left = 256
      Top = 28
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '8080'
    end
    object edtTimeOut: TEdit
      Left = 472
      Top = 28
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '10000'
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 73
    Width = 185
    Height = 563
    Align = alLeft
    TabOrder = 2
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 80
      Width = 169
      Height = 137
      Caption = 'Autentica'#231#227'o'
      ItemIndex = 0
      Items.Strings = (
        'None'
        'Basic'
        'Token'
        'JWT')
      TabOrder = 0
    end
    object rdLog: TCheckBox
      Left = 16
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Request Log'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object btnClearLog: TButton
      Left = 24
      Top = 248
      Width = 75
      Height = 25
      Caption = 'Clear Log'
      TabOrder = 2
      OnClick = btnClearLogClick
    end
  end
end
