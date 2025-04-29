object Form1: TForm1
  Left = 192
  Top = 125
  Width = 663
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
    Top = 65
    Width = 462
    Height = 571
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 647
    Height = 65
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 179
      Top = 25
      Width = 31
      Height = 13
      Caption = 'Porta: '
    end
    object Label2: TLabel
      Left = 400
      Top = 24
      Width = 57
      Height = 13
      Caption = 'Timeout(ms)'
    end
    object btnSyna: TButton
      Left = 8
      Top = 16
      Width = 105
      Height = 25
      Caption = 'Iniciar Servidor'
      TabOrder = 0
      OnClick = btnSynaClick
    end
    object edtPorta: TEdit
      Left = 225
      Top = 21
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '8080'
    end
    object edtTimeOut: TEdit
      Left = 464
      Top = 20
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '10000'
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 65
    Width = 185
    Height = 571
    Align = alLeft
    TabOrder = 2
    object CBxNonBlockMode: TCheckBox
      Left = 16
      Top = 48
      Width = 97
      Height = 17
      Caption = 'NonBlockMode'
      TabOrder = 0
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 80
      Width = 169
      Height = 105
      Caption = 'Autentica'#231#227'o'
      ItemIndex = 0
      Items.Strings = (
        'None'
        'Basic'
        'Token')
      TabOrder = 1
    end
    object rdLog: TCheckBox
      Left = 16
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Log'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object btnClearLog: TButton
      Left = 16
      Top = 216
      Width = 75
      Height = 25
      Caption = 'Clear Log'
      TabOrder = 3
      OnClick = btnClearLogClick
    end
  end
end
