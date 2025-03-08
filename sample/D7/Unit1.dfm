object Form1: TForm1
  Left = 192
  Top = 125
  Width = 1305
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
    Width = 1104
    Height = 571
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 1289
    Height = 65
    Align = alTop
    TabOrder = 1
    object btnSyna: TButton
      Left = 8
      Top = 16
      Width = 105
      Height = 25
      Caption = 'Iniciar Servidor'
      TabOrder = 0
      OnClick = btnSynaClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 65
    Width = 185
    Height = 571
    Align = alLeft
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 80
      Width = 31
      Height = 13
      Caption = 'Porta: '
    end
    object CBxNonBlockMode: TCheckBox
      Left = 16
      Top = 48
      Width = 97
      Height = 17
      Caption = 'NonBlockMode'
      TabOrder = 0
    end
    object edtPorta: TEdit
      Left = 56
      Top = 80
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '8080'
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 176
      Width = 169
      Height = 105
      Caption = 'Autentica'#231#227'o'
      ItemIndex = 0
      Items.Strings = (
        'None'
        'Basic'
        'Token')
      TabOrder = 2
    end
    object rdLog: TCheckBox
      Left = 16
      Top = 112
      Width = 97
      Height = 17
      Caption = 'Log'
      TabOrder = 3
    end
  end
end
