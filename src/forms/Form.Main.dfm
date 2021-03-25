object AppForm: TAppForm
  Left = 0
  Top = 0
  Caption = ' Form Validaton - Created by DCE-Systems'
  ClientHeight = 342
  ClientWidth = 557
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 5
  Padding.Top = 5
  Padding.Right = 5
  Padding.Bottom = 5
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 212
    Height = 326
    Align = alLeft
    Caption = 'Form fields'
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    TabOrder = 0
    ExplicitLeft = 10
    object LabelEmail: TLabel
      AlignWithMargins = True
      Left = 10
      Top = 109
      Width = 192
      Height = 13
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Email'
      ExplicitLeft = 64
      ExplicitTop = 181
      ExplicitWidth = 24
    end
    object LabelFirstname: TLabel
      AlignWithMargins = True
      Left = 10
      Top = 23
      Width = 192
      Height = 13
      Margins.Bottom = 0
      Align = alTop
      Caption = 'First name'
      ExplicitWidth = 50
    end
    object LabelLastname: TLabel
      AlignWithMargins = True
      Left = 10
      Top = 66
      Width = 192
      Height = 13
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Last name'
      ExplicitWidth = 49
    end
    object LabelPwd: TLabel
      AlignWithMargins = True
      Left = 10
      Top = 152
      Width = 192
      Height = 13
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Password'
      ExplicitWidth = 46
    end
    object Label1: TLabel
      AlignWithMargins = True
      Left = 10
      Top = 195
      Width = 192
      Height = 13
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Retype password'
      ExplicitWidth = 84
    end
    object EditEmail: TEdit
      AlignWithMargins = True
      Left = 10
      Top = 125
      Width = 192
      Height = 21
      Align = alTop
      TabOrder = 2
      ExplicitLeft = 64
      ExplicitTop = 200
      ExplicitWidth = 201
    end
    object EditFirstname: TEdit
      AlignWithMargins = True
      Left = 10
      Top = 39
      Width = 192
      Height = 21
      Align = alTop
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      ExplicitLeft = 12
    end
    object EditLastname: TEdit
      AlignWithMargins = True
      Left = 10
      Top = 82
      Width = 192
      Height = 21
      Align = alTop
      TabOrder = 1
      ExplicitLeft = 32
      ExplicitTop = 128
      ExplicitWidth = 201
    end
    object EditPassword: TEdit
      AlignWithMargins = True
      Left = 10
      Top = 168
      Width = 192
      Height = 21
      Align = alTop
      PasswordChar = '*'
      TabOrder = 3
    end
    object EditRetypePassword: TEdit
      AlignWithMargins = True
      Left = 10
      Top = 211
      Width = 192
      Height = 21
      Align = alTop
      PasswordChar = '*'
      TabOrder = 4
      ExplicitLeft = 12
    end
  end
  object GroupBox2: TGroupBox
    Left = 223
    Top = 5
    Width = 128
    Height = 332
    Align = alLeft
    Caption = 'Commands'
    TabOrder = 1
    ExplicitLeft = 297
    ExplicitTop = 85
    ExplicitHeight = 217
    object Bevel1: TBevel
      AlignWithMargins = True
      Left = 5
      Top = 114
      Width = 118
      Height = 26
      Align = alTop
      Shape = bsSpacer
      ExplicitLeft = 2
      ExplicitTop = 139
      ExplicitWidth = 124
    end
    object btnSimpleValidation: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 118
      Height = 42
      Align = alTop
      Caption = 'Simple validation'
      TabOrder = 0
      OnClick = btnSimpleValidationClick
    end
    object btnAnnotationValidation: TButton
      AlignWithMargins = True
      Left = 5
      Top = 66
      Width = 118
      Height = 42
      Align = alTop
      Caption = 'Annotation Validation'
      TabOrder = 1
      OnClick = btnAnnotationValidationClick
      ExplicitLeft = 7
    end
    object btnPopulateForm: TButton
      AlignWithMargins = True
      Left = 5
      Top = 146
      Width = 118
      Height = 42
      Align = alTop
      Caption = 'Populate Form'
      TabOrder = 2
      OnClick = btnPopulateFormClick
      ExplicitLeft = 32
      ExplicitTop = 192
      ExplicitWidth = 75
    end
  end
  object MemoValidation: TMemo
    AlignWithMargins = True
    Left = 354
    Top = 8
    Width = 191
    Height = 326
    Align = alLeft
    Lines.Strings = (
      'MemoValidation')
    ScrollBars = ssVertical
    TabOrder = 2
    ExplicitHeight = 266
  end
end
