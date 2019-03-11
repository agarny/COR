object PasswordForm: TPasswordForm
  Left = 468
  Top = 419
  ActiveControl = PasswordVal
  BorderStyle = bsDialog
  Caption = 'Password'
  ClientHeight = 82
  ClientWidth = 197
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PasswordLab: TLabel
    Left = 4
    Top = 4
    Width = 50
    Height = 13
    Caption = 'Password:'
    FocusControl = PasswordVal
    Transparent = True
  end
  object OKBtn: TButton
    Left = 17
    Top = 53
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 105
    Top = 53
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object PasswordVal: TJvValidateEdit
    Left = 4
    Top = 24
    Width = 189
    Height = 21
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    TabOrder = 0
  end
end
