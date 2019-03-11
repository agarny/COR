object UpdateForm: TUpdateForm
  Left = 656
  Top = 173
  ActiveControl = CancelBtn
  BorderStyle = bsDialog
  Caption = 'COR Update'
  ClientHeight = 113
  ClientWidth = 317
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ElapsedLab: TLabel
    Left = 4
    Top = 48
    Width = 41
    Height = 13
    Caption = 'Elapsed:'
    Transparent = True
  end
  object ElapsedVal: TLabel
    Left = 52
    Top = 48
    Width = 3
    Height = 13
    Transparent = True
  end
  object RemainingLab: TLabel
    Left = 156
    Top = 48
    Width = 53
    Height = 13
    Caption = 'Remaining:'
    Transparent = True
  end
  object RemainingVal: TLabel
    Left = 216
    Top = 48
    Width = 3
    Height = 13
    Transparent = True
  end
  object TransferRateLab: TLabel
    Left = 92
    Top = 64
    Width = 68
    Height = 13
    Caption = 'Transfer rate:'
    Transparent = True
  end
  object TransferRateVal: TLabel
    Left = 164
    Top = 64
    Width = 3
    Height = 13
    Transparent = True
  end
  object CurrentVersionLab: TLabel
    Left = 4
    Top = 4
    Width = 79
    Height = 13
    Caption = 'Current version:'
    Transparent = True
  end
  object CurrentVersionVal: TLabel
    Left = 84
    Top = 4
    Width = 3
    Height = 13
    Transparent = True
  end
  object NewVersionLab: TLabel
    Left = 168
    Top = 4
    Width = 63
    Height = 13
    Caption = 'New version:'
    Transparent = True
  end
  object NewVersionVal: TLabel
    Left = 236
    Top = 4
    Width = 3
    Height = 13
    Transparent = True
  end
  object CancelBtn: TButton
    Left = 121
    Top = 84
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    Default = True
    ModalResult = 2
    TabOrder = 0
    OnClick = CancelBtnClick
  end
  object ProgressBar: TProgressBar
    Left = 4
    Top = 24
    Width = 309
    Height = 17
    TabOrder = 1
  end
  object Timer: TTimer
    Interval = 1
    OnTimer = TimerTimer
    Left = 240
    Top = 80
  end
end
