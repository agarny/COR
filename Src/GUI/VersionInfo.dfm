object VersionInfoForm: TVersionInfoForm
  Left = 366
  Top = 267
  ActiveControl = YesBtn
  BorderStyle = bsDialog
  Caption = 'COR Version Info'
  ClientHeight = 343
  ClientWidth = 411
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MsgLab: TLabel
    Left = 4
    Top = 4
    Width = 36
    Height = 13
    Caption = 'MsgLab'
    Transparent = True
    WordWrap = True
  end
  object QuestionLab: TLabel
    Left = 4
    Top = 228
    Width = 60
    Height = 13
    Caption = 'QuestionLab'
    Transparent = True
  end
  object YesBtn: TButton
    Left = 112
    Top = 272
    Width = 75
    Height = 25
    Caption = '&Yes'
    Default = True
    ModalResult = 6
    TabOrder = 1
  end
  object NoBtn: TButton
    Left = 200
    Top = 272
    Width = 75
    Height = 25
    Caption = '&No'
    ModalResult = 7
    TabOrder = 2
  end
  object VersionInfoScrollBox: TScrollBox
    Left = 4
    Top = 32
    Width = 381
    Height = 185
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    object VersionInfoPanel: TPanel
      Left = 0
      Top = 0
      Width = 265
      Height = 145
      BevelOuter = bvNone
      Enabled = False
      TabOrder = 0
      object VersionInfoVal: TRichEdit
        Left = 3
        Top = 3
        Width = 265
        Height = 145
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 0
        WantReturns = False
        OnResizeRequest = VersionInfoValResizeRequest
      end
    end
  end
end
