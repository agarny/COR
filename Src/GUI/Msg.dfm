object MsgForm: TMsgForm
  Left = 315
  Top = 261
  BorderStyle = bsNone
  ClientHeight = 84
  ClientWidth = 334
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Visible = True
  DesignSize = (
    334
    84)
  PixelsPerInch = 96
  TextHeight = 13
  object Shape: TShape
    Left = 0
    Top = 0
    Width = 334
    Height = 84
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitWidth = 392
    ExplicitHeight = 17
  end
  object Msg: TLabel
    Left = 4
    Top = 2
    Width = 326
    Height = 80
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'Msg'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -39
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ExplicitWidth = 384
    ExplicitHeight = 13
  end
end
