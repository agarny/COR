object MainForm: TMainForm
  Left = 0
  Top = 0
  ActiveControl = CloseBtn
  BorderStyle = bsToolWindow
  Caption = 'COR - File Extensions Update'
  ClientHeight = 237
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    467
    237)
  PixelsPerInch = 96
  TextHeight = 13
  object MessagePanel: TPanel
    Left = 4
    Top = 4
    Width = 459
    Height = 185
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvLowered
    Enabled = False
    TabOrder = 0
    object MessageVal: TRichEdit
      Left = 1
      Top = 1
      Width = 457
      Height = 183
      Align = alClient
      Alignment = taCenter
      BorderStyle = bsNone
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      Lines.Strings = (
        
          'As of version 0.9.31.337, and in compliance with the CellML MIME' +
          ' '
        
          'type document, COR uses the '#39'.cellml'#39' extension for CellML files' +
          ' '
        
          'rather than '#39'.cml'#39'. While we were at it, we also decided to repl' +
          'ace the '
        
          #39'.cor'#39' extension for COR projects with '#39'.corproj'#39', even though t' +
          'he '
        'concept of COR project has yet to be implemented in COR.'
        ''
        
          'This utility allows you to rename the extension of all your file' +
          's for you '
        
          '(just drag and drop the file(s) which extension you want to rena' +
          'me '
        
          'or a directory that contains such a file / files), as well as up' +
          'date the '
        
          'registry with the new extensions (just click on the button below' +
          ').')
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      WantReturns = False
    end
  end
  object UpdateBtn: TButton
    Left = 4
    Top = 208
    Width = 181
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Update file extensions information'
    TabOrder = 1
    OnClick = UpdateBtnClick
  end
  object CloseBtn: TButton
    Left = 388
    Top = 208
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    Default = True
    TabOrder = 2
    OnClick = CloseBtnClick
  end
  object ProgressBar: TProgressBar
    Left = 4
    Top = 193
    Width = 459
    Height = 11
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
  end
end
