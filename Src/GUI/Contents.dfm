object ContentsFrame: TContentsFrame
  Left = 0
  Top = 0
  Width = 600
  Height = 280
  TabOrder = 0
  TabStop = True
  object StatusBar: TStatusBar
    Left = 0
    Top = 260
    Width = 600
    Height = 20
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
    OnDrawPanel = StatusBarDrawPanel
  end
  object RootPanel: TPanel
    Left = 0
    Top = 52
    Width = 600
    Height = 208
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object BottomSplitter: TSplitter
      Left = 0
      Top = 192
      Width = 600
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      Color = clBtnFace
      ParentColor = False
      ResizeStyle = rsUpdate
    end
    object TopSplitter: TSplitter
      Left = 0
      Top = 13
      Width = 600
      Height = 3
      Cursor = crVSplit
      Align = alTop
      AutoSnap = False
      Color = clBtnFace
      ParentColor = False
      ResizeStyle = rsUpdate
    end
    object LeftSplitter: TSplitter
      Left = 13
      Top = 17
      Height = 175
      AutoSnap = False
      Color = clBtnFace
      ParentColor = False
      ResizeStyle = rsUpdate
    end
    object RightSplitter: TSplitter
      Left = 584
      Top = 17
      Height = 175
      Align = alRight
      AutoSnap = False
      Color = clBtnFace
      ParentColor = False
      ResizeStyle = rsUpdate
    end
    object BottomDockSite: TPanel
      Tag = 3
      Left = 0
      Top = 195
      Width = 600
      Height = 13
      Align = alBottom
      BevelOuter = bvNone
      DockSite = True
      TabOrder = 2
      OnDockDrop = BottomDockSiteDockDrop
      OnDockOver = DockSiteDockOver
      OnGetSiteInfo = DockSiteGetSiteInfo
      OnUnDock = BottomDockSiteUnDock
    end
    object TopDockSite: TPanel
      Tag = 1
      Left = 0
      Top = 0
      Width = 600
      Height = 13
      Align = alTop
      BevelOuter = bvNone
      DockSite = True
      TabOrder = 0
      OnDockDrop = TopDockSiteDockDrop
      OnDockOver = DockSiteDockOver
      OnGetSiteInfo = DockSiteGetSiteInfo
      OnUnDock = TopDockSiteUnDock
    end
    object LeftDockSite: TPanel
      Tag = 2
      Left = 0
      Top = 17
      Width = 13
      Height = 175
      Align = alLeft
      BevelOuter = bvNone
      DockSite = True
      TabOrder = 1
      OnDockDrop = LeftDockSiteDockDrop
      OnDockOver = DockSiteDockOver
      OnGetSiteInfo = DockSiteGetSiteInfo
      OnUnDock = LeftDockSiteUnDock
    end
    object RightDockSite: TPanel
      Tag = 4
      Left = 587
      Top = 17
      Width = 13
      Height = 175
      Align = alRight
      BevelOuter = bvNone
      DockSite = True
      TabOrder = 3
      OnDockDrop = RightDockSiteDockDrop
      OnDockOver = DockSiteDockOver
      OnGetSiteInfo = DockSiteGetSiteInfo
      OnUnDock = RightDockSiteUnDock
    end
    object DummyPanel: TPanel
      Left = 0
      Top = 16
      Width = 600
      Height = 1
      Align = alTop
      TabOrder = 4
    end
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 52
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object ControlBar: TControlBar
      Tag = 1
      Left = 0
      Top = 0
      Width = 600
      Height = 52
      Align = alClient
      AutoSize = True
      BevelKind = bkNone
      PopupMenu = ControlBarPopupMenu
      TabOrder = 0
      OnGetSiteInfo = ControlBarGetSiteInfo
      object ControlBarMenuToolBar: TToolBar
        Tag = 1
        Left = 11
        Top = 2
        Width = 254
        Height = 22
        AutoSize = True
        ButtonHeight = 21
        ButtonWidth = 29
        ShowCaptions = True
        TabOrder = 0
        Wrapable = False
      end
      object ControlBarRunMenuToolBar: TToolBar
        Tag = 1
        Left = 227
        Top = 28
        Width = 150
        Height = 22
        ButtonWidth = 33
        DragKind = dkDock
        DragMode = dmAutomatic
        Images = ImageList
        TabOrder = 2
        Wrapable = False
      end
      object ControlBarFileMenuToolBar: TToolBar
        Tag = 1
        Left = 11
        Top = 28
        Width = 203
        Height = 22
        AutoSize = True
        DragKind = dkDock
        DragMode = dmAutomatic
        Images = ImageList
        TabOrder = 1
        Wrapable = False
      end
    end
  end
  object ProgressBar: TProgressBar
    Left = 169
    Top = 169
    Width = 150
    Height = 16
    TabOrder = 3
  end
  object Menu: TMainMenu
    Images = ImageList
    Left = 40
    Top = 128
    object MenuFile: TMenuItem
      Caption = 'File'
      object MenuFileExit: TMenuItem
        Action = ActionFileExit
      end
    end
    object MenuView: TMenuItem
      Caption = 'View'
      object MenuViewSep1: TMenuItem
        Caption = '-'
      end
      object MenuViewToolbars: TMenuItem
        Caption = 'Toolbars'
        object MenuViewToolbarsFile: TMenuItem
          Action = ActionViewFileToolBar
          AutoCheck = True
        end
        object MenuViewToolbarsRun: TMenuItem
          Action = ActionViewRunToolBar
          AutoCheck = True
        end
      end
    end
    object MenuRun: TMenuItem
      Caption = 'Run'
      object MenuRunRun: TMenuItem
        Action = ActionRunRun
      end
    end
    object MenuTools: TMenuItem
      Caption = 'Tools'
      object MenuToolsOptions: TMenuItem
        Action = ActionToolsOptions
      end
      object MenuToolsSep1: TMenuItem
        Caption = '-'
      end
      object MenuToolsResetAll: TMenuItem
        Action = ActionToolsResetAll
      end
    end
    object MenuHelp: TMenuItem
      Caption = 'Help'
      object MenuHelpHelp: TMenuItem
        Action = ActionHelpHelp
      end
      object MenuHelpSep1: TMenuItem
        Caption = '-'
      end
      object MenuHelpHowTo: TMenuItem
        Action = ActionHelpHowTo
      end
      object MenuHelpFAQ: TMenuItem
        Action = ActionHelpFAQ
      end
      object MenuHelpSep2: TMenuItem
        Caption = '-'
      end
      object MenuHelpHomePage: TMenuItem
        Action = ActionHelpHomePage
      end
      object MenuHelpSep3: TMenuItem
        Caption = '-'
      end
      object MenuHelpUpdate: TMenuItem
        Action = ActionHelpUpdate
      end
      object MenuHelpSep4: TMenuItem
        Caption = '-'
      end
      object MenuHelpCellMLHomePage: TMenuItem
        Action = ActionHelpCellMLHomePage
      end
      object MenuHelpCellMLRepository: TMenuItem
        Action = ActionHelpCellMLRepository
      end
      object MenuHelpSep5: TMenuItem
        Caption = '-'
      end
      object MenuHelpAbout: TMenuItem
        Action = ActionHelpAbout
      end
    end
  end
  object ImageList: TImageList
    Left = 72
    Top = 128
    Bitmap = {
      494C010102000400100010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000001079F000313A9000418AE000419AE000313A9000108A0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000104
      9D00041CB1000730C0000734C4000735C5000735C5000734C3000731C100041F
      B30001069E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000109A100052B
      C3000735C7000733C2000732C2000732C2000732C2000732C2000733C3000735
      C400062DBE00020CA40000000000000000000000000000000000000000000000
      000000FF0000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000001049B00052BCA000636
      D8000431CD000027C400032EC1000732C2000732C2000430C1000027BF00042F
      C1000735C400072EBE0001069E00000000000000000000000000000000000000
      0000C6C6C60000FF0000C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000031ABA000537E7000331
      DD00123DD8006480E0001840CB00002CC100022DC0000F38C4006580D9001B43
      C700052FC1000735C500051FB300000000000000000000000000000000000000
      000000FF0000C6C6C60000FF0000C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000001049E000430E4000436F100002A
      E4005070E900FFFFFF00B7C4F1000D36CA00042DC300A2B2E800FFFFFF006984
      DA000026BE000733C3000731C1000108A0000000000000000000000000000000
      0000C6C6C60000FF0000C6C6C60000FF0000C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000020FAF000336FA000335F8000232
      EE000A35E8008CA2F200FFFFFF00B4C2F100A9B8ED00FFFFFF00A7B7E900133A
      C400052FC1000732C2000734C4000313AA000000000000000000000000000000
      000000FF0000C6C6C60000FF0000C6C6C60000FF0000C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000619BC001747FE00093AFC000435
      F8000131F000002BE80091A5F400FFFFFF00FFFFFF00ABBAEF00062FC500022D
      C0000732C2000732C2000736C5000419AE000000000000000000000000000000
      0000C6C6C60000FF0000C6C6C60000FF0000C6C6C60000FF0000C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000B1DBE004168FE001C49FC000335
      FB000031F9000531F200A4B5F700FFFFFF00FFFFFF00B9C6F2000D36D000002C
      C6000732C2000732C2000736C5000418AD000000000000000000000000000000
      000000FF0000C6C6C60000FF0000C6C6C60000FF0000C6C6C60000FF0000C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000613B4005B7CFC00486CFD000133
      FB00113CFB00A1B4FE00FFFFFF00A4B6F80092A7F500FFFFFF00B6C4F2001A41
      D300042FC8000732C4000734C3000212A9000000000000000000000000000000
      0000C6C6C60000FF0000C6C6C60000FF0000C6C6C60000FF0000C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000003A0004A6AF3008FA6FF001F46
      FB004C6FFC00FFFFFF00A7B8FE000733F600002AED008CA2F600FFFFFF00627F
      E7000028D0000734CC000730C30000069F000000000000000000000000000000
      000000FF0000C6C6C60000FF0000C6C6C60000FF0000C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001A2FCB0099AFFF008BA2
      FE00214DFB004D71FC000E3DFB000030FB000031F7000636F1004C6EF100103C
      E3000432DB000636D700041CB500000000000000000000000000000000000000
      0000C6C6C60000FF0000C6C6C60000FF0000C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000004A000415EEC00B8C7
      FF009CAFFD003A5CFC000A3AFB000335FB000335FB000133F900052FF2000635
      EB000537E900052CCD0000049C00000000000000000000000000000000000000
      000000FF0000C6C6C60000FF0000C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000309A5004260
      EC00A9BBFF00BDCAFF008EA5FE006483FD005073FC004A6EFD003961FD001444
      F900042CD7000109A20000000000000000000000000000000000000000000000
      0000C6C6C60000FF0000C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000004
      A0001E32CD005876F600859EFE008BA3FF007994FE005376FC00234AF000051E
      C50001049C000000000000000000000000000000000000000000000000000000
      000000FF0000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000004A0000917B6001022C3000D1FC2000311B40001059F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00F81FFFFF00000000E007F3FF00000000
      C003F1FF000000008001F0FF000000008001F07F000000000000F03F00000000
      0000F01F000000000000F00F000000000000F00F000000000000F01F00000000
      0000F03F000000008001F07F000000008001F0FF00000000C003F1FF00000000
      E007F3FF00000000F81FFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object ActionList: TActionList
    Images = ImageList
    Left = 8
    Top = 128
    object ActionHelpHelp: TAction
      Category = 'Help'
      Caption = 'Help'
      Hint = 'Help|COR help'
      ShortCut = 112
      OnExecute = ActionHelpHelpExecute
    end
    object ActionHelpHowTo: TAction
      Category = 'Help'
      Caption = 'How To...'
      Hint = 'How To|How to...'
      OnExecute = ActionHelpHowToExecute
    end
    object ActionHelpFAQ: TAction
      Category = 'Help'
      Caption = 'FAQ'
      Hint = 'FAQ|FAQ'
      OnExecute = ActionHelpFAQExecute
    end
    object ActionHelpHomePage: TAction
      Category = 'Help'
      Caption = 'Home Page'
      Hint = 'Home Page|Look up the COR home page'
      OnExecute = ActionHelpHomePageExecute
    end
    object ActionViewFileToolBar: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'File'
      Checked = True
      Hint = 'File|Show or hide the file toolbar'
      OnExecute = ActionViewToolBarsExecute
      OnUpdate = ActionViewToolBarsUpdate
    end
    object ActionViewRunToolBar: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Run'
      Checked = True
      Hint = 'Run|Show or hide the run toolbar'
      OnExecute = ActionViewToolBarsExecute
      OnUpdate = ActionViewToolBarsUpdate
    end
    object ActionFileExit: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Exit|Exit COR'
      ImageIndex = 0
      ShortCut = 32883
    end
    object ActionRunRun: TAction
      Category = 'Run'
      Caption = 'Run'
      Hint = 'Run|Run the current file'
      ImageIndex = 1
      ShortCut = 120
    end
    object ActionHelpUpdate: TAction
      Category = 'Help'
      Caption = 'Update...'
      Hint = 
        'Update|Update COR with the latest version available from the COR' +
        ' web site'
      OnExecute = ActionHelpUpdateExecute
      OnUpdate = ActionHelpUpdateUpdate
    end
    object ActionHelpCellMLHomePage: TAction
      Category = 'Help'
      Caption = 'CellML Home Page'
      Hint = 'CellML Home Page|Look up the CellML home page'
      OnExecute = ActionHelpCellMLHomePageExecute
    end
    object ActionHelpCellMLRepository: TAction
      Category = 'Help'
      Caption = 'CellML Repository'
      Hint = 'CellML Repository|Look up the CellML repository'
      OnExecute = ActionHelpCellMLRepositoryExecute
    end
    object ActionHelpAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      Hint = 
        'About|Some general information about COR, CellML, third party co' +
        'mponents and the system'
      OnExecute = ActionHelpAboutExecute
    end
    object ActionToolsOptions: TAction
      Category = 'Tools'
      Caption = 'Options...'
      Hint = 'Options|Options for COR'
      OnExecute = ActionToolsOptionsExecute
    end
    object ActionToolsResetAll: TAction
      Category = 'Tools'
      Caption = 'Reset All'
      Hint = 'Reset All|Reset all the settings and options'
      OnExecute = ActionToolsResetAllExecute
    end
  end
  object ControlBarPopupMenu: TPopupMenu
    Left = 8
    Top = 160
    object ControlBarPopupMenuFileMenuItem: TMenuItem
      Action = ActionViewFileToolBar
      AutoCheck = True
    end
    object ControlBarPopupMenuRunMenuItem: TMenuItem
      Action = ActionViewRunToolBar
      AutoCheck = True
    end
  end
end
