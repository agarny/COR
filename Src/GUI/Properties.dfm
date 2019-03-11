inherited PropertiesFrame: TPropertiesFrame
  Tag = 2
  Width = 288
  Height = 208
  ExplicitWidth = 288
  ExplicitHeight = 208
  object RootPanel: TPanel
    Left = 0
    Top = 0
    Width = 288
    Height = 208
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    object VirtualStringTree: TVirtualStringTree
      Left = 1
      Top = 1
      Width = 286
      Height = 206
      Align = alClient
      BorderStyle = bsNone
      Header.AutoSizeIndex = 0
      Header.DefaultHeight = 17
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoColumnResize, hoDblClickResize, hoShowSortGlyphs, hoVisible]
      HintMode = hmHint
      Margin = 0
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoDeleteMovedNodes]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toFullRowSelect]
      OnBeforeCellPaint = VirtualStringTreeBeforeCellPaint
      OnChange = VirtualStringTreeChange
      OnCollapsed = VirtualStringTreeCollapsedOrExpanded
      OnCreateEditor = VirtualStringTreeCreateEditor
      OnEdited = VirtualStringTreeEdited
      OnEditing = VirtualStringTreeEditing
      OnExpanded = VirtualStringTreeCollapsedOrExpanded
      OnGetText = VirtualStringTreeGetText
      OnPaintText = VirtualStringTreePaintText
      OnGetNodeDataSize = VirtualStringTreeGetNodeDataSize
      OnKeyDown = VirtualStringTreeKeyDown
      OnMouseDown = VirtualStringTreeMouseDown
      OnMouseMove = VirtualStringTreeMouseMove
      OnMouseUp = VirtualStringTreeMouseUp
      OnResize = VirtualStringTreeResize
      Columns = <
        item
          Color = cl3DLight
          MinWidth = 0
          Options = [coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
          Position = 0
          Width = 169
          WideText = 'Properties'
        end
        item
          MinWidth = 0
          Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
          Position = 1
          Width = 69
          WideText = 'Values'
        end
        item
          MinWidth = 0
          Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
          Position = 2
          Width = 69
          WideText = 'Units'
        end>
    end
  end
end
