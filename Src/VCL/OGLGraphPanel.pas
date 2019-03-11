//==============================================================================
// OpenGL graph component
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 24/10/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================
// TOGLGPBase (Owner)
//  |
//  +--- TOGLGPSpecs (Enabled)
//  |     |
//  |     +--- TOGLGPSpecsColor (Color, Alpha)
//  |     |     |
//  |     |     +--- TOGLGPSpecsSize (Size)
//  |     |           |
//  |     |           +--- TOGLGPSpecsLine (Style)
//  |     |
//  |     +--- TOGLGPSpecsCoords (Inverted)
//  |
//  +--- TOGLGPVisual (LineSpecs)
//        |
//        +--- TOGLGPVisualCoords (CoordsSpecs)
//        |     |
//        |     +--- TOGLGPVisualCoordsEnhanced [DataPts, ComputingDataPts]
//        |     |     |
//        |     |     +--- TOGLGPAxis (Min, Max, Start, Length, Grid,
//        |     |     |                OnTheMinEdge, OnTheMaxEdge, ZoomFactor,
//        |     |     |                CanZoomIn, CanZoomOut)
//        |     |     |
//        |     |     +--- TOGLGPGrid (Interval, IntervalType)
//        |     |
//        |     +--- TOGLGPCoordinates [Active]
//        |           |
//        |           +--- TOGLGPCoords (X, Y, Snap, BothAxes)
//        |           |
//        |           +--- TOGLGP2Coords (X1, Y1, X2, Y2)
//        |                 |
//        |                 +--- TOGLGPDims (Snap)
//        |                 |
//        |                 +--- TOGLGPRegion (FillingSpecs)
//        |
//        +--- TOGLGPGraph (Name, Data, YMin, YMax)
//
// TOGLGraphPanel
//==============================================================================
// Note #1: ideally one would use "glFloat" instead of "Double", but the fact is
//          that there are occasions where a double precision is required, so...
// Note #2: we don't check for circular references (regarding "ConnectedTo"),
//          e.g. 1->2->3->2...
//==============================================================================

Unit OGLGraphPanel;

//==============================================================================

Interface

//==============================================================================

Uses
   Windows, SysUtils, Graphics, Classes, Controls, OGL, ExtCtrls;

//==============================================================================

{$M+}

//==============================================================================

Const
   ZOOM_IN_FACTOR = 0.5;
   ZOOM_OUT_FACTOR = 2;

//==============================================================================

Type
   TArrayOfTOGLGPData = Array Of Double;
   TOGLGPDataPt = Record
      X: Double;
      Y: Double;
   End;
   TArrayOfTOGLGPDataPts = Array Of TOGLGPDataPt;
   TOGLGraphPanel = Class;
   TOGLGPBase = Class(TPersistent)
      Protected
         // Properties used for internal purposes

         Owner: TOGLGraphPanel;   // Direct access to the OGL graph panel

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel);
   End;
   TOGLGPSpecs = Class(TOGLGPBase)
      Private
         // Methods to modify the different published properties

         Procedure SetEnabled(Const aValue: Boolean); Inline;

      Protected
         // Private representation of published properties

         FEnabled: Boolean;   // Enable the object or not

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel);

      Published
         // Published properties

         Property Enabled: Boolean Read FEnabled Write SetEnabled Default True;
   End;
   TOGLGPSpecsColor = Class(TOGLGPSpecs)
      Private
         // Methods to modify the different published properties

         Procedure SetColor(Const aValue: TColor); Inline;
         Procedure SetAlpha(Const aValue: Single); Inline;

      Protected
         // Private representation of published properties

         FColor: TColor;   // Colour of the object
         FAlpha: Single;   // Alpha component

         // Methods used for internal purposes

         Procedure DefineProperties(aFiler: TFiler); Override;

         Procedure ReadColor(aReader: TReader); Inline;
         Procedure WriteColor(aWriter: TWriter); Inline;

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel);

      Published
         // Published properties

         Property Color: TColor Read FColor Write SetColor;
         Property Alpha: Single Read FAlpha Write SetAlpha;
   End;
   TOGLGPSpecsSize = Class(TOGLGPSpecsColor)
      Private
         // Methods to modify the different published properties

         Procedure SetSize(Const aValue: Byte); Inline;

      Protected
         // Private representation of published properties

         FSize: Byte;   // Size of the object

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel);

      Published
         // Published properties

         Property Size: Byte Read FSize Write SetSize Default 1;
   End;
   TOGLGPSpecsLine = Class(TOGLGPSpecsSize)
      Private
         // Methods to modify the different published properties

         Procedure SetStyle(Const aValue: TPenStyle); Inline;

      Protected
         // Private representation of published properties

         FStyle: TPenStyle;   // Style of the line

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel);

      Published
         // Published properties

         Property Style: TPenStyle Read FStyle Write SetStyle;
   End;
   TOGLGPSpecsCoords = Class(TOGLGPSpecs)
      Private
         // Methods to modify the different published properties

         Procedure SetInverted(Const aValue: Boolean); Inline;

      Protected
         // Private representation of published properties

         FInverted: Boolean;   

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel);

      Published
         // Published properties

         Property Inverted: Boolean Read FInverted Write SetInverted Default True;
   End;
   TOGLGPVisual = Class(TOGLGPBase)
      Private
         // Methods to modify the different published properties

         Procedure SetLineSpecs(Const aValue: TOGLGPSpecsLine); Inline;

      Protected
         // Private representation of published properties

         FLineSpecs: TOGLGPSpecsLine;   // Specifications for lines

         // Methods used for internal purposes

         Procedure Paint; Virtual; Abstract;

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel);
         Destructor Destroy; Override;

      Published
         // Published properties

         Property LineSpecs: TOGLGPSpecsLine Read FLineSpecs Write SetLineSpecs;
   End;
   TOGLGPVisualCoords = Class(TOGLGPVisual)
      Private
         // Methods to modify the different published properties

         Procedure SetCoordsSpecs(Const aValue: TOGLGPSpecsCoords); Inline;

      Protected
         // Private representation of published properties

         FCoordsSpecs: TOGLGPSpecsCoords;   // Specifications for coordinates

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel);
         Destructor Destroy; Override;

      Published
         // Published properties

         Property CoordsSpecs: TOGLGPSpecsCoords Read FCoordsSpecs Write SetCoordsSpecs;
   End;
   TOGLGPVisualCoordsEnhanced = Class(TOGLGPVisualCoords)
      Protected
         // Properties used for internal purposes

         DataPts: TArrayOfTOGLGPDataPts;

         ComputingDataPts: Boolean;

         // Methods used for internal purposes

         Procedure ComputePts; Virtual; Abstract;

         Procedure Paint; Override;

         Procedure PaintCoordinates; Virtual; Abstract;
   End;
   TOGLGPGrid = Class;
   TOGLGPAxisType = (atX, atY);
   TOGLGPAxis = Class(TOGLGPVisualCoordsEnhanced)
      Private
         // Properties used for internal purposes

         AxisType: TOGLGPAxisType;   // Type of axis

         // Methods to modify the different published properties

         Function GetOnTheMinEdge: Boolean; Inline;
         Function GetOnTheMaxEdge: Boolean; Inline;

         Function GetCanZoomIn: Boolean; Inline;
         Function GetCanZoomOut: Boolean; Inline;

         Procedure SetMin(Const aValue: Double); Inline;
         Procedure SetMax(Const aValue: Double); Inline;

         Procedure SetStart(Const aValue: Double); Inline;
         Procedure SetLength(Const aValue: Double); Inline;

         Procedure SetGrid(Const aValue: TOGLGPGrid); Inline;

         // Methods used for internal purposes

         Procedure CheckData(Const aComputePtsAndRepaint: Boolean = True);

      Protected
         // Private representation of published properties

         FMin: Double;   // Minimum value
         FMax: Double;   // Maximum value

         FStart: Double;    // Where to start
         FLength: Double;   // For how long to last

         FZoomFactor: Double;   // Zoom factor

         FGrid: TOGLGPGrid;   // Axis grid associate to this axis

         // Methods used for internal purposes

         Procedure DefineProperties(aFiler: TFiler); Override;

         Procedure ReadMin(aReader: TReader); Inline;
         Procedure WriteMin(aWriter: TWriter); Inline;

         Procedure ReadMax(aReader: TReader); Inline;
         Procedure WriteMax(aWriter: TWriter); Inline;

         Procedure ReadStart(aReader: TReader); Inline;
         Procedure WriteStart(aWriter: TWriter); Inline;

         Procedure ReadLength(aReader: TReader); Inline;
         Procedure WriteLength(aWriter: TWriter); Inline;

         Procedure ComputePts; Override;

         Procedure PaintCoordinates; Override;

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel; Const aAxisType: TOGLGPAxisType);
         Destructor Destroy; Override;

      Published
         // Published properties

         Property Min: Double Read FMin Write SetMin;
         Property Max: Double Read FMax Write SetMax;

         Property Start: Double Read FStart Write SetStart;
         Property Length: Double Read FLength Write SetLength;

         Property Grid: TOGLGPGrid Read FGrid Write SetGrid;

         Property OnTheMinEdge: Boolean Read GetOnTheMinEdge;
         Property OnTheMaxEdge: Boolean Read GetOnTheMaxEdge;

         Property ZoomFactor: Double Read FZoomFactor;

         Property CanZoomIn: Boolean Read GetCanZoomIn;
         Property CanZoomOut: Boolean Read GetCanZoomOut;
   End;
   TOGLGPGridIntervalType = (itAutomatic, itFixed);
   TOGLGPGrid = Class(TOGLGPVisualCoordsEnhanced)
      Private
         // Properties used for internal purposes

         Axis: TOGLGPAxis;   // Axis that holds this grid

         // Methods to modify the different published properties

         Procedure SetInterval(Const aValue: Double); Inline;
         Procedure SetIntervalType(Const aValue: TOGLGPGridIntervalType); Inline;

         // Methods used for internal purposes

         Procedure ComputeGridLines(Const aFirst, aLast, aInterval, aMin, aMax: Double);

      Protected
         // Private representation of published properties

         FInterval: Double;
         FIntervalType: TOGLGPGridIntervalType;

         // Methods used for internal purposes

         Procedure ComputePts; Override;

         Procedure Paint; Override;
         Procedure PaintCoordinates; Override;

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel; aAxis: TOGLGPAxis);

      Published
         // Published properties

         Property Interval: Double Read FInterval Write SetInterval;
         Property IntervalType: TOGLGPGridIntervalType Read FIntervalType Write SetIntervalType Default itFixed;
   End;
   TOGLGPCoordinates = Class(TOGLGPVisualCoords)
      Protected
         // Properties used for internal purposes

         Active: Boolean;

         // Methods used for internal purposes

         Procedure PaintCoordinate(aBackgroundSpecs: TOGLGPSpecsSize; aCoordsSpecs: TOGLGPSpecsCoords; Const aX, aY: Double; Const aTopLeft: Boolean);
   End;
   TOGLGPCoords = Class(TOGLGPCoordinates)
      Protected
         // Private representation of published properties

         FX, FY: Double;

         FSnap: Boolean;

         FBothAxes: Boolean;

         // Methods used for internal purposes

         Procedure Paint; Override;

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel);

      Published
         // Published properties

         Property X: Double Read FX Write FX Stored False;
         Property Y: Double Read FY Write FY Stored False;

         Property Snap: Boolean Read FSnap Write FSnap Stored False;

         Property BothAxes: Boolean Read FBothAxes Write FBothAxes Stored True;
   End;
   TOGLGP2Coords = Class(TOGLGPCoordinates)
      Protected
         // Private representation of published properties

         FX1, FY1: Double;
         FX2, FY2: Double;

      Published
         // Published properties

         Property X1: Double Read FX1 Write FX1 Stored False;
         Property Y1: Double Read FY1 Write FY1 Stored False;

         Property X2: Double Read FX2 Write FX2 Stored False;
         Property Y2: Double Read FY2 Write FY2 Stored False;
   End;
   TOGLGPDims = Class(TOGLGP2Coords)
      Protected
         // Private representation of published properties

         FSnap: Boolean;

         // Methods used for internal purposes

         Procedure PaintDimension(aBackgroundSpecs: TOGLGPSpecsSize; Const aX, aY: Double; Const aDim: Double; Const aXDim, aTopLeft: Boolean);

         Procedure Paint; Override;

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel);

      Published
         // Published properties

         Property Snap: Boolean Read FSnap Write FSnap Stored False;
   End;
   TOGLGPRegion = Class(TOGLGP2Coords)
      Private
         // Methods to modify the different published properties

         Procedure SetFillingSpecs(Const aValue: TOGLGPSpecsColor); Inline;

      Protected
         // Private representation of published properties

         FFillingSpecs: TOGLGPSpecsColor;

         // Methods used for internal purposes

         Procedure Paint; Override;

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel);
         Destructor Destroy; Override;

      Published
         // Published properties

         Property FillingSpecs: TOGLGPSpecsColor Read FFillingSpecs Write SetFillingSpecs;
   End;
   TOGLGPGraph = Class(TOGLGPVisual)
      Private
         // Properties used for internal purposes

         Data: TArrayOfTOGLGPData;

         DataSize, DataIter: Integer;

         YMinAndYMax: Boolean;

         // Methods to modify the different published properties

         Procedure SetQuickPaintEnabled(Const aValue: Boolean);

         // Methods used for internal purposes

         Procedure StartAndEndPts(Var aStartIter, aEndIter: Integer);

         Procedure PaintLines(Const aStartIter, aEndIter: Integer; Const aQuickPaint: Boolean);

         Function NeedQuickPaint(Var aStartIter, aEndIter: Integer): Boolean; Overload; Inline;
         Function NeedQuickPaint: Boolean; Overload; Inline;
         Function QuickPaint: Integer;

         Function IsValid: Boolean; Inline;

      Protected
         // Private representation of published properties

         FName: String;   // Unique identifier for the graph
         FLegend: String;

         FQuickPaintEnabled: Boolean;

         FShift: Integer;

         FXRes, FOne_XRes: Double;

         FYMin, FYMax: Double;

         // Methods used for internal purposes

         Procedure Paint; Override;

      Public
         // Constructor & Destructor

         Constructor Create(aOGLGraphPanel: TOGLGraphPanel; Const aName, aLegend: String; Const aShift: Integer; Const aLength, aXRes: Double);

         // User's methods

         Procedure AddPt(Const aY: Double);

      Published
         // Published properties

         Property Name: String Read FName;
         Property Legend: String Read FLegend Write FLegend;

         Property QuickPaintEnabled: Boolean Read FQuickPaintEnabled Write SetQuickPaintEnabled;

         Property Shift: Integer Read FShift;

         Property XRes: Double Read FXRes;
         Property One_XRes: Double Read FOne_XRes;

         Property YMin: Double Read FYMin;
         Property YMax: Double Read FYMax;
   End;
   TArrayOfOGLGPGraph = Array Of TOGLGPGraph;
   TOGLGraphPanel = Class(TOGL)
      Private
         // Properties used for internal purposes

         MouseCaptured: Boolean;

         XOrigPt, YOrigPt: Integer;

         TransXVal, TransYVal: Integer;
         ZoomXVal, ZoomYVal: Integer;
         Pt1XVal, Pt1YVal, Pt2XVal, Pt2YVal: Integer;
         CoordsXVal, CoordsYVal: Integer;

         PanningTimer: TTimer;
         ZoomingTimer: TTimer;

         IsPanning: Boolean;
         IsZooming: Boolean;
         IsShowingCoords: Boolean;
         IsShowingDims: Boolean;
         IsZoomingRegion: Boolean;
         IsPopupMenuing: Boolean;

         QuickGraphsList: TArrayOfOGLGPGraph;

         QuickGraphsListSize: Integer;

         Leader: TOGLGraphPanel;

         NeedUpdateYMinAndYMax: Boolean;

         ShowPopupMenu: Boolean;

         OldAutoPopup: Boolean;

         One_Width: Double;
         One_Height: Double;

         ConvOpenGLXToWinXCst: Double;

         ConvOpenGLYToWinYCst1: Double;
         ConvOpenGLYToWinYCst2: Double;

         // Methods to modify the different published properties

         Function GetConnectedTo: TOGLGraphPanel; Inline;

         Function GetYMin: Double; Inline;
         Function GetYMax: Double; Inline;

         Procedure SetConnectedTo(Const aValue: TOGLGraphPanel); Inline;

         Procedure SetXAxis(Const aValue: TOGLGPAxis); Inline;
         Procedure SetYAxis(Const aValue: TOGLGPAxis); Inline;

         Procedure SetCoords(Const aValue: TOGLGPCoords); Inline;
         Procedure SetDims(Const aValue: TOGLGPDims); Inline;
         Procedure SetRegion(Const aValue: TOGLGPRegion); Inline;

         Procedure SetAutoYRange(Const aValue: Boolean); Inline;

         // Methods used for internal purposes

         Procedure UpdateLeader(Const aLeader: TOGLGraphPanel);

         Procedure MouseDownCoords(aOGLGraphPanel: TOGLGraphPanel; aShift: TShiftState);
         Procedure MouseUpCoords(aOGLGraphPanel: TOGLGraphPanel; aShift: TShiftState);

         Procedure InitPt1AndPt2; Inline;
         Procedure UpdatePt2(Const aX, aY: Integer); Inline;

         Procedure ComputePtsAndRepaint(Const aRepaint: Boolean = True); Inline;

         Procedure Pan;

         Procedure PanningAutomatically(aSender: TObject); Inline;

         Function ConvWinXToOpenGLX(Const aWinX: Double): Double; Inline;
         Function ConvWinYToOpenGLY(Const aWinY: Double): Double; Inline;

         Function ConvWinXSizeToOpenGLXSize(Const aWinXSize: Double): Double; Inline;
         Function ConvWinYSizeToOpenGLYSize(Const aWinYSize: Double): Double; Inline;

         Function ConvWinTextWidthToOpenGLTextWidth(Const aString: String): Double; Inline;
         Function ConvWinTextHeightToOpenGLTextHeight(Const aString: String): Double; Inline;

         Procedure UpdateGPCsts;

         Function ConvOpenGLXToWinX(Const aOpenGLX: Double): Integer; Inline;
         Function ConvOpenGLYToWinY(Const aOpenGLY: Double): Integer; Inline;

         Function ConvOpenGLXSizeToWinXSize(Const aOpenGLXSize: Double): Integer; Inline;
         Function ConvOpenGLYSizeToWinYSize(Const aOpenGLYSize: Double): Integer; Inline;

         Procedure UpdateYMinAndYMax;

         Function GetYMin2(Const aOptimise: Boolean): Double;
         Function GetYMax2(Const aOptimise: Boolean): Double;

         Procedure ShowCoords;
         Procedure EndShowCoords;

         Procedure ShowDims;
         Procedure EndShowDims;

         Procedure ZoomRegion;
         Function EndZoomRegion: Boolean;

         Procedure Zoom;
         Function ZoomAxis(Const aAxis: TOGLGPAxis; Var aZoomVal: Integer): Boolean;

         Procedure ZoomingAutomatically(aSender: TObject); Inline;

         Function NeedUpdateYAxis: Boolean;
         Function UpdateYAxis: Boolean;

         Procedure PaintStr(aColorSpecs: TOGLGPSpecsSize; aCoordsSpecs: TOGLGPSpecsCoords; Const aX, aY: Double; Const aStr: String; Const aXShift, aYShift: Double);

         Function ClosestYGraph(Const aX, aY: Integer; Var aOGLGraphIter: Integer): Double;

         Function ClosestY(Const aX, aY: Integer; Var aOGLGraphIter: Integer): Double; Overload;
         Function ClosestY(Const aX, aY: Integer): Double; Overload; Inline;

      Protected
         // Private representation of published properties

         FConnectedTo: TOGLGraphPanel;

         FLegend: String;

         FXAxis: TOGLGPAxis;
         FYAxis: TOGLGPAxis;

         FGraphsList: TArrayOfOGLGPGraph;

         FGraphsListSize: Integer;

         FCoords: TOGLGPCoords;
         FDims: TOGLGPDims;
         FRegion: TOGLGPRegion;

         FAutoYRange: Boolean;

         FYMin, FYMax: Double;

         // Methods handling different messages

         Procedure GLInit(aSender: TObject);
         Procedure GLPaint(aSender: TObject);
         Procedure GLResize(aSender: TObject); Inline;

         Procedure MouseDown(aButton: TMouseButton; aShift: TShiftState; aX, aY: Integer); Override;
         Procedure MouseMove(aShift: TShiftState; aX, aY: Integer); Override;
         Procedure MouseUp(aButton: TMouseButton; aShift: TShiftState; aX, aY: Integer); Override;

      Public
         // Constructor & Destructor

         Constructor Create(aOwner: TComponent); Override;
         Destructor Destroy; Override;

         // User's methods

         Function AddGraph(Const aName, aLegend: String; Const aShift: Integer; Const aLength, aXRes: Double): TOGLGPGraph;

         Procedure RemoveGraph(Const aName: String);
         Procedure RemoveAllGraphs;

         Function NbOfGraphs: Integer; Inline;
         Function NbOfValidGraphs: Integer;

         Function NeedQuickPaint: Boolean;
         Procedure QuickPaint;
         Procedure FinaliseQuickPaint; Inline;

         Function HasQuickGraphs: Boolean; Inline;

         Class Procedure ExportToCSV(Const aOGLGraphPanels: Array Of TOGLGraphPanel; Const aFileName: String);

      Published
         // Published properties

         Property ConnectedTo: TOGLGraphPanel Read GetConnectedTo Write SetConnectedTo;

         Property Legend: String Read FLegend Write FLegend;

         Property XAxis: TOGLGPAxis Read FXAxis Write SetXAxis;
         Property YAxis: TOGLGPAxis Read FYAxis Write SetYAxis;

         Property GraphsList: TArrayOfOGLGPGraph Read FGraphsList;
         Property GraphsListSize: Integer Read FGraphsListSize;

         Property Coords: TOGLGPCoords Read FCoords Write SetCoords;
         Property Dims: TOGLGPDims Read FDims Write SetDims;
         Property Region: TOGLGPRegion Read FRegion Write SetRegion;

         Property YMin: Double Read GetYMin;
         Property YMax: Double Read GetYMax;

         Property AutoYRange: Boolean Read FAutoYRange Write SetAutoYRange;
   End;

//==============================================================================

Function FindOGLPtIter(Const aData: TArrayOfTOGLGPData;
                       Const aDataSize, aShift: Integer;
                       Const aX, aOne_XRes: Double;
                       Const aPrev: Boolean): Integer;

//==============================================================================

Procedure Register;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF OPT_MATH}
   OptMath,
{$ENDIF}
   Math, DeCAL, Dialogs, Clipbrd, OpenGL_SG;

//==============================================================================

Type
   TGraphData = Class
      Public
         // Published properties

         Exported: Boolean;

         Legend: String;

         Shift: Integer;
         XRes: Double;
         Iter: Integer;
         Data: TArrayOfTOGLGPData;
         Size: Integer;

         // Constructor & Destructor

         Constructor Create(Const aLegend: String; Const aShift: Integer; Const aXRes: Double; Const aData: TArrayOfTOGLGPData; Const aDataSize: Integer);
   End;

//==============================================================================

Function FindOGLPtIter(Const aData: TArrayOfTOGLGPData;
                       Const aDataSize, aShift: Integer;
                       Const aX, aOne_XRes: Double;
                       Const aPrev: Boolean): Integer;
   Function Compare(Const aX1, aX2: Double): Integer; Inline;
   Begin
      If (aX1 < aX2) Then
         Result := -1
      Else If (aX1 > aX2) Then
         Result := 1
      Else
         Result := 0;
   End;
Var
   LastPtIndex: Integer;
   ResDbl: Double;
   ResLow, ResHigh: Integer;
Begin
   // Note: this algorithm is optimised for the case where the first data point
   //       of a data set has an abscissa of 0 and subsequent data points have
   //       an abscissa that is a multiple of "TOGLGraphPanel.XRes"

   LastPtIndex := aDataSize-1;

   ResDbl := aX*aOne_XRes;

{$IFDEF OPT_MATH}
   ResLow  := OptFloor(ResDbl)-aShift;
{$ELSE}
   ResLow  := Floor(ResDbl)-aShift;
{$ENDIF}
   ResHigh := ResLow+1;

   If (ResDbl = ResLow) Then
      Result := ResLow
   Else Begin
      If (aPrev) Then
         Result := ResLow
      Else
         Result := ResHigh;
   End;

   If ((Result < 0) Or (Result > LastPtIndex)) Then
      Result := -1;
End;

//==============================================================================

Constructor TOGLGPBase.Create(aOGLGraphPanel: TOGLGraphPanel);
Begin
   Owner := aOGLGraphPanel;
End;

//==============================================================================

Constructor TOGLGPSpecs.Create(aOGLGraphPanel: TOGLGraphPanel);
Begin
   Inherited;

   FEnabled := True;
End;

//==============================================================================

Procedure TOGLGPSpecs.SetEnabled(Const aValue: Boolean);
Begin
   If (aValue <> FEnabled) Then Begin
      FEnabled := aValue;

      Owner.ForceOGLRepaint;
   End;
End;

//==============================================================================

Constructor TOGLGPSpecsColor.Create(aOGLGraphPanel: TOGLGraphPanel);
Begin
   Inherited;

   FColor := clBlack;
   FAlpha := 1;
End;

//==============================================================================

Procedure TOGLGPSpecsColor.DefineProperties(aFiler: TFiler);
Begin
   Inherited;

   aFiler.DefineProperty('Color', ReadColor, WriteColor, True);
End;

//==============================================================================

Procedure TOGLGPSpecsColor.ReadColor(aReader: TReader);
Begin
   Color := aReader.ReadInteger;
End;

//==============================================================================

Procedure TOGLGPSpecsColor.WriteColor(aWriter: TWriter);
Begin
   aWriter.WriteInteger(Color);
End;

//==============================================================================

Procedure TOGLGPSpecsColor.SetColor(Const aValue: TColor);
Begin
   If (aValue <> FColor) Then Begin
      FColor := aValue;

      Owner.ForceOGLRepaint;
   End;
End;

//==============================================================================

Procedure TOGLGPSpecsColor.SetAlpha(Const aValue: Single);
Begin
   If ((aValue >= 0) And (aValue <= 1) And (aValue <> FAlpha)) Then Begin
      FAlpha := aValue;

      Owner.ForceOGLRepaint;
   End;
End;

//==============================================================================

Constructor TOGLGPSpecsSize.Create(aOGLGraphPanel: TOGLGraphPanel);
Begin
   Inherited;

   FSize := 1;
End;

//==============================================================================

Procedure TOGLGPSpecsSize.SetSize(Const aValue: Byte);
Begin
   If ((aValue > 0) And (aValue <> FSize)) Then Begin
      FSize := aValue;

      Owner.ForceOGLRepaint;
   End;
End;

//==============================================================================

Constructor TOGLGPSpecsLine.Create(aOGLGraphPanel: TOGLGraphPanel);
Begin
   Inherited;

   FStyle := psSolid;
End;

//==============================================================================

Procedure TOGLGPSpecsLine.SetStyle(Const aValue: TPenStyle);
Begin
   If (aValue <> FStyle) Then Begin
      FStyle := aValue;

      Owner.ForceOGLRepaint;
   End;
End;

//==============================================================================

Constructor TOGLGPSpecsCoords.Create(aOGLGraphPanel: TOGLGraphPanel);
Begin
   Inherited;

   FInverted := True;
End;

//==============================================================================

Procedure TOGLGPSpecsCoords.SetInverted(Const aValue: Boolean);
Begin
   If (aValue <> FInverted) Then Begin
      FInverted := aValue;

      Owner.ForceOGLRepaint;
   End;
End;

//==============================================================================

Constructor TOGLGPVisual.Create(aOGLGraphPanel: TOGLGraphPanel);
Begin
   Inherited;

   FLineSpecs := TOGLGPSpecsLine.Create(aOGLGraphPanel);
End;

//==============================================================================

Destructor TOGLGPVisual.Destroy;
Begin
   FLineSpecs.Free;
End;

//==============================================================================

Procedure TOGLGPVisual.SetLineSpecs(Const aValue: TOGLGPSpecsLine);
Begin
   FLineSpecs.Assign(aValue);
End;

//==============================================================================

Constructor TOGLGPVisualCoords.Create(aOGLGraphPanel: TOGLGraphPanel);
Begin
   Inherited;

   FCoordsSpecs := TOGLGPSpecsCoords.Create(aOGLGraphPanel);
End;

//==============================================================================

Destructor TOGLGPVisualCoords.Destroy;
Begin
   FCoordsSpecs.Free;

   Inherited;
End;

//==============================================================================

Procedure TOGLGPVisualCoords.SetCoordsSpecs(Const aValue: TOGLGPSpecsCoords);
Begin
   FCoordsSpecs.Assign(aValue);
End;

//==============================================================================

Procedure TOGLGPVisualCoordsEnhanced.Paint;
Var
   Iter: Integer;
Begin
   If (FLineSpecs.Enabled) Then Begin
      If (Owner.WinAPI) Then Begin
         Owner.BufferCanvas.Pen.Color := FLineSpecs.Color;
         Owner.BufferCanvas.Pen.Width := FLineSpecs.Size;
         Owner.BufferCanvas.Pen.Style := FLineSpecs.Style;

         Iter := 0;

         While Iter <> Length(DataPts) Do Begin
            With DataPts[Iter] Do
               Owner.BufferCanvas.MoveTo(Owner.ConvOpenGLXToWinX(X), Owner.ConvOpenGLYToWinY(Y));

            With DataPts[Iter+1] Do
               Owner.BufferCanvas.LineTo(Owner.ConvOpenGLXToWinX(X), Owner.ConvOpenGLYToWinY(Y));

            Inc(Iter, 2);
         End;
      End Else Begin
         OGLColor(FLineSpecs.Color, FLineSpecs.Alpha);
         OGLLineWidth(FLineSpecs.Size);
         OGLLineStyle(FLineSpecs.Style);
         OGLLineSmooth(False);

         glBegin(GL_LINES);
            Iter := 0;

            While Iter <> Length(DataPts) Do Begin
               With DataPts[Iter] Do
                  glVertex2f(X, Y);

               With DataPts[Iter+1] Do
                  glVertex2f(X, Y);

               Inc(Iter, 2);
            End;
         glEnd;
      End;
   End;
End;

//==============================================================================

Constructor TOGLGPAxis.Create(aOGLGraphPanel: TOGLGraphPanel;
                              Const aAxisType: TOGLGPAxisType);
Begin
   Inherited Create(aOGLGraphPanel);

   AxisType := aAxisType;

   FMin := -10;
   FMax := 10;

   FStart  := -5;
   FLength := 10;

   FGrid := TOGLGPGrid.Create(aOGLGraphPanel, Self);

   FLineSpecs.Size := 2;

   System.SetLength(DataPts, 2);

   Owner.UpdateGPCsts;
End;

//==============================================================================

Destructor TOGLGPAxis.Destroy;
Begin
   FGrid.Free;

   Inherited;
End;

//==============================================================================

Procedure TOGLGPAxis.DefineProperties(aFiler: TFiler);
Begin
   Inherited;

   aFiler.DefineProperty('Min', ReadMin, WriteMin, True);
   aFiler.DefineProperty('Max', ReadMax, WriteMax, True);
   aFiler.DefineProperty('Start', ReadStart, WriteStart, True);
   aFiler.DefineProperty('Length', ReadLength, WriteLength, True);
End;

//==============================================================================

Procedure TOGLGPAxis.ReadMin(aReader: TReader);
Begin
   Min := aReader.ReadFloat;
End;

//==============================================================================

Procedure TOGLGPAxis.WriteMin(aWriter: TWriter);
Begin
   aWriter.WriteFloat(Min);
End;

//==============================================================================

Procedure TOGLGPAxis.ReadMax(aReader: TReader);
Begin
   Max := aReader.ReadFloat;
End;

//==============================================================================

Procedure TOGLGPAxis.WriteMax(aWriter: TWriter);
Begin
   aWriter.WriteFloat(Max);
End;

//==============================================================================

Procedure TOGLGPAxis.ReadStart(aReader: TReader);
Begin
   Start := aReader.ReadFloat;
End;

//==============================================================================

Procedure TOGLGPAxis.WriteStart(aWriter: TWriter);
Begin
   aWriter.WriteFloat(Start);
End;

//==============================================================================

Procedure TOGLGPAxis.ReadLength(aReader: TReader);
Begin
   Length := aReader.ReadFloat;

   Owner.UpdateGPCsts;
End;

//==============================================================================

Procedure TOGLGPAxis.WriteLength(aWriter: TWriter);
Begin
   aWriter.WriteFloat(Length);
End;

//==============================================================================

Function TOGLGPAxis.GetOnTheMinEdge: Boolean;
Begin
   Result := (FStart = FMin);
End;

//==============================================================================

Function TOGLGPAxis.GetOnTheMaxEdge: Boolean;
Begin
   Result := ((FStart+FLength) = FMax);
End;

//==============================================================================

Function TOGLGPAxis.GetCanZoomIn: Boolean;
Begin
   Result := FZoomFactor < 1024;
End;

//==============================================================================

Function TOGLGPAxis.GetCanZoomOut: Boolean;
Begin
   Result := FZoomFactor > 1;
End;

//==============================================================================

Procedure TOGLGPAxis.SetMin(Const aValue: Double);
Begin
   If ((aValue <> FMin) And (aValue < FMax)) Then Begin
      FMin := aValue;

      CheckData;
   End;
End;

//==============================================================================

Procedure TOGLGPAxis.SetMax(Const aValue: Double);
Begin
   If ((aValue <> FMax) And (aValue > FMin)) Then Begin
      FMax := aValue;

      CheckData;
   End;
End;

//==============================================================================

Procedure TOGLGPAxis.SetStart(Const aValue: Double);
Begin
   If (aValue <> FStart) Then Begin
      FStart := aValue;

      CheckData;
   End;
End;

//==============================================================================

Procedure TOGLGPAxis.SetLength(Const aValue: Double);
Begin
   If ((aValue > 0) And (aValue <> FLength)) Then Begin
      FLength := aValue;

      CheckData;

      Owner.UpdateGPCsts;
   End;
End;

//==============================================================================

Procedure TOGLGPAxis.SetGrid(Const aValue: TOGLGPGrid);
Begin
   FGrid.Assign(aValue);
End;

//==============================================================================

Procedure TOGLGPAxis.CheckData(Const aComputePtsAndRepaint: Boolean);
Var
   OtherOGLGraphPanel: TOGLGraphPanel;
Begin
   // Check whether it's ok with the current length

   If ((FStart+FLength) > FMax) Then
      FStart := FMax-FLength;

   // Check whether Start is within the range

   If (FStart < FMin) Then
      FStart := FMin;

   // Make sure that the length is still ok

   If ((FStart+FLength) > FMax) Then Begin
      FLength := FMax-FStart;

      Owner.UpdateGPCsts;
   End;

   If (aComputePtsAndRepaint) Then Begin
      // Compute the zoom factor
      // Note: this only need to be done when "FMin", "FMax" and "FLength" are
      //       changed, i.e. when "aComputePtsAndRepaint" is true, hence we do
      //       it here...

      FZoomFactor := (FMax-FMin)/FLength;

      // Compute the points and repaint

      Owner.ComputePtsAndRepaint;

      // Go through the connected graph panels and update them

      If (AxisType = atX) Then Begin
         OtherOGLGraphPanel := Owner.ConnectedTo;

         While ((OtherOGLGraphPanel <> Nil) And (OtherOGLGraphPanel <> Owner)) Do Begin
            With OtherOGLGraphPanel Do Begin
               XAxis.FMin := FMin;
               XAxis.FMax := FMax;

               XAxis.FStart  := FStart;
               XAxis.FLength := FLength;

               UpdateGPCsts;

               ComputePtsAndRepaint;
            End;

            OtherOGLGraphPanel := OtherOGLGraphPanel.ConnectedTo;
         End;
      End;
   End;
End;

//==============================================================================

Procedure TOGLGPAxis.ComputePts;
Begin
   If (ComputingDataPts) Then   // We don't want to compute the points again in
      Exit;                     // case we come here through "CheckData"...

   ComputingDataPts := True;

   // Generate the points that make up the axis

   If ((AxisType = atX) And
       (Owner.YAxis.Start <= 0) And
       ((Owner.YAxis.Start+Owner.YAxis.Length) >= 0)) Then Begin
      DataPts[0].X := FStart;
      DataPts[1].X := FStart+FLength;
   End Else If ((AxisType = atY) And
                (Owner.XAxis.Start <= 0) And
                ((Owner.XAxis.Start+Owner.XAxis.Length) >= 0)) Then Begin
      DataPts[0].Y := FStart;
      DataPts[1].Y := FStart+FLength;
   End;

   // Compute the grid's points

   FGrid.ComputePts;

   ComputingDataPts := False;
End;

//==============================================================================

Procedure TOGLGPAxis.PaintCoordinates;
Begin
   // Show the coordinates if necessary

   If (FCoordsSpecs.Enabled) Then Begin
      If (AxisType = atX) Then
         Owner.PaintStr(FLineSpecs, FCoordsSpecs, DataPts[0].X, DataPts[0].Y, '0', 0, -0.5)
      Else
         Owner.PaintStr(FLineSpecs, FCoordsSpecs, DataPts[0].X, DataPts[0].Y, '0', -0.5, 0);
   End;
End;

//==============================================================================

Constructor TOGLGPGrid.Create(aOGLGraphPanel: TOGLGraphPanel; 
                              aAxis: TOGLGPAxis);
Begin
   Inherited Create(aOGLGraphPanel);

   Axis := aAxis;

   FLineSpecs.Color := clGray;
   FLineSpecs.Style := psDot;

   FInterval := 1;

   FIntervalType := itFixed;
End;

//==============================================================================

Procedure TOGLGPGrid.SetInterval(Const aValue: Double);
Begin
   If ((aValue > 0) And (aValue <> FInterval)) Then Begin
      FInterval := aValue;

      Owner.ComputePtsAndRepaint;
   End;
End;

//==============================================================================

Procedure TOGLGPGrid.SetIntervalType(Const aValue: TOGLGPGridIntervalType);
Begin
   If (aValue <> FIntervalType) Then Begin
      FIntervalType := aValue;

      Owner.ComputePtsAndRepaint;
   End;
End;

//==============================================================================

Procedure TOGLGPGrid.ComputeGridLines(Const aFirst, aLast, aInterval, aMin, aMax: Double);
Var
   I: Double;
   Last: Double;
Begin
   If (((aInterval > 0) And (aFirst > aLast)) Or
       ((aInterval < 0) And (aFirst < aLast))) Then
      Exit;

   I := aFirst;

   Last := aLast+0.5*aInterval;   // Allow for a margin error (because of the
                                  // test not being 100% accurate in some cases)

   If (Axis.AxisType = atX) Then
      Repeat
         SetLength(DataPts, Length(DataPts)+2);

         With DataPts[High(DataPts)-1] Do Begin
             X := aMin;
             Y := I;
         End;

         With DataPts[High(DataPts)] Do Begin
            X := aMax;
            Y := I;
         End;

         I := I+aInterval;
      Until ((aInterval > 0) And (I > Last)) Or ((aInterval < 0) And (I < Last))
   Else
      Repeat
         SetLength(DataPts, Length(DataPts)+2);

         With DataPts[High(DataPts)-1] Do Begin
             X := I;
             Y := aMin;
         End;

         With DataPts[High(DataPts)] Do Begin
            X := I;
            Y := aMax;
         End;

         I := I+aInterval;
      Until ((aInterval > 0) And (I > Last)) Or ((aInterval < 0) And (I < Last));
End;

//==============================================================================

Procedure TOGLGPGrid.ComputePts;
   Function Modulo(Const aX, aY: Double): Double; Inline;
   Begin
      Try
{$IFDEF OPT_MATH}
         Result := aX-aY*OptFloor(aX/aY);
{$ELSE}
         Result := aX-aY*Floor(aX/aY);
{$ENDIF}
      Except
         // "aX/aY" is most likely too small, so "OptFloor" generated an
         // exception...

         Result := aX;
      End;
   End;
Var
   RealInterval: Double;
   First, Last: Double;
   First1, Last1: Double;
   First2, Last2: Double;
   Min, Max: Double;
   OtherAxis: TOGLGPAxis;
Begin
   If (ComputingDataPts) Then   // We don't want to compute the points again in
      Exit;                     // case we come here through "CheckData"...

   ComputingDataPts := True;

   If (Axis.AxisType = atX) Then
      OtherAxis := Owner.YAxis
   Else
      OtherAxis := Owner.XAxis;

   // Determine the interval between the grid lines

   If (FIntervalType = itAutomatic) Then Begin
{$IFDEF OPT_MATH}
      RealInterval := OptPower(10, OptFloor(OptLog10(OtherAxis.Length)));
{$ELSE}
      RealInterval := Power(10, Floor(Log10(OtherAxis.Length)));
{$ENDIF}

      If (((OtherAxis.Start < 0) And ((OtherAxis.Start+OtherAxis.Length) > 0) And (OtherAxis.Length <= (2*RealInterval))) Or
          (OtherAxis.Length < (2*RealInterval))) Then
         RealInterval := 0.1*RealInterval;
   End Else
      RealInterval := FInterval;

   // Find out about where to start displaying the grid lines and for how long

   If ((OtherAxis.Start <= 0) And ((OtherAxis.Start+OtherAxis.Length) >= 0)) Then Begin
      First := 0;
      Last  := 0;

      First1 := -RealInterval;
      Last1  := OtherAxis.Start-Modulo(OtherAxis.Start, RealInterval);

      First2 := RealInterval;
      Last2  := OtherAxis.Start+OtherAxis.Length-Modulo(OtherAxis.Start+OtherAxis.Length, RealInterval);
   End Else Begin
      First := OtherAxis.Start-Modulo(OtherAxis.Start, RealInterval);
      Last  := OtherAxis.Start+OtherAxis.Length-Modulo(OtherAxis.Start+OtherAxis.Length, RealInterval);

      First1 := 0;
      Last1  := 0;

      First2 := 0;
      Last2  := 0;
   End;

   // Clear all previous points that form the different grid lines

   SetLength(DataPts, 0);

   // Find out which kind of graph we are dealing with and create the different
   // grid lines from there

   Min := Axis.Start;
   Max := Min+Axis.Length;

   If (First <> Last) Then
      ComputeGridLines(First, Last, RealInterval, Min, Max)
   Else Begin
      ComputeGridLines(First1, Last1, -RealInterval, Min, Max);
      ComputeGridLines(First2, Last2, RealInterval, Min, Max);
   End;

   ComputingDataPts := False;
End;

//==============================================================================

Procedure TOGLGPGrid.Paint;
Begin
   If (Axis.LineSpecs.Enabled) Then
      Inherited;
End;

//==============================================================================

Procedure TOGLGPGrid.PaintCoordinates;
Var
   Iter: Integer;
   OldCoordsPos: Double;
   OtherAxis: TOGLGPAxis;
   StrX, StrY: String;
Begin
   If (FCoordsSpecs.Enabled And (Length(DataPts) <> 0)) Then Begin
      If (Axis.AxisType = atX) Then Begin
         OtherAxis := Owner.YAxis;

         If ((OtherAxis.Start <= 0) And ((OtherAxis.Start+OtherAxis.Length) >= 0)) Then Begin
            // Because there are grid lines on both sides of the other axis, we
            // have to keep in mind that the coordinates for the other axis may
            // be displayed, so we don't want any overlapping...

            If (DataPts[0].Y <= 0) Then Begin
               // First, deal with the grid lines to the "left" of the other
               // axis

               If (Axis.CoordsSpecs.Enabled) Then
                  OldCoordsPos := 0
               Else
                  OldCoordsPos := Owner.ConvWinTextHeightToOpenGLTextHeight('0');

               Iter := 0;

               Repeat
                  With DataPts[Iter] Do Begin
                     StrY := FloatToStr(Y);

                     If ((Y+Owner.ConvWinTextHeightToOpenGLTextHeight(StrY)) < OldCoordsPos) Then Begin
                        Owner.PaintStr(FLineSpecs, FCoordsSpecs, X, Y, StrY, 0, -0.5);

                        OldCoordsPos := Y;
                     End;
                  End;

                  Inc(Iter, 2);
               Until (Iter = Length(DataPts)) Or (DataPts[Iter].Y > 0);
            End Else
               Iter := 0;

            If (Iter <> Length(DataPts)) Then Begin
               // Second, deal with the grid lines to the "right" of the other
               // axis

               If (Axis.CoordsSpecs.Enabled) Then
                  OldCoordsPos := Owner.ConvWinTextHeightToOpenGLTextHeight('0')
               Else
                  OldCoordsPos := 0;

               Repeat
                  With DataPts[Iter] Do
                     If (Y > OldCoordsPos) Then Begin
                        StrY := FloatToStr(Y);

                        Owner.PaintStr(FLineSpecs, FCoordsSpecs, X, Y, StrY, 0, -0.5);

                        OldCoordsPos := Y+Owner.ConvWinTextHeightToOpenGLTextHeight(StrY);
                     End;

                  Inc(Iter, 2);
               Until Iter = Length(DataPts);
            End;
         End Else Begin
            OldCoordsPos := Owner.ConvWinYToOpenGLY(-2*Owner.Height);

            Iter := 0;

            Repeat
               With DataPts[Iter] Do
                  If (Y > OldCoordsPos) Then Begin
                     StrY := FloatToStr(Y);

                     Owner.PaintStr(FLineSpecs, FCoordsSpecs, X, Y, StrY, 0, -0.5);

                     OldCoordsPos := Y+Owner.ConvWinTextHeightToOpenGLTextHeight(StrY);
                  End;

               Inc(Iter, 2);
            Until Iter = Length(DataPts);
         End;
      End Else Begin
         OtherAxis := Owner.XAxis;

         If ((OtherAxis.Start <= 0) And ((OtherAxis.Start+OtherAxis.Length) >= 0)) Then Begin
            // Because there are grid lines on both sides of the other axis, we
            // have to keep in mind that the coordinates for the other axis may
            // be displayed, so we don't want any overlapping...

            If (DataPts[0].X <= 0) Then Begin
               // First, deal with the grid lines to the "left" of the other
               // axis

               If (Axis.CoordsSpecs.Enabled) Then
                  OldCoordsPos := 0
               Else
                  OldCoordsPos := Owner.ConvWinTextWidthToOpenGLTextWidth('0');

               Iter := 0;

               Repeat
                  With DataPts[Iter] Do Begin
                     StrX := FloatToStr(X);

                     If ((X+Owner.ConvWinTextWidthToOpenGLTextWidth(StrX)) < OldCoordsPos) Then Begin
                        Owner.PaintStr(FLineSpecs, FCoordsSpecs, X, Y, StrX, -0.5, 0);

                        OldCoordsPos := X;
                     End;
                  End;

                  Inc(Iter, 2);
               Until (Iter = Length(DataPts)) Or (DataPts[Iter].X > 0);
            End Else
               Iter := 0;

            If (Iter <> Length(DataPts)) Then Begin
               // Second, deal with the grid lines to the "right" of the other
               // axis

               If (Axis.CoordsSpecs.Enabled) Then
                  OldCoordsPos := Owner.ConvWinTextWidthToOpenGLTextWidth('0')
               Else
                  OldCoordsPos := 0;

               Repeat
                  With DataPts[Iter] Do
                     If (X > OldCoordsPos) Then Begin
                        StrX := FloatToStr(X);

                        Owner.PaintStr(FLineSpecs, FCoordsSpecs, X, Y, StrX, -0.5, 0);

                        OldCoordsPos := X+Owner.ConvWinTextWidthToOpenGLTextWidth(StrX);
                     End;

                  Inc(Iter, 2);
               Until Iter = Length(DataPts);
            End;
         End Else Begin
            OldCoordsPos := Owner.ConvWinXToOpenGLX(-Owner.Width);

            Iter := 0;

            Repeat
               With DataPts[Iter] Do
                  If (X > OldCoordsPos) Then Begin
                     StrX := FloatToStr(X);

                     Owner.PaintStr(FLineSpecs, FCoordsSpecs, X, Y, StrX, -0.5, 0);

                     OldCoordsPos := X+Owner.ConvWinTextWidthToOpenGLTextWidth(StrX);
                  End;

               Inc(Iter, 2);
            Until Iter = Length(DataPts);
         End;
      End;
   End;
End;

//==============================================================================

Procedure TOGLGPCoordinates.PaintCoordinate(aBackgroundSpecs: TOGLGPSpecsSize;
                                            aCoordsSpecs: TOGLGPSpecsCoords; 
                                            Const aX, aY: Double;
                                            Const aTopLeft: Boolean);
Var
   Space: Integer;
   XSize, YSize: Double;
   LineXSize, LineYSize: Double;
   X, Y: Double;
   CoordsStr: String;
   CoordsStrWidth, CoordsStrHeight: Double;
Begin
   // Specify some stuff that are dependent on the graphical implementation
   // Note: the ideal solution is with the Windows API, while with OpenGL, it
   //       can be a pain in the neck to get things right, hence the different
   //       constants...

   If (Owner.WinAPI) Then Begin
      Space := 1;

      LineXSize := Owner.ConvWinXSizeToOpenGLXSize(aBackgroundSpecs.Size);
      LineYSize := Owner.ConvWinYSizeToOpenGLYSize(aBackgroundSpecs.Size);
   End Else Begin
      Space := 2;

      LineXSize := 0;
      LineYSize := 0;
   End;

   XSize := Owner.ConvWinXSizeToOpenGLXSize(Space);
   YSize := Owner.ConvWinYSizeToOpenGLYSize(Space);

   CoordsStr := '('+FloatToStr(aX)+', '+FloatToStr(aY)+')';

   CoordsStrWidth  := Owner.ConvWinTextWidthToOpenGLTextWidth(CoordsStr);
   CoordsStrHeight := Owner.ConvWinTextHeightToOpenGLTextHeight(CoordsStr);

   If (aTopLeft) Then Begin
      If ((aX-XSize-CoordsStrWidth) < Owner.XAxis.Start) Then
         X := aX+LineXSize+XSize
      Else
         X := aX-XSize-CoordsStrWidth;

      If ((aY+YSize+CoordsStrHeight) > (Owner.YAxis.Start+Owner.YAxis.Length)) Then
         Y := aY-LineYSize-YSize-CoordsStrHeight
      Else
         Y := aY+YSize;
   End Else Begin
      If ((aX+XSize+CoordsStrWidth) > (Owner.XAxis.Start+Owner.XAxis.Length)) Then
         X := aX-XSize-CoordsStrWidth
      Else
         X := aX+LineXSize+XSize;

      If ((aY-YSize-CoordsStrHeight) < Owner.YAxis.Start) Then
         Y := aY+YSize
      Else
         Y := aY-LineYSize-YSize-CoordsStrHeight;
   End;

   Owner.PaintStr(aBackgroundSpecs, aCoordsSpecs, X, Y, CoordsStr, 0, 0);
End;

//==============================================================================

Constructor TOGLGPCoords.Create(aOGLGraphPanel: TOGLGraphPanel);
Begin
   Inherited;

   FLineSpecs.Color := clTeal;
   FLineSpecs.Style := psDash;
End;

//==============================================================================

Procedure TOGLGPCoords.Paint;
Begin
   If (Active) Then Begin
      If (Owner.WinAPI) Then Begin
         Owner.BufferCanvas.Pen.Color := FLineSpecs.Color;
         Owner.BufferCanvas.Pen.Width := FLineSpecs.Size;
         Owner.BufferCanvas.Pen.Style := FLineSpecs.Style;

         // Horizontal line

         If (BothAxes) Then Begin
            Owner.BufferCanvas.MoveTo(Owner.ConvOpenGLXToWinX(Owner.XAxis.Start),
                                      Owner.ConvOpenGLYToWinY(FY));
            Owner.BufferCanvas.LineTo(Owner.ConvOpenGLXToWinX(Owner.XAxis.Start+Owner.XAxis.Length),
                                      Owner.ConvOpenGLYToWinY(FY));
         End;

         // Vertical line

         Owner.BufferCanvas.MoveTo(Owner.ConvOpenGLXToWinX(FX),
                                   Owner.ConvOpenGLYToWinY(Owner.YAxis.Start));
         Owner.BufferCanvas.LineTo(Owner.ConvOpenGLXToWinX(FX),
                                   Owner.ConvOpenGLYToWinY(Owner.YAxis.Start+Owner.YAxis.Length));
      End Else Begin
         OGLColor(FLineSpecs.Color, FLineSpecs.Alpha);
         OGLLineWidth(FLineSpecs.Size);
         OGLLineStyle(FLineSpecs.Style);
         OGLLineSmooth(False);

         glBegin(GL_LINES);
            If (BothAxes) Then Begin
               // Horizontal line

               glVertex2f(Owner.XAxis.Start, FY);
               glVertex2f(Owner.XAxis.Start+Owner.XAxis.Length, FY);
            End;

            // Vertical line

            glVertex2f(FX, Owner.YAxis.Start);
            glVertex2f(FX, Owner.YAxis.Start+Owner.YAxis.Length);
         glEnd;
      End;

      If (BothAxes) Then
         // Draw the coordinates' label

         PaintCoordinate(FLineSpecs, FCoordsSpecs, FX, FY, True);
   End;
End;

//==============================================================================

Constructor TOGLGPDims.Create(aOGLGraphPanel: TOGLGraphPanel);
Begin
   Inherited;

   FLineSpecs.Color := clPurple;
End;

//==============================================================================

Procedure TOGLGPDims.PaintDimension(aBackgroundSpecs: TOGLGPSpecsSize;
                                    Const aX, aY: Double;
                                    Const aDim: Double;
                                    Const aXDim, aTopLeft: Boolean);
Var
   Space: Integer;
   XSize, YSize: Double;
   LineXSize, LineYSize: Double;
   X, Y: Double;
   DimStr: String;
   DimStrWidth, DimStrHeight: Double;
Begin
   // Specify some stuff that are dependent on the graphical implementation
   // Note: the ideal solution is with the Windows API, while with OpenGL, it
   //       can be a pain in the neck to get things right, hence the different
   //       constants...

   If (Owner.WinAPI) Then Begin
      Space := 1;

      LineXSize := Owner.ConvWinXSizeToOpenGLXSize(aBackgroundSpecs.Size);
      LineYSize := Owner.ConvWinYSizeToOpenGLYSize(aBackgroundSpecs.Size);
   End Else Begin
      Space := 2;

      LineXSize := 0;
      LineYSize := 0;
   End;

   XSize := Owner.ConvWinXSizeToOpenGLXSize(Space);
   YSize := Owner.ConvWinYSizeToOpenGLYSize(Space);

   DimStr := FloatToStr(Abs(aDim));

   DimStrWidth  := Owner.ConvWinTextWidthToOpenGLTextWidth(DimStr);
   DimStrHeight := Owner.ConvWinTextHeightToOpenGLTextHeight(DimStr);

   If (aXDim) Then Begin
      X := aX-0.5*DimStrWidth;

      If (X < Owner.XAxis.Start) Then
         X := Owner.XAxis.Start
      Else If ((X+DimStrWidth) > (Owner.XAxis.Start+Owner.XAxis.Length)) Then
         X := Owner.XAxis.Start+Owner.XAxis.Length-DimStrWidth;

      If (aTopLeft) Then Begin
         If ((aY+YSize+DimStrHeight) > (Owner.YAxis.Start+Owner.YAxis.Length)) Then
            Y := aY-LineYSize-YSize-DimStrHeight
         Else
            Y := aY+YSize;
      End Else Begin
         If ((aY-LineYSize-YSize-DimStrHeight) < Owner.YAxis.Start) Then
            Y := aY+YSize
         Else
            Y := aY-LineYSize-YSize-DimStrHeight;
      End;
   End Else Begin
      If (aTopLeft) Then Begin
         If ((aX-LineXSize-XSize-DimStrWidth) < Owner.XAxis.Start) Then
            X := aX+LineXSize+XSize
         Else
            X := aX-XSize-DimStrWidth;
      End Else Begin
         If ((aX+XSize+DimStrWidth) > (Owner.XAxis.Start+Owner.XAxis.Length)) Then
            X := aX-XSize-DimStrWidth
         Else
            X := aX+LineXSize+XSize;
      End;

      Y := aY-0.5*DimStrHeight;

      If (Y < Owner.YAxis.Start) Then
         Y := Owner.YAxis.Start
      Else If ((Y+DimStrHeight) > (Owner.YAxis.Start+Owner.YAxis.Length)) Then
         Y := Owner.YAxis.Start+Owner.YAxis.Length-DimStrHeight
   End;

   Owner.PaintStr(aBackgroundSpecs, FCoordsSpecs, X, Y, DimStr, 0, 0);
End;

//==============================================================================

Procedure TOGLGPDims.Paint;
Const
   BorderSize = 3;
Begin
   If (Active) Then Begin
      If (FLineSpecs.Enabled) Then Begin
         If (Owner.WinAPI) Then Begin
            // Border

            Owner.BufferCanvas.Pen.Color := FLineSpecs.Color;
            Owner.BufferCanvas.Pen.Width := FLineSpecs.Size;
            Owner.BufferCanvas.Pen.Style := FLineSpecs.Style;

            Owner.BufferCanvas.MoveTo(Owner.ConvOpenGLXToWinX(X1), Owner.ConvOpenGLYToWinY(Y1)-BorderSize*FLineSpecs.Size);
            Owner.BufferCanvas.LineTo(Owner.ConvOpenGLXToWinX(X1), Owner.ConvOpenGLYToWinY(Y1)+BorderSize*FLineSpecs.Size);

            Owner.BufferCanvas.MoveTo(Owner.ConvOpenGLXToWinX(X1), Owner.ConvOpenGLYToWinY(Y1));
            Owner.BufferCanvas.LineTo(Owner.ConvOpenGLXToWinX(X2), Owner.ConvOpenGLYToWinY(Y1));
            Owner.BufferCanvas.LineTo(Owner.ConvOpenGLXToWinX(X2), Owner.ConvOpenGLYToWinY(Y2));

            Owner.BufferCanvas.MoveTo(Owner.ConvOpenGLXToWinX(X2)-BorderSize*FLineSpecs.Size, Owner.ConvOpenGLYToWinY(Y2));
            Owner.BufferCanvas.LineTo(Owner.ConvOpenGLXToWinX(X2)+BorderSize*FLineSpecs.Size, Owner.ConvOpenGLYToWinY(Y2));
         End Else Begin
            // Dimensions themselves

            OGLColor(FLineSpecs.Color, FLineSpecs.Alpha);
            OGLLineWidth(FLineSpecs.Size);
            OGLLineStyle(FLineSpecs.Style);
            OGLLineSmooth(False);

            glBegin(GL_LINES);
               glVertex2f(FX1, FY1-BorderSize*Owner.ConvWinYSizeToOpenGLYSize(FLineSpecs.Size));
               glVertex2f(FX1, FY1+BorderSize*Owner.ConvWinYSizeToOpenGLYSize(FLineSpecs.Size));

               glVertex2f(FX1, FY1);
               glVertex2f(FX2, FY1);

               glVertex2f(FX2, FY1);
               glVertex2f(FX2, FY2);

               glVertex2f(FX2-BorderSize*Owner.ConvWinXSizeToOpenGLXSize(FLineSpecs.Size), FY2);
               glVertex2f(FX2+BorderSize*Owner.ConvWinXSizeToOpenGLXSize(FLineSpecs.Size), FY2);
            glEnd;
         End;
      End;

      // Draw the top-left and bottom-right coordinates' labels

      PaintDimension(FLineSpecs, 0.5*(FX1+FX2), FY1, FX2-FX1, True, FY1 > FY2);
      PaintDimension(FLineSpecs, FX2, 0.5*(FY1+FY2), FY2-FY1, False, FX1 > FX2);
   End;
End;

//==============================================================================

Constructor TOGLGPRegion.Create(aOGLGraphPanel: TOGLGraphPanel);
Begin
   Inherited;

   FLineSpecs.Color := clMaroon;

   FFillingSpecs := TOGLGPSpecsColor.Create(aOGLGraphPanel);

   FFillingSpecs.Color := clYellow;
   FFillingSpecs.Alpha := 0.15;
End;

//==============================================================================

Destructor TOGLGPRegion.Destroy;
Begin
   FFillingSpecs.Free;

   Inherited;
End;

//==============================================================================

Procedure TOGLGPRegion.SetFillingSpecs(Const aValue: TOGLGPSpecsColor);
Begin
   FFillingSpecs.Assign(aValue);
End;

//==============================================================================

Procedure TOGLGPRegion.Paint;
Var
   IStart, IEnd, JStart, JEnd: Integer;
   ScanLine: PByte;
   ScanLineDelta: Integer;
   I, J: Integer;
   P: POGLBmpRGB;
   AlphaR, AlphaG, AlphaB: Single;
   One_Alpha: Single;
Begin
   If (Active) Then Begin
      If (Owner.WinAPI) Then Begin
{$IFDEF OPT_MATH}
         IStart := OptMaxI(0, Owner.ConvOpenGLXToWinX(X1));
         IEnd   := OptMinI(Owner.ConvOpenGLXToWinX(X2)+1, Owner.Buffer.Width-1);

         JStart := OptMaxI(0, Owner.ConvOpenGLYToWinY(Y1));
         JEnd   := OptMinI(Owner.ConvOpenGLYToWinY(Y2)+1, Owner.Buffer.Height-1);
{$ELSE}
         IStart := Max(0, Owner.ConvOpenGLXToWinX(X1));
         IEnd   := Min(Owner.ConvOpenGLXToWinX(X2)+1, Owner.Buffer.Width-1);

         JStart := Max(0, Owner.ConvOpenGLYToWinY(Y1));
         JEnd   := Min(Owner.ConvOpenGLYToWinY(Y2)+1, Owner.Buffer.Height-1);
{$ENDIF}

         // Background (through blending of the region)

         AlphaR := FFillingSpecs.Alpha*(ColorToRGB(FFillingSpecs.Color) Mod 256);
         AlphaG := FFillingSpecs.Alpha*((ColorToRGB(FFillingSpecs.Color) ShR 8) Mod 256);
         AlphaB := FFillingSpecs.Alpha*(ColorToRGB(FFillingSpecs.Color) ShR 16);

         One_Alpha := (1-FFillingSpecs.Alpha);

         ScanLine := Owner.Buffer.ScanLine[0];
         ScanLineDelta := Integer(Owner.Buffer.ScanLine[1])-Integer(ScanLine);

         Inc(ScanLine, JStart*ScanLineDelta);

         For J := JStart To JEnd Do Begin
            P := POGLBmpRGB(ScanLine);

            Inc(P, IStart);

            For I := IStart To IEnd Do Begin
               P.R := Round(AlphaR+One_Alpha*P.R);
               P.G := Round(AlphaG+One_Alpha*P.G);
               P.B := Round(AlphaB+One_Alpha*P.B);

               Inc(P);
            End;

            Inc(Scanline, ScanLineDelta);
         end;

         // Border

         If (FLineSpecs.Enabled) Then Begin
            Owner.BufferCanvas.Pen.Color := FLineSpecs.Color;
            Owner.BufferCanvas.Pen.Width := FLineSpecs.Size;
            Owner.BufferCanvas.Pen.Style := FLineSpecs.Style;
         End Else
            Owner.BufferCanvas.Pen.Style := psClear;

         Owner.BufferCanvas.Rectangle(IStart, JStart, IEnd+1, JEnd+1);
      End Else Begin
         // Background

         If (FFillingSpecs.Enabled) Then Begin
            OGLColor(FFillingSpecs.Color, FFillingSpecs.Alpha);

            glBegin(GL_QUADS);
               glVertex2f(FX1, FY1);
               glVertex2f(FX1, FY2);
               glVertex2f(FX2, FY2);
               glVertex2f(FX2, FY1);
            glEnd;
         End;

         // Border

         If (FLineSpecs.Enabled) Then Begin
            OGLColor(FLineSpecs.Color, FLineSpecs.Alpha);
            OGLLineWidth(FLineSpecs.Size);
            OGLLineStyle(FLineSpecs.Style);
            OGLLineSmooth(False);

            glBegin(GL_LINE_LOOP);
               glVertex2f(FX1, FY1);
               glVertex2f(FX1, FY2);
               glVertex2f(FX2, FY2);
               glVertex2f(FX2, FY1);
            glEnd;
         End;
      End;

      // Draw the top-left and bottom-right coordinates' labels

      PaintCoordinate(FLineSpecs, FCoordsSpecs, FX1, FY1, True);
      PaintCoordinate(FLineSpecs, FCoordsSpecs, FX2, FY2, False);
   End;
End;

//==============================================================================

Constructor TOGLGPGraph.Create(aOGLGraphPanel: TOGLGraphPanel;
                               Const aName, aLegend: String;
                               Const aShift: Integer;
                               Const aLength, aXRes: Double);
Var
   OldEnabled: Boolean;
Begin
   Inherited Create(aOGLGraphPanel);

   OldEnabled := aOGLGraphPanel.Enabled;

   aOGLGraphPanel.Enabled := False;

   FName := aName;
   FLegend := aLegend;

   QuickPaintEnabled := True;
   // Note: we are using "QuickPaintEnabled" and not "FQuickPaintEnabled" so
   //       that "SetQuickPaintEnabled" gets called...

   FShift := aShift;

   FXRes := aXRes;
   FOne_XRes := 1/aXRes;

   SetLength(Data, Round(aLength*FOne_XRes-aShift+1));

   DataSize := 0;
   DataIter := 0;

   aOGLGraphPanel.Enabled := OldEnabled;
End;

//==============================================================================

Procedure TOGLGPGraph.SetQuickPaintEnabled(Const aValue: Boolean);
Var
   I, QGIndex: Integer;
Begin
   If (aValue <> FQuickPaintEnabled) Then Begin
      FQuickPaintEnabled := aValue;

      If (FQuickPaintEnabled) Then Begin
         // Add the quick graph to the list of quick graphs

         If (Owner.QuickGraphsListSize+1 > Length(Owner.QuickGraphsList)) Then
            // Not enough space, so make some...

            SetLength(Owner.QuickGraphsList, 2*Length(Owner.QuickGraphsList));

         Owner.QuickGraphsList[Owner.QuickGraphsListSize]:= Self;

         Inc(Owner.QuickGraphsListSize);
      End Else Begin
         // Remove the graph from the list of quick graphs

         QGIndex := -1;

         For I := 0 To Owner.QuickGraphsListSize-1 Do
            If (CompareStr(Name, Owner.QuickGraphsList[I].Name) = 0) Then Begin
               QGIndex := I;

               Break;
            End;

         If (QGIndex <> -1) Then Begin
            For I := QGIndex+1 To Owner.QuickGraphsListSize-1 Do
               Owner.QuickGraphsList[I-1] := Owner.QuickGraphsList[I];

            Dec(Owner.QuickGraphsListSize);
         End;
      End;
   End;
End;

//==============================================================================

Procedure TOGLGPGraph.StartAndEndPts(Var aStartIter, aEndIter: Integer);
Begin
   aStartIter := FindOGLPtIter(Data, DataSize, FShift, Owner.XAxis.Start, FOne_XRes, True);

   If (aStartIter = -1) Then
      // There is no point at or before the beginning of the X axis, so just
      // take the very first point in the graph

      aStartIter := 0;

   aEndIter   := FindOGLPtIter(Data, DataSize, FShift, Owner.XAxis.Start+Owner.XAxis.Length, FOne_XRes, False);

   If (aEndIter = -1) Then
      // There is no point at or after the end of the X axis, so just take the
      // very last point in the graph

      aEndIter := DataSize-1;
End;

//==============================================================================

Procedure TOGLGPGraph.Paint;
Var
   StartIter, EndIter: Integer;
Begin
   StartAndEndPts(StartIter, EndIter);

   // Lines

   PaintLines(StartIter, EndIter, False);
End;

//==============================================================================

Procedure TOGLGPGraph.AddPt(Const aY: Double);
Begin
   If (DataSize+1 > Length(Data)) Then
      // Not enough space, so make some...

      SetLength(Data, 2*Length(Data));

   Data[DataSize]:= aY;

   Inc(DataSize);

   If (Not IsInfinite(aY)) Then Begin
      // "aY" is not an infinite number, so we can use it to determine "FYMin"
      // and "FYMax"
      // Note: infinite numbers are a pain in the butt, so we don't want to deal
      //       with them as such...

      If (Not YMinAndYMax) Then Begin
         FYMin := aY;
         FYMax := aY;

         YMinAndYMax := True;

         Owner.NeedUpdateYMinAndYMax := True;
      End Else Begin
         If (aY < FYMin) Then Begin
            FYMin := aY;

            Owner.NeedUpdateYMinAndYMax := True;
         End;

         If (aY > FYMax) Then Begin
            FYMax := aY;

            Owner.NeedUpdateYMinAndYMax := True;
         End;
      End;
   End;
End;

//==============================================================================

Procedure TOGLGPGraph.PaintLines(Const aStartIter, aEndIter: Integer;
                                 Const aQuickPaint: Boolean);
Var
   Iter: Integer;
   RealCanvas: TCanvas;
   DataPts: Array Of TPoint;
Begin
   // Note: due to the nature of the component, which relies on "XRes" for the
   //       abscissa of the data to be plotted, if "XRes" is equal to 1, then we
   //       can't normally have a plot finishing at 1.5. It may, however, be
   //       good to support such a finishing. The way to go about it is to
   //       'pretend' that there is a data point at 2, but plot it at 1.5
   //       (through the "OptMinD"/"Min" call)...         

   If (FLineSpecs.Enabled And (aStartIter <> -1)) Then Begin
      If (Owner.WinAPI) Then Begin
         If (aQuickPaint) Then
            RealCanvas := Owner.DestCanvas
         Else
            RealCanvas := Owner.BufferCanvas;

         RealCanvas.Pen.Color := FLineSpecs.Color;
         RealCanvas.Pen.Width := FLineSpecs.Size;
         RealCanvas.Pen.Style := FLineSpecs.Style;

         SetLength(DataPts, aEndIter-aStartIter+1);

         For Iter := aStartIter To aEndIter Do
            With DataPts[Iter-aStartIter] Do Begin
{$IFDEF OPT_MATH}
               X := Owner.ConvOpenGLXToWinX(OptMinD((Shift+Iter)*XRes, Owner.XAxis.Max));
{$ELSE}
               X := Owner.ConvOpenGLXToWinX(Min((Shift+Iter)*XRes, Owner.XAxis.Max));
{$ENDIF}
               Y := Owner.ConvOpenGLYToWinY(Data[Iter]);
            End;

         RealCanvas.PolyLine(DataPts);
      End Else Begin
         OGLColor(FLineSpecs.Color, FLineSpecs.Alpha);
         OGLLineWidth(FLineSpecs.Size);
         OGLLineStyle(FLineSpecs.Style);
         OGLLineSmooth(True);

         Iter := aStartIter;

         glBegin(GL_LINE_STRIP);
            While (Iter <= aEndIter) Do Begin
{$IFDEF OPT_MATH}
               glVertex2f(OptMinD((Shift+Iter)*XRes, Owner.XAxis.Max), Data[Iter]);
{$ELSE}
               glVertex2f(Min((Shift+Iter)*XRes, Owner.XAxis.Max), Data[Iter]);
{$ENDIF}

               Inc(Iter);
            End;
         glEnd;
      End;
   End;
End;

//==============================================================================

Function TOGLGPGraph.NeedQuickPaint(Var aStartIter, aEndIter: Integer): Boolean;
Begin
   StartAndEndPts(aStartIter, aEndIter);

   // Update the starting point, if necessary...

   If (aStartIter < DataIter) Then
      aStartIter := DataIter;

   Result := aEndIter-aStartIter >= 1;
End;

//==============================================================================

Function TOGLGPGraph.NeedQuickPaint: Boolean;
Var
   DummyStartIter, DummyEndIter: Integer;
Begin
   Result := False;

   If (Not Owner.NeedUpdateYAxis) Then
      Result := NeedQuickPaint(DummyStartIter, DummyEndIter);
End;

//==============================================================================

Function TOGLGPGraph.QuickPaint: Integer;
Var
   StartIter, EndIter: Integer;
Begin
   // Result: 0, no points have been drawn
   //         1, some points have been drawn

   // Quickly update the graph, based on the last point that was plotted

   If (NeedQuickPaint(StartIter, EndIter)) Then Begin
      // Perfom an optimised paiting of the graph (i.e. only the points after
      // the last painted one)

      PaintLines(StartIter, EndIter, True);

      DataIter := EndIter;

      Result := 1;
   End Else
      Result := 0;
End;

//==============================================================================

Function TOGLGPGraph.IsValid: Boolean;
Begin
   // A "valid" graph is a graph that is visible and that contains at least one
   // point (i.e. has a minimum and maximum)

   Result := FLineSpecs.Enabled And YMinAndYMax;
End;

//==============================================================================

Constructor TOGLGraphPanel.Create(aOwner: TComponent);
Begin
   Inherited;

   OnInit   := GLInit;
   OnPaint  := GLPaint;
   OnResize := GLResize;

   // Default background colour

   Color := clWhite;

   // Create the region and coordinates objects

   FCoords := TOGLGPCoords.Create(Self);
   FDims   := TOGLGPDims.Create(Self);
   FRegion := TOGLGPRegion.Create(Self);

   // Initialise the panning timer for the automatic panning

   PanningTimer := TTimer.Create(Self);

   PanningTimer.Interval := 13;   // Nearly as fast as possible
   PanningTimer.Enabled  := False;

   PanningTimer.OnTimer := PanningAutomatically;

   // Initialise the zoom timer for the automatic panning

   ZoomingTimer := TTimer.Create(Self);

   ZoomingTimer.Interval := 13;   // Nearly as fast as possible
   ZoomingTimer.Enabled := False;

   ZoomingTimer.OnTimer := ZoomingAutomatically;

   // Initialise the axes

   FXAxis := TOGLGPAxis.Create(Self, atX);
   FYAxis := TOGLGPAxis.Create(Self, atY);

   FXAxis.CheckData(False);   // Note: we don't want to compute
   FYAxis.CheckData(False);   //       the points...

   ComputePtsAndRepaint(False);   // Generate the points, but do NOT repaint!

   SetLength(QuickGraphsList, 1);
   SetLength(FGraphsList, 1);

   QuickGraphsListSize := 0;
   FGraphsListSize := 0;
End;

//==============================================================================

Destructor TOGLGraphPanel.Destroy;
Begin
   // Free up some objects

   FXAxis.Free;
   FYAxis.Free;

   FCoords.Free;
   FDims.Free;
   FRegion.Free;

   PanningTimer.Free;
   ZoomingTimer.Free;

   // Remove all the graphs

   RemoveAllGraphs;

   Inherited;
End;

//==============================================================================

Function TOGLGraphPanel.GetConnectedTo: TOGLGraphPanel;
Begin
   Result := Nil;

   If (Assigned(FConnectedTo) And
       (Owner.FindComponent(FConnectedTo.Name) <> Nil)) Then
      Result := FConnectedTo;
End;

//==============================================================================

Procedure TOGLGraphPanel.UpdateYMinAndYMax;
Var
   Iter: Integer;
   NeedInit: Boolean;
Begin
   If (Not NeedUpdateYMinAndYMax) Then
      Exit;

   FYMin := 0;
   FYMax := 0;

   NeedInit := True;

   For Iter := 0 To FGraphsListSize-1 Do
      If (FGraphsList[Iter].IsValid) Then Begin
         If (NeedInit) Then Begin
            FYMin := FGraphsList[Iter].YMin;
            FYMax := FGraphsList[Iter].YMax;

            NeedInit := False;
         End Else Begin
            If (FGraphsList[Iter].YMin < FYMin) Then
               FYMin := FGraphsList[Iter].YMin;

            If (FGraphsList[Iter].YMax > FYMax) Then
               FYMax := FGraphsList[Iter].YMax;
         End;
      End;

   NeedUpdateYMinAndYMax := False;
End;

//==============================================================================

Function TOGLGraphPanel.GetYMin2(Const aOptimise: Boolean): Double;
Var
   AbsYMin, AbsYMax: Double;
   FloorLog10AbsYMin, FloorLog10AbsYMax: Double;
   Factor: Double;
Begin
   UpdateYMinAndYMax;

   Result := FYMin;

   If (aOptimise And (Result <> 0)) Then Begin
      // Round the number

      AbsYMin := Abs(Result);
      AbsYMax := Abs(GetYMax2(False));

{$IFDEF OPT_MATH}
      FloorLog10AbsYMin := OptFloor(OptLog10(AbsYMin));
{$ELSE}
      FloorLog10AbsYMin := Floor(Log10(AbsYMin));
{$ENDIF}

      If (AbsYMax <> 0) Then
{$IFDEF OPT_MATH}
         FloorLog10AbsYMax := OptFloor(OptLog10(AbsYMax))
{$ELSE}
         FloorLog10AbsYMax := Floor(Log10(AbsYMax))
{$ENDIF}
      Else
         FloorLog10AbsYMax := FloorLog10AbsYMin;

      If (FloorLog10AbsYMin >= FloorLog10AbsYMax) Then Begin
{$IFDEF OPT_MATH}
         Factor := OptPower(10, FloorLog10AbsYMin);

         Result := Factor*OptFloor(Result/Factor);
{$ELSE}
         Factor := Power(10, FloorLog10AbsYMin);

         Result := Factor*Floor(Result/Factor);
{$ENDIF}
      End Else Begin
         If (Result < 0) Then
{$IFDEF OPT_MATH}
            Result := -OptPower(10, FloorLog10AbsYMax)
{$ELSE}
            Result := -Power(10, FloorLog10AbsYMax)
{$ENDIF}
         Else
            Result := 0;
      End;
   End;
End;

//==============================================================================

Function TOGLGraphPanel.GetYMin: Double;
Begin
   Result := GetYMin2(True);
End;

//==============================================================================

Function TOGLGraphPanel.GetYMax2(Const aOptimise: Boolean): Double;
Var
   AbsYMin, AbsYMax: Double;
   FloorLog10AbsYMin, FloorLog10AbsYMax: Double;
   Factor: Double;
Begin
   UpdateYMinAndYMax;

   Result := FYMax;

   If (aOptimise And (Result <> 0)) Then Begin
      // Round the number

      AbsYMin := Abs(GetYMin2(False));
      AbsYMax := Abs(Result);

{$IFDEF OPT_MATH}
      FloorLog10AbsYMax := OptFloor(OptLog10(AbsYMax));
{$ELSE}
      FloorLog10AbsYMax := Floor(Log10(AbsYMax));
{$ENDIF}

      If (AbsYMin <> 0) Then
{$IFDEF OPT_MATH}
         FloorLog10AbsYMin := OptFloor(OptLog10(AbsYMin))
{$ELSE}
         FloorLog10AbsYMin := Floor(Log10(AbsYMin))
{$ENDIF}
      Else
         FloorLog10AbsYMin := FloorLog10AbsYMax;

      If (FloorLog10AbsYMax >= FloorLog10AbsYMin) Then Begin
{$IFDEF OPT_MATH}
         Factor := OptPower(10, FloorLog10AbsYMax);

         Result := Factor*OptCeil(Result/Factor);
{$ELSE}
         Factor := Power(10, FloorLog10AbsYMax);

         Result := Factor*Ceil(Result/Factor);
{$ENDIF}
      End Else Begin
         If (Result < 0) Then
            Result := 0
         Else
{$IFDEF OPT_MATH}
            Result := OptPower(10, FloorLog10AbsYMin);
{$ELSE}
            Result := Power(10, FloorLog10AbsYMin);
{$ENDIF}
      End;
   End;
End;

//==============================================================================

Function TOGLGraphPanel.GetYMax: Double;
Begin
   Result := GetYMax2(True);
End;

//==============================================================================

Procedure TOGLGraphPanel.SetConnectedTo(Const aValue: TOGLGraphPanel);
Begin
   FConnectedTo := Nil;

   If (Assigned(aValue) And (aValue <> Self) And
       (Owner.FindComponent(aValue.Name) <> Nil)) Then
      FConnectedTo := aValue;
End;

//==============================================================================

Procedure TOGLGraphPanel.SetXAxis(Const aValue: TOGLGPAxis);
Begin
   FXAxis.Assign(aValue);
End;

//==============================================================================

Procedure TOGLGraphPanel.SetYAxis(Const aValue: TOGLGPAxis);
Begin
   FYAxis.Assign(aValue);
End;

//==============================================================================

Procedure TOGLGraphPanel.SetCoords(Const aValue: TOGLGPCoords);
Begin
   FCoords.Assign(aValue);
End;

//==============================================================================

Procedure TOGLGraphPanel.SetDims(Const aValue: TOGLGPDims);
Begin
   FDims.Assign(aValue);
End;

//==============================================================================

Procedure TOGLGraphPanel.SetRegion(Const aValue: TOGLGPRegion);
Begin
   FRegion.Assign(aValue);
End;

//==============================================================================

Procedure TOGLGraphPanel.SetAutoYRange(Const aValue: Boolean);
Begin
   If (aValue <> FAutoYRange) Then Begin
      FAutoYRange := aValue;

      ComputePtsAndRepaint;
   End;
End;

//==============================================================================

Function TOGLGraphPanel.NeedUpdateYAxis: Boolean;
Var
   YAxisMin, YAxisMax: Double;
Begin
   YAxisMin := GetYMin;
   YAxisMax := GetYMax;

   Result := FAutoYRange And (YAxisMin <> YAxisMax) And
             ((YAxisMin <> FYAxis.Min) Or (YAxisMax <> FYAxis.Max));
End;

//==============================================================================

Function TOGLGraphPanel.UpdateYAxis: Boolean;
Var
   YAxisMin, YAxisMax: Double;
   CanUpdateStartAndLength: Boolean;
   OldEnabled: Boolean;
Begin
   YAxisMin := GetYMin;
   YAxisMax := GetYMax;

   If (FAutoYRange And (YAxisMin <> YAxisMax) And
       ((YAxisMin <> FYAxis.Min) Or (YAxisMax <> FYAxis.Max))) Then Begin
      OldEnabled := Enabled;

      Enabled := False;

      CanUpdateStartAndLength := False;

      If (YAxisMin <> FYAxis.Min) Then Begin
         FYAxis.Min := YAxisMin;

         CanUpdateStartAndLength := True;
      End;

      If (YAxisMax <> FYAxis.Max) Then Begin
         FYAxis.Max := YAxisMax;

         CanUpdateStartAndLength := True;
      End;

      If (CanUpdateStartAndLength) Then Begin
         FYAxis.Start  := YAxisMin;
         FYAxis.Length := YAxisMax-YAxisMin;

         UpdateGPCsts;
      End;

      ComputePtsAndRepaint(False);   // Generate the points, but do NOT repaint!

      Enabled := OldEnabled;

      Result := True;
   End Else
      Result := False;
End;

//==============================================================================

Procedure TOGLGraphPanel.PaintStr(aColorSpecs: TOGLGPSpecsSize;
                                  aCoordsSpecs: TOGLGPSpecsCoords;
                                  Const aX, aY: Double;
                                  Const aStr: String;
                                  Const aXShift, aYShift: Double);
Var
   X, Y: Double;
   TruncedX, TruncedY: Integer;
   ValidX, ValidY: Double;
   CoordsStrWidth, CoordsStrHeight: Double;
Begin
   // Compute the dimensions of the coordinates

   If (WinAPI) Then Begin
      CoordsStrWidth  := BufferCanvas.TextWidth(aStr)+1;
      // Note: see "ConvWinTextWidthToOpenGLTextWidth" for the reason behind
      //       "+1"
      CoordsStrHeight := BufferCanvas.TextHeight(aStr);

      // Update the position of the coordinates, if necessary

      TruncedX := Trunc(ConvOpenGLXToWinX(aX)+aXShift*CoordsStrWidth);
      TruncedY := Trunc(ConvOpenGLYToWinY(aY)-aYShift*CoordsStrHeight);

      // Background

      If (aCoordsSpecs.Inverted) Then Begin
         BufferCanvas.Brush.Color := aColorSpecs.Color;
         BufferCanvas.Brush.Style := bsSolid;

         BufferCanvas.FillRect(Rect(TruncedX, TruncedY-Trunc(CoordsStrHeight),
                                    TruncedX+Trunc(CoordsStrWidth), TruncedY));
      End;

      // Text

      BufferCanvas.Brush.Style := bsClear;
      // Note: "TextOut" can paint a background, but it really fits the text it
      //       paints, while it looks better with slightly more space around,
      //       hence we paint a proper background ourselves (see above) and not
      //       via "TextOut"...

      If (aCoordsSpecs.Inverted) Then
         BufferCanvas.Font.Color := Color
      Else
         BufferCanvas.Font.Color := aColorSpecs.Color;

      BufferCanvas.TextOut(TruncedX, TruncedY-Trunc(CoordsStrHeight), aStr);
   End Else Begin
      CoordsStrWidth  := ConvWinTextWidthToOpenGLTextWidth(aStr);
      CoordsStrHeight := ConvWinTextHeightToOpenGLTextHeight(aStr);

      // Update the position of the coordinates, if necessary

      X := aX+aXShift*CoordsStrWidth;
      Y := aY+aYShift*CoordsStrHeight;

      // Background

      If (aCoordsSpecs.Inverted) Then Begin
         OGLColor(aColorSpecs.Color, aColorSpecs.Alpha);

         glBegin(GL_QUADS);
            glVertex2f(X, Y);
            glVertex2f(X+CoordsStrWidth, Y);
            glVertex2f(X+CoordsStrWidth, Y+CoordsStrHeight);
            glVertex2f(X, Y+CoordsStrHeight);
         glEnd;
      End;

      // Text

      If (aCoordsSpecs.Inverted) Then
         OGLColor(Color)
      Else
         OGLColor(aColorSpecs.Color);

      ValidX := FXAxis.Start+0.5*FXAxis.Length;
      ValidY := FYAxis.Start+0.5*FYAxis.Length;

      Text2D(ValidX, ValidY, aStr, True,
             ConvOpenGLXSizeToWinXSize(X-ValidX),
             ConvOpenGLYSizeToWinYSize(Y+ConvWinYSizeToOpenGLYSize(0.25*Font.Size)-ValidY));
      // Note: the "ConvWinYSizeToOpenGLYSize(0.25*Font.Size)" is because
      //       otherwise the text is displayed too low (about 2 pixels when the
      //       font has a size of 8), so...
   End;
End;

//==============================================================================

Function TOGLGraphPanel.ClosestYGraph(Const aX, aY: Integer;
                                      Var aOGLGraphIter: Integer): Double;
Var
   Iter: Integer;
   Y1, Y2: Double;
   NeedValue: Boolean;
   X, Y: Double;
Begin
   Result := 0;
   // Note: this value is ALWAYS overwritten, but we leave it as otherwise a
   //       warning is generated because of "Result" being used in a test, so
   //       the compiler needs to believe that it is defined before being
   //       used...

   NeedValue := True;

   X := ConvWinXToOpenGLX(aX);

   If (X < FXAxis.Start) Then
      X := FXAxis.Start
   Else If (X > (FXAxis.Start+FXAxis.Length)) Then
      X := FXAxis.Start+FXAxis.Length;

   With FGraphsList[aOGLGraphIter] Do
      If (IsValid) Then Begin
         // Get the current or previous point AND the next one

         Iter := FindOGLPtIter(Data, DataSize, Shift, X, One_XRes, True);

         If (Iter <> -1) Then Begin
            If (Iter <> High(Data)) Then Begin
               // The iterator is valid, so get the point that is associated
               // to it, as well as the one after it

               Y1 := Data[Iter];
               Y2 := Data[Iter+1];

               // The abscissa of the current point is somewhere between the
               // first and the second points, so use a linear interpolation
               // to determine its ordinate

               Y := Y1+(X-((Shift+Iter)*XRes))*(Y2-Y1)*One_XRes;
            End Else
               // Last point, so...

               Y := Data[Iter];

            If ((Y >= FYAxis.Start) And (Y <= (FYAxis.Start+FYAxis.Length))) Then Begin
               Result := Y;

               NeedValue := False;
            End;
         End;
      End;

   If (NeedValue) Then Begin
      Result := ConvWinYToOpenGLY(aY);

      aOGLGraphIter := FGraphsListSize;
   End;
End;

//==============================================================================

Function TOGLGraphPanel.ClosestY(Const aX, aY: Integer;
                                 Var aOGLGraphIter: Integer): Double;
Var
   Iter, Iter2: Integer;
   NeedInit: Boolean;
   Y, YOrig: Double;
Begin
   Result := 0;
   // Note: this value is ALWAYS overwritten, but we leave it as otherwise a
   //       warning is generated because of "Result" being used in a test, so
   //       the compiler needs to believe that it is defined before being
   //       used...

   NeedInit := True;

   YOrig := ConvWinYToOpenGLY(aY);

   For Iter := 0 To FGraphsListSize-1 Do Begin
      Iter2 := Iter;

      Y := ClosestYGraph(aX, aY, Iter2);

      If (Iter2 <> FGraphsListSize) Then Begin
         If (NeedInit And
             (Y >= FYAxis.Start) And (Y <= (FYAxis.Start+FYAxis.Length))) Then Begin
            Result := Y;

            aOGLGraphIter := Iter2;

            NeedInit := False;
         End Else If ((Abs(YOrig-Y) < Abs(YOrig-Result)) And
                      (Y >= FYAxis.Start) And (Y <= (FYAxis.Start+FYAxis.Length))) Then Begin
            Result := Y;

            aOGLGraphIter := Iter2;
         End;
      End;
   End;

   If (NeedInit) Then Begin
      // We haven't found any close point, so just return the ordinate of the
      // mouse point

      Result := YOrig;

      aOGLGraphIter := FGraphsListSize;
   End;
End;

//==============================================================================

Function TOGLGraphPanel.ClosestY(Const aX, aY: Integer): Double;
Var
   DummyOGLGraphIter: Integer;
Begin
   Result := ClosestY(aX, aY, DummyOGLGraphIter);
End;

//==============================================================================

Procedure TOGLGraphPanel.GLInit(aSender: TObject);
Begin
   // Alpha blending

   glEnable(GL_BLEND);

   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

   // Allow line stippling

   glEnable(GL_LINE_STIPPLE);

   // Allow face culling (since we never see the back face of a polygon)
   // Note: the default cull face mode is set to "GL_BACK", which is what we
   //       want

   glEnable(GL_CULL_FACE);
End;

//==============================================================================

Procedure TOGLGraphPanel.GLPaint(aSender: TObject);
Var
   Iter: Integer;
   CanvasRect: TRect;
Begin
   UpdateYAxis;   // To ensure that the Y axis is properly set, if necessary
                  // (i.e. if "FAutoYRange" is true)

   If (FWinAPI) Then Begin
      If (NeedWinAPIBackupUpdate) Then Begin
         // Clear everything up

         BufferCanvas.Brush.Color := Color;
         BufferCanvas.Pen.Color   := Color;

         BufferCanvas.Rectangle(0, 0, Width+1, Height+1);
         // Note: for some reasons, it is necessary to have "+1" when plotting
         //       points, so...
      End;
   End Else Begin
      OGLDrawBuffer(GL_BACK);
      // Specify to draw directly to the back buffer, in case this is necessary
      // (i.e. we were quick painting before)

      // Dimensions of the graph
      // Note: "1+1/Width" and "1+1/Height" are used to slightly increase the
      //       projection view so as to ensure that any rendering that is on the
      //       edge of the view is visible. Indeed, it happens that it's not
      //       always the case (e.g. with the axes' grid lines), so...

      glMatrixMode(GL_PROJECTION);

      glLoadIdentity;

      gluOrtho2D(FXAxis.Start, FXAxis.Start+FXAxis.Length*(1+One_Width),
                 FYAxis.Start, FYAxis.Start+FYAxis.Length*(1+One_Height));

      // Get ready for rendering the scene

      glMatrixMode(GL_MODELVIEW);

      glLoadIdentity;

      // Background colour

      OGLClearColor(Color, 1);

      // Clear everything up

      glClear(GL_COLOR_BUFFER_BIT);

      // Pan towards the far end

      glTranslatef(0, 0, -1);
   End;

   If ((FWinAPI And NeedWinAPIBackupUpdate) Or Not FWinAPI) Then Begin
      // Display the axes' grids
      // Note: in an ideal world, the axes' grids would be painted within the
      //       procedure that paints the axes themselves, but that would mean
      //       that either the grids of the X or Y axis would overlay those of
      //       the X or Y axis, and we simply don't want that, so...
      // Note: the previous note is also stands for the coordinates...

      FXAxis.Grid.Paint;
      FYAxis.Grid.Paint;

      // Display the axes

      FXAxis.Paint;
      FYAxis.Paint;

      // Display the axes' grids' coordinates

      FXAxis.Grid.PaintCoordinates;
      FYAxis.Grid.PaintCoordinates;

      // Display the axes' coordinates

      FXAxis.PaintCoordinates;
      FYAxis.PaintCoordinates;

      // Display the different graphs

      For Iter := 0 To FGraphsListSize-1 Do
         FGraphsList[Iter].Paint;
   End;

   // Deal with the region and the coordinates
   // Note: the Windows API part is a bit tricky, because we want to minimise
   //       as much as possible the number of calls to "CopyRect", since it's
   //       kind of time consuming...
   // Note: in the case of a Windows API painting, we call "QuickPaint" just in
   //       case points have been added and a call to "Paint" was made
   //       afterwards... Note that this would be silly to do, since the more
   //       points to be drawn, the slower the component gets. In such a case,
   //       one would use "QuickPaint" instead of "Paint"... unless one adds
   //       lots of points and decide to paint them only at the very end, in
   //       which case "Paint" would most probably be used (even though
   //       "QuickPaint" would also do)

   If (FWinAPI) Then Begin
      // Remember about the dimensions of the canvas

      CanvasRect := Rect(0, 0, Width, Height);

      If (NeedWinAPIBackupUpdate) Then Begin
         // Save the buffer for future possible use (in case of a Windows API
         // painting)

         Backup.Width  := Width;
         Backup.Height := Height;

         BackupCanvas.CopyRect(CanvasRect, BufferCanvas, CanvasRect);

         NeedWinAPIBackupUpdate := False;

         // Display the region to be zoomed in and the coordinates, if necessary
         // (the test is done within the "Paint" procedure)

         FCoords.Paint;
         FDims.Paint;
         FRegion.Paint;

         DestCanvas.CopyRect(CanvasRect, BufferCanvas, CanvasRect);
      End Else Begin
         If (NeedQuickPaint) Then Begin
            // Need for a quick paint, so get the component's canvas ready, if
            // necessary...

            If (((Canvas.ClipRect.Right-Canvas.ClipRect.Left) <> 0) And
                ((Canvas.ClipRect.Bottom-Canvas.ClipRect.Top) <> 0)) Then
               DestCanvas.CopyRect(CanvasRect, BackupCanvas, CanvasRect);

            // Do the quick painting itself...

            QuickPaint;
         End Else Begin
            // No need for a quick paint, so do what we would have done by
            // default...

            If (FCoords.Active Or FDims.Active Or FRegion.Active) Then Begin
               BufferCanvas.CopyRect(CanvasRect, BackupCanvas, CanvasRect);

               // Display the region to be zoomed in and the coordinates, if
               // necessary (the test is done within the "Paint" procedure)

               FCoords.Paint;
               FDims.Paint;
               FRegion.Paint;

               DestCanvas.CopyRect(CanvasRect, BufferCanvas, CanvasRect);
            End Else
               // Nothing special, so just retrieve the backup...

               DestCanvas.CopyRect(CanvasRect, BackupCanvas, CanvasRect);
         End;
      End;
   End Else Begin
      // Display the region to be zoomed in and the coordinates, if necessary
      // (the test is done within the "Paint" procedure)

      FCoords.Paint;
      FDims.Paint;
      FRegion.Paint;

      // Ensure that all GL commands are executed in a finite time...

      glFlush;
   End;
End;

//==============================================================================

Procedure TOGLGraphPanel.GLResize(aSender: TObject);
Begin
   If (Not FWinAPI) Then
      // Viewport

      glViewport(0, 0, Width, Height);

   UpdateGPCsts;
End;

//==============================================================================

Function TOGLGraphPanel.NeedQuickPaint: Boolean;
Var
   Iter: Integer;
Begin
   // Note: see "QuickPaint" for the reasoning behind the algorithm used here...

   Result := False;

   If (Not Enabled Or (QuickGraphsListSize = 0) Or Not FWinAPI) Then
      Exit;

   For Iter := 0 To QuickGraphsListSize-1 Do
      Result := Result Or QuickGraphsList[Iter].NeedQuickPaint;
End;

//==============================================================================

Procedure TOGLGraphPanel.QuickPaint;
Var
   Iter: Integer;
   PointsDrawn: Boolean;
Begin
   If (Not Enabled Or (QuickGraphsListSize = 0)) Then
      Exit;

   PointsDrawn := False;

   If (FWinAPI) Then Begin
      If (UpdateYAxis) Then
         ForceOGLRepaint
      Else Begin
         For Iter := 0 To QuickGraphsListSize-1 Do
            If (QuickGraphsList[Iter].QuickPaint = 1) Then
               PointsDrawn := True;

         NeedWinAPIBackupUpdate := NeedWinAPIBackupUpdate Or PointsDrawn;
         // Note: "PointsDrawn" is equal to "False" if none of the quick
         //       paintings has resulted in some points being drawn. In such a
         //       case, we must NOT update the backup image...
      End;
   End Else Begin
      If (UpdateYAxis) Then
         ForceOGLRepaint
      Else Begin
         ActivateRC;

         OGLDrawBuffer(GL_FRONT);
         // Specify to draw directly to the front buffer, in case this is necessary
         // (i.e. we were not quick painting before)

         For Iter := 0 To QuickGraphsListSize-1 Do
            If (QuickGraphsList[Iter].QuickPaint = 1) Then
               PointsDrawn := True;

//---GRY--- MAY WANT TO CHECK ABOUT SHARING THE RENDERING CONTEXT, OTHERWISE
//          VERY SLOW IF MORE THAN ONE GRAPH PANEL...

         If (PointsDrawn) Then
            // Ensure that all GL commands are executed in a finite time...

            glFlush;
      End;
   End;
End;

//==============================================================================

Procedure TOGLGraphPanel.FinaliseQuickPaint;
Begin
   If (Not FWinAPI) Then
      ForceOGLRepaint;
      // Note: this ensures that the graph panel is really up to date, since we
      //       quick paint, i.e. render to the front buffer for faster rendering
      //       (check "TOGLGraphPanel.QuickPaint"), so...
End;

//==============================================================================

Function TOGLGraphPanel.HasQuickGraphs: Boolean;
Begin
   Result := QuickGraphsListSize <> 0;
End;

//==============================================================================

Procedure TOGLGraphPanel.UpdateLeader(Const aLeader: TOGLGraphPanel);
Var
   OtherOGLGraphPanel: TOGLGraphPanel;
Begin
   Leader := aLeader;

   // Go through the connected graph panels and update them

   OtherOGLGraphPanel := ConnectedTo;

   While ((OtherOGLGraphPanel <> Nil) And (OtherOGLGraphPanel <> Self)) Do Begin
      OtherOGLGraphPanel.Leader := aLeader;

      OtherOGLGraphPanel := OtherOGLGraphPanel.ConnectedTo;
   End;
End;

//==============================================================================

Procedure TOGLGraphPanel.MouseDownCoords(aOGLGraphPanel: TOGLGraphPanel;
                                         aShift: TShiftState);
Var
   OtherOGLGraphPanel: TOGLGraphPanel;
Begin
   With aOGLGraphPanel Do Begin
      CoordsXVal := XOrigPt;
      CoordsYVal := Height-YOrigPt-1;

      FCoords.Snap := Not (ssCtrl In aShift);

      FCoords.BothAxes := True;

      FCoords.Active := True;

      IsShowingCoords := True;

      // Go through the connected graph panels and update them

      OtherOGLGraphPanel := ConnectedTo;

      While ((OtherOGLGraphPanel <> Nil) And (OtherOGLGraphPanel <> aOGLGraphPanel)) Do Begin
         With OtherOGLGraphPanel Do Begin
            // We are connected to another graph panel, so deal with it...

            Coords.BothAxes := False;

            Coords.Active := True;

            IsShowingCoords := True;
         End;

         OtherOGLGraphPanel := OtherOGLGraphPanel.ConnectedTo;
      End;

      ShowCoords;
   End;
End;

//==============================================================================

Procedure TOGLGraphPanel.MouseUpCoords(aOGLGraphPanel: TOGLGraphPanel;
                                       aShift: TShiftState);
Begin
   With aOGLGraphPanel Do Begin
      CoordsXVal := XOrigPt;
      CoordsYVal := Height-YOrigPt-1;

      FCoords.Snap := Not (ssCtrl In aShift);

      EndShowCoords;
   End;
End;

//==============================================================================

Procedure TOGLGraphPanel.InitPt1AndPt2;
Begin
   Pt1XVal := XOrigPt;
   Pt1YVal := Height-YOrigPt-1;

   Pt2XVal := Pt1XVal;
   Pt2YVal := Pt1YVal;
End;

//==============================================================================

Procedure TOGLGraphPanel.UpdatePt2(Const aX, aY: Integer);
Begin
   Pt2XVal := aX;
   Pt2YVal := Height-aY-1;
End;

//==============================================================================

Procedure TOGLGraphPanel.MouseDown(aButton: TMouseButton; aShift: TShiftState;
                                   aX, aY: Integer);
Begin
   If (Assigned(OnMouseDown)) Then
      OnMouseDown(Self, aButton, aShift, aX, aY);

   ShowPopupMenu := False;

   If ((aButton = mbLeft) And 
       (ssDouble In aShift) And Not (ssAlt In aShift) And Not (ssShift In aShift)) Then Begin
      If (ssCtrl In aShift) Then
         CopyToClipboard
      Else
         WinAPI := Not WinAPI;
         // Note: we must NOT use "FWinAPI", as some OpenGL related things are
         //       to be done when switching from the Windows API to OpenGL and
         //       vice versa...
   End Else Begin
      // Set the graph panel leader

      UpdateLeader(Self);

      // Deal with the mouse itself

      XOrigPt := aX;
      YOrigPt := aY;

      If ((aButton = mbLeft) And 
          Not (ssCtrl In aShift) And Not (ssAlt In aShift) And Not (ssShift In aShift)) Then Begin
         IsPanning := True;

         PanningTimer.Enabled := False;
      End Else If ((aButton = mbLeft) And 
                   Not (ssAlt In aShift) And (ssShift In aShift)) Then
         MouseDownCoords(Self, aShift)
      Else If ((aButton = mbLeft) And 
               (ssAlt In aShift) And (ssShift In aShift)) Then Begin
         InitPt1AndPt2;

         FDims.Snap := Not (ssCtrl In aShift);

         FDims.Active := True;

         IsShowingDims := True;

         ShowDims;
      End Else If (Not PanningTimer.Enabled And Not ZoomingTimer.Enabled And
                   (aButton = mbRight) And
                   (ssCtrl In aShift) And Not (ssAlt In aShift) And Not (ssShift In aShift)) Then Begin
         InitPt1AndPt2;

         ShowPopupMenu := True;

         FRegion.Active := True;

         IsZoomingRegion := True;

         ZoomRegion;
      End Else If ((aButton = mbRight) And
                   Not (ssCtrl In aShift) And Not (ssAlt In aShift) And Not (ssShift In aShift)) Then Begin
         InitPt1AndPt2;

         ShowPopupMenu := True;

         IsZooming := True;

         ZoomingTimer.Enabled := False;
      End Else If ((aButton = mbRight) And
                   Not (ssCtrl In aShift) And ((ssAlt In aShift) Or (ssShift In aShift))) Then Begin
         // Just to allow the popup menu when pressing the right button together
         // with the shift/alt key

         InitPt1AndPt2;

         ShowPopupMenu := True;

         IsPopupMenuing := True;
      End;

      If (IsPanning Or IsShowingCoords Or IsShowingDims Or IsZoomingRegion Or IsZooming Or IsPopupMenuing) Then Begin
         SetCaptureControl(Self);

         MouseCaptured := True;
      End;
   End;

   If (PopupMenu <> Nil) Then Begin
      OldAutoPopup := PopupMenu.AutoPopup;

      PopupMenu.AutoPopup := False;   // Just to be on the safe side
   End;
End;

//==============================================================================

Procedure TOGLGraphPanel.MouseMove(aShift: TShiftState; aX, aY: Integer);
   Function DistanceToMouse(Const aOGLGraphPanel: TOGLGraphPanel;
                            Const aPt: TPoint): Double;
   Var
      OverDistance: Double;
      OtherPtTopLeft, OtherPtBottomRight: TPoint;
   Begin
      OverDistance := 1.5*Sqrt(Sqr(GetSystemMetrics(SM_CXSCREEN))+Sqr(GetSystemMetrics(SM_CYSCREEN)));
      // Note: the "1.5*" is just to be on the safe side...

      With aOGLGraphPanel Do Begin
         // We are connected to another graph panel, so check whether we are
         // over it...

         OtherPtTopLeft     := ClientToScreen(Point(0, 0));
         OtherPtBottomRight := ClientToScreen(Point(Width, Height));

         If (aPt.X < OtherPtTopLeft.X) Then Begin
            If ((aPt.Y >= OtherPtTopLeft.Y) And (aPt.Y <= OtherPtBottomRight.Y)) Then
               // Left of the graph panel

               Result := OtherPtTopLeft.X-aPt.X
            Else
               // Top- or bottom-left of the graph panel

               Result := OverDistance;
         End Else If (aPt.X > OtherPtBottomRight.X) Then Begin
            If ((aPt.Y >= OtherPtTopLeft.Y) And (aPt.Y <= OtherPtBottomRight.Y)) Then
               // Right of the graph panel

               Result := aPt.X-OtherPtBottomRight.X
            Else
               // Top- or bottom-right of the graph panel

               Result := OverDistance;
         End Else Begin
            If (aPt.Y < OtherPtTopLeft.Y) Then
               // Top of the graph panel

               Result := OtherPtTopLeft.Y-aPt.Y
            Else If (aPt.Y > OtherPtBottomRight.Y) Then Begin
               // Bottom of the graph panel

               Result := aPt.Y-OtherPtBottomRight.Y
            End Else
               // Graph panel itself!

               Result := 0;
         End;
      End;
   End;
Var
   OtherOGLGraphPanel, NewLeader: TOGLGraphPanel;
   Pt: TPoint;
   MinDistance, Distance: Double;
   Cancelled: Boolean;
   IsTryingToPan, IsTryingToShowCoords, IsTryingToShowDims,
   IsTryingToZoomRegion, IsTryingToZoomIn, IsTryingToPopupMenu: Boolean;
Begin
   If (Assigned(OnMouseMove)) Then
      OnMouseMove(Self, aShift, aX, aY);

   If (MouseCaptured) Then Begin
      NewLeader := Leader;

      IsTryingToPan := IsPanning And
                       (ssLeft In aShift) And Not (ssRight In aShift) And
                       Not (ssCtrl In aShift) And Not (ssAlt In aShift) And Not (ssShift In aShift);


      IsTryingToShowCoords := IsShowingCoords And
                              (ssLeft In aShift) And Not (ssRight In aShift) And
                              Not (ssAlt In aShift) And (ssShift In aShift);

      IsTryingToShowDims := IsShowingDims And
                            (ssLeft In aShift) And Not (ssRight In aShift) And
                            (ssAlt In aShift) And (ssShift In aShift);

      IsTryingToZoomRegion := IsZoomingRegion And
                              Not (ssLeft In aShift) And (ssRight In aShift) And
                              (ssCtrl In aShift) And Not (ssAlt In aShift) And Not (ssShift In aShift);

      IsTryingToZoomIn := IsZooming And
                          Not (ssLeft In aShift) And (ssRight In aShift) And
                          Not (ssCtrl In aShift) And Not (ssAlt In aShift) And Not (ssShift In aShift);

      IsTryingToPopupMenu := IsPopupMenuing And
                             Not (ssLeft In aShift) And (ssRight In aShift) And
                             Not (ssCtrl In aShift) And ((ssAlt In aShift) Or (ssShift In aShift));

      If (IsTryingToShowCoords And
          Not (IsTryingToPan Or IsTryingToShowDims Or
               IsTryingToZoomRegion Or IsTryingToZoomIn Or
               IsTryingToPopupMenu)) Then Begin
         Pt := ClientToScreen(Point(aX, aY));

         Distance := DistanceToMouse(Self, Pt);

         If (Distance > 0) Then Begin
            // Go through the connected graph panels and determine the one that
            // is nearest to the mouse position

            MinDistance := Distance;

            OtherOGLGraphPanel := ConnectedTo;

            While ((OtherOGLGraphPanel <> Nil) And (OtherOGLGraphPanel <> Self)) Do Begin
               Distance := DistanceToMouse(OtherOGLGraphPanel, Pt);

               If (Distance < MinDistance) Then Begin
                  MinDistance := Distance;

                  NewLeader := OtherOGLGraphPanel;
               End;

               OtherOGLGraphPanel := OtherOGLGraphPanel.ConnectedTo;
            End;
         End;
      End;

      // Deal with the new leader, if any, or deal with the current one by
      // processing the mouse movement

      If (NewLeader <> Leader) Then Begin
         With NewLeader.ScreenToClient(Self.ClientToScreen(Point(aX, aY))) Do Begin
            // Finish showing the coordinates of the current leader

            MouseUpCoords(Self, aShift);

            MouseCaptured := False;

            // Get ready for the new leader

            SetCaptureControl(NewLeader);

            UpdateLeader(NewLeader);

            NewLeader.XOrigPt := X;
            NewLeader.YOrigPt := Y;

            MouseDownCoords(NewLeader, aShift);

            NewLeader.MouseCaptured := True;
         End;
      End Else Begin
         If (IsTryingToPan) Then Begin
            TransXVal := XOrigPt-aX;
            TransYVal := aY-YOrigPt;

            Pan;
         End Else If (IsTryingToShowCoords) Then Begin
            CoordsXVal := XOrigPt;
            CoordsYVal := Height-YOrigPt-1;

            FCoords.Snap := Not (ssCtrl In aShift);

            ShowCoords;
         End Else If (IsTryingToShowDims) Then Begin
            UpdatePt2(aX, aY);

            FDims.Snap := Not (ssCtrl In aShift);

            ShowDims;
         End Else If (IsTryingToZoomRegion) Then Begin
            UpdatePt2(aX, aY);

            ZoomRegion;
         End Else If (IsTryingToZoomIn) Then Begin
            UpdatePt2(aX, aY);

            ZoomXVal := XOrigPt-aX;
            ZoomYVal := YOrigPt-aY;

            Zoom;
         End Else If (IsTryingToPopupMenu) Then
            UpdatePt2(aX, aY)
         Else Begin
            // Nothing that we can recognise, so cancel whatever we were 
            // doing...

            Cancelled := False;

            If (IsShowingCoords) Then Begin
               EndShowCoords;

               Cancelled := True;
            End Else If (IsShowingDims) Then Begin
               EndShowDims;

               Cancelled := True;
            End Else If (IsZoomingRegion) Then Begin
               FRegion.X2 := FRegion.X1;   // Will disable
               FRegion.Y2 := FRegion.Y1;   // the region zooming

               ShowPopupMenu := EndZoomRegion And ShowPopupMenu;

               Cancelled := True;
            End;

            If (Cancelled) Then Begin
               MouseCaptured := False;

               SetCaptureControl(Nil);
            End;
         End;

         ShowPopupMenu := ShowPopupMenu And (Pt1XVal = Pt2XVal) And (Pt1YVal = Pt2YVal);

         XOrigPt := aX;
         YOrigPt := aY;
      End;
   End;
End;

//==============================================================================

Procedure TOGLGraphPanel.MouseUp(aButton: TMouseButton; aShift: TShiftState;
                                 aX, aY: Integer);
Var
   CanReleaseMouse: Boolean;
Begin
   CanReleaseMouse := False;

   If (MouseCaptured) Then Begin
      // Start the automatic tranlsating/rotating/zooming, depending on the case

      If (IsPanning And
          (aButton = mbLeft) And 
          Not (ssCtrl In aShift) And Not (ssAlt In aShift) And Not (ssShift In aShift)) Then Begin
         PanningTimer.Enabled := (Abs(TransXVal) > 1) Or (Abs(TransYVal) > 1);

         If ((FConnectedTo <> Nil) And PanningTimer.Enabled) Then
            // We do NOT want to allow the automatic panning if the connected
            // graph panel is doing exactly that!

            PanningTimer.Enabled := Not FConnectedTo.PanningTimer.Enabled;

         IsPanning := False;

         CanReleaseMouse := True;
      End Else If (IsShowingCoords And
                   (aButton = mbLeft) And 
                   Not (ssAlt In aShift) And (ssShift In aShift)) Then Begin
         MouseUpCoords(Self, aShift);

         CanReleaseMouse := True;
      End Else If (IsShowingDims And
                   (aButton = mbLeft) And 
                   (ssAlt In aShift) And (ssShift In aShift)) Then Begin
         UpdatePt2(aX, aY);

         FDims.Snap := Not (ssCtrl In aShift);

         EndShowDims;

         CanReleaseMouse := True;
      End Else If (IsZoomingRegion And
                   (aButton = mbRight) And
                   (ssCtrl In aShift) And Not (ssAlt In aShift) And Not (ssShift In aShift)) Then Begin
         UpdatePt2(aX, aY);

         ShowPopupMenu := EndZoomRegion And ShowPopupMenu;

         CanReleaseMouse := True;
      End Else If (IsZooming And
                   (aButton = mbRight) And
                   Not (ssCtrl In aShift) And Not (ssAlt In aShift) And Not (ssShift In aShift)) Then Begin
         UpdatePt2(aX, aY);

         ZoomingTimer.Enabled := (Abs(ZoomXVal) > 2) Or (Abs(ZoomYVal) > 2);

         If ((FConnectedTo <> Nil) And ZoomingTimer.Enabled) Then
            // We do NOT want to allow the automatic zooming if the connected
            // graph panel is doing exactly that!

            ZoomingTimer.Enabled := Not FConnectedTo.ZoomingTimer.Enabled;

         IsZooming := False;

         CanReleaseMouse := True;
      End Else If (IsPopupMenuing And
                   (aButton = mbRight) And
                   Not (ssCtrl In aShift) And ((ssAlt In aShift) Or (ssShift In aShift))) Then Begin
         UpdatePt2(aX, aY);

         IsPopupMenuing := False;

         CanReleaseMouse := True;
      End;

      ShowPopupMenu := ShowPopupMenu And (Pt1XVal = Pt2XVal) And (Pt1YVal = Pt2YVal);

      If (CanReleaseMouse) Then Begin
         MouseCaptured := False;

         SetCaptureControl(Nil);
      End;
   End;

   If (PopupMenu <> Nil) Then Begin
      If (ShowPopupMenu) Then 
         With Mouse.CursorPos Do
            PopupMenu.Popup(X, Y);

      PopupMenu.AutoPopup := OldAutoPopup;
   End;

   If (Assigned(OnMouseUp)) Then
      OnMouseUp(Self, aButton, aShift, aX, aY);
End;

//==============================================================================

Procedure TOGLGraphPanel.ComputePtsAndRepaint(Const aRepaint: Boolean);
Begin
   FXAxis.ComputePts;
   FYAxis.ComputePts;

   If (aRepaint) Then
      ForceOGLRepaint;
End;

//==============================================================================

Procedure TOGLGraphPanel.Pan;
Var
   NeedRepainting: Boolean;
   OldEnabled: Boolean;
Begin
   NeedRepainting := False;

   OldEnabled := Enabled;

   Enabled := False;

   If (((TransXVal < 0) And Not FXAxis.OnTheMinEdge) Or
       ((TransXVal > 0) And Not FXAxis.OnTheMaxEdge)) Then Begin
      FXAxis.Start := FXAxis.Start+(FXAxis.Length*TransXVal)*One_Width;

      NeedRepainting := True;
   End;

   If (((TransYVal < 0) And Not FYAxis.OnTheMinEdge) Or
       ((TransYVal > 0) And Not FYAxis.OnTheMaxEdge)) Then Begin
      FYAxis.Start := FYAxis.Start+(FYAxis.Length*TransYVal)*One_Height;

      NeedRepainting := True;
   End;

   If (FXAxis.OnTheMinEdge Or FXAxis.OnTheMaxEdge) Then
      TransXVal := 0;

   If (FYAxis.OnTheMinEdge Or FYAxis.OnTheMaxEdge) Then
      TransYVal := 0;

   If ((TransXVal = 0) And (TransYVal = 0)) Then
      PanningTimer.Enabled := False;

   Enabled := OldEnabled;

   If (NeedRepainting) Then
      ForceOGLRepaint;
End;

//==============================================================================

Procedure TOGLGraphPanel.PanningAutomatically(aSender: TObject);
Begin
   Pan;
End;

//==============================================================================

Function TOGLGraphPanel.ConvWinXToOpenGLX(Const aWinX: Double): Double;
Begin
   // Note: in the case of an OpenGL rendering, to use "gluUnProject" should
   //       yield a more accurate result, but the fact is that it doesn't return
   //       a meaningful value... despite "gluUnProject" returning "GL_TRUE",
   //       i.e. giving the impression that everything's ok... So, in the end
   //       we just go for the WinAPI version for both WinAPI and OpenGL...

   Result := FXAxis.Start+aWinX*FXAxis.Length*One_Width;
End;

//==============================================================================

Function TOGLGraphPanel.ConvWinYToOpenGLY(Const aWinY: Double): Double;
Begin
   // Note: see comment in "ConvWinXToOpenGLX"...

   Result := FYAxis.Start+aWinY*FYAxis.Length*One_Height;
End;

//==============================================================================

Function TOGLGraphPanel.ConvWinXSizeToOpenGLXSize(Const aWinXSize: Double): Double;
Begin
   Result := ConvWinXToOpenGLX(aWinXSize)-FXAxis.Start;
End;

//==============================================================================

Function TOGLGraphPanel.ConvWinYSizeToOpenGLYSize(Const aWinYSize: Double): Double;
Begin
   Result := ConvWinYToOpenGLY(aWinYSize)-FYAxis.Start;
End;

//==============================================================================

Function TOGLGraphPanel.ConvWinTextWidthToOpenGLTextWidth(Const aString: String): Double;
Begin
   Result := ConvWinXSizeToOpenGLXSize(DestCanvas.TextWidth(aString)+1);
   // Note: the "+1" is for allowing one pixel to the right
End;

//==============================================================================

Function TOGLGraphPanel.ConvWinTextHeightToOpenGLTextHeight(Const aString: String): Double;
Begin
   Result := ConvWinYSizeToOpenGLYSize(DestCanvas.TextHeight(aString));
End;

//==============================================================================

Procedure TOGLGraphPanel.UpdateGPCsts;
Begin
   One_Width  := 1/Width;
   One_Height := 1/Height;

   If (Assigned(FXAxis)) Then
      ConvOpenGLXToWinXCst := (Width-1)/FXAxis.Length;

   ConvOpenGLYToWinYCst1 := Height-1;

   If (Assigned(FYAxis)) Then
      ConvOpenGLYToWinYCst2 := 1/FYAxis.Length;
End;

//==============================================================================

Function TOGLGraphPanel.ConvOpenGLXToWinX(Const aOpenGLX: Double): Integer;
Begin
   // Note: see comment in "ConvWinXToOpenGLX"...

   Result := Trunc(ConvOpenGLXToWinXCst*(aOpenGLX-FXAxis.Start));
End;

//==============================================================================

Function TOGLGraphPanel.ConvOpenGLYToWinY(Const aOpenGLY: Double): Integer;
   Function InternalConvOpenGLYToWinY(Const aYAxisStart, aOpenGLY, aConvOpenGLYToWinYCst1, aConvOpenGLYToWinYCst2: Double): Integer; Inline;
   Begin
      Result := Trunc(aConvOpenGLYToWinYCst1*(1-(aOpenGLY-aYAxisStart)*aConvOpenGLYToWinYCst2));
   End;
Begin
   // Note: see comment in "ConvWinXToOpenGLX"...

   Try
      Result := InternalConvOpenGLYToWinY(FYAxis.Start, aOpenGLY, ConvOpenGLYToWinYCst1, ConvOpenGLYToWinYCst2);
   Except
      // "aOpenGLY" is equal to +INF or -INF, so...

      If (Sign(aOpenGLY) = 1) Then
         // +INF, so pretend that we are dealing with "FYMax"

         Result := InternalConvOpenGLYToWinY(FYAxis.Start, FYMax, ConvOpenGLYToWinYCst1, ConvOpenGLYToWinYCst2)
      Else
         // -INF, so pretend that we are dealing with "FYMin"

         Result := InternalConvOpenGLYToWinY(FYAxis.Start, FYMin, ConvOpenGLYToWinYCst1, ConvOpenGLYToWinYCst2);
   End;
End;

//==============================================================================

Function TOGLGraphPanel.ConvOpenGLXSizeToWinXSize(Const aOpenGLXSize: Double): Integer;
Begin
   Result := ConvOpenGLXToWinX(aOpenGLXSize)-ConvOpenGLXToWinX(0);
End;

//==============================================================================

Function TOGLGraphPanel.ConvOpenGLYSizeToWinYSize(Const aOpenGLYSize: Double): Integer;
Begin
   Result := ConvOpenGLYToWinY(0)-ConvOpenGLYToWinY(aOpenGLYSize);
End;

//==============================================================================

Procedure TOGLGraphPanel.ShowCoords;
Var
   OtherOGLGraphPanel: TOGLGraphPanel;
Begin
   // Convert the coordinates from Windows to OpenGL

   FCoords.X := ConvWinXToOpenGLX(CoordsXVal);

   If (FCoords.Snap) Then
      FCoords.Y := ClosestY(CoordsXVal, CoordsYVal)
   Else
      FCoords.Y := ConvWinYToOpenGLY(CoordsYVal);

   // Check the coordinates are not outside the given region (occurs when moving
   // the mouse outside the control)

   If (FCoords.X < FXAxis.Start) Then
      FCoords.X := FXAxis.Start;

   If (FCoords.X > (FXAxis.Start+FXAxis.Length)) Then
      FCoords.X := FXAxis.Start+FXAxis.Length;

   If (FCoords.Y < FYAxis.Start) Then
      FCoords.Y := FYAxis.Start;

   If (FCoords.Y > (FYAxis.Start+FYAxis.Length)) Then
      FCoords.Y := FYAxis.Start+FYAxis.Length;

   OGLRepaint;   // Note: no need for "ForceOGLRepaint", since we just draw the
                 // overlay layer

   // Go through the connected graph panels and update them

   OtherOGLGraphPanel := ConnectedTo;

   While ((OtherOGLGraphPanel <> Nil) And (OtherOGLGraphPanel <> Self)) Do Begin
      With OtherOGLGraphPanel Do Begin
         Coords.X := Self.Coords.X;

         OGLRepaint;
      End;

      OtherOGLGraphPanel := OtherOGLGraphPanel.ConnectedTo;
   End;
End;

//==============================================================================

Procedure TOGLGraphPanel.EndShowCoords;
Var
   OtherOGLGraphPanel: TOGLGraphPanel;
Begin
   ShowCoords;

   FCoords.Active := False;

   IsShowingCoords := False;

   OGLRepaint;   // Note: no need for "ForceOGLRepaint", since we just draw the
                 //       overlay layer

   // Go through the connected graph panels and update them

   OtherOGLGraphPanel := ConnectedTo;

   While ((OtherOGLGraphPanel <> Nil) And (OtherOGLGraphPanel <> Self)) Do Begin
      With OtherOGLGraphPanel Do Begin
         // We are connected to another graph panel, so deal with it...

         Coords.Active := False;

         IsShowingCoords := False;

         OGLRepaint;
      End;

      OtherOGLGraphPanel := OtherOGLGraphPanel.ConnectedTo;
   End;
End;

//==============================================================================

Procedure TOGLGraphPanel.ShowDims;
Var
   OGLGraphIter: Integer;
Begin
   // Convert the coordinates from Windows to OpenGL
   // Note: we check the second point first, because we want to be free to go
   //       from one graph to another without having to restart the dimensions
   //       feature...

   FDims.X2 := ConvWinXToOpenGLX(Pt2XVal);

   If (FDims.Snap) Then
      FDims.Y2 := ClosestY(Pt2XVal, Pt2YVal, OGLGraphIter)
   Else
      FDims.Y2 := ConvWinYToOpenGLY(Pt2YVal);

   FDims.X1 := ConvWinXToOpenGLX(Pt1XVal);

   If (FDims.Snap) Then Begin
      If (OGLGraphIter <> FGraphsListSize) Then
         FDims.Y1 := ClosestYGraph(Pt1XVal, Pt1YVal, OGLGraphIter)
      Else
         FDims.Y1 := ClosestY(Pt1XVal, Pt1YVal);
   End Else
      FDims.Y1 := ConvWinYToOpenGLY(Pt1YVal);

   // Check the dimensions' coordinates are not outside the given region (occurs
   // when moving the mouse outside the control)

   If (FDims.X1 < FXAxis.Start) Then
      FDims.X1 := FXAxis.Start
   Else If (FDims.X1 > (FXAxis.Start+FXAxis.Length)) Then
      FDims.X1 := FXAxis.Start+FXAxis.Length;

   If (FDims.Y1 > (FYAxis.Start+FYAxis.Length)) Then
      FDims.Y1 := FYAxis.Start+FYAxis.Length
   Else If (FDims.Y1 < FYAxis.Start) Then
      FDims.Y1 := FYAxis.Start;

   If (FDims.X2 < FXAxis.Start) Then
      FDims.X2 := FXAxis.Start
   Else If (FDims.X2 > (FXAxis.Start+FXAxis.Length)) Then
      FDims.X2 := FXAxis.Start+FXAxis.Length;

   If (FDims.Y2 > (FYAxis.Start+FYAxis.Length)) Then
      FDims.Y2 := FYAxis.Start+FYAxis.Length
   Else If (FDims.Y2 < FYAxis.Start) Then
      FDims.Y2 := FYAxis.Start;

   OGLRepaint;   // Note: no need for "ForceOGLRepaint", since we just draw the
                 //       overlay layer
End;

//==============================================================================

Procedure TOGLGraphPanel.EndShowDims;
Begin
   ShowDims;

   FDims.Active := False;

   IsShowingDims := False;

   OGLRepaint;   // Note: no need for "ForceOGLRepaint", since we just draw the
                 //       overlay layer
End;

//==============================================================================

Procedure TOGLGraphPanel.ZoomRegion;
Var
   Temp: Double;
Begin
   If (Not FXAxis.CanZoomIn And Not FYAxis.CanZoomIn) Then
      Exit;

   // Convert the coordinates from Windows to OpenGL

   If (FXAxis.CanZoomIn) Then Begin
      FRegion.X1 := ConvWinXToOpenGLX(Pt1XVal);
      FRegion.X2 := ConvWinXToOpenGLX(Pt2XVal);
   End Else Begin
      FRegion.X1 := ConvWinXToOpenGLX(0);
      FRegion.X2 := ConvWinXToOpenGLX(Width-1);
   End;

   If (FYAxis.CanZoomIn) Then Begin
      FRegion.Y1 := ConvWinYToOpenGLY(Pt1YVal);
      FRegion.Y2 := ConvWinYToOpenGLY(Pt2YVal);
   End Else Begin
      FRegion.Y1 := ConvWinYToOpenGLY(0);
      FRegion.Y2 := ConvWinYToOpenGLY(Height-1);
   End;

   // Make sure that the Pt1 is the top-left point and Pt2 the bottom-right one

   If (FRegion.X1 > FRegion.X2) Then Begin
      Temp := FRegion.X2;

      FRegion.X2 := FRegion.X1;
      FRegion.X1 := Temp;
   End;

   If (FRegion.Y1 < FRegion.Y2) Then Begin
      Temp := FRegion.Y2;

      FRegion.Y2 := FRegion.Y1;
      FRegion.Y1 := Temp;
   End;

   // Check the region's coordinates are not outside the given region (occurs
   // when moving the mouse outside the control)

   If (FRegion.X1 < FXAxis.Start) Then
      FRegion.X1 := FXAxis.Start;

   If (FRegion.X2 > (FXAxis.Start+FXAxis.Length)) Then
      FRegion.X2 := FXAxis.Start+FXAxis.Length;

   If (FRegion.Y1 > (FYAxis.Start+FYAxis.Length)) Then
      FRegion.Y1 := FYAxis.Start+FYAxis.Length;

   If (FRegion.Y2 < FYAxis.Start) Then
      FRegion.Y2 := FYAxis.Start;

   OGLRepaint;   // Note: no need for "ForceOGLRepaint", since we just draw the
                 //       overlay layer
End;

//==============================================================================

Function TOGLGraphPanel.EndZoomRegion: Boolean;
Var
   OldEnabled: Boolean;
Begin
   Result := (FRegion.X1 <> FRegion.X2) And (FRegion.Y1 <> FRegion.Y2);

   If (Result) Then Begin
      ZoomRegion;

      // Zoom the region in
      // Note: make sure that we get up to date information from the mouse

      OldEnabled := Enabled;

      Enabled := False;

      If (FXAxis.CanZoomIn) Then Begin
         FXAxis.Length := FRegion.X2-FRegion.X1;
         FXAxis.Start  := FRegion.X1;
      End;

      If (FYAxis.CanZoomIn) Then Begin
         FYAxis.Length := FRegion.Y1-FRegion.Y2;
         FYAxis.Start  := FRegion.Y2;
      End;

      Enabled := OldEnabled;

      UpdateGPCsts;   // Just to be on the safe side...
   End;

   FRegion.Active := False;

   IsZoomingRegion := False;

   OGLRepaint;   // Note: no need for "ForceOGLRepaint", since we just draw the
                 //       overlay layer
End;

//==============================================================================

Procedure TOGLGraphPanel.Zoom;
Var
   NeedRepainting: Boolean;
   OldEnabled: Boolean;
Begin
   NeedRepainting := False;

   OldEnabled := Enabled;

   Enabled := False;

   If (ZoomAxis(FXAxis, ZoomXVal)) Then
      NeedRepainting := True;

   If (ZoomAxis(FYAxis, ZoomYVal)) Then
      NeedRepainting := True;

   If ((ZoomXVal = 0) And (ZoomYVal = 0)) Then
      ZoomingTimer.Enabled := False;

   Enabled := OldEnabled;

   If (NeedRepainting) Then
      ForceOGLRepaint;
End;

//==============================================================================

Function TOGLGraphPanel.ZoomAxis(Const aAxis: TOGLGPAxis;
                                 Var aZoomVal: Integer): Boolean;
Var
   LocZoomFactor: Double;
   OldLength: Double;
Begin
   Result := False;

   If ((aZoomVal <= -1) And aAxis.CanZoomIn) Then
      LocZoomFactor := ZOOM_IN_FACTOR
   Else If ((aZoomVal >= 1) And aAxis.CanZoomOut) Then
      LocZoomFactor := ZOOM_OUT_FACTOR
   Else Begin
      aZoomVal := 0;

      Exit;
   End;

   OldLength := aAxis.Length;

   aAxis.Length := LocZoomFactor*aAxis.Length;

   aAxis.Start := aAxis.Start+0.5*(OldLength-aAxis.Length);

   If (((LocZoomFactor = ZOOM_IN_FACTOR) And Not aAxis.CanZoomIn) Or
       ((LocZoomFactor = ZOOM_OUT_FACTOR) And Not aAxis.CanZoomOut)) Then
      aZoomVal := 0;

   Result := True;
End;

//==============================================================================

Procedure TOGLGraphPanel.ZoomingAutomatically(aSender: TObject);
Begin
   Zoom;
End;

//==============================================================================

Function TOGLGraphPanel.AddGraph(Const aName, aLegend: String;
                                 Const aShift: Integer;
                                 Const aLength, aXRes: Double): TOGLGPGraph;
Begin
   // Add the graph to the list of graphs

   If (FGraphsListSize+1 > Length(FGraphsList)) Then
      // Not enough space, so make some...

      SetLength(FGraphsList, 2*Length(FGraphsList));

   Result := TOGLGPGraph.Create(Self, aName, aLegend, aShift, aLength, aXRes);

   FGraphsList[FGraphsListSize] := Result;

   Inc(FGraphsListSize);
End;

//==============================================================================

Procedure TOGLGraphPanel.RemoveGraph(Const aName: String);
   Function RemoveGraphFromList(Const aName: String;
                                Var aList: TArrayOfOGLGPGraph;
                                Var aListSize: Integer): TOGLGPGraph;
   Var
      I, OGLGraphIter: Integer;
   Begin
      OGLGraphIter := -1;

      For I := 0 To aListSize-1 Do
         If (CompareStr(aName, aList[I].Name) = 0) Then Begin
            OGLGraphIter := I;

            Break;
         End;

      If (OGLGraphIter <> -1) Then Begin
         Result := aList[OGLGraphIter];

         For I := OGLGraphIter+1 To aListSize-1 Do
            aList[I-1] := aList[I];

         Dec(aListSize);
      End Else
         Result := Nil;
   End;
Begin
   // First, remove the graph from the list of quick graphs, if necessary

   RemoveGraphFromList(aName, QuickGraphsList, QuickGraphsListSize);

   // Second, remove the graph from the list of graphs and free it

   RemoveGraphFromList(aName, FGraphsList, FGraphsListSize).Free;

   NeedWinAPIBackupUpdate := True;   // We never know, so...

   NeedUpdateYMinAndYMax := True;   // Again, we never know, so...
End;

//==============================================================================

Procedure TOGLGraphPanel.RemoveAllGraphs;
Begin
   QuickGraphsListSize := 0;
   FGraphsListSize := 0;

   NeedWinAPIBackupUpdate := True;   // We never know, so...

   NeedUpdateYMinAndYMax := True;   // Again, we never know, so...
End;

//==============================================================================

Function TOGLGraphPanel.NbOfGraphs: Integer;
Begin
   Result := FGraphsListSize;
End;

//==============================================================================

Function TOGLGraphPanel.NbOfValidGraphs: Integer;
Var
   Iter: Integer;
Begin
   Result := 0;

   For Iter := 0 To FGraphsListSize-1 Do
      If (FGraphsList[Iter].IsValid) Then
         Inc(Result);
End;

//==============================================================================

Function GraphDataCompare(aPtr: Pointer; Const aObj1, aObj2: DObject): Integer; Inline;
Begin
   Result := CompareStr(TGraphData(aObj1.VObject).Legend, TGraphData(aObj2.VObject).Legend);
End;

//==============================================================================

Constructor TGraphData.Create(Const aLegend: String; Const aShift: Integer;
                              Const aXRes: Double;
                              Const aData: TArrayOfTOGLGPData;
                              Const aDataSize: Integer);
Begin
   Exported := False;

   Legend := aLegend;

   Shift := aShift;
   XRes  := aXRes;
   Iter  := 0;
   Data  := aData;
   Size  := aDataSize;
End;

//==============================================================================

Class Procedure TOGLGraphPanel.ExportToCSV(Const aOGLGraphPanels: Array Of TOGLGraphPanel;
                                           Const aFileName: String);
Var
   TextFileID: TextFile;
   CrtNbOfGraphs, TotalNbOfGraphs: Integer;
   GPIter, Iter: Integer;
   GraphsData: DArray;
   DataX, CrtX, NextX: Double;
   GraphOrGraphs: String;
   GraphData: TGraphData;
Begin
   GraphsData := DArray.CreateWith(MakeComparator(GraphDataCompare));

   // Determine the abscissa of the further left point among the different
   // graphs

{$IFDEF OPT_MATH}
   NextX := OptMaxDouble;
{$ELSE}
   NextX := MaxDouble;
{$ENDIF}

   For GPIter := 0 To High(aOGLGraphPanels) Do
      For Iter := 0 To aOGLGraphPanels[GPIter].GraphsListSize-1 Do
         With aOGLGraphPanels[GPIter].GraphsList[Iter] Do
            // First point

            If (Length(Data) <> 0) Then Begin
               DataX := Shift*XRes;

               If (NextX > DataX) Then
                  NextX := DataX;
            End;

   // Determine the total number of graphs to export

   TotalNbOfGraphs := 0;

   For GPIter := 0 To High(aOGLGraphPanels) Do
      TotalNbOfGraphs := TotalNbOfGraphs+aOGLGraphPanels[GPIter].GraphsListSize;

   CrtNbOfGraphs := TotalNbOfGraphs;

   // Output the data

   Try
      // Prepare the CSV file

      AssignFile(TextFileID, aFileName);

      Rewrite(TextFileID);

      // Get the various graphs iterator ready, as well as output the header for
      // the CSV file

      // Time legend (so only get that from the first graph panel indeed)

      Write(TextFileID, aOGLGraphPanels[0].Legend);

      For GPIter := 0 To High(aOGLGraphPanels) Do
         For Iter := 0 To aOGLGraphPanels[GPIter].GraphsListSize-1 Do
            With aOGLGraphPanels[GPIter].GraphsList[Iter] Do
               GraphsData.Add([TGraphData.Create(Legend, Shift, XRes, Data, DataSize)]);

      Sort(GraphsData);

      For Iter := 0 To GraphsData.Size-1 Do
         Write(TextFileID, ', '+TGraphData(GraphsData.At(Iter).VObject).Legend);

      WriteLn(TextFileID);

      // Output the data

      Repeat
         // Scan through the graphs

         CrtX  := NextX;
{$IFDEF OPT_MATH}
         NextX := OptMaxDouble;
{$ELSE}
         NextX := MaxDouble;
{$ENDIF}

         Write(TextFileID, FloatToStr(CrtX));

         For Iter := 0 To GraphsData.Size-1 Do Begin
            GraphData := TGraphData(GraphsData.At(Iter).VObject);

            If (GraphData.Iter <> GraphData.Size) Then Begin
               DataX := (GraphData.Shift+GraphData.Iter)*GraphData.XRes;

               If (CrtX = DataX) Then Begin
                  // The graph's current point's abscissa is of the right
                  // value, so export it...

                  Write(TextFileID, ', '+FloatToStr(GraphData.Data[GraphData.Iter]));

                  Inc(GraphData.Iter);

                  If (GraphData.Iter <> GraphData.Size) Then Begin
                     // Check whether the graph's next point's abscissa is of
                     // the right value or not...

                     DataX := (GraphData.Shift+GraphData.Iter)*GraphData.XRes;

                     If (NextX > DataX) Then
                        NextX := DataX;
                  End;
               End Else Begin
                  // The graph's current point's abscissa is not of the right
                  // value, so just output a blank and check whether it might
                  // be in the future...

                  Write(TextFileID, ', ');

                  If (NextX > DataX) Then
                     NextX := DataX;
               End;
            End Else
               // We have already exported the graph, so just output a blank...

               Write(TextFileID, ', ');

            If (Not GraphData.Exported And (GraphData.Iter = GraphData.Size)) Then Begin
               // The CSV export of the graph has just been completed, so...

               GraphData.Exported := True;

               Dec(CrtNbOfGraphs);
            End;
         End;

         WriteLn(TextFileID);
      Until CrtNbOfGraphs = 0;

      // Close the CSV file

      CloseFile(TextFileID);
   Except
      If (TotalNbOfGraphs = 1) Then
         GraphOrGraphs := 'graph'
      Else
         GraphOrGraphs := 'graphs';

      MessageDlg('The '+GraphOrGraphs+' cannot be exported to '''+aFileName+'''.', mtError, [mbOK], 0);
   End;

   FreeAndClear(GraphsData);
End;

//==============================================================================

Procedure Register;
Begin
   RegisterComponents('COR', [TOGLGraphPanel]);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

