//==============================================================================
// Engine class
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 15/05/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit Engine;

//==============================================================================

Interface

//==============================================================================

Uses
   Dialogs, DeCAL;

//==============================================================================

Type
   TXMLNextItemType = (xitNone, xitSame, xitOther);
   TMathMLOpType = (motOperand, motFunc, motOwn);
   TEngineMsg = Class
      Public
         // Published properties

         MsgType: TMsgDlgType;
         FileName: String;
         LineNb, ColNb: Integer;
         Msg: String;

         // Constructor & Destructor

         Constructor Create(Const aMsgType: TMsgDlgType; Const aFileName: String; Const aLineNb, aColNb: Integer; Const aMsg: String);

         // User's methods

         Class Procedure SortAndRemoveDuplicates(Const aMessages: DArray);

         Function Equals(Const aEngineMsg: TEngineMsg): Boolean; Reintroduce; Inline;
   End;

//==============================================================================

Function EngineMsgCompare(aPtr: Pointer; Const aObj1, aObj2: DObject): Integer; Inline;

//==============================================================================

Implementation

//==============================================================================

Uses
   SysUtils;

//==============================================================================

Constructor TEngineMsg.Create(Const aMsgType: TMsgDlgType;
                              Const aFileName: String;
                              Const aLineNb, aColNb: Integer;
                              Const aMsg: String);
Begin
   MsgType  := aMsgType;
   FileName := aFileName;
   LineNb   := aLineNb;
   ColNb    := aColNb;
   Msg      := aMsg;
End;

//==============================================================================

Class Procedure TEngineMsg.SortAndRemoveDuplicates(Const aMessages: DArray);
Var
   Iter: Integer;
   NextEngineMsg: TEngineMsg;
Begin
   Sort(aMessages);

   If (aMessages.Size > 1) Then Begin
      // Remove duplicates

      Iter := 0;

      Repeat
         NextEngineMsg := TEngineMsg(aMessages.At(Iter+1).VObject);

         If (TEngineMsg(aMessages.At(Iter).VObject).Equals(NextEngineMsg)) Then Begin
            aMessages.RemoveAt(Iter+1);

            NextEngineMsg.Free;
         End Else
            Inc(Iter);
      Until Iter = aMessages.Size-1;
   End;
End;

//==============================================================================

Function TEngineMsg.Equals(Const aEngineMsg: TEngineMsg): Boolean;
Begin
   Result := (MsgType = aEngineMsg.MsgType) And
             (CompareStr(FileName, aEngineMsg.FileName) = 0) And
             (LineNb = aEngineMsg.LineNb) And (ColNb = aEngineMsg.ColNb) And
             (CompareStr(Msg, aEngineMsg.Msg) = 0);
End;

//==============================================================================

Function EngineMsgCompare(aPtr: Pointer; Const aObj1, aObj2: DObject): Integer;
Var
   Msg1, Msg2: TEngineMsg;
Begin
   Msg1 := TEngineMsg(aObj1.VObject);
   Msg2 := TEngineMsg(aObj2.VObject);

   If (Msg1.LineNb > Msg2.LineNb) Then
      Result := 1
   Else If (Msg1.LineNb < Msg2.LineNb) Then
      Result := -1
   Else Begin
      If (Msg1.ColNb > Msg2.ColNb) Then
         Result := 1
      Else If (Msg1.ColNb < Msg2.ColNb) Then
         Result := -1
      Else
         Result := CompareStr(Msg1.Msg, Msg2.Msg);
   End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

