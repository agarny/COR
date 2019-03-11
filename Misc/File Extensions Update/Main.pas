unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, XPMan;

type
  TMainForm = class(TForm)
    MessagePanel: TPanel;
    MessageVal: TRichEdit;
    UpdateBtn: TButton;
    CloseBtn: TButton;
    ProgressBar: TProgressBar;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdateBtnClick(Sender: TObject);
  private
    { Private declarations }

    Procedure WMDropFiles(Var aMsg: TWMDropFiles); Message WM_DROPFILES;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
   ShellAPI, Registry;

Const
   _COR_ = '.cor';
   _CORPROJ_ = '.corproj';
   _CML_ = '.cml';
   _CELLML_ = '.cellml';

procedure TMainForm.CloseBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
   DragAcceptFiles(Handle, True);
end;

Function ShowMessageDlg(Const aMsg: String): Integer;
Begin
   With CreateMessageDialog(aMsg, mtInformation, [mbOk]) Do
      Try
         Position := poOwnerFormCenter;

         Result := ShowModal;
      Finally
         Free;
      End;
End;

procedure TMainForm.UpdateBtnClick(Sender: TObject);
Var
   CORProjAlreadyDone, CellMLAlreadyDone: Boolean;
begin
   CORProjAlreadyDone := False;
   CellMLAlreadyDone  := False;

   With TRegistry.Create Do Begin
      RootKey := HKEY_CLASSES_ROOT;

      If (KeyExists(_CORPROJ_)) Then
         CORProjAlreadyDone := True
      Else If (KeyExists(_COR_)) Then
         MoveKey(_COR_, _CORPROJ_, True);

      If (KeyExists(_CELLML_)) Then
         CellMLAlreadyDone := True
      Else If (KeyExists(_CML_)) Then
         MoveKey(_CML_, _CELLML_, True);

      Free;
   End;

   If (CORProjAlreadyDone And CellMLAlreadyDone) Then
      ShowMessageDlg('The registry is already up to date for both COR projects and CellML files...')
   Else If (CORProjAlreadyDone) Then
      ShowMessageDlg('The registry is already up to date for COR projects, but has been updated for CellML files.')
   Else If (CellMLAlreadyDone) Then
      ShowMessageDlg('The registry is already up to date for CellML files, but has been updated for COR projects.')
   Else
      ShowMessageDlg('The registry has been updated for both COR projects and CellML files.');
end;

Procedure TMainForm.WMDropFiles(Var aMsg: TWMDropFiles);
   Function ItemNameExt(Const aItemName: String): String; Inline;
   Begin
      Result := LowerCase(ExtractFileExt(aItemName));
   End;
   Function ValidItem(Const aItemName: String): UINT;
   Begin
      If ((ItemNameExt(aItemName) = _COR_) Or (ItemNameExt(aItemName) = _CML_)) Then
         Result := 1
      Else
         Result := 0;
   End;
   Function ProperDirName(Const aDirName: String): String;
   Begin
      If (Copy(aDirName, Length(aDirName), 1) = '\') Then
         Result := aDirName
      Else
         Result := aDirName+'\';
   End;
   Function CountValidItemsIn(Const aDirName: String): UINT;
   Var
      SearchRec: TSearchRec;
   Begin
      Result := 0;

      If (FindFirst(ProperDirName(aDirName)+'*.*', faAnyFile, SearchRec) = 0) Then Begin
         Repeat
            If ((SearchRec.Name <> '.') And (SearchRec.Name <> '..')) Then Begin
               If (SearchRec.Attr = faDirectory) Then
                  Inc(Result, CountValidItemsIn(ProperDirName(aDirName)+SearchRec.Name))
               Else
                  Inc(Result, ValidItem(ProperDirName(aDirName)+SearchRec.Name));
            End;
         Until FindNext(SearchRec) <> 0;

         FindClose(SearchRec);
      End;
   End;
   Procedure UpdateProgressBar;
   Begin
      ProgressBar.Position := ProgressBar.Position+1;

      Application.ProcessMessages;
   End;
   Procedure RenameItem(Const aItemName: String);
   Begin
      If (ItemNameExt(aItemName) = _COR_) Then Begin
         RenameFile(aItemName, Copy(aItemName, 1, Length(String(aItemName))-Length(_COR_))+_CORPROJ_);

         UpdateProgressBar;
      End Else If (ExtractFileExt(aItemName) = _CML_) Then Begin
         RenameFile(aItemName, Copy(aItemName, 1, Length(String(aItemName))-Length(_CML_))+_CELLML_);

         UpdateProgressBar;
      End;
   End;
   Procedure RenameItemsIn(Const aDirName: String);
   Var
      SearchRec: TSearchRec;
   Begin
      If (FindFirst(ProperDirName(aDirName)+'*.*', faAnyFile, SearchRec) = 0) Then Begin
         Repeat
            If ((SearchRec.Name <> '.') And (SearchRec.Name <> '..')) Then Begin
               If (SearchRec.Attr = faDirectory) Then
                  RenameItemsIn(ProperDirName(aDirName)+SearchRec.Name)
               Else
                  RenameItem(ProperDirName(aDirName)+SearchRec.Name);
            End;
         Until FindNext(SearchRec) <> 0;

         FindClose(SearchRec);
      End;
   End;
Var
   NbOfItems, NbOfValidItems: UINT;
   I: UINT;
   ItemName: Array[0..MAX_PATH] Of Char;
Begin
   NbOfItems := DragQueryFile(aMsg.Drop, $FFFFFFFF, Nil, 0);

   // Count the number of files to be renamed, if any...

   NbOfValidItems := 0;

   For I := 0 To NbOfItems-1 Do Begin
      DragQueryFile(aMsg.Drop, I, ItemName, SizeOf(ItemName));

      If (FileExists(ItemName)) Then
         Inc(NbOfValidItems, ValidItem(ItemName))
      Else If (DirectoryExists(ItemName)) Then
         Inc(NbOfValidItems, CountValidItemsIn(ItemName));
   End;

   // Do the actual renaming...

   If (NbOfValidItems = 0) Then
      ShowMessageDlg('No renaming required...')
   Else Begin
      ProgressBar.Position := 0;

      ProgressBar.Max := NbOfValidItems;

      For I := 0 To NbOfItems-1 Do Begin
         DragQueryFile(aMsg.Drop, I, ItemName, SizeOf(ItemName));

         If (FileExists(ItemName)) Then
            RenameItem(ItemName)
         Else If (DirectoryExists(ItemName)) Then
            RenameItemsIn(ItemName);
      End;

      ProgressBar.Position := 0;

      ShowMessageDlg('Renaming done...');
   End;

   DragFinish(aMsg.Drop);
End;

end.
