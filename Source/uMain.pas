unit uMain;

{
 This is a small demonstration program for the LetterQuiz class made by KodeZwerg.
 Demo is currently only full working for windows, add a CenteredDialog() solution for your target os.
 Damo done by KodeZwerg.
}

{$I '.\kz.inc'}
// if you miss above file, activate next lines by removing the beginning dot "." and deactivate opposite way above line
{.$IFNDEF FPC}
{.$DEFINE DELPHI}
{.$ENDIF}
{.$IFDEF DELPHI}
{.$IF CompilerVersion >= 23}
{.$DEFINE NameSpace}
{.$IFEND}
{.$ENDIF}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
{$IFDEF DELPHI}
{$IFDEF NameSpace}
{$IF DEFINED(MSWindows)}
  Winapi.Windows,
{$ENDIF MSWindows}
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
{$ELSE NameSpace}
{$IF DEFINED(MSWindows)}
  Windows,
{$ENDIF MSWindows}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
{$ENDIF NameSpace}
{$ENDIF DELPHI}
{$IFDEF FPC}
{$IF DEFINED(MSWindows)}
  Windows,
{$IFEND MSWindows}
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
{$ENDIF FPC}
  kz.Game.LetterQuiz;

type
  TfrmMain = class(TForm)
    pnlWord: TPanel;
    pnlTitle: TPanel;
    pnlButtons: TPanel;
    pnlGame: TPanel;
    btnNew: TButton;
    pnlScores: TPanel;
    pnlABC: TPanel;
    rgABC: TRadioGroup;
    btnTry: TButton;
    pnlScore1: TPanel;
    pnlScore2: TPanel;
    lblWon: TLabel;
    lblSkipped: TLabel;
    lblFound: TLabel;
    lblMissed: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnTryClick(Sender: TObject);
  private
    // the letterquiz class
    LQ: TkzLetterQuiz;
    // what happen when user want to skip a word
    procedure DoSkipEvent(const Sender: TObject; const ASkipped: Integer);
    // what happen when we found a letter
    procedure DoFoundEvent(const Sender: TObject; const AFound, ALeft: Integer);
    // what happen when the word is full resolved
    procedure DoWonEvent(const Sender: TObject; const AWon: Integer);
    // what happen when the letter is not in word
    procedure DoMissEvent(const Sender: TObject; const AMissed, ALeftPerGame: Integer);
    // what happen when class detect a new word as the one you must guess
    procedure DoWordSetEvent(const Sender: TObject; const AWinner: string; const ALeft: Integer);
    // what happen when from internal storage an entry was removed
    procedure DoWordDelEvent(const Sender: TObject; const ARemoved: string; const ALeft: Integer);
    // what happen when the entered letter was already tried to set
    procedure DoCharUsedEvent(const Sender: TObject; const AChar: Char; const AChars: string);
    // what happen when the entered letter was a new one
    procedure DoCharAddEvent(const Sender: TObject; const AChar: Char; const AChars: string);
    // what happen when we lost to solve a word (either by tries per word or by skipping a word)
    procedure DoLostWordEvent(const Sender: TObject; const ATotal, ALeft: Integer);
    // what happen when game detect that we lost
    procedure DoLostGameEvent(const Sender: TObject; const ATotal, ALeft: Integer);
    // what happen when game detect that we lost one time
    procedure DoLostSingleEvent(const Sender: TObject; const ATotal, ALeft: Integer);
    // the generic statistic update event, in many cases this is all you would need
    procedure DoStatisticEvent(const Sender: TObject; const AGameLimit, AWordLimit, AGameMainLeft, AGameWordLeft, AGameWon, AGameLost, ACharsFound, ACharsLeft, CurrentLostWords, ASkipped, AMissed, ALeftWordCount: Integer; const AWinner, AChars: string);
    {$IF DEFINED(MSWindows)}
    // small helper to display a modal dialog centered on form that called it
    function CenteredDialog(const AMsg, ACaption: string; const ADlgType: TMsgDlgType = mtInformation; const AButtons: TMsgDlgButtons = [mbOk]; const AFontName: string = 'Segoe UI'; const AFontSize: Integer = 9): Integer;
    {$ENDIF MSWindows}
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$IFDEF DELPHI}
{$R *.dfm}
{$ENDIF DELPHI}
{$IFDEF FPC}
{$R *.lfm}
{$ENDIF FPC}


procedure TfrmMain.btnNewClick(Sender: TObject);
begin
// enable or disable the "gaming" controls
  if LQ.CreateEdits then
    begin
      btnTry.Enabled := True;
      rgABC.Enabled := True;
    end
    else
    begin
      btnTry.Enabled := False;
      rgABC.Enabled := False;
    end;
end;

procedure TfrmMain.btnTryClick(Sender: TObject);
begin
  // 65 is the ASCII number for "A", rgABC.ItemIndex begins at index 0 for "A",
  // so i just add the itemindex to the 65 for the input letter
  // you can easily create your own way, this is just quick and dirty to demonstrate the basics :)
  LQ.TryLetter(Char(65 + rgABC.ItemIndex));
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // create our letterquiz object to play with
  LQ := TkzLetterQuiz.Create(Self, pnlWord);
  // exemplary add manual some strings
  // load a file i suggest to do :-)
  // LQ.WordList.LoadFromFile('x:\y\z.txt');
  // or use a stream
  // LQ.WordList.LoadFromStream(streamhandle);
  LQ.WordList.Add('This is a Test');
  LQ.WordList.Add('for a');
  LQ.WordList.Add('Guess the word');
  LQ.WordList.Add('game');
  LQ.WordList.Add('clone');
  LQ.WordList.Add('made in August');
  LQ.WordList.Add('by');
  LQ.WordList.Add('KodeZwerg');
  // setup a lifecycle (without it is unlimited)
  // how many "lifes"
  LQ.LimitGame := 3;
  // how many times you can guess a wrong letter before it cost a "life"
  LQ.LimitWord := 3;
  // setup all events (you just setup what you need, this just demonstate all events)
  {$IFDEF DELPHI}
  LQ.OnFound          := DoFoundEvent;
  LQ.OnMiss           := DoMissEvent;
  LQ.OnSkip           := DoSkipEvent;
  LQ.OnWon            := DoWonEvent;
  LQ.OnCharUsed       := DoCharUsedEvent;
  LQ.OnCharAdd        := DoCharAddEvent;
  LQ.OnWordSet        := DoWordSetEvent;
  LQ.OnWordDel        := DoWordDelEvent;
  LQ.OnLostWord       := DoLostWordEvent;
  LQ.OnLostGame       := DoLostGameEvent;
  LQ.OnLostSingle     := DoLostSingleEvent;
  LQ.OnStatisticEvent := DoStatisticEvent;
  {$ENDIF DELPHI}
  {$IFDEF FPC}
  LQ.OnFound          := @DoFoundEvent;
  LQ.OnMiss           := @DoMissEvent;
  LQ.OnSkip           := @DoSkipEvent;
  LQ.OnWon            := @DoWonEvent;
  LQ.OnCharUsed       := @DoCharUsedEvent;
  LQ.OnCharAdd        := @DoCharAddEvent;
  LQ.OnWordSet        := @DoWordSetEvent;
  LQ.OnWordDel        := @DoWordDelEvent;
  LQ.OnLostWord       := @DoLostWordEvent;
  LQ.OnLostGame       := @DoLostGameEvent;
  LQ.OnLostSingle     := @DoLostSingleEvent;
  LQ.OnStatisticEvent := @DoStatisticEvent;
  {$ENDIF FPC}
  // you can setup a lot of more things, have fun finding out how all works together :-D
end;

procedure TfrmMain.DoSkipEvent(const Sender: TObject; const ASkipped: Integer);
begin
  lblSkipped.Caption := Format('Skipped %d times', [ASkipped]);
end;

procedure TfrmMain.DoFoundEvent(const Sender: TObject; const AFound, ALeft: Integer);
begin
  lblFound.Caption := Format('Found %d Characters, %d left', [AFound, ALeft]);
end;

procedure TfrmMain.DoWonEvent(const Sender: TObject; const AWon: Integer);
begin
  lblWon.Caption := Format('Won %d times', [AWon]);
  btnTry.Enabled := False;
  rgABC.Enabled := False;
end;

procedure TfrmMain.DoLostWordEvent(const Sender: TObject; const ATotal, ALeft: Integer);
begin
  Self.Caption := Format('(word) from a total of %d you have %d tries left', [ATotal, ALeft]);
  btnTry.Enabled := False;
  rgABC.Enabled := False;
end;

procedure TfrmMain.DoLostGameEvent(const Sender: TObject; const ATotal, ALeft: Integer);
begin
  Self.Caption := Format('(game/full) from a total of %d you have %d tries left', [ATotal, ALeft]);
  btnTry.Enabled := False;
  rgABC.Enabled := False;
end;

procedure TfrmMain.DoLostSingleEvent(const Sender: TObject; const ATotal, ALeft: Integer);
begin
  Self.Caption := Format('(game/single) from a total of %d you have %d tries left', [ATotal, ALeft]);
  btnTry.Enabled := False;
  rgABC.Enabled := False;
end;

procedure TfrmMain.DoMissEvent(const Sender: TObject; const AMissed, ALeftPerGame: Integer);
begin
  lblMissed.Caption := Format('Missed %d times', [AMissed]);
end;

procedure TfrmMain.DoWordSetEvent(const Sender: TObject; const AWinner: string; const ALeft: Integer);
begin
(*
{$IF DEFINED(MSWindows)}
  if (CenteredDialog('The entry "' + AWinner + '" is now set to guess!' + #13#10 +
                       'Entries left: ' + IntToStr(ALeft),
                       'Winner Defined!',
                       mtConfirmation) = MB_OK) then
       else
       ;
{$ENDIF MSWindows}
*)
//  Self.Caption := AWinner;
end;

procedure TfrmMain.DoWordDelEvent(const Sender: TObject; const ARemoved: string; const ALeft: Integer);
begin
(*
{$IF DEFINED(MSWindows)}
  if (CenteredDialog('The entry "' + ARemoved + '" is now removed!' + #13#10 +
                       'Entries left: ' + IntToStr(ALeft),
                       'Entry Removed!',
                       mtConfirmation) = MB_OK) then
       else
       ;
{$ENDIF MSWindows}
*)
//  Self.Caption := ARemoved;
end;

procedure TfrmMain.DoCharAddEvent(const Sender: TObject; const AChar: Char; const AChars: string);
begin
(*
{$IF DEFINED(MSWindows)}
  if (CenteredDialog('The Character "' + AChar + '" is now marked as used!' + #13#10 +
                     'Current used letters: ' + AChars,
                     'Input added!',
                     mtConfirmation) = MB_OK) then
     else
     ;
{$ENDIF MSWindows}
*)
//  Self.Caption := AChars;
end;

procedure TfrmMain.DoCharUsedEvent(const Sender: TObject; const AChar: Char; const AChars: string);
begin
{$IF DEFINED(MSWindows)}
  if (CenteredDialog('The Character "' + AChar + '" was already used!' + #13#10 +
                     'Please select a different one!' + #13#10 +
                     'Current used letters: ' + AChars,
                     'Wrong Input!') = MB_OK) then
     // you can do additional tasks after user has selected OK
     else
     // you can do additional tasks when user aborted the dislog
     ;
{$ENDIF MSWindows}
end;

procedure TfrmMain.DoStatisticEvent(const Sender: TObject; const AGameLimit, AWordLimit, AGameMainLeft, AGameWordLeft, AGameWon, AGameLost, ACharsFound, ACharsLeft, CurrentLostWords, ASkipped, AMissed, ALeftWordCount: Integer; const AWinner, AChars: string);
begin
  lblSkipped.Caption := Format('Skipped %d times', [ASkipped]);
  lblFound.Caption := Format('Found %d Characters, %d left', [ACharsFound, ACharsLeft]);
  lblWon.Caption := Format('Won %d times', [AGameWon]);
  lblMissed.Caption := Format('Missed %d times', [AMissed]);
  Self.Caption := Format('%d times won, %d times lost, %d lifes left, %d word tries left, %d words left, %d chars found, %d chars left', [AGameWon, AGameLost, AGameMainLeft, AGameWordLeft, ALeftWordCount, ACharsFound, ACharsLeft]);
  // Self.Caption := Format('(word) from a total of %d you have %d tries left', [ATotal, ALeft]);
  // Self.Caption := Format('(game/full) from a total of %d you have %d tries left', [ATotal, ALeft]);
  // Self.Caption := Format('(game/single) from a total of %d you have %d tries left', [ATotal, ALeft]);
end;

{$IF DEFINED(MSWindows)}
// small work-around to display a custom dialog centered to callers form, for compatibility reasons i did removed the default button support of delphi
function TfrmMain.CenteredDialog(const AMsg, ACaption: string; const ADlgType: TMsgDlgType = mtInformation; const AButtons: TMsgDlgButtons = [mbOk]; const AFontName: string = 'Segoe UI'; const AFontSize: Integer = 9): Integer;
var
  Dlg: TForm;
  R: TRect;
begin
  Dlg := CreateMessageDialog(AMsg, ADlgType, AButtons);
  try
    Dlg.Caption := ACaption;
    {$IFDEF DELPHI}
    // delphi create a label called "message" to put text on
    if (Dlg.FindComponent('message') <> nil) then
      begin
        TLabel(Dlg.FindComponent('message')).Caption := AMsg;
        // try tweak the font
        if AFontName <> '' then
          TLabel(Dlg.FindComponent('message')).Font.Name  := AFontName;
        TLabel(Dlg.FindComponent('message')).Font.Size  := AFontSize;
        TLabel(Dlg.FindComponent('message')).Font.Color := clBlack;
      end;
    {$ENDIF DELPHI}
    {$IFDEF FPC}
    // lazarus just paint
    Dlg.Font.Name := AFontName;
    Dlg.Font.Size := AFontSize;
    Dlg.Font.Color := clBlack;
    {$ENDIF FPC}
    R := TRect.Create(0,0,0,0);
    GetWindowRect(Screen.ActiveForm.Handle, R);
    Dlg.Position := poDesigned;
    Dlg.Left := R.Left + ((R.Right - R.Left) div 2) - (Dlg.Width div 2);
    Dlg.Top := R.Top + ((R.Bottom - R.Top) div 2) - (Dlg.Height div 2);
    Result := Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;
{$ENDIF MSWindows}

end.
