program LetterQuiz;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {frmMain},
  kz.Game.LetterQuiz in 'kz.Game.LetterQuiz.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
