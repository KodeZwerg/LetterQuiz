unit kz.Game.LetterQuiz;

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

{
  projett: Letter based Quiz system
  purpose: an easy to use Class, controlled over methods and fields.
           optional fire events for caller but also give access over properties
           this class can be used to create a quiz game where user must guess a letter
           the "gaming logic/rules" the caller must define (ie: how many times can we "miss" a letter)
  description: this class can be used to automagical place "TEdit" controls inside of a control
               plus it is very easy to setup games like "guess the word", "hangman" etc...
  limitation: only works good for words or strings based on "A-Z" and optional "-" and " " (space)
              i do not check characters if they are valid! <- WARNING
              TEdit controls are only usable from left to right side of reading
  usage: i suggest that you create a TPanel with a height of 32 when you use my defaults
         initialize it like that:
             LQ: TkzLetterQuiz; // write this into your private part of class
           begin
             // *** this code should be called once when you want to intitalize the object
             // create the object
             LQ := TkzLetterQuiz.Create(Self, pnlWordToGuess);
             // put words into internal storage of words
             LQ.WordList.LoadFromFile('x:\y\z.txt');
             // setup events (i really suggest you to use them!)
             // a copy and paste example for all events; you find at the type definition
             LQ.OnFound := DoFoundEvent;
             LQ.OnMiss := DoMissEvent;
             LQ.OnSkip := DoSkipEvent;
             LQ.OnWon := DoWonEvent;
             LQ.OnCharUsed := DoCharUsedEvent;
             LQ.OnCharNew := DoCharAddEvent;
             LQ.OnWordSet := DoWordSetEvent;
             LQ.OnWordDel := DoWordDelEvent;

             // *** this code should be called from other methods, like a buttonclick event
             // you will need to setup internals somewhere like
             if LQ.CreateEdits then
               begin
               // here we have successfully setup the class with one entry from the internal storage
               end;
             // or you setup a custom word to skip internal storage
             if LQ.CreateEdits('Custom Word') then
               begin
               // here we have successfully setup the class with "Custom Word" as the string to guess
               end;

             // and this is the "final" game logic to test a letter
             // if you want to use the events, call it like that, exemplary
             LQ.TryLetter(Char('A')));

             // for manual working, call it like
             if LQ.TryLetter(Char('A'))) then
               begin
               // we have successful found a letter (you could access the read only properties to show something
               end
               else
               begin
               // the letter was not found or already tried (you could access the read only properties to show something
               end;
             // (the manual (no events) way of course works also)
             // for that purpose i made all internal definitions accessible via properties
             // that way you could rewrite from outside the whole internal game logic at any time
             // and skip using the events
           end;
         at this point, everything is event-driven or in a manual way of working...

  classname: TkzLetterQuiz
  author: KodeZwerg
  creation date: 2022 august
  license: NoLicense

  this class is tested with and compatible to FreePascal/Lazarus and ObjectPascal/Delphi.
  - Lazarus 2.2.2 (rev lazarus_2_2_2-0-g537f43754c) FPC 3.2.2 x86_64-win64-win32/win64
  - Delphi Rio 10.3.3 (RAD Studio 10.3)
}
interface

uses
{$IFDEF DELPHI}
{$IFDEF NameSpace}
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls;
{$ELSE NameSpace}
  SysUtils, Classes,
  Graphics, Controls, StdCtrls;
{$ENDIF NameSpace}
{$ENDIF DELPHI}
{$IFDEF FPC}
  SysUtils, Classes,
  Graphics, Controls, StdCtrls;
{$ENDIF FPC}

type
  TEdits = array of TEdit; // type definition for a TEdit array

  // this class can fire different kind of events that the caller can use
  TOnCharUsedEvent = procedure(const Sender: TObject; const AChar: Char; const AChars: string) of object;
  TOnCharAddEvent = procedure(const Sender: TObject; const AChar: Char; const AChars: string) of object;
  TOnSkipEvent = procedure(const Sender: TObject; const ASkipped: Integer) of object;
  TOnLostWordEvent = procedure(const Sender: TObject; const ATotal, ALeft: Integer) of object;
  TOnLostSingleEvent = procedure(const Sender: TObject; const ATotal, ALeft: Integer) of object;
  TOnLostGameEvent = procedure(const Sender: TObject; const ATotal, ALeft: Integer) of object;
  TOnFoundEvent = procedure(const Sender: TObject; const AFound, ALeft: Integer) of object;
  TOnWonEvent = procedure(const Sender: TObject; const AWon: Integer) of object;
  TOnMissEvent = procedure(const Sender: TObject; const AMissed, ATriesLeft: Integer) of object;
  TOnWordSetEvent = procedure(const Sender: TObject; const AWinner: string; const ALeft: Integer) of object;
  TOnWordDelEvent = procedure(const Sender: TObject; const ARemoved: string; const ALeft: Integer) of object;
  TOnStatisticEvent = procedure(const Sender: TObject; const AGameLimit, AWordLimit, AGameMainLeft, AGameWordLeft, AGameWon, AGameLost, ACharsFound, ACharsLeft, CurrentLostWords, ASkipped, AMissed, ALeftWordCount: Integer; const AWinner, AChars: string) of object;
  {
    you can copy and paste this into your class (private) definition to use events
    (afterwards you must ofc manual create those events)
    procedure DoSkipEvent(const Sender: TObject; const ASkipped: Integer);
    procedure DoFoundEvent(const Sender: TObject; const AFound, ALeft: Integer);
    procedure DoWonEvent(const Sender: TObject; const AWon: Integer);
    procedure DoLostWordEvent(const Sender: TObject; const ATotal, ALeft: Integer);
    procedure DoLostGameEvent(const Sender; const ATotal, ALeft: Integer);
    procedure DoMissEvent(const Sender: TObject; const AMissed: Integer);
    procedure DoCharUsedEvent(const Sender: TObject; const AChar: Char);
    procedure DoCharAddEvent(const Sender: TObject; const AChar: Char; const AChars: string);
    procedure DoWordNewEvent(const Sender: TObject; const AWinner: string);
    procedure DoWordDelEvent(const Sender: TObject; const ARemoved: string; const ALeft: Integer);
    procedure DoStatisticEvent(const Sender: TObject; const AGameLimit, AWordLimit, AGameMainLeft, AGameWordLeft, AGameWon, AGameLost, ACharsFound, ACharsLeft, CurrentLostWords, ASkipped, AMissed, ALeftWordCount: Integer; const AWinner, AChars: string);
    (
      afterwards you must ofc manual create those events, exemplary:
      procedure TfrmMain.DoFoundEvent(const Sender: TObject; const AFound, ALeft: Integer);
      begin
        lblFound.Caption := Format('Found %d Characters, %d left', [AFound, ALeft]);
      end;
    )
  }

  // the main class object
  TkzLetterQuiz = class(TObject)
  strict private  // internal variable management
    FOwner: TComponent; // the owner is responsible to free me
    FParent: TWinControl; // the parent will be used to put TEdit controls on
    FEdits: TEdits; // administration of internal TEdit controls
    FMissed: Integer; // how many times did we failed guessing a letter on current word
    FWon: Integer; // how many times did we found all letters
    FLost: Integer; // keeps track of total losses
    FCurrentLostWord: Integer; // internal counter how many times a word was not guessed within same round of gameplay
    FLimitWord: Integer; // set the amount of tries to guess a letter, per default unlimited
    FLimitGame: Integer; // set the amount of tries to complete the game
    FLeftChars: Integer; // how many letters are left to guess
    FFound: Integer; // here i do save the letters that was tried to use
    FSkipped: Integer; // how many times did we skip a word (similar to a "Lost" in terms of "Win" ;-)
    FWinner: string; // here i do save the current word that the user need to guess, letter by letter
    FCurrent: string; // internal copy of the winner word, will be used to set found and left chars
    FChars: string; // storage for chars that we tried to enter
    FEditVowel: TColor; // color of TEdit for vowel characters (AEIOU)
    FEditSpace: TColor; // color for a space character " " (space)
    FEditWon: TColor; // color for correct guessed characters
    FEditColor: TColor; // color for normal characters
    FEditFont: TFont; // used font and its characteristics for the TEdit controls
    FEditWidth: Integer; // width of one TEdit control (height is controlled by Parent)
    FEditMarginLeft: Integer; // left space between TEdit control
    FEditMarginRight: Integer; // right space between TEdit control
    FEditMarginTop: Integer; // top space between TEdit control
    FEditMarginBottom: Integer; // bottom space between TEdit control
    FEditMarginEnable: Boolean; // enable to deactive margins (space around TEdit control)
    FWordList: TStrings; // this will hold all words or strings in general that will be internal used as a database
    FOnFoundEvent: TOnFoundEvent;  // will be fired when the "TryLetter" method say that the letter was found (success)
    FOnMissEvent: TOnMissEvent;  // will be fired when the "TryLetter" method say that the letter was not found (fail)
    FOnSkipEvent: TOnSkipEvent;  // will be fired when the "CreateEdits" method say that we wasn't finished yet (fail)
    FOnWonEvent: TOnWonEvent;  // will be fired when the "TryLetter" method say that the complete word is now unrevealed (success)
    FOnLostWordEvent: TOnLostWordEvent;  // will be fired when the internal "FLimitWord" is reached, per default unlimited trys
    FOnLostGameEvent: TOnLostGameEvent;  // will be fired when the internal "FLimitGame" is reached, per default unlimited trys
    FOnLostSingleEvent: TOnLostSingleEvent;  // will be fired when the internal "FLost" increases
    FOnCharUsedEvent: TOnCharUsedEvent; // will be fired when the "TryLetter" method say that the letter was already used (fail)
    FOnCharAddEvent: TOnCharAddEvent; // will be fired when the "TryLetter" method say that the letter was new (success)
    FOnWordSetEvent: TOnWordSetEvent;  // will be fired when the "CreateEdits()" method say that we set a new winner word to work on
    FOnWordDelEvent: TOnWordDelEvent;  // will be fired when the "CreateEdits" method say that we removed a word or string from internal storage
                                       // in fact it happen inside GetRandomWord method
                                       // and hopefully see that i wrote "CreateEdits()" and "CreateEdits" explicit to mean the different implementations
                                       // in both you will also be informed about the amount that is currently within internal storage
    FOnStatisticEvent: TOnStatisticEvent; // this event gives all-in-one statistics
    procedure FreeEdits; // free all TEdit controls
    function GetRandomWord: string; // this method pick and remove a random word from internal "FWordList"
  public
    // object create, to initialize many defaults
    constructor Create(const AOwner: TComponent; const AParent: TWinControl);
    // object destructor, to free everything
    destructor Destroy; override;
    // when caller used "WordList" to put words in internal storage, this is the method to call
    function CreateEdits: Boolean; overload;
    // when you want outside of internal storage a "custom" variant. call this
    function CreateEdits(const AString: String): Boolean; overload;
    { whatever "CreateEdits" you call, all events are fired to wake caller up }
    // this method returns true when a letter was found, fires events for input letters
    function TryLetter(const AChar: Char): Boolean;
  public
    // definition of what is what is written behind variables definition at strict private
    // you can freely read current internal data at any time, when using events, skip those
    // zhose i forbid you to write them from outside
    property Winner: string read FWinner;
    property Won: Integer read FWon;
    property Missed: Integer read FMissed;
    property LostWords: Integer read FCurrentLostWord;
    property Skipped: Integer read FSkipped;
    property LeftChars: Integer read FLeftChars;
    // full read write access for internal "holy" fields, take care what you do :-)
    // not needed to access at all in event-driven mode / method mode
    property Edits: TEdits read FEdits write FEdits;
    property WordList: TStrings read FWordList write FWordList;
    // game logic for number of tries per word before we loose against this word, per default unlimited
    property LimitWord: Integer read FLimitWord write FLimitWord;
    // game logic for number of tries in general, per default unlimited
    property LimitGame: Integer read FLimitGame write FLimitGame;
    // game logic for general left tries before game stops working
    property Lost: Integer read FLost write FLost;
    // here you find tweaks for the TEdit controls that be used on creation
    property EditVowel: TColor read FEditVowel write FEditVowel;
    property EditSpace: TColor read FEditSpace write FEditSpace;
    property EditColor: TColor read FEditColor write FEditColor;
    property EditFont: TFont read FEditFont write FEditFont;
    property EditWidth: Integer read FEditWidth write FEditWidth;
    property EditMarginEnable: Boolean read FEditMarginEnable write FEditMarginEnable;
    property EditMarginLeft: Integer read FEditMarginLeft write FEditMarginLeft;
    property EditMarginRight: Integer read FEditMarginRight write FEditMarginRight;
    property EditMarginTop: Integer read FEditMarginTop write FEditMarginTop;
    property EditMarginBottom: Integer read FEditMarginBottom write FEditMarginBottom;
    // when you like to run event-driven, than those are a must to set by caller
    property OnFound: TOnFoundEvent read FOnFoundEvent write FOnFoundEvent;
    property OnWon: TOnWonEvent read FOnWonEvent write FOnWonEvent;
    property OnLostWord: TOnLostWordEvent read FOnLostWordEvent write FOnLostWordEvent;
    property OnLostGame: TOnLostGameEvent read FOnLostGameEvent write FOnLostGameEvent;
    property OnLostSingle: TOnLostSingleEvent read FOnLostSingleEvent write FOnLostSingleEvent;
    property OnMiss: TOnMissEvent read FOnMissEvent write FOnMissEvent;
    property OnSkip: TOnSkipEvent read FOnSkipEvent write FOnSkipEvent;
    property OnWordSet: TOnWordSetEvent read FOnWordSetEvent write FOnWordSetEvent;
    property OnWordDel: TOnWordDelEvent read FOnWordDelEvent write FOnWordDelEvent;
    property OnCharUsed: TOnCharUsedEvent read FOnCharUsedEvent write FOnCharUsedEvent;
    property OnCharAdd: TOnCharAddEvent read FOnCharAddEvent write FOnCharAddEvent;
    property OnStatisticEvent: TOnStatisticEvent read FOnStatisticEvent write FOnStatisticEvent;
  end;

implementation

constructor TkzLetterQuiz.Create(const AOwner: TComponent; const AParent: TWinControl);
begin
  inherited Create;
  Randomize; // for safety call it once on create
  // setup all defaults
  FOwner            := AOwner;
  FParent           := AParent;
  FWinner           := '';
  FChars            := '';
  FMissed           := 0;
  FWon              := 0;
  FLost             := 0;
  FFound            := 0;
  FLeftChars        := 0;
  FSkipped          := 0;
  FCurrentLostWord  := 0;
  FEditMarginEnable := True;
  FEditWon          := clMoneyGreen;
  FEditVowel        := clYellow;
  FEditSpace        := clBtnFace;
  FEditColor        := clWhite;
  FLimitWord        := High(Integer);
  FLimitGame        := High(Integer);
  FEditFont         := TFont.Create;
  try
    FEditFont.Name  := 'Segoe UI';
    FEditFont.Size  := 9;
    FEditFont.Color := clBlack;
  finally
  end;
  FEditWidth        := 20;
  FEditMarginLeft   := 5;
  FEditMarginRight  := 5;
  FEditMarginTop    := 5;
  FEditMarginBottom := 5;
  FOnFoundEvent     := nil;
  FOnWonEvent       := nil;
  FOnLostWordEvent  := nil;
  FOnLostGameEvent  := nil;
  FOnMissEvent      := nil;
  FOnWordSetEvent   := nil;
  FOnWordDelEvent   := nil;
  FOnSkipEvent      := nil;
  FOnCharUsedEvent  := nil;
  FOnStatisticEvent := nil;
  {$IFDEF DELPHI}
  FWordList         := TStringList.Create(dupIgnore, False, False);
  {$ENDIF DELPHI}
  {$IFDEF FPC}
  FWordList         := TStringList.Create;
  try
    TStringList(FWordList).Duplicates    := dupIgnore;
    TStringList(FWordList).Sorted        := False;
    TStringList(FWordList).CaseSensitive := False;
    TStringList(FWordList).SortStyle     := sslNone;
  finally
  end;
  {$ENDIF FPC}
end;

destructor TkzLetterQuiz.Destroy;
begin
  // free and nil everything we used
  FreeEdits;
  FWordList.Free;
  FWordList := nil;
  FEditFont.Free;
  FEditFont := nil;
  FOnFoundEvent     := nil;
  FOnWonEvent       := nil;
  FOnLostWordEvent  := nil;
  FOnLostGameEvent  := nil;
  FOnMissEvent      := nil;
  FOnWordSetEvent   := nil;
  FOnWordDelEvent   := nil;
  FOnSkipEvent      := nil;
  FOnCharUsedEvent  := nil;
  FOnStatisticEvent := nil;
  inherited Destroy;
end;

function TkzLetterQuiz.TryLetter(const AChar: Char): Boolean;
  // simple checker if we used a character already
  function IsCharUsed(const AChar: Char): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := Low(FChars) to High(FChars) do
      if FChars[i] = AChar then
        Exit(True);
  end;
  // convert low chars into high
  function UpChar(const AChar: Char): Char;
  begin
    Result := AChar;
    case AChar of
      'a'..'z': Result := Char(Word(AChar) and $FFDF);
    end;
  end;
var
  i, ii: Integer;
  LChar: Char;
begin
  // initialize result
  Result := False;
  // nothing more to do
  if FLeftChars = 0 then
    Exit;
  // create a local copy of char and make it uppercase
  LChar := UpChar(AChar);
  // was char already used?
  if IsCharUsed(LChar) then
    begin
      if (Assigned(FOnCharUsedEvent)) then
        FOnCharUsedEvent(Self, LChar, FChars);
      Exit;
    end;
  // add new char into char-list
  FChars := FChars + LChar;
  // fire event for new char
  if (Assigned(FOnCharAddEvent)) then
    FOnCharAddEvent(Self, LChar, FChars);
  // iterate over the word
  for i := Low(FCurrent) to High(FCurrent) do
    // we have a match
    if (FCurrent[i] = LChar) then
      begin
        ii := Pred(i);
        // remove current char from internal copy
        FCurrent[i] := '_';
        // show char in the TEdit control
        if ((ii >= Low(FEdits)) and (ii <= High(FEdits))) then
          begin
            FEdits[ii].Text := LChar;
            FEdits[ii].Color := FEditWon;
          end;
        // decrease internal counter of left chars
        Dec(FLeftChars, 1);
        // increase internal counter of found chars
        Inc(FFound, 1);
        // tell result we found something
        Result := True;
        // terminate loop if nothing is left
        if (FLeftChars <= 0) then
          Break;
      end;
  // character not found
  if (not Result) then
    begin
      // increase the missed counter
      Inc(FMissed, 1);
      if (FMissed >= FLimitWord) then
        begin
          // keep track about how many times on this word we failed to guess a correct letter
          Inc(FCurrentLostWord, 1);
          if ((FLimitWord - FCurrentLostWord <= 0) and (Assigned(FOnLostWordEvent))) then
            begin
              FOnLostWordEvent(Self, FLimitWord, FLimitWord - FCurrentLostWord);
              FLeftChars := 0;
            end;
          if ((FCurrentLostWord >= FLimitWord) and (Assigned(FOnLostSingleEvent))) then
            begin
              Inc(FLost);
              FOnLostSingleEvent(Self, FLimitGame, FLimitGame - FLost);
              FLeftChars := 0;
              FCurrentLostWord := 0;
            end;
          // increase the "lost game" counter
          if ((FLost >= FLimitGame) and (Assigned(FOnLostGameEvent))) then
            begin
              FOnLostGameEvent(Self, FLimitGame, FLimitGame - FLost);
            end;
          FMissed := 0;
        end
        else
          // fire the generic missed event that give caller current internal game logic
          if (Assigned(FOnMissEvent)) then
            FOnMissEvent(Self, FMissed, FLimitGame - FLost);
    end;
  // character found, it was the last one
  if (Result and (FLeftChars <= 0)) then
    begin
      Inc(FWon, 1);
      if (Assigned(FOnWonEvent)) then
        FOnWonEvent(Self, FWon);
    end;
  // character found
  if (Result and (Assigned(FOnFoundEvent))) then
    FOnFoundEvent(Self, FFound, FLeftChars);
  // give back all internal data at once
  if (Assigned(FOnStatisticEvent)) then
    OnStatisticEvent(Self, FLimitGame, FLimitWord, FLimitGame - FLost, FLimitWord - FCurrentLostWord, FWon, FLost, FFound, FLeftChars, FCurrentLostWord, FSkipped, FMissed, High(FEdits), FWinner, FChars);
end;

function TkzLetterQuiz.CreateEdits: Boolean;
begin
  // call the custom "CreateEdits" with a random word
  Result := CreateEdits(GetRandomWord);
end;

function TkzLetterQuiz.CreateEdits(const AString: String): Boolean;
  // simple compare to check if input is a vowel
  function IsVowel(const AChar: Char): Boolean;
  begin
    Result := ((AChar = 'A') or (AChar = 'E') or (AChar = 'I') or (AChar = 'O') or (AChar = 'U'));
  end;
var
  i, ii: Integer;
begin
  // free old TEdit controls
  FreeEdits;
  // detect a skip
  if (FLeftChars > 0) then
    begin
      Inc(FSkipped, 1);
      Inc(FLost, 1);
      if (Assigned(FOnMissEvent)) then
        FOnMissEvent(Self, FMissed, FLimitGame - FLost);
      if (Assigned(FOnSkipEvent)) then
        FOnSkipEvent(Self, FSkipped);
      if ((FLost < FLimitGame) and (Assigned(FOnLostSingleEvent))) then
        FOnLostSingleEvent(Self, FLimitGame, FLimitGame - FLost);
      if ((FLost >= FLimitGame) and (Assigned(FOnLostGameEvent))) then
        FOnLostGameEvent(Self, FLimitGame, FLimitGame - FLost);
    end;
  // setup internal defaults
  FChars := '';
  FCurrentLostWord := 0;
  // work with uppercase strings
  FWinner := UpperCase(AString);
  FFound := 0;
  FMissed := 0;
//  FSkipped := 0;
  FLeftChars := 0;
  // prepare internal array to hold TEdits
  SetLength(FEdits, High(FWinner));
  for i := Low(FWinner) to High(FWinner) do
    begin
      ii := Pred(i);
      // create the object
      FEdits[ii] := TEdit.Create(FOwner);
      // set defaults, always begin with setting Parent (!)
      FEdits[ii].Parent := FParent;
      FEdits[ii].Visible := False;
      {$IFDEF DELPHI}
      FEdits[ii].AlignWithMargins := FEditMarginEnable;
      FEdits[ii].Margins.Left := FEditMarginLeft;
      FEdits[ii].Margins.Right := FEditMarginRight;
      FEdits[ii].Margins.Top := FEditMarginTop;
      FEdits[ii].Margins.Bottom := FEditMarginBottom;
      {$ENDIF DELPHI}
      {$IFDEF FPC}
      if FEditMarginEnable then
        begin
          FEdits[ii].BorderSpacing.Space[akLeft] := FEditMarginLeft;
          FEdits[ii].BorderSpacing.Space[akRight] := FEditMarginRight;
          FEdits[ii].BorderSpacing.Space[akTop] := FEditMarginTop;
          FEdits[ii].BorderSpacing.Space[akBottom] := FEditMarginBottom;
        end
        else
        begin
          FEdits[ii].BorderSpacing.Space[akLeft] := 0;
          FEdits[ii].BorderSpacing.Space[akRight] := 0;
          FEdits[ii].BorderSpacing.Space[akTop] := 0;
          FEdits[ii].BorderSpacing.Space[akBottom] := 0;
        end;
      {$ENDIF FPC}
      if ((FEditFont <> nil) and (FEditFont.Name <> '')) then
        FEdits[ii].Font := FEditFont;
      FEdits[ii].Enabled := False;
      FEdits[ii].ReadOnly := True;
      FEdits[ii].Width := FEditWidth;
      {$IFDEF DELPHI}
      FEdits[ii].Left := High(Integer);
      {$ENDIF DELPHI}
      {$IFDEF FPC}
      FEdits[ii].Left := (FEdits[ii].Width * (i + 2));
      {$ENDIF FPC}
      FEdits[ii].Align := alLeft;
      FEdits[ii].Alignment := taCenter;
      FEdits[ii].MaxLength := 1;
      // convert a "-" into a " " (space) to exclude that character as a legit letter
      if (FWinner[i] = '-') then
        FWinner[i] := ' ';
      // setup default color, dependand on given letter type (vowel, space or all others)
      if IsVowel(FWinner[i]) then
        FEdits[ii].Color := FEditVowel
      else
        if (FWinner[i] = ' ') then
          FEdits[ii].Color := FEditSpace
        else
          FEdits[ii].Color := FEditColor;
      // display a space char as "-", all others are "_"
      if (FWinner[i] = ' ') then
        FEdits[ii].Text := '-'
      else
        FEdits[ii].Text := '_';
      // increment internal counter for characters to search for
      // this includes also same characters used more than once
      if (FWinner[i] <> ' ') then
        Inc(FLeftChars, 1);
      FEdits[ii].Visible := True;
    end;
  // create from input a new copy that be used internal while playing
  FCurrent := FWinner;
  // fire all events to initialize caller (beside the delete event)
  if (Assigned(FOnFoundEvent)) then
    FOnFoundEvent(Self, FFound, FLeftChars);
  if Assigned(FOnWordSetEvent) then
    FOnWordSetEvent(Self, FWinner, High(FEdits));
  if (Assigned(FOnMissEvent)) then
    FOnMissEvent(Self, FMissed, FLimitGame - FLost);
  if (Assigned(FOnWonEvent)) then
    FOnWonEvent(Self, FWon);
  if (Assigned(FOnSkipEvent)) then
    FOnSkipEvent(Self, FSkipped);
  if (Assigned(FOnStatisticEvent)) then
    OnStatisticEvent(Self, FLimitGame, FLimitWord, FLimitGame - FLost, FLimitWord - FCurrentLostWord, FWon, FLost, FFound, FLeftChars, FCurrentLostWord, FSkipped, FMissed, High(FEdits), FWinner, FChars);
  // return true when we have at least create one TEdit control
  Result := (High(FEdits) > 0);
end;

procedure TkzLetterQuiz.FreeEdits;
var
  i: Integer;
begin
  // free and nil all TEdit controls
  for i := High(FEdits) downto Low(FEdits) do
    begin
      FEdits[i].Free;
      FEdits[i] := nil;
      SetLength(FEdits, i);
    end;
  // zero the array
  SetLength(FEdits, 0);
end;

function TkzLetterQuiz.GetRandomWord: string;
var
  i: Integer;
begin
  // fallback value
  Result := '';
  // do we have a word in list to be used?
  if (FWordList.Count > 0) then
    begin
      // generate a random number for the new word-pick
      i := Random(FWordList.Count);
      // copy word to result
      Result := FWordList.Strings[i];
      // fire event that we removed a word or string
      if Assigned(FOnWordDelEvent) then
        FOnWordDelEvent(Self, Result, High(FEdits));
      // remove the word from internal list that it never appear again
      FWordList.Delete(i);
    end;
end;

end.
