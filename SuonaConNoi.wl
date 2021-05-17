(* ::Package:: *)

(* :Title : SuonaConNoi *)
(* :Context : SuonaConNoi` *)
(* :Author : Gruppo 5 *)
(* :Summary : Package che permette di creare un paino virtuale e di effettuare delle operazioni su di esso *)
(* :Copyright : Gruppo 5 2021 *)
(* :Package Version : 1 *)
(* :Mathematica Version : 12.2 *)
(* :History : *)
(* :Discussion : *)

BeginPackage["SuonaConNoi`", {"JLink`", "Experimental`"}]

Piano::usage = "Piano[ottava, onDwn, onUp] ritorna un nuovo pianoforte che parte da octave";
KeyDown::usage = "KeyDown[nota, ottava] preme il tasto del piano corrsiposndente alla nota e all'ottava specificata";
KeysUp::usage = "KeyUp[] Rilascia tutti i tasti del piano";
PlaySong::usage = "ddddd";
ResetPiano::usage = "dddddd";
LearnNotes::usage = "dddddd";
GenerateMidi::usage = "Randomly generates a MIDI file";
GetNotesFromMidi::usage = "dd";
RepeatPlaySong::usage = "RepeatPlaySong[notes] fa ripetere l'esercizio PlaySong all'utente";
GenerateNewPlaySong::usage = "GenerateNewPlaySong[notes] genera un nuovo esercio PlaySong con una melodia differente dalla precedente";

Begin["`Private`"]

(* Load MIDI synth *)
(* Needs["JLink`"]; *)
LoadJavaClass["javax.sound.midi.MidiSystem"];
synth = MidiSystem`getSynthesizer[];
synth @ open[];
$Channel = First[synth @ getChannels[]];

allNotes = List["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
toPitch[note_, oct_] :=
    (First[First[Position[allNotes, note]]] - 1) + (oct * 12);
toNote[pitch_] :=
    allNotes[[Mod[pitch, 12] + 1]];
joinNote[note_, oct_] :=
    StringJoin[note, ToString[oct]];

pianoNumOfOct = 2;
octaveLearn = 3;
octaveTutorial = 3;
octaveFree = 3;

selectedNoteLearn = "";
selectedNoteTutorial = "";


KeyDown[note_, oct_] :=
    Block[{},
        selectedNote = toPitch[note, oct]; $Channel @ noteOn[toPitch[note, oct], 80]
    ];
KeysUp[] :=
    Block[{},
        selectedNote = -1; $Channel @ allNotesOff[]
    ];

(* pstate, 0 = learn, 1 = tutorial, 2 = free *)
Piano[onDwn_, onUp_, pstate_] :=
    With[{pianostate = pstate},
        DynamicModule[{scale = 2.8, shownotes = False, playedNote = -1},
            onKeyDown[note_, oct_, pianostate_] :=
                Block[{},
                    playedNote = toPitch[note, oct];
                    $Channel @ noteOn[toPitch[note, oct], 80];
                    If[pianostate != 0,
                        onDwn[joinNote[note, oct]]
                    ]
                ];
            onKeyUp[note_, oct_, pianostate_] :=
                Block[{},
                    playedNote = -1;
                    $Channel @ allNotesOff[];
                    If[pianostate != 0,
                        onUp[joinNote[note, oct]]
                    ]
                ];
            Manipulate[
                whitekey[note_, oct_] :=
                    Mouseover[(* Normal not pressed *)
                        Graphics[{{EdgeForm[],
                        Switch[pianostate,
                            0,
                                If[selectedNoteLearn == joinNote[note, oct],
                                    Lighter[Green]
                                    ,
                                    White
                                ]
                            ,
                            1,
                                If[selectedNoteTutorial == joinNote[note, oct],
                                    Lighter[Green]
                                    ,
                                    White
                                ]
                            ,
                            2,
                                White
                        ],
                        Rectangle[{0, 0}, {1, 2}]}, {Line[{{0, 2}, {0, 0}, {1, 0}, {1, 2}}]},
                        If[shownotes,
                            Text[Style[note <> ToString[oct], 10, "Label"], {0.5, 1}]
                            ,
                            {}
                        ]},
                        ImageSize -> {22 * scale, 41 * scale},
                        PlotRange -> {{-0.01, 1.01}, {-0.05, 2}},
                        ImageMargins -> 0],
                        (* On mouse over*)
                        EventHandler[Dynamic[
                            Graphics[{{EdgeForm[],
                            Switch[pianostate,
                                0,
                                    If[selectedNoteLearn == joinNote[note, oct],
                                        Lighter[Green]
                                        ,
                                        White
                                    ]
                                ,
                                (* Se il tasto \[EGrave] selezionato allora verde, altrimenti se premuto rosso.*)
                                1,
                                    If[selectedNoteTutorial == joinNote[note, oct],
                                        Lighter[Green]
                                        ,
                                        If[dwn && selectedNoteTutorial != "" && selectedNoteTutorial != "END",
                                            Lighter[Red]
                                            ,
                                            White
                                        ]
                                    ]
                                ,
                                2,
                                    White
                            ],
                            Rectangle[{0, If[dwn,
                                    0.15
                                    ,
                                    0
                                ]
                            }, {1, 2}]}, Line[{{0, 2}, {0, If[dwn,
                                    0.15
                                    ,
                                    0
                                ]
                            }, {1, If[dwn,
                                    0.15
                                    ,
                                    0
                                ]
                            }, {1, 2}}], If[shownotes,
                                Text[Style[note <> ToString[oct], If[dwn,
                                        9
                                        ,
                                        10
                                    ], "Label"
                                ], {0.5, If[dwn,
                                        1.1
                                        ,
                                        1
                                    ]
                                }]
                                ,
                                {}
                            ]},
                            ImageSize -> {22 * scale, 41 * scale},
                            PlotRange -> {{-0.01, 1.01}, {-0.05, 2}},
                            ImageMargins -> 0]
                        ],
                        {"MouseDown" :> (dwn = True; onKeyDown[note, oct, pianostate]),
                            "MouseUp" :> (dwn = False; onKeyUp[note, oct, pianostate])
                        }]
                    ];
                space[wd_] :=
                    Graphics[{White, Rectangle[{0, 0}, {wd, 1}]}, ImageSize -> {wd * scale, 10}, PlotRange -> {{0, 0}, {2.5, 5}}, ImageMargins -> 0];
                vert :=
                    Graphics[Line[{{0, 0}, {0, 1}}], PlotRange -> {{-0.001, 0.001}, {0, 1}}, ImageSize -> {2 * scale, 60 * scale}, ImageMargins -> 0];
                blackkey[note_, oct_] :=
                    Mouseover[(* Normal not pressed *)
                        Graphics[{{EdgeForm[Black],
                        Switch[pianostate,
                            0,
                                If[selectedNoteLearn == joinNote[note, oct],
                                    Darker[Green]
                                    ,
                                    Black
                                ]
                            ,
                            1,
                                If[selectedNoteTutorial == joinNote[note, oct],
                                    Darker[Green]
                                    ,
                                    Black
                                ]
                            ,
                            2,
                                Black
                        ],
                        Rectangle[{0, 2.5}, {0.5, 5}]}, If[shownotes,
                            Text[Style[note <> " " <> ToString[oct], 10, White, "Label"], {0.25, 3.5}, {0, 0}, {0, 1}]
                            ,
                            {}
                        ]}, ImageSize -> {12 * scale, 60 * scale},
                        PlotRange -> {{0, 0.5}, {2.5, 5}},
                        ImageMargins -> 0],
                        EventHandler[Dynamic[
                            Graphics[{{EdgeForm[Black],
                            Switch[pianostate,
                                0,
                                    If[selectedNoteLearn == joinNote[note, oct],
                                        Darker[Green]
                                        ,
                                        Black
                                    ]
                                ,
                                (* Se il tasto \[EGrave] selezionato allora verde, altrimenti se premuto rosso.*)
                                1,
                                    If[selectedNoteTutorial == joinNote[note, oct],
                                        Darker[Green]
                                        ,
                                        If[dwn && selectedNoteTutorial != "",
                                            Darker[Red]
                                            ,
                                            Black
                                        ]
                                    ]
                                ,
                                2,
                                    Black
                            ],
                            Rectangle[{If[dwn,
                                0.05
                                ,
                                0
                            ], 2.5}, {If[dwn,
                                0.4
                                ,
                                0.5
                            ], 5}], If[shownotes,
                                Text[Style[note <> " " <> ToString[oct], If[dwn,
                                        9
                                        ,
                                        10
                                    ], White, "Label"
                                ], {0.25, 3.5}, {0, 0}, {0, 1}]
                                ,
                                {}
                            ]}}, ImageSize -> {12 * scale, 60 * scale}, PlotRange -> {{0, 0.5}, {2.5, 5}}, ImageMargins -> 0]
                        ],
                        {"MouseDown" :> (dwn = True; onKeyDown[note, oct, pianostate]),
                            "MouseUp" :> (dwn = False; onKeyUp[note, oct, pianostate])
                        }]
                    ];
                whiteset[oct_] :=
                    Grid[{Flatten[Map[(whitekey[#, oct])&, {"C", "D", "E", "F", "G", "A", "B"}]]}, Spacings -> 0];
                blackset[oct_] :=
                    Grid[{{
                        vert, space[15], blackkey["C#", oct],
                        space[10], blackkey["D#", oct],
                        space[13], vert, vert, space[14], blackkey["F#", oct], space[10], blackkey["G#", oct], space[10], blackkey["A#", oct], space[14], vert
                    }}, Spacings -> 0, Alignment -> Left];
                keyboardsplit[octinit_] :=
                    Grid[{Flatten[Table[blackset[i], {i, octinit, octinit + pianoNumOfOct - 1}]], Flatten[Table[whiteset[i], {i, octinit, octinit + pianoNumOfOct - 1}]]}, Spacings -> {"Columns" -> {{0}}, "Rows" -> {{-0.1}}}, Alignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}}];
                Deploy[keyboardsplit[Switch[pianostate,
                    0,
                        octaveLearn
                    ,
                    1,
                        octaveTutorial
                    ,
                    2,
                        octaveFree
                ]]],
                {{shownotes, False, "Mostra note"}, {True, False}},
                TrackedSymbols :> {shownotes, selectedNoteLearn, selectedNoteTutorial, octaveFree},
                Initialization :> {dwn = False},
                LocalizeVariables -> False,
                ContinuousAction -> False
            ]
        ]
    ]

(*************************************************************** Tutorial *****************************************************************************************)
midiNotesTutorial = {};
playedNote = "";

onDwNote[note_] :=
    playedNote = note;
onUpNote[note_] :=
    playedNote = "";

PlaySong[notesList_] := (
    midiNotesTutorial = notesList;
    nextNoteTutorial[];
    Piano[onDwNote, onUpNote, 1]
)

ResetPiano[] := (
    selectedNoteTutorial = "";
    midiNotesTutorial = {};
);

nextNoteTutorial[] := (
    If[Length[midiNotesTutorial] > 0,
        (selectedNoteTutorial = First[midiNotesTutorial];
            midiNotesTutorial = Delete[midiNotesTutorial, 1];
            noteOct = octaveNote[selectedNoteTutorial];
            If[noteOct < octaveTutorial || noteOct > (octaveTutorial + pianoNumOfOct - 1),
               octaveTutorial = octaveNote[selectedNoteTutorial] - 1;
            ]
        )
        ,
        ResetPiano[];
        selectedNoteTutorial = "END";
    ]
);

checkNote[] :=
    If[selectedNoteTutorial == playedNote,
        nextNoteTutorial[]
    ];

Experimental`ValueFunction[playedNote] :=
    If[playedNote != "",
        checkNote[]
    ];

(*************************************************************** Learn *****************************************************************************************)
midiNotesLearn = {};
noteLearnStarted = False;

LearnNotes[notesList_] := (
    errors = 0;
    midiNotesLearn = notesList;
    selectedNoteLearn = "";
    textField = "";
    Panel[
        Column[{
            Dynamic[If[noteLearnStarted,
                Button["Ricomincia",
                    errors = 0;
                    midiNotesLearn = notesList;
                    nextNoteLearn[];
                ]
                ,
                Button["Inizia", noteLearnStarted = True; nextNoteLearn[]]
            ]],
            Piano[_, _, 0],
            Dynamic[If[selectedNoteLearn != "END" && noteLearnStarted,
                Column[{
                    Text[Style["Inserisci la nota e clicca il bottone controlla", Black, Bold, 18]],
                    InputField[Dynamic[textField], String, BaseStyle -> 15, FieldHint -> "Inserire la nota qui.."],
                    If[ToUpperCase[textField] == baseNote[selectedNoteLearn],
                        Text[Style["OK, molto bene!", Green, Bold, 18]]
                        ,
                        If[textField != "",
                            Text[Style["Nota non corretta", Red, Bold, 18]]
                            ,
                            Text[Style["", Black, Bold, 18]]
                        ]
                    ]
                }, Center]
                ,
                If[noteLearnStarted,
                    Column[{
                        Text[Style["Hai finito, complimenti!", Green, Bold, 20]],
                        Text[Style[StringForm["Errori: " <> ToString[errors]], If[errors == 0, Green, Orange], Bold, 18]]
                    }, Center]
                    ,
                    Text[""]
                ]
            ]],
            Dynamic[If[selectedNoteLearn != "END" && noteLearnStarted,
                Button["Avanti", (Pause[0.6];
                        If[ToUpperCase[textField] == baseNote[selectedNoteLearn] && selectedNoteLearn != "",
                            nextNoteLearn[]
                            ,
                            errors = errors + 1;
                        ];
                        textField = "";
                    ), ImageSize -> {200, 50}
                ]
                ,
                Text[""]
            ]]
        }, ItemSize -> {90, Automatic}, Alignment -> Center]
    ]
)

nextNoteLearn[] := (
    $Channel @ allNotesOff[];
    If[Length[midiNotesLearn] > 0,
        (selectedNoteLearn = First[midiNotesLearn];
            midiNotesLearn = Delete[midiNotesLearn, 1];
            $Channel @ noteOn[toPitch[baseNote[selectedNoteLearn], 3], 80]
        )
        ,
        selectedNoteLearn = "END";
    ]
)

baseNote[note_] :=
    First[StringCases[note, {RegularExpression["[A-G]#?"], RegularExpression["\\d+"]}]]
octaveNote[note_] :=
    ToExpression[Last[StringCases[note, {RegularExpression["[A-G]#?"], RegularExpression["\\d+"]}]]];


GetNotesFromMidi[midiPath_] := (
    SetDirectory[NotebookDirectory[]];
    sn = Import[midiPath, "SoundNotes"];
    sn = Part[sn, 2];
    Table[sn[[i]][[1]], {i, Length[sn]}]
)

(* Restituisce il path del file selezionato *)
GenerateMidi[] :=
    Button["Genera casualmente", Print[SystemDialogInput["FileOpen"]], Method -> "Queued", BaseStyle -> {"GenericButton", 16, Bold, Orange}, Background -> LightOrange]

(* Ripeti esercizio PlaySong *)
RepeatPlaySong[notes_] :=
    Button["Ripeti esercizio", PlaySong[notes], Method -> "Queued", BaseStyle -> {"GenericButton", 16, Bold, Orange}, Background -> LightOrange]

(* Genera nuovo esercizio PlaySong *)
GenerateNewPlaySong[notes] :=
    Button["Genera nuovo esercizio", GeneratePlaySong[notes], Method -> "Queued", BaseStyle -> {"GenericButton", 16, Bold, Orange}, Background -> LightOrange]

End[];
EndPackage[];



