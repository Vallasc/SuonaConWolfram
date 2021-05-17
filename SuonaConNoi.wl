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
BeginPackage["SuonaConNoi`", "JLink`"]

Piano::usage = "Piano[ottava, onDwn, onDwnPar, onUp, onUpPar] ritorna un nuovo pianoforte che parte da octave";
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

selectedNote = "";
octave = 3;
(* 0 = learn, 1 = tutorial, 2 = free *)
state = 2;

KeyDown[note_, oct_] :=
    Block[{},
        selectedNote = toPitch[note, oct]; $Channel @ noteOn[toPitch[note, oct], 80]
    ];
KeysUp[] :=
    Block[{},
        selectedNote = -1; $Channel @ allNotesOff[]
    ];

Piano[onDwn_, onUp_] :=

    DynamicModule[{scale = 2, shownotes = False, pianoInitOctave = octave, playedNote = -1},
        onKeyDown[note_, oct_] :=
            Block[{},
                playedNote = toPitch[note, oct];
                $Channel @ noteOn[toPitch[note, oct], 80];
                If[state != 0,
                    onDwn[joinNote[note, oct]]
                ]
            ];
        onKeyUp[note_, oct_] :=
            Block[{},
                playedNote = -1;
                $Channel @ allNotesOff[];
                If[state != 0,
                    onUp[joinNote[note, oct]]
                ]
            ];
        Manipulate[
            scale = 2;
            whitekey[note_, oct_] :=
                Mouseover[(* Normal not pressed *)
                    Graphics[{{EdgeForm[],
                    If[selectedNote == joinNote[note, oct],
                        Lighter[Green]
                        ,
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
                        (* Se il tasto \[EGrave] selezionato allora verde, altrimenti se premuto rosso.*)
                        If[selectedNote == joinNote[note, oct],
                            Lighter[Green]
                            ,
                            If[dwn && selectedNote != "",
                                Lighter[Red]
                                ,
                                White
                            ]
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
                    {"MouseDown" :> (dwn = True; onKeyDown[note, oct]),
                        "MouseUp" :> (dwn = False; onKeyUp[note, oct])
                    }]
                ];
            space[wd_] :=
                Graphics[{White, Rectangle[{0, 0}, {wd, 1}]}, ImageSize -> {wd * scale, 10}, PlotRange -> {{0, 0}, {2.5, 5}}, ImageMargins -> 0];
            vert :=
                Graphics[Line[{{0, 0}, {0, 1}}], PlotRange -> {{-0.001, 0.001}, {0, 1}}, ImageSize -> {2 * scale, 60 * scale}, ImageMargins -> 0];
            blackkey[note_, oct_] :=
                Mouseover[(* Normal not pressed *)
                    Graphics[{{EdgeForm[Black],
                    If[selectedNote == joinNote[note, oct],
                        Darker[Green]
                        ,
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
                        (* Se il tasto \[EGrave] selezionato allora verde, altrimenti se premuto rosso.*)
                        If[selectedNote == joinNote[note, oct],
                            Darker[Green]
                            ,
                            If[dwn && selectedNote != "",
                                Darker[Red]
                                ,
                                Black
                            ]
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
                    {"MouseDown" :> (dwn = True; onKeyDown[note, oct]),
                        "MouseUp" :> (dwn = False; onKeyUp[note, oct])
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
Grid[{Flatten[Table[blackset[i], {i, octinit, octinit + 1}]], Flatten[Table[whiteset[i], {i, octinit, octinit + 1}]]}, Spacings -> {"Columns" -> {{0}}, "Rows" -> {{-0.1}}}, Alignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}}];
            Deploy[keyboardsplit[octave]],
            {{shownotes, False, "Mostra note"}, {True, False}},
            TrackedSymbols :> {shownotes, selectedNote, octave},
            Initialization :> {dwn = False},
            LocalizeVariables -> False,
            ContinuousAction -> False
        ]
    ]
(* Tutorial *)
Needs["Experimental`"];
midiNotes = {};
playedNote = "";
onDwNote[note_] :=
    playedNote = note;
onUpNote[note_] :=
    playedNote = "";

PlaySong[notesList_] := (
    state = 1;
    midiNotes = notesList;
    selectNextNote[];
    Piano[ onDwNote, onUpNote]
)

ResetPiano[] := (
    selectedNote = "";
    midiNotes = {};
);

selectNextNote[] := (
    If[Length[midiNotes] > 0,
        (selectedNote = First[midiNotes];
            midiNotes = Delete[midiNotes, 1];
            octave = octaveSelectedNote[];
            Print[octave]
        )
        ,
        ResetPiano[];
        selectedNote = "END";
    ]
);

checkNote[] :=
    If[selectedNote == playedNote,
        selectNextNote[]
    ];

Experimental`ValueFunction[playedNote] :=
    If[playedNote != "",
        checkNote[]
    ];

selectNextNoteLearn[] := (
    $Channel @ allNotesOff[];
    If[Length[midiNotes] > 0,
        (selectedNote = First[midiNotes];
            midiNotes = Delete[midiNotes, 1];
            $Channel @ noteOn[toPitch[baseSelectedNote[], 3], 80]
        )
        ,
        ResetPiano[];
        selectedNote = "END";
    ]
);

baseSelectedNote[] :=
    First[StringCases[selectedNote, {RegularExpression["[A-G]#?"], RegularExpression["\\d+"]}]];
octaveSelectedNote[] :=
    ToExpression[Last[StringCases[selectedNote, {RegularExpression["[A-G]#?"], RegularExpression["\\d+"]}]]];
    
textField = "Inserisci la nota qui";
textOut = "";

LearnNotes[notesList_] := (
    state = 0;
    error = 0;
    midiNotes = notesList;
    selectNextNoteLearn[];
    textField = "";
    Panel[
    Column[{
        Piano[onDwNote, onUpNote],
        Dynamic[If[selectedNote != "END" && state == 0,
            Column[{
                Text[Style["Inserisci la nota e clicca il bottone controlla", Black,Bold,18]],
                InputField[Dynamic[textField], String, BaseStyle-> 15,FieldHint->"Inserire la nota qui.."],
                
                If[ToUpperCase[textField] == baseSelectedNote[],
                    Text[Style["OK, molto bene!", Green, Bold, 18]]
                    ,
                    If[textField != "", 
                       
                        Text[Style["Nota non corretta", Red, Bold, 18]]
                
                        ,
                        Text[""]
                    ]
                ] 
            }, Center]
            ,
            Column[{  Text[Style["Hai finito, complimenti!", Green, Bold, 20]],
            Text[Style[StringForm["Errori: "<>ToString[error]], If[error == 0, Green , Orange], Bold, 18]]}, Center]
          
        ]],
        Dynamic[If[selectedNote != "END",
            Button["Controlla",(Pause[0.6];
                    If[ToUpperCase[textField] == baseSelectedNote[] && selectedNote != "",
                        selectNextNoteLearn[];
                        textField = "",
                        error=error+1;
                    ]
                ),ImageSize->{200,50}
            ]
            ,
            Text[""]
        ]]
    },ItemSize->{100,Automatic},Alignment->Center]]
)

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



