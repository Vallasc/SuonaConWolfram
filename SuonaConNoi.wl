(* ::Package:: *)

(* :Title : SuonaConNoi *)
(* :Context : SuonaConNoi` *)
(* :Author : Gruppo 5 *)
(* :Summary : Package che permette di creare un pianoforte virtuale e di effettuare delle operazioni su di esso *)
(* :Copyright : Gruppo 5 2021 *)
(* :Package Version : 1 *)
(* :Mathematica Version : 12.2 *)
(* :History : *)
(* :Discussion : *)

BeginPackage["SuonaConNoi`", {"JLink`", "Experimental`"}]

Piano::usage = "Piano[pstate] crea un pianoforte virtuale all modalit\[AGrave] pstate";
KeyDown::usage = "KeyDown[nota, ottava] preme il tasto del piano corrsiposndente alla nota e all'ottava specificata";
KeysUp::usage = "KeyUp[] rilascia tutti i tasti del piano";
PlaySong::usage = "PlaySong[] genera l'interfaccia del pianoforte per suonare una traccia MIDI";
LearnNotes::usage = "LearnNotes[notesList] data una lista di note controlla che il pitch inserito dall'utente sia corretto";
GetNotesFromMidi::usage = "GetNotesFromMidi[midiPath] dato un file MIDI, ritorna una lista di tutte le note che lo compongono";

Begin["`Private`"]

(* Load MIDI synth *)
(* Needs["JLink`"]; *)
LoadJavaClass["javax.sound.midi.MidiSystem"];
synth = MidiSystem`getSynthesizer[];
synth @ open[];
(* Canale del synth midi*)
$Channel = First[synth @ getChannels[]];

(* Note musicali *)
allNotes = List["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
(* Converte da nota e ottava a pitch*)
toPitch[note_, oct_] :=
    (First[First[Position[allNotes, note]]] - 1) + (oct * 12);
(* Concatena la nota e l'ottava*)
joinNote[note_, oct_] :=
    StringJoin[note, ToString[oct]];

(* Numero di ottave nel piano *)
pianoNumOfOct = 2;
(* Ottava iniziale nel piano in modalit\[AGrave] Learn*)
octaveLearn = 3;
(* Ottava iniziale nel piano in modalit\[AGrave] Tutorial*)
octaveTutorial = 3;
(* Ottava iniziale nel piano in modalit\[AGrave] Free*)
octaveFree = 3;

(* Nota selezionata nel piano in modalit\[AGrave] Learn *)
selectedNoteLearn = "";
(* Nota selezionata nel piano in modalit\[AGrave] Tutorial *)
selectedNoteTutorial = "";
(* Nota premuta nel piano *)
playedNote = "";

(* Suona la nota all'ottava nel piano corrente*)
KeyDown[note_, oct_] := (
        selectedNote = toPitch[note, oct]; $Channel @ noteOn[toPitch[note, oct], 80]
        )
(* Rilascia la nota nel piano corrente*)
KeysUp[] := (
        selectedNote = -1; $Channel @ allNotesOff[]
        );

(* Visualizza il pianoforte virtuale a schermo *)
(* pstate in dica la modalit\[AGrave] del piano, 0 = learn, 1 = tutorial, 2 = free *)
Piano[pstate_] :=
    With[{pianostate = pstate},
        DynamicModule[{scale = 2.8, shownotes = False},
        
            (* Chiamata quando viene premuto un tasto del piano *)
            onKeyDown[note_, oct_, pianostate_] := (
                playedNote = joinNote[note, oct];
                $Channel @ noteOn[toPitch[note, oct], 80];
            );
            
            (* Chiamata quando viene rilasciato un tasto del piano *)
            onKeyUp[note_, oct_, pianostate_] := (
                playedNote = -1;
                $Channel @ allNotesOff[];
            );
            
            Manipulate[
                (* Disegna a schermo un tasto bianco *)
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
                (* Disegna a schermo uno spazio bianco *)
                space[wd_] :=
                    Graphics[{White, Rectangle[{0, 0}, {wd, 1}]}, ImageSize -> {wd * scale, 10}, PlotRange -> {{0, 0}, {2.5, 5}}, ImageMargins -> 0];
                (* Disegna a schermo la barra verticale che divide i tasti *)
                vert :=
                    Graphics[Line[{{0, 0}, {0, 1}}], PlotRange -> {{-0.001, 0.001}, {0, 1}}, ImageSize -> {2 * scale, 60 * scale}, ImageMargins -> 0];
                (* Disegna a schermo un tasto bianco *)
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
                                        If[dwn && selectedNoteTutorial != "" && selectedNoteTutorial != "END",
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
                    
                (* Disegna a schermo tutti i tasti bianchi del piano *)
                whiteset[oct_] :=
                    Grid[{Flatten[Map[(whitekey[#, oct])&, {"C", "D", "E", "F", "G", "A", "B"}]]}, Spacings -> 0];
                    
               (* Disegna a schermo tutti i tasti neri del piano *)
                 blackset[oct_] :=
                    Grid[{{
                        vert, space[15], blackkey["C#", oct],
                        space[10], blackkey["D#", oct],
                        space[13], vert, vert, space[14], blackkey["F#", oct], space[10], blackkey["G#", oct], space[10], blackkey["A#", oct], space[14], vert
                    }}, Spacings -> 0, Alignment -> Left];
                    
                (* Disegna l'intero pianoforte *)
                keyboardsplit[octinit_] :=
                    Grid[{Flatten[Table[blackset[i], {i, octinit, octinit + pianoNumOfOct - 1}]], 
                          Flatten[Table[whiteset[i], {i, octinit, octinit + pianoNumOfOct - 1}]]}, 
                          Spacings -> {"Columns" -> {{0}}, "Rows" -> {{-0.1}}}, 
                          Alignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}}
                    ];
                (* Deploy manipulate *)    
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
                (* Simboli che vengono tracciati dalla manipulate *)
                TrackedSymbols :> {shownotes, selectedNoteLearn, selectedNoteTutorial, octaveFree},
                Initialization :> {dwn = False},
                LocalizeVariables -> False,
                ContinuousAction -> False
            ]
        ]
    ]

(* *********************************************** Learn *********************************************** *)

(* Lista delle liste di note con i relativi titoli da visualizzare *)
midiNotesLearn = {};
(* Lista delle note correnti che vengono visualizzate *)
currentList = {};

(* Data una lista di note controlla che il pitch inserito dall'utente sia corretto *)
LearnNotes[notesList_] := (
    titleLearn = "Come si suona un pianoforte";
    errorsLearn = 0;
    midiNotesLearn = notesList;
    selectedNoteLearn = "";
    textField = "";
    noteLearnStarted = False;
    Panel[
        Column[{
            (* Titolo *)
            Dynamic[Text[Style[titleLearn, Black, Bold, 24]]],
            Dynamic[If[noteLearnStarted,
                Button["Ricomincia",
                    errorsLearn = 0;
                    midiNotesLearn = notesList;
                    currentList = {};
                    nextNoteLearn[],
                    ImageSize -> {200, Automatic},
                    Method -> "Queued", BaseStyle -> {"GenericButton", 16, Bold, Darker[Orange]}, Background -> LightOrange
                ]
                ,
                Button["Start", noteLearnStarted = True; nextNoteLearn[],
                    ImageSize -> {200, Automatic},
                    Method -> "Queued", BaseStyle -> {"GenericButton", 16, Bold, Darker[Orange]}, Background -> LightOrange
                ]
            ]],
            Piano[0],
            (* Input Field *)
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
                        Text[Style[StringForm["Errori: " <> ToString[errorsLearn]], If[errorsLearn == 0, Green, Orange], Bold, 18]]
                    }, Center]
                    ,
                    Text[""]
                ]
            ]],
            (* Bottone che controlla la correttezza dell'input *)
            Dynamic[If[selectedNoteLearn != "END" && noteLearnStarted,
                Button["Avanti", (Pause[0.6];
                        If[ToUpperCase[textField] == baseNote[selectedNoteLearn] && selectedNoteLearn != "",
                            nextNoteLearn[],
                            errorsLearn += 1;
                        ];
                        textField = "";
                    ), ImageSize -> {200, 50},
                    Method -> "Queued", BaseStyle -> {"GenericButton", 16, Bold, Darker[Orange]}, Background -> LightOrange
                ],
                Text[""]
            ]]
        }, ItemSize -> {Automatic, Automatic}, Alignment -> Center]
    ]
)

(* Passa alla prossima nota della lista learn*)
nextNoteLearn[] := (
	(* Ferma tutte le note gi\[AGrave] in esecuzione *)
    $Channel @ allNotesOff[];
    (* Controlla se ci sono liste di note *)
    If[Length[currentList] == 0 && Length[midiNotesLearn] > 0,
        stepEnd = True;
        (* Seleziona la prima lista *)
        currentList = First[First[midiNotesLearn]];
        titleLearn = Last[First[midiNotesLearn]];
        midiNotesLearn = Delete[midiNotesLearn, 1];
    ];
    (* Controlla se ci sono note nella lista selezionata *)
    If[Length[currentList] > 0,
        (selectedNoteLearn = First[currentList];
            currentList = Delete[currentList, 1];
            (* Riproduce il suono della nota *)
            $Channel @ noteOn[toPitch[baseNote[selectedNoteLearn], 3], 80]
        ),
        selectedNoteLearn = "END";
    ]
)

baseNote[note_] :=
    First[StringCases[note, {RegularExpression["[A-G]#?"], RegularExpression["\\d+"]}]]
octaveNote[note_] :=
    ToExpression[Last[StringCases[note, {RegularExpression["[A-G]#?"], RegularExpression["\\d+"]}]]];

(* *********************************************** Tutorial *********************************************** *)

(* Lista delle note importate dal file MIDI *)
midiNotesTutorial = {};

(* Genera l'interfaccia del pianoforte per suonare una traccia MIDI *)
PlaySong[] := (
    titleTutorial = "Suona la traccia MIDI";
    errorsTutorial = 0;
    playSongEnded = False;
    Panel[
        Column[{
            Dynamic[Text[Style[titleTutorial, Black, Bold, 24]]],
            Row[{
                generateMidi[],
                pickMidi[]
            }],
            Piano[1],
            Dynamic[If[playSongEnded,
                    Text[Style["Bravo!", Green, Bold, 20]],
                    Text[""]
                ]]
        }, Alignment -> Center]
    ]
)

(* Passa alla prossima nota della lista tutorial *)
nextNoteTutorial[] := (
   (* Controlla se ci sono note disponibili *)
    If[Length[midiNotesTutorial] > 0,
        (* Seleziona la prima nota e la elimina dalla lista *)
        (selectedNoteTutorial = First[midiNotesTutorial];
            midiNotesTutorial = Delete[midiNotesTutorial, 1];
            noteOct = octaveNote[selectedNoteTutorial];
            (* Aggiornamento dinamico dell'ottava durante il tutorial *)
            If[noteOct < octaveTutorial || noteOct > (octaveTutorial + pianoNumOfOct - 1),
                octaveTutorial = octaveNote[selectedNoteTutorial] - 1;
            ]
        ),
        (* Azzera lo stato *)
        midiNotesTutorial = {};
        selectedNoteTutorial = "END";
        playSongEnded = True;
    ]
)

(* Controlla che la nota suonata sia corretta, se si passa alla prossima *)
checkNote[] := (
    If[selectedNoteTutorial == playedNote,
        nextNoteTutorial[]
    ]
)

(* Resta in ascolto del cambiamento di playedNote *)
ValueFunction[playedNote] := (
    If[playedNote != "",
        checkNote[]
    ]
)

(* *********************************************** MIDI *********************************************** *)
SetDirectory[NotebookDirectory[]];

(* Lista dei midi presenti nella directory MIDI *)
paths = List[
    "./MIDI/50_special.mid",
    "./MIDI/Africa_Toto.mid",
    "./MIDI/Attenti_al_lupo.mid",
    "./MIDI/Blinding_Lights_The_Weeknd.mid",
    "./MIDI/Centro_di_gravita_permanente.mid",
    "./MIDI/Cuccurucuccu.mid",
    "./MIDI/La_canzone_del_sole.mid",
    "./MIDI/Marcia_imperiale.mid",
    "./MIDI/Mila_e_Shiro.mid",
    "./MIDI/Nella_vecchia_fattoria.mid",
    "./MIDI/Per_Elisa.mid",
    "./MIDI/Stand_by_me.mid",
    "./MIDI/Super_Mario_Bros.mid",
    "./MIDI/The_Final_Countdown.mid",
    "./MIDI/Africa_Toto.mid"
];

(* Permette di selezionare un file MIDI dal proprio dispositivo *)
pickMidi[] := (
    Button["Seleziona file MIDI",
        filePath = ToString[SystemDialogInput["FileOpen", ".mid"]];
        (* Se il file \[EGrave] stato scelto correttamente *)
        If[filePath !="$Canceled",
           (* Estrae le note dal MIDI *)
           midiNotesTutorial = GetNotesFromMidi[filePath];
           titleTutorial = FileNameTake[filePath];
           (* Ricomincia selezionando la prima nota *)
           nextNoteTutorial[];
           playSongEnded = False
        ],
        ImageSize -> {200, Automatic},
        Method -> "Queued", BaseStyle -> {"GenericButton", 16, Bold, Darker[Orange]}, Background -> LightOrange
    ]
)

(* Seleziona random un file MIDI dalla directory MIDI *)
generateMidi[] := (
    Button["Genera casualmente",
        filePath = paths[[RandomInteger[{1, Length[paths]}]]];
        titleTutorial = FileNameTake[filePath];
        (* Estrae le note dal MIDI *)
        midiNotesTutorial = GetNotesFromMidi[filePath];
        (* Ricomincia selezionando la prima nota *)
        nextNoteTutorial[];
        playSongEnded = False,
        ImageSize -> {200, Automatic},
        Method -> "Queued", BaseStyle -> {"GenericButton", 16, Bold, Darker[Orange]}, Background -> LightOrange
    ]
)

(* Dato il path del file MIDI, ritorna una lista di tutte le note che lo compongono *)
GetNotesFromMidi[midiPath_] := (
    sn = Import[midiPath, "SoundNotes"];
    sn = Part[sn, 2];
    Table[sn[[i]][[1]], {i, Length[sn]}]
)

End[];
EndPackage[];



