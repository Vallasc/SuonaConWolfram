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

Piano::usage = "Piano[pstate] crea un pianoforte virtuale alla modalit\[AGrave] pstate";
KeyDown::usage = "KeyDown[nota, ottava] preme il tasto del piano corrsiposndente alla nota e all'ottava specificata";
KeysUp::usage = "KeyUp rilascia tutti i tasti del piano";
PlaySong::usage = "PlaySong genera l'interfaccia del pianoforte per suonare una traccia MIDI";
PlayFree::usage = "Suona il pianoforte liberamente";
LearnNotes::usage = "LearnNotes[notesList] data una lista di note controlla che il pitch inserito dall'utente sia corretto";
GetNotesFromMidi::usage = "GetNotesFromMidi[midiPath] dato un file MIDI, ritorna una lista di tutte le note che lo compongono";

Begin["`Private`"]

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

(* *********************************************** Parametri *********************************************** *)
(* Volume dei tasti premuti *)
volume = 80;

(* Numero di ottave nel piano in modalit\[AGrave] Learn *)
numOctLearn = 2;
(* Numero di ottave nel piano in modalit\[AGrave] Tutorial*)
numOctTutorial = 2;
(* Numero di ottave nel piano in modalit\[AGrave] Free*)
numOctFree = 2;
(* Ottava iniziale nel piano in modalit\[AGrave] Learn*)
initOctLearn = 3;
(* Ottava iniziale nel piano in modalit\[AGrave] Tutorial*)
initOctTutorial = 3;
(* Ottava iniziale nel piano in modalit\[AGrave] Free*)
initOctFree = 3;

(* Scalatura del piano *)
scale = 2.8;

(* Nota selezionata nel piano in modalit\[AGrave] Learn *)
selectedNoteLearn = "";
(* Nota selezionata nel piano in modalit\[AGrave] Tutorial *)
selectedNoteTutorial = "";
(* Nota premuta nel piano *)
playedNote = "";
(* Note musicali *)
allNotes = List["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
octaveColors = List[ Orange, Blue, Yellow, Magenta, Green, Cyan, Brown, Gray];

(* Load MIDI synth *)
(* Needs["JLink`"]; *)
LoadJavaClass["javax.sound.midi.MidiSystem"];
synth = MidiSystem`getSynthesizer[];
synth @ open[];
(* Canale del synth midi *)
channel = First[synth @ getChannels[]];

(* Converte da nota e ottava a pitch*)
toPitch[note_, oct_] :=
    (First[First[Position[allNotes, note]]] - 1) + (oct * 12);
(* Concatena la nota e l'ottava*)
joinNote[note_, oct_] :=
    StringJoin[note, ToString[oct]];
    
(* Visualizza il pianoforte virtuale a schermo *)
(* pstate in dica la modalit\[AGrave] del piano, 0 = learn, 1 = tutorial, 2 = free *)
Piano[pstate_] :=
    With[{pianostate = pstate},
    
        DynamicModule[{shownotes = False},
        
            (* Chiamata quando viene premuto un tasto del piano *)
            onKeyDown[note_, oct_] := Module[{},
                playedNote = joinNote[note, oct];
                channel @ noteOn[toPitch[note, oct], volume]
            ];
            
            (* Chiamata quando viene rilasciato un tasto del piano *)
            onKeyUp[note_, oct_] := Module[{},
                playedNote = -1;
                channel @ allNotesOff[];
            ];
            
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
                                    Lighter[octaveColors[[oct]], 0.9]
                                ]
                            ,
                            2,
                                White
                        ],
                        Rectangle[{0, 0}, {1, 2}]}, {Line[{{0, 2}, {0, 0}, {1, 0}, {1, 2}}]},
                        If[shownotes,
                            Text[Style[note <> ToString[oct], 10, "Label"], {0.5, 1}] (* TODO Style label *)
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
                                            Lighter[octaveColors[[oct]], 0.9]
                                        ]
                                    ]
                                ,
                                2,
                                    White
                            ],
                            (* Mostra animazione tasti quando vengono premuti, in questo caso l'altezza *)
                            Rectangle[{0, If[dwn,0.15,0]
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
                            (* Mostra animazione tasti quando vengono premuti, in questo caso la larghezza *)
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
                    
                (* Disegna a schermo tutti i tasti bianchi del piano *)
                whiteset[oct_] :=
                    Grid[{Flatten[Map[(whitekey[#, oct])&, {"C", "D", "E", "F", "G", "A", "B"}]]}, Spacings -> 0];
                    
                blackset[oct_] :=
                    Grid[{{
                        vert, space[15], blackkey["C#", oct],
                        space[10], blackkey["D#", oct],
                        space[13], vert, vert, space[14], blackkey["F#", oct], space[10], blackkey["G#", oct], space[10], blackkey["A#", oct], space[14], vert
                    }}, Spacings -> 0, Alignment -> Left];
                    
                keyboardsplit[initOct_, numOct_] :=
                    Grid[{Flatten[Table[blackset[i], {i, initOct, initOct + numOct - 1}]], 
                    Flatten[Table[whiteset[i], {i, initOct, initOct + numOct - 1}]]}, 
                    Spacings -> {"Columns" -> {{0}}, "Rows" -> {{-0.1}}}, 
                    Alignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}}];
                (* Deploy manipulate *)    
                Deploy[keyboardsplit[Switch[pianostate,
                    0,
                        initOctLearn
                    ,
                    1,
                        initOctTutorial
                    ,
                    2,
                        initOctFree
                ], Switch[pianostate,
                    0,
                        numOctLearn
                    ,
                    1,
                        numOctTutorial
                    ,
                    2,
                        numOctFree
                ]]],
                
                {{shownotes, False, "Mostra note"}, {True, False}},
                (* Simboli che vengono tracciati dalla manipulate *)
                TrackedSymbols :> {shownotes, selectedNoteLearn, selectedNoteTutorial,
                                   initOctLearn, initOctTutorial, initOctFree, numOctLearn, numOctTutorial, numOctFree},
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
LearnNotes[notesList_] := Module[{},
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
                    nextNoteLearn,
                    ImageSize -> {200, Automatic},
                    Method -> "Queued", BaseStyle -> {"GenericButton", 16, Bold, Darker[Orange]}, Background -> LightOrange
                ]
                ,
                Button["Start", noteLearnStarted = True; nextNoteLearn,
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
                Button["Avanti ", (Pause[0.6];
                        If[textField != "" && ToUpperCase[textField] == baseNote[selectedNoteLearn] && selectedNoteLearn != "",
                            nextNoteLearn,
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
]

(* Passa alla prossima nota della lista learn*)
nextNoteLearn := Module[{},
	(* Ferma tutte le note gi\[AGrave] in esecuzione *)
    channel @ allNotesOff[];
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
            channel @ noteOn[toPitch[baseNote[selectedNoteLearn], 3], volume]
        ),
        selectedNoteLearn = "END";
    ]
]

baseNote[note_] :=
    First[StringCases[note, {RegularExpression["[A-G]#?"], RegularExpression["\\d+"]}]]
    
octaveNote[note_] :=
    ToExpression[Last[StringCases[note, {RegularExpression["[A-G]#?"], RegularExpression["\\d+"]}]]]

(* *********************************************** Tutorial *********************************************** *)

(* Lista delle note importate dal file MIDI *)
midiNotesTutorial = {};

(* Genera l'interfaccia del pianoforte per suonare una traccia MIDI *)
PlaySong := Module[{},
    titleTutorial = "Suona la traccia MIDI";
    playSongEnded = False;
    Panel[
        Column[{
            Dynamic[Text[Style[titleTutorial, Black, Bold, 24]]],
            Row[{
                generateMidi,
                pickMidi
            }],
            Dynamic[If[selectedNoteTutorial != "END" && selectedNoteTutorial !="",
                 Text[Style["Nota corrente: "<>selectedNoteTutorial, Black, Bold, 20]],
                 Text[""]
            ]],
            Row[{
               Piano[1],
               SwatchLegend[octaveColors, 
                  Table["oct"<>ToString[i], {i, 1, Length[octaveColors]}]
               ]
            }],
            Dynamic[If[playSongEnded,
                  Text[Style["Bravo!", Green, Bold, 20]],
                  Text[""]
             ]]
        }, Alignment -> Center]
    ]
]

(* Passa alla prossima nota della lista tutorial *)
nextNoteTutorial := Module[{},
   (* Controlla se ci sono note disponibili *)
    If[Length[midiNotesTutorial] > 0,
        (* Seleziona la prima nota e la elimina dalla lista *)
        (selectedNoteTutorial = First[midiNotesTutorial];
            midiNotesTutorial = Delete[midiNotesTutorial, 1];
            noteOct = octaveNote[selectedNoteTutorial];
            (* Aggiornamento dinamico dell'ottava durante il tutorial *)
            If[noteOct < initOctTutorial || noteOct > (initOctTutorial + numOctTutorial - 1),
                initOctTutorial = octaveNote[selectedNoteTutorial] - 1;
            ]
        ),
        (* Azzera lo stato *)
        midiNotesTutorial = {};
        selectedNoteTutorial = "END";
        playSongEnded = True;
    ]
]

(* Controlla che la nota suonata sia corretta, se si passa alla prossima *)
checkNote := If[selectedNoteTutorial == playedNote,
    nextNoteTutorial
]

(* Resta in ascolto del cambiamento di playedNote *)
ValueFunction[playedNote] := Module[{},
    If[playedNote != "",
        checkNote
    ]
]

(* *********************************************** MIDI *********************************************** *)
SetDirectory[NotebookDirectory[]];

(* Permette di selezionare un file MIDI dal proprio dispositivo *)
pickMidi := Button["Seleziona file MIDI ",
         filePath = ToString[SystemDialogInput["FileOpen", ".mid"]];
         If[filePath !="$Canceled",
           midiNotesTutorial = GetNotesFromMidi[filePath];
           titleTutorial = FileNameTake[filePath];
           nextNoteTutorial;
           playSongEnded = False
        ],
        ImageSize -> {200, Automatic},
        Method -> "Queued", 
        BaseStyle -> {"GenericButton", 16, Bold, Darker[Orange]}, Background -> LightOrange
    ]
    
(* Seleziona random un file MIDI dalla directory MIDI *)
generateMidi := Button["Genera casualmente ",
        filePath = paths[[RandomInteger[{1, Length[paths]}]]];
        titleTutorial = FileNameTake[filePath];
        (* Estrae le note dal MIDI *)
        midiNotesTutorial = GetNotesFromMidi[filePath];
        (* Ricomincia selezionando la prima nota *)
        nextNoteTutorial;
        playSongEnded = False,
        ImageSize -> {200, Automatic},
        Method -> "Queued", BaseStyle -> {"GenericButton", 16, Bold, Darker[Orange]}, Background -> LightOrange
    ]

(* Dato il path del file MIDI, ritorna una lista di tutte le note che lo compongono *)
GetNotesFromMidi[midiPath_] := Module[{},
    sn = Import[midiPath, "SoundNotes"];
    sn = Part[sn, 2];
    Table[sn[[i]][[1]], {i, Length[sn]}]
]

PlayFree := Piano[2]

(* Non utilizzate! *)
(* Suona la nota all'ottava nel piano corrente*)
KeyDown[note_, oct_] := Module[{},
        selectedNote = toPitch[note, oct]; channel @ noteOn[toPitch[note, oct], volume]
 ]
(* Rilascia la nota nel piano corrente*)
KeysUp := Module[{},
        selectedNote = -1; channel @ allNotesOff[]
]
        
End[];
EndPackage[];






