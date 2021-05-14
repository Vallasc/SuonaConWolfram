(* ::Package:: *)

(* :Title : SuonaConWolfram *)
(* :Context : SuonaConWolfram` *)
(* :Author : Gruppo 5 *)
(* :Summary : Package che permette di creare un paino virtuale e di effettuare delle operazioni su di esso *)
(* :Copyright : Gruppo 5 2021 *)
(* :Package Version : 1 *)
(* :Mathematica Version : 12.2 *)
(* :History : *)
(* :Discussion : *)

BeginPackage["SuonaConWolfram`"]

Piano::usage = "Piano[ottava, onDwn, onDwnPar, onUp, onUpPar] ritorna un nuovo pianoforte che parte da octave";
KeyDown::usage = "KeyDown[nota, ottava] preme il tasto del piano corrsiposndente alla nota e all'ottava specificata";
KeysUp::usage = "KeyUp[] Rilascia tutti i tasti del piano";

Begin["`Private`"]

(* Load MIDI synth *)
Needs["JLink`"];
LoadJavaClass["javax.sound.midi.MidiSystem"];
synth = MidiSystem`getSynthesizer[];
synth @ open[];
$Channel = First[synth @ getChannels[]];

allNotes = List["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
toPitch[note_, oct_] :=
    (First[First[Position[allNotes, note]]] - 1) + (oct * 12);

selectedNote = -1;
KeyDown[note_, oct_] :=
    Block[{},
        selectedNote = toPitch[note, oct]; $Channel @ noteOn[toPitch[note, oct], 80]
    ];
KeysUp[] :=
    Block[{},
        selectedNote = -1; $Channel @ allNotesOff[]
    ];

Piano[octave_, onDwn_, onDwnPar_, onUp_, onUpPar_] :=
    DynamicModule[{scale = 2, shownotes = False, pianoInitOctave = octave, playedNote = -1},
        onKeyDown[note_, oct_] :=
            Block[{},
                playedNote = toPitch[note, oct];
                $Channel @ noteOn[toPitch[note, oct], 80];
                onDwn[onDwnPar]
            ];
        onKeyUp[note_, oct_] :=
            Block[{},
                playedNote = -1;
                $Channel @ allNotesOff[];
                onUp[onUpPar]
            ];
        Manipulate[
            scale = 2;
            whitekey[note_, oct_] :=
                Mouseover[(* Normal not pressed *)
                    Graphics[{{EdgeForm[],
                    If[selectedNote == toPitch[note, oct],
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
                        If[selectedNote == toPitch[note, oct],
                            Lighter[Green]
                            ,
                            If[dwn && selectedNote != -1,
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
                Graphics[{White, Rectangle[{0, 0}, {wd, 1}]}, ImageSize -> {wd * scale, Automatic}, PlotRange -> {{0, 0}, {2.5, 5}}, ImageMargins -> 0];
            vert :=
                Graphics[Line[{{0, 0}, {0, 1}}], PlotRange -> {{-0.001, 0.001}, {0, 1}}, ImageSize -> {2 * scale, 60 * scale}, ImageMargins -> 0];
            blackkey[note_, oct_] :=
                Mouseover[(* Normal not pressed *)
                    Graphics[{{EdgeForm[Black],
                    If[selectedNote == toPitch[note, oct],
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
                        If[selectedNote == toPitch[note, oct],
                            Darker[Green]
                            ,
                            If[dwn && selectedNote != -1,
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
                    space[14.01], vert, space[0.01], vert, space[14.5], blackkey["F#", oct], space[11], blackkey["G#", oct], space[10.5], blackkey["A#", oct], space[14], vert
                }}, Spacings -> -0.04, Alignment -> Left];
            keyboardsplit[octinit_] :=
                Grid[{Flatten[Table[blackset[i], {i, octinit, octinit + 1}]], Flatten[Table[whiteset[i], {i, octinit, octinit + 1}]]}, Spacings -> {"Columns" -> {{0}}, "Rows" -> {{-0.1}}}, Alignment -> {"Columns" -> {{Left}}, "Rows" -> {{Top}}}];
            Deploy[keyboardsplit[pianoInitOctave]],
            {{shownotes, False, "show notes"}, {True, False}},
            TrackedSymbols :> {shownotes, selectedNote},
            Initialization :> {dwn = False},
            LocalizeVariables -> False,
            ContinuousAction -> False
        ]
    ]


End[]
EndPackage[]
