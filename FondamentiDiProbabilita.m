(* ::Package:: *)

(* :Title: FondamentiDiProbabilita *)


(* :Context: ProgrammingInMathematica`FondamentiDiProbabilita` *)
(* :Author: Amerigo Mancino, Andrea Segalini, Bartolomeo Lombardi *)
(* :Summary: un pacchetto per l'introduzione al calcolo delle probabilit\[AGrave] e alla statistica per scuole superiori *)
(* :Copyright: *)
(* :Package Version *)
(* :Mathematica Version: 10.3 *)
(* :History: *)
(* :Keywords: probabilit\[AGrave], statistica, programmazione, scuola superiore *)
(* :Sources: biblio *)
(* :Discussion: *)

BeginPackage["FondamentiDiProbabilita`"]

LancioDado::usage = StringJoin[
	"RollDice[] simula il lancio di un dado, restituendo un valore compreso fra 0 e 6.\n",
	"RollDice[n] simula n volte il lancio di un dado, restituendo una lista di valori compresi fra 0 e 6."
]

PlotLancioDado::usage =
	"PlotLancioDado[n] disegna un istogramma per n prove ripetute del lancio di un dado."

CalcoloProbabilita::usage =
	"CalcoloProbabilita[m,n] restituisce la probabilit\[AGrave] che un evento E accada, dati m casi favorevoli e n contrari."

CalcoloProbabilitaContraria::usage =
	"CalcoloProbabilitaContaria[m,n] restituisce la probabilit\[AGrave] contraria di un evento, dati m casi favorevoli e n contrari."

TreCarte::usage = 
	"TreCarte[] simula il famoso gioco delle tre carte."

RipetiTreCarte::usage =
	"RipetiTreCarte[] simula prove ripetute per il gioco delle tre carte, resitituendo un istogramma dei risultati."

MediaLancioDado::usage =
	"MediaLancioDado[N_] calcola la media di N esiti del lancio di un dado."

BottoneProbabilita::usage =
	"BottoneProbabilita[] genera un bottone che permette di imparare il funzionamento della Formula della Probabilit\[AGrave] Classica."

BottoneProbabilitaN::usage =
	"BottoneProbabilitaN[] genera un bottone che permette di imparare il funzionamento della Formula della Probabilit\[AGrave] Classica."

LanciaDadoProbabilita::usage =
	"LanciaDadoProbabilita[] genera un bottone che permette di lanciare un dado un certo numero di volte."

PlotLanciaDadoProbabilita::usage =
	"PlotLanciaDadoProbabilita[] genera un bottone per eseguire delle prove ripetute per il lancio di un dado."

DadoTreD::usage =
	"DadoTreD[] disegna un dado in tre dimensioni."

BottoneTreCarte::usage = 
	"BottoneTreCarte[] genera un bottone che permette di giocare al Gioco delle Tre Carte."

BottoneTreCarteMultiplo::usage =
	"BottoneTreCarteMultiplo[] genera un bottone che permette di giocare al Gioco delle Tre Carte in maniera ripetuta e di ottenere un grafico dei risultati."

KCombinationNoRipetitions::usage =
	"KCombinationNoRipetitions[elements_List, k_Integer] restituisce la lista delle disposizioni semplici dei possibili eventi presenti nella lista elements_List in un esperimento ripetuto k volte. Non considera le possibili permutazioni."

DrawKCombinationTree::usage =
	"DrawKCombinationTree[elements_List, k_Integer] permette il disegno di un albero che mostra le disposizioni semplici dei possibili eventi presenti nella lista elements_List in un esperimento ripetuto k volte."

KCombination::usage =
	"KCombination[elements_List, k_Integer] restituisce la lista delle disposizioni semplici dei possibili eventi presenti nella lista elements_List in un esperimento ripetuto k volte."

ProveRipetuteSimulazione::usage =
	"ProveRipetuteSimulazione[p_Real, n_Integer, m_Integer] permette di simulare un esperimento di prove ripetute. La probabilit\[AGrave] di successo \[EGrave] p, la simulazione \[EGrave] ripetuta n volte e si vogliono ottenere m successi."

Begin["Private`"]

(* Function che simula n volte il lancio di un dado. *)
(* Se chiamata senza parametri, il valore di default \[EGrave] 1 *)
LancioDado[n_:1] /; IntegerQ[n] && NonNegative[n] := 
	Table[RandomChoice[{1,2,3,4,5,6}],{n}]

(* Function che genera un grafico a barre per il lancio di un dado eseguito n volte *)
PlotLancioDado[n_] /; IntegerQ[n] && NonNegative[n] :=
	Module[{trials, sumTrials},
	trials = LancioDado[n];
	sumTrials = Range[6];
	For[i = 1, i < 7, i++,
			sumTrials[[i]] = Count[trials, i];
	];
	BarChart[sumTrials, ChartLabels -> {"1","2","3","4","5","6"}, ChartElementFunction -> "GlassRectangle", ChartStyle -> "Pastel", ImageSize->Medium]
]

(* Calcolo della probabilit\[AGrave] di un evento *)
CalcoloProbabilita[favorevoli_, totali_] /; 
	IntegerQ[favorevoli] && NonNegative[favorevoli] &&
	IntegerQ[totali] && NonNegative[totali] && 
	favorevoli <= totali :=
		favorevoli/totali

(* Calcolo della probabilit\[AGrave] contraria di un evento *)
CalcoloProbabilitaContraria[favorevoli_, totali_] /; 
	IntegerQ[favorevoli] && NonNegative[favorevoli] &&
	IntegerQ[totali] && NonNegative[totali] && 
	favorevoli <= totali :=
		1 - favorevoli/totali

(* Gioco delle tre carte *)
TreCarte[] := 
 Module[{positionCards = {1, 2, 3}, tableCards, choise, notRevealed, 
   appoggio, nextchoise, revelated, myHand},
  tableCards = RandomChoice[Permutations[{1, 0, 0}]];
  choise = DialogInput[
    DialogNotebook[{
      Row[{TextCell["Scegli una carta."]}],
      Row[{
        Button[Graphics[Import["cover.jpg", "Graphics"]], 
         DialogReturn[1]],
        Button[Graphics[Import["cover.jpg", "Graphics"]], 
         DialogReturn[2]],
        Button[Graphics[Import["cover.jpg", "Graphics"]], 
         DialogReturn[3]]
        }]
      }]
    ];
  myHand = 
   Complement[positionCards, Intersection[positionCards, {choise}]];
  If[tableCards[[myHand[[1]]]] == 0 && 
    tableCards[[myHand[[2]]]] == 0,
   notRevealed = RandomChoice[myHand];
   revelated = Complement[myHand, {notRevealed}],
   If[tableCards[[myHand[[1]]]] == 1,
     notRevealed = myHand[[1]];
     revelated = {myHand[[2]]},
     notRevealed = myHand[[2]];
     revelated = {myHand[[1]]}
     ];
   ];
  If[revelated[[1]] == 1,
   nextchoise = DialogInput[
     DialogNotebook[{
       Row[{TextCell["Vuoi cambiare la carta scelta in precedenza?"]}],
       Row[{
         Import["coverChoose.jpg", "Graphics"],
         Button[Graphics[Import["cover.jpg", "Graphics"]], 
          DialogReturn[2]],
         Button[Graphics[Import["cover.jpg", "Graphics"]], 
          DialogReturn[3]]
         }]
       }]
     ]
   ];
  If[revelated[[1]] == 2,
   nextchoise = DialogInput[
     DialogNotebook[{
       Row[{TextCell["Vuoi cambiare la carta scelta in precedenza?"]}],
       Row[{
         Button[Graphics[Import["cover.jpg", "Graphics"]], 
          DialogReturn[1]],
         Import["coverChoose.jpg", "Graphics"],
         Button[Graphics[Import["cover.jpg", "Graphics"]], 
          DialogReturn[3]]
         }]
       }]
     ]
   ];
  If[revelated[[1]] == 3,
   nextchoise = DialogInput[
     DialogNotebook[{
       Row[{TextCell["Vuoi cambiare la carta scelta in precedenza?"]}],
       Row[{
         Button[Graphics[Import["cover.jpg", "Graphics"]], 
          DialogReturn[1]],
         Button[Graphics[Import["cover.jpg", "Graphics"]], 
          DialogReturn[2]],
         Import["coverChoose.jpg", "Graphics"]
         }]
       }]
     ]
   ];
  If[tableCards[[nextchoise]] == 1,
   If[nextchoise == 1,
    DialogInput[
     DialogNotebook[{
       Row[{TextCell["Complimenti, hai vinto!"]}],
       Grid[{{
          Import["coverWin.jpg", "Graphics"],
          Import["coverChoose.jpg", "Graphics"],
          Import["coverChoose.jpg", "Graphics"]
          }, {Null, Button["Fine Partita", DialogReturn[0]]}}]
       }]
     ]];
   If[nextchoise == 2,
    DialogInput[
     DialogNotebook[{
       Row[{TextCell["Complimenti, hai vinto!"]}],
       Grid[{{
          Import["coverChoose.jpg", "Graphics"],
          Import["coverWin.jpg", "Graphics"],
          Import["coverChoose.jpg", "Graphics"]
          }, {Null, Button["Fine Partita", DialogReturn[0]]}}]
       }]
     ]];
   If[nextchoise == 3,
    DialogInput[
     DialogNotebook[{
       Row[{TextCell["Complimenti, hai vinto!"]}],
       Grid[{{
          Import["coverChoose.jpg", "Graphics"],
          Import["coverChoose.jpg", "Graphics"],
          Import["coverWin.jpg", "Graphics"]
          }, {Null, Button["Fine Partita", DialogReturn[0]]}}]
       }]
     ]];
   Return[0],
   If[tableCards[[1]] == 1,
    DialogInput[
     DialogNotebook[{
       Row[{TextCell["Mi dispiace, hai perso!"]}],
       Grid[{{
          Import["coverWin.jpg", "Graphics"],
          Import["coverChoose.jpg", "Graphics"],
          Import["coverChoose.jpg", "Graphics"]
          }, {Null, Button["Fine Partita", DialogReturn[0]]}}]
       }]
     ]];
   If[tableCards[[2]] == 1,
    DialogInput[
     DialogNotebook[{
       Row[{TextCell["Mi dispiace, hai perso!"]}],
       Grid[{{
          Import["coverChoose.jpg", "Graphics"],
          Import["coverWin.jpg", "Graphics"],
          Import["coverChoose.jpg", "Graphics"]
          }, {Null, Button["Fine Partita", DialogReturn[0]]}}]
       }]
     ]];
   If[tableCards[[3]] == 1,
    DialogInput[
     DialogNotebook[{
       Row[{TextCell["Mi dispiace, hai perso!"]}],
       Grid[{{
          Import["coverChoose.jpg", "Graphics"],
          Import["coverChoose.jpg", "Graphics"],
          Import["coverWin.jpg", "Graphics"]
          }, {Null, Button["Fine Partita", DialogReturn[0]]}}]
       }]
     ]];
   Return[1]];
  ]

(* Function delle prove ripetute del gioco delle tre carte *)
RipetiTreCarte[] := Module[
  {answer = 0, numVinte = 0, numPerse = 0, res},
  While[answer == 0,
   res = TreCarte[];
   If[res == 0,
    numVinte = numVinte + 1,
    numPerse = numPerse + 1
    ];
   answer = ChoiceDialog["Vuoi continuare?", {"Si" -> 0, "No" -> 1}];
   ];
  BarChart[{numVinte, numPerse}, 
   ChartLabels -> {"Vittorie", "Sconfitte"}, 
   ChartElementFunction -> "GlassRectangle", ChartStyle -> "Pastel", ImageSize->Medium]
  ]

(* Function che calcola la media delle uscite del lancio di un dado ripetuto un certo numero di volte *)
MediaLancioDado[n_] := 
	N[Mean[Table[RandomInteger[{1, 6}], {n}]]]

(* Funtion che calcola le disposizioni semplici, senza considerare le possibili permutazioni *)
KCombinationNoRipetitions[elements_List, k_Integer] :=
  Module[{kcombinations, list},
   FindKCombination[list_, i_, n_] := 
    Module[{j, templist, sortedlist},
     If[i < n,
      For[j = 1, j <= Length[elements], j++,
       templist = Append[list, elements[[j]]];
       FindKCombination[templist, i + 1, n];
       ],
      sortedlist = Sort[list ];
      kcombinations = Union[kcombinations, {sortedlist}];
      ]
     ];
   kcombinations = {};
   list :=  {};
   FindKCombination[list, 0, k];
   Return[kcombinations];
   ]

(* Function per il calcolo delle disposizioni semplici *)
KCombination[elements_List, k_Integer] :=
  Module[{kcombinations, list},
   FindKCombination[list_, i_, n_] := 
    Module[{j, templist},
     If[i < n,
      For[j = 1, j <= Length[elements], j++,
       templist = Append[list, elements[[j]]];
       FindKCombination[templist, i + 1, n];
       ],
      kcombinations = Append[kcombinations, list];
      ]
     ];
   kcombinations = {};
   list :=  {};
   FindKCombination[list, 0, k];
   Return[kcombinations];
   ]

(* Funcion per il disegno delle disposizioni semplici servendosi dell'ausilio di un albero *)
DrawKCombinationTree[elements_List, k_Integer] :=
  Module[{kcombinations, list, edges},
   FindKCombination[list_, i_, n_, father_] := 
    Module[{j, templist, node},
     If[i < n,
      For[j = 1, j <= Length[elements], j++,
       templist = Append[list, elements[[j]]];
       kcombinations = Append[kcombinations, templist];
       node = Length[kcombinations];
       edges = Append[edges, templist -> father];
       FindKCombination[templist, i + 1, n, templist];
       ]
      ]
     ];
   kcombinations = {};
   list = {};
   edges = {};
   FindKCombination[list, 0, k, {}];
   TreePlot[edges, VertexLabeling -> True, ImageSize->Large]
   ]

(* Simulazione delle prove ripetute e della distribuzione binomiale *)
ProveRipetuteSimulazione[p_Real, n_Integer, m_Integer] := 
 Module[{randomValues, i, successCounter, j, results},
  results = Table[0, {n + 1}];
  For[j = 1, j < m, j++,
   randomValues = RandomReal[{0, 1}, n];
   successCounter = 0;
   For[i = 1, i <= n, ++i,
    If[randomValues[[i]] <= p,
      successCounter++
      ];
    ];
   results[[successCounter + 1]]++;
  ];
	For[j = 1, j <= Length[results], ++j,
		results[[j]] = N[results[[j]] / m];
	];
	Return[results];
 ]

(* FUCTION AUSILIARIE *)

(* Function per il disegno di un dado a sei facce *)
DadoTreD[] := Module[{fullQuad, faces},
	faces = Import /@ {"http://i.stack.imgur.com/FdfMj.png", 
    "http://i.stack.imgur.com/Qv7w6.png", 
    "http://i.stack.imgur.com/WayOQ.png", 
    "http://i.stack.imgur.com/mgjkA.png", 
    "http://i.stack.imgur.com/qa5bK.png", 
    "http://i.stack.imgur.com/T8szh.png"};

	fullQuad = {{0, 0}, {1, 0}, {1, 1}, {0, 1}};
	cube = PolyhedronData["Cube", "Faces"];

	Graphics3D[
 		MapThread[{Texture[#], 
    	Polygon[cube[[1, #2]], 
    	VertexTextureCoordinates -> fullQuad]} &, {faces, cube[[2, 1]]}],
  Lighting -> "Neutral", Boxed -> False]
]

(* Function ausiliaria del bottone per la Probabilit\[AGrave] classica *)
BottoneProbabilita[] := Module[{output = "Premi il bottone per calcolare la probabilit\[AGrave] di un evento."},
	Button["Probabilit\[AGrave] Classica",
		output = DialogInput[
			{},
			Column[
				{"Casi favorevoli",
				 InputField[Dynamic[favorevoli],Number],"Casi possibili",
				 InputField[Dynamic[possibili],Number],
				 Button["Calcola", 
				 	DialogReturn[
						If[favorevoli > possibili || favorevoli < 0 || possibili < 0,
							 output = "Errore: i casi favorevoli non possono essere maggiori in numero dei casi possibili n\[EGrave] negativi.",
							 output = Grid[{{"Probabilit\[AGrave]:", CalcoloProbabilita[favorevoli,possibili]}}],
							 output = "Inserisci i casi favorevoli e possibili per eseguire il calcolo."
						]
					]
				]
			 }
			]
		], Method->"Queued"]
	Dynamic@output
]

(* Function ausiliaria del bottone per la Probabilit\[AGrave] classica con numericizza *)
BottoneProbabilitaN[] := Module[{output = "Premi il bottone per calcolare la probabilit\[AGrave] di un evento."},
	Button["Probabilit\[AGrave] Classica",
		output = DialogInput[
			{},
			Column[
				{"Casi favorevoli",
				 InputField[Dynamic[favorevoli],Number],"Casi possibili",
				 InputField[Dynamic[possibili],Number],
				 Button["Calcola", 
				 	DialogReturn[
						If[favorevoli > possibili || favorevoli < 0 || possibili < 0,
							 output = "Errore: i casi favorevoli non possono essere maggiori in numero dei casi possibili n\[EGrave] negativi.",
							 output = Grid[{{"Probabilit\[AGrave]:", CalcoloProbabilita[favorevoli,possibili]}}],
							 output = "Inserisci i casi favorevoli e possibili per eseguire il calcolo."
						]
					]
				]
			 }
			]
		], Method->"Queued"]
	Dynamic@N[output]
]

LanciaDadoProbabilita[] := Module[{output = "Premi il bottone per simulare il lancio di un dado a 6 facce.", volte},
	Button["Lancia dado", 
 		output = DialogInput[
   		DialogNotebook[
				{TextCell["Quante volte vuoi lanciare il dado?"], 
     		 InputField[Dynamic[volte], Number], 
     		 Button["Lancia", DialogReturn[
					If[volte > 50 || volte < 1,
						output = "Errore: il numero di lanci deve essere maggiore di uno e non superiore a cinquanta.",	
						output = Grid[{{"Risultati lanci:", LancioDado[volte]}}],
						output = "Inserisci un numero di lanci."
					]]], 
     		 Button["Lancia una sola volta", DialogReturn[output = Grid[{{"Risultato lancio:", LancioDado[1]}}]]]
				}
			]
		], Method -> "Queued"]
	Dynamic@output
]

PlotLanciaDadoProbabilita[] := Module[{output = PlotLancioDado[10], volte},
	Button["Prove ripetute",
 		output = DialogInput[
   		DialogNotebook[{
     		TextCell["Quante volte vuoi lanciare il dado?"],
     		InputField[Dynamic[volte], Number],
     		Button["Lancia", DialogReturn[
					If[volte < 1, 
						output = "Errore: il numero di lanci deve essere maggiore di o uguale ad uno.",					
						PlotLancioDado[volte],
						output = "Inserisci un numero di lanci."
					]]]
			}]
		], Method -> "Queued"]
	Dynamic@output
]

BottoneTreCarte[] = 
  Button["Gioca", TreCarte[], Method -> "Queued"]

BottoneTreCarteMultiplo[] =
 Module[{output = BarChart[{20, 4}, 
   ChartLabels -> {"Vittorie", "Sconfitte"}, 
   ChartElementFunction -> "GlassRectangle", ChartStyle -> "Pastel", ImageSize->Medium]}, 
  Button["Gioca", output = RipetiTreCarte[], 
    Method -> "Queued"] Dynamic@output]

End[]
Protect[FondamentiDiProbabilita];
EndPackage[]
