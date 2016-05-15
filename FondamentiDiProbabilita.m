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
	"CalcoloProbabilita[m,n] restituisce la probabilità che un evento E accada, dati m casi favorevoli e n contrari."

CalcoloProbabilitaContraria::usage =
	"CalcoloProbabilitaContaria[m,n] restituisce la probabilità contraria di un evento, dati m casi favorevoli e n contrari."

PlayableTreCarte::usage = 
	"Function per il gioco delle tre carte."

Begin["Private`"]

(* Function che simula n volte il lancio di un dado. *)
(* Se chiamata senza parametri, il valore di default \[EGrave] 1 *)
LancioDado[n_:1] /; IntegerQ[n] && NonNegative[n] := 
	Table[RandomChoice[{1,2,3,4,5,6}],n]

(* Function che genera un grafico a barre per il lancio di un dado eseguito n volte *)
PlotLancioDado[n_] /; IntegerQ[n] && NonNegative[n] :=
	Module[{trials, sumTrials},
	trials = LancioDado[n];
	sumTrials = Range[6];
	For[i = 1, i < 7, i++,
			sumTrials[[i]] = Count[trials, i];
	];
	BarChart[sumTrials, ChartLabels -> {"1","2","3","4","5","6"}, ChartElementFunction -> "GlassRectangle", ChartStyle -> "Pastel"]
]

(* Calcolo della probabilità di un evento *)
CalcoloProbabilita[favorevoli_, totali_] /; 
	IntegerQ[favorevoli] && NonNegative[favorevoli] &&
	IntegerQ[totali] && NonNegative[totali] && 
	favorevoli <= totali :=
		favorevoli/totali

(* Calcolo della probabilità contraria di un evento *)
CalcoloProbabilitaContraria[favorevoli_, totali_] /; 
	IntegerQ[favorevoli] && NonNegative[favorevoli] &&
	IntegerQ[totali] && NonNegative[totali] && 
	favorevoli <= totali :=
		1 - favorevoli/totali

(* Gioco delle tre carte *)
PlayableTreCarte[] := 
 Module[{positionCards = {1, 2, 3}, tableCards, choise, notRevealed, 
   appoggio, nextchoise},
  tableCards = RandomChoice[Permutations[{1, 0, 0}]];
  Print[tableCards];
  choise = DialogInput[
    DialogNotebook[{
      Row[{TextCell["Scegli una carta"]}],
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
  Print[myHand];
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
  Print[revelated[[1]]];
  nextchoise = DialogInput[
    DialogNotebook[{
      Row[{TextCell["Vuoi cambiare carta?"]}],
      If[revelated[[1]] == 1,
       Row[{
         Import["coverChoose.jpg", "Graphics"],
         Button[Graphics[Import["cover.jpg", "Graphics"]], 
          DialogReturn[2]],
         Button[Graphics[Import["cover.jpg", "Graphics"]], 
          DialogReturn[3]]
         }]
       ],
      If[revelated[[1]] == 2,
       Row[{
         Button[Graphics[Import["cover.jpg", "Graphics"]], 
          DialogReturn[1]],
         Import["coverChoose.jpg", "Graphics"],
         Button[Graphics[Import["cover.jpg", "Graphics"]], 
          DialogReturn[3]]
         }]
       ],
      If[revelated[[1]] == 3,
       Row[{
         Button[Graphics[Import["cover.jpg", "Graphics"]], 
          DialogReturn[1]],
         Button[Graphics[Import["cover.jpg", "Graphics"]], 
          DialogReturn[2]],
         Import["coverChoose.jpg", "Graphics"]
         }]
       ]
      }]];
  If[tableCards[[nextchoise]] == 1,
   DialogInput[
    DialogNotebook[{
      Row[{TextCell["Complimenti, hai vinto!!!"]}],
      If[nextchoise == 1,
       Row[{
         Import["coverWin.jpg", "Graphics"],
         Import["coverChoose.jpg", "Graphics"],
         Import["coverChoose.jpg", "Graphics"]
         }]
       ],
      If[nextchoise == 2,
       Row[{
         Import["coverChoose.jpg", "Graphics"],
         Import["coverWin.jpg", "Graphics"],
         Import["coverChoose.jpg", "Graphics"]
         }]
       ],
      If[nextchoise == 3,
       Row[{
         Import["coverChoose.jpg", "Graphics"],
         Import["coverChoose.jpg", "Graphics"],
         Import["coverWin.jpg", "Graphics"]
         }]
       ]
      }]],
   DialogInput[
    DialogNotebook[{
      Row[{TextCell["Mi dispiace, hai perso!!!"]}],
      If[nextchoise == 1,
       Row[{
         Import["coverWin.jpg", "Graphics"],
         Import["coverChoose.jpg", "Graphics"],
         Import["coverChoose.jpg", "Graphics"]
         }]
       ],
      If[nextchoise == 2,
       Row[{
         Import["coverChoose.jpg", "Graphics"],
         Import["coverWin.jpg", "Graphics"],
         Import["coverChoose.jpg", "Graphics"]
         }]
       ],
      If[nextchoise == 3,
       Row[{
         Import["coverChoose.jpg", "Graphics"],
         Import["coverChoose.jpg", "Graphics"],
         Import["coverWin.jpg", "Graphics"]
         }]
       ]
      }]]
   ];
  ]

End[]
Protect[FondamentiDiProbabilita];
EndPackage[]
