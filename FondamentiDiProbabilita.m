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

TreCarte::usage = 
	"TreCarte[] simula il famoso gioco delle tre carte."

RipetiTreCarte::usage =
	"RipetiTreCarte[] simula prove ripetute per il gioco delle tre carte, resitituendo un istogramma dei risultati."

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
TreCarte[] := 
 Module[{positionCards = {1, 2, 3}, tableCards, choise, notRevealed, 
   appoggio, nextchoise, revelated, myHand},
  tableCards = RandomChoice[Permutations[{1, 0, 0}]];
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
       Row[{TextCell["Vuoi cambiare carta?"]}],
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
       Row[{TextCell["Vuoi cambiare carta?"]}],
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
       Row[{TextCell["Vuoi cambiare carta?"]}],
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
       Row[{TextCell["Complimenti, hai vinto!!!"]}],
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
       Row[{TextCell["Complimenti, hai vinto!!!"]}],
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
       Row[{TextCell["Complimenti, hai vinto!!!"]}],
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
       Row[{TextCell["Mi dispiace, hai perso!!!"]}],
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
       Row[{TextCell["Mi dispiace, hai perso!!!"]}],
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
       Row[{TextCell["Mi dispiace, hai perso!!!"]}],
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
   ChartElementFunction -> "GlassRectangle", ChartStyle -> "Pastel"]
  ]

End[]
Protect[FondamentiDiProbabilita];
EndPackage[]
