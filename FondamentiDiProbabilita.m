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
 Module[{positionCards = {1, 2, 3}, tableCards, choise, myHand, 
   notRevealed, appoggio, temp2},
  tableCards = RandomChoice[Permutations[{1, 0, 0}]];
  choise = 
   ChoiceDialog[
    "Scegli una carta", {prima -> 1, seconda -> 2, terza -> 3}];
  myHand = 
   Complement[positionCards, Intersection[positionCards, {choise}]];
  If[tableCards[[myHand[[1]]]] == 0 && 
    tableCards[[myHand[[2]]]] == 0,
   notRevealed = RandomChoice[myHand],
   If[tableCards[[myHand[[1]]]] == 1,
     notRevealed = myHand[[1]],
     notRevealed = myHand[[2]]
     ];
   ];
  temp2 = ChoiceDialog["Vuoi cambiare carta?", {Si -> 1, No -> 2}];
  If[temp2 == 1,
   appoggio = notRevealed;
   notRevealed = choise;
   choise = appoggio;
   ];
  If[tableCards[[choise]] == 1,
   MessageDialog["Hai vinto!"],
   MessageDialog["Hai perso!"]
   ];
  ]

(* Calcolo della somma di eventi *)
CalcolaSomma[eventi_ /; ListQ[eventi], probEventi /; ListQ[probEventi], listaDiEventi_ /; ListQ[listaDiEventi]] :=
	



End[]
Protect[FondamentiDiProbabilita];
EndPackage[]
