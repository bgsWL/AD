(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 20-Oct-2016 *)

BeginPackage["AD`"]
(* Exported symbols added here with SymbolName::usage *) 

makePartial::usage=""
propagatead::usage=""
ad::usage=""
toValue::usage=""
recursiveStatistics::usage=""


Begin["`Private`"]
(* Implementation of the package *)

(* TODO: get rid of these *)
mathSymbolsOne = {Sin, Cos, Tan, Exp, Log, Erf, Erfc};
mathSymbolsTwo = {Power};

makePartial[s_, expr_, pos_] := Apply[s][ReplacePart[expr, pos->#]] &

(* TODO: make it work with arrays *)
(* One variable *)
propagatead /: propagatead[s_, expr_, assoc_Association] := 
	With[{local=D[s[\[FormalZ]], \[FormalZ]] /. \[FormalZ] -> expr}, 
	Association[KeyValueMap[#1 -> #2 local &, assoc]]
	]
propagatead /: propagatead[s_, expr_List, assoc_List] := 
	Merge[Map[propagatead[makePartial[s, expr, #], expr[[#]], assoc[[#]]] &, Range[Length[expr]]], Total]

ad /: c_?NumericQ + ad[x_, y_Association] := ad[c + x, y]
ad /: c_?NumericQ ad[x_, y_Association] := ad[c x, Association[KeyValueMap[#1 -> c #2 &, y]]]
(* TODO: The conjugate of an expressions keeps the same sensitivities*)
ad /: Conjugate[ad[x_, y_Association]] := ad[Conjugate[x], y]
ad /: Power[ad[x_, y_], 0] := ad[1, Association[KeyValueMap[#1 -> 0 &, y]]]
ad /: Power[ad[x_, y_], n_?NumericQ] := ad[x^n, Association[KeyValueMap[#1-> n x^(n-1) #2 &, y]]]
ad /: Power[a_?NumericQ, ad[x_, y_]] := ad[a^x, propagatead[a^# &, x, y]]
ad /: ad[expr1_, y1_] + ad[expr2_, y2_] := ad[expr1 + expr2, propagatead[#1+#2 &, {expr1, expr2}, {y1, y2}]]
ad /: ad[expr1_, y1_] ad[expr2_, y2_] := ad[expr1 expr2, propagatead[#1 #2 &, {expr1, expr2}, {y1, y2}]]
ad /: s_[ad[x_, y_Association]] /; MemberQ[mathSymbolsOne, s] := ad[s[x], propagatead[s, x, y]];
ad /: s_[ad[x1_, y1_Association], ad[x2_, y2_Association]] /; MemberQ[mathSymbolsTwo, s] := 
	ad[s[x1, x2], propagatead[s[#1, #2] &, {x1, x2}, {y1, y2}]]

(* TODO: why message ? *)
toValue[expr_] := expr /. x_ad -> x[[1]]

recursiveStatistics[x_List] := Module[
	{out, out2, length=Length[x], counter=2},
	out=x[[1]];
	out2=x[[1]]^2;
	While[counter <= length,
	out = out + x[[counter]];
	out2 = out2 + x[[counter]]^2;
	counter++
	];
	{out / length, Sqrt[out2 / length - (out / length)^2] / Sqrt[length]}
	]

End[]
EndPackage[]

