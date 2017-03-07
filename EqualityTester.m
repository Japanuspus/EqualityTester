(* ::Package:: *)

(* ::Title:: *)
(*EqualityTester*)


(* ::Text:: *)
(*A Mathematica package for testing equalities numerically.*)
(*(C) janus@insignificancegalore.net, 2010*)
(*EqualityTester is released under Creative Commons Attribution-ShareAlike 3.0 Unported License, see http://creativecommons.org/licenses/by-sa/3.0/.  No warranties.*)


(* ::Section::Closed:: *)
(*Package Prolog*)


BeginPackage["EqualityTester`"]


(* ::Section:: *)
(*Declarations*)


DistanceMeasure::usage="\
An option specifying a \
function which must return a distance between two expressions. \
Default is (Norm[#1-#2]/Norm[#1+#2]&).\
The expressions are deemed equal if the distance is less than Tolerance";
DefaultRNG::usage="\
An option specifying an expression that evaluates to a random number.\
Used to asign values to symbols matched by the Automatic rule.";


EqualityTest::usage="\
`EqualityTest[lhs, rhs, [distributions,] [Options]]` \
compares lhs and rhs numerically by assigning values to all symbols as \
specified by `distributions`, calculating a distance between `lhs` and `rhs` \
using `DistanceMeasure` and comparing it to `Tolerance`. This is repeated \ 
`MaxIterations` times.

The distributions parameter must be a list {p_1,p_2,...} where p_i is either 
Automatic, a pattern  or a delayed rule `pattern:>RNG`, e.g. `x:>RandomReal[{-1,1}]`. \
It defaults to `{Automatic}`
All components can be `Automatic`, e.g
-  `Automatic:>Automatic`
-  `Automatic:>RandomReal[{0,1}]`
-  `Subscript[y,_]:>Automatic (equivalent to Subscript[y,_] )`
The default/`Automatic` pattern is `s_Symbol/;Not@Or[ValueQ[s],NumericQ[s]]`.
The default/`Automatic` RNG is `RandomReal[NormalDistribution[]]`, \
this can be changed with the `DefaultRNG` option.";
MakeEqualityTester::usage="\
`eq=MakeEqualityTester[{p1,p2,...}]` returns an equality test function eq so that\
`MakeEqualityTester[args][lhs,rhs]` is equivalent to `EqualityTest[lhs,rhs,args___]`.";


(* ::Section:: *)
(*Implementation*)


Begin["`Private`"];


Options[MakeEqualityTester]={
  Tolerance->10^-($MachinePrecision/2),
  MaxIterations->10,
  DistanceMeasure->(Norm[#1-#2]/Sqrt[Norm[#1]^2+Norm[#2]^2]&),
  DefaultRNG:>RandomReal[NormalDistribution[0,1]]
};
SetAttributes[MakeEqualityTester,HoldFirst];
EqualityTest::"notEqual"="\
The expressions `` and `` had unequal values `` and `` at the following point:";


EqualityTest[lhs_,rhs_,args___]:=MakeEqualityTester[args][lhs,rhs]


MakeEqualityTester[
  symbols_List:{Automatic},
  opts:OptionsPattern[]
]:=Module[{rngopts,rng,mpatterns},
  rngopts=FilterRules[Join[Flatten[{opts}],
      Options[MakeEqualityTester]],DefaultRNG];
  rng=DefaultRNG/.(rngopts/.HoldPattern[a_:>b_]:>a->Hold[b]);
  (* Fill in for Automatics in the pattern list *)
  mpatterns=Replace[Replace[Unevaluated[symbols],{
        Automatic -> {Automatic, Automatic},
        p:Except[_:>_]->{p, Automatic},
        HoldPattern[s_:>g_]->{s,Hold[g]}
      },1],
	{p_,d_} :> {
      p/. Automatic -> sym_Symbol/;Not@Or[ValueQ[sym],NumericQ[sym]],
      d/. Automatic -> rng },1];
  (* To construct a standalone function, 
     use With to insert values into unevaluated body *)
  With[{
      wpatterns=mpatterns,
      wn=OptionValue[MaxIterations],
      wtolerance=OptionValue[Tolerance],
      wdistancemeasure=OptionValue[DistanceMeasure]},
    Function[{lhs,rhs},Module[{
        n=wn,
        tolerance=wtolerance,
        patterns=wpatterns,
        distancemeasure=wdistancemeasure,
        vars,values,result},
      (* Each pattern may correspond to more than one variable *)
      vars = DeleteDuplicates@Flatten[
        Cases[{lhs,rhs},First[#1],Infinity]&/@patterns];
      (* Duplicate RNG expressions to get independent values *)
      values = (#1->(#1/.Apply[Rule,patterns,{1}])&)/@vars;
      result=NestWhile[
        With[{valuesnow=ReleaseHold[values]},
          If[distancemeasure[lhs/.valuesnow,rhs/.valuesnow]<tolerance,
            True, valuesnow]
        ]&,True,TrueQ,1,n^Length[vars]];
      (* Check whether nestwhile terminated because of MaxIterations *)
      If[!TrueQ[result],
         Message[EqualityTest::"notEqual",
           HoldForm[lhs],HoldForm[rhs],
           lhs/.result,rhs/.result]];
      result]
,HoldAll]]]


(* ::Section::Closed:: *)
(*Epilog*)


End[]
EndPackage[]
