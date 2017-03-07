(* ::Package:: *)

(* To allow import via package name` when this directory is on $Path *)
(*
With[{packagename="ApproximateFourier"},
  Get@FileNameJoin@{ 
    DirectoryName@FindFile[packagename<>"`"],
    packagename<>".m"}]
*)


Module[{packageName,packageFileName,fileName},
  (* $Input is set to Foo.m when evaluating Foo/init.m *)
  If[$Input=="",
    Print["init.m cannot run interactively"];Abort[]];
  packageName=StringDrop[$Input,-2];
  packageFileName=FindFile[packageName<>"`"];
  If[packageFileName==$Failed, 
    Print["Unable to find package "<>packageName];Abort[]];
  fileName=FileNameJoin@{
    DirectoryName@packageFileName,
    packageName<>".m"};
  Check[
    Get[fileName],
    "init.m failed to load package "<>packageName<>" from "<>fileName]
]



