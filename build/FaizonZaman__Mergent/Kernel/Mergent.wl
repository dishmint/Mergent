(* ::Package:: *)
BeginPackage["FaizonZaman`Mergent`"];

DeconstructExpression::usage = "DeconstructExpression[expr] returns an association containing the expr head and the positions of subexpressions";

ReconstructExpression::usage = "ReconstructExpression[<| h -> argindex |>] returns an expression with head h and the keys of argindex as arguments";

MergeExpression::usage = "MergeExpression[e1, e2, ...] merges the ei by head"

Begin["`Private`"];
(* 
	TODO: All arguments should be held
	
	In[5]:= MergeExpression[head[a], head[1 + 1, c]]

	Out[5]= {head[{a, 2}, {c}]}
*)

(* -------------------------- DeconstructExpression ------------------------- *)
(* A deconstructed expression is an Association where the Head of expr maps to the PositionIndex of its arguments *)
DeconstructExpression[expr_?AtomQ] := <| Head[expr] -> <| expr -> {1} |> |>
(* [^] Atomic expressions (a String for example) have no depth, thus "PositionIndex" of the expr is 1 *)

DeconstructExpression[expr_] := <| Head[expr] -> PositionIndex[List @@ expr] |>
(* [^] Any non-atomic expression will apply PositionIndex to the List of exprs arguments *)

(* -------------------------- ReconstructExpression ------------------------- *)
FirstKey[assoc_Association ? AssociationQ] := (First@*Keys)@assoc
(* [^] `Keys` returns a list whether there's one key or more; Atomic expression will always have one key, the expr itself *)

ReconstructExpression[KeyValuePattern[{String -> val_}]] := FirstKey[val]
(* [^] Strings are not expressed in f-a form (String[xyz]) so just the Values of args are taken.*)
ReconstructExpression[KeyValuePattern[{h_ -> args_}]] := Apply[h, Keys[args]]
(* [^] Any non-string expression will reconstruct as the head h applied to the Values of args *)

(* ----------------------------- MergeExpression ---------------------------- *)
HeadCount[exprs_List] := (Map[Head]/*DeleteDuplicates/*Counts/*Total)@exprs;
(* [^] count the number of heads in the list of expressions *)
MergeResult[1, res_] := First[res]
MergeResult[_, res_] := res

MergeExpression[expr_] := expr
MergeExpression[expr__] :=
	Block[
		{exprs, headcount, deconstruct, headmerge, argmerge, heuristicMerge, result},
		exprs = {expr};
		headcount = HeadCount@exprs;

		(* [1] -- Deconstruct all exprssions *)
		deconstruct = Map[DeconstructExpression][exprs];
		(* [2] -- Merge expression by head *)
		headmerge = Merge[Identity]@deconstruct;
		(* [3] -- Merge arguments for each head *)
		argmerge = Query[All, All/*Merge[Identity], KeyValueMap[Rule/*Reverse]/*Association]@headmerge;
		(* [4] -- Delete duplicates | Add any merge heuristics after DeleteDuplicates (DD /* MergeHueristics) *)
		heuristicMerge = argmerge // Query[All, All, DeleteDuplicates];
		(* [5] -- Put the exprs back together *)
		result = heuristicMerge // KeyValueMap[Rule/*MapAt[KeyValueMap[Rule/*Reverse], {2}]/*Association/*ReconstructExpression];
		(* [6] -- Return singletons as the merged expr otherwise the list of merged exprs *)
		MergeResult[headcount, result]
	]

End[];
EndPackage[];