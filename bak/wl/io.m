(* ::Package:: *)

BeginPackage["DataIO`"];
toDataset::usage = "toDataset[data_List] convert a list-data to Dataset.";
applyDataset::usage = "applyDataset[data_Dataset] apply func from fucLine to all column";
excleData::usage = "excleData[fileName_String,sheetName_String] get data from excle by file=fileName,sheet=sheetName.";
excleIndex::usage = "excleIndex[data_,lookup_Rule] get index by lookup.value";

Begin["`Private`"];
(* toDataset *)
Options[toDataset]={"Head"->1,"ID"->""};
toDataset[data_List,OptionsPattern[]]:= Module[{da=Dataset[Association/@Table[Thread[data[[OptionValue["Head"]]] -> data[[n]]],{n, OptionValue["Head"]+1,  Length[data]}]],id=OptionValue["ID"]},If[id!="",da[GroupBy[id],First],da]];
(* applyDataset *)
Options[applyDataset]={"FucLine"->1};
applyDataset[data_Dataset,OptionsPattern[]]:= Module[{columns=Normal@Normal@DeleteCases[data[OptionValue["FucLine"]],""][All,ToExpression]},If[Length[columns]>0,data[OptionValue["FucLine"]+1;;,columns],data]];
(* excleData *)
Options[excleData]={"LineRange"->{1,-1},"Comment"->"#","Tag"->{}};
excleData[fileName_String,sheetName_String,OptionsPattern[]]:= Module[{da=DeleteCases[Take[Import[fileName, {"Data", sheetName}],OptionValue["LineRange"]],{OptionValue["Comment"],___}],tag,tagFilter},
														tag = DeleteCases[Union[First[da]],""];
														tag = If[Length@OptionValue["Tag"]>0,tag\[Intersection]OptionValue["Tag"],tag];
														tagFilter[t_String]:= t->Transpose[Select[Transpose@da,First[#]==t&]][[2;;]];
														da = If[Length@tag>0,Association[tagFilter/@tag],da];
														Append[da,"tag"->tag]];
(* excleIndex *)
excleIndex[data_Association,match_Rule/;StringQ[Values@match],key_String]:= toDataset[data][FirstPosition[KeyValuePattern[lookup]][[1]]];
excleIndex[data_List,lookup_Rule/;NumberQ[Values@lookup]]:= Module[{v=Min[Values@lookup,data[Last,Keys@lookup]],k=Keys@lookup},data[FirstPosition[n_/;n>=v],k][[1]]];


End[];
EndPackage[];


(*excleIndex::usage = "excleIndex[data_,lookup_Rule] get index by lookup.value";
excleLookUp::usage = "excleLookUp[data_,lookup_Rule,key_String] get key.value by lookup.value";
excleInterpolation::usage = "excleInterpolation[data_Dataset,lookup_Rule,key_String] get interpolation key.value by lookup.value";
*)


(*
excleIndex[data_Dataset,lookup_Rule/;StringQ[Values@lookup]]:= Module[{v=Values@lookup,k=Keys@lookup},data[FirstPosition[n_/;n==v],k][[1]]];
excleIndex[data_Dataset,lookup_Rule/;NumberQ[Values@lookup]]:= Module[{v=Min[Values@lookup,data[Last,Keys@lookup]],k=Keys@lookup},data[FirstPosition[n_/;n>=v],k][[1]]];
excleLookUp[data_Dataset,lookup_Rule,key_]:= Module[{id=excleIndex[data,lookup]},If[id=="NotFound",id,data[id,key]]];
excleLookUp[{from_Dataset,lookup_Rule},{to_Dataset,key_}]:= Module[{id=excleIndex[from,lookup]},If[id=="NotFound",id,to[id,key]]];
excleInterpolation[data_Dataset,lookup_Rule,key_]:= Module[{index=excleIndex[data,lookup]},If[index==1,data[index,key],InterpolatingPolynomial[Normal@Values@data[{index-1,index},{Keys@lookup,key}],x]/.x->Values@lookup]];
*)
