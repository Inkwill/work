(* ::Package:: *)

BeginPackage["Base`"];
excleData::usage = "getDataset[fileName_,tag_] get dataSet from excle by file=fileName,tag.";
excleIndex::usage = "excleIndex[data_,lookup_Rule] get index by lookup.value";
excleLookUp::usage = "excleLookUp[data_,lookup_Rule,key_String] get key.value by lookup.value";
excleInterpolation::usage = "excleInterpolation[data_Dataset,lookup_Rule,key_String] get interpolation key.value by lookup.value";
mergeDataset::usage = "mergeDataset[data_List] merge two Datasets by 'Total'";
joinDataset::usage = "joinDataset[data_List] join two Datasets";
createDataset::usage = "createDataset[keys_List, values_List] create Dataset by keys and values";
applyDataset::usage = "applyDataset[data_Dataset,startLine_Integer] apply func from first line to column";

getXls::usage = "getXls[fileName_String,sheetName_String,OptionsPattern[]] get a excle table to dataset.";
addBOM::usage = "addBOM[fileName_String] add the BOM({239,187,191}) to a utf8 file.";
xlsExport::usage = "xlsxExport[fileName_String] export txt file(unicode)from every sheet.";
txtToCSV::usage = "txtToCSV[fileName_String] export csv file(utf8)from txt(unicode).";
txtToXLS::usage = "txtToXLS[fileName_String] export xlsx file from txt(unicode).";
interList::usage = "interList[min,max,num] get a inter num-list between [min,max)";
incrementSmooth::usage = "incrementSmooth[list] smooth the list data to increment"; 
importDataset::usage= "import simple data to dataset";
percentDataset::usage = "Normalize a column of dataset";
ruleParser::usage = "parse string into rules: key -> value";


Begin["`Private`"];
excleData[fileName_String,sheetName_String] := Import[fileName, {"Data", sheetName}]; 
excleData[fileName_String,sheetName_String,tag_] := Module[{data=Import[fileName,{"Data", sheetName}],da},
	da = DeleteCases[Table[Pick[data[[n]], data[[1]],If[ListQ[tag],Alternatives@@tag,tag]], {n, 2, Length[data]}],{x_,y___}/;x=="#"];
	If[First[da]=={"key","value"},(da/.{x_,y_}->{x->y})[[2;;]] //Association,
	Association /@ Table[Thread[da[[1]] -> da[[n]]], {n, 2,  Length[da]}] // Dataset]];
excleData[fileName_String,sheetName_String,tag_,update_]:= Module[{data= excleData[fileName,sheetName,tag]},data[Select[#update!=""&],DeleteCases[Normal@First@Keys@data,update]]];
excleIndex[data_Dataset,lookup_Rule/;StringQ[Values@lookup]]:= Module[{v=Values@lookup,k=Keys@lookup},data[FirstPosition[n_/;n==v],k][[1]]];
excleIndex[data_Dataset,lookup_Rule/;NumberQ[Values@lookup]]:= Module[{v=Min[Values@lookup,data[Last,Keys@lookup]],k=Keys@lookup},data[FirstPosition[n_/;n>=v],k][[1]]];
excleLookUp[data_Dataset,lookup_Rule,key_]:= Module[{id=excleIndex[data,lookup]},If[id=="NotFound",id,data[id,key]]];
excleLookUp[{from_Dataset,lookup_Rule},{to_Dataset,key_}]:= Module[{id=excleIndex[from,lookup]},If[id=="NotFound",id,to[id,key]]];
excleInterpolation[data_Dataset,lookup_Rule,key_]:= Module[{index=excleIndex[data,lookup]},If[index==1,data[index,key],InterpolatingPolynomial[Normal@Values@data[{index-1,index},{Keys@lookup,key}],x]/.x->Values@lookup]];
mergeDataset[data_List]:= Merge[Normal/@data,Total]//Dataset;
mergeDataset[data_List,func_]:= MapThread[func,Normal/@data]//Dataset;
joinDataset[data_List]:= Transpose[Join@@Transpose/@data];
createDataset[keys_List,values_List]:=Dataset[Apply[Association,#]&/@Transpose@MapThread[Thread[#1->#2]&,{keys,values}]];
createDataset[indexs_List,keys_List,values_List]:=Dataset[Association@Thread[indexs->(Apply[Association,#]&/@Transpose@MapThread[Thread[#1->#2]&,{keys,values}])]];
applyDataset[data_Dataset,funcLine_Integer]:= Module[{columns=Normal@Normal@DeleteCases[data[funcLine],""][All,ToExpression]},If[Length[columns]>0,data[funcLine+1;;,columns],data]];
applyDataset[data_Dataset]:= applyDataset[data,1];

Options[getXls]={"StartLine"->3};
getXls[fileName_String,sheetName_String,OptionsPattern[]]:= Module[{data,startLine=OptionValue["StartLine"]},
	data = Import[fileName,{"Data",sheetName}][[startLine;;]];
	Dataset[Association/@Table[Thread[data[[1]]->data[[n]]],{n,2,Length@data}]][All,All,If[NumberQ[#]&&FractionalPart[#]==0,IntegerPart[#],#]&]];
addBOM[fileName_String]:= Export[fileName,ByteArray[Join[{239,187,191},Normal[ReadByteArray[fileName]]]],"Binary"];
xlsExport[fileName_]:=Module[{sheets=Import[fileName,"Sheets"],data,file},data=AssociationThread[sheets->Import[fileName]];file=(Export[FileNameJoin[Append[FileNameSplit[fileName][[;;-2]],If[NumberQ[ToExpression@StringTake[#,Min[4,StringLength@#]]],StringTake[#,{Min[6,StringLength@#],-1}],#]<>".csv"]],data[#]/. x_/;And[NumberQ@x,FractionalPart[x]==0]:> IntegerPart[x],"CSV",CharacterEncoding->"Unicode"]&/@sheets);file];
txtToCSV[fileName_]:=Module[{file=Export[StringReplace[fileName,".txt"->".csv"],Import[fileName,"Data",CharacterEncoding->"Unicode"],"CSV",CharacterEncoding->"UTF8","TextDelimiters"->""]},addBOM@file];
txtToXLS[fileName_]:=Module[{file=Export[StringReplace[fileName,".txt"->".xlsx"],Import[fileName,"Data",CharacterEncoding->"Unicode"],"XLSX"]},file];
interList[min_,max_,num_]:= Table[min+IntegerPart@N[n*(max-min)/num],{n,0,num-1}];
incrementSmooth[data_List]:= Module[{d=Sort@data},Flatten@Fold[If[Length@#2>1,Append[#1,interList[d[[First@#2]],If[Length[d]>(Last[#2]+1),d[[Last@#2+1]],Last[d]*2],Length@#2]],Append[#1,d[[First@#2]]]]&,{},Values@PositionIndex@d]];
importDataset[file_]:= Module[{data=Import[file,"Data"]},Dataset[Table[Association@@Thread[data[[1]]-> data[[n]]],{n,2,Length@data}]]];
percentDataset[data_,key_String]:= data[All,{key->(PercentForm[N[#/data[Total,key]]]&)}];
percentDataset[data_,key_List]:= Fold[percentDataset,data,key];
ruleParser[str_String]:= Rule[First@#,ToExpression[Last@#]]&@StringSplit[str,"->"];


End[];
EndPackage[];







