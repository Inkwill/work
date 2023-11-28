(* ::Package:: *)

BeginPackage["sslib`"];
Needs["Base`",FileNameJoin[{$UserDocumentsDirectory,"Wolfram Mathematica","base.m"}]];
ssDataPath::usage = "data path";
ssCN::usage = "txet of CN";
ssIAP::usage = "iap data of ss";
ssItem::usage = "item data of ss";
itemInfo::usage = "itemInfo";
itemGroup::usage = "itemGroup by string(item1|num1,item2|num2)";
getCN::usage = "get text_cn by text";


Begin["Private`"];
ssDataPath = "D:\\Work\\SS\\_data\\";
ssCN = Base`importDataset[FileNameJoin[{ssDataPath,"config","key_cn_20220415.tsv"}]];
ssItem = Base`importDataset[FileNameJoin[{ssDataPath,"config","itemlist.tsv"}]];
ssIAP =  Base`importDataset[FileNameJoin[{ssDataPath,"config","iap_package.tsv"}]][GroupBy["id"],First,{"reward_resource","reward_item","alliance_reward_item","bonus_diamond^c","discount","iap_class","iap_type"}];
itemInfo[id_]:= ssItem[GroupBy["id"],First,{"id","name","description"}][id,{"name"->getCN,"description"->getCN}];
getCN[key_String]:= ssCN[Select[#Keys==key&],Last]//Normal//First;
itemGroup[str_String]:= Module[{data = Association[Thread[First@#->Last@#]&[StringSplit[#,"|"]]&/@StringSplit[str,","]]},Values[itemInfo[Keys@data][All,Append[#,"num"->data[#["id"]]]&]]];


End[];
EndPackage[];
