(* ::Package:: *)

BeginPackage["ssrlib`"];
Needs["Base`",FileNameJoin[{$UserDocumentsDirectory,"Wolfram Mathematica","base.m"}]];
(*config*)
csvPath::usage = "csv path";
basePath::usage = "C:\\ssr-config\\trunk\\config\\designer";
modelPath::usage = "C:\\ssr-config\\trunk\\config\\designer\\ssr-model.xlsx";
battlePath ::usage = "C:\\ssr-config\\trunk\\config\\designer\\SSR\:914d\:7f6e\:8868_\:6218\:6597\:5c5e\:6027.xlsm";
economyPath::usage = "C:\\ssr-config\\trunk\\config\\designer\\SSR\:914d\:7f6e\:8868_\:7ecf\:6d4e\:6295\:653e.xlsm";
tablePath::usage = "C:\\ssr-config\\trunk\\config\\designer\\SSR\:7ed3\:6784\:8868_\:603b\:89c8.xlsx";
mainConfig::usage = "modelPath -> main -> main";
operateConfig::usage = "modelPath -> operate -> base";
userConfig::usage = "modelPath -> benchmark -> user";
ltvConfig::usage = "modelPath -> benchmark -> ltv";
valueConfig::usage = "economy -> value -> item";
itemData::usage = "ssrTable['item']";
igpData::usage = "ssrTable['itemGroup']";
attrData::usage = "ssrTable['attrGroup']";

userLayer::usage = "\:8d85,\:5927,\:4e2d,\:5c0f";
maxDay ::usage = "the max day of planning";
maxLv ::usage = "the max lv of planning";

(*normal*)
int::usage = "convert value type to integer";
strRange::usage:= "convert 'min,max' to Range[min,max]";
lvParam::usage := "lvParam[config_String] get a param value by lv, config: rangeList->ValueList";
strFunc::usage := "strFunc[param_] get f[x]:=ToExpression[text]/.param->x "; 
(*err message*)
ssrErr::notFound = "The input [`1`] is not found in [`2`]";

(*main*)
lvToday::usage = "lvToday[lv_] get day by main lv.";
dayTolv::usage = "dayTolv[day_] get main lv by day.";
lvToMobLv::usage = "";
moblvToLv::usage = "";


(*pay*)
ltvMark::usage = "ltvMark[day_] get ltv benchmark of ssr";
ltv::usage = "ltv[day_] get ltv data";
ltvK::usage = "ltvK[ltvFunc_,day_] get k(ltv[n]= ltv[1] * (1 + (n-1)*k) of a group of ltvData by ltvFunc";

(*ssrTable*)
ssrTable::usage = "table data of ssr : Association";
ssrTableKey::usage = "ssrTableKey[tableName,...] get the Keys by some rule from a ssr-table";
ssrExportTable::usage = "export table of ssr from Dataset";
ssrSetTable::usage = "set table row-data of ssr from Association or ssr-table";
ssrDelTable::usage = "delete rows from Association or ssr-table";
ssrSetColumn::usage = "set columns from ssr-table by rule: {columnName->function} or {columnName->List}";
ssrRefIntKey::usage = "ssrRefIntKey[table]refresh int-key for a ssr-table";
ssrSortById::usage = "ssrSortById[table]sort ssr-table by int-key";
ssrTableForm::usage = "ssrTableForm[tableName] get the structure of a ssr-table";
ssrSplitTableRule::usage = "ssrSplitTableRule[key] get the ssr-table split rule by key";
ssrReplaceCell::usage = "ssrReplaceCell[tableName_String,replace_] replace all cell from tableName by replace_rule";
(*attr*)
ssrAttrByGroup::usage = "ssrAttrByGroup[groupId_String] get the attrData like <|attrName->attrValue|> by attrgroup.csv";
ssrCalculateAttr::usage = "ssrCalculateAttr[attrlist_List,fomula_Association] calculate the attrlist by fomula";
attrFomula::usage = "attrFomula[type_String] get attr fomula {'se','slg'}";
ssrPower::usage = "ssrPower[config_Dataset,data_Association] caculate the attr-power from data by config";
(*hero*)
heroLv::usage = "";
lvToherolv::usage = "lvToherolv[Lv_] get hero lv by mainLv.";
ssrHeroAttr::usage = "ssrHeroAttr[id_String,lv_Integer,star_Integer] get hero attr info";
ssrNpcAttr::usage = "ssrNpcAttr[id_String] get the senpc attr info";
(*item*)
itemValue::usage = "itemValue[name_String] get the item value.";
itemNum::usage = "itemNum[data_Association] get the item num of <|name->totalValue|>.";
itemName::usage = "";
itemNameText::usage = "";
itemId::usage = "";
itemPrice::usage = "";
igpForm::usage = "igpForm[data_] get a itemgroup form like <|\:9053\:5177->item, \:6570\:91cf->itemNum,\:6743\:91cd->itemWeight|> ";
igpFill::usage = "igpFill[data_Association] fill StringId\:3001blank-items and other field like \:6ce8\:91ca,\:5305\:7c7b\:578b,\:83b7\:5f97\:662f\:5426\:5f39\:7a97,\:5f39\:7a97\:7c7b\:578b";
igpInfo::usage = "";
igpshow::usage = "";
itemQ::usage = "";
igpItemNum::usage = "";
(*other*)
dayToDuration::usage = " days -> {d,h,m,s}";
ssrAttrGroupForm::usage = "ssrAttrGroupForm[data_Association,rule_Dataset{attrKey,attrName,index}] get <|\:5c5e\:6027n->attrName,\:6570\:503cn->value|> by attrKey->value";
ssrAnalyzeTable::usage = "convert analyzeData to table";
ssrExtendAnalyze::usage = "extend data from analyzeData table";
ssrDeleteColumn::usage = "delete columns from ssr-table";
ssrDataFromExcle::usage = "load data from a excle file";
ssrSplitTable::usage = "ssrSplitTable[data_Dataset,rules] split ssr-table by rule or rules";



Begin["Private`"];
(*normal*)
int[value_]:= If[value=="","",IntegerPart@value];
strRange[text_String]:= Range@@ToExpression@StringSplit[text,","]
lvParam[map_Rule]:= Function[{lv},Module[{index=FirstPosition[Keys@map,_?(#>=lv&)][[1]]},If[index=="NotFound",Values[map][[-1]],Values[map][[index]]]]];
lvParam[config_String]:= lvParam[ToExpression@config];
strFunc[param_String]:= Function[{body},If[body=="","",ToExpression[StringReplace[body<>"&",param->"#"]]]];
strFunc[params_List]:= Function[{body},If[body=="","",ToExpression[StringReplace[body<>"&",Thread[params->("#"<>ToString[#]&/@Range[Length@params])]]]]];

(*config*)
csvPath = "C:\\ssr-config\\branches\\sprint_0.6.5\\config\\csv\\";
basePath = "C:\\ssr-config\\trunk\\config\\designer";
modelPath = FileNameJoin[{basePath,"ssr-model.xlsx"}]; 
battlePath = FileNameJoin[{basePath,"SSR\:914d\:7f6e\:8868_\:6218\:6597\:5c5e\:6027.xlsm"}]; 
economyPath = FileNameJoin[{basePath,"SSR\:914d\:7f6e\:8868_\:7ecf\:6d4e\:6295\:653e.xlsm"}];
tablePath = FileNameJoin[{basePath,"SSR\:7ed3\:6784\:8868_\:603b\:89c8.xlsx"}];
mainConfig = Base`excleData[modelPath,"main","main"][Select[#lv!=""&],All][All,{"lv"->IntegerPart,"lv(pay)"->IntegerPart}];
operateConfig = Base`excleData[modelPath,"operate","base"][Select[#id!=""&],All];
userConfig = Base`excleData[modelPath,"benchmark","user"][Select[#user!=""&],All];
ltvConfig = Base`excleData[modelPath,"benchmark","ltv"][Select[#day!=""&],All][All,{"day"->IntegerPart}];
valueConfig = Base`applyDataset[Base`excleData[economyPath,"value","item"]][Select[#name!=""&]];
userLayer = AssociationThread[Normal@userConfig[All,"user"]->Normal@userConfig[All,"arppu"]];
maxDay =  IntegerPart@mainConfig[Last,"day"];
maxLv = IntegerPart@mainConfig[Last,"lv"];

(*item*)
itemData = ssrTable["Item"];
igpData = ssrTable["ItemGroup"];
itemQ[key_]:= If[StringQ[key],itemQ[key],False];
itemQ[key_String]:= MemberQ[Keys@itemData,itemId[key]];
itemValue[key_String]:= Module[{keyTitle=If[MemberQ[valueConfig[All,"name"],key],"name","id"]},Base`excleLookUp[valueConfig,keyTitle->key,"value"]];
itemValue[numData_Association]:= Merge[KeyValueMap[<|#1->#2*itemValue[#1]|>&,numData],Total];
itemNum[valueData_Association]:= Merge[KeyValueMap[<|#1->Round[#2/itemValue[#1],1]|>&,valueData],Total];
itemName[key_String]:= If[MemberQ[valueConfig[All,"id"],key],Base`excleLookUp[valueConfig,"id"->key,"name"],key];
itemName[igp_Association]:= KeyMap[itemName,igp];
itemId[key_String]:= If[MemberQ[valueConfig[All,"name"],key],Base`excleLookUp[valueConfig,"name"->key,"id"],key];
itemNameText[key_String]:= itemData[itemId[itemName[key]],"\:9053\:5177\:540d"];
itemPrice[key_String,currency_String]:= Ceiling[Base`excleLookUp[valueConfig,"name"->itemName[key],"value"]/Base`excleLookUp[valueConfig,"name"->currency,"value"]];
igpInfo[id_String]:= If[MemberQ[Keys@igpData,id],Module[{data=igpData[id],info,weights},
						info=KeyDrop[Association[Table[(#[[1]]->{#[[2]],#[[3]]})&@data[[{ToString@n~~"\:9053\:5177",ToString@n~~"\:6570\:91cf",ToString@n~~"\:6743\:91cd"}]],{n,1,data["\:6700\:5927\:6570\:91cf"]}]],""];
						weights = Total@Map[Last[#]&,info];
						If[data["\:5305\:7c7b\:578b"]=="\:4f9d\:6b21\:53d6",Map[First[#]&,info],Map[(Last[#]/weights)&,info]]],
						Message[ssrErr::notFound,id,"igpData"]];
igpInfo[id_String,num_Integer]:= igpInfo[id]*num;
igpInfo[id_List/;Depth@id==2]:= Merge[igpInfo/@id,Total];
igpInfo[data_List/;Depth@data==3]:=Merge[igpInfo@@@data,Total];
igpshow[id_String]:= If[MemberQ[Keys@igpData,id],StringReplace[ToString@Keys[igpInfo[id]],{","->";","{"->"","}"->""," "->""}],""];
igpForm[id_String]:= Function[{items,nums,weights},<|id->Merge[MapThread[AssociationThread,{Outer[StringJoin,ToString/@Range[1,Length@items],{"\:9053\:5177","\:6570\:91cf","\:6743\:91cd"}],Transpose@{items,nums,weights}}],Total]|>];
igpForm[id_String,items_List,nums_List,weights_List]:= igpForm[id][items,nums,weights];
igpForm[id_String,items_List,nums_List]:= igpForm[id,items,nums,Table[1,Length@items]];
igpForm[id_String,data_Association]:= igpForm[id,Keys@data,Values@data];
igpFill[data_Association]:= Association@KeyValueMap[#1->Prepend[#2,"StringId"->#1]&,data];
igpFill[data_Association,intId_Integer]:= Association@KeyValueMap[#1->Prepend[#2,"Id"->intId]&,igpFill[data]];
igpFill[data_Association,field_List]:= Fold[Map[Function[{element},Append[element,If[StringQ[Values@#2],#2,Keys@#2->(Values[#2][data])]]],#1]&,igpFill@data,field];
igpFill[data_Association,fillDataset_Dataset,fillId_String]:= igpFill[data,Normal@Normal@fillDataset[SelectFirst[#id==fillId&],2;;-1]];
igpItemNum[data_Association]:= Length[KeySelect[First@Values@data,StringContainsQ[#,"\:9053\:5177"]&]];


(*pay*)
ltvMark[day_]:= Base`excleInterpolation[ltvConfig,"day"->day,"ss"];
ltv[day_,first_,k_]:= first * (1 + (day - 1)*k);
ltv[day_] := If[day<0,ltv[0],Base`excleInterpolation[mainConfig,"day"->(day+1),"ltv"]];
ltvK[ltvF_,day_] := If[day<=2,(ltvF[day]/ltvF[day-1]-1),(ltvF[day]/ltvF[1]-1)/(day-1)];
ltvK[day_] := ltvK[ltv,day];


(*mainLv*)
dayTolv[day_]:= Base`excleInterpolation[mainConfig,"day"->day,"lv"];
lvToday[lv_]:= Base`excleInterpolation[mainConfig,"lv"->lv,"day"];
lvToMobLv[lv_]:= Base`excleInterpolation[mainConfig,"lv"->lv,"moblv"];
moblvToLv[moblv_]:= Base`excleInterpolation[Base`excleData[modelPath,"main","mob"],"moblv"->moblv,"lv"];
heroLv[exp_]:= With[{lv=LengthWhile[Accumulate[heroExp],#<=exp&]},{lv,exp - Accumulate[heroExp][[lv]]}];
lvToherolv[lv_]:= Base`excleInterpolation[mainConfig,"lv"->lv,"herolv"];


(*ssrTable*)
Options[ssrTable]={"CharacterEncoding"-> "UTF8","Key"->"StringId"};
ssrTable[fileName_String/;FileExistsQ[If[DirectoryName[fileName]=="",csvPath<>fileName,fileName]],OptionsPattern[]] :=Module[{result,data=Import[If[DirectoryName[fileName]=="",csvPath<>fileName,fileName],"CSV",CharacterEncoding->OptionValue["CharacterEncoding"]]},
  	result = Table[AssociationThread[data[[1]]->data[[n]]],{n,2,Length@data}];
  	AssociationThread[Part[#,OptionValue["Key"]]&/@result->result]];
ssrTable[fileName_String/;!FileExistsQ[If[DirectoryName[fileName]=="",csvPath<>fileName,fileName]],opts:OptionsPattern[]]:= Merge[ssrTable[#,opts]&/@(Keys/@ssrSplitTableRule[fileName]),First];
ssrTable[table_Association,OptionsPattern[]]:= GroupBy[Values[table],#[OptionValue["Key"]]&,First]; 
ssrTable[table_Association]:= table; 
  
ssrTableKey[table_]:= Module[{data= Dataset[ssrTable[table]]},Values@data[3;;,{"Id","StringId"}]];
ssrTableKey[table_,key_String]:= Module[{data=Dataset[ssrTable[table]],result}, result =Normal@Values[data[Select[#StringId==key&],"Id"]];If[Length@result>0,First@result,{"Invalid StringId:"<>key,Max[Keys@data[[4;;]]]}]];
ssrTableKey[table_,key_Integer]:= Module[{data=Dataset[ssrTable[table]],result}, result =Normal@Values[data[Select[#Id==key&],"StringId"]];If[Length@result>0,First@result,{"Invalid Id:"<>key,Max[Keys@data[[4;;]]]}]];
ssrTableKey[table_,startKey_Integer,step_]:= Module[{data=ssrTable[table,"Key"->"Id"]},Part[Keys@data,Range[#,If[Or[(#+step-1)>(Length@Keys[data]),step==Infinity],Length@Keys[data],#+step-1]]&@First[FirstPosition[Keys[data],startKey]]]];
ssrTableKey[table_,startKey_String,step_]:= Module[{data=ssrTable[table,"Key"->"StringId"]},Part[Keys@data,Range[#,If[Or[(#+step-1)>(Length@Keys[data]),step==Infinity],Length@Keys[data],#+step-1]]&@First[FirstPosition[Keys[data],startKey]]]];

ssrExportTable[fileName_String,data_Association]:= Module[{commaHanding,dataHanding},
  	commaHanding[da_Association]:=ReplaceAll[da,x_/;And[StringQ[x],StringMatchQ[x,___~~","~~___]]:> ToString[StringForm["\"``\"",x]]];
  	dataHanding = commaHanding[ssrTableForm[fileName,data]];
  	Base`addBOM[Export[If[DirectoryName[fileName]=="",csvPath<>fileName,fileName],Prepend[Values@Values@dataHanding,First@Keys@Values[dataHanding]],"CSV","TextDelimiters"->""]]];
ssrExportTable[fileName_String,data_Dataset]:= ssrExportTable[fileName,Normal@data];
  
Options[ssrSetTable]={"Key"->"StringId","output"-> "default","fileName"->"ssrTempData.csv"};
ssrSetTable[table_,data_Association,OptionsPattern[]]:= Module[{result=Merge[{ssrTable[table,"Key"->OptionValue["Key"]],data},Merge[#,Last]&]},Switch[OptionValue["output"],"default",result,"Dataset",Dataset@result,"export",If[StringQ[table],ssrExportTable[table,result],ssrExportTable[OptionValue["fileName"],result]]]];
  
Options[ssrSetColumn]={"Key"->"StringId","range"->3;;,"output"->"default","fileName"->"ssrTempData.csv"};
ssrSetColumn[table_,rule_Association,opts:OptionsPattern[]]:= Module[{data= Dataset[ssrTable[table,"Key"->OptionValue["Key"]]][OptionValue["range"]],result,temp,cFun},
  	cFun[row_,rul_]:= MapAt[Values[rul][row,#,First@FirstPosition[Keys@data,row[OptionValue["Key"]]]]&,row,Keys@rul];
  	result= If[ListQ[First@Values@rule]
  				,temp=Normal@Transpose@Values@data;KeyValueMap[(temp[#1]=#2)&,rule];Normal@Transpose[Dataset[temp]][GroupBy[OptionValue["Key"]],First]
  				,data[All,Fold[cFun,#,Normal@rule]&]//Normal];
  	Switch[OptionValue["output"],"default",result,"Dataset",Dataset@result,"export",ssrSetTable[table,result,"output"->"export","Key"->OptionValue["Key"],"fileName"->OptionValue["fileName"]],"data",data]];
  
ssrRefIntKey[table_,startKey_Integer]:= Module[{oriKeys=Normal@ssrTableKey[table][All,"Id"]},ssrSetColumn[table,<|"Id"->Range[startKey,startKey+Length@oriKeys-1]|>,"Key"->"StringId","output"->"export"]];
ssrRefIntKey[table_]:= ssrRefIntKey[table,First@Normal@ssrTableKey[table][All,"Id"]];

ssrSortById[table_]:= Module[{data=Dataset[ssrTable[table]],result}, result = Prepend[data[3;;][SortBy["Id"]],data[1;;2]]; If[StringQ@table,ssrExportTable[table,result],result]];
  

Options[ssrDelTable]={"Key"->"StringId","output"-> "default","fileName"->"ssrTempData.csv"};
ssrDelTable[table_,key_,OptionsPattern[]]:= Module[{result=KeyDrop[ssrTable[table,"Key"->OptionValue["Key"]],key]},Switch[OptionValue["output"],"default",result,"Dataset",Dataset[result],"export",ssrExportTable[If[StringQ[table],table,OptionValue["fileName"]],result]]];
 
ssrTableForm[table_]:= First@Values[ssrTable[table]];
ssrTableForm[table_,data_Association]:= Module[{tab=ssrTable[table],keys},keys=Complement[Keys@data,ssrTableKey[tab][All,If[StringQ[First@Keys@data],"StringId","Id"]]];MapAt[Merge[{""&/@ssrTableForm[tab],#},Last]&,data,List[Key[#]]&/@keys]];

ssrSplitTableRule[key_String]:=Module[{data=Base`excleData[tablePath,"split","split"]},If[MemberQ[data[All,"\:4e3b\:8868"],key],Normal@data[Select[#\:4e3b\:8868==key&],{"value"->Base`ruleParser}][All,"value"],{key<>".csv"->Missing}]];

ssrReplaceCell[tableName_String,replace_]:=ssrExportTable[#,ssrTable[#]/.replace]&/@Keys[ssrSplitTableRule[tableName]];


ssrDeleteColumn[fileName_String,columns_]:= ssrExportTable[fileName,KeyDrop[#,columns]&/@ssrTable[fileName]];
ssrDataFromExcle[fileName_String,sheetName_String]:= Module[{data = Base`getXls[fileName,sheetName,"StartLine"->1]},AssociationThread[Normal@data[All,"ID"]->Normal@data]];
ssrSplitTable[data_Association,rule_Rule]:= ssrExportTable[Keys@rule,KeyTake[data,Join[{"int","Id"},(#[["Id"]]&/@Values[data])\[Intersection]Range@@Values[rule]]]];
ssrSplitTable[data_Association,rule_List]:= ssrSplitTable[data,#]&/@rule;


(*attr*)
attrData = ssrTable["AttrGroup"];
attrFomula[type_String]:= Module[{data=Base`excleData["D:\\Work\\SSR\\basePlan\\ssr-battle.xlsx","attr",type]},AssociationThread[Normal@data[All,"name"]->ToExpression/@Normal[data[All,"fomula"]]]];
ssrAttrByGroup[groupId_String]:= With[{data=attrData[groupId]},KeyDrop[Thread[Values@KeySelect[data,StringContainsQ["\:7c7b\:578b"]]->Values@KeySelect[data,StringContainsQ["\:6570\:503c"]]],""]];
ssrCalculateAttr[attrlist_List,fomula_Association]:= Module[{data=Merge[attrlist,Total],cal},cal[f_]:= f/.x_String:>If[KeyMemberQ[data,x],data[x],0];cal/@fomula];
ssrPower[config_Dataset,data_Association,powerType_String]:= IntegerPart@Total[Normal[config[GroupBy["attrKey"],First][Keys@data,powerType]]*data];
ssrPower[config_Dataset,data_Association]:= ssrPower[config,data,"point"];


(*hero*)
ssrHeroAttr[id_String,lv_Integer,star_Integer]:= Module[{lvData=ssrAttrByGroup["attr_"<>id<>"_lv"<>ToString@lv],starData= ssrAttrByGroup["attr_heroStre_"<>ToString@star]},<|"lvData"->lvData,"starData"->starData,"se"->ssrCalculateAttr[{lvData,starData},attrFomula["se"]],"slg"->ssrCalculateAttr[{lvData,starData},attrFomula["slg"]]|>];
ssrNpcAttr[id_String]:= Module[{attr=ssrAttrByGroup[ssrTable["SeNpc"][id]["\:5c5e\:6027\:7ec4ID"]]},ssrCalculateAttr[{attr},attrFomula["se"]]];


(*other*)
dayToDuration[day_]:=Module[{d=IntegerPart[day],h=IntegerPart[FractionalPart@day*24],m=FractionalPart[FractionalPart@day*24]*60,time},time=StringReplace[TextString[TimeObject[{h,IntegerPart@m,FractionalPart@m*60}],TimeFormat->{"Hour","h","Minute","m","Second","s"}],{"00h"->"","00m"->"","00s"->""}];If[d>=1,ToString[d]<>"d"<>ToString[h]<>"h",time]];
ssrAttrGroupForm[data_Association,config_Dataset,index_String]:= Association@Flatten[{"\:5c5e\:6027"<>ToString[IntegerPart@Base`excleLookUp[config,"attrKey"->Keys@#,index]]<>"\:7c7b\:578bId"->Base`excleLookUp[config,"attrKey"->Keys@#,"attrName"],"\:5c5e\:6027"<>ToString[IntegerPart@Base`excleLookUp[config,"attrKey"->Keys@#,index]]<>"\:6570\:503c"->Values@#}&/@Normal[data]];
ssrAttrGroupForm[data_Association,config_Dataset]:= Association@Flatten[{"\:5c5e\:6027"<>ToString[#]<>"\:7c7b\:578bId"->Base`excleLookUp[config,"attrKey"->Keys[data][[#]],"attrName"],"\:5c5e\:6027"<>ToString[#]<>"\:6570\:503c"->data[[#]]}&/@Range[Length@data]];
ssrAnalyzeTable[file_String]:=Module[{data=ImportString[First@#,"RawJSON"]&/@Import[file,"CSV"][[2;;]],func},func[rowJson_Association]:= Merge[{KeyTake[rowJson,{"app_id","event","event_ts","fpid"}],rowJson["detail"],KeyTake[rowJson["properties"],{"game_uid","game_uid_create_ts","gameserver_id","level","vip_level","aid"}]},First];
								Dataset[func/@data]];
ssrExtendAnalyze[file_String]:=Module[{data=SemanticImport[file]},data[All,Append[#,{"lv"->ImportString[StringReplace[#"EXTRA_PROPERTIES","\"\""->"\""],"RawJSON"]["level"],"detail"->ImportString[StringReplace[#"EXTRA_DETAIL","\"\""->"\""],"RawJSON"]}]&]];


End[];
EndPackage[];
