(* ::Package:: *)

(* ::Section:: *)
(*settings*)


(* ::Subsection::Closed:: *)
(*output filenames*)


rawOutputFilename= FileNameJoin[{NotebookDirectory[], "Weingartner-raw-scrape.txt"}];


(* ::Subsection::Closed:: *)
(*input page URLs*)


mainPageURL= "https://wbpb.blogspot.com/";
rulesForMatchPlayURL= "http://wbpb.blogspot.com/2006/06/rules-for-match-play.html";
backgroundInfoURL= "http://wbpb.blogspot.com/2006/06/background-info.html";
shotURLTemplate= StringTemplate["http://wbpb.blogspot.com/2006/05/wbp-shot-`1`.html"];


(* ::Section::Closed:: *)
(*scrape top-level text*)


(* ::Subsection::Closed:: *)
(*scrape intro text*)


mainPageSource= URLFetch[mainPageURL]


programInstructionText= StringSplit[mainPageSource, {
	"src=\"//photos1.blogger.com/blogger/813/2300/400/wbpb-1.gif\" border=\"0\"><br /><br /><br />",
	"<br /><br /><br /><a href=\"http://easyhitcounters.com/stats.php?site=wbpblog\""
}][[2]]


(* ::Subsection::Closed:: *)
(*scrape rules for match play*)


rulesForMatchPlaySource= URLFetch[rulesForMatchPlayURL]


rulesForMatchPlayText= StringSplit[rulesForMatchPlaySource, {
	"src=\"http://photos1.blogger.com/blogger/813/2300/400/wbpb-1.gif\" border=\"0\" alt=\"\" /></a><br /><br />",
	"<div style=\"clear:both; padding-bottom:0.25em\"></div> </div> </div>"
}][[2]]


(* ::Subsection::Closed:: *)
(*scrape background info*)


backgroundInfoSource= URLFetch[backgroundInfoURL]


backgroundInfoText= StringSplit[backgroundInfoSource, {
	"src=\"http://photos1.blogger.com/blogger/813/2300/320/MRHeinrichWeingartner.gif\" border=\"0\" alt=\"\" /></a><br /><br />",
	"<div style=\"clear:both; padding-bottom:0.25em\"></div> </div> </div>"
}][[2]]


(* ::Section::Closed:: *)
(*scrape shots*)


(* ::Subsection::Closed:: *)
(*generic shot scrape code*)


ClearAll[shotSource]
shotSource[n_]:= shotSource[n]= URLFetch[shotURLTemplate[n]]


ClearAll[scrapeShotPage]
$shotBeginPattern= "</embed></object><br /><br />";
$shotEndPattern= "<div style=\"clear:both; padding-bottom:0.25em\"></div> </div> </div>";
scrapeShotPage[n_]:= scrapeShotPage[n]= StringSplit[shotSource[n], {$shotBeginPattern, $shotEndPattern}][[2]]


(* ::Subsection::Closed:: *)
(*get all page source*)


shotNumStrings= Join[StringJoin["0", #]& /@ ToString/@ Range[1, 9], ToString/@Range[10,76]]
shotSource/@%;


(* ::Subsection::Closed:: *)
(*validate source*)


(* find individual shot pages that don't match expected string pattern *)
Select[shotNumStrings, Length[StringSplit[shotSource[#], {$shotBeginPattern, $shotEndPattern}]]<3&]


(* replace bad shot source text with corrected URL *)
shotSource["03"]= URLFetch["http://wbpb.blogspot.com/2006/05/wbp-shot-03_114930983596269199.html"];


(* ::Subsection::Closed:: *)
(*scrape all source*)


scrapeShotPage/@shotNumStrings;


(* ::Section::Closed:: *)
(*write raw results*)


delimiterStringTemplate = StringTemplate["(*******************`1`*****************************************************************)\n"];


fileOut= OpenWrite[rawOutputFilename];
WriteString[fileOut, delimiterStringTemplate[" Program Instructions "], programInstructionText, "\n"]
WriteString[fileOut, delimiterStringTemplate[" Rules for Match Play "], rulesForMatchPlayText, "\n"]
WriteString[fileOut, delimiterStringTemplate[" Background Info "], backgroundInfoText, "\n\n\n"]
WriteString[fileOut, delimiterStringTemplate[" shot " <> #], scrapeShotPage[#], "\n"]&/@ shotNumStrings;
Close[fileOut]


(* ::Section::Closed:: *)
(*read raw results from file*)


ReadLine[rawOutputFilename];
programInstructionText=ReadLine[rawOutputFilename];
ReadLine[rawOutputFilename];
rulesForMatchPlayText=ReadLine[rawOutputFilename];
ReadLine[rawOutputFilename];
backgroundInfoText=ReadLine[rawOutputFilename];
Do[
	ReadLine[rawOutputFilename];
	scrapeShotPage[n]=ReadLine[rawOutputFilename]
	,
	{n, 76}
]
Close[rawOutputFilename];


(* ::Section:: *)
(*shot reconstruction*)


(* ::Subsection::Closed:: *)
(*parseShotText*)


ClearAll[parseShotText]
parseShotText::usage= "Use parseShotText[\"01\"] or parseShotText[1] depending on which definition above was evaluated.";  
parseShotText[shotNum_]:= parseShotText[shotNum]= Module[
	{
		strList, categoryPos, setupPos, targetPos, notesPos, posAssoc, targetMissingQ
	},
	strList= StringTrim[StringSplit[scrapeShotPage[shotNum], {"<br />", "<br />", "<b>", "</b>"}]//.""->Nothing[]];
	categoryPos= ResourceFunction["PositionedCases"][strList, "CATAGORY"] /. {} -> ({Missing["NotFound"]} -> "CATAGORY");
	setupPos= ResourceFunction["PositionedCases"][strList, "INITIAL POSITION"] /. {} -> ({Missing["NotFound"]} -> "INITIAL POSITION");
	targetPos= ResourceFunction["PositionedCases"][strList, _String?(StringMatchQ[#, "Target Gather Zone: "~~__]&)] /. {{} -> ({Missing["NotFound"]} -> "TARGET ZONE"), _String -> "TARGET ZONE"};
	notesPos= ResourceFunction["PositionedCases"][strList, "NOTES"] /. {} -> ({Missing["NotFound"]} -> "NOTES");
	posAssoc= Normal[KeyMap[First, Merge[{categoryPos, setupPos, targetPos, notesPos}, First]]];
	posAssoc= Association@@Reverse /@ posAssoc;
	targetMissingQ= !TrueQ[Length[DeleteMissing[posAssoc]]===4]; (* the following shots are missing target zones: {4,5,6,7,10,11,12}*)
	If[
		targetMissingQ,
		<|
			"Category" -> Take[strList, {posAssoc[["CATAGORY"]]+1, posAssoc[["INITIAL POSITION"]]-1}],
			"InitialPosition" -> Take[strList, {posAssoc[["INITIAL POSITION"]]+1, posAssoc[["NOTES"]]-1}],
			"Notes" -> Take[strList, {posAssoc[["NOTES"]]+1, Length[strList]}]
		|>,
		<|
			"Category" -> Take[strList, {posAssoc[["CATAGORY"]]+1, posAssoc[["INITIAL POSITION"]]-1}],
			"InitialPosition" -> Take[strList, {posAssoc[["INITIAL POSITION"]]+1, posAssoc[["TARGET ZONE"]]-1}],
			"TargetZone" -> Take[strList, {posAssoc[["TARGET ZONE"]], posAssoc[["NOTES"]]-1}],
			"Notes" -> Take[strList, {posAssoc[["NOTES"]]+1, Length[strList]}]
		|>
	]
]


parseShotText[1]


parseShotText[4]


(* ::Subsection::Closed:: *)
(*parse statistics (categories, initPositions, targetZones, notes)*)


categories= AssociationMap["Category" /. parseShotText[#]&, Range[76]];
initPositions= AssociationMap["InitialPosition" /. parseShotText[#]&, Range[76]];
targetZones= AssociationMap["TargetZone" /. parseShotText[#] /. "TargetZone"->{}&, Range[76]];
notes= AssociationMap["Notes" /. parseShotText[#]&, Range[76]];


Tally[Length /@ categories]


Tally[Length /@ initPositions]
Select[Range[76], Length[parseShotText[#][["InitialPosition"]]]==4&]
initPositions[60]


Tally[Length /@ targetZones]
Select[Range[76], Length[parseShotText[#][["TargetZone"]]]==0&]
Select[Range[76], Length[parseShotText[#][["TargetZone"]]]==3&]
targetZones[73]


Tally[Length /@ notes]


(* ::Subsection::Closed:: *)
(*parseInitialPositions*)


ClearAll[parseInitialConditions]
parseInitialConditions::usage= "Use parseInitialConditions[\"01\"] or parseInitialConditions[1] depending on which definition above was evaluated.";  
parseInitialConditions[shotNum_]:= parseInitialConditions[shotNum]= Module[
	{
		res, posAssoc
	},
	res= parseShotText[shotNum]["InitialPosition"]
]


parseInitialConditions[1]


(* ::Subsection::Closed:: *)
(*define ball position grammar (ballPositionGrammar)*)


ballPositionGrammar = CloudDeploy[
  GrammarRules[
   {
     FixedOrder[
         c : GrammarToken["BallColor"], "Ball:",
         d1 : GrammarToken["SemanticNumber"], bd1: "ball"|"diamond", "from", "the", r1: "bottom"|"top"|"left"|"right", "rail,",
         d2 : GrammarToken["SemanticNumber"], bd2: "ball"|"diamond", "from", "the", r2: "bottom"|"top"|"left"|"right", "rail"]
         :>
         {c, {d1, bd1, r1}, {d2, bd2, r2}}
   },
   {
      "BallColor" -> "White" :> 0,
      "BallColor" -> "Yellow" :> 2,
      "BallColor" -> "Red" :> 3 
   }
 ]
]


(* ::Subsection::Closed:: *)
(*test ballPositionGrammar*)


stringIn1=initPositions[[1,1]]
stringIn2=initPositions[[1,2]]
stringIn3=initPositions[[1,3]]


GrammarApply[ballPositionGrammar, stringIn1]
GrammarApply[ballPositionGrammar, stringIn2]
GrammarApply[ballPositionGrammar, stringIn3]


(* ::Subsection::Closed:: *)
(*ballPositionGrammar statistics*)


allInitPositionLines= initPositions//Values//Flatten//DeleteDuplicates;
Length[%]


GrammarApply[ballPositionGrammar, #]&/@allInitPositionLines[[1;;5]]


(* ::Section:: *)
(*scratch*)
