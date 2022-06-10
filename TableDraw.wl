(* ::Package:: *)

(* ::Section:: *)
(*table graphics*)


(* ::Subsection::Closed:: *)
(*global variables*)


(* ::Subsubsection::Closed:: *)
(*table dimensions*)


(* overall dimensions *)
$tableOrientation= Horizontal;
$tableSize= Quantity[100., "Inches"];
$inchesPerDiamond= $tableSize/8;


(* slate cut dimensions *)
$cornerShelfDiameter= Quantity[8.5, "Inches"];
$cornerShelfBackset= Quantity[3.5, "Inches"];
$sideShelfDiameter= Quantity[6., "Inches"];
$sideShelfBackset= Quantity[3, "Inches"];


(* pocket jaw dimensions*)
$cornerJawWidth= Quantity[5.0, "Inches"];
$sideJawWidth= Quantity[5.5, "Inches"];
$cornerJawAngle= Quantity[145., "AngularDegrees"];
$sideJawAngle= Quantity[105., "AngularDegrees"];


(* rail, cushion, and ball dimensions*)
$railWidth= Quantity[5., "Inches"];
$railRoundingRadius= Quantity[1., "Inches"];
$cushionWidth= Quantity[2.125, "Inches"];


(* ::Subsubsection::Closed:: *)
(*iron dimensions*)


(* generic iron measurements *)
$ironRailLip= Quantity[1.0, "Inches"];
$ironRailWidth:= $railWidth + $cushionWidth + $ironRailLip;
$ironCirclePointCount=100;
$ironDiameter= Quantity[4.2, "Inches"];


(* corner iron measurements *)
$cornerIronHoleBackset= Quantity[2, "Inches"];
$cornerIronPocketLip= Quantity[1, "Inches"];
$cornerIronRoundingRadius= Quantity[4, "Inches"];
$cornerIronRoundingCirclePointCount= $ironCirclePointCount;


(* side iron measurements *)
$sideIronHoleBackset= Quantity[2, "Inches"];
$sideIronPocketLip= Quantity[6, "Inches"];
$sideIronRoundingRadius= Quantity[1, "Inches"];
$sideIronRoundingCirclePointCount= $ironCirclePointCount;


(* ::Subsubsection::Closed:: *)
(*table colors*)


$bedColor= RGBColor[0.141176, 0.701961, 0.47451];
$cushionColor= Darker[$bedColor, 0.15];
$holeColor= Black;
$railColor= RGBColor[0.768627, 0.513725, 0.133333];
$ironColor=GrayLevel[0.745098];


(* ::Subsubsection::Closed:: *)
(*table decorations*)


SetDirectory[NotebookDirectory[]];
$headSpotGraphic= None;
$diamondGraphic= Import["ABALONE-OUT-MOP-IN-DIAMOND.jpg"];
$diamondStyle= Sequence[PointSize[Medium], RGBColor[0.67451, 0.337255, 0]];
$graphicsEdgeForm= EdgeForm[{Thin, Black}];


(* ::Subsection::Closed:: *)
(*define main Graphics items*)


(* ::Subsubsection::Closed:: *)
(*rail, bed, and shelf-cut graphics*)


railGraphic= Graphics[{$graphicsEdgeForm,$railColor, 
	Rectangle[
		{-($railWidth+$cushionWidth)/$inchesPerDiamond, -($railWidth+$cushionWidth)/$inchesPerDiamond},{8+($railWidth+$cushionWidth)/$inchesPerDiamond, 4+($railWidth+$cushionWidth)/$inchesPerDiamond},
		RoundingRadius -> $railRoundingRadius/$inchesPerDiamond]
	}];


bedGraphic= Graphics[{$bedColor, Rectangle[{-$cushionWidth,-$cushionWidth}/$inchesPerDiamond,{8+$cushionWidth/$inchesPerDiamond,4+$cushionWidth/$inchesPerDiamond}]}];


holeGraphic= Graphics[{$holeColor, 
	Disk[{4,0} + $sideShelfBackset/$inchesPerDiamond{0, -1}, $sideShelfDiameter/(2$inchesPerDiamond)],       (* pocket A *)
	Disk[{0,0} + $cornerShelfBackset/$inchesPerDiamond{-1, -1}, $cornerShelfDiameter/(2$inchesPerDiamond)],  (* pocket B *)
	Disk[{0,4} + $cornerShelfBackset/$inchesPerDiamond{-1, 1}, $cornerShelfDiameter/(2$inchesPerDiamond)],   (* pocket C *)
	Disk[{4,4} + $sideShelfBackset/$inchesPerDiamond{0, 1}, $sideShelfDiameter/(2$inchesPerDiamond)],        (* pocket D *)
	Disk[{8,4} + $cornerShelfBackset/$inchesPerDiamond{1, 1}, $cornerShelfDiameter/(2$inchesPerDiamond)],    (* pocket E *)
	Disk[{8,0} + $cornerShelfBackset/$inchesPerDiamond{1, -1}, $cornerShelfDiameter/(2$inchesPerDiamond)]    (* pocket F *)
}];


holeGraphic= Graphics[{$holeColor, 
	Disk[{4,0} + $sideShelfBackset/$inchesPerDiamond{0, -1}, $sideShelfDiameter/(2$inchesPerDiamond)],       (* pocket A *)
	Disk[{0,0} + $cornerShelfBackset/(Sqrt[2]$inchesPerDiamond){-1, -1}, $cornerShelfDiameter/(2$inchesPerDiamond)],  (* pocket B *)
	Disk[{0,4} + $cornerShelfBackset/(Sqrt[2]$inchesPerDiamond){-1, 1}, $cornerShelfDiameter/(2$inchesPerDiamond)],   (* pocket C *)
	Disk[{4,4} + $sideShelfBackset/$inchesPerDiamond{0, 1}, $sideShelfDiameter/(2$inchesPerDiamond)],        (* pocket D *)
	Disk[{8,4} + $cornerShelfBackset/(Sqrt[2]$inchesPerDiamond){1, 1}, $cornerShelfDiameter/(2$inchesPerDiamond)],    (* pocket E *)
	Disk[{8,0} + $cornerShelfBackset/(Sqrt[2]$inchesPerDiamond){1, -1}, $cornerShelfDiameter/(2$inchesPerDiamond)]    (* pocket F *)
}];


(* ::Subsubsection::Closed:: *)
(*cushion graphics*)


cushionPoly1= Polygon[{
	{($cornerJawWidth/Sqrt[2]-Tan[$cornerJawAngle-Quantity[90.,"AngularDegrees"]]$cushionWidth)/$inchesPerDiamond, -$cushionWidth/$inchesPerDiamond},
	{$cornerJawWidth/(Sqrt[2]$inchesPerDiamond), 0},
	{4-$sideJawWidth/(2$inchesPerDiamond), 0},
	{4-($sideJawWidth/2-Tan[$sideJawAngle-Quantity[90.,"AngularDegrees"]]$cushionWidth)/$inchesPerDiamond, -$cushionWidth/$inchesPerDiamond}
}];


cushionPoly2= Polygon[{
	{-$cushionWidth/$inchesPerDiamond, $cornerJawWidth/(Sqrt[2]$inchesPerDiamond)-(Tan[$cornerJawAngle-Quantity[90.,"AngularDegrees"]])$cushionWidth/$inchesPerDiamond},
	{-$cushionWidth/$inchesPerDiamond, 4-$cornerJawWidth/(Sqrt[2]$inchesPerDiamond)+(Tan[$cornerJawAngle-Quantity[90.,"AngularDegrees"]])$cushionWidth/$inchesPerDiamond},
	{0, 4-$cornerJawWidth/(Sqrt[2]$inchesPerDiamond)},
	{0, $cornerJawWidth/(Sqrt[2]$inchesPerDiamond)}
}];


cushionPoly3= Polygon[{
	{$cornerJawWidth/(Sqrt[2]$inchesPerDiamond), 4},
	{($cornerJawWidth/Sqrt[2]-Tan[$cornerJawAngle-Quantity[90.,"AngularDegrees"]]$cushionWidth)/$inchesPerDiamond, 4+$cushionWidth/$inchesPerDiamond},
	{4-($sideJawWidth/2-Tan[$sideJawAngle-Quantity[90.,"AngularDegrees"]]$cushionWidth)/$inchesPerDiamond, 4+$cushionWidth/$inchesPerDiamond},
	{4-$sideJawWidth/(2$inchesPerDiamond), 4}
}];


cushionPoly4= Polygon[{
	{4+$sideJawWidth/(2$inchesPerDiamond), 4},
	{4+($sideJawWidth/2-Tan[$sideJawAngle-Quantity[90.,"AngularDegrees"]]$cushionWidth)/$inchesPerDiamond, 4+$cushionWidth/$inchesPerDiamond},
	{8-($cornerJawWidth/Sqrt[2]-Tan[$cornerJawAngle-Quantity[90.,"AngularDegrees"]]$cushionWidth)/$inchesPerDiamond, 4+$cushionWidth/$inchesPerDiamond},
	{8-$cornerJawWidth/(Sqrt[2]$inchesPerDiamond), 4}
}];


cushionPoly5= Polygon[{
	{8, $cornerJawWidth/(Sqrt[2]$inchesPerDiamond)},
	{8, 4-$cornerJawWidth/(Sqrt[2]$inchesPerDiamond)},
	{8+$cushionWidth/$inchesPerDiamond, 4-$cornerJawWidth/(Sqrt[2]$inchesPerDiamond)+(Tan[$cornerJawAngle-Quantity[90.,"AngularDegrees"]])$cushionWidth/$inchesPerDiamond},
	{8+$cushionWidth/$inchesPerDiamond, $cornerJawWidth/(Sqrt[2]$inchesPerDiamond)-(Tan[$cornerJawAngle-Quantity[90.,"AngularDegrees"]])$cushionWidth/$inchesPerDiamond}
}];


cushionPoly6= Polygon[{
	{4+($sideJawWidth/2+Tan[Quantity[90.,"AngularDegrees"]-$sideJawAngle]$cushionWidth)/$inchesPerDiamond, -$cushionWidth/$inchesPerDiamond},
	{4+$sideJawWidth/(2$inchesPerDiamond), 0},
	{8-$cornerJawWidth/(Sqrt[2]$inchesPerDiamond), 0},
	{8-($cornerJawWidth/Sqrt[2]-Tan[$cornerJawAngle-Quantity[90.,"AngularDegrees"]]$cushionWidth)/$inchesPerDiamond, -$cushionWidth/$inchesPerDiamond}
}];


cushionGraphic= Graphics[{$graphicsEdgeForm, $cushionColor, 
	cushionPoly1, cushionPoly2, cushionPoly3, cushionPoly4, cushionPoly5, cushionPoly6
}];


(* ::Subsubsection::Closed:: *)
(*diamond graphics*)


$diamondOffset= ($cushionWidth + $railWidth/2)/$inchesPerDiamond;
diamondGraphics=Graphics[{$diamondStyle,
	(* left rail *)
	Point[{1, -$diamondOffset}], Point[{2, -$diamondOffset}],
	Point[{3, -$diamondOffset}], Point[{5, -$diamondOffset}],
	Point[{6, -$diamondOffset}], Point[{7, -$diamondOffset}],
	(* right rail *)
	Point[{1, 4+$diamondOffset}], Point[{2, 4+$diamondOffset}],
	Point[{3, 4+$diamondOffset}], Point[{5, 4+$diamondOffset}],
	Point[{6, 4+$diamondOffset}], Point[{7, 4+$diamondOffset}],
	(* head rail *)
	Point[{-$diamondOffset, 1}], Point[{-$diamondOffset, 2}],
	Point[{-$diamondOffset, 3}],
	(* foot rail *)
	Point[{8+$diamondOffset, 1}], Point[{8+$diamondOffset, 2}],
	Point[{8+$diamondOffset, 3}]
	}];


(* ::Subsubsection::Closed:: *)
(*corner iron graphics*)


cpFull= CirclePoints[$cornerIronHoleBackset/$inchesPerDiamond{-1,-1}, {$ironDiameter /(2 $inchesPerDiamond), Pi/2}, $ironCirclePointCount];
cp= Select[cpFull, #[[1]]<-$cushionWidth/$inchesPerDiamond || #[[2]]<-$cushionWidth/$inchesPerDiamond&];
cp= Select[cp, #[[1]] < 0 && #[[2]] < 0 &];


cp2Full= CirclePoints[($ironRailWidth-$cornerIronRoundingRadius)/$inchesPerDiamond{-1,-1}, {$cornerIronRoundingRadius/$inchesPerDiamond, Pi}, $cornerIronRoundingCirclePointCount];
cp2= Reverse[cp2Full[[1;; Quotient[$cornerIronRoundingCirclePointCount,4]]]];


cornerIronFilledCurve=FilledCurve[{Line[{
		$cushionWidth/($inchesPerDiamond){1, 1}+ $ironDiameter/(2 Sqrt[2] $inchesPerDiamond){-1,1}+$cornerIronPocketLip/$inchesPerDiamond{0,1}+{-$ironRailWidth/$inchesPerDiamond,0}, (* x corner *)
		$cushionWidth/$inchesPerDiamond{-1,1}+$ironDiameter/(2 Sqrt[2] $inchesPerDiamond){0,1}+$cornerIronPocketLip/$inchesPerDiamond{0,1},
		{-$cushionWidth/$inchesPerDiamond, $cornerJawWidth/(Sqrt[2]$inchesPerDiamond)-(Tan[$cornerJawAngle-Quantity[90.,"AngularDegrees"]])$cushionWidth/$inchesPerDiamond},
		Sequence@@cp,
		{($cornerJawWidth/Sqrt[2]-Tan[$cornerJawAngle-Quantity[90.,"AngularDegrees"]]$cushionWidth)/$inchesPerDiamond, -$cushionWidth/$inchesPerDiamond},
		$cushionWidth/($inchesPerDiamond){1, -1}+$ironDiameter/(2 Sqrt[2] $inchesPerDiamond){1,0}+$cornerIronPocketLip/$inchesPerDiamond{1,0},
		$cushionWidth/($inchesPerDiamond){1, 0}+$ironDiameter/(2 Sqrt[2] $inchesPerDiamond){1,0}+$cornerIronPocketLip/$inchesPerDiamond{1,0}+{0,-$ironRailWidth/$inchesPerDiamond}, (* y corner *)
		Sequence@@cp2,                                  (* x-y corner *)                                                                                                 (* x-y corner *)
		$cushionWidth/($inchesPerDiamond){0, 1}+ $ironDiameter/(2 Sqrt[2] $inchesPerDiamond){0,1}+$cornerIronPocketLip/$inchesPerDiamond{0,1}+{-$ironRailWidth/$inchesPerDiamond, 0} (* x corner *)
	}]}];


ironBGraphic=Graphics[{$graphicsEdgeForm, $ironColor, 
  cornerIronFilledCurve}]


ironEGraphic= Graphics[{$graphicsEdgeForm, $ironColor, 
	Rotate[cornerIronFilledCurve, 180 Degree, {4,2}]
}];


ironCGraphic= Graphics[{$graphicsEdgeForm, $ironColor, 
	GeometricTransformation[cornerIronFilledCurve, ReflectionTransform[{0,1}, {0,2}]]
}];


ironFGraphic= Graphics[{$graphicsEdgeForm, $ironColor, 
	GeometricTransformation[cornerIronFilledCurve, ReflectionTransform[{1,0}, {4,0}]]
}];


cornerIronGraphics= {ironBGraphic, ironCGraphic, ironEGraphic, ironFGraphic};


(* ::Subsubsection::Closed:: *)
(*side iron graphics*)


cpFull= CirclePoints[{4,-$sideIronHoleBackset/$inchesPerDiamond}, {$ironDiameter /(2 $inchesPerDiamond), Pi/2}, $ironCirclePointCount];
cp= Select[cpFull, #[[2]]<-$cushionWidth/$inchesPerDiamond&];


cp2Full= CirclePoints[
	{4 - ($sideIronPocketLip-$sideIronRoundingRadius)/($inchesPerDiamond), -($ironRailWidth-$sideIronRoundingRadius)/$inchesPerDiamond}, 
	{$sideIronRoundingRadius/$inchesPerDiamond, Pi}, $sideIronRoundingCirclePointCount
];
cp2=Reverse[cp2Full[[1;; Quotient[$sideIronRoundingCirclePointCount,4]]]];


cp3Full= CirclePoints[
	{4 + ($sideIronPocketLip-$sideIronRoundingRadius)/($inchesPerDiamond), -($ironRailWidth-$sideIronRoundingRadius)/$inchesPerDiamond}, 
	{$sideIronRoundingRadius/$inchesPerDiamond, 3Pi/2}, $sideIronRoundingCirclePointCount
];
cp3=Reverse[cp3Full[[1;; Quotient[$sideIronRoundingCirclePointCount,4]]]];


sideIronFilledCurve=FilledCurve[{Line[{
		Sequence@@cp2,
		{4 - $sideIronPocketLip/($inchesPerDiamond), -($ironRailWidth-$sideIronRoundingRadius)/$inchesPerDiamond},
		{4 - $sideIronPocketLip/($inchesPerDiamond), -$cushionWidth/$inchesPerDiamond},
		Sequence@@cp,
		{4 + $sideIronPocketLip/($inchesPerDiamond), -$cushionWidth/$inchesPerDiamond},
		{4 + $sideIronPocketLip/($inchesPerDiamond), -($ironRailWidth-$sideIronRoundingRadius)/$inchesPerDiamond},
		Sequence@@cp3
	}]}];


ironAGraphic=Graphics[{$graphicsEdgeForm, $ironColor, 
  sideIronFilledCurve}]


ironDGraphic=Graphics[{$graphicsEdgeForm, $ironColor, 
  GeometricTransformation[sideIronFilledCurve, ReflectionTransform[{0,1}, {4,2}]]
}];


sideIronGraphics={ironAGraphic, ironDGraphic};


(* ::Subsection::Closed:: *)
(*show table*)


tableGraphics= {railGraphic, bedGraphic, holeGraphic, cushionGraphic, cornerIronGraphics, sideIronGraphics, diamondGraphics};
Show[tableGraphics]


(* ::Section:: *)
(*ball graphics*)


(* ::Subsubsection:: *)
(*ball colors and dimensions*)


$ballDiameter= Quantity[2.25, "Inches"]
$ballColorList= {White, Yellow, Blue, Red, Purple, Orange, Green, Brown, Black};


(* ::Subsubsection:: *)
(*ball graphics*)


ballGraphics= Graphics[{#, Disk[{0, 0}, $ballDiameter/(2$inchesPerDiamond)]}, ImageSize->20]& /@ $ballColorList


ballGraphics= Graphics3D[{#, Sphere[{0, 0, 0}, $ballDiameter/(2$inchesPerDiamond)]}, ImageSize->20, Boxed->False]& /@ $ballColorList


(* ::Section:: *)
(*moving balls with Locators*)


?LocatorPane


LocatorPane[{4,2}, Show[tableGraphics], Appearance -> Rasterize[ballGraphics[[1]], Background->None]]
