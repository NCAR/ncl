*ndvPlotStyleName : Test Style
!*ndvPlotClass : contourPlotClass
*ndvPlotName : testplot

!*ndvFuncDirs : (/ ./plot_styles /)
*ndvFuncFiles : (/ test_style.ncl, basic_funcs.ncl /)
!*ndvFunctions : (/ spread_colors , title_func /)
!*ndvExecFunction : test_style

*ndvData : (/ psl : 2 , tmp : 1 /)
*ndvObjects : (/ \
	cnplot : contourPlotClass , \
	map : mapPlotClass , \
	anno1 : textItemClass ,\
	sf : scalarFieldClass , \
	maptick : logLinPlotClass \
/)

*psl@Pattern : (/  /.*psl.*/i , /.*/ /)
*psl!-1@Pattern : /.*lon.*/i
*psl!-2@Pattern : /.*lat.*/i

*sf@sfDataArray : $psl$
*sf@sfDataArray%Profile : (/ Name : Sea level pressure /)
*sf@sfXArray : $psl$!-1
*sf@sfXArray%Profile : (/ Name : Longitude /)
*sf@sfYArray : $psl$!-2

*map@pmOverlays : (/ $cnplot$, $maptick$ /)
*map@mpDataLimitObject : $cnplot$
!*map@pmAnnoViews : (/ $maptick$ /)
*maptick@ndvUpdateFunc : map_tickmarks($map$,$maptick$)
*maptick*tfDoNDCOverlay : True
*maptick*pmTickMarkDisplayMode : always
*maptick*FontHeightF : 0.012

*cnplot@cnScalarFieldData : $sf$
*cnplot@tiMainString : $psl$@long_name + " (" + $psl$@units + ")"
*cnplot@tiMainString%Profile : 	(/ \
	Name : main title, \
	ReferenceResource : sfDataArray, \
	Visibility : True \
/)

!*cnplot@tiXAxisString : title_func($psl$!-1)

!*cnplot@tiXAxisString : $psl$!-1@long_name
!*cnplot@tiYAxisString : $psl$!-2@long_name

*cnplot@tiXAxisString%Profile : (/ \
	Name : x axis title, \
	ReferenceResource : sfXAxisArray, \
	Visibility : True \
/)

*cnplot@cnFillColors : spread_colors($cnplot$)
*cnplot@pmAnnoViews : (/ $anno1$ /)

*anno1*amZone : 7
*anno1*amSide : bottom
*anno1*amJust : topleft
*anno1*amParallelPosF : -0.05
*anno1*amOrthogonalPosF : 0.05
*anno1@txString : "NCAR DataVision: " + systemfunc("date")
*anno1@txString%Profile : (/ Name : Time Stamp /)
*anno1*txFontHeightF : 0.012

!*map*mpProjection : orthographic
!*map*mpProjection : mercator
*map*mpGeophysicalLineColor : Background
*map*vpXF : 0.05
*map*vpWidthF : 0.55

*vpUseSegments : True
*MaxLevelCount : 20
*cnplot*cnInfoLabelZone : 6
*cnplot*cnRasterModeOn : True
*cnplot*cnRasterSmoothingOn : True
*cnplot*cnLinesOn : False
*cnplot*cnLineLabelsOn : False
*cnplot*pmLabelBarDisplayMode : always
*cnplot*pmLabelBarZone : 2
*cnplot*pmLabelBarWidthF : .1
*cnplot*vpXF: 0.1

*maptick*amZone : 0
*maptick*amResizeNotify : True

