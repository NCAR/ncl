!
!
*ndvPlotStyleName : Grib Grid 211
*ndvPlotName : viewgrib
*ndvFuncFiles : (/ test_style.ncl , ../common.ncl /)

*ndvData : (/ geosf : 2 /)
*ndvObjects : (/ \
	sf : scalarFieldClass , \
	cnplot : contourPlotClass , \
	map : mapPlotClass /)
*geosf@Description : Grib Grid 211 Scalar Field
*geosf@Required : True
!
! Main data scalar field
!
*sf@sfDataArray : $geosf$
*sf@sfDataArray%Profile : (/ Name : Primary Data Var /)
!
! The map
!
*map@pmOverlays : (/ $cnplot$ /)
*map*mpGeophysicalLineColor : Background
*map*vpXF : 0.1
*map*vpWidthF : 0.8
*map*vpHeightF : 0.556522
*map*mpShapeMode : FIXEDASPECTNOFITBB
*map*mpProjection : LAMBERTCONFORMAL
*map*mpLambertMeridianF : -95
*map*mpLambertParallel1F: 25
*map*mpLambertParallel2F: 25
*map*mpLimitMode : corners
*map*mpLeftCornerLatF : 12.19
*map*mpLeftCornerLonF : -133.459
*map*mpRightCornerLonF : -49.3863
*map*mpRightCornerLatF : 57.28923
*map@ndvUpdateFunc1  : set_colormap($map$,"3saw")
*map@ndvUpdateFunc1%Profile : (/ InitializeOnly : True, Name : Set Colormap /)
*map@ndvUpdateFunc0 : \
	NgSetMapLimits($map$,$cnplot$,5,-133.459,-49.3863,12.19,57.28923)
*map@ndvUpdateFunc0%Profile : (/ Name : Map Limits /)

!
! the ContourPlot
!
*cnplot@cnScalarFieldData : $sf$
*cnplot*cnLevelSelectionMode : manuallevels
*cnplot*cnFillOn : True
*cnplot*cnRasterModeOn : True
*cnplot*cnRasterSmoothingOn : False
*cnplot*tfDoNDCOverlay: NDCDataExtent 
*cnplot*cnLinesOn : False
*cnplot*cnLineLabelsOn : False
*cnplot*pmLabelBarDisplayMode : always
*cnplot*pmLabelBarZone : 3
*cnplot*pmLabelBarSide : bottom
*cnplot*lbOrientation : horizontal
*cnplot*lbPerimOn : False
*cnplot@cnFillColors : spread_colors($cnplot$,2,-1)
*cnplot@cnFillColors%Profile : (/ Name : Level Colors , \
				SaveForCompare : True /)
!*cnplot@ndvUpdateFunc : SetContourLevels($cnplot$,0,0,0)
!*cnplot@ndvUpdateFunc%Profile : (/ Name : Contour Level Control /)

*vpUseSegments : True
*MaxLevelCount : 20
*TextFuncCode : ~

