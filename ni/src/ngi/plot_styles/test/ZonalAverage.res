!
! The plot style name is the name that appears in the plot style menu
! Any string is permissable
!
*ndvPlotStyleName : Zonal Average
!
! The plot name is the base ncl variable name for the plot. It must conform
! to the usual restrictions for variable names. It is a graphic array 
! containing references to each of the objects in the ndvObjects list.
! Actual plot instances have digits appended to the base name to make them
! unique.
! 
*ndvPlotName : zoneavg
!
! Load these files containing the non-intrinsic Ncl functions referenced
! in this plot style file. Functions used in the plot style file are
! of two types, and used in two different contexts:
!
! The first type is used as a resource value and must return a value 
! convertable to the resource type. In this category also belong expressions
! that evaluate to a value of a suitable type. (These are defined in line 
! within the plot style file and could be considered 'anonymous' functions.)
! These functions are "filter" type functions. They should generally avoid
! side effects, since their purpose is simply to return a value.
!
! The other function type is used as the value of the special NDV resource:
! "ndvUpdateFunc[x]" (where [x] represents a small integer). Functions of 
! this type must return a three-way value where -1 indicates an error,
! 0 indicates that the function did not actually do anything, and 1
! indicates that the function changed something.
! These functions are evaluated for their side effects, such as setting 
! the values of objects passed in as parameters. Update functions are 
! only executed after all objects belonging to the plot have been created, 
! so you may freely pass as a parameter any object in the ndvObjects list, 
! knowing it will exist by the time the function is called. Also note that
! if an object is to have more than one update function, the resource name 
! must have a different integer suffix. When more than one update function
! exists they will be executed in the order of their extensions. No
! extension is equivalent to an extension value of 0. 
! All variables created inside the functions should be declared local.
!

*ndvFuncFiles : (/ test_style.ncl , ../common.ncl /)
!
! Handles for referencing data sets. The required dimensionality follows the
! colon. Eventually there may be support for specifying a dimensionality range.
! 
!*ndvData : (/ geosf : 2 , geosf2 : 2 /)
*ndvData : (/ geosf : 2 /)
!
! Objects to be created by the plot. These may be referenced within NDV either
! through array indexing of the plot graphic array variable, ordered as
! they appear in this list, or by individual names, formed by prefixing the
! plot instance name followed by an underscore to the base name of the object. 
! When accessed individually plot objects have the following attributes:
! "ndvClass" -- the string name of the class 
! "ndvWks"   -- a reference to the object's Workstation (View objects only)
!
*ndvObjects : (/ \
	sf : scalarFieldClass , \
	ca : coordArraysClass, \
	cnplot : contourPlotClass , \
	map : mapPlotClass , \
	xy : xyPlotClass , \
	left_title : textItemClass , \
	right_title : textItemClass , \
	center_title : textItemClass , \
	timestamp : textItemClass ,\
	maptick : logLinPlotClass \
/)
! 
! describes the data set
!
*geosf@Pattern : (/  /.*psl.*/i , /.*/ /)
*geosf!-1@Pattern : /.*lon.*/i
*geosf!-2@Pattern : /.*lat.*/i
*geosf@Description : Georeferenced Scalar Field
*geosf@Required : True

!
! if the geosf2 data, along with the Required attribute is uncommented, is
! will cause the plot not to be created immediately. A popup window will 
! say that there is still undefined data. When the OK is clicked, the 
! plot page will come up and the user will be expected to fill in a valid
! value for this data item. (Note that this plot style does not actually need
! a second data set. It is only to test the mechanism.) 
!
!*geosf2@Pattern : (/  /.*psl.*/i , /.*/ /)
!*geosf2!-1@Pattern : /.*lon.*/i
!*geosf2!-2@Pattern : /.*lat.*/i
!*geosf2@Description : Second Geographic Scalar Field
!*geosf2@Required : True

!
! Zonal average coordinate array
!
*ca@caXArray : dim_average($geosf$)
*ca@caXArray%Profile : (/ Name : Zonal Average X /)
*ca@caYArray : $geosf$!-2
*ca@caYArray%Profile : (/ Name : Zonal Average Y /)
!
! Zonal average XyPlot
!
*xy@xyCoordData : $ca$
*xy@ndvUpdateFunc4 : adjust_tickmarks($maptick$,$xy$,0,0,0)
*xy@ndvUpdateFunc4%Profile : \
	(/ Name : Xy TickMarks /)
*xy@ndvUpdateFunc3 : pos_xy_anno($maptick$,$ca$,$xy$)
*xy@ndvUpdateFunc3%Profile : \
	(/ Name : Zonal Average Plot Pos /)
*xy*amSide : right
*xy*amZone : 3
*xy*amParallelPosF : .5
*xy*amOrthogonalPosF : 0.0
*xy*amResizeNotify : True
*xy*tmYLLabelsOn : False
*xy*tiMainString : Zonal Average
*xy*xyComputeYMax : False
*xy*xyComputeYMin : False
!*xy*xyComputeXMax : False
!*xy*xyComputeXMin : False
*xy*tmXBFormat : *+^sg

!
! Main data scalar field
!
*sf@sfDataArray : fix_longitude($geosf$)
*sf@sfDataArray%Profile : (/ Name : Primary Data Var /)
*sf@sfXArray : fix_longitude_coord($geosf$!-1)
*sf@sfXArray%Profile : (/ Name : Longitude /)
*sf@sfYArray : $geosf$!-2
*sf@sfYArray%Profile : (/ Name : Latitude /)
!
! The map
!
*map@pmOverlays : (/ $cnplot$, $maptick$ /)
*map@pmOverlays%Profile : (/ InitializeOnly : True /)

*map@ndvUpdateFunc : set_map_limits_from_object($map$,$cnplot$,0)
*map@ndvUpdateFunc%Profile : (/ Name : Set Map Limits from Data Extent /)

*map@pmAnnoViews : (/$xy$,$left_title$,$center_title$,$right_title$ /)
*map@pmAnnoViews%Profile : (/ InitializeOnly : True /)

!*map@ndvUpdateFunc1 : plot_title($map$,\
	$left_title$,$geosf$@long_name,$right_title$,$geosf$@units,0.014,1)
!*map@ndvUpdateFunc1%Profile : (/ Name : Plot Titles /)
!*map@ndvUpdateFunc1 : plot_titles($map$,\
!	$left_title$,$geosf$@long_name,\
!	$center_title$,$geosf$!-3 + " " + $geosf$!-3@units,\
!	$right_title$,$geosf$@units,0.014,1)
*map@ndvUpdateFunc1 : plot_titles($map$,\
	$left_title$,$geosf$@long_name,\
	$center_title$,"",\
	$right_title$,$geosf$@units,0.012,1)
*map@ndvUpdateFunc1%Profile : (/ Name : Plot Titles /)

!*map@ndvUpdateFunc2 : test_func ($geosf$!-3 )
!*map@ndvUpdateFunc2%Profile : (/ Name : Test Func /)

!*map*mpGeophysicalLineColor : Foreground
*map*vpXF : 0.08
*map*vpWidthF : 0.57
!
! Loglinplot used to overlay tickmarks
!
*maptick@ndvUpdateFunc2 : map_tickmarks($map$,$maptick$,0.009,0.012,0.006)
*maptick@ndvUpdateFunc2%Profile : (/ Name : Lat-Lon Tickmarks /)
*maptick*tfDoNDCOverlay : True
*maptick*pmTickMarkDisplayMode : always
*maptick*amZone : 0
*maptick*amResizeNotify : True
*maptick*FontHeightF : 0.012

!
! the ContourPlot
!
*cnplot@cnScalarFieldData : $sf$
*cnplot@cnFillColors : spread_colors($cnplot$,2,-1)
*cnplot@cnFillColors%Profile : (/ Name : Level Colors , \
				SaveForCompare : True /)
*cnplot@ndvUpdateFunc : SetContourLevels($cnplot$,0,0,0)
*cnplot@ndvUpdateFunc%Profile : (/ Name : Contour Level Control /)
*cnplot@pmAnnoViews : (/ $timestamp$ /)
*cnplot@pmAnnoViews%Profile : (/ InitializeOnly : True /)
*cnplot@ndvUpdateFunc4 : adjust_labelbar($maptick$,$cnplot$,0,0)
*cnplot@ndvUpdateFunc4%Profile : (/ Name : Contour LabelBar /)
*cnplot*cnRasterModeOn : True
*cnplot*cnRasterSmoothingOn : True
*cnplot*cnLinesOn : False
*cnplot*cnLineLabelsOn : False
*cnplot*pmLabelBarDisplayMode : always
*cnplot*pmLabelBarZone : 3
*cnplot*pmLabelBarSide : bottom
*cnplot*lbOrientation : horizontal
*cnplot*lbPerimOn : False
*cnplot*cnInfoLabelOn : False
!*cnplot*cnInfoLabelZone : 6
!
! the Time Stamp annotation
!
*timestamp@txString : "NCAR DataVision: " + systemfunc("date")
*timestamp@txString%Profile : (/ Name : Time Stamp /)
*timestamp*amZone : 7
*timestamp*amSide : bottom
*timestamp*amJust : topleft
*timestamp*amParallelPosF : -0.05
*timestamp*amOrthogonalPosF : 0.05
*timestamp*txFontHeightF : 0.012
!
! the left title annotation
!
*left_title*amZone : 3
*left_title*amSide : top
*left_title*amParallelPosF : 0.0
*left_title*amOrthogonalPosF : 0.05
*left_title*amJust : bottomleft
*left_title*amResizeNotify : True
!
! the center title annotation
!
*center_title*amZone           : 3
*center_title*amSide           : Top
*center_title*amParallelPosF   : 0.5
*center_title*amOrthogonalPosF : 0.05
*center_title*amJust           : BottomCenter
*center_title*amResizeNotify   : True
!
! the right title annotation
!
*right_title*amZone : 3
*right_title*amSide : top
*right_title*amParallelPosF : 1.0
*right_title*amOrthogonalPosF : 0.05
*right_title*amJust : bottomright
*right_title*amResizeNotify : True
!
! these are things that should be globally True. This will be replaced with
! a global file that is read in separately.
!
*vpUseSegments : True
TickMarkClass*vpUseSegments : False
LabelBarClass*vpUseSegments : False
*MaxLevelCount : 20
*TextFuncCode : ~

