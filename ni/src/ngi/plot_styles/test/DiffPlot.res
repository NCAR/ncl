!
! The plot style name is the name that appears in the plot style menu
! Any string is permissable
!
*ndvPlotStyleName : Difference Plot
!
! The plot name is the base ncl variable name for the plot. It must conform
! to the usual restrictions for variable names. It is a graphic array 
! containing references to each of the objects in the ndvObjects list.
! Actual plot instances have digits appended to the base name to make them
! unique.
! 
*ndvPlotName : diffplot
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
! The ConformalGroup resource requires the data sets it references to conform
! in dimensionality, dimension sizes, and dimension names (if specified).
! This implies use of the same coordinate vars if they exist.
!
*ndvData : (/ geosf1 : 2 , geosf2 : 2 /)
*ndvData@ConformalGroup : (/ $geosf1$, $geosf2$ /)
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
	sf1 : scalarFieldClass , \
	sf2 : scalarFieldClass , \
	sf3 : scalarFieldClass , \
	cn1 : contourPlotClass , \
	map1 : mapPlotClass , \
	maptick1 : logLinPlotClass ,\
	left_title1 : textItemClass , \
	right_title1 : textItemClass , \
	center_title1 : textItemClass , \
	cn2 : contourPlotClass , \
	map2 : mapPlotClass , \
 	maptick2 : logLinPlotClass , \
	left_title2 : textItemClass , \
	right_title2 : textItemClass , \
	center_title2 : textItemClass , \
	cn3 : contourPlotClass , \
	map3 : mapPlotClass , \
 	maptick3 : logLinPlotClass , \
	left_title3 : textItemClass , \
	right_title3 : textItemClass , \
	center_title3 : textItemClass \
/)
;	timestamp : textItemClass \


! 
! describes the data set
!
! The Pattern resource specifies a Perl regular expression. 
!

*geosf1!-1@Pattern : /.*lon.*/i
*geosf1!-2@Pattern : /.*lat.*/i
*geosf1@Description : First Georeferenced Scalar Field
*geosf1@Required : True

*geosf2!-1@Pattern : /.*lon.*/i
*geosf2!-2@Pattern : /.*lat.*/i
*geosf2@Description : Second Georeferenced Scalar Field
*geosf2@Required : True

!
! scalar fields
!

*sf1@sfDataArray : fix_longitude($geosf1$)
*sf1@sfDataArray%Profile : (/ Name : First Data Set /)
*sf1@sfXArray : fix_longitude_coord($geosf1$!-1)
*sf1@sfXArray%Profile : (/ Name : Longitude /)
*sf1@sfYArray : $geosf1$!-2
*sf1@sfYArray%Profile : (/ Name : Latitude /)

*sf2@sfDataArray : fix_longitude($geosf2$)
*sf2@sfDataArray%Profile : (/ Name : Second Data Set /)
*sf2@sfXArray : fix_longitude_coord($geosf2$!-1)
*sf2@sfXArray%Profile : (/ Name : Longitude /)
*sf2@sfYArray : $geosf2$!-2
*sf2@sfYArray%Profile : (/ Name : Latitude /)

*sf3@sfDataArray : fix_longitude(difference($geosf1$,$geosf2$))
*sf3@sfDataArray%Profile : (/ Name : Difference Data Set /)
*sf3@sfXArray : fix_longitude_coord($geosf1$!-1)
*sf3@sfXArray%Profile : (/ Name : Longitude /)
*sf3@sfYArray : $geosf1$!-2
*sf3@sfYArray%Profile : (/ Name : Latitude /)

!!!!!!!!!!!!!!!!!!!!
! First plot
!
! MapPlot
!
*map1@pmOverlays : (/ $cn1$, $maptick1$ /)
*map1@pmOverlays%Profile : (/ InitializeOnly : True /)
!
;*map1@pmAnnoViews : (/ $left_title1$,$right_title1$,$center_title1$ /)
*map1@pmAnnoViews : (/ $left_title1$,$right_title1$,$center_title1$, \
			$map3$,$map2$ /)
*map1@pmAnnoViews%Profile : (/ InitializeOnly : True /)
!
*map1@ndvUpdateFunc0 : set_map_limits_from_object($map1$,$cn1$,0)
*map1@ndvUpdateFunc0%Profile : \
	(/ Name : Set Map Limits from Data Extent, \
           Visible : False /)

!
!*map1@ndvUpdateFunc2 : plot_titles($map1$,\
!	$left_title1$,$geosf1$@long_name,\
!	$center_title1$,"Step: " + $geosf1$!-3,\
!	$right_title1$,$geosf1$@units,0.014,1)
*map1@ndvUpdateFunc2 : plot_titles($map1$,\
	$left_title1$,$geosf1$@long_name,\
	$center_title1$,"",\
	$right_title1$,$geosf1$@units,0.014,1)
*map1@ndvUpdateFunc2%Profile : (/ Name : Plot Titles /)

*map1@ndvUpdateFunc3  : set_colormap($map1$,"ViBlGrWhYeOrRe")
*map1@ndvUpdateFunc3%Profile : (/ InitializeOnly : True, Name : Set Colormap /)

!
*map1*vpXF : 0.08
*map1*vpYF : 0.88
*map1*vpHeightF : 0.22
!
! Loglinplot used to overlay tickmarks
!
*maptick1@ndvUpdateFunc2 : map_tickmarks($map1$,$maptick1$,0.009,0.012,0.006)
*maptick1@ndvUpdateFunc2%Profile : (/ Name : Lat-Lon Tickmarks /)
*maptick1*tfDoNDCOverlay : True
*maptick1*pmTickMarkDisplayMode : always
*maptick1*amZone : 0
*maptick1*amResizeNotify : True
*maptick1*FontHeightF : 0.012
!
! ContourPlot
!
*cn1@cnScalarFieldData : $sf1$
*cn1@cnFillColors : spread_colors($cn1$,2,-1)
*cn1@cnFillColors%Profile : (/ Name : Level Colors , \
				SaveForCompare : True /)
*cn1@ndvUpdateFunc1 : SetContourLevels($cn1$,0,0,0)
*cn1@ndvUpdateFunc1%Profile : (/ Name : Contour Level Control /)
*cn1@ndvUpdateFunc4 : adjust_labelbar($maptick1$,$cn1$,0,0)
*cn1@ndvUpdateFunc4%Profile : (/ Name : Main LabelBar /)
*cn1*cnRasterModeOn : True
*cn1*cnRasterSmoothingOn : True
*cn1*cnLinesOn : False
*cn1*cnLineLabelsOn : False
*cn1*pmLabelBarDisplayMode : always
*cn1*pmLabelBarZone : 3
*cn1*pmLabelBarSide : right
*cn1*pmLabelBarParallelPosF : -0.1
*cn1*pmLabelBarHeightF : .45
*cn1*pmLabelBarWidthF : .035
*cn1*pmLabelBarOrthogonalPosF : .05
*cn1*lbLabelOffsetF : 0.4
*cn1*lbPerimOn : False
*cn1*lbLeftMarginF : 0.0
*cn1*lbRightMarginF : 0.0
*cn1*lbTopMarginF : 0.0
*cn1*lbBottomMarginF : 0.0
*cn1*lbBoxMinorExtentF : 1.0
*cn1*pmLabelBarKeepAspect : True
!*cn1*lbOrientation : horizontal
*cn1*lbPerimOn : False
*cn1*cnInfoLabelOn : False
*cn1*vpHeightF : 0.22
!
! TextItem annotations
!
! the data title
!
*left_title1*amZone : 2
*left_title1*amSide : top
*left_title1*amParallelPosF : 0.0
*left_title1*amOrthogonalPosF : 0.08
*left_title1*amJust : bottomleft
*left_title1*amResizeNotify : True
!
! the step title annotation
!
*center_title1*amZone           : 2
*center_title1*amSide           : Top
*center_title1*amParallelPosF   : 0.5
*center_title1*amOrthogonalPosF : 0.08
*center_title1*amJust           : BottomCenter
*center_title1*amResizeNotify   : True
!
! the data units title annotation
!
*right_title1*amZone : 2
*right_title1*amSide : top
*right_title1*amParallelPosF : 1.0
*right_title1*amOrthogonalPosF : 0.08
*right_title1*amJust : bottomright
*right_title1*amResizeNotify : True
!
!!!!!!!!!!!!!!!!!!!!
! Second plot
!
! MapPlot
!
*map2@pmOverlays : (/ $cn2$, $maptick2$ /)
*map2@pmOverlays%Profile : (/ InitializeOnly : True /)
!
*map2@pmAnnoViews : (/ $left_title2$,$right_title2$,$center_title2$ /)
*map2@pmAnnoViews%Profile : (/ InitializeOnly : True /)
!
*map2@ndvUpdateFunc0 : set_map_limits_from_object($map2$,$cn2$,0)
*map2@ndvUpdateFunc0%Profile : 	(/ Visible : False /)

!
!*map2@ndvUpdateFunc2 : plot_titles($map2$,\
!	$left_title2$,$geosf2$@long_name,\
!	$center_title2$,"Step: " + $geosf2$!-3,\
!	$right_title2$,$geosf2$@units,0.014,1)
*map2@ndvUpdateFunc2 : plot_titles($map2$,\
	$left_title2$,$geosf2$@long_name,\
	$center_title2$,"",\
	$right_title2$,$geosf2$@units,0.014,1)
*map2@ndvUpdateFunc2%Profile : (/ Name : Plot Titles /)
!
!
*map2.vpHeightF : 0.22
*map2.amZone : 2
*map2.amSide : bottom
!*map2.amOrthogonalPosF : 0.3
*map2.amJust : TopLeft
*map2.amResizeNotify : True

!
! Loglinplot used to overlay tickmarks
!
*maptick2@ndvUpdateFunc2 : map_tickmarks($map2$,$maptick2$,0.009,0.012,0.006)
*maptick2@ndvUpdateFunc2%Profile : (/ Name : Lat-Lon Tickmarks /)
*maptick2*tfDoNDCOverlay : True
*maptick2*pmTickMarkDisplayMode : always
*maptick2*amZone : 0
*maptick2*amResizeNotify : True
*maptick2*FontHeightF : 0.012
!
! ContourPlot
!
*cn2@cnScalarFieldData : $sf2$
*cn2@ndvUpdateFunc1 : match_contour_plots($cn1$,$cn2$)
*cn2@ndvUpdateFunc1%Profile : (/ Visible : False /)
*cn2*cnRasterModeOn : True
*cn2*cnRasterSmoothingOn : True
*cn2*cnLinesOn : False
*cn2*cnLineLabelsOn : False
*cn2*pmLabelBarDisplayMode : never
*cn2*pmLabelBarZone : 3
*cn2*pmLabelBarSide : right
*cn2*pmLabelBarParallelPosF : 1.1
!*cn2*lbOrientation : horizontal
*cn2*lbPerimOn : False
*cn2*cnInfoLabelOn : False
!
! TextItem annotations
!
! the data title
!
*left_title2*amZone : 2
*left_title2*amSide : top
*left_title2*amParallelPosF : 0.0
*left_title2*amOrthogonalPosF : 0.08
*left_title2*amJust : bottomleft
*left_title2*amResizeNotify : True
!
! the step title annotation
!
*center_title2*amZone           : 2
*center_title2*amSide           : Top
*center_title2*amParallelPosF   : 0.5
*center_title2*amOrthogonalPosF : 0.08
*center_title2*amJust           : BottomCenter
*center_title2*amResizeNotify   : True
!
! the data units title annotation
!
*right_title2*amZone : 2
*right_title2*amSide : top
*right_title2*amParallelPosF : 1.0
*right_title2*amOrthogonalPosF : 0.08
*right_title2*amJust : bottomright
*right_title2*amResizeNotify : True
!
!!!!!!!!!!!!!!!!!!!!
! Third plot
!
! MapPlot
!
*map3@pmOverlays : (/ $cn3$, $maptick3$ /)
*map3@pmOverlays%Profile : (/ InitializeOnly : True /)
!
*map3@pmAnnoViews : (/ $left_title3$,$right_title3$,$center_title3$ /)
*map3@pmAnnoViews%Profile : (/ InitializeOnly : True /)
!
*map3@ndvUpdateFunc0 : set_map_limits_from_object($map3$,$cn3$,0)
*map3@ndvUpdateFunc0%Profile : \
	(/ Name : Set Map Limits from Data Extent, \
           Visible : False /)
!
!*map3@ndvUpdateFunc2 : plot_titles($map3$,\
!	$left_title3$,$geosf2$@long_name,\
!	$center_title3$,"Step: " + $geosf2$!-3,\
!	$right_title3$,$geosf2$@units,0.014,1)
*map3@ndvUpdateFunc2 : plot_titles($map3$,\
	$left_title3$,"",\
	$center_title3$,"",\
	$right_title3$,$geosf2$@units,0.014,1)
*map3@ndvUpdateFunc2%Profile : (/ Name : Plot Titles /)
!
*map3@ndvUpdateFunc1 : match_viewport($map1$,(/$map2$,$map3$/))
*map3@ndvUpdateFunc1%Profile : \
	(/ Name : Match Viewport, \
           Visible : False /)
*map3@ndvUpdateFunc3 : adjust_plot_spacing($map1$,(/$map2$,$map3$/))
*map3@ndvUpdateFunc3%Profile : \
	(/ Name : Adjust Plot Spacing, \
           Visible : False /)
!
*map3.vpHeightF : 0.22
*map3.amZone : 3
*map3.amSide : bottom
!*map3.amOrthogonalPosF : 0.2
*map3.amJust : TopLeft
*map3.amResizeNotify : True
!
! Loglinplot used to overlay tickmarks
!
*maptick3@ndvUpdateFunc2 : map_tickmarks($map3$,$maptick3$,0.009,0.012,0.006)
*maptick3@ndvUpdateFunc2%Profile : (/ Name : Lat-Lon Tickmarks /)
*maptick3*tfDoNDCOverlay : True
*maptick3*pmTickMarkDisplayMode : always
*maptick3*amZone : 0
*maptick3*amResizeNotify : True
*maptick3*FontHeightF : 0.012
!
! ContourPlot
!
*cn3@cnScalarFieldData : $sf3$
*cn3@ndvUpdateFunc1 : SetContourLevels($cn3$,0,0,0)
*cn3@ndvUpdateFunc1%Profile : (/ Name : Contour Level Control /)
*cn3@ndvUpdateFunc3 : spread_colors_around_level_val($cn3$,2,-1,0)
*cn3@ndvUpdateFunc3%Profile : (/ Name : Difference Plot Colors , \
				SaveForCompare : True /)
*cn3@ndvUpdateFunc4 : adjust_labelbar($maptick3$,$cn3$,0,0)
*cn3@ndvUpdateFunc4%Profile : (/ Name : Difference LabelBar /)
*cn3*cnRasterModeOn : True
*cn3*cnRasterSmoothingOn : True
*cn3*cnLinesOn : False
*cn3*cnLineLabelsOn : False
*cn3*pmLabelBarDisplayMode : always
*cn3*pmLabelBarZone : 3
*cn3*pmLabelBarSide : right
*cn3*pmLabelBarWidthF : .083
*cn3*pmLabelBarOrthogonalPosF : .05
*cn3*lbPerimOn : False
*cn3*lbLeftMarginF : 0.0
*cn3*lbRightMarginF : 0.0
*cn3*lbTopMarginF : 0.0
*cn3*lbBottomMarginF : 0.0
*cn3*lbBoxMinorExtentF : 1.0
*cn3*lbLabelOffsetF : 0.4
*cn3*pmLabelBarKeepAspect : True
*cn3*cnInfoLabelOn : False
!
! TextItem annotations
!
! the data title
!
*left_title3*amZone : 2
*left_title3*amSide : top
*left_title3*amParallelPosF : 0.0
*left_title3*amOrthogonalPosF : 0.08
*left_title3*amJust : bottomleft
*left_title3*amResizeNotify : True
!
! the step title annotation
!
*center_title3*amZone           : 2
*center_title3*amSide           : Top
*center_title3*amParallelPosF   : 0.5
*center_title3*amOrthogonalPosF : 0.08
*center_title3*amJust           : BottomCenter
*center_title3*amResizeNotify   : True
!
! the data units title annotation
!
*right_title3*amZone : 2
*right_title3*amSide : top
*right_title3*amParallelPosF : 1.0
*right_title3*amOrthogonalPosF : 0.08
*right_title3*amJust : bottomright
*right_title3*amResizeNotify : True
!
! the Time Stamp annotation
!
*timestamp@txString : "NCAR DataVision: " + systemfunc("date")
*timestamp@txString%Profile : (/ Name : Time Stamp /)
*timestamp*txFontHeightF: 0.012
*timestamp*txJust: BottomLeft
*timestamp*txPosXF: 0.08
*timestamp*txPosYF: 0.02
*timestamp3*amZone           : 5
*timestamp3*amSide           : Bottom
*timestamp3*amParallelPosF   : 0.0
*timestamp3*amOrthogonalPosF : 0.05
*timestamp3*amJust           : BottomLeft
*timestamp3*amResizeNotify   : True
!
! these are things that should be globally True. This will be replaced with
! a global file that is read in separately.
!
*vpUseSegments : True
*MaxLevelCount : 20
*TextFuncCode : ~

