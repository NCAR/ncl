*ndvPlotStyleName : color
*ndvPlotName      : vector

*ndvFuncFiles     : (/ ../csm_utils.ncl, ../../common.ncl /)

*ndvData          : (/ udata : 2, vdata : 2 /)
*ndvData@ConformalGroup : (/ $udata$, $vdata$ /)

;
; Define the objects we created.
;
*ndvObjects : (/ \
  vf             : vectorFieldClass, \
  vector         : vectorPlotClass, \
  map            : mapPlotClass, \
  tickmark       : logLinPlotClass, \
  left_title     : textItemClass, \
  center_title   : textItemClass, \
  right_title    : textItemClass \
/)   

;
; Describe the data.
;
*udata@Pattern     : (/ /.*U.*/i , /.*/ /)
*vdata@Pattern     : (/ /.*V.*/i , /.*/ /)
*udata!-1@Pattern  : /.*lon.*/i
*udata!-2@Pattern  : /.*lat.*/i
*udata@Description : U Component of Vector Field
*udata@Required    : True
*vdata!-1@Pattern  : /.*lon.*/i
*vdata!-2@Pattern  : /.*lat.*/i
*vdata@Description : V Component of Vector Field
*vdata@Required    : True

;
; Vector field setup.
;
*vf@vfUDataArray         : FixLongitude($udata$)
*vf@vfVDataArray         : FixLongitude($vdata$)
*vf@vfUDataArray%Profile : (/ Name : U Component /)
*vf@vfVDataArray%Profile : (/ Name : V Component /)
*vf@vfXArray             : FixLongitudeCoord($vdata$!-1)
*vf@vfXArray%Profile     : (/ Name : UV longitude /)
*vf@vfYArray             : $vdata$!-2
*vf@vfYArray%Profile     : (/ Name : UV latitude /)

;
; map resources
;
*map@pmOverlays     : (/ $vector$, $tickmark$ /)
*map@pmAnnoViews    : (/ $left_title$, $center_title$, $right_title$ /)
*map@ndvUpdateFunc  : SetColormap($map$,"rainbow")
*map@ndvUpdateFunc%Profile : (/ InitializeOnly : True, Name : Set Colormap /)
*map@ndvUpdateFunc2 : PlotTitles($map$, $left_title$, $udata$@long_name,\
                                         $center_title$, "", \
                                         $right_title$, $udata$@units, \
                                         0.014,1)
*map@ndvUpdateFunc2%Profile : (/ Name : Plot Titles /)
*map@ndvUpdateFunc3  : AdjustFontSizes($vector$,$tickmark$,1.0)
*map@ndvUpdateFunc3%Profile : (/ Name : Adjust LabelBar Font Sizes /)

*map*mpPerimOn        : True
*map*mpGridAndLimbOn  : False

;
; map tickmark resources
;
*tickmark@ndvUpdateFunc         : MapTickmarks($map$,$tickmark$)
*tickmark*tfDoNDCOverlay        : True
*tickmark*pmTickMarkDisplayMode : always
*tickmark*amZone                : 0
*tickmark*amResizeNotify        : True

;
; vector resources
;
*vector@vcVectorFieldData     : $vf$
*vector*vcMonoLineArrowColor  : False
*vector@vcLevelColors         : SpreadColors($vector$,2,-1)
*vector@vcLevelColors%Profile : (/ Name : Spread Colors, SaveForCompare : True/)
*vector*pmLabelBarDisplayMode : Conditional
*vector*pmLabelBarSide        : Bottom
*vector*lbOrientation         : Horizontal
*vector*lbPerimOn             : False
*vector*lbLabelStride         : 2
*vector@ndvUpdateFunc    : set_vector_size_params($vector$,0,0.04,0.0,0.02)
*vector@ndvUpdateFunc%Profile : (/ Name : Vector Size/Spacing Control /)

;
; Title resources
;
*left_title*amZone             : 3
*left_title*amSide             : Top
*left_title*amParallelPosF     : 0.0
*left_title*amOrthogonalPosF   : 0.05
*left_title*amJust             : BottomLeft
*left_title*amResizeNotify     : True

*center_title*amZone           : 3
*center_title*amSide           : Top
*center_title*amParallelPosF   : 0.5
*center_title*amOrthogonalPosF : 0.05
*center_title*amJust           : BottomCenter
*center_title*amResizeNotify   : True

*right_title*amZone            : 3
*right_title*amSide            : Top
*right_title*amParallelPosF    : 1.0
*right_title*amOrthogonalPosF  : 0.05
*right_title*amJust            : BottomRight
*right_title*amResizeNotify    : True

;
; Other resources.
;
*vpYF                 : 0.85
*vpXF                 : 0.1
*vpWidthF             : 0.85
*vpUseSegments        : True
*MaxLevelCount        : 20
*Font                 : helvetica-bold
*TextFuncCode         : ~
