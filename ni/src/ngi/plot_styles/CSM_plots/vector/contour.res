*ndvPlotStyleName : with contours
*ndvPlotName      : vector
*ndvData          : (/ udata : 2, vdata : 2, sdata : 2 /)
*ndvFuncFiles     : (/ ../csm_utils.ncl /)

;
; Define the objects we created.
;
*ndvObjects : (/ \
  sf             : scalarFieldClass, \
  vf             : vectorFieldClass, \
  vector         : vectorPlotClass, \
  contour        : contourPlotClass, \
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
*udata!-1@Pattern  : /.*lon.*/i
*udata!-2@Pattern  : /.*lat.*/i
*udata@Description : vector data
*udata@Required    : True

*vdata@Pattern     : (/ /.*V.*/i , /.*/ /)
*vdata!-1@Pattern  : /.*lon.*/i
*vdata!-2@Pattern  : /.*lat.*/i
*vdata@Description : vector data
*vdata@Required    : True

*sdata@Pattern     : (/ /.*T.*/i , /.*/ /)
*sdata!-1@Pattern  : /.*lon.*/i
*sdata!-2@Pattern  : /.*lat.*/i
*sdata@Description : scalar data
*sdata@Required    : True

;
; Vector field setup.
;
*vf@vfUDataArray         : FixLongitude($udata$)
*vf@vfVDataArray         : FixLongitude($vdata$)
*vf@vfUDataArray%Profile : (/ Name : U /)
*vf@vfVDataArray%Profile : (/ Name : V /)
*vf@vfXArray             : FixLongitudeCoord($vdata$&-1)
*vf@vfXArray%Profile     : (/ Name : UV longitude /)
*vf@vfYArray             : $vdata$&-2
*vf@vfYArray%Profile     : (/ Name : UV latitude /)

;
; Scalar field setup.
;
*sf@sfDataArray         : FixLongitude($sdata$)
*sf@sfDataArray%Profile : (/ Name : scalar /)
*sf@sfXArray            : FixLongitudeCoord($sdata$&-1)
*sf@sfXArray%Profile    : (/ Name : scalar longitude /)
*sf@sfYArray            : $sdata$&-2
*sf@sfYArray%Profile    : (/ Name : scalar latitude /)

;
; map resources
;
*map@pmOverlays     : (/ $vector$, $contour$, $tickmark$ /)
*map@pmAnnoViews    : (/ $left_title$, $center_title$, $right_title$ /)
*map@ndvUpdateFunc : PlotTitles($map$, $left_title$, $udata$@long_name,\
                                         $center_title$, "", \
                                         $right_title$, $udata$@units, \
                                         0.014,1)
*map@ndvUpdateFunc%Profile : (/ Name : Plot Titles /)
*map@ndvUpdateFunc2  : AdjustFontSizes($contour$,$tickmark$,1.0)
*map@ndvUpdateFunc2%Profile : (/ Name : Adjust Contour Info Label Font Size /)
*map@ndvUpdateFunc3  : AdjustFontSizes($vector$,$tickmark$,1.0)
*map@ndvUpdateFunc3%Profile : (/ Name : Adjust Vector Ref Anno Font Size /)

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

;
; contour resources
;
*contour@cnScalarFieldData    : $sf$

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
