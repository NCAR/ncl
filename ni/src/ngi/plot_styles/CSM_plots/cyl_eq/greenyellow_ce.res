*ndvPlotStyleName : green/yellow
*ndvPlotName      : color_ce
*ndvData          : (/ cndata : 2 /)
*ndvFuncFiles     : (/ ../csm_utils.ncl, ../../common.ncl /)

;
; Define the objects we created.
;
*ndvObjects : (/ \
  sf             : scalarFieldClass, \
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
*cndata@Pattern     : (/ /.*T.*/i , /.*/ /)
*cndata!-1@Pattern  : /.*lon.*/i
*cndata!-2@Pattern  : /.*lat.*/i
*cndata@Description : contour data
*cndata@Required    : True

;
; Scalar field setup.
;
*sf@sfDataArray         : NgAdjustLongitude($cndata$,0,0)
*sf@sfDataArray%Profile : (/ Name : Primary Data Var /)
*sf@sfXArray            : NgAdjustLongitudeCoord($cndata$&-1,0,0)
*sf@sfXArray%Profile    : (/ Name : Longitude /)
*sf@sfYArray            : $cndata$&-2
*sf@sfYArray%Profile    : (/ Name : Latitude /)

;
; map resources
;
*map@pmOverlays     : (/ $contour$, $tickmark$ /)
*map@pmAnnoViews    : (/ $left_title$, $center_title$, $right_title$ /)
*map@ndvUpdateFunc0 : NgSetMapLimits($map$,$contour$,-1,0,0,0,0)
*map@ndvUpdateFunc0%Profile : (/ Name : Map Limits /)
*map@ndvUpdateFunc1 : SetColormap($map$,"GreenYellow")
*map@ndvUpdateFunc1%Profile : (/ InitializeOnly : True, Name : Set Colormap /)
*map@ndvUpdateFunc2 : PlotTitles($map$, $left_title$, $cndata$@long_name,\
                                  $center_title$,"", \
                                  $right_title$, $cndata$@units, \
                                  0.014,1)
*map@ndvUpdateFunc2%Profile : (/ Name : Plot Titles /)
*map@ndvUpdateFunc3 : AdjustFontSizes($contour$,$tickmark$,1.0)
*map@ndvUpdateFunc3%Profile : (/ Name : Adjust LabelBar Font Sizes /)

*map*mpPerimOn        : True
*map*mpGridAndLimbOn  : False
*map*vpWidthF    : 0.85
*map*vpHeightF   : 0.425
*map*mpShapeMode : FIXEDASPECTNOFITBB

;
; map tickmark resources
;
*tickmark@ndvUpdateFunc         : NgMapTickmarks \
               ($map$,$tickmark$,0.011,0.012,0.006)
*tickmark*tfDoNDCOverlay        : True
*tickmark*pmTickMarkDisplayMode : always
*tickmark*amZone                : 0
*tickmark*amResizeNotify        : True

;
; contour resources
;
*contour@cnScalarFieldData          : $sf$
*contour*cnFillOn                   : True
*contour@cnFillColors               : SpreadColors($contour$,2,-1)
*contour@cnFillColors%Profile : (/ Name : Spread Colors, SaveForCompare : True/)
*contour*pmLabelBarDisplayMode      : Conditional
*contour*pmLabelBarSide             : Bottom
*contour*lbOrientation              : Horizontal
*contour*lbPerimOn                  : False
*contour*lbLabelStride              : 2
*contour*cnLinesOn                  : False
*contour*cnLineLabelsOn             : False
*contour@ndvUpdateFunc              : SetContourLevels($contour$,0.,0.,0.)
*contour@ndvUpdateFunc%Profile      : (/ Name : Contour Level Control /)

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
*vpXF                 : 0.1
*vpWidthF             : 0.85
*vpUseSegments        : True
*MaxLevelCount        : 20
*Font                 : helvetica-bold
*TextFuncCode         : ~
