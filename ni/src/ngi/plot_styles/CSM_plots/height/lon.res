*ndvPlotStyleName : vs. longitude (b/w)
*ndvPlotName      : hgt
*ndvData          : (/ cndata : 2 /)
*ndvFuncFiles     : (/ ../csm_utils.ncl /)

;
; Define the objects we created.
;
*ndvObjects : (/ \
  sf             : scalarFieldClass, \
  tickmark       : logLinPlotClass, \
  contour        : contourPlotClass, \
  left_title     : textItemClass, \
  center_title   : textItemClass, \
  right_title    : textItemClass, \
  raxis_title    : textItemClass \
/)   

;
; Describe the data.
;
*cndata@Pattern     : (/ /.*T.*/i , /.*/ /)
*cndata!-1@Pattern  : /.*lon.*/i
*cndata!-2@Pattern  : /.*lev.*/i
*cndata@Description : contour data
*cndata@Required    : True

;
; Scalar field setup.
;
*sf@sfDataArray         : $cndata$
*sf@sfDataArray%Profile : (/ Name : Primary Data Var /)
*sf@sfXArray            : $cndata$&-1
*sf@sfXArray%Profile    : (/ Name : Longitude /)
*sf@sfYArray            : $cndata$&-2
*sf@sfYArray%Profile    : (/ Name : Level /)

;
; contour resources
;
*contour@cnScalarFieldData          : $sf$
*contour*cnLineLabelBackgroundColor : transparent
*tickmark@pmOverlays    : (/ $contour$ /)
*contour@pmAnnoViews    : (/ $left_title$, $center_title$, $right_title$, \
                             $raxis_title$ /)
*contour@ndvUpdateFunc  : LabelLon($contour$,30,15)
*contour@ndvUpdateFunc%Profile : (/ Name : Longitude Tickmarks on X Axis /)
*contour@ndvUpdateFunc2 : LabelHgtYR($contour$,$tickmark$,$cndata$&-2, \
                                     $raxis_title$)
*contour@ndvUpdateFunc2%Profile : (/ Name : Height Labels on Right Y Axis /)
*contour@ndvUpdateFunc3 : SetContourLevels($contour$,0.,0.,0.)
*contour@ndvUpdateFunc3%Profile : (/ Name : Contour Level Control /)
*contour@ndvUpdateFunc4 : SetContourLevelThickness($contour$,0.,-1.)
*contour@ndvUpdateFunc4%Profile : (/ Name : Contour Level Thickness /)
*contour@ndvUpdateFunc5 : LtGtContourDashPattern($contour$,0.,-2,0.,-2)
*contour@ndvUpdateFunc5%Profile : (/ Name : Contour Dash Patterns /)
*contour@ndvUpdateFunc6 : LtGtContourFillPattern($contour$,0.,-2,1.0,\
                                                           0.,-2,1.0)
*contour@ndvUpdateFunc6%Profile : (/ Name : Contour Fill Patterns /)
*contour@ndvUpdateFunc7 : PlotTitles($contour$, \
                                      $left_title$, $cndata$@long_name, \
                                      $center_title$, "", \
                                      $right_title$, $cndata$@units, \
                                      0.024,1.2)
*contour@ndvUpdateFunc7%Profile : (/ Name : Plot Titles /)
*contour@ndvUpdateFunc8 : AdjustFontSizes($contour$,$contour$,1.0)
*contour@ndvUpdateFunc8%Profile : (/ Name : Adjust Contour Info Label Font Size /)

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

*raxis_title*txString         : Height (km)
*raxis_title*amZone           : 3
*raxis_title*amSide           : Right
*raxis_title*amParallelPosF   : 0.5
*raxis_title*amOrthogonalPosF : 0.05
*raxis_title*amJust           : CenterCenter
*raxis_title*amResizeNotify   : True

;
; Other resources.
;
*vpYF                 : 0.85
*vpUseSegments        : True
*MaxLevelCount        : 20
*Font                 : helvetica-bold
*TextFuncCode         : ~
