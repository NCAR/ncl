*ndvPlotStyleName : vs. latitude (b/w)
*ndvPlotName      : time_plot
*ndvData          : (/ cndata : 2 /)
*ndvFuncFiles     : (/ ../csm_utils.ncl /)

;
; Define the objects we created.
;
*ndvObjects : (/ \
  sf             : scalarFieldClass, \
  contour        : contourPlotClass, \
  left_title     : textItemClass, \
  center_title   : textItemClass, \
  right_title    : textItemClass \
/)   

;
; Describe the data.
;
*cndata@Pattern     : (/ /.*T.*/i , /.*/ /)
*cndata!-1@Pattern  : /.*time.*/i
*cndata!-2@Pattern  : /.*lat.*/i
*cndata@Description : contour data
*cndata@Required    : True

;
; Scalar field setup.
;
*sf@sfDataArray         : $cndata$
*sf@sfDataArray%Profile : (/ Name : Primary Data Var /)
*sf@sfYArray            : $cndata$&-2
*sf@sfYArray%Profile    : (/ Name : Latitude /)

;
; contour resources
;
*contour@cnScalarFieldData          : $sf$
*contour*cnLineLabelBackgroundColor : transparent
*contour@tiXAxisString              : $cndata$&-1@long_name
*contour@pmAnnoViews    : (/ $left_title$, $center_title$, $right_title$ /)
*contour@ndvUpdateFunc  : LabelLatY($contour$,30,15)
*contour@ndvUpdateFunc2 : PlotTitles($contour$, \
                                      $left_title$,$cndata$@long_name, \
                                      $center_title$, "", \
                                      $right_title$, $cndata$@units, \
                                      0.024,1)
*contour@ndvUpdateFunc2%Profile : (/ Name : Plot Titles /)
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
*vpUseSegments        : True
*MaxLevelCount        : 20
*Font                 : helvetica-bold
*TextFuncCode         : ~
