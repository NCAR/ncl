*ndvPlotStyleName : northern hemisphere (b/w)
*ndvPlotName      : polar
*ndvData          : (/ cndata : 2 /)
*ndvFuncFiles     : (/ ../csm_utils.ncl /)

;
; Define the objects we created.
;
*ndvObjects : (/ \
  sf             : scalarFieldClass, \
  contour        : contourPlotClass, \
  map            : mapPlotClass, \
  left_title     : textItemClass, \
  center_title   : textItemClass, \
  right_title    : textItemClass, \
  lon0           : textItemClass, \
  lon30          : textItemClass, \
  lon60          : textItemClass, \
  lon90          : textItemClass, \
  lon120         : textItemClass, \
  lon150         : textItemClass, \
  lon180         : textItemClass, \
  lon210         : textItemClass, \
  lon240         : textItemClass, \
  lon270         : textItemClass, \
  lon300         : textItemClass, \
  lon330         : textItemClass \
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
*sf@sfDataArray         : FixLongitude($cndata$)
*sf@sfDataArray%Profile : (/ Name : Primary Data Var /)
*sf@sfXArray            : FixLongitudeCoord($cndata$!-1)
*sf@sfXArray%Profile    : (/ Name : Longitude /)
*sf@sfYArray            : $cndata$!-2
*sf@sfYArray%Profile    : (/ Name : Latitude /)

;
; map resources
;
*map@pmOverlays     : (/ $contour$ /)
*map@pmAnnoViews    : (/$left_title$,$center_title$,$right_title$,\
                        $lon0$,$lon30$,$lon60$,$lon90$,$lon120$,$lon150$,\
                        $lon180$,$lon210$,$lon240$,$lon270$,$lon300$,\
                        $lon330$/)
*map@ndvUpdateFunc  : SetColormap($map$,"hlu_default")
*map@ndvUpdateFunc%Profile : (/ InitializeOnly : True, Name : Set Colormap /)
*map@ndvUpdateFunc2 : SetMapFillColors($map$,"white","transparent", \
                                "lightgray","transparent")
*map@ndvUpdateFunc2%Profile : (/ Name : Set Map Fill Colors /)
*map@ndvUpdateFunc3 : PlotTitles($map$, $left_title$, $cndata$@long_name,\
                                         $center_title$, "", \
                                         $right_title$, $cndata$@units, \
                                         0.02,1)
*map@ndvUpdateFunc3%Profile : (/ Name : Plot Titles /)
*map@ndvUpdateFunc4 : MapPolarTickmarks($map$,"NH",(/$lon0$,$lon30$,\
                      $lon60$,$lon90$,$lon120$,$lon150$,$lon180$,$lon210$,\
                      $lon240$,$lon270$,$lon300$,$lon330$/),1.0)
*map@ndvUpdateFunc4%Profile : (/ Name : Polar Tickmark Labels /)
*map@ndvUpdateFunc5  : AdjustFontSizes($contour$,$lon0$,1.0)
*map@ndvUpdateFunc5%Profile : (/ Name: Adjust Contour Info Label Font Size /)

*map*mpProjection         : Stereographic
*map*mpEllipticalBoundary : True
*map*mpFillOn             : True
*map*mpPerimOn            : True
*map*mpOutlineOn          : False
*map*mpLimitMode          : LatLon
*map*mpMinLatF            :  0.
*map*mpCenterLatF         : 90.
*map*mpGridAndLimbOn      : True
*map*mpGridLineDashPattern: 2
*map*mpGridLonSpacingF    : 30.
*map*vpXF                 : 0.10
*map*vpYF                 : 0.86
*map*vpWidthF             : 0.78
*map*vpHeightF            : 0.78

;
; contour resources
;
*contour@cnScalarFieldData          : $sf$
*contour*cnLineLabelBackgroundColor : transparent
*contour@ndvUpdateFunc              : SetContourLevels($contour$,0.,0.,0.)
*contour@ndvUpdateFunc%Profile : (/ Name : Contour Level Control /)
*contour@ndvUpdateFunc2    : LtGtContourFillPattern($contour$,0.,-2,1.0,\
                                                              0.,-2,1.0)
*contour@ndvUpdateFunc2%Profile : (/ Name : Contour Fill Patterns /)
*contour@ndvUpdateFunc3    : LtGtContourDashPattern($contour$,0.,-1,\
                                                              0.,-1)
*contour@ndvUpdateFunc3%Profile : (/ Name : Contour Dash Patterns /)
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
