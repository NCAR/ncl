!
! ViewPort resources
!
*vpXF:       0.2
*vpYF:       0.92
*vpWidthF:   0.6
*vpHeightF:  0.6
!
! XyPlot resources
!
!  If you want temperature values for other stations, change
!  the 'xyExplicitLegendLabels' resource accordingly.  You must 
!  know the three letter code for the city you want.  This resource
!  is only being used by the C and Fortran program version of this
!  example.  The NCL example sets the stations right in the script.
!
*xyExplicitLegendLabels: (/DEN,FTW,HNL,MIA,PHL,SAN,SEA,ALB,LAS/)
*xyData.xyLineColors:    (/90,96,68,23,50,83,19,58,40/)
*xyData.xyMarkLineMode:  MARKLINES
*xyData.xyMarkerColors:  (/90,96,68,23,50,83,19,58,40/)
*xyPlot1.tiYAxisString:  Degrees (:S:o:N:C)
*xyPlot2.tiYAxisString:  Degrees (:S:o:N:F)
!
! Legend resources
!
*pmLegendDisplayMode:     conditional
*lgPerimOn:               True
*lgAutoManage:            False
!
! Title resources
!
*tiMainString:        Temperatures on March 18, 1995
*tiXAxisString:       Hour of the Day
*tiMainFontHeightF:   0.02
*tiXAxisFontHeightF:  0.02
*tiYAxisFontHeightF:  0.02
!
! TickMark resources
!
*tmXBMinorOn:  False
*tmXBMode:     EXPLICIT
*tmXBLabels:   (/0,2,4,6,8,10,12,14,16,18,20,22/)
*tmXBValues:   (/0,2,4,6,8,10,12,14,16,18,20,22/)
