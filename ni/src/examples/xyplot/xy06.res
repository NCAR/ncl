!
! Set all fonts to helvetica
!
*Font : Helvetica
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
!  example.  The NCL example sets the stations right in the NCL script.
!
! DEN = Denver, FTW = Fort Worth, HNL = Honolulu, MIA = Miami,
! PHL = Philadelpha, SAN = San Diego, SEA = Seattle, ALB = Albuquerque
! LAS = Las Vegas
!
*xyExplicitLegendLabels: (/DEN,FTW,HNL,MIA,PHL,SAN,SEA,ALB/)
*xyData.xyLineColors:    (/2,3,4,5,6,7,8,9/)
*xyData.xyMarkLineMode:  MARKLINES
*xyData.xyMarkerColors:  (/2,3,4,5,6,7,8,9/)
!
! Legend resources
!
*pmLegendDisplayMode:     conditional
*lgPerimOn:               True
*lgAutoManage:            False
*lgLabelFontHeightF:      0.015

!
! Title resources
!
*tiXAxisString:       Hour of the Day
*xyPlot1.tiYAxisString:  Degrees (:S:o:N:C)
*xyPlot2.tiYAxisString:  Pressure (Hectopascals)
*xyPlot3.tiYAxisString:  Speed (meters/second)
*xyPlot1.tiMainString:   Temperatures on March 18, 1995
*xyPlot2.tiMainString:   Sea Level Pressure on March 18, 1995
*xyPlot3.tiMainString:   Wind Speed on March 18, 1995
*tiXAxisFontHeightF:  0.02
*tiYAxisFontHeightF:  0.02
*tiMainFontHeightF:   0.02
!
! TickMark resources
!
*tmXBMinorOn:  False
*tmXBMode:     EXPLICIT
*tmXBLabels:   (/0,2,4,6,8,10,12,14,16,18,20,22/)
*tmXBValues:   (/0,2,4,6,8,10,12,14,16,18,20,22/)
