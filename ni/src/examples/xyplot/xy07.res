!
! Define Title resources
!
*tiMainString:           Scattergram
*tiXAxisString:          X
*tiYAxisString:          Y
!
! Define Transform resources
!
*trXMinF:                -0.6
*trYMinF:                -0.6
*trXMaxF:                1.6
*trYMaxF:                1.6
!
! Define DataSpec resources
!
*xyMarkLineMode:         MARKERS
*xyData0*xyMarkerColor:  20 
*xyData0*xyMarkerSizeF:  .02
*xyData0*xyMarker:       2
*xyData1*xyMarkerColor:  45
*xyData1*xyMarkerSizeF:  .015
*xyData1*xyMarker:       5
!
! Change the color map slightly
!
*wkBackgroundColor:      (/1.,1.,1./)
*wkForegroundColor:      (/0.,0.,0./)
!
! Define TickMark X axis resources
!
*tmXBDataLeftF:          -0.6
*tmXBDataRightF:         1.6
*tmXBLabelFontHeightF:   0.015
*tmXBLabels:             (/-.6,-.4,-.2,0.,.2,.4,.6,.8,1.0,1.2,1.4,1.6/)
*tmXBMinorOn:            False
*tmXBMode:               Explicit
*tmXBValues:             (/-.6,-.4,-.2,0.,.2,.4,.6,.8,1.0,1.2,1.4,1.6/)
!
! Define TickMark Y axis resources
!
*tmYLDataLeftF:          -0.6
*tmYLDataRightF:         1.6
*tmYLLabelFontHeightF:   0.015
*tmYLLabels:             (/-.6,-.4,-.2,0.,.2,.4,.6,.8,1.0,1.2,1.4,1.6/)
*tmYLMinorOn:            False
*tmYLMode:               Explicit
*tmYLValues:             (/-.6,-.4,-.2,0.,.2,.4,.6,.8,1.0,1.2,1.4,1.6/)
