!
! Title resources
!
*xyPlot1*tiMainString:   Location of all stations
*xyPlot2*tiMainString:   Location of stations over mainland US
*mpPlot1*tiMainString:   Cylindrical Equidistant Projection
*mpPlot2*tiMainString:   Lambert Conformal Conic Projection
*xyPlot1*tiXAxisString:          longitude
*xyPlot1*tiYAxisString:          latitude
*xyPlot2*tiXAxisString:          longitude
*xyPlot2*tiYAxisString:          latitude
!
! Data resources
!
*xyData*xyMarkerSizeF:   0.02
*xyMarker:               2
*xyMarkerColor:          1
*xyMarkLineMode:         MARKERS
*caYMissingV:            -9999.
*caXMissingV:            -9999.
!
! XyPlot resources
!
*xyPlot1*trYMinF:        -100.
*xyPlot1*trYMaxF:        100.
*xyPlot1*trXMinF:        -200.
*xyPlot1*trXMaxF:        200.

*xyPlot2*trYMinF:        22.
*xyPlot2*trYMaxF:        50.
*xyPlot2*trXMinF:        -125.
*xyPlot2*trXMaxF:        -65.
!
! Map Resources
!
*mpPerimOn:                      True
*mpFillOn:                       True
*mpGridAndLimbOn:                False
*mpOutlineOn:                    False
*mpShapeMode:                    FreeAspect
*mpLabelsOn:                     False

*mpPlot1*mpFillBoundarySets:     National
*mpPlot1*mpLimitMode:            MaximalArea
*mpPlot1*mpProjection:           CylindricalEquidistant

*mpPlot2*mpFillBoundarySets:     USStates
*mpPlot2*mpLimitMode:            LatLon
*mpPlot2*mpMaxLatF:              50.
*mpPlot2*mpMaxLonF:              -65.
*mpPlot2*mpMinLatF:              22.
*mpPlot2*mpMinLonF:              -125.
*mpPlot2*mpProjection:           LambertConformal
*mpPlot2*mpRelativeCenterLat:    True
*mpPlot2*mpRelativeCenterLon:    True
!
! Color map resources
!
*wkBackgroundColor:               (/1.,1.,1./)
*wkForegroundColor:               (/0.,0.,0./)
*wkColorMap:                      uniform
