;
; Set the main title attributes.
;
*tiMainFont:        25
*tiMainFontHeightF: 0.015

;
; Set the contour levels and colors.
;
*cnLevels:         15
*cnFillOn:         True
*cnFillColors:     (/7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22/)
*cnHighLabelsOn:   False
*cnLineLabelsOn:   False
*cnLinesOn:        False
*cnLowLabelsOn:    False
*cnInfoLabelOn:    False

; 
; Set the line thickness and color for circle.
;
*gsLineColor:         black
*gsMarkerColor:       yellow
*gsMarkerSizeF:       0.014
*gsMarkerThicknessF:  2.
*gsLineThicknessF:    2.

; 
; Set map projection and attributes.
;
*mpProjection:        satellite
*mpCenterLatF:        38.
*mpCenterLonF:        -76.
*mpCenterRotF:        75.
*mpSatelliteAngle2F:  90.
*mpSatelliteDistF:    1.3
*mpLimitMode:         angles
*mpLeftAngleF:        20.
*mpRightAngleF:       20.
*mpTopAngleF:         20.
*mpBottomAngleF:      20.
*MapPlot.vpXF:        .05
*MapPlot.vpYF:        .9
*MapPlot.vpWidthF:    .85
*MapPlot.vpHeightF:   .85

*mpFillColor:           gray40
*mpFillOn:              True
*mpGridAndLimbDrawOrder: draw
*mpGridLineColor:       gray60
*mpGridLineThicknessF:  2.0
*mpGridSpacingF:        1.0
*mpMonoFillColor:       True
*mpOutlineBoundarySets: geophysicalandusstates
*mpOutlineDrawOrder:    postdraw
*mpUSStateLineThicknessF: 2.0
*mpGeophysicalLineThicknessF: 2.0
*mpPerimOn:             True

; 
; Set resources for US state names.
;
*txAngleF:       -45.
*txFont:         25
*txFontColor:    yellow
*txFontHeightF:  0.02

;
; Set some resources for AnnoManager object.
;
*amTrackData:  true
*amZone:       1
