
; Notice that resources can be set in any order, and that ';' is used
; for comments.  If you want a resource to apply to all possible plots,
; then precede it with the wildcard character '*'.  Resources are set by
; listing the resource, followed by a colon (instead of an equal sign
; like in NCL scripts) followed by its value.  Notice that you don't
; need to include strings in double quotes.

; Set the main title attributes.

*tiMainFont:        25
*tiMainFontColor:   6
*tiMainString:      Satellite view of surface temperature (:S:o:N:C) in January
*tiMainFontHeightF:  0.02

; Set the contour levels and colors.

*cnLevelSelectionMode: manuallevels
*cnMinLevelValF:       0.
*cnMaxLevelValF:       20.
*cnLevelSpacingF:      2.
*cnFillOn:             True
*cnInfoLabelOn:        False
*cnFillColors:         (/35,34,31,28,25,24,22,19,16,13,10,7/)
*cnHighLabelsOn:       False
*cnLineLabelsOn:       False
*cnLinesOn:            False
*cnLowLabelsOn:        False


; Set some viewport resources. Notice that these four resources have an
; extra string in them "gsun07n_map". Since the asterick ('*') serves
; as a wildcard, the "gsun07n_map" string further narrows down which
; plot these resources are supposed to apply to. Each plot created with
; the ez_* calls is named by the second string passed to open_wks, appended
; with an underscore ('_') and the type of plot it is (map, contour, vector,
; or streamline).  So, in this case, we passed "gsun07n" to open_wks, and
; it's the map plot we want to change the view port of, so we use the string
; "gsun07n_map to further qualify it.

*gsun07n_map.vpXF:        .04
*gsun07n_map.vpYF:        .85
*gsun07n_map.vpWidthF:    .7
*gsun07n_map.vpHeightF:   .7

; Set some resources for the polyline and polymarkers.

*gsLineColor:         3
*gsMarkerColor:       6
*gsMarkerSizeF:       0.014
*gsMarkerThicknessF:  2.
*gsLineThicknessF:    2.

; Set map projection and attributes.

*mpProjection:        satellite
*mpCenterLatF:        38.
*mpCenterLonF:        -76.
*mpCenterRotF:        75.
*mpSatelliteAngle2F:  90.
*mpSatelliteDistF:    1.5
*mpSatelliteAngle1F:  30.
*mpLimitMode:         angles
*mpLeftAngleF:        20.
*mpRightAngleF:       20.
*mpTopAngleF:         20.
*mpBottomAngleF:      20.

; We only want a label bar added to our contour plot, not both the contour
; plot *and* the map plot (which is what would happen if we just used
; *pmLabelBarDisplayMode), so further qualify this resource with
; "gsun07n_contour".

*gsun07n_contour*pmLabelBarDisplayMode : always

; Set some labelbar resources.

*lbPerimOn:    False
*lbLabelFont:  25

; Set some map resources

*mpFillColor:                 5
*mpFillOn:                    True
*mpGridAndLimbDrawOrder:       draw
*mpGridLineColor:             2
*mpGridLineThicknessF:        2.0
*mpGridSpacingF:              1.0
*mpMonoFillColor:             True
*mpOutlineBoundarySets:       geophysicalandusstates
*mpOutlineDrawOrder:          postdraw
*mpUSStateLineThicknessF:     2.0
*mpGeophysicalLineThicknessF: 2.0
*mpPerimOn:                   True

; Set text resources.

*txFont:         25
*txFontColor:    6
*txFontHeightF:  0.018

