!
! Increase the size and change the position of the plot on the
! viewport.
!
*vpXF                        : 0.05
*vpYF                        : 0.90
*vpWidthF                    : 0.75
*vpHeightF                   : 0.75

!
! Fill contours, turn off high, low, and informational labels,
! turn off drawing of contour lines, and set levels to 0.10.
!
*cnFillOn                    : True
*cnHighLabelsOn              : False
*cnLowLabelsOn               : False
*cnInfoLabelOn               : False
*cnLineLabelsOn              : False
*cnLevelSpacingF             : 0.10

!
! Create a stereographic map with an elliptical boundary and change
! the center of the projection.
!
*mpProjection                : Stereographic
*mpEllipticalBoundary        : True
*mpCenterLatF                : -90.

!
! Limit the map view to a smaller area.
!
*mpLimitMode                 : Angles
*mpBottomAngleF              : 65.
*mpLeftAngleF                : 65.
*mpRightAngleF               : 65.
*mpTopAngleF                 : 65.

! 
! Turn on map fill and fill land in gray and leave oceans and inland
! water transparent.  Make the outlines gray and double their thickness
!
*mpFillOn                    : True
*mpGeophysicalLineColor      : gray
*mpLandFillColor             : gray
*mpInlandWaterFillColor      : transparent
*mpOceanFillColor            : transparent
*mpGeophysicalLineThicknessF : 2.

!
! Don't draw the lat/lon grid lines over land, draw them 45 degrees
! apart, and draw them before the contours. Turn on the drawing of
! a map perimeter.
!
*mpGridMaskMode              : MaskNotOcean
*mpGridSpacingF              : 45.
*mpGridAndLimbDrawOrder      : Predraw
*mpPerimOn                   : True

!
! If you just set *pmLabelBarDisplayMode to True, then a label bar
! for *both* the contour plot and the map would be drawn. So, only
! turn on the label bar for the contour plot. Turning off the label
! bar for the map plot is not necessary since that's the default.
!
*gsun09n_contour*pmLabelBarDisplayMode : Always
*gsun09n_map*pmLabelBarDisplayMode     : Never

!
! Increase the label bar width and height, turn off its perimeter,
! and change the label bar label font to Times-Roman.
!
*pmLabelBarWidthF            : 0.07
*pmLabelBarHeightF           : 0.65
*lbPerimOn                   : False
*lbLabelFont                 : 25

! 
! Change the font and height of the main title.
!
*tiMainFont                  : 25
*tiMainFontHeightF           : 0.027
