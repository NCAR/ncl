! 
! mp05 resource file
!
! Position each map in a different location in the viewport
! with the "vp" resources.
!
*mpPerimOn:                   true
*map0*mpProjection:           LambertConformal
*map0*vpXF:                   0.025
*map0*vpYF:                   0.87
*map0*vpWidthF:               0.21875
*map0*vpHeightF:              0.21875
*map0*mpLambertParallel1F:    30.
*map0*mpLambertParallel2F:    45.
*map0*mpLambertMeridianF:     0.
*map0*tiMainString:           Lambert Conformal Conic

*map1*mpProjection:           Stereographic
*map1*vpXF:                   0.26875
*map1*vpYF:                   0.87
*map1*vpWidthF:               0.21875
*map1*vpHeightF:              0.21875
*map1*tiMainString:           Stereographic

*map2*mpProjection:           Orthographic
*map2*vpXF:                   0.5125
*map2*vpYF:                   0.87
*map2*vpWidthF:               0.21875
*map2*vpHeightF:              0.21875
*map2*tiMainString:           Orthographic

*map3*mpProjection:           LambertEqualArea
*map3*vpXF:                   0.75625
*map3*vpYF:                   0.87
*map3*vpWidthF:               0.21875
*map3*vpHeightF:              0.21875
*map3*tiMainString:           Lambert Equal-Area

*map4*mpProjection:           Gnomonic
*map4*vpXF:                   0.025
*map4*vpYF:                   0.5875
*map4*vpWidthF:               0.21875
*map4*vpHeightF:              0.21875
*map4*tiMainString:           Gnomonic

*map5*mpProjection:           AzimuthalEquidistant
*map5*vpXF:                   0.26875
*map5*vpYF:                   0.5875
*map5*vpWidthF:               0.21875
*map5*vpHeightF:              0.21875
*map5*tiMainString:           Azimuthal Equidistant

*map6*mpProjection:           Satellite
*map6*vpXF:                   0.5125
*map6*vpYF:                   0.5875
*map6*vpWidthF:               0.21875
*map6*vpHeightF:              0.21875
*map6*tiMainString:           Satellite View

*map7*mpProjection:           Mercator
*map7*vpXF:                   0.75625
*map7*vpYF:                   0.5875
*map7*vpWidthF:               0.21875
*map7*vpHeightF:              0.21875
*map7*tiMainString:           Mercator

*map8*mpProjection:           CylindricalEquidistant
*map8*vpXF:                   0.025
*map8*vpYF:                   0.33125
*map8*vpWidthF:               0.4625
*map8*vpHeightF:              0.33125
*map8*tiMainString:           Cylindrical Equidistant

*map9*mpProjection:           Mollweide
*map9*vpXF:                   0.5125
*map9*vpYF:                   0.33125
*map9*vpWidthF:               0.4625
*map9*vpHeightF:              0.33125
*map9*tiMainString:           Mollweide Type
!
! global map resources
!
*mpFillBoundarySets:           National
*mpFillColors:                 (/100,65,83,100,23,35,45,81,100/)
*mpGridLineDashPattern:        1
*mpLabelFontHeightF:           .01
*mpMonoFillColor:              False
!
! title resources
!
*pmTitleDisplayMode:           always
*tiMainFont:                   courier-bold
*tiMainFontHeightF:            .01
!
! color map resources
!
*wkColorMap:                   example
*wkBackgroundColor:            (/1.,1.,1./)
*wkForegroundColor:            (/0.,0.,0./)
