!
!example vc05 resource file (vc05.res)
!

!Resources for the vc05Work object.
!Change the workstation output from the default colorMap
!(default consists of a random sequence of colors)
!to a sequence of colors (cold to hot). 
 
vc05.vc05Work.wkColorMap : temp1

!Resources for the VectorPlot1 object.
!Turn off black and white arrows and use the default 16 colors spread
!between the lowest and highest vector magnitudes.

vc05.vc05Work.VectorPlot1.vcMonoLineArrowColor : false
vc05.vc05Work.VectorPlot1.vcMonoFillArrowFillColor : false
vc05.vc05Work.VectorPlot1.vcMaxLevelCount : 61 


!Resources for the "increasingvectors2" and "increasingvectors3" objects 
!Map the domain and range of the vector locations to the latitude
!and longitude ranges.

vc05.increasingvectors2.vfXCStartV : -180
vc05.increasingvectors2.vfXCEndV : 180

vc05.increasingvectors2.vfYCStartV : -90
vc05.increasingvectors2.vfYCEndV : 90

vc05.increasingvectors3.vfXCStartV : -180
vc05.increasingvectors3.vfXCEndV : 180

vc05.increasingvectors3.vfYCStartV : 50
vc05.increasingvectors3.vfYCEndV : 90


!Resources for the VectorPlot2 object. 
!Make the minimum vector length a fraction of the reference vector length.

vc05.vc05Work.VectorPlot2.vcMonoLineArrowColor : false
vc05.vc05Work.VectorPlot2.vcMonoFillArrowFillColor : false
vc05.vc05Work.VectorPlot2.vcMinFracLengthF : .33
vc05.vc05Work.VectorPlot2.vcMaxLevelCount : 61 
!Resources for the Map1 object.
!Turn off the grid lines.

vc05.vc05Work.Map1.mpGridAndLimbOn : false

!Resources for the VectorPlot3 object
!Change the default vector length in NDC. 
!Display a horizontal label bar.
!Decrease the height of the label bar.

vc05.vc05Work.VectorPlot3.vcMonoLineArrowColor : false
vc05.vc05Work.VectorPlot3.vcMonoFillArrowFillColor : false
vc05.vc05Work.VectorPlot3.vcMaxLevelCount : 61 
vc05.vc05Work.VectorPlot3.vcMinFracLengthF : .33
vc05.vc05Work.VectorPlot3.vcRefLengthF : .05
vc05.vc05Work.VectorPlot3.pmLabelBarDisplayMode : always
vc05.vc05Work.VectorPlot3.pmLabelBarSide : bottom
vc05.vc05Work.VectorPlot3.lbOrientation  : horizontal
vc05.vc05Work.VectorPlot3.pmLabelBarHeightF : .11
vc05.vc05Work.VectorPlot3.pmLabelBarWidthF : .6 
vc05.vc05Work.VectorPlot3.lbLabelStride :  5      ; Label every 5th entry




!Resources for the Map2 object
!Define the kind of map and the dimensions of the map.

vc05.vc05Work.Map2.mpGridAndLimbOn : false
vc05.vc05Work.Map2.mpProjection : Orthographic
vc05.vc05Work.Map2.mpCenterLatF : 90.0
vc05.vc05Work.Map2.mpCenterLonF : 0.0
vc05.vc05Work.Map2.mpLimitMode  : points
vc05.vc05Work.Map2.mpBottomPointLatF : 50.0
vc05.vc05Work.Map2.mpTopPointLatF : 50.0
vc05.vc05Work.Map2.mpLeftPointLatF : 50.0
vc05.vc05Work.Map2.mpRightPointLatF : 50.0
vc05.vc05Work.Map2.mpBottomPointLonF : 0.0
vc05.vc05Work.Map2.mpTopPointLonF : 180.0
vc05.vc05Work.Map2.mpLeftPointLonF : -90.0
vc05.vc05Work.Map2.mpRightPointLonF : 90.0

*mpPerimOn: True

