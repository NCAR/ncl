
! MapPlot resources

*wkBackgroundColor : black

*TextFuncCode          : :             
*txFuncCode            : :

*mapplot*vpXF : 0.03
*mapplot*vpYF : 0.75
*mapplot*vpWidthF : 0.8

*mpFillOn : True
*mpFillDrawOrder : predraw
*mpOutlineDrawOrder : draw
*mpLabelsOn : False
*mpPerimOn : False
*mpGridAndLimbOn : False
*mpProjection : LambertEqualArea
*mpLimitMode : latlon
*mpMinLatF : 18.0
*mpMaxLatF : 65.
*mpMinLonF : -128.
*mpMaxLonF : -58.
*mpCenterLonF : -100.0
*mpCenterLatF : 40.0
!*mpFillColors : (/16,10, 8,10,26,22,11,23,13,19,24,25,21,20,18/)

! ContourPlot resources

*contourplot*pmLabelBarDisplayMode : always
*contourplot*pmLabelBarHeightF : 0.1
*contourplot*pmLabelBarWidthF : 0.6
*contourplot*lbOrientation : horizontal
*contourplot*pmLabelBarSide : top
*contourplot*lbTitleString : Surface Temperature (:F34:0:F:F)
*contourplot*lbTitleExtentF : 0.25
*cnFillColors : (/ 2,3,4,5,6,8,10,12,16,20,22,24,26,27 /)
*cnMinLevelValF : -20
*cnMaxLevelValF : 100.
*cnLevelSpacingF : 10.
*cnLevelSelectionMode : manuallevels
*cnInfoLabelOn : false
*cnLineLabelsOn : false
*cnHighLabelsOn : false
*cnLowLabelsOn : false

!VectorPlot resources

*vectorplot*pmLabelBarDisplayMode : always
*vectorplot*pmLabelBarWidthF : 0.1
*vectorplot*lbTitleOn : true
*vectorplot*lbTitleString : Sea Level Pressure
*vectorplot*lbTitlePosition : left
*vectorplot*lbTitleOffsetF : 0.13
*vectorplot*lbBoxMinorExtentF : 0.25
*vcRefLengthF : 0.045
*vcMinFracLengthF : 0.15
*vcFillArrowsOn : true
*vcFillArrowWidthF : 0.06
*vcFillArrowMinFracWidthF : 0.3
*vcFillArrowHeadMinFracXF : 0.2
*vcFillArrowHeadMinFracYF : 0.35
*vcFillArrowEdgeWidthF : 3.0
*vcMonoFillArrowFillColor : false
*vcRefMagnitudeF : 20.0
*vcMinMagnitudeF : 0.001
*vcLevelSelectionMode : manuallevels
*vcLevelColors : (/ 28,30,32,34,36,38,40,42,45,48,51,54,57,60 /)
*vcLevelSpacingF : 5.0
*vcMinLevelValF : 980.0
*vcMaxLevelValF : 1040.0
*vcUseRefAnnoRes : true
*vcRefAnnoArrowUseVecColor : false
*vcRefAnnoBackgroundColor : 2
*vcRefAnnoFontColor : black
*vcRefAnnoFont : helvetica-bold
*vcRefAnnoArrowFillColor : 28
*vcRefAnnoArrowLineColor : 1
*vcRefAnnoPerimOn : false
*vcRefAnnoString2 : Surface winds
*vcRefAnnoString1 : $VMG$ meters/second
*vcMinAnnoString2On : false
*vcMinAnnoString1 : $VMG$ meters/second
*vcMinAnnoOn : true
*vcMinAnnoExplicitMagnitudeF : 5.0
*vcMinAnnoZone : 4
*vcMinAnnoParallelPosF : 0.78

! StreamlinePlot resources

*streamlineplot*pmTitleZone : 8
*streamlineplotanno*txString : Streamlines (in black) represent 500 mb winds
*streamlineplotanno*txFontHeightF : 0.013
*streamlineplotanno*txBackgroundFillColor : 2
*streamlineplotanno*txFontColor : black
*streamlineplotanno*txFont : helvetica-bold
*streamlineplotanno*amZone : 4
*streamlineplotanno*amJust : topleft
*streamlineplotanno*amOrthogonalPosF : 0.02
*stLineThicknessF : 2.0
*stLineColor : black
*stArrowLengthF : 0.01
*stMinArrowSpacingF : 0.02
