! 
! cn04 resource file
!
! no tick marks
*cn04Work.ContourPlot1*pmTickMarkDisplayMode : never
!
! set the maximum number of levels high enough to allow the desired level
! spacing, and choose the automatic level selection mode
!
*cn04Work.ContourPlot1*cnMaxLevelCount : 22
*cn04Work.ContourPlot1*cnLevelSelectionMode : automaticlevels
!
! set the formatting of the Max data value (which by default controls the
! formatting of all numeric labels in the plot)
! (See formatting documentation >>>> doesn't exist yet)
!
*cn04Work.ContourPlot1*cnMaxDataValueFormat : .4?5g
! turn fill on: with multiple patterns and line thicknesses but mono colored;
! adjust the fill scale
!
*cn04Work.ContourPlot1*cnFillOn : true
*cn04Work.ContourPlot1*cnMonoFillColor : true
*cn04Work.ContourPlot1*cnMonoFillPattern : false
*cn04Work.ContourPlot1*cnMonoLineThickness : false
*cn04Work.ContourPlot1*cnFillScaleF : 0.625
!
! use the 'computed' method of spacing labels and adjust spacing to every
! fourth contour line; make line labels horizontal
!
*cn04Work.ContourPlot1*cnLowLabelsOn : true
*cn04Work.ContourPlot1*cnHighLabelsOn : true
*cn04Work.ContourPlot1*cnLineLabelPlacementMode : computed
*cn04Work.ContourPlot1*cnLineLabelInterval : 4
*cn04Work.ContourPlot1*cnLineLabelAngleF : 0.0
*cn04Work.ContourPlot1*cnLineLabelTextHeightF : 0.008
!
! couple low and high label resources, and set perim on and the overlap mode
! set the high/low formatting to include trailing decimals
!
*cn04Work.ContourPlot1*cnLowUseHighLabelRes : true
*cn04Work.ContourPlot1*cnHighLabelPerimOn : true
*cn04Work.ContourPlot1*cnHighLowLabelOverlapMode : AdjustVpOmitOverHLandInfo
*cn04Work.ContourPlot1*cnHighLabelFormat : 0.4?5g
!
! adjust title height and distance from viewport
!
*cn04Work.ContourPlot1*tiMainFontHeightF : 0.010
*cn04Work.ContourPlot1*tiDeltaF : 1.25
!
! turn on the grid boundary
!
*cn04Work.ContourPlot1*cnGridBoundPerimOn : true
*cn04Work.ContourPlot1*cnGridBoundPerimThicknessF : 2.0
!
! position the information label inside the viewport; adjust height;
! set the info string to include the scale factor;
! set the info label format
!
*cn04Work.ContourPlot1*cnInfoLabelZone : 0
*cn04Work.ContourPlot1*cnInfoLabelOrthogonalPosF : .48
*cn04Work.ContourPlot1*cnInfoLabelParallelPosF : -.48
*cn04Work.ContourPlot1*cnInfoLabelJust : bottomleft
*cn04Work.ContourPlot1*cnInfoLabelTextHeightF : 0.009
*cn04Work.ContourPlot1*cnInfoLabelString : CONTOUR FROM $CMN$ TO $CMX$ BY $CIU$ (X $SFU$)
!
! >>>> (I don't understand why the exponent switch value needs to be 4 here -- 
! >>>> I would have thought 5 would be sufficient)
!
*cn04Work.ContourPlot1*cnInfoLabelFormat : .4?4^sg
!
! set the text emulation label resources
!
*cn04Work.TextItem1*txString : Emulation of example 'cpex02' using HLU library
*cn04Work.TextItem1*txJust : 2
*cn04Work.TextItem1*txFontHeightF : 0.012
*cn04Work.TextItem1*txPosXF : 0.01
*cn04Work.TextItem1*txPosYF : 0.005
