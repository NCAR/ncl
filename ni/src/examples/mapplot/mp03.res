! 
! mp03 resource file
!
! set the default viewport; note that if you do not want the size of
! the labelbar belonging to the Contour to be adjusted when it is 
! overlaid on the MapPlot, the Contour's view should be set to the same
! size as the MapPlot's.
!
*mp03Work.Contour1.vpYF: 0.775
*mp03Work.Contour1.vpHeightF: 0.45
*mp03Work.Contour1.vpXF: 0.1
*mp03Work.Contour1.vpWidthF: 0.8
*mp03Work.Map1.vpYF: 0.775
*mp03Work.Map1.vpHeightF: 0.45
*mp03Work.Map1.vpXF: 0.1
*mp03Work.Map1.vpWidthF: 0.8
!
! no tick marks
!
*mp03Work.Contour1*pmTickMarkDisplayMode : nocreate
*mp03Work.Map1*pmTickMarkDisplayMode : nocreate
!
! display a labelbar
!
*mp03Work.Contour1*pmLabelBarDisplayMode : always
*mp03Work.Contour1*lbOrientation : horizontal
*mp03Work.Contour1*pmLabelBarSide : bottom
*mp03Work.Contour1*pmLabelBarWidthF : 0.8
*mp03Work.Contour1*pmLabelBarHeightF : 0.25
*mp03Work.Contour1*pmLabelBarOrthogonalOffsetF : 0.1
*mp03Work.Contour1*lbTitleOn : false
*mp03Work.Contour1*lbPerimOn : false
*mp03Work.Contour1*lbLeftMarginF : 0.0
*mp03Work.Contour1*lbRightMarginF : 0.0

*mp03Work.Contour1*lbAutoManage : false
*mp03Work.Contour1*lbLabelFontHeightF : 0.015
*mp03Work.Contour1*lbLabelAngleF  : -30.0
*mp03Work.Contour1*lbLabelOffsetF : 0.05
!
! set the maximum number of levels high enough to allow the desired level
! spacing, and choose the automatic level selection mode
!
*mp03Work.Contour1*cnMaxLevelCount : 22
*mp03Work.Contour1*cnLevelSelectionMode : automaticlevels
!
! set the map labels and grid off
!
*mp03Work.Map1*mpGridAndLimbOn  : false
*mp03Work.Map1*mpLabelsOn  : false
*mpPerimOn  : true
!
! set the formatting of the Max data value (which by default controls the
! formatting of all numeric labels in the plot)
!
*mp03Work.Contour1*cnMaxDataValueFormat : .4?5g
!
! turn fill on: 
!
*mp03Work.Contour1*cnFillOn : true
!
! line labels off
! however, since the strings that would appear for the line labels are
! used for the LabelBar labels set the string format to include trailing
! decimals.
!
*mp03Work.Contour1*cnLineLabelsOn : false
*mp03Work.Contour1*cnLineLabelFormat : 0.4?5g
!
! couple low and high label resources, and set perim on and the overlap mode
! set the high/low formatting to include trailing decimals
!
*mp03Work.Contour1*cnHighLabelsOn : true
*mp03Work.Contour1*cnLowUseHighLabelRes : true
*mp03Work.Contour1*cnHighLabelPerimOn : true
*mp03Work.Contour1*cnHighLowLabelOverlapMode : AdjustVpOmitOverHLandInfo
*mp03Work.Contour1*cnHighLabelFormat : 0.4?5g
!
! Set up the colors we want to use from the 
! "example" color map.
!
*mp03Work*mpLandFillColor:  58
*wkColorMap:   example

*cnFillColors:  (/2,5,11,16,19,24,36,39,43,51,53,56,61,66,69,71,81,86,91,101/)
