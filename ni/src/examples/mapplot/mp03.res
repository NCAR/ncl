! 
! mp03 resource file
!
! no tick marks
!
*mp03Work.Contour1*ovTickMarkDisplayMode : nocreate
*mp03Work.Map1*ovTickMarkDisplayMode : nocreate
!
! create a labelbar
!
*mp03Work.Contour1*ovLabelBarDisplayMode : always
*mp03Work.Contour1*lbOrientation : horizontal
*mp03Work.Contour1*ovLabelBarSide : bottom
*mp03Work.Contour1*ovLabelBarHeightF : 0.8
*mp03Work.Contour1*ovLabelBarHeightF : 0.1
*mp03Work.Contour1*ovLabelBarOrthogonalOffsetF : 0.1
*mp03Work.Contour1*lbDrawTitle : 0
*mp03Work.Contour1*lbDrawPerim : 0
*mp03Work.Contour1*lbAutoManage : false
*mp03Work.Contour1*lbLabelFontHeightF : 0.01
*mp03Work.Contour1*lbLabelAngleF  : -30.0
*mp03Work.Contour1*lbLabelOffsetF : 0.05
!
! set the maximum number of levels high enough to allow the desired level
! spacing, and choose the automatic level selection mode
!
*mp03Work.Contour1*cnMaxLevelCount : 22
*mp03Work.Contour1*cnLevelSelectionMode : automatic
!
! set the map labels and grid off
!
*mp03Work.Map1*mpGridAndLimbOn  : false
*mp03Work.Map1*mpLabelsOn  : false
!
! set the formatting of the Max data value (which by default controls the
! formatting of all numeric labels in the plot)
! (See formatting documentation >>>> doesn't exist yet)
!
*mp03Work.Contour1*cnMaxDataValueFormat : .4?5g
!
! turn fill on: with multiple patterns and line thicknesses but mono colored;
! adjust the fill scale
!
*mp03Work.Contour1*cnFillOn : true
!
! line labels off
!
*mp03Work.Contour1*cnLineLabelsOn : false
!
! couple low and high label resources, and set perim on and the overlap mode
! set the high/low formatting to include trailing decimals
!
*mp03Work.Contour1*cnLowUseHighLabelRes : true
*mp03Work.Contour1*cnHighLabelPerim : true
*mp03Work.Contour1*cnHighLowLabelOverlapMode : AdjustVpOmitOverHLandInfo
*mp03Work.Contour1*cnHighLabelFormat : 0.4?5g
