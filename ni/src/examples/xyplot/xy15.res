!
! xy15.res
!
! set resources that are the same for all the plots
!
*vpHeightF : 0.2
*vpWidthF : 0.8
*vpXF : 0.1

*amResizeNotify : true
*tmYLMode : Manual
*trYMinF : -1.25
*trYMaxF : 1.25
*tmYLTickSpacingF : 0.5
*tmYLTickStartF : -1.0
*tmYLTickEndF : 1.0
*tmYLLabelFontHeightF : 0.015
*tmXBLabelFontHeightF : 0.02
*tmYLMajorLengthF : 0.02
*tmYLMinorLengthF : 0.01
*tmXBMajorLengthF : 0.02
*tmXBMinorLengthF : 0.01

!
! The following resource assignments make use of the fact that the
! AnnoManager created to manage the each XyPlot annotation is 
! assigned the same name as the annotation it manages
!

*xy2.tmXBLabelsOn : False
*xy2.amSide : top
*xy2.amZone : 3
*xy2.amParallelPosF:  .5

*xy3.tmXBLabelsOn : False
*xy3.amSide : top
*xy3.amZone : 4
*xy3.amParallelPosF:  .5

*xy4.tmXBLabelsOn : False
*xy4.amSide : top
*xy4.amZone : 5
*xy4.amParallelPosF:  .5
