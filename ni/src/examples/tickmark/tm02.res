!
! Explicitly set resources for this example include:
!
*tm02Work.TickMarks.tmXBDataLeftF    : 0.
*tm02Work.TickMarks.tmXBDataRightF   : 1.
*tm02Work.TickMarks.tmYLDataTopF     : 0.
*tm02Work.TickMarks.tmYLDataBottomF  : 1.
*tm02Work.TickMarks.tmXBOn    : True
*tm02Work.TickMarks.tmYLOn    : True
!
! The full set of resources for this Object are commented below.
!
!  ==============================================================
!  The following resources relate to the composite TickMark Object.
!  ==============================================================
!
!       Colors are chosen from the HLU colormap.  Index choices are:
!
!     Index   Color     Index   Color    Index  Color       Index   Color
!
!       1-4   white      5-18  yellows   19-21  tans        22-24   oranges
!     29-32   magentas     34  lavender  40-50  reds           57   gray
!     60-66   cyans     70-80  greens    81-90  dark grns  91-100   blues
!   106-114   blacks
!
!       Color of the plot perimeter
! *tm02Work.TickMarks.tmBorderLineColor           : 1
!
!       Width of the plot perimeter
! *tm02Work.TickMarks.tmBorderThicknessF          : 2.0
!
!       Number of scale digits at which scientific notation switches on.
! *tm02Work.TickMarks.tmSciNoteCutoff             : 6
!
!       Use the bottom tick scaling on the top axis
! *tm02Work.TickMarks.tmXUseBottom                : True
!
!       Use the left tick scaling on the right axis
! *tm02Work.TickMarks.tmYUseLeft                  : True
!
!
!  ==============================================================
!  The following resources relate to the viewport on the drawing
!  canvas.
!  ==============================================================
!
!       Height in NDC (0. to 1.) of the viewport
! *tm02Work.TickMarks.vpHeightF                   : 0.8
!
!       Switch to allow the plot X to Y aspect ratio to remain fixed
!       when resizing or moving the object.
! *tm02Work.TickMarks.vpKeepAspect                : False
!
!       Save this graphical object as a GKS segment
! *tm02Work.TickMarks.vpUseSegments               : False
!
!       Width in NDC (0. to 1.) of the viewport
! *tm02Work.TickMarks.vpWidthF                    : 0.8
!
!       NDC (0. to 1.) X location of the upper left corner of the plot
! *tm02Work.TickMarks.vpXF                        : 0.1
!
!       NDC (0. to 1.) Y location of the upper left corner of the plot
! *tm02Work.TickMarks.vpYF                        : 0.9
!
!
!  ==============================================================
!  The following resources relate to major and minor grid lines
!  between the X axes (top to bottom), or Y axes (left to right).
!  ==============================================================
!
!       Quick switch to turn major grid lines on/off
! *tm02Work.TickMarks.tmXMajorGrid                : False
! *tm02Work.TickMarks.tmYMajorGrid                : False
!
!       Color of major grid lines (GKS indexed)
! *tm02Work.TickMarks.tmXMajorGridLineColor       : 1
! *tm02Work.TickMarks.tmYMajorGridLineColor       : 1
!
!       Dash pattern for major grid lines
! *tm02Work.TickMarks.tmXMajorGridLineDashPattern : 0
! *tm02Work.TickMarks.tmYMajorGridLineDashPattern : 0
!
!       Width of major grid lines
! *tm02Work.TickMarks.tmXMajorGridThicknessF      : 2.0
! *tm02Work.TickMarks.tmYMajorGridThicknessF      : 2.0
!
!       Quick switch to turn minor grid lines on/off
! *tm02Work.TickMarks.tmXMinorGrid                : False
! *tm02Work.TickMarks.tmYMinorGrid                : False
!
!       Color of minor grid lines (GKS indexed)
! *tm02Work.TickMarks.tmXMinorGridLineColor       : 1
! *tm02Work.TickMarks.tmYMinorGridLineColor       : 1
!
!       Dash pattern for minor grid lines
! *tm02Work.TickMarks.tmXMinorGridLineDashPattern : 0
! *tm02Work.TickMarks.tmYMinorGridLineDashPattern : 0
!
!       Width of minor grid lines
! *tm02Work.TickMarks.tmXMinorGridThicknessF      : 1.0
! *tm02Work.TickMarks.tmYMinorGridThicknessF      : 1.0
!
!  ==============================================================
!  The following resources relate to the four scaleable axes:
!  X axes (top and bottom), or Y axes (left and right).
!
!  Note that tmXUseBottom can be used to slave the top axis to the
!  resources set for the bottom axis, and tmYUseLeft can be used
!  to slave the right axis to the resources set for the left axis.
!  ==============================================================
!
!  -----------------
!  Tick mark scaling
!  -----------------
!
!       The mode for drawing tick marks on an axis(AUTOMATIC,EXPLICIT,MANUAL)
!
!    Resources required by NhlAUTOMATIC for XB axis:
!       tmXBDataLeftF and tmXBDataRightF
!
!    Resources required by NhlMANUAL for XB axis:
!       tmXBDataLeftF, tmXBDataRightF, tmXBTickStartF,
!       tmXBTickEndF, and tmXBTickSpacingF
!
!    Resources required by NhlEXPLICIT for XB axis:
!       tmXBDataLeftF, tmXBDataRightF, tmXBValues, and  tmXBLabels
!
!    Similar requirements apply to the other 3 axes XT, YL, and YR.
!
! *tm02Work.TickMarks.tmXBMode                    : AUTOMATIC
! *tm02Work.TickMarks.tmXTMode                    : AUTOMATIC
! *tm02Work.TickMarks.tmYLMode                    : AUTOMATIC
! *tm02Work.TickMarks.tmYRMode                    : AUTOMATIC
!
!       The maximum number of major ticks in AUTOMATIC mode
! *tm02Work.TickMarks.tmXBMaxTicks                : 7
! *tm02Work.TickMarks.tmXTMaxTicks                : 7
! *tm02Work.TickMarks.tmYLMaxTicks                : 7
! *tm02Work.TickMarks.tmYRMaxTicks                : 7
!
!       Use automatic calculation of scaling increments on the indicated axis
! *tm02Work.TickMarks.tmXBAutoPrecision           : True
! *tm02Work.TickMarks.tmXTAutoPrecision           : True
! *tm02Work.TickMarks.tmYLAutoPrecision           : True
! *tm02Work.TickMarks.tmYRAutoPrecision           : True
!
!       Draw the indicated border of the grid perimeter
! *tm02Work.TickMarks.tmXBBorderOn                : True
! *tm02Work.TickMarks.tmXTBorderOn                : True
! *tm02Work.TickMarks.tmYLBorderOn                : True
! *tm02Work.TickMarks.tmYRBorderOn                : True
!
!       Grid scale style for the indicated axis.
!       Options are LINEAR, LOG, IRREGULAR, TIME, and GEOGRAPHIC.
! *tm02Work.TickMarks.tmXBStyle                   : LINEAR
! *tm02Work.TickMarks.tmXTStyle                   : LINEAR
! *tm02Work.TickMarks.tmYLStyle                   : LINEAR
! *tm02Work.TickMarks.tmYRStyle                   : LINEAR
!
!       NDC (0. to 1.) location of the left tick (X) or bottom tick (Y)
! *tm02Work.TickMarks.tmXBTickStartF              : 0.0
! *tm02Work.TickMarks.tmXTTickStartF              : 0.0
! *tm02Work.TickMarks.tmYLTickStartF              : 0.0
! *tm02Work.TickMarks.tmYRTickStartF              : 0.0
!
!       NDC (0. to 1.) location of the right tick (X) or top tick (Y)
! *tm02Work.TickMarks.tmXBTickEndF                : 0.0
! *tm02Work.TickMarks.tmXTTickEndF                : 0.0
! *tm02Work.TickMarks.tmYLTickEndF                : 0.0
! *tm02Work.TickMarks.tmYRTickEndF                : 0.0
!
!       NDC spacing between major tick marks along the indicated axis
!       Used with Mode = MANUAL only.
! *tm02Work.TickMarks.tmXBTickSpacingF            : 0.0
! *tm02Work.TickMarks.tmXTTickSpacingF            : 0.0
! *tm02Work.TickMarks.tmYLTickSpacingF            : 0.0
! *tm02Work.TickMarks.tmYRTickSpacingF            : 0.0
!
!       Currently unused
! *tm02Work.TickMarks.tmXBSpacingType             : 0
! *tm02Work.TickMarks.tmXTSpacingType             : 0
! *tm02Work.TickMarks.tmYLSpacingType             : 0
! *tm02Work.TickMarks.tmYRSpacingType             : 0
!
!       Scale value of data at the left of the indicated axis
! *tm02Work.TickMarks.tmXBDataLeftF               : 0.0
! *tm02Work.TickMarks.tmXTDataLeftF               : 0.0
! *tm02Work.TickMarks.tmYLDataLeftF               : 0.0
! *tm02Work.TickMarks.tmYRDataLeftF               : 0.0
!
!       Scale value of data at the right of the indicated axis
! *tm02Work.TickMarks.tmXBDataRightF              : 0.0
! *tm02Work.TickMarks.tmXTDataRightF              : 0.0
! *tm02Work.TickMarks.tmYLDataRightF              : 0.0
! *tm02Work.TickMarks.tmYRDataRightF              : 0.0
!
!       The number of digits precision in AUTOMATIC mode scaling.
! *tm02Work.TickMarks.tmXBPrecision               : 4
! *tm02Work.TickMarks.tmXTPrecision               : 4
! *tm02Work.TickMarks.tmYLPrecision               : 4
! *tm02Work.TickMarks.tmYRPrecision               : 4
!
!       Spline interpolation tension for data along an IRREGULAR style axis.
! *tm02Work.TickMarks.tmXBIrrTensionF             : 2.0
! *tm02Work.TickMarks.tmXTIrrTensionF             : 2.0
! *tm02Work.TickMarks.tmYLIrrTensionF             : 2.0
! *tm02Work.TickMarks.tmYRIrrTensionF             : 2.0
!
!       An array of irregular values to be placed linearly along an axis.
!       Must be monotonically increasing.  For IRREGULAR style only.
! *tm02Work.TickMarks.tmXBIrregularPoints         : Null
! *tm02Work.TickMarks.tmXTIrregularPoints         : Null
! *tm02Work.TickMarks.tmYLIrregularPoints         : Null
! *tm02Work.TickMarks.tmYRIrregularPoints         : Null
!
!       An array of labels when Mode is EXPLICIT
! *tm02Work.TickMarks.tmXBLabels                  : Null
! *tm02Work.TickMarks.tmXTLabels                  : Null
! *tm02Work.TickMarks.tmYLLabels                  : Null
! *tm02Work.TickMarks.tmYRLabels                  : Null
!
!       An array of major tick values when Mode is EXPLICIT
! *tm02Work.TickMarks.tmXBValues                  : Null
! *tm02Work.TickMarks.tmXTValues                  : Null
! *tm02Work.TickMarks.tmYLValues                  : Null
! *tm02Work.TickMarks.tmYRValues                  : Null
!
!  --------------------------
!  Labels at major tick marks
!  --------------------------
!
!       Quick switch for turning labels on/off
! *tm02Work.TickMarks.tmXBLabelsOn                : True
! *tm02Work.TickMarks.tmXTLabelsOn                : True
! *tm02Work.TickMarks.tmYLLabelsOn                : True
! *tm02Work.TickMarks.tmYRLabelsOn                : True
!
!       Angle at which the indicated axis scale labels are drawn
! *tm02Work.TickMarks.tmXBLabelAngleF             : 0.0
! *tm02Work.TickMarks.tmXTLabelAngleF             : 0.0
! *tm02Work.TickMarks.tmYLLabelAngleF             : 0.0
! *tm02Work.TickMarks.tmYRLabelAngleF             : 0.0
!
!       Spacing between the scale values and the grid axis
! *tm02Work.TickMarks.tmXBLabelDeltaF             : 0.0
! *tm02Work.TickMarks.tmXTLabelDeltaF             : 0.0
! *tm02Work.TickMarks.tmYLLabelDeltaF             : 0.0
! *tm02Work.TickMarks.tmYRLabelDeltaF             : 0.0
!
!       Direction in which scale labels are drawn (UP,DOWN,ACROSS)
!       Normal direction is across for all axes.
! *tm02Work.TickMarks.tmXBLabelDirection          : ACROSS
! *tm02Work.TickMarks.tmXTLabelDirection          : ACROSS
! *tm02Work.TickMarks.tmYLLabelDirection          : ACROSS
! *tm02Work.TickMarks.tmYRLabelDirection          : ACROSS
!
!       The font style to use for labels
! *tm02Work.TickMarks.tmXBLabelFont               : 0
! *tm02Work.TickMarks.tmXTLabelFont               : 0
! *tm02Work.TickMarks.tmYLLabelFont               : 0
! *tm02Work.TickMarks.tmYRLabelFont               : 0
!
!       The height to width aspect ratio of label characters
! *tm02Work.TickMarks.tmXBLabelFontAspectF        : 1.3125
! *tm02Work.TickMarks.tmXTLabelFontAspectF        : 1.3125
! *tm02Work.TickMarks.tmYLLabelFontAspectF        : 1.3125
! *tm02Work.TickMarks.tmYRLabelFontAspectF        : 1.3125
!
!       The color for label text
! *tm02Work.TickMarks.tmXBLabelFontColor          : 1
! *tm02Work.TickMarks.tmXTLabelFontColor          : 1
! *tm02Work.TickMarks.tmYLLabelFontColor          : 1
! *tm02Work.TickMarks.tmYRLabelFontColor          : 1
!
!       The NDC (0. to 1.) height of the label text
! *tm02Work.TickMarks.tmXBLabelFontHeightF        : .02
! *tm02Work.TickMarks.tmXTLabelFontHeightF        : .02
! *tm02Work.TickMarks.tmYLLabelFontHeightF        : .02
! *tm02Work.TickMarks.tmYRLabelFontHeightF        : .02
!
!       How label text is justified in a text extent rectangle
! *tm02Work.TickMarks.tmXBLabelJust               : 3
! *tm02Work.TickMarks.tmXTLabelJust               : 3
! *tm02Work.TickMarks.tmYLLabelJust               : 3
! *tm02Work.TickMarks.tmYRLabelJust               : 3
!
!       The stride between labeled major ticks
! *tm02Work.TickMarks.tmXBLabelStride             : 0
! *tm02Work.TickMarks.tmXTLabelStride             : 0
! *tm02Work.TickMarks.tmYLLabelStride             : 0
! *tm02Work.TickMarks.tmYRLabelStride             : 0
!
!  ------------------------
!  Appearance of tick marks
!  ------------------------
!
!       Quick switch for turning indicated ticks on/off
! *tm02Work.TickMarks.tmXBOn                      : True
! *tm02Work.TickMarks.tmXTOn                      : True
! *tm02Work.TickMarks.tmYLOn                      : True
! *tm02Work.TickMarks.tmYROn                      : True
!
!       The number of minor tick marks per major tick mark
! *tm02Work.TickMarks.tmXBMinorPerMajor           : 3
! *tm02Work.TickMarks.tmXTMinorPerMajor           : 3
! *tm02Work.TickMarks.tmYLMinorPerMajor           : 3
! *tm02Work.TickMarks.tmYRMinorPerMajor           : 3
!
!       The length of major tick marks
! *tm02Work.TickMarks.tmXBMajorLengthF            : .02
! *tm02Work.TickMarks.tmXTMajorLengthF            : .02
! *tm02Work.TickMarks.tmYLMajorLengthF            : .02
! *tm02Work.TickMarks.tmYRMajorLengthF            : .02
!
!       The color of major tick marks
! *tm02Work.TickMarks.tmXBMajorLineColor          : 1
! *tm02Work.TickMarks.tmXTMajorLineColor          : 1
! *tm02Work.TickMarks.tmYLMajorLineColor          : 1
! *tm02Work.TickMarks.tmYRMajorLineColor          : 1
!
!       The length of major tick marks outside the border
! *tm02Work.TickMarks.tmXBMajorOutwardLengthF     : 0.0
! *tm02Work.TickMarks.tmXTMajorOutwardLengthF     : 0.0
! *tm02Work.TickMarks.tmYLMajorOutwardLengthF     : 0.0
! *tm02Work.TickMarks.tmYRMajorOutwardLengthF     : 0.0
!
!       The width of major tick marks
! *tm02Work.TickMarks.tmXBMajorThicknessF         : 2.0
! *tm02Work.TickMarks.tmXTMajorThicknessF         : 2.0
! *tm02Work.TickMarks.tmYLMajorThicknessF         : 2.0
! *tm02Work.TickMarks.tmYRMajorThicknessF         : 2.0
!
!       The length of minor tick marks
! *tm02Work.TickMarks.tmXBMinorLengthF            : .01
! *tm02Work.TickMarks.tmXTMinorLengthF            : .01
! *tm02Work.TickMarks.tmYLMinorLengthF            : .01
! *tm02Work.TickMarks.tmYRMinorLengthF            : .01
!
!       The color of minor tick marks
! *tm02Work.TickMarks.tmXBMinorLineColor          : 1
! *tm02Work.TickMarks.tmXTMinorLineColor          : 1
! *tm02Work.TickMarks.tmYLMinorLineColor          : 1
! *tm02Work.TickMarks.tmYRMinorLineColor          : 1
!
!       Quick switch for turning indicated minor ticks on/off
! *tm02Work.TickMarks.tmXBMinorOn                 : True
! *tm02Work.TickMarks.tmXTMinorOn                 : True
! *tm02Work.TickMarks.tmYLMinorOn                 : True
! *tm02Work.TickMarks.tmYRMinorOn                 : True
!
!       The length of minor tick marks outside the border
! *tm02Work.TickMarks.tmXBMinorOutwardLengthF     : 0.0
! *tm02Work.TickMarks.tmXTMinorOutwardLengthF     : 0.0
! *tm02Work.TickMarks.tmYLMinorOutwardLengthF     : 0.0
! *tm02Work.TickMarks.tmYRMinorOutwardLengthF     : 0.0
!
!       The width of minor tick marks
! *tm02Work.TickMarks.tmXBMinorThicknessF         : 1.0
! *tm02Work.TickMarks.tmXTMinorThicknessF         : 1.0
! *tm02Work.TickMarks.tmYLMinorThicknessF         : 1.0
! *tm02Work.TickMarks.tmYRMinorThicknessF         : 1.0
