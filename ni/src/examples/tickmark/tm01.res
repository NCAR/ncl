!
! Example:  This resource file shows all TickMark resources and
!           their default values.
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
! tm01.tm01Work.TickMarks.tmBorderLineColor           : 1
!
!       Width of the plot perimeter
! tm01.tm01Work.TickMarks.tmBorderThicknessF          : 2.0
!
!       Number of scale digits at which scientific notation switches on.
! tm01.tm01Work.TickMarks.tmSciNoteCutoff             : 6
!
!       Use the bottom tick scaling on the top axis
! tm01.tm01Work.TickMarks.tmXUseBottom                : True
!
!       Use the left tick scaling on the right axis
! tm01.tm01Work.TickMarks.tmYUseLeft                  : True
!
!
!  ==============================================================
!  The following resources relate to the viewport on the drawing
!  canvas.
!  ==============================================================
!
!       Height in NDC (0. to 1.) of the viewport
! tm01.tm01Work.TickMarks.vpHeightF                   : 0.8
!
!       Switch to allow the plot X to Y aspect ratio to remain fixed
!       when resizing or moving the object.
! tm01.tm01Work.TickMarks.vpKeepAspect                : False
!
!       Save this graphical object as a GKS segment
! tm01.tm01Work.TickMarks.vpUseSegments               : False
!
!       Width in NDC (0. to 1.) of the viewport
! tm01.tm01Work.TickMarks.vpWidthF                    : 0.8
!
!       NDC (0. to 1.) X location of the upper left corner of the plot
! tm01.tm01Work.TickMarks.vpXF                        : 0.1
!
!       NDC (0. to 1.) Y location of the upper left corner of the plot
! tm01.tm01Work.TickMarks.vpYF                        : 0.9
!
!
!  ==============================================================
!  The following resources relate to major and minor grid lines
!  between the X axes (top to bottom), or Y axes (left to right).
!  ==============================================================
!
!       Quick switch to turn major grid lines on/off
! tm01.tm01Work.TickMarks.tmXMajorGrid                : False
! tm01.tm01Work.TickMarks.tmYMajorGrid                : False
!
!       Color of major grid lines (GKS indexed)
! tm01.tm01Work.TickMarks.tmXMajorGridLineColor       : 1
! tm01.tm01Work.TickMarks.tmYMajorGridLineColor       : 1
!
!       Dash pattern for major grid lines
! tm01.tm01Work.TickMarks.tmXMajorGridLineDashPattern : 0
! tm01.tm01Work.TickMarks.tmYMajorGridLineDashPattern : 0
!
!       Width of major grid lines
! tm01.tm01Work.TickMarks.tmXMajorGridThicknessF      : 2.0
! tm01.tm01Work.TickMarks.tmYMajorGridThicknessF      : 2.0
!
!       Quick switch to turn minor grid lines on/off
! tm01.tm01Work.TickMarks.tmXMinorGrid                : False
! tm01.tm01Work.TickMarks.tmYMinorGrid                : False
!
!       Color of minor grid lines (GKS indexed)
! tm01.tm01Work.TickMarks.tmXMinorGridLineColor       : 1
! tm01.tm01Work.TickMarks.tmYMinorGridLineColor       : 1
!
!       Dash pattern for minor grid lines
! tm01.tm01Work.TickMarks.tmXMinorGridLineDashPattern : 0
! tm01.tm01Work.TickMarks.tmYMinorGridLineDashPattern : 0
!
!       Width of minor grid lines
! tm01.tm01Work.TickMarks.tmXMinorGridThicknessF      : 1.0
! tm01.tm01Work.TickMarks.tmYMinorGridThicknessF      : 1.0
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
! tm01.tm01Work.TickMarks.tmXBMode                    : AUTOMATIC
! tm01.tm01Work.TickMarks.tmXTMode                    : AUTOMATIC
! tm01.tm01Work.TickMarks.tmYLMode                    : AUTOMATIC
! tm01.tm01Work.TickMarks.tmYRMode                    : AUTOMATIC
!
!       The maximum number of major ticks in AUTOMATIC mode
! tm01.tm01Work.TickMarks.tmXBMaxTicks                : 7
! tm01.tm01Work.TickMarks.tmXTMaxTicks                : 7
! tm01.tm01Work.TickMarks.tmYLMaxTicks                : 7
! tm01.tm01Work.TickMarks.tmYRMaxTicks                : 7
!
!       Use automatic calculation of scaling increments on the indicated axis
! tm01.tm01Work.TickMarks.tmXBAutoPrecision           : True
! tm01.tm01Work.TickMarks.tmXTAutoPrecision           : True
! tm01.tm01Work.TickMarks.tmYLAutoPrecision           : True
! tm01.tm01Work.TickMarks.tmYRAutoPrecision           : True
!
!       Draw the indicated border of the grid perimeter
! tm01.tm01Work.TickMarks.tmXBBorderOn                : True
! tm01.tm01Work.TickMarks.tmXTBorderOn                : True
! tm01.tm01Work.TickMarks.tmYLBorderOn                : True
! tm01.tm01Work.TickMarks.tmYRBorderOn                : True
!
!       Grid scale style for the indicated axis.
!       Options are LINEAR, LOG, IRREGULAR, TIME, and GEOGRAPHIC.
! tm01.tm01Work.TickMarks.tmXBStyle                   : LINEAR
! tm01.tm01Work.TickMarks.tmXTStyle                   : LINEAR
! tm01.tm01Work.TickMarks.tmYLStyle                   : LINEAR
! tm01.tm01Work.TickMarks.tmYRStyle                   : LINEAR
!
!       NDC (0. to 1.) location of the left tick (X) or bottom tick (Y)
! tm01.tm01Work.TickMarks.tmXBTickStartF              : 0.0
! tm01.tm01Work.TickMarks.tmXTTickStartF              : 0.0
! tm01.tm01Work.TickMarks.tmYLTickStartF              : 0.0
! tm01.tm01Work.TickMarks.tmYRTickStartF              : 0.0
!
!       NDC (0. to 1.) location of the right tick (X) or top tick (Y)
! tm01.tm01Work.TickMarks.tmXBTickEndF                : 0.0
! tm01.tm01Work.TickMarks.tmXTTickEndF                : 0.0
! tm01.tm01Work.TickMarks.tmYLTickEndF                : 0.0
! tm01.tm01Work.TickMarks.tmYRTickEndF                : 0.0
!
!       NDC spacing between major tick marks along the indicated axis
!       Used with Mode = MANUAL only.
! tm01.tm01Work.TickMarks.tmXBTickSpacingF            : 0.0
! tm01.tm01Work.TickMarks.tmXTTickSpacingF            : 0.0
! tm01.tm01Work.TickMarks.tmYLTickSpacingF            : 0.0
! tm01.tm01Work.TickMarks.tmYRTickSpacingF            : 0.0
!
!       Currently unused
! tm01.tm01Work.TickMarks.tmXBSpacingType             : 0
! tm01.tm01Work.TickMarks.tmXTSpacingType             : 0
! tm01.tm01Work.TickMarks.tmYLSpacingType             : 0
! tm01.tm01Work.TickMarks.tmYRSpacingType             : 0
!
!       Scale value of data at the left of the indicated axis
! tm01.tm01Work.TickMarks.tmXBDataLeftF               : 0.0
! tm01.tm01Work.TickMarks.tmXTDataLeftF               : 0.0
!
!       Scale value of data at the right of the indicated axis
! tm01.tm01Work.TickMarks.tmXBDataRightF              : 10.0
! tm01.tm01Work.TickMarks.tmXTDataRightF              : 0.0
!
!       Scale value of data at the top of the indicated axis
! tm01.tm01Work.TickMarks.tmYLDataTopF                : 0.0
! tm01.tm01Work.TickMarks.tmYRDataTopF                : 0.0
!
!       Scale value of data at the bottom of the indicated axis
! tm01.tm01Work.TickMarks.tmYLDataBottomF             : 0.0
! tm01.tm01Work.TickMarks.tmYRDataBottomF             : 0.0
!
!       The number of digits precision in AUTOMATIC mode scaling.
! tm01.tm01Work.TickMarks.tmXBPrecision               : 4
! tm01.tm01Work.TickMarks.tmXTPrecision               : 4
! tm01.tm01Work.TickMarks.tmYLPrecision               : 4
! tm01.tm01Work.TickMarks.tmYRPrecision               : 4
!
!       Spline interpolation tension for data along an IRREGULAR style axis.
! tm01.tm01Work.TickMarks.tmXBIrrTensionF             : 2.0
! tm01.tm01Work.TickMarks.tmXTIrrTensionF             : 2.0
! tm01.tm01Work.TickMarks.tmYLIrrTensionF             : 2.0
! tm01.tm01Work.TickMarks.tmYRIrrTensionF             : 2.0
!
!       An array of irregular values to be placed linearly along an axis.
!       Must be monotonically increasing.  For IRREGULAR style only.
! tm01.tm01Work.TickMarks.tmXBIrregularPoints         : Null
! tm01.tm01Work.TickMarks.tmXTIrregularPoints         : Null
! tm01.tm01Work.TickMarks.tmYLIrregularPoints         : Null
! tm01.tm01Work.TickMarks.tmYRIrregularPoints         : Null
!
!       An array of labels when Mode is EXPLICIT
! tm01.tm01Work.TickMarks.tmXBLabels                  : Null
! tm01.tm01Work.TickMarks.tmXTLabels                  : Null
! tm01.tm01Work.TickMarks.tmYLLabels                  : Null
! tm01.tm01Work.TickMarks.tmYRLabels                  : Null
!
!       An array of major tick values when Mode is EXPLICIT
! tm01.tm01Work.TickMarks.tmXBValues                  : Null
! tm01.tm01Work.TickMarks.tmXTValues                  : Null
! tm01.tm01Work.TickMarks.tmYLValues                  : Null
! tm01.tm01Work.TickMarks.tmYRValues                  : Null
!
!  --------------------------
!  Labels at major tick marks
!  --------------------------
!
!       Quick switch for turning labels on/off
! tm01.tm01Work.TickMarks.tmXBLabelsOn                : True
! tm01.tm01Work.TickMarks.tmXTLabelsOn                : True
! tm01.tm01Work.TickMarks.tmYLLabelsOn                : True
! tm01.tm01Work.TickMarks.tmYRLabelsOn                : True
!
!       Angle at which the indicated axis scale labels are drawn
! tm01.tm01Work.TickMarks.tmXBLabelAngleF             : 0.0
! tm01.tm01Work.TickMarks.tmXTLabelAngleF             : 0.0
! tm01.tm01Work.TickMarks.tmYLLabelAngleF             : 0.0
! tm01.tm01Work.TickMarks.tmYRLabelAngleF             : 0.0
!
!       Spacing between the scale values and the grid axis
! tm01.tm01Work.TickMarks.tmXBLabelDeltaF             : 0.0
! tm01.tm01Work.TickMarks.tmXTLabelDeltaF             : 0.0
! tm01.tm01Work.TickMarks.tmYLLabelDeltaF             : 0.0
! tm01.tm01Work.TickMarks.tmYRLabelDeltaF             : 0.0
!
!       Direction in which scale labels are drawn (UP,DOWN,ACROSS)
!       Normal direction is across for all axes.
! tm01.tm01Work.TickMarks.tmXBLabelDirection          : ACROSS
! tm01.tm01Work.TickMarks.tmXTLabelDirection          : ACROSS
! tm01.tm01Work.TickMarks.tmYLLabelDirection          : ACROSS
! tm01.tm01Work.TickMarks.tmYRLabelDirection          : ACROSS
!
!       The font style to use for labels
! tm01.tm01Work.TickMarks.tmXBLabelFont               : 0
! tm01.tm01Work.TickMarks.tmXTLabelFont               : 0
! tm01.tm01Work.TickMarks.tmYLLabelFont               : 0
! tm01.tm01Work.TickMarks.tmYRLabelFont               : 0
!
!       The height to width aspect ratio of label characters
! tm01.tm01Work.TickMarks.tmXBLabelFontAspectF        : 1.3125
! tm01.tm01Work.TickMarks.tmXTLabelFontAspectF        : 1.3125
! tm01.tm01Work.TickMarks.tmYLLabelFontAspectF        : 1.3125
! tm01.tm01Work.TickMarks.tmYRLabelFontAspectF        : 1.3125
!
!       The color for label text
! tm01.tm01Work.TickMarks.tmXBLabelFontColor          : 1
! tm01.tm01Work.TickMarks.tmXTLabelFontColor          : 1
! tm01.tm01Work.TickMarks.tmYLLabelFontColor          : 1
! tm01.tm01Work.TickMarks.tmYRLabelFontColor          : 1
!
!       The NDC (0. to 1.) height of the label text
! tm01.tm01Work.TickMarks.tmXBLabelFontHeightF        : .02
! tm01.tm01Work.TickMarks.tmXTLabelFontHeightF        : .02
! tm01.tm01Work.TickMarks.tmYLLabelFontHeightF        : .02
! tm01.tm01Work.TickMarks.tmYRLabelFontHeightF        : .02
!
!       How label text is justified in a text extent rectangle
! tm01.tm01Work.TickMarks.tmXBLabelJust               : 3
! tm01.tm01Work.TickMarks.tmXTLabelJust               : 3
! tm01.tm01Work.TickMarks.tmYLLabelJust               : 3
! tm01.tm01Work.TickMarks.tmYRLabelJust               : 3
!
!       The stride between labeled major ticks
! tm01.tm01Work.TickMarks.tmXBLabelStride             : 0
! tm01.tm01Work.TickMarks.tmXTLabelStride             : 0
! tm01.tm01Work.TickMarks.tmYLLabelStride             : 0
! tm01.tm01Work.TickMarks.tmYRLabelStride             : 0
!
!  ------------------------
!  Appearance of tick marks
!  ------------------------
!
!       Quick switch for turning indicated ticks on/off
! tm01.tm01Work.TickMarks.tmXBOn                      : True
! tm01.tm01Work.TickMarks.tmXTOn                      : True
! tm01.tm01Work.TickMarks.tmYLOn                      : True
! tm01.tm01Work.TickMarks.tmYROn                      : True
!
!       The number of minor tick marks per major tick mark
! tm01.tm01Work.TickMarks.tmXBMinorPerMajor           : 3
! tm01.tm01Work.TickMarks.tmXTMinorPerMajor           : 3
! tm01.tm01Work.TickMarks.tmYLMinorPerMajor           : 3
! tm01.tm01Work.TickMarks.tmYRMinorPerMajor           : 3
!
!       The length of major tick marks
! tm01.tm01Work.TickMarks.tmXBMajorLengthF            : .02
! tm01.tm01Work.TickMarks.tmXTMajorLengthF            : .02
! tm01.tm01Work.TickMarks.tmYLMajorLengthF            : .02
! tm01.tm01Work.TickMarks.tmYRMajorLengthF            : .02
!
!       The color of major tick marks
! tm01.tm01Work.TickMarks.tmXBMajorLineColor          : 1
! tm01.tm01Work.TickMarks.tmXTMajorLineColor          : 1
! tm01.tm01Work.TickMarks.tmYLMajorLineColor          : 1
! tm01.tm01Work.TickMarks.tmYRMajorLineColor          : 1
!
!       The length of major tick marks outside the border
! tm01.tm01Work.TickMarks.tmXBMajorOutwardLengthF     : 0.0
! tm01.tm01Work.TickMarks.tmXTMajorOutwardLengthF     : 0.0
! tm01.tm01Work.TickMarks.tmYLMajorOutwardLengthF     : 0.0
! tm01.tm01Work.TickMarks.tmYRMajorOutwardLengthF     : 0.0
!
!       The width of major tick marks
! tm01.tm01Work.TickMarks.tmXBMajorThicknessF         : 2.0
! tm01.tm01Work.TickMarks.tmXTMajorThicknessF         : 2.0
! tm01.tm01Work.TickMarks.tmYLMajorThicknessF         : 2.0
! tm01.tm01Work.TickMarks.tmYRMajorThicknessF         : 2.0
!
!       The length of minor tick marks
! tm01.tm01Work.TickMarks.tmXBMinorLengthF            : .01
! tm01.tm01Work.TickMarks.tmXTMinorLengthF            : .01
! tm01.tm01Work.TickMarks.tmYLMinorLengthF            : .01
! tm01.tm01Work.TickMarks.tmYRMinorLengthF            : .01
!
!       The color of minor tick marks
! tm01.tm01Work.TickMarks.tmXBMinorLineColor          : 1
! tm01.tm01Work.TickMarks.tmXTMinorLineColor          : 1
! tm01.tm01Work.TickMarks.tmYLMinorLineColor          : 1
! tm01.tm01Work.TickMarks.tmYRMinorLineColor          : 1
!
!       Quick switch for turning indicated minor ticks on/off
! tm01.tm01Work.TickMarks.tmXBMinorOn                 : True
! tm01.tm01Work.TickMarks.tmXTMinorOn                 : True
! tm01.tm01Work.TickMarks.tmYLMinorOn                 : True
! tm01.tm01Work.TickMarks.tmYRMinorOn                 : True
!
!       The length of minor tick marks outside the border
! tm01.tm01Work.TickMarks.tmXBMinorOutwardLengthF     : 0.0
! tm01.tm01Work.TickMarks.tmXTMinorOutwardLengthF     : 0.0
! tm01.tm01Work.TickMarks.tmYLMinorOutwardLengthF     : 0.0
! tm01.tm01Work.TickMarks.tmYRMinorOutwardLengthF     : 0.0
!
!       The width of minor tick marks
! tm01.tm01Work.TickMarks.tmXBMinorThicknessF         : 1.0
! tm01.tm01Work.TickMarks.tmXTMinorThicknessF         : 1.0
! tm01.tm01Work.TickMarks.tmYLMinorThicknessF         : 1.0
! tm01.tm01Work.TickMarks.tmYRMinorThicknessF         : 1.0
