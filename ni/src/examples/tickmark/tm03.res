!
! Explicitly set resources for this example include:
!
*tm03Work.TickMarks.tmBorderThicknessF : 3.0
*tm03Work.TickMarks.TmMajorThicknessesF : 3.0
*tm03Work.TickMarks.TmMinorThicknessesF : 2.0
*tm03Work.TickMarks.TmLabelFontHeightsF : .02
*tm03Work.TickMarks.TmLabelFonts : 25
*tm03Work.TickMarks.tmXBNoMinor : 1
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
! *tm03Work.TickMarks.tmBorderLineColor           : 1
!
!       Width of the plot perimeter
! *tm03Work.TickMarks.tmBorderThicknessF          : 2.0
!
!       Number of scale digits at which scientific notation switches on.
! *tm03Work.TickMarks.tmSciNoteCutoff             : 6
!
!       Use the bottom tick scaling on the top axis
! *tm03Work.TickMarks.tmXUseBottom                : True
!
!       Use the left tick scaling on the right axis
! *tm03Work.TickMarks.tmYUseLeft                  : True
!
!
!  ==============================================================
!  The following resources relate to the viewport on the drawing
!  canvas.
!  ==============================================================
!
!       Height in NDC (0. to 1.) of the viewport
! *tm03Work.TickMarks.vpHeightF                   : 0.8
!
!       Switch to allow the plot X to Y aspect ratio to remain fixed
!       when resizing or moving the object.
! *tm03Work.TickMarks.vpKeepAspect                : False
!
!       Save this graphical object as a GKS segment
! *tm03Work.TickMarks.vpUseSegments               : False
!
!       Width in NDC (0. to 1.) of the viewport
! *tm03Work.TickMarks.vpWidthF                    : 0.8
!
!       NDC (0. to 1.) X location of the upper left corner of the plot
! *tm03Work.TickMarks.vpXF                        : 0.1
!
!       NDC (0. to 1.) Y location of the upper left corner of the plot
! *tm03Work.TickMarks.vpYF                        : 0.9
!
!
!  ==============================================================
!  The following resources relate to major and minor grid lines
!  between the X axes (top to bottom), or Y axes (left to right).
!  ==============================================================
!
!       Quick switch to turn major grid lines on/off
! *tm03Work.TickMarks.tmXMajorGrid                : False
! *tm03Work.TickMarks.tmYMajorGrid                : False
!
!       Color of major grid lines (GKS indexed)
! *tm03Work.TickMarks.tmXMajorGridLineColor       : 1
! *tm03Work.TickMarks.tmYMajorGridLineColor       : 1
!
!       Dash pattern for major grid lines
! *tm03Work.TickMarks.tmXMajorGridLineDashPattern : 0
! *tm03Work.TickMarks.tmYMajorGridLineDashPattern : 0
!
!       Width of major grid lines
! *tm03Work.TickMarks.tmXMajorGridThicknessF      : 2.0
! *tm03Work.TickMarks.tmYMajorGridThicknessF      : 2.0
!
!       Quick switch to turn minor grid lines on/off
! *tm03Work.TickMarks.tmXMinorGrid                : False
! *tm03Work.TickMarks.tmYMinorGrid                : False
!
!       Color of minor grid lines (GKS indexed)
! *tm03Work.TickMarks.tmXMinorGridLineColor       : 1
! *tm03Work.TickMarks.tmYMinorGridLineColor       : 1
!
!       Dash pattern for minor grid lines
! *tm03Work.TickMarks.tmXMinorGridLineDashPattern : 0
! *tm03Work.TickMarks.tmYMinorGridLineDashPattern : 0
!
!       Width of minor grid lines
! *tm03Work.TickMarks.tmXMinorGridThicknessF      : 1.0
! *tm03Work.TickMarks.tmYMinorGridThicknessF      : 1.0
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
! *tm03Work.TickMarks.tmXBMode                    : AUTOMATIC
! *tm03Work.TickMarks.tmXTMode                    : AUTOMATIC
! *tm03Work.TickMarks.tmYLMode                    : AUTOMATIC
! *tm03Work.TickMarks.tmYRMode                    : AUTOMATIC
!
!       The maximum number of major ticks in AUTOMATIC mode
! *tm03Work.TickMarks.tmXBMaxTicks                : 7
! *tm03Work.TickMarks.tmXTMaxTicks                : 7
! *tm03Work.TickMarks.tmYLMaxTicks                : 7
! *tm03Work.TickMarks.tmYRMaxTicks                : 7
!
!       Use automatic calculation of scaling increments on the indicated axis
! *tm03Work.TickMarks.tmXBAutoPrecision           : True
! *tm03Work.TickMarks.tmXTAutoPrecision           : True
! *tm03Work.TickMarks.tmYLAutoPrecision           : True
! *tm03Work.TickMarks.tmYRAutoPrecision           : True
!
!       Draw the indicated border of the grid perimeter
! *tm03Work.TickMarks.tmXBBorderOn                : True
! *tm03Work.TickMarks.tmXTBorderOn                : True
! *tm03Work.TickMarks.tmYLBorderOn                : True
! *tm03Work.TickMarks.tmYRBorderOn                : True
!
!       Grid scale style for the indicated axis.
!       Options are LINEAR, LOG, IRREGULAR, TIME, and GEOGRAPHIC.
! *tm03Work.TickMarks.tmXBStyle                   : LINEAR
! *tm03Work.TickMarks.tmXTStyle                   : LINEAR
! *tm03Work.TickMarks.tmYLStyle                   : LINEAR
! *tm03Work.TickMarks.tmYRStyle                   : LINEAR
!
!       NDC (0. to 1.) location of the left tick (X) or bottom tick (Y)
! *tm03Work.TickMarks.tmXBTickStartF              : 0.0
! *tm03Work.TickMarks.tmXTTickStartF              : 0.0
! *tm03Work.TickMarks.tmYLTickStartF              : 0.0
! *tm03Work.TickMarks.tmYRTickStartF              : 0.0
!
!       NDC (0. to 1.) location of the right tick (X) or top tick (Y)
! *tm03Work.TickMarks.tmXBTickEndF                : 0.0
! *tm03Work.TickMarks.tmXTTickEndF                : 0.0
! *tm03Work.TickMarks.tmYLTickEndF                : 0.0
! *tm03Work.TickMarks.tmYRTickEndF                : 0.0
!
!       NDC spacing between major tick marks along the indicated axis
!       Used with Mode = MANUAL only.
! *tm03Work.TickMarks.tmXBTickSpacingF            : 0.0
! *tm03Work.TickMarks.tmXTTickSpacingF            : 0.0
! *tm03Work.TickMarks.tmYLTickSpacingF            : 0.0
! *tm03Work.TickMarks.tmYRTickSpacingF            : 0.0
!
!       Currently unused
! *tm03Work.TickMarks.tmXBSpacingType             : 0
! *tm03Work.TickMarks.tmXTSpacingType             : 0
! *tm03Work.TickMarks.tmYLSpacingType             : 0
! *tm03Work.TickMarks.tmYRSpacingType             : 0
!
!       Scale value of data at the left of the indicated axis
! *tm03Work.TickMarks.tmXBDataLeftF               : 0.0
! *tm03Work.TickMarks.tmXTDataLeftF               : 0.0
! *tm03Work.TickMarks.tmYLDataLeftF               : 0.0
! *tm03Work.TickMarks.tmYRDataLeftF               : 0.0
!
!       Scale value of data at the right of the indicated axis
! *tm03Work.TickMarks.tmXBDataRightF              : 0.0
! *tm03Work.TickMarks.tmXTDataRightF              : 0.0
! *tm03Work.TickMarks.tmYLDataRightF              : 0.0
! *tm03Work.TickMarks.tmYRDataRightF              : 0.0
!
!       The number of digits precision in AUTOMATIC mode scaling.
! *tm03Work.TickMarks.tmXBPrecision               : 4
! *tm03Work.TickMarks.tmXTPrecision               : 4
! *tm03Work.TickMarks.tmYLPrecision               : 4
! *tm03Work.TickMarks.tmYRPrecision               : 4
!
!       Spline interpolation tension for data along an IRREGULAR style axis.
! *tm03Work.TickMarks.tmXBIrrTensionF             : 2.0
! *tm03Work.TickMarks.tmXTIrrTensionF             : 2.0
! *tm03Work.TickMarks.tmYLIrrTensionF             : 2.0
! *tm03Work.TickMarks.tmYRIrrTensionF             : 2.0
!
!       An array of irregular values to be placed linearly along an axis.
!       Must be monotonically increasing.  For IRREGULAR style only.
! *tm03Work.TickMarks.tmXBIrregularPoints         : Null
! *tm03Work.TickMarks.tmXTIrregularPoints         : Null
! *tm03Work.TickMarks.tmYLIrregularPoints         : Null
! *tm03Work.TickMarks.tmYRIrregularPoints         : Null
!
!       An array of labels when Mode is EXPLICIT
! *tm03Work.TickMarks.tmXBLabels                  : Null
! *tm03Work.TickMarks.tmXTLabels                  : Null
! *tm03Work.TickMarks.tmYLLabels                  : Null
! *tm03Work.TickMarks.tmYRLabels                  : Null
!
!       An array of major tick values when Mode is EXPLICIT
! *tm03Work.TickMarks.tmXBValues                  : Null
! *tm03Work.TickMarks.tmXTValues                  : Null
! *tm03Work.TickMarks.tmYLValues                  : Null
! *tm03Work.TickMarks.tmYRValues                  : Null
!
!  --------------------------
!  Labels at major tick marks
!  --------------------------
!
!       Quick switch for turning labels on/off
! *tm03Work.TickMarks.tmXBLabelsOn                : True
! *tm03Work.TickMarks.tmXTLabelsOn                : True
! *tm03Work.TickMarks.tmYLLabelsOn                : True
! *tm03Work.TickMarks.tmYRLabelsOn                : True
!
!       Angle at which the indicated axis scale labels are drawn
! *tm03Work.TickMarks.tmXBLabelAngleF             : 0.0
! *tm03Work.TickMarks.tmXTLabelAngleF             : 0.0
! *tm03Work.TickMarks.tmYLLabelAngleF             : 0.0
! *tm03Work.TickMarks.tmYRLabelAngleF             : 0.0
!
!       Spacing between the scale values and the grid axis
! *tm03Work.TickMarks.tmXBLabelDeltaF             : 0.0
! *tm03Work.TickMarks.tmXTLabelDeltaF             : 0.0
! *tm03Work.TickMarks.tmYLLabelDeltaF             : 0.0
! *tm03Work.TickMarks.tmYRLabelDeltaF             : 0.0
!
!       Direction in which scale labels are drawn (UP,DOWN,ACROSS)
!       Normal direction is across for all axes.
! *tm03Work.TickMarks.tmXBLabelDirection          : ACROSS
! *tm03Work.TickMarks.tmXTLabelDirection          : ACROSS
! *tm03Work.TickMarks.tmYLLabelDirection          : ACROSS
! *tm03Work.TickMarks.tmYRLabelDirection          : ACROSS
!
!       The font style to use for labels
! *tm03Work.TickMarks.tmXBLabelFont               : 0
! *tm03Work.TickMarks.tmXTLabelFont               : 0
! *tm03Work.TickMarks.tmYLLabelFont               : 0
! *tm03Work.TickMarks.tmYRLabelFont               : 0
!
!       The height to width aspect ratio of label characters
! *tm03Work.TickMarks.tmXBLabelFontAspectF        : 1.3125
! *tm03Work.TickMarks.tmXTLabelFontAspectF        : 1.3125
! *tm03Work.TickMarks.tmYLLabelFontAspectF        : 1.3125
! *tm03Work.TickMarks.tmYRLabelFontAspectF        : 1.3125
!
!       The color for label text
! *tm03Work.TickMarks.tmXBLabelFontColor          : 1
! *tm03Work.TickMarks.tmXTLabelFontColor          : 1
! *tm03Work.TickMarks.tmYLLabelFontColor          : 1
! *tm03Work.TickMarks.tmYRLabelFontColor          : 1
!
!       The NDC (0. to 1.) height of the label text
! *tm03Work.TickMarks.tmXBLabelFontHeightF        : .02
! *tm03Work.TickMarks.tmXTLabelFontHeightF        : .02
! *tm03Work.TickMarks.tmYLLabelFontHeightF        : .02
! *tm03Work.TickMarks.tmYRLabelFontHeightF        : .02
!
!       How label text is justified in a text extent rectangle
! *tm03Work.TickMarks.tmXBLabelJust               : 3
! *tm03Work.TickMarks.tmXTLabelJust               : 3
! *tm03Work.TickMarks.tmYLLabelJust               : 3
! *tm03Work.TickMarks.tmYRLabelJust               : 3
!
!       The stride between labeled major ticks
! *tm03Work.TickMarks.tmXBLabelStride             : 0
! *tm03Work.TickMarks.tmXTLabelStride             : 0
! *tm03Work.TickMarks.tmYLLabelStride             : 0
! *tm03Work.TickMarks.tmYRLabelStride             : 0
!
!  ------------------------
!  Appearance of tick marks
!  ------------------------
!
!       Quick switch for turning indicated ticks on/off
! *tm03Work.TickMarks.tmXBOn                      : True
! *tm03Work.TickMarks.tmXTOn                      : True
! *tm03Work.TickMarks.tmYLOn                      : True
! *tm03Work.TickMarks.tmYROn                      : True
!
!       The number of minor tick marks per major tick mark
! *tm03Work.TickMarks.tmXBMinorPerMajor           : 3
! *tm03Work.TickMarks.tmXTMinorPerMajor           : 3
! *tm03Work.TickMarks.tmYLMinorPerMajor           : 3
! *tm03Work.TickMarks.tmYRMinorPerMajor           : 3
!
!       The length of major tick marks
! *tm03Work.TickMarks.tmXBMajorLengthF            : .02
! *tm03Work.TickMarks.tmXTMajorLengthF            : .02
! *tm03Work.TickMarks.tmYLMajorLengthF            : .02
! *tm03Work.TickMarks.tmYRMajorLengthF            : .02
!
!       The color of major tick marks
! *tm03Work.TickMarks.tmXBMajorLineColor          : 1
! *tm03Work.TickMarks.tmXTMajorLineColor          : 1
! *tm03Work.TickMarks.tmYLMajorLineColor          : 1
! *tm03Work.TickMarks.tmYRMajorLineColor          : 1
!
!       The length of major tick marks outside the border
! *tm03Work.TickMarks.tmXBMajorOutwardLengthF     : 0.0
! *tm03Work.TickMarks.tmXTMajorOutwardLengthF     : 0.0
! *tm03Work.TickMarks.tmYLMajorOutwardLengthF     : 0.0
! *tm03Work.TickMarks.tmYRMajorOutwardLengthF     : 0.0
!
!       The width of major tick marks
! *tm03Work.TickMarks.tmXBMajorThicknessF         : 2.0
! *tm03Work.TickMarks.tmXTMajorThicknessF         : 2.0
! *tm03Work.TickMarks.tmYLMajorThicknessF         : 2.0
! *tm03Work.TickMarks.tmYRMajorThicknessF         : 2.0
!
!       The length of minor tick marks
! *tm03Work.TickMarks.tmXBMinorLengthF            : .01
! *tm03Work.TickMarks.tmXTMinorLengthF            : .01
! *tm03Work.TickMarks.tmYLMinorLengthF            : .01
! *tm03Work.TickMarks.tmYRMinorLengthF            : .01
!
!       The color of minor tick marks
! *tm03Work.TickMarks.tmXBMinorLineColor          : 1
! *tm03Work.TickMarks.tmXTMinorLineColor          : 1
! *tm03Work.TickMarks.tmYLMinorLineColor          : 1
! *tm03Work.TickMarks.tmYRMinorLineColor          : 1
!
!       Quick switch for turning indicated minor ticks on/off
! *tm03Work.TickMarks.tmXBMinorOn                 : True
! *tm03Work.TickMarks.tmXTMinorOn                 : True
! *tm03Work.TickMarks.tmYLMinorOn                 : True
! *tm03Work.TickMarks.tmYRMinorOn                 : True
!
!       The length of minor tick marks outside the border
! *tm03Work.TickMarks.tmXBMinorOutwardLengthF     : 0.0
! *tm03Work.TickMarks.tmXTMinorOutwardLengthF     : 0.0
! *tm03Work.TickMarks.tmYLMinorOutwardLengthF     : 0.0
! *tm03Work.TickMarks.tmYRMinorOutwardLengthF     : 0.0
!
!       The width of minor tick marks
! *tm03Work.TickMarks.tmXBMinorThicknessF         : 1.0
! *tm03Work.TickMarks.tmXTMinorThicknessF         : 1.0
! *tm03Work.TickMarks.tmYLMinorThicknessF         : 1.0
! *tm03Work.TickMarks.tmYRMinorThicknessF         : 1.0

