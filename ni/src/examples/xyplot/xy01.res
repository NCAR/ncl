!
! Example:  This resource file shows all XYPlot resources and
!           their default values.
!
!  ==============================================================
!  The following resources relate to the viewport on the drawing
!  canvas.
!  ==============================================================
!
!       Height in NDC (0. to 1.) of the viewport
! *xy01Work.XYPlot.vpHeightF                   : 0.8
!
!       Switch to allow the plot X to Y aspect ratio to remain fixed
!       when resizing or moving the object.
! *xy01Work.XYPlot.vpKeepAspect                : False
!
!       Save this graphical object as a GKS segment
! *xy01Work.XYPlot.vpUseSegments               : False
!
!       Width in NDC (0. to 1.) of the viewport
! *xy01Work.XYPlot.vpWidthF                    : 0.8
!
!       NDC (0. to 1.) X location of the upper left corner of the plot
! *xy01Work.XYPlot.vpXF                        : 0.1
!
!       NDC (0. to 1.) Y location of the upper left corner of the plot
! *xy01Work.XYPlot.vpYF                        : 0.9
!
! --------------------
! THE COMPOSITE XYPLOT
! --------------------
!
!       Specifies the array of data object ids that provide the x/y
!       coordinate pairs to be plotted. 
! *xy01Work.XYPlot.xyCurveData                 : NULL
!
!       Sets the linewidth scale factor for all curves.
! *xy01Work.XYPlot.xyCurveThicknessF           : 1.0
!
!       This resource indicates the length of each segment of a dash pattern.
!       It is the length in NDC before the dash pattern repeats itself.  This
!       resource automatically scales with changes in the size of the viewport
!       of the XyPlot.
! *xy01Work.XYPlot.xyDashSegmentLengthF        : 0.15
!
!
!       This allows the user to set the size of the characters used for the
!       curve labels when the Data Dependent resource 'xyLabelMode' is set to
!       LETTERED or CUSTOM. The size of the font is expressed in NDC and
!       scales with changes in the size of the Viewport of the XyPlot object.
! *xy01Work.XYPlot.xyLineLabelFontHeightF      : 0.01
!
!       Sets the style for the [xy]-axis of the XyPlot.  The styles are LOG,
!       LINEAR, IRREGULAR, TIME, and GEOGRAPHIC. These are identical to the
!       styles available through the TickMark object.  The XyPlot does not
!       allow the programmer to set the opposing side styles directly like it
!       is done in the TickMark object.  To achieve multiple scales (i.e.
!       Pressure on top, Km on bottom) either the tm[XY][TB]Mode can be set
!       to EXPLICIT and strings representing the alternate scale provided, or
!       the xy[XY]AlternateCoords resource can be used.
! *xy01Work.XYPlot.xyXStyle                    : LINEAR
! *xy01Work.XYPlot.xyYStyle                    : LINEAR
!
!       Specifies the tension to apply to the spline approximation used to
!       determine the transformation for the xy[XY]IrregularPoints.  This
!       resource is only used if xy[XY]Style is IRREGULAR.
! *xy01Work.XYPlot.xyXIrrTensionF              : 2.0
! *xy01Work.XYPlot.xyYIrrTensionF              : 2.0
!
!       An array of floats that specify a discrete representation of an
!       irregular coordinate system. The values in this array must be
!       monotonic increasing or decreasing. This resource is only used if
!       xy[XY]Style is IRREGULAR
! *xy01Work.XYPlot.xyXIrregularPoints                : NULL
! *xy01Work.XYPlot.xyYIrregularPoints                : NULL
!
!       If this resource is False the XyPlot sets its left extent to be
!       xyXMinF and its right extent to be xyXMaxF. If this resource is True,
!       the X-axis is inverted so that xyXMinF is on the right, and xyXMaxF is
!       on the left.
! *xy01Work.XYPlot.xyXReverse                        : False
!
!       If this resource is False, the XyPlot sets its bottom extent to be
!       xyYMinF and the top extent to be xyYMaxF.  If this resource is True,
!       the Y-axis is inverted so that xyYMinF is on the top, and xyYMaxF is
!       on the bottom.
! *xy01Work.XYPlot.xyYReverse                  : False
!
!       If this resource is True, then the xy[XY]MinF resource will be
!       re-calculated every time the xyCurveData resource is modified.
!       If this resource is False, then the xy[XY]MinF resource will
!       not be re-calculated each time the xyCurveData resource is modified.
!
!       If the programmer specifies the xy[XY]MinF resource, then the
!       default for this resource will be False.  If the programmer
!       doesn't specify the xy[XY]MinF resource, then the default for this
!       resource will be True.
! *xy01Work.XYPlot.xyComputeXMin               : dynamic
! *xy01Work.XYPlot.xyComputeYMin               : dynamic
!
!       If this resource is True, then the xy[XY]MaxF resource will be
!       re-calculated every time the xyCurveData resource is modified.
!       If this resource is False, then the xy[XY]MaxF resource will not
!       be re-calculated each time the xyCurveData resource is modified.
!
!       If the programmer specifies the xy[XY]MaxF resource, then the
!       default for this resource will be False.  If the programmer
!       doesn't specify the xy[XY]MaxF resource, then the default for this
!       resource will be True.
! *xy01Work.XYPlot.xyComputeXMax               : dynamic
! *xy01Work.XYPlot.xyComputeYMax               : dynamic
! 
!       This resource specifies the minimum value that will be displayed along
!       the X-axis of the plot.  The xyXReverse resource controls if that is
!       on the left or right side of the plot.
! 
!       If the xyComputeXMin resource is True, then this resource gets
!       re-calculated each time the xyCurveData resource changes.  Otherwise,
!       this resource gets the minimum value of the first data specified, and
!       retains that value.
! *xy01Work.XYPlot.xyXMinF                     : dynamic
!
!       This resource specifies the maximum value that will be displayed along
!       the X-axis of the plot.  The xyXReverse resource controls if that is
!       on the left or right side of the plot.
! 
!       If the xyComputeXMax resource is True, then this resource gets
!       re-calculated each time the xyCurveData resource changes.  Otherwise,
!       this resource gets the maximum value of the first data specified, and
!       retains that value.
! *xy01Work.XYPlot.xyXMaxF                     : dynamic
!
!       This resource specifies the minimum value that will be displayed along
!       the Y-axis of the plot.  The xyYReverse resource controls if that is
!       on the top or bottom side of the plot.
!
!       If the xyComputeYMin resource is True, then this resource gets
!       re-calculated each time the xyCurveData resource changes.  Otherwise,
!       this resource gets the minimum value of the first data specified, and
!       retains that value.
! *xy01Work.XYPlot.xyYMinF                     : dynamic
!
!       This resource specifies the maximum value that will be displayed along
!       the y axis of the plot.  The xyYReverse resource controls if that is
!       on the top or bottom side of the plot.
! 
!       If the xyComputeYMax resource is True, then this resource gets
!       re-calculated each time the xyCurveData resource changes.  Otherwise,
!       this resource gets the maximum value of the first data specified, and
!       retains that value.
! *xy01Work.XYPlot.xyYMaxF                     : dynamic
!
!       These resources are currently not available:
! *xy01Work.XYPlot.xyXAlternate                : NULL
! *xy01Work.XYPlot.xyYAlternate                : NULL
! *xy01Work.XYPlot.xyYAlternateCoords          : NULL
! *xy01Work.XYPlot.xyXAlternateCoords          : NULL
! *xy01Work.XYPlot.xyXOriginalCoords           : NULL
! *xy01Work.XYPlot.xyYOriginalCoords           : NULL
! 
! XyDataDep Class Resource
! ========================
! 
!       This resource determines what color index all the curves derived from
!       the associated Data Object will be drawn with if xyMonoColor is True.
! *xy01Work.XYDep.xyColor                      : NhlFOREGROUND
!
!       An array of color indices to use when drawing the corresponding curves
!       in the associated Data Object. If there are more curves then the
!       number of color indices in this resource, the remaining curves will be
!       drawn using the xyColor resource.
!
!       This resource is only used if the xyMonoColor resource is False. 
! *xy01Work.XYDep.xyColors                     : NULL
!
!       If this resource is True, then all the curves in the associated Data
!       Object will be drawn in the color specified by the xyColor resource.
!       Otherwise, the curves are drawn in the color specified by the
!       corresponding index in the xyColors array resource.
! *xy01Work.XYDep.xyMonoColor                  : False
!
!       This resource determines what dash pattern index all the curves
!       derived from the associated Data Object will be drawn with if
!       xyMonoDashPattern is True.
! *xy01Work.XYDep.xyDashPattern                : NhlSOLIDLINE
!
!       An array of integer dash pattern indices to use when drawing the
!       corresponding curves in the associated Data Object. If there are more
!       curves then the number of dash pattern indices in this resource, the
!       remaining curves will be drawn using the xyDashPattern resource.
!
!       This resource is only used if the xyMonoDashPattern resource is False. 
! *xy01Work.XYDep.xyDashPatterns               : NULL
!
!       If this resource is True, then all the curves in the associated Data
!       Object will be drawn with the dash pattern specified by the
!       xyDashPattern resource.  Otherwise, the curves are drawn with the dash
!       pattern specified by the corresponding index in the xyDashPatterns
!       array resource.
! *xy01Work.XYDep.xyMonoDashPattern            : False
!
!       This resource determines if markers, curves, or both markers and
!       curves will be displayed for all coordinate arrays derived from the
!       associated Data Object if xyMonoMarkerMode is True.
! *xy01Work.XYDep.xyMarkerMode                 : NhlNOMARKERS
!
!       If this resource is True, then all the curves in the associated Data
!       Object will be drawn as specified by the xyMarkerMode resource.
!       Otherwise, the curves are drawn in the marker mode specified by the
!       corresponding index in the xyMarkerModes array resource.
! *xy01Work.XYDep.xyMonoMarkerMode             : False
!
!       An array of NhlMarkerMode enumerations to use when drawing the
!       corresponding curves in the associated Data Object. If there are more
!       curves then the number of enumerations in this resource, the remaining
!       curves will be drawn using the value of the xyMarkerMode resource.
! *xy01Work.XYDep.xyMarkerModes                : NULL
!
!       This resource indicates which Marker index all the coordinate pair
!       arrays derived from the associated Data Object will be drawn with if
!       xyMonoMarker is True.
! *xy01Work.XYDep.xyMarker                     : 0
!
!       An array of Marker indices to use when drawing the corresponding
!       coordinate arrays in the associated Data Object. If there are more
!       coordinate arrays then the number of enumerations in this resource,
!       the remaining coordinate arrays will be drawn using the value of the
!       xyMarker resource.
! *xy01Work.XYDep.xyMarkers                    : NULL
!
!       If this resource is True, then all the coordinate arrays in the
!       associated Data Object will be drawn with the marker index specified
!       by the xyMarker resource.  Otherwise, the coordinate arrays are drawn
!       with the marker specified by the corresponding index in the xyMarkers
!       array resource.
! *xy01Work.XYDep.xyMonoMarker                 : False
!
!       This resource indicates what color index all markers drawn using the
!       coordinate arrays from the associated Data Object will be drawn with
!       if xyMonoMarkerColor is True.
! *xy01Work.XYDep.xyMarkerColor                : NhlFOREGROUND
!
!       An array of color indices to use when drawing the markers for the
!       corresponding coordinate arrays in the associated Data Object.  If
!       there are more coordinate arrays then the number of color indices in
!       this resource, the remaining coordinate arrays will be drawn using the
!       xyColor resource.
! *xy01Work.XYDep.xyMarkerColors               : NULL
!
!       If this resource is True, then all the markers for the corresponding
!       coordinate arrays in the associated Data Object will be drawn in the
!       color specified by the xyMarkerColor resource.  Otherwise, each
!       coordinate arrays markers are drawn in the color specified by the
!       corresponding index in the xyMarkerColors array resource.
! *xy01Work.XYDep.xyMonoMarkerColor            : False
!
!       This resource indicates the size for all markers drawn using the
!       coordinate arrays from the associated Data Object if xyMonoMarkerColor
!       is True. The size is in NDC.
! *xy01Work.XYDep.xyMarkerSizeF                : .01
!
!       An array of floats that indicate the size for the markers in the
!       corresponding coordinate arrays in the associated Data Object. If
!       there are more coordinate arrays then the number of floats in this
!       resource, the remaining coordinate arrays will be drawn using the
!       xyMarkerSizeF resource.
! *xy01Work.XYDep.xyMarkerSizes                : NULL
!
!       If this resource is True, then all the markers for the corresponding
!       coordinate arrays in the associated Data Object will be drawn with the
!       size specified by the xyMarkerSizeF resource.  Otherwise, each
!       coordinate arrays markers are drawn with the size specified by the
!       corresponding index in the xyMarkerSizes array resource.
! *xy01Work.XYDep.xyMonoMarkerSize             : False
!
!       This can be set to either NOLABELS, LETTERED, or CUSTOM.  NOLABELS
!       means that the curves from the associated DataItem will not be
!       labeled.  LETTERED means that the curves will be labeled with capital
!       letters in the order in which they appear in the associated DataItem.
!       The current letter is retained so that if the next XyDataDep in the
!       list is using LETTERED the letter will not be reused until the entire
!       alphabet has been used.  (NOTE: This means that LETTERED curves do not
!       necessarily retain the same label when Data is added or removed from
!       the xyCurveData resource.  CUSTOM means that the curves will be
!       labeled with the corresponding strings provided by the
!       xyExplicitLabels resource. If there is no corresponding string, the
!       curves will be labeled by using the name of the XyDataDep object and
!       appending the curve index.
! *xy01Work.XYDep.xyLabelMode                  : NOLABELS
!
!
!       An array of character strings to place inside the corresponding curve
!       in the associated DataItem. If there are more curves than the number
!       of strings in this resource, the remaining curves will be labeled by
!       using the name of the XyDataDep object and appending the curve
!       index. This resource is only used if xyLabelMode is set to CUSTOM.
! *xy01Work.XYDep.xyExplicitLabels             : NULL
