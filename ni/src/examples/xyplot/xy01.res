!
! Example:  This resource file shows all xyPlot resources and
!           their default values.
!
!  ==============================================================
!  The following resources relate to the viewport on the drawing
!  canvas.
!  ==============================================================
!
!       Height in NDC (0. to 1.) of the viewport
! xy01.xy01Work.xyPlot.vpHeightF                   : 0.8
!
!       Switch to allow the plot X to Y aspect ratio to remain fixed
!       when resizing or moving the object.
! xy01.xy01Work.xyPlot.vpKeepAspect                : False
!
!       Save this graphical object as a GKS segment
! xy01.xy01Work.xyPlot.vpUseSegments               : False
!
!       Width in NDC (0. to 1.) of the viewport
! xy01.xy01Work.xyPlot.vpWidthF                    : 0.8
!
!       NDC (0. to 1.) X location of the upper left corner of the plot
! xy01.xy01Work.xyPlot.vpXF                        : 0.1
!
!       NDC (0. to 1.) Y location of the upper left corner of the plot
! xy01.xy01Work.xyPlot.vpYF                        : 0.9
!
! --------------------
! The Composite XyPlot
! --------------------
!
!       Specifies the array of data object ids that provide the x/y
!       coordinate pairs to be plotted. 
! xy01.xy01Work.xyPlot.xyCoordData                 : NULL
!
!       This resource is a retrievable resource only. It is used to
!       allow the user a greater amount of configurability of how each
!       coordinate pair array is displayed in the XyPlot. After adding
!       Data Item's to the XyPlot xyCoordData resource, this resource
!       can be retrieved to get an array of XyDataSpec object id's.
!       This array is ordered so that the elements of this array
!       correspond directly with the elements of the xyCoordData array
!       resource. The XyDataSpec resources can be set to each object
!       in this array.  This will determine exactly how the
!       corresponding DataItem in the xyCoordData resource will be
!       rendered in the XyPlot.
! xy01.xy01Work.xyPlot.xyDataSpec                  : NULL
!
!       Sets the style for the [xy]-axis of the xyPlot.  The styles are
!       LOG, LINEAR, IRREGULAR, TIME, and GEOGRAPHIC. These are
!       identical to the styles available through the TickMark object.
!       The xyPlot does not allow the programmer to set the opposing
!       side styles directly like it is done in the TickMark object.
!       To achieve multiple scales (i.e. Pressure on top, Km on bottom)
!       either the tm[XY][TB]Mode can be set to EXPLICIT and strings
!       representing the alternate scale provided, or the
!       xy[XY]AlternateCoords resource can be used.
! xy01.xy01Work.xyPlot.xyXStyle                    : LINEAR
! xy01.xy01Work.xyPlot.xyYStyle                    : LINEAR
!
!       An array of floats that specify a discrete representation of an
!       irregular coordinate system. The values in this array must be
!       monotonic increasing or decreasing. This resource is only used
!       if xy[XY]Style is IRREGULAR
! xy01.xy01Work.xyPlot.xyXIrregularPoints          : NULL
! xy01.xy01Work.xyPlot.xyYIrregularPoints          : NULL
!
!       Specifies the Tension to apply to the spline approximation used
!       to determine the transformation for the xy[XY]IrregularPoints.
!       This resource is only used if xyXStyle is IRREGULAR. 
! xy01.xy01Work.xyPlot.xyXIrrTensionF              : 2.0
! xy01.xy01Work.xyPlot.xyYIrrTensionF              : 2.0
!
!       If this resource is True, then the tr[XY]MinF resource will be
!       re-calculated every time the xyCoordData resource is modified.
!       If this resource is False, then the tr[XY]MinF resource will
!       not be re-calculated each time the xyCoordData resource is
!       modified.
!
!       If the programmer specifies the tr[XY]MinF resource, then the
!       default for this resource will be False.  If the programmer
!       doesn't specify the tr[XY]MinF resource, then the default for
!       this resource will be True.
! xy01.xy01Work.xyPlot.xyComputeXMin               : dynamic
! xy01.xy01Work.xyPlot.xyComputeYMin               : dynamic
!
!       If this resource is True, then the tr[XY]MaxF resource will be
!       re-calculated every time the xyCoordData resource is modified.
!       If this resource is False, then the tr[XY]MaxF resource will not
!       be re-calculated each time the xyCoordData resource is modified.
!
!       If the programmer specifies the tr[XY]MaxF resource, then the
!       default for this resource will be False.  If the programmer
!       doesn't specify the tr[XY]MaxF resource, then the default for
!       this resource will be True.
! xy01.xy01Work.xyPlot.xyComputeXMax               : dynamic
! xy01.xy01Work.xyPlot.xyComputeYMax               : dynamic
!
!       These resources are currently not available:
! xy01.xy01Work.xyPlot.xyXAlternate                : NULL
! xy01.xy01Work.xyPlot.xyYAlternate                : NULL
! xy01.xy01Work.xyPlot.xyYAlternateCoords          : NULL
! xy01.xy01Work.xyPlot.xyXAlternateCoords          : NULL
! xy01.xy01Work.xyPlot.xyXOriginalCoords           : NULL
! xy01.xy01Work.xyPlot.xyYOriginalCoords           : NULL
! 
! XyCoordDataSpec Class Resources
! ===============================
! If an xyCoordDataSpec object is not explicitly created in your program,
! then one is created for you, and made a child of the xyPlot object,
! and having same name as the Data object.  In this case, the "xy01"
! example's data object is called "xyData", and the XyPlot object is
! "xyPlot", hence the following resources' full pathname is
! "xy01.xy01Work.xyPlot.xyData".
!
!       This resource indicates the length of each segment of a dash
!       pattern. It is the length in NDC before the dash pattern
!       repeats itself.  This resource automatically scales with
!       changes in the size of the viewport of the xyPlot.
! xy01.xy01Work.xyPlot.xyData.xyLineDashSegLenF    : 0.15
!
!       This allows the user to set the size of the characters used for
!       the curve labels when the Data Dependent resource 'xyLabelMode'
!       is set to LETTERED or CUSTOM. The size of the font is expressed
!       in NDC and scales with changes in the size of the Viewport of
!       the xyPlot object.
! xy01.xy01Work.xyPlot.xyData.xyLineLabelFontHeightF: 0.01
!
!       This resource determines what dash pattern index all the curves
!       derived from the associated Data Object will be drawn with if
!       xyMonoDashPattern is True.
! xy01.xy01Work.xyPlot.xyData.xyDashPattern        : SOLIDLINE
!
!       An array of integer dash pattern indices to use when drawing the
!       corresponding curves in the associated Data Object. If there
!       are more curves then the number of dash pattern indices in
!       this resource, the remaining curves will be drawn using the
!       xyDashPattern resource.
!
!       This resource is only used if the xyMonoDashPattern resource
!       is False. 
! xy01.xy01Work.xyPlot.xyData.xyDashPatterns       : NULL
!
!       If this resource is True, then all the curves in the associated
!       Data Object will be drawn with the dash pattern specified by the
!       xyDashPattern resource.  Otherwise, the curves are drawn with
!       the dash pattern specified by the corresponding index in the
!       xyDashPatterns array resource.
! xy01.xy01Work.xyPlot.xyData.xyMonoDashPattern    : False
!
!       This resource determines if markers, curves, or both markers and
!       curves will be displayed for all coordinate arrays derived from
!       the associated Data Object if xyMonoMarkerLineMode is True.
! xy01.xy01Work.xyPlot.xyData.xyMarkLineMode       : LINES
!
!       An array of MarkerMode enumerations to use when drawing the
!       corresponding curves in the associated Data Object. If there
!       are more curves then the number of enumerations in this
!       resource, the remaining curves will be drawn using the value
!       of the xyMarkLineMode resource.
! xy01.xy01Work.xyPlot.xyData.xyMarkLineModes      : NULL
!
!       If this resource is True, then all the curves in the associated
!       Data Object will be drawn as specified by the xyMarkLineMode
!       resource. Otherwise, the curves are drawn in the marker mode
!       specified by the corresponding index in the xyMarkLineModes
!       array resource.
! xy01.xy01Work.xyPlot.xyData.xyMonoMarkLineMode   : False
!
!       An array of character strings to use as Labels in the XyPlot
!       legend. The legend labels will default to the values in the
!       xyExplicitLabels resource. If there are more coordinate pair
!       arrays then the number of elements in this resource, the
!       remaining coordinate pair arrays will also be labeled with
!       the corresponding value of the xyExplicitLabels resource.
! xy01.xy01Work.xyPlot.xyData.xyExplicitLegendLabels: NULL
!
!       This resource determines what color index all the curves
!       derived from the associated Data Object will be drawn with
!       if xyLineMonoColor is True.
! xy01.xy01Work.xyPlot.xyData.xyLineColor          : FOREGROUND
!
!       An array of color indices to use when drawing the corresponding
!       curves in the associated Data Object. If there are more curves
!       then the number of color indices in this resource, the
!       remaining curves will be drawn using the xyLineColor resource.
!
!       This resource is only used if the xyLineMonoColor resource is
!       False.
! xy01.xy01Work.xyPlot.xyData.xyLineColors         : NULL
!
!       If this resource is True, then all the curves in the associated
!       Data Object will be drawn in the color specified by the
!       xyLineColor resource. Otherwise, the curves are drawn in the
!       color specified by the corresponding index in the xyLineColors
!       array resource.
! xy01.xy01Work.xyPlot.xyData.xyMonoLineColor      : False
!
!      This resource indicates what color index to draw the line
!      labels if xyLabelMode is set to LETTERED or CUSTOM. This value
!      is only used if xyMonoLineLabelColor is True, or the
!      xyLineLabelFontColors array doesn't specify enough values.
! xy01.xy01Work.xyPlot.xyData.xyLineLabelFontColor : FOREGROUND
!
!      This resource indicates what color index to draw the line labels
!      in if xyLabelMode is set to NhlLETTERED or NhlCUSTOM. It is an
!      array of NhlColorIndex's that indicate the color for the line
!      labels in the corresponding coordinate arrays in the associated
!      Data Object. If there are more coordinate arrays then the number
!      of NhlColorIndex's in this resource, the remaining coordinate
!      arrays will be drawn using the xyLineLabelFontColor resource.
! xy01.xy01Work.xyPlot.xyData.xyLineLabelFontColors: NULL
!
!      If this resource is True, then all the line labels for the
!      corresponding coordinate arrays in the associated Data Object
!      will be drawn with the color specified by the xyLineLabelFontColor
!      resource. Otherwise, each coordinate arrays markers are drawn
!      with the color specified by the corresponding index in the
!      xyLineLabelFontColors array resource.
! xy01.xy01Work.xyPlot.xyData.xyMonoLineLabelColor  : False
!
!       This can be set to either NOLABELS, LETTERED, or CUSTOM.
!       NOLABELS means that the curves from the associated DataItem
!       will not be labeled.  LETTERED means that the curves will be
!       labeled with capital letters in the order in which they appear
!       in the associated DataItem. The current letter is retained so
!       that if the next xyDataSpec in the list is using LETTERED the
!       letter will not be reused until the entire alphabet has been
!       used.  (NOTE: This means that LETTERED curves do not
!       necessarily retain the same label when Data is added or
!       removed from the xyCoordData resource.  CUSTOM means that the
!       curves will be labeled with the corresponding strings provided
!       by the xyExplicitLabels resource. If there is no corresponding
!       string, the curves will be labeled by using the name of the
!       xyPlot.xyData object and appending the curve index.
! xy01.xy01Work.xyPlot.xyData.xyLabelMode          : NOLABELS
!
!       An array of character strings to place inside the corresponding
!       curve in the associated DataItem. If there are more curves than
!       the number of strings in this resource, the remaining curves
!       will be labeled by using the name of the xyPlot.xyData object
!       and appending the curve index. This resource is only used if
!       xyLabelMode is set to CUSTOM.
! xy01.xy01Work.xyPlot.xyData.xyExplicitLabels     : NULL
!
!       This resource sets the linewidth scale factor for curves derived
!       from the associated Data Object if xyMonoLineThickness is True. 
! xy01.xy01Work.xyPlot.xyData.xyLineThicknessF     : 1.0
!
!       An array of floating point numbers that are used as linewidth
!       scale factors to use when drawing the corresponding curves in
!       the associated Data Object. If there are more curves then the
!       number of floating point numbers in this resource, the
!       remaining curves will be drawn using the xyLineThicknessF
!       resource. 
!
!       This resource is only used if the xyMonoLineThickness resource
!       is False. 
! xy01.xy01Work.xyPlot.xyData.xyLineThicknesses    : NULL
!
!       If this resource is True, then all the curves in the associated
!       Data Object will be drawn with the width factor specified by
!       the xyLineThicknessF resource. Otherwise, the curves are drawn
!       with the thickness specified by the corresponding index in the 
!       xyLineThicknesses array resource. 
! xy01.xy01Work.xyPlot.xyData.xyMonoLineThickness  : False
!
!       This resource indicates which Marker index all the coordinate
!       pair arrays derived from the associated Data Object will be
!       drawn with if xyMonoMarker is True.
! xy01.xy01Work.xyPlot.xyData.xyMarker             : 0
!
!       An array of Marker indices to use when drawing the corresponding
!       coordinate arrays in the associated Data Object. If there are
!       more coordinate arrays then the number of enumerations in this
!       resource, the remaining coordinate arrays will be drawn using
!       the value of the xyMarker resource.
! xy01.xy01Work.xyPlot.xyData.xyMarkers            : NULL
!
!       If this resource is True, then all the coordinate arrays in the
!       associated Data Object will be drawn with the marker index
!       specified by the xyMarker resource.  Otherwise, the coordinate
!       arrays are drawn with the marker specified by the corresponding
!       index in the xyMarkers array resource.
! xy01.xy01Work.xyPlot.xyData.xyMonoMarker         : False
!
!       This resource indicates what color index all markers drawn
!       using the coordinate arrays from the associated Data Object
!       will be drawn with if xyMonoMarkerColor is True.
! xy01.xy01Work.xyPlot.xyData.xyMarkerColor        : FOREGROUND
!
!       An array of color indices to use when drawing the markers for
!       the corresponding coordinate arrays in the associated Data
!       Object.  If there are more coordinate arrays then the number of
!       color indices in this resource, the remaining coordinate arrays
!       will be drawn using the xyLineColor resource.
! xy01.xy01Work.xyPlot.xyData.xyMarkerColors       : NULL
!
!       If this resource is True, then all the markers for the
!       corresponding coordinate arrays in the associated Data Object
!       will be drawn in the color specified by the xyMarkerColor
!       resource.  Otherwise, each coordinate arrays markers are drawn
!       in the color specified by the corresponding index in the
!       xyMarkerColors array resource.
! xy01.xy01Work.xyPlot.xyData.xyMonoMarkerColor    : False
!
!       This resource indicates the size for all markers drawn using
!       the coordinate arrays from the associated Data Object if
!       xyMonoMarkerColor is True. The size is in NDC.
! xy01.xy01Work.xyPlot.xyData.xyMarkerSizeF        : .01
!
!       An array of floats that indicate the size for the markers in
!       the corresponding coordinate arrays in the associated Data
!       Object. If there are more coordinate arrays then the number of
!       floats in this resource, the remaining coordinate arrays will
!       be drawn using the xyMarkerSizeF resource.
! xy01.xy01Work.xyPlot.xyData.xyMarkerSizes        : NULL
!
!       If this resource is True, then all the markers for the
!       corresponding coordinate arrays in the associated Data Object
!       will be drawn with the size specified by the xyMarkerSizeF
!       resource.  Otherwise, each coordinate arrays markers are drawn
!       with the size specified by the corresponding index in the
!       xyMarkerSizes array resource.
! xy01.xy01Work.xyPlot.xyData.xyMonoMarkerSize     : False
!
!       This resource indicates the thickness for all markers drawn
!       using the coordinate arrays from the associated Data Object if
!       xyMonoMarkerColor is True.
! xy01.xy01Work.xyPlot.xyData.xyMarkerThicknessF   : .01
!
!       An array of floats that indicate the thickness for the markers
!       in the corresponding coordinate arrays in the associated Data
!       Object. If there are more coordinate arrays then the number of
!       floats in this resource, the remaining coordinate arrays will
!       be drawn using the xyMarkerThicknessF resource.
! xy01.xy01Work.xyPlot.xyData.xyMarkerThicknesses  : NULL
!
!       If this resource is True, then all the markers for the
!       corresponding coordinate arrays in the associated Data Object
!       will be drawn with the thickness specified by the
!       xyMarkerThicknessF resource.  Otherwise, each coordinate
!       arrays markers are drawn with the thickness specified by the
!       corresponding index in the xyMarkerThicknesses array resource.
! xy01.xy01Work.xyPlot.xyData.xyMonoMarkerThickness: True
!
! LogLinTransformation resources
! ========================
!
!       If this resource is False the xyPlot sets its left extent to be
!       trXMinF and its right extent to be trXMaxF. If this resource is
!       True, the X-axis is inverted so that trXMinF is on the right,
!       and trXMaxF is on the left.
! xy01.xy01Work.xyPlot.trXReverse                  : False
!
!       If this resource is False, the xyPlot sets its bottom extent to
!       be trYMinF and the top extent to be trYMaxF.  If this resource
!       is True, the Y-axis is inverted so that trYMinF is on the top,
!       and trYMaxF is on the bottom.
! xy01.xy01Work.xyPlot.trYReverse                  : False
!
!       This resource specifies the minimum value that will be
!       displayed along the X-axis of the plot.  The trXReverse
!       resource controls if that is on the left or right side of the
!       plot.
!
!       If the xyComputeXMin resource is True, then this resource gets
!       re-calculated each time the xyCoordData resource changes.
!       Otherwise, this resource gets the minimum value of the first
!       data specified, and retains that value.
! xy01.xy01Work.xyPlot.trXMinF                     : dynamic
!
!       This resource specifies the maximum value that will be
!       displayed along the X-axis of the plot.  The trXReverse
!       resource controls if that is on the left or right side of the
!       plot.
!
!       If the xyComputeXMax resource is True, then this resource gets
!       re-calculated each time the xyCoordData resource changes.
!       Otherwise, this resource gets the maximum value of the first
!       data specified, and retains that value.
! xy01.xy01Work.xyPlot.trXMaxF                     : dynamic
!
!       This resource specifies the minimum value that will be
!       displayed along the Y-axis of the plot.  The trYReverse
!       resource controls if that is on the top or bottom side of the
!       plot.
!
!       If the xyComputeYMin resource is True, then this resource gets
!       re-calculated each time the xyCoordData resource changes.
!       Otherwise, this resource gets the minimum value of the first
!       data specified, and retains that value.
! xy01.xy01Work.xyPlot.trYMinF                     : dynamic
!
!       This resource specifies the maximum value that will be
!       displayed along the y axis of the plot.  The trYReverse
!       resource controls if that is on the top or bottom side of the
!       plot.
! 
!       If the xyComputeYMax resource is True, then this resource gets
!       re-calculated each time the xyCoordData resource changes.
!       Otherwise, this resource gets the maximum value of the first
!       data specified, and retains that value.
! xy01.xy01Work.xyPlot.trYMaxF                     : dynamic
!
! PlotManager resources
! =================
!
!       These resources determine whether the PlotManager
!       displays a Title, TickMark, Legend, and/or LabelBar object.
!       Each one has four settings: 
!
!       NOCREATE 
!         If pm*DisplayMode has this value when the Plot object
!         is created, the PlotManager does
!         not create that object. The Base Plot will never be able to 
!         display an object belonging to itself during its lifetime, 
!         and attempts  to change the value of pm*DisplayMode later
!         will fail with an WARNING message. However, the Base Plot 
!         still would be able display an object belonging to a Plot added
!         as an Overlay. 
!       NEVER 
!         If pm*DisplayMode has this value when the Plot is created, 
!         the PlotManager does 
!         create the object, but it will not be drawn. There is 
!         no restriction on changing the value of pm*DisplayMode
!         at a later time. 
!       ALWAYS 
!         If pm*DisplayMode has this value when the Base Plot 
!         is created, the PlotManager 
!         creates the object that will be drawn assuming it meets 
!         all conditions of displayability set by the object 
!         itself. There is no restriction on changing the value of 
!         pm*DisplayMode at a later time. 
!       CONDITIONAL 
!         If pm*DisplayMode has this value when the Base Plot 
!         is created, the PlotManager 
!         creates the object that may be drawn assuming it meets all 
!         conditions of displayability set by the object 
!         itself. If the object initially belongs to a PlotManager 
!         whose base plot becomes a member of a compound plot 
!         plot, the object will be displayed only if no other
!         objects belonging to other plots in the compound plot 
!         have already been displayed. The sequential ordering of plots
!         belonging to the compound plot determines the order in
!         which objects are considered. 
! xy01.xy01Work.xyPlot.pmTitleDisplayMode          : NOCREATE 
! xy01.xy01Work.xyPlot.pmTickMarkDisplayMode       : NOCREATE 
! xy01.xy01Work.xyPlot.pmLegendDisplayMode         : NOCREATE 
! xy01.xy01Work.xyPlot.pmLabelBarDisplayMode       : NOCREATE 
!



