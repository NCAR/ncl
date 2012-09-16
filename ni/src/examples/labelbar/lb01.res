!
! Example:  This resource file shows all LabelBar resources and
!           their default values.
!
!  ==============================================================
!  The following resources relate to the viewport on the drawing
!  canvas.
!  ==============================================================
!
!       Height in NDC (0. to 1.) of the viewport
! *lb01Work.LabelBar.vpHeightF                   : 0.8
!
!       Switch to allow the plot X to Y aspect ratio to remain fixed
!       when resizing or moving the object.
! *lb01Work.LabelBar.vpKeepAspect                : False
!
!       Save this graphical object as a GKS segment
! *lb01Work.LabelBar.vpUseSegments               : False
!
!       Width in NDC (0. to 1.) of the viewport
! *lb01Work.LabelBar.vpWidthF                    : 0.8
!
!       NDC (0. to 1.) X location of the upper left corner of the plot
! *lb01Work.LabelBar.vpXF                        : 0.1
!
!       NDC (0. to 1.) Y location of the upper left corner of the plot
! *lb01Work.LabelBar.vpYF                        : 0.9
!
! ----------------------
! THE COMPOSITE LABELBAR
! ----------------------
!
!       Switch to turn on/off drawing of the labelbar
! *lb01Work.LabelBar.lbLabelBar            : True
!
!       Is title and label sizing to be done automatically
! *lb01Work.LabelBar.lbAutoManage          : True
!
!       Should the labelbar be HORIZONTAL or VERTICAL
! *lb01Work.LabelBar.lbOrientation         : VERTICAL
!
!   When the label bar changes size the justification determines
!   a fixed point about which the size change occurs. Any of the
!   corners, the center of any edge, or the current center of the
!   LabelBar may be set to the fixed justification point.
!
!       TOPLEFT
!       CENTERLEFT
!       BOTTOMLEFT
!       TOPCENTER
!       CENTERCENTER
!       BOTTOMCENTER
!       TOPRIGHT
!       CENTERRIGHT
!       BOTTOMRIGHT
!
! *lb01Work.LabelBar.lbJustification       : BOTTOMLEFT
!
!   The color index used for the background of all the boxes in
!   the LabelBar when the fill type is not solid.
! *lb01Work.LabelBar.lbFillBackground      : TRANSPARENT
!
!       Not currently defined in the HLU Reference Manual
! *lb01Work.LabelBar.lbMarginMode          : 0
!
!       Distance from LabelBar perimeter to the LabelBar information
!       as a fraction of the smaller perim dimension.
! *lb01Work.LabelBar.lbLeftMarginF         : .05
!
!       Distance from LabelBar perimeter to the LabelBar information
!       as a fraction of the smaller perim dimension.
! *lb01Work.LabelBar.lbRightMarginF         : .05
!
!       Distance from LabelBar perimeter to the LabelBar information
!       as a fraction of the smaller perim dimension.
! *lb01Work.LabelBar.lbBottomMarginF         : .05
!
!       Distance from LabelBar perimeter to the LabelBar information
!       as a fraction of the smaller perim dimension.
! *lb01Work.LabelBar.lbTopMarginF         : .05
!
! ------------------
! THE LABELBAR TITLE
! ------------------
!
!       Draw the labelbar title (default is True)
! *lb01Work.LabelBar.lbDrawTitle           : 1
!
!       If a string is set DrawTitle defaults to True.  Else
!       DrawTitle defaults to False.  If DrawTitle is set to
!       True, but no string is entered into TitleString, then
!       a title consisting of the name of the current instantiation
!       of the object is inserted into TitleString.
!
! *lb01Work.LabelBar.lbTitleString          :
!
!       Determines where the title goes relative to the labelbar object.
!       Options are TOP, BOTTOM, LEFT, and RIGHT.
! *lb01Work.LabelBar.lbTitlePosition        : TOP
!
!       Determines the NDC distance between the title and the label/box info.
! *lb01Work.LabelBar.lbTitleOffsetF         : .03
!
!       NDC extent allocated for the title when AutoManage is on.
! *lb01Work.LabelBar.lbTitleExtentF      : .15
!
!       Rotation angle of the title
! *lb01Work.LabelBar.lbTitleAngleF          : 0.
!
!       Direction the title is to be drawn
!       Defaults to ACROSS when TitlePosition = TOP or BOTTOM
!       Defaults to DOWN when TitlePosition = LEFT or RIGHT
! *lb01Work.LabelBar.lbTitleDirection       : ACROSS
!
!       How the title text is justified within the text bounding box
!       TOPLEFT
!       CENTERLEFT
!       BOTTOMLEFT
!       TOPCENTER
!       CENTERCENTER
!       BOTTOMCENTER
!       TOPRIGHT
!       CENTERRIGHT
!       BOTTOMRIGHT
!
! *lb01Work.LabelBar.lbTitleJust           : CENTERCENTER
!
!       The type of character font where the options include:
! |-------------------------------------------------------|
! | INDEX | TYPE          | NAME                          |
! |=======================================================|
! |  0    | stroked       | pwritx_database               |
! |-------------------------------------------------------|
! |  1    | stroked       | default                       |
! |-------------------------------------------------------|
! |  2    | stroked       | cartographic_roman            |
! |-------------------------------------------------------|
! |  3    | stroked       | cartographic_greek            |
! |-------------------------------------------------------|
! |  4    | stroked       | simplex_roman                 |
! |-------------------------------------------------------|
! |  5    | stroked       | simplex_greek                 |
! |-------------------------------------------------------|
! |  6    | stroked       | simplex_script                |
! |-------------------------------------------------------|
! |  7    | stroked       | complex_roman                 |
! |-------------------------------------------------------|
! |  8    | stroked       | complex_greek                 |
! |-------------------------------------------------------|
! |  9    | stroked       | complex_script                |
! |-------------------------------------------------------|
! | 10    | stroked       | complex_italic                |
`! |-------------------------------------------------------|
! | 11    | stroked       | complex_cyrillic              |
! |-------------------------------------------------------|
! | 12    | stroked       | duplex_roman                  |
! |-------------------------------------------------------|
! | 13    | stroked       | triplex_roman                 |
! |-------------------------------------------------------|
! | 14    | stroked       | triplex_italic                |
! |-------------------------------------------------------|
! | 15    | stroked       | gothic_german                 |
! |-------------------------------------------------------|
! | 16    | stroked       | gothic_english                |
! |-------------------------------------------------------|
! | 17    | stroked       | gothic_italian                |
! |-------------------------------------------------------|
! | 18    | stroked       | math_symbols                  |
! |-------------------------------------------------------|
! | 19    | stroked       | symbol_set1                   |
! |-------------------------------------------------------|
! | 20    | stroked       | symbol_set2                   |
! |-------------------------------------------------------|
! | 21    | filled        | helvetica                     |
! |-------------------------------------------------------|
! | 22    | filled        | helvetica-bold                |
! |-------------------------------------------------------|
! | 25    | filled        | times-roman                   |
! |-------------------------------------------------------|
! | 26    | filled        | times-bold                    |
! |-------------------------------------------------------|
! | 29    | filled        | courier                       |
! |-------------------------------------------------------|
! | 30    | filled        | courier-bold                  |
! |-------------------------------------------------------|
! | 33    | filled        | greek                         |
! |-------------------------------------------------------|
! | 34    | filled        | math-symbols                  |
! |-------------------------------------------------------|
! | 35    | filled        | text-symbols                  |
! |-------------------------------------------------------|
! | 36    | filled        | weather1                      |
! |-------------------------------------------------------|
! | 37    | filled        | weather2                      |
! +-------------------------------------------------------+
! The default pwritx_database font contains many characters
! (564) which can be accessed using the FuncCode resource.
!
! *lb01Work.LabelBar.lbTitleFont            : 1
!
!     Color indices are taken from the default HLU colormap.
!
! *lb01Work.LabelBar.lbTitleFontColor       : FOREGROUND
!
!       The character height in NDCs of the text to be written
! *lb01Work.LabelBar.lbTitleFontHeightF     : .025
!
!       The height/width aspect ratio of the text to be written
! *lb01Work.LabelBar.lbTitleFontAspectF     : 1.0
!
!       The width of the lines that form the text outline
! *lb01Work.LabelBar.lbTitleFontThicknessF  : 1.0
!
!       The degree of quality of the text to be written (LOW,MEDIUM,HIGH)
! *lb01Work.LabelBar.lbTitleFontQuality     : HIGH
!
!       The spacing between characters of the text to be written
! *lb01Work.LabelBar.lbTitleConstantSpacingF: 0.0
!
!       A process for accessing the many characters of the default
!       pwritx font.
! *lb01Work.LabelBar.lbTitleFuncCode        : :
!
!
! ------------------
! THE LABELBAR PERIM
! ------------------
!
!       Draw the labelbar perim
! *lb01Work.LabelBar.lbDrawPerim           : False
!
!       Color of the labelbar perim
! *lb01Work.LabelBar.lbPerimColor          : FOREGROUND
!
!       Fill style of the labelbar perim
! *lb01Work.LabelBar.lbPerimFill           : FOREGROUND
!
!       Fill color of the labelbar perim
! *lb01Work.LabelBar.lbPerimFillColor      : FOREGROUND
!
!       Width of the labelbar perim
! *lb01Work.LabelBar.lbPerimThicknessF     : 1.0
!
!       Dash pattern of the labelbar perim
! *lb01Work.LabelBar.lbPerimDashPattern    : 0
!
!       Length of dash segments of the labelbar perim
! *lb01Work.LabelBar.lbPerimDashLengthF    : 0.15
!
! ------------------------
! THE LABELBAR ITEM BOXES
! ------------------------
!
!       A relative box width.  0. = no width, 1. = touching boxes
! *lb01Work.LabelBar.lbBoxMajorExtentF     : 1.0
!
!       A relative box length in a box + label pair.
!       If set to 1.0 it crowds out the label.
! *lb01Work.LabelBar.lbBoxMinorExtentF     : 0.33
!
!       The number of boxes in the labelbar.
! *lb01Work.LabelBar.lbBoxCount            : 16
!
!   When set to UNIFORMSIZING all boxes have the same size.
!   EXPLICITSIZING allows the values in the array, lbBoxFractions
!   to set the relative size of each box.
! *lb01Work.LabelBar.lbBoxSizing           : UNIFORMSIZING
!
!   When BoxSizing = EXPLICITSIZING (BoxCount+1) values starting at 0.
!   and ending at 1.0 are used to set the relative size of each box.
! *lb01Work.LabelBar.lbBoxFractions        : NULL
!
!       Fill pattern of the interior of the labelbar boxes
! *lb01Work.LabelBar.lbMonoFillPattern     : False
!
!       Fill color of the interior of the labelbar boxes
! *lb01Work.LabelBar.lbMonoFillColor       : False
!
!       Fill scale of the interior of the labelbar boxes
! *lb01Work.LabelBar.lbMonoFillScale       : True
!
!       Fill pattern of the interior of the labelbar boxes
! *lb01Work.LabelBar.lbFillPatterns        : SolidFill
!
!       Fill color of the interior of the labelbar boxes
! *lb01Work.LabelBar.lbFillColors          : False
!
!       Fill scale of the interior of the labelbar boxes
! *lb01Work.LabelBar.lbFillScales          : True
!
!       Line width when the fill style of labelbar boxes is a pattern
! *lb01Work.LabelBar.lbFillLineThicknessF  : 1.0
!
! ------------------
! THE BOX PERIMETERS
! ------------------
!
!       Draw the box perimeters
! *lb01Work.LabelBar.lbDrawBoxLines        : 1
!
!       Color of the box perimeter
! *lb01Work.LabelBar.lbBoxLineColor          : FOREGROUND
!
!       Width of the box perimeter
! *lb01Work.LabelBar.lbBoxLineThicknessF     : 1.0
!
!       Dash pattern of the box perimeter
! *lb01Work.LabelBar.lbBoxLineDashPattern    : 0
!
!       Length of dash segments of the box perimeter
! *lb01Work.LabelBar.lbBoxLineDashLengthF    : 0.15
!
! ------------------------
! THE LABELBAR ITEM LABELS
! ------------------------
!
!       Quick switch for turning labels on/off
! *lb01Work.LabelBar.lbDrawLabels              : True
!
!       Determines the position of the labels with respect to the
!  LabelBar boxes.  If orientation is HORIZONTAL valid values are TOP,
!  CENTER, or BOTTOM.  If the orientation is VERTICAL valid values are
!  LEFT, CENTER, and RIGHT.
! *lb01Work.LabelBar.lbLabelPosition           : RIGHT
!
!       Angle at which the scale labels are drawn
! *lb01Work.LabelBar.lbLabelAngleF             : 0.0
!
!       Determines the alignment of the labels to the boxes.
!       BOXCENTERS    - BoxCount labels centered at the boxes.
!       INTERIOREDGES - BoxCount-1 labels starting at the bottom of box 1.
!       EXTERNALEDGES - BoxCount+1 labels starting at the top of box 1.
!
! *lb01Work.LabelBar.lbLabelAlignment          : BOXCENTERS
!
!       Direction in which scale labels are drawn (UP,DOWN,ACROSS)
!       Normal direction is across for all axes.
! *lb01Work.LabelBar.lbLabelDirection          : ACROSS
!
!       How label text is justified in a text extent rectangle
! *lb01Work.LabelBar.lbLabelJust               : CENTERCENTER
!
!       The font style to use for labels
! *lb01Work.LabelBar.lbLabelFont               : 1
!
!       The color for label text
! *lb01Work.LabelBar.lbLabelFontColor          : FOREGROUND
!
!       The NDC (0. to 1.) height of the label text
! *lb01Work.LabelBar.lbLabelFontHeightF        : .02
!
!       The height to width aspect ratio of label characters
! *lb01Work.LabelBar.lbLabelFontAspectF        : 1.0
!
!       The width of the lines that form the text outline
! *lb01Work.LabelBar.lbLabelFontThicknessF      : 1.0
!
!       The degree of quality of the text to be written (LOW,MEDIUM,HIGH)
! *lb01Work.LabelBar.lbLabelFontQuality         : HIGH
!
!       The spacing between characters of the text to be written
! *lb01Work.LabelBar.lbLabelConstantSpacingF    : 0.0
!
!       A process for accessing the many characters of the default
!       pwritx font.
! *lb01Work.LabelBar.lbLabelFuncCode            : :
!
!       The stride between labeled bars
! *lb01Work.LabelBar.lbLabelStride             : 1
!
!       Fractional distance between the box and the label
! *lb01Work.LabelBar.lbLabelOffsetF            : 0.
!
!       An array of label strings
! *lb01Work.LabelBar.lbLabelStrings            : Label_

*wkColorMap : default