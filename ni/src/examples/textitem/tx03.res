!
! Example:  This resource file shows all TextItem resources and
!           their default values.
!
!  ==============================================================
!  The following resources relate to the viewport on the drawing
!  canvas.
!  ==============================================================
!
!       Height in NDC (0. to 1.) of the viewport
! tx03.tx03Work.TextItems.vpHeightF                   : 0.8
!
!       Switch to allow the plot X to Y aspect ratio to remain fixed
!       when resizing or moving the object.
! tx03.tx03Work.TextItems.vpKeepAspect                : False
!
!       Save this graphical object as a GKS segment
! tx03.tx03Work.TextItems.vpUseSegments               : False
!
!       Width in NDC (0. to 1.) of the viewport
! tx03.tx03Work.TextItems.vpWidthF                    : 0.8
!
!       NDC (0. to 1.) X location of the upper left corner of the plot
! tx03.tx03Work.TextItems.vpXF                        : 0.1
!
!       NDC (0. to 1.) Y location of the upper left corner of the plot
! tx03.tx03Work.TextItems.vpYF                        : 0.9
!
!
!  ==============================================================
!  The following resources relate to the location and orientation
!  of the text string.
!  ==============================================================
!
!       Sets the X location of the justification point in NDCs.
!                                         default = 0.
tx03.tx03Work.TextItems.txPosXF              : 0.5
!
!       Sets the Y location of the justification point in NDCs.
!                                         default = 1.
tx03.tx03Work.TextItems.txPosYF              : 0.5
!
!       Sets the rotation angle of the text around the justification
!       point. The angle is counter-clockwise and 0 degrees is
!       horizontal on the screen.
!                                         default = 0.
tx03.tx03Work.TextItems.txAngleF             : 45.0
!
!       How text is justified within the text bounding box
!       TOPLEFT              = 0,
!       CENTERLEFT           = 1,
!       BOTTOMLEFT           = 2,
!       TOPCENTER            = 3,
!       CENTERCENTER         = 4,
!       BOTTOMCENTER         = 5,
!       TOPRIGHT             = 6,
!       CENTERRIGHT          = 7,
!       BOTTOMRIGHT          = 8
!
! tx03.tx03Work.TextItems.txJust               : 4
!
!       Direction in which text is to be drawn   (UP, DOWN, ACROSS)
! tx03.tx03Work.TextItems.txDirection          : ACROSS
!
!
!  ==============================================================
!  The following resources relate to the appearance of the text
!  ==============================================================
!
!       The text string to be written
tx03.tx03Work.TextItems.txString              : NCAR Graphics
!
!       The type of character font where the options include:
! |-------------------------------------------------------|
! | INDEX | TYPE          | NAME                          |
! |=======================================================|
! |  0    | stroked       | pwritx_database               |
! |-------------------------------------------------------|
! |  1    | stroked       | default   (See txFont)        |
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
! |-------------------------------------------------------|
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
! The default font, the pwritx_database font (font index 0),
! differs from all other fonts in that it intrinsically contains
! many more characters than the other fonts (564 as opposed
! to somewhere between 97 and 128). A number of the Text
! Function Codes allow special access to the characters of the
! pwritx_database font.
!
!                                         default = 1
tx03.tx03Work.TextItems.txFont                : 22
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
!       The color of the text to be written
!                                         default = 1
tx03.tx03Work.TextItems.txFontColor           : 2
!
!       The character height in NDCs of the text to be written
! tx03.tx03Work.TextItems.txFontHeightF         : .05
!
!       The height/width aspect ratio of the text to be written
! tx03.tx03Work.TextItems.txFontAspectF         : 1.3125
!
!       The width of the lines that form the text outline
! tx03.tx03Work.TextItems.txFontThicknessF      : 1.0
!
!       The degree of quality of the text to be written (LOW,MEDIUM,HIGH)
! tx03.tx03Work.TextItems.txFontQuality         : HIGH
!
!       The spacing between characters of the text to be written
! tx03.tx03Work.TextItems.txConstantSpacingF    : 0.0
!
!       A process for accessing the many characters of the default
!       pwritx font.
! tx03.tx03Work.TextItems.txFuncCode            : :
!
!  ==============================================================
!  The following resources relate to the bounding box of the text
!  ==============================================================
!
!       Draw the text bounding box
!                                         default = False
tx03.tx03Work.TextItems.txPerimOn             : True
!
!       Color of the text bounding box
!                                         default = FOREGROUND
tx03.tx03Work.TextItems.txPerimColor          : 1
!
!       Width of the text bounding box
! tx03.tx03Work.TextItems.txPerimThicknessF     : 1.0
!
!       Dash pattern of the text bounding box
! tx03.tx03Work.TextItems.txPerimDashPattern    : 0
!
!       Length of dash segments of the text bounding box
! tx03.tx03Work.TextItems.txPerimDashLengthF    : 0.15
!
!       Length between dash segments of the text bounding box
! tx03.tx03Work.TextItems.txPerimSpaceF         : 0.5
!
!       Fill color of the interior of the bounding box
!                                         default = TRANSPARENT
tx03.tx03Work.TextItems.txBackgroundFillColor : 44
