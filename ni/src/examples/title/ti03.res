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
! *ti03Work.Titles.vpHeightF                   : 0.8
!
!       Switch to allow the plot X to Y aspect ratio to remain fixed
!       when resizing or moving the object.
! *ti03Work.Titles.vpKeepAspect                : False
!
!       Save this graphical object as a GKS segment
! *ti03Work.Titles.vpUseSegments               : False
!
!       Width in NDC (0. to 1.) of the viewport
! *ti03Work.Titles.vpWidthF                    : 0.8
!
!       NDC (0. to 1.) X location of the upper left corner of the plot
! *ti03Work.Titles.vpXF                        : 0.1
!
!       NDC (0. to 1.) Y location of the upper left corner of the plot
! *ti03Work.Titles.vpYF                        : 0.9
!
!  ==============================================================
!  This object draws a main title, X axis title, & a Y axis title
!  ==============================================================
!  The following resources relate to all three titles.
!  ==============================================================
!
!       Sets the title offset from the viewport.  The offset is
!       tiDeltaF * ti...FontHeightF in NDC units.
! *ti03Work.Titles.tiDeltaF             : 1.5
!
!       When true, the X and Y titles inherit the resources set
!       for the main title.
! *ti03Work.Titles.tiUseMainAttributes  : False
!
!  ==============================================================
!  The following resources relate to the location and orientation
!  of the main, X, and Y titles.         ------------------------
!  ==============================================================
!
!  Setting this boolean resource to True causes the Main title to
!  appear. Setting it False turns the Main title off. By default,
!  tiMainOn will be set True if and only if you explicitly set
!  the title String.
! *ti03Work.Titles.tiMainOn             : False
! *ti03Work.Titles.tiXAxisOn            : False
! *ti03Work.Titles.tiYAxisOn            : False
!
!  Sets the string to use for the titles. The string may contain
!  Text Function Codes that allow you to control typographical
!  attributes such as subscripting and superscripting, change
!  fonts within a string, embed newlines, etc.
*ti03Work.Titles.tiMainString         : Title Resource Setting
*ti03Work.Titles.tiXAxisString        : Amounts of Chlorine Released
*ti03Work.Titles.tiYAxisString        : Total Ozone Concentration
!
!  Specifies the side of the viewport relative to which the
!  justification point of the title is located. Main and XAxis
!  can be TOP or BOTTOM. YAxis can be LEFT or RIGHT.
! *ti03Work.Titles.tiMainSide           : TOP
! *ti03Work.Titles.tiXAxisSide          : BOTTOM
! *ti03Work.Titles.tiYAxisSide          : LEFT
!
!  Specifies the base location of the justification point for a
!  title with respect to the viewport boundaries. There are three
!  choices: LEFT, CENTER, RIGHT
! *ti03Work.Titles.tiMainPosition       : CENTER
! *ti03Work.Titles.tiXAxisPosition      : CENTER
! *ti03Work.Titles.tiYAxisPosition      : CENTER
!
!       Sets an offset in NDC coordinates by which a title
!  will be displaced in a a direction parallel to the X-Axis from
!  its base location. This offset goes beyond any offset due to
!  tiDeltaF. It does not scale in response to a changing viewport.
! *ti03Work.Titles.tiMainOffsetXF       : 0.0
! *ti03Work.Titles.tiXAxisOffsetXF      : 0.0
! *ti03Work.Titles.tiYAxisOffsetXF      : 0.0
!
!       Sets an offset in NDC coordinates by which a title
!  will be displaced in a a direction parallel to the Y-Axis from
!  its base location. This offset goes beyond any offset due to
!  tiDeltaF. It does not scale in response to a changing viewport.
! *ti03Work.Titles.tiMainOffsetYF       : 0.0
! *ti03Work.Titles.tiXAxisOffsetYF      : 0.0
! *ti03Work.Titles.tiYAxisOffsetYF      : 0.0
!
!       How text is justified within the text bounding box
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
! *ti03Work.Titles.tiMainJust           : CENTERCENTER
! *ti03Work.Titles.tiXAxisJust          : CENTERCENTER
! *ti03Work.Titles.tiYAxisJust          : CENTERCENTER
!
!       Angle of counter-clockwise rotation of a title.
! *ti03Work.Titles.tiMainAngleF         : 0.0
! *ti03Work.Titles.tiXAxisAngleF        : 0.0
! *ti03Work.Titles.tiYAxisAngleF        : 90.0
!
!       Direction in which text is to be drawn   (UP, DOWN, ACROSS)
! *ti03Work.Titles.tiMainDirection      : ACROSS
! *ti03Work.Titles.tiXAxisDirection     : ACROSS
! *ti03Work.Titles.tiYAxisDirection     : ACROSS
!
!
!  =============================================================
!  The following resources relate to the appearance of the main,
!  X, and Y titles.                      ----------
!  =============================================================
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
! The default pwritx_database font contains many characters
! (564) which can be accessed using the FuncCode resource.
!
*ti03Work.Titles.tiMainFont             : 30
*ti03Work.Titles.tiXAxisFont            : 22
*ti03Work.Titles.tiYAxisFont            : 26
!
!     Color indices are taken from the default HLU colormap.  Choices are:
!
!     Index   Color     Index   Color    Index  Color       Index   Color
!
!       1-4   white      5-18  yellows   19-21  tans        22-24   oranges
!     29-32   magentas     34  lavender  40-50  reds           57   gray
!     60-66   cyans     70-80  greens    81-90  dark grns  91-100   blues
!   106-114   blacks
!
*ti03Work.Titles.tiMainFontColor        : 20
*ti03Work.Titles.tiXAxisFontColor       : 23
*ti03Work.Titles.tiYAxisFontColor       : 65
!
!       The character height in NDCs of the text to be written
*ti03Work.Titles.tiMainFontHeightF      : .04
*ti03Work.Titles.tiXAxisFontHeightF     : .03
*ti03Work.Titles.tiYAxisFontHeightF     : .03
!
!       The height/width aspect ratio of the text to be written
! *ti03Work.Titles.tiMainFontAspectF      : 1.3125
! *ti03Work.Titles.tiXAxisFontAspectF     : 1.3125
! *ti03Work.Titles.tiYAxisFontAspectF     : 1.3125
!
!       The width of the lines that form the text outline
*ti03Work.Titles.tiMainFontThicknessF       : 2.0
*ti03Work.Titles.tiXAxisFontThicknessF      : 2.0
*ti03Work.Titles.tiYAxisFontThicknessF      : 2.0
!
!       The degree of quality of the text to be written (LOW,MEDIUM,HIGH)
! *ti03Work.Titles.tiMainFontQuality      : HIGH
! *ti03Work.Titles.tiXAxisFontQuality     : HIGH
! *ti03Work.Titles.tiYAxisFontQuality     : HIGH
!
!       The spacing between characters of the text to be written
! *ti03Work.Titles.tiMainConstantSpacingF : 0.0
! *ti03Work.Titles.tiXAxisConstantSpacingF: 0.0
! *ti03Work.Titles.tiYAxisConstantSpacingF: 0.0
!
!       A process for accessing the many characters of the default
!       pwritx font.
! *ti03Work.Titles.tiMainFuncCode         : :
! *ti03Work.Titles.tiXAxisFuncCode        : :
! *ti03Work.Titles.tiYAxisFuncCode        : :
!
