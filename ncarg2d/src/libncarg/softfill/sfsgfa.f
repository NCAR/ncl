C
C	$Id: sfsgfa.f,v 1.1.1.1 1992-04-17 22:32:53 ncargd Exp $
C
C***********************************************************************
C S O F T F I L L   -   I N T R O D U C T I O N
C***********************************************************************
C
C This file contains materials for a software area-fill package called
C SOFTFILL.  Double-spaced headers like the one above set off major
C portions of the file.  Included are implementation instructions, a
C write-up, user-level routines, and internal routines.
C
C***********************************************************************
C S O F T F I L L   -   I M P L E M E N T A T I O N
C***********************************************************************
C
C The master version of SOFTFILL is written in IFTRAN, an extended form
C of FORTRAN which provides many conveniences.  Running it through the
C IFTRAN preprocessor yields a standard FORTRAN 77 file, which is the
C version distributed as a part of NCAR Graphics.  If the file in which
C you are reading these words begins with
C
C    .OP LS=10001 LI=1 CB RT OC UC=0
C
C it is the IFTRAN version; otherwise, it is the FORTRAN 77 version.
C
C SOFTFILL requires various parts of the NCAR Graphics package to have
C been implemented (in particular, it uses the support routine SETER
C and various routines from SPPS).
C
C***********************************************************************
C S O F T F I L L   -   W R I T E - U P
C***********************************************************************
C
C SUBROUTINE SFSGFA (XRA,YRA,NRA,DST,NST,IND,NND,ICI)
C
C DIMENSION OF          XRA(NRA), YRA(NRA), DST(NST), IND(NND), where
C ARGUMENTS             NST=NRA+NIM and NND=NRA+2*NIM, where "NIM" is
C                       as defined under "ARGUMENTS (INPUT)", below.
C
C LATEST REVISION       June, 1989.
C
C PURPOSE               To fill, in one of various ways, an area of the
C                       plotter frame defined by a given set of points.
C                       SFSGFA (which stands for "SOFTFILL - Simulate
C                       GFA") is intended to provide a way to use solid
C                       color fill, if that capability is available, or
C                       a suitable pattern-fill substitute, otherwise.
C                       If all area fills are done using SFSGFA, then
C                       the way in which they are done can be changed
C                       by modifying the value of a single internal
C                       parameter.
C
C USAGE                 CALL SFSGFA (XRA,YRA,NRA,DST,NST,IND,NND,ICI)
C
C                       This call causes the area defined by the points
C                       "((XRA(I),YRA(I)),I=1,NRA)", which are given in
C                       the current world coordinate system, to be
C                       filled.  The values of the internal parameter
C                       'TY' (for 'TYPE OF FILL') and the argument ICI
C                       determine how the fill should be done, as
C                       follows:
C
C                       When 'TY' has the value 0 (the default), then
C                       color fill (by means of a call to GFA) is used.
C                       ICI, if zero or greater, specifies the fill
C                       area color index.  A negative value of ICI
C                       specifies that the fill area color index is not
C                       to be set before the call to GFA.
C
C                       When 'TY' has the value 1, then the area is
C                       filled with parallel lines (by means of a call
C                       to SFWRLD).  ICI, if zero or greater, specifies
C                       the polyline color index.  A negative value of
C                       ICI specifies that the polyline color index is
C                       not to be set before the call to SFWRLD.
C
C                       When 'TY' has the value 2, then the area is
C                       filled with parallel lines (by means of a call
C                       to SFWRLD) and it is then filled again with
C                       parallel lines perpendicular to the first set
C                       (by means of a call to SFNORM).  ICI, if zero
C                       or greater, specifies the polyline color index.
C                       A negative value of ICI specifies that the
C                       polyline color index is not to be set before
C                       the call to SFWRLD.
C
C                       When 'TY' has a negative value between -4 and
C                       -1, one or more calls to SFWRLD and/or SFNORM
C                       are used to fill the area with lines forming a
C                       pattern.  ICI is used to select the pattern.
C                       A zero or negative value of ICI will select a
C                       blank pattern.  Positive values of ICI will
C                       select patterns which increase in density as
C                       the value of ICI increases.  The absolute
C                       value of 'TY' specifies which set of patterns
C                       is to be used; the largest usable value of ICI
C                       is approximately 5*ABS('TY'), beyond that, the
C                       pattern becomes essentially solid.  The fill
C                       patterns generated may be described as follows:
C                       Let "n" represent the value ABS('TY').  For
C                       ICI = 0, no fill lines are drawn at all.  For
C                       each of the next "n" values of ICI, the pattern
C                       is elaborated by the addition of fill lines,
C                       32*'SP' units apart, at one of the angles 'AN',
C                       'AN'+180/n, 'AN'+2*180/n, ... .  For each of
C                       the next "n" values of ICI, the pattern is
C                       elaborated by halving the distance between fill
C                       lines at one of the angles.  This final step is
C                       then repeated ad infinitum.
C
C                       Values of 'TY' not specifically mentioned above
C                       are reserved for future use.  (At the moment,
C                       values greater than 2 will be treated as equal
C                       to 2 and values less than -4 will be treated as
C                       equal to -4, but this may not be true in the
C                       future.)
C
C                       The routines SFGETI, SFGETR, SFSETI, and SFSETR
C                       are used to access the parameters 'TY', 'AN',
C                       and 'SP'.
C
C ARGUMENTS (INPUT)     XRA - an array containing the X coordinates of
C                       the points defining the area to be filled, in
C                       the world coordinate system (as defined by the
C                       current viewport and window - these may be set
C                       by GKS calls or by a call to the SPPS routine
C                       SET).
C
C                       YRA - an array containing the Y coordinates of
C                       the points defining the area to be filled, in
C                       the world coordinate system (as defined by the
C                       current viewport and window - these may be set
C                       by GKS calls or by a call to the SPPS routine
C                       SET).
C
C                       NRA - the number of points defining the area
C                       to be filled.  NRA must be greater than two.
C
C                       DST - a real scratch array, dimensioned "NST".
C                       This array is only used when fill lines are
C                       generated by means of calls to SFWRLD and/or
C                       SFNORM.
C
C                       NST - the dimension of DST - must be greater
C                       than or equal to "NRA+NIM", where "NIM" is the
C                       largest number of intersection points of any
C                       fill line with the boundary lines.  To be sure
C                       DST is large enough, use NIM=NRA.  In practice,
C                       NIM rarely needs to be that large.
C
C                       IND - an integer scratch array, dimensioned
C                       "NND".  This array is only used when fill lines
C                       are generated by means of calls to SFWRLD and/or
C                       SFNORM.
C
C                       NND - the dimension of IND - must be greater
C                       than or equal to "NRA+2*NIM", where "NIM" is
C                       the largest number of intersection points of
C                       any fill line with the boundary lines.  To be
C                       sure IND is large enough, use NIM=NRA.  In
C                       practice, NIM rarely needs to be that large.
C
C                       ICI - nominally, the fill-area color index to
C                       be used.  When the internal parameter 'TY' has
C                       a value other than zero, ICI may be used in
C                       some other way (as described above).
C
C ARGUMENTS (OUTPUT)    XRA - the input X coordinates, converted to
C                       normalized device coordinate units.
C
C                       YRA - the input Y coordinates, converted to
C                       normalized device coordinate units.
C
C                       NRA - unchanged.
C
C                       DST - overstored or unchanged, depending on the
C                       values of 'TY' and ICI.
C
C                       NST - unchanged.
C
C                       IND - overstored or unchanged, depending on the
C                       values of 'TY' and ICI.
C
C                       NND - unchanged.
C
C                       ICI - unchanged.
C
C ENTRY POINTS          SFSGFA.
C
C COMMON BLOCKS         SFCOMN.
C
C I/O                   Graphic output generated by calls to the GKS
C                       routine GFA or to the SPPS routines "PLOTIF"
C                       and/or "POINTS".
C
C PRECISION             Single.
C
C REQUIRED LIBRARY      None.
C ROUTINES
C
C LANGUAGE              FORTRAN (master file is in IFTRAN).
C
C HISTORY               Written by Dave Kennison in June, 1989.
C
C TIMING                Depends on which type of fill is done.
C
C PORTABILITY           Portable.
C
C-----------------------------------------------------------------------
C
C SUBROUTINE SFWRLD (XRA,YRA,NRA,DST,NST,IND,NND)
C SUBROUTINE SFNORM (XRA,YRA,NRA,DST,NST,IND,NND)
C
C DIMENSION OF          XRA(NRA), YRA(NRA), DST(NST), IND(NND), where
C ARGUMENTS             NST=NRA+NIM and NND=NRA+2*NIM, where "NIM" is
C                       as defined under "ARGUMENTS (INPUT)", below.
C
C LATEST REVISION       January, 1989.
C
C PURPOSE               To fill, with evenly spaced parallel lines, an
C                       area of the plotter frame defined by a given
C                       set of points in one of two coordinate systems.
C
C USAGE                 CALL SFWRLD (XRA,YRA,NRA,DST,NST,IND,NND)
C                       CALL SFNORM (XRA,YRA,NRA,DST,NST,IND,NND)
C
C                       Either of these calls causes the area defined
C                       by the points "((XRA(I),YRA(I)),I=1,NRA)" to be
C                       filled with lines.  Use "SFWRLD" if XRA and YRA
C                       hold "world coordinates", "SFNORM" if they hold
C                       "normalized device coordinates".  (These are
C                       sometimes referred to as "user coordinates" and
C                       "fractional coordinates", respectively.)
C
C                       Several internal parameters, each of which has
C                       a two-character mnemonic name, determine the
C                       appearance of the fill lines used:  'AN' gives
C                       the angle, in degrees, at which fill lines are
C                       to be drawn.  'SP' specifies the spacing between
C                       fill lines.  'DO' specifies whether the lines
C                       are to be solid or dotted.  If dots are used,
C                       they are positioned 'SP' units apart, so that
C                       they fall at the interstices of a square grid;
C                       an 8x8 array of 0s and 1s specifies a repeating
C                       square pattern according to which dots are or
C                       are not drawn, thus allowing for the creation
C                       of textured fill patterns.  The parameter 'CH'
C                       specifies whether each dot used is just a dot
C                       or a character and, if the latter, what
C                       character.
C
C                       The default values of the parameters are such
C                       as to give solid horizontal lines .00125
C                       normalized device coordinate units apart.
C
C                       The routines SFGETx and SFSETx, where "x" is a
C                       "C", an "I", or an "R", are used to access the
C                       parameters 'AN', 'CH', 'DO', and 'SP'.  The
C                       routines SFGETP and SFSETP are used to get/set
C                       the contents of the 8x8 dot-pattern array.
C
C                       The lines connecting point 1 to point 2, point
C                       2 to point 3, ... point "NRA-1" to point "NRA",
C                       and point "NRA" to point 1 bound the area to
C                       be filled.  (These lines are not themselves
C                       drawn.)  The given boundary lines may cross
C                       each other.  If a given fill line intersects
C                       the boundary lines at "n" points, the "n"
C                       points are ordered in the obvious way and those
C                       segments of the fill line starting at an
C                       odd-numbered point and ending at an
C                       even-numbered point are drawn.
C
C                       To fill a simple polygon, give the coordinates
C                       of its vertices.  To fill a polygon with a hole,
C                       give the coordinates of the outer boundary, add
C                       the vertices of the hole and then repeat the
C                       last vertex of the outer boundary.  To produce
C                       a "negative", just add the coordinates of the
C                       frame corners and then repeat the last vertex
C                       of the polygon.
C
C                       Consecutive calls, using different fill angles,
C                       may be used to create a cross-hatched effect.
C
C ARGUMENTS (INPUT)     XRA - an array containing the X coordinates of
C                       the points defining the area to be filled.  If
C                       SFWRLD is being called, these are given in the
C                       world coordinate system (as defined by the
C                       current viewport and window - these may be set
C                       by GKS calls or by a call to the SPPS routine
C                       SET).  If SFNORM is being called, the X
C                       coordinates are normalized device coordinates,
C                       between 0 and 1.
C
C                       YRA - an array containing the Y coordinates of
C                       the points defining the area to be filled.  If
C                       SFWRLD is being called, these are given in the
C                       world coordinate system (as defined by the
C                       current viewport and window - these may be set
C                       by GKS calls or by a call to the SPPS routine
C                       SET).  If SFNORM is being called, the Y
C                       coordinates are normalized device coordinates,
C                       between 0 and 1.
C
C                       NRA - the number of points defining the area
C                       to be filled.  NRA must be greater than two.
C
C                       DST - a real scratch array, dimensioned "NST".
C
C                       NST - the dimension of DST - must be greater
C                       than or equal to "NRA+NIM", where "NIM" is the
C                       largest number of intersection points of any
C                       fill line with the boundary lines.  To be sure
C                       DST is large enough, use NIM=NRA.  In practice,
C                       NIM rarely needs to be that large.
C
C                       IND - an integer scratch array, dimensioned
C                       "NND".
C
C                       NND - the dimension of IND - must be greater
C                       than or equal to "NRA+2*NIM", where "NIM" is
C                       the largest number of intersection points of
C                       any fill line with the boundary lines.  To be
C                       sure IND is large enough, use NIM=NRA.  In
C                       practice, NIM rarely needs to be that large.
C
C ARGUMENTS (OUTPUT)    XRA - the input X coordinates, converted to
C                       normalized device coordinate units if the
C                       routine SFWRLD was called, unchanged if SFNORM
C                       was called.  In either case, the values are
C                       suitable for a subsequent call to SFNORM.
C
C                       YRA - the input Y coordinates, converted to
C                       normalized device coordinate units if the
C                       routine SFWRLD was called, unchanged if SFNORM
C                       was called.  In either case, the values are
C                       suitable for a subsequent call to SFNORM.
C
C                       NRA - unchanged.
C
C                       DST - overstored.
C
C                       NST - unchanged.
C
C                       IND - overstored.
C
C                       NND - unchanged.
C
C ENTRY POINTS          SFWRLD, SFNORM.
C
C COMMON BLOCKS         SFCOMN.
C
C I/O                   Graphic output generated by calls to the SPPS
C                       routines "PLOTIF" and/or "POINTS".
C
C PRECISION             Single.
C
C REQUIRED LIBRARY      None.
C ROUTINES
C
C LANGUAGE              FORTRAN (master file is in IFTRAN).
C
C HISTORY               Written by Dave Kennison in April, 1982, using
C                       an algorithm of his invention (influenced by
C                       an algorithm used by Richard Helgason in his
C                       "Sherwin-Williams" program - lo, these many
C                       years ago).  Added to NCAR Graphics in January,
C                       1989.
C
C ALGORITHM             The points are sorted in order of increasing
C                       distance from a "baseline" having the desired
C                       fill angle.  Then, a "fill line" is passed over
C                       the area, starting at the minimum distance
C                       found and stopping at the maximum distance
C                       found.  Each time the "fill line" passes one of
C                       the points, a list of the boundary lines which
C                       the fill line intersects is updated.  At each
C                       desired drawing position, a list of intersection
C                       points is generated, sorted in the order of
C                       their projection on the x axis or the y axis
C                       (depending on the fill angle) and the segments
C                       of the fill line which fall inside the figure
C                       to be filled are drawn, using solid lines or
C                       dotted lines.
C
C TIMING                Roughly speaking, the timing of SFWRLD and
C                       SFNORM varies linearly with the number of line
C                       segments or dots produced.  It is little
C                       affected by the number of points defining the
C                       area to be filled (except insofar as this
C                       results in an increase in the number of lines
C                       drawn).
C
C                       On a Cray X-MP, filling a "circle" defined by
C                       10 points and having a radius of .25 normalized
C                       device coordinate units with lines at default
C                       spacing required .031 CPU seconds.  Using dots
C                       to fill the circle boosted the time to .571 CPU
C                       seconds.  Reducing the radius of the circle by
C                       a factor of 2 reduced the times for line fill
C                       and dot fill to .018 and .144 CPU seconds,
C                       respectively.  Defining the circle with 100
C                       points boosted the corresponding times to .036,
C                       .608, .019, and .155 CPU seconds.
C
C PORTABILITY           Portable.
C
C-----------------------------------------------------------------------
C
C SUBROUTINE SFGETx (CNP,xVP)
C
C DIMENSION OF          CNP is of type "CHARACTER*n", where "n" is 2 or
C ARGUMENTS             greater.  When "x" is a "C", then the variable
C                       CVP is of type "CHARACTER*1".
C
C LATEST REVISION       January, 1989.
C
C PURPOSE               To retrieve the current values of internal
C                       parameters affecting the behavior of "SFSGFA",
C                       "SFWRLD", and "SFNORM".
C
C USAGE                 CALL SFGETx (CNP,xVP)
C
C                       where "x" is a 'C', an 'I', or an 'R', depending
C                       on the type of value to be retrieved (character,
C                       integer, or real).
C
C ARGUMENTS (INPUT)     CNP is one of the five character strings 'AN',
C                       'CH', 'DO', 'SP', or 'TY' (or a longer string
C                       starting with one of these strings), meaning
C                       "ANgle", "CHaracter", "DOtting", "SPacing", and
C                       "TYpe", respectively.
C
C                       xVP is the name of a variable into which the
C                       value of the parameter specified by "CNP" is to
C                       be retrieved.
C
C ARGUMENTS (OUTPUT)    CNP is unchanged.
C
C                       When CNP='AN', xVP is the angle at which fill
C                       lines are to be drawn, in degrees.  Use "x" =
C                       "I" to get an integer value in IVP or use "x" =
C                       "R" to get a real value in RVP.
C
C                       When CNP='CH', xVP is the character selector -
C                       an integer 0 means that dots are to be used.
C                       A positive integer value "n" implies that the
C                       character "CHAR(n)" is to be used.  A negative
C                       integer "-n" implies that the GKS polymarker of
C                       type "n" is to be used.  Use "x" = "I" to get
C                       an integer value in IVP or use "x" = "C" to get
C                       a character value in CVP (if the internal value
C                       is less than or equal to zero, a single blank
C                       will be returned as the value of CVP).
C
C                       When CNP='DO', xVP is the pattern selector -
C                       the value 0 means that solid lines are to be
C                       used, the value 1 that dotted lines are to be
C                       used (the dots to be arranged according to the
C                       pattern specified by an internal array LDP -
C                       see the description of the routine SFSETP,
C                       below).  Use "x" = "I" to get an integer value
C                       for IVP.
C
C                       When CNP='SP', xVP is the distance between each
C                       fill line and the next.  The value is given in
C                       the normalized-device-coordinate system and
C                       represents a fraction of the distance across
C                       the plotter frame, between 0. and 1.  Use "x" =
C                       "R" to get a real value for RVP.
C
C                       When CNP='TY', xVP is the type of fill to be
C                       used by the routine SFSGFA.  Use "x" = "I" to
C                       get an integer value for IVP.
C
C ENTRY POINTS          SFGETC, SFGETI, and SFGETR.
C
C COMMON BLOCKS         SFCOMN.
C
C I/O                   None.
C
C PRECISION             Single.
C
C REQUIRED LIBRARY      None.
C ROUTINES
C
C LANGUAGE              FORTRAN (master file is in IFTRAN).
C
C HISTORY               Written in January, 1989.
C
C ALGORITHM             Not applicable.
C
C TIMING                Insignificant.
C
C PORTABILITY           Portable.
C
C-----------------------------------------------------------------------
C
C SUBROUTINE SFGETP (IDP)
C
C DIMENSION OF          IDP(8,8).
C ARGUMENTS
C
C LATEST REVISION       January, 1989.
C
C PURPOSE               To retrieve the dot pattern currently defined
C                       for use by "SFWRLD" and "SFNORM".
C
C USAGE                 CALL SFGETP (IDP)
C
C ARGUMENTS (INPUT)     IDP is an 8x8 integer array in which the dot
C                       pattern is to be returned.  See the description
C                       of SFSETP for further details.
C
C ARGUMENTS (OUTPUT)    The current dot pattern has been put into IDP.
C
C ENTRY POINTS          SFGETP.
C
C COMMON BLOCKS         SFCOMN.
C
C I/O                   None.
C
C PRECISION             Single.
C
C REQUIRED LIBRARY      None.
C ROUTINES
C
C LANGUAGE              FORTRAN (master file is in IFTRAN).
C
C HISTORY               Created in January, 1989, for inclusion in
C                       NCAR Graphics.
C
C ALGORITHM             Not applicable.
C
C TIMING                Insignificant.
C
C PORTABILITY           Portable.
C
C-----------------------------------------------------------------------
C
C SUBROUTINE SFSETx (CNP,xVP)
C
C DIMENSION OF          CNP is of type "CHARACTER*n", where "n" is 2 or
C ARGUMENTS             greater.  When "x" is a "C", then the variable
C                       CVP is of type "CHARACTER*1".
C
C LATEST REVISION       January, 1989.
C
C PURPOSE               To set internal parameters affecting the
C                       behavior of "SFSGFA", "SFWRLD", and "SFNORM".
C
C USAGE                 CALL SFSETx (CNP,xVP)
C
C                       where "x" is a 'C', an 'I', or an 'R', depending
C                       on the type of value being given to a parameter
C                       (character, integer, or real).
C
C ARGUMENTS (INPUT)     CNP is one of the four character strings 'AN',
C                       'CH', 'DO', 'SP', or 'TY' (or a longer string
C                       starting with one of these strings), meaning
C                       "ANgle", "CHaracter", "DOtting", "SPacing", or
C                       "TYpe", respectively.
C
C                       xVP is the value to be given to the parameter
C                       specified by "CNP".  This value is transferred
C                       into an internal labelled common block and
C                       retains its value until another call resets it.
C
C                       When CNP='AN', xVP is the angle at which fill
C                       lines are to be drawn, in degrees.  Use "x" =
C                       "I" and give an integer value for IVP or use
C                       "x" = "R" and give a real value for RVP.  The
C                       default value is 0.
C
C                       When CNP='CH', xVP is the character selector -
C                       an integer 0 means that dots are to be used.
C                       A positive integer value "n" implies that the
C                       character "CHAR(n)" is to be used.  A negative
C                       integer "-n" implies that the GKS polymarker of
C                       type "n" is to be used.  Use "x" = "I" and give
C                       an integer value for IVP or use "x" = "C" and
C                       give a desired character for CVP.  The default
C                       value is 0.
C
C                       When CNP='DO', xVP is the pattern selector -
C                       the value 0 means that solid lines are to be
C                       used, the value 1 that dotted lines are to be
C                       used (the dots to be arranged according to the
C                       pattern specified by an internal array LDP -
C                       see the description of the routine SFSETP,
C                       below).  Use "x" = "I" and give an integer
C                       value for IVP.  The default value is 0.
C
C                       When CNP='SP', xVP is the distance between each
C                       fill line and the next.  The value is given in
C                       the normalized-device-coordinate system and
C                       represents a fraction of the distance across
C                       the plotter frame, between 0. and 1.  Use "x" =
C                       "R" and give a real value for RVP.  The default
C                       value is .00125.  A value less than or equal to
C                       zero or greater than or equal to 1 will have the
C                       same effect as the value .00125.
C
C                       When CNP='TY', xVP is the type of fill to be
C                       used by the routine SFSGFA.  Use "x" = "I" and
C                       give an integer value for IVP.
C
C ARGUMENTS (OUTPUT)    Unchanged.
C
C ENTRY POINTS          SFSETC, SFSETI, and SFSETR.
C
C COMMON BLOCKS         SFCOMN.
C
C I/O                   None.
C
C PRECISION             Single.
C
C REQUIRED LIBRARY      None.
C ROUTINES
C
C LANGUAGE              FORTRAN (master file is in IFTRAN).
C
C HISTORY               Written in January, 1989.
C
C ALGORITHM             Not applicable.
C
C TIMING                Insignificant.
C
C PORTABILITY           Portable.
C
C-----------------------------------------------------------------------
C
C SUBROUTINE SFSETP (IDP)
C
C DIMENSION OF          IDP(8,8).
C ARGUMENTS
C
C LATEST REVISION       January, 1989.
C
C PURPOSE               To reset the dot pattern used by "SFWRLD" and
C                       "SFNORM".
C
C USAGE                 CALL SFSETP (IDP)
C
C ARGUMENTS (INPUT)     IDP is an 8x8 integer array of 0s and 1s giving
C                       the dot pattern to be used by SFWRLD and SFNORM.
C                       A 1 turns the corresponding dot on and a 0 turns
C                       the dot off.  The dot pattern is replicated as
C                       required to cover an entire filled area.  The
C                       area occupied by one mapping of IDP onto a plot
C                       is 8*'SP' units wide and 8*'SP' units high.
C                       Each fill line is drawn using dots positioned as
C                       specified by a particular row of IDP, applied
C                       repetitively along the line.  Succeeding lines
C                       use the following rows of IDP, in a circular
C                       fashion.
C
C                       If the fill angle is 0 (the default), then
C                       incrementing the first subscript of IDP
C                       corresponds to a horizontal motion across the
C                       plot from left to right and incrementing the
C                       second subscript of IDP corresponds to a
C                       vertical motion across the plot from top to
C                       bottom.  This allows the contents of IDP to
C                       be declared in a data statement which creates
C                       a "picture" of the pattern.  For example,
C
C                             DATA IDP / 0, 0, 0, 0, 0, 0, 0, 0,
C                            +           0, 1, 1, 0, 0, 1, 1, 1,
C                            +           0, 1, 1, 0, 0, 1, 1, 0,
C                            +           0, 1, 1, 0, 1, 1, 0, 0,
C                            +           0, 1, 1, 1, 1, 0, 0, 0,
C                            +           0, 1, 1, 0, 1, 1, 0, 0,
C                            +           0, 1, 1, 0, 0, 1, 1, 0,
C                            +           0, 1, 1, 0, 0, 1, 1, 1/
C
C                       creates the letter "K", with the correct
C                       orientation.
C
C                       The contents of the array IDP are transferred
C                       to the array LDP, in the common block SFCOMN.
C                       The new dot pattern is used until a subsequent
C                       call to SFSETP changes the array again.
C
C                       The default dot pattern consists of all 1s.
C
C ARGUMENTS (OUTPUT)    Unchanged.
C
C ENTRY POINTS          SFSETP.
C
C COMMON BLOCKS         SFCOMN.
C
C I/O                   None.
C
C PRECISION             Single.
C
C REQUIRED LIBRARY      None.
C ROUTINES
C
C LANGUAGE              FORTRAN (master file is in IFTRAN).
C
C HISTORY               Written and standardized in April, 1982.
C                       Updated, with a name change, for inclusion in
C                       NCAR Graphics, in January, 1989.
C
C ALGORITHM             Not applicable.
C
C TIMING                Insignificant.
C
C PORTABILITY           Portable.
C
C***********************************************************************
C S O F T F I L L   -   U S E R - L E V E L   R O U T I N E S
C***********************************************************************
C
      SUBROUTINE SFSGFA (XRA,YRA,NRA,DST,NST,IND,NND,ICI)
C
C Declare the dimensions of argument arrays.
C
      DIMENSION XRA(NRA),YRA(NRA),DST(NST),IND(NND)
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,LCH,LDP(8,8)
C
C Declare the block data routine external to force its loading.
C
      EXTERNAL SFBLDA
C
C Do fill of the type selected by the values of ICI and ITY.
C
      IF (.NOT.(ITY.EQ.0)) GO TO 10001
      IF (.NOT.(ICI.GE.0)) GO TO 10002
      CALL GQFACI (IER,ISC)
      IF (.NOT.(IER.NE.0)) GO TO 10003
      CALL SETER ('SFSGFA - ERROR EXIT FROM GQFACI',1,2)
      STOP
10003 CONTINUE
      CALL GSFACI (ICI)
10002 CONTINUE
      DO 10004 I=1,NRA
      XRA(I)=CUFX(XRA(I))
      YRA(I)=CUFY(YRA(I))
10004 CONTINUE
      CALL GETSET (XLVP,XRVP,YBVP,YTVP,XLWD,XRWD,YBWD,YTWD,LNLG)
      CALL SET    (XLVP,XRVP,YBVP,YTVP,XLVP,XRVP,YBVP,YTVP,   1)
      CALL GFA    (NRA,XRA,YRA)
      CALL SET    (XLVP,XRVP,YBVP,YTVP,XLWD,XRWD,YBWD,YTWD,LNLG)
      IF (ICI.GE.0) CALL GSFACI (ISC)
      GO TO 10005
10001 CONTINUE
      IF (.NOT.(ITY.GT.0)) GO TO 10006
      IF (.NOT.(ICI.GE.0)) GO TO 10007
      CALL GQPLCI (IER,ISC)
      IF (.NOT.(IER.NE.0)) GO TO 10008
      CALL SETER ('SFSGFA - ERROR EXIT FROM GQPLCI',2,2)
      STOP
10008 CONTINUE
      CALL GSPLCI (ICI)
10007 CONTINUE
      CALL SFWRLD (XRA,YRA,NRA,DST,NST,IND,NND)
      IF (.NOT.(ITY.GT.1)) GO TO 10009
      AID=AID+90.
      CALL SFNORM (XRA,YRA,NRA,DST,NST,IND,NND)
      AID=AID-90.
10009 CONTINUE
      IF (ICI.GE.0) CALL GSPLCI (ISC)
      GO TO 10005
10006 CONTINUE
      IF (.NOT.(ICI.GT.0)) GO TO 10010
      AIS=AID
      DBS=DBL
      IT1=-ITY
      DO 10011 I=1,IT1
      IT2=(ICI+IT1-I)/IT1
      IF (.NOT.(IT2.GT.0)) GO TO 10012
      AID=AIS+REAL((I-1)*(180/IT1))
      DBL=MAX(DBS/8.,DBS*2.**(6-IT2))
      IF (.NOT.(I.EQ.1)) GO TO 10013
      CALL SFWRLD (XRA,YRA,NRA,DST,NST,IND,NND)
      GO TO 10014
10013 CONTINUE
      CALL SFNORM (XRA,YRA,NRA,DST,NST,IND,NND)
10014 CONTINUE
10012 CONTINUE
10011 CONTINUE
      AID=AIS
      DBL=DBS
10010 CONTINUE
10005 CONTINUE
C
C Done.
C
      RETURN
C
      END
