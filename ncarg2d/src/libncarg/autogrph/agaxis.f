C
C ---------------------------------------------------------------------
C A   B R I E F   D E S C R I P T I O N   O F   A U T O G R A P H
C ---------------------------------------------------------------------
C
C Following is a brief description of the AUTOGRAPH package.  For a
C complete write-up, see the programmer document.
C
C
C PACKAGE AUTOGRAPH
C
C LATEST REVISION        July, 1992.
C
C PURPOSE                To draw graphs, each with a labelled background
C                        and each displaying one or more curves.
C
C ACCESS (UNDER UNIX)    On a Unix system on which NCAR Graphics has
C                        been installed, the command "ncargf77" will
C                        supply the necessary references to the NCAR
C                        Graphics libraries containing AUTOGRAPH.
C
C                        To get smoother curves, drawn using spline
C                        interpolation, use the ncargf77 option
C                        "-dashsmooth".
C
C                        AUTOGRAPH contains a routine AGPWRT, which it
C                        calls to draw labels.  This routine just passes
C                        its arguments on to the system-plot-package
C                        routine PWRIT.  To use one of the fancier
C                        character-drawers, like PWRITX or PWRITY,
C                        just compile a routine AGPWRT to replace the
C                        default version; it has the same arguments as
C                        PWRIT and may either draw the character string
C                        itself, or just pass the arguments on to a
C                        desired character-drawer.  The AUTOGRAPH
C                        specialist has some "standard" versions of
C                        AGPWRT and should be consulted for help in
C                        avoiding pitfalls.  One standard version,
C                        may be obtained by using the ncargf77 option
C                        "-agupwrtx".
C
C USAGE                  Following this indented preamble are given two
C                        lists:  one describing the AUTOGRAPH routines
C                        and another describing the arguments of those
C                        routines.
C
C ENTRY POINTS           Except for seven routines which are included
C                        in the package for historical reasons (EZY,
C                        EZXY, EZMY, EZMXY, IDIOT, ANOTAT, and DISPLA),
C                        the AUTOGRAPH routines have six-character names
C                        beginning with the characters 'AG'.  An alpha-
C                        betized list follows:
C
C                          AGAXIS AGBACK AGBNCH AGCHAX AGCHCU AGCHIL
C                          AGCHNL AGCTCS AGCTKO AGCURV AGDASH AGDFLT
C                          AGDLCH AGDSHN AGEXAX AGEXUS AGEZSU AGFPBN
C                          AGFTOL AGGETC AGGETF AGGETI AGGETP AGGTCH
C                          AGINIT AGKURV AGLBLS AGMAXI AGMINI AGNUMB
C                          AGPPID AGPWRT AGQURV AGRPCH AGRSTR AGSAVE
C                          AGSCAN AGSETC AGSETF AGSETI AGSETP AGSRCH
C                          AGSTCH AGSTUP AGUTOL
C
C                        NOTE:  The "routine" AGDFLT is a block-data
C                        routine specifying the default values of
C                        AUTOGRAPH control parameters.
C
C SPECIAL CONDITIONS     Under certain conditions, AUTOGRAPH may print
C                        an error message (via the routine SETER) and
C                        stop.  Each error message includes the name of
C                        the routine which issued it.  A description of
C                        the condition which caused the error may be
C                        found in the AUTOGRAPH write-up in the NCAR
C                        graphics manual; look in the write-up of the
C                        routine which issued the error message, under
C                        the heading 'SPECIAL CONDITIONS'.
C
C                        For error messages issued by the routine
C                        AGNUMB, see the write-up of the routine AGSTUP.
C
C                        If you get an error in the routine ALOG10, it
C                        probably means that you are using a logarithmic
C                        axis and some of the coordinate data along that
C                        axis are zero or negative.
C
C COMMON BLOCKS          The AUTOGRAPH common blocks are AGCONP, AGORIP,
C                        AGOCHP, AGCHR1, and AGCHR2.  AGCONP contains
C                        the AUTOGRAPH "control parameters", primary and
C                        secondary, all of which are real, AGORIP other
C                        real and/or integer parameters, AGOCHP other
C                        character parameters, AGCHR1 and AGCHR2 the
C                        variables implementing the character-storage-
C                        and-retrieval scheme of AUTOGRAPH.
C
C I/O                    Lower-level plotting routines are called to
C                        produce graphical output and, when errors
C                        occur, error messages may be written to the
C                        system error file, as defined by I1MACH(4),
C                        either directly or by way of a call to SETER.
C
C REQUIRED ULIB          AUTOGRAPH uses the software dashed-line package
C ROUTINES               DASHCHAR.  Of course, either of the packages
C                        DASHSMTH or DASHSUPR may be used instead, to
C                        get smoother curves.
C
C SPECIALIST             Dave Kennison, Scientific Computing Division,
C                        National Center for Atmospheric Research
C
C LANGUAGE               FORTRAN
C
C HISTORY                Dave Robertson wrote the original routine
C                        IDIOT, which was intended to provide a simple,
C                        quick-and-dirty, x-y graph-drawing capability.
C                        In time, as it became obvious that many users
C                        were adapting IDIOT to more sophisticated
C                        tasks, Dan Anderson wrote the first AUTOGRAPH
C                        package, based on IDIOT.  It allowed the user
C                        to put more than one curve on a graph, to use
C                        more sophisticated backgrounds, to specify
C                        coordinate data in a variety of ways, and to
C                        more easily control the scaling and positioning
C                        of graphs.  Eventually, this package, too, was
C                        found wanting.  In 1977, Dave Kennison entirely
C                        re-wrote AUTOGRAPH, with the following goals:
C                        to maintain the ease of use for simple graphs
C                        which had been the principal virtue of the
C                        package, to provide the user with as much
C                        control as possible, to incorporate desirable
C                        new features, and to make the package as
C                        portable as possible.  In 1984, the package
C                        was again worked over by Dave Kennison, to
C                        make it compatible with FORTRAN-77 and
C                        to remove any dependency on the LOC function,
C                        which had proved to cause difficulties on
C                        certain machines.  The user interface was
C                        changed somewhat and some new features were
C                        added.  A GKS-compatible version was written.
C
C PORTABILITY            AUTOGRAPH may be ported with few modifications
C                        to most systems having a FORTRAN-77 compiler.
C
C                        The labelled common blocks may have to be
C                        declared in a part of the user program which
C                        is always core-resident so that variables
C                        in them will maintain their values from one
C                        AUTOGRAPH-routine call to the next.  Such a
C                        problem may arise when AUTOGRAPH is placed in
C                        an overlay or when some sort of memory-paging
C                        scheme is used.
C
C REQUIRED RESIDENT      AUTOGRAPH uses the DASHCHAR routines DASHDB,
C ROUTINES               DASHDC, FRSTD, LASTD, LINED, AND VECTD, the
C                        system-plot-package routines FRAME, GETSET,
C                        GETSI, LINE, PWRIT, and SET, the support
C                        routines ISHIFT and IOR, the (modified)
C                        PORT utilities SETER and I1MACH, and the
C                        FORTRAN-library routines ALOG10, ATAN2, COS,
C                        SIN, AND SQRT.
C
C REQUIRED GKS LEVEL     0A
C
C ---------------------------------------------------------------------
C U S E R - C A L L A B L E   A U T O G R A P H   R O U T I N E S
C ---------------------------------------------------------------------
C
C Following is a list of AUTOGRAPH routines to be called by the user
C (organized by function).  Each routine is described briefly.  The
C arguments of the routines are described in the next section.
C
C Each of the following routines draws a complete graph with one call.
C Each is implemented by a set of calls to the lower-level AUTOGRAPH
C routines AGSTUP, AGCURV, and AGBACK (which see, below).
C
C -- EZY (YDRA,NPTS,GLAB) - draws a graph of the curve defined by the
C    data points ((I,YDRA(I)),I=1,NPTS), with a graph label specified
C    by GLAB.
C
C -- EZXY (XDRA,YDRA,NPTS,GLAB) - draws a graph of the curve defined by
C    the data points ((XDRA(I),YDRA(I)),I=1,NPTS), with a graph label
C    specified by GLAB.
C
C -- EZMY (YDRA,IDXY,MANY,NPTS,GLAB) - draws a graph of the family of
C    curves defined by data points (((I,YDRA(I,J)),I=1,NPTS),J=1,MANY),
C    with a graph label specified by GLAB.  The order of the subscripts
C    of YDRA may be reversed - see the routine DISPLA, argument LROW.
C
C -- EZMXY (XDRA,YDRA,IDXY,MANY,NPTS,GLAB) - draws a graph of the
C    family of curves defined by the data points (((XDRA(I),YDRA(I,J)),
C    I=1,NPTS),J=1,MANY), with a graph label specified by GLAB.  XDRA
C    may be doubly-subscripted and the order of the subscripts of XDRA
C    and YDRA may be reversed - see the routine DISPLA, argument LROW.
C
C -- IDIOT (XDRA,YDRA,NPTS,LTYP,LDSH,LABX,LABY,LABG,LFRA) - implements
C    the routine from which AUTOGRAPH grew - not recommended - provided
C    for antique lovers.
C
C The following routines provide user access to the AUTOGRAPH control
C parameters (in the labelled common block AGCONP).
C
C -- ANOTAT (XLAB,YLAB,LBAC,LSET,NDSH,DSHL) - may be used to change the
C    x- and y-axis (non-numeric) labels, the background type, the way
C    in which graphs are positioned and scaled, and the type of dash
C    patterns to be used in drawing curves.
C
C -- DISPLA (LFRA,LROW,LTYP) - may be used to specify when, if ever,
C    the EZ... routines do a frame advance, how input arrays for EZMY
C    and EZMXY are dimensioned, and the linear/log nature of graphs.
C
C -- AGSETP (TPGN,FURA,LURA) - a general-purpose parameter-setting
C    routine, used to set the group of parameters specified by TPGN,
C    using values obtained from the array (FURA(I),I=1,LURA).
C
C -- AGSETF (TPGN,FUSR) - used to set the single parameter specified by
C    TPGN, giving it the floating-point value FUSR.
C
C -- AGSETI (TPGN,IUSR) - used to set the single parameter specified by
C    TPGN, giving it the floating-point value FLOAT(IUSR).
C
C -- AGSETC (TPGN,CUSR) - the character string CUSR is stashed in an
C    array inside AUTOGRAPH and the floating-point equivalent of an
C    identifier which may be used for later retrieval of the string is
C    stored as the value of the single parameter specified by TPGN.  The
C    single parameter must be a label name, a dash pattern, the text of
C    a label line, or the line-terminator character.
C
C -- AGGETP (TPGN,FURA,LURA) - a general-purpose parameter-getting
C    routine, used to get the group of parameters specified by TPGN,
C    putting the result in the array (FURA(I),I=1,LURA).
C
C -- AGGETF (TPGN,FUSR) - used to get, in FUSR, the floating-point
C    value of the single parameter specified by TPGN.
C
C -- AGGETI (TPGN,IUSR) - used to get, in IUSR, the integer equivalent
C    of the value of the single parameter specified by TPGN.
C
C -- AGGETC (TPGN,CUSR) - used to get, in CUSR, the character string
C    whose identifier is specified by the integer equivalent of the
C    single parameter specified by TPGN.  The single parameter must
C    be a label name, a dash pattern, the text of a label line, or the
C    line-terminator character.
C
C The following are lower-level routines, which may be used to draw
C graphs of many different kinds.  The EZ... routines call these.  They
C are intended to be called by user programs, as well.
C
C -- AGSTUP (XDRA,NVIX,IIVX,NEVX,IIEX,YDRA,NVIY,IIVY,NEVY,IIEY) - this
C    routine must be called prior to the first call to either of the
C    two routines AGBACK and AGCURV, to force the set-up of secondary
C    parameters controlling the behavior of those routines.  After any
C    parameter-setting call, AGSTUP must be called again before calling
C    either AGBACK or AGCURV again.  AGSTUP calls the routine "SET", in
C    the plot package, so that user x/y coordinates in subsequent calls
C    will map properly into the plotter space.
C
C -- AGBACK - draws the background defined by the current state of the
C    AUTOGRAPH control parameters.
C
C -- AGCURV (XVEC,IIEX,YVEC,IIEY,NEXY,KDSH) - draws the curve defined
C    by the arguments, positioning it as specified by the current state
C    of the AUTOGRAPH control parameters.
C
C The following utility routines are called by the user.
C
C -- AGSAVE (IFNO) - used to save the current state of AUTOGRAPH by
C    writing the appropriate information to a specified file.  Most
C    commonly used to save the default state for later restoration.
C    This routine should be used instead of AGGETP when the object
C    is to save the whole state of AUTOGRAPH, since it saves not only
C    the primary control parameters, but all of the character strings
C    pointed to by the primary control parameters.  It is the user's
C    responsibility to position the file before calling AGSAVE.
C
C -- AGRSTR (IFNO) - used to restore a saved state of AUTOGRAPH by
C    reading the appropriate information from a specified file.  Most
C    commonly used to restore AUTOGRAPH to its default state.  It is
C    the user's responsibility to position the file before calling
C    AGRSTR.
C
C -- AGBNCH (IDSH) - a function, of type CHARACTER*16 (it must be
C    declared as such in a user routine referencing it), whose value,
C    given a 16-bit binary dash pattern, is the equivalent character
C    dash pattern.
C
C -- AGDSHN (IDSH) - a function, of type CHARACTER*16 (it must be
C    declared as such in a user routine referencing it), whose value,
C    given an integer "n" (typically between 1 and 26) is the character
C    string 'DASH/ARRAY/nnnn.', which is the name of the nth dash
C    pattern parameter.  To set the 13th dash pattern, for example,
C    one might use "CALL AGSETC (AGDSHN(13),'$$$$$$CURVE 13$$$$$$')".
C
C The following utility routines are called by AUTOGRAPH.  The versions
C included in AUTOGRAPH itself are dummies; they do nothing but RETURN.
C The user may replace one or more of these routines with versions to
C accomplish specific purposes.
C
C -- AGUTOL (IAXS,FUNS,IDMA,VINP,VOTP) - called by AUTOGRAPH to perform
C    the mapping from user-system values along an axis to label-system
C    values along the axis and vice-versa.  This routine may be replaced
C    by the user to create a desired graph.
C
C -- AGCHAX (IFLG,IAXS,IPRT,VILS) - called by AUTOGRAPH just before and
C    just after the various parts of the axes are drawn.
C
C -- AGCHCU (IFLG,KDSH) - called by AUTOGRAPH just before and just after
C    each curve is drawn.
C
C -- AGCHIL (IFLG,LBNM,LNNO) - called by AUTOGRAPH just before and just
C    after each line of an informational label is drawn.
C
C -- AGCHNL (IAXS,VILS,CHRM,MCIM,NCIM,IPXM,CHRE,MCIE,NCIE) - called by
C    AUTOGRAPH just after the character strings defining a numeric label
C    have been generated.
C
C ---------------------------------------------------------------------
C D E S C R I P T I O N S   O F   A R G U M E N T S
C ---------------------------------------------------------------------
C
C In calls to the routines EZY, EZXY, EZMY, and EZMXY:
C
C -- XDRA is an array of x coordinates, dimensioned as implied by the
C    current value of the AUTOGRAPH control parameter 'ROW.' (see the
C    description of the argument LROW, below).  The value of the
C    AUTOGRAPH parameter 'NULL/1.' (1.E36, by default) when used as an
C    x coordinate, implies a missing data point; the curve segments
C    on either side of such a point are not drawn.
C
C -- YDRA is an array of y coordinates, dimensioned as implied by the
C    current value of the AUTOGRAPH control parameter 'ROW.' (see the
C    description of the argument LROW, below).  The value of the
C    AUTOGRAPH parameter 'NULL/1.' (1.E36, by default) when used as a
C    y coordinate, implies a missing data point; the curve segments
C    on either side of such a point are not drawn.
C
C -- IDXY is the first dimension of the arrays XDRA (if it has two
C    dimensions) and YDRA.
C
C -- MANY is the number of curves to be drawn by the call to EZ... -
C    normally, the second dimension of XDRA (if it has two dimensions)
C    and YDRA.
C
C -- NPTS is the number of points defining each curve to be drawn by
C    the routine EZ... - normally, the first (or only) dimension of
C    XDRA and YDRA.
C
C -- GLAB is a character constant or a character variable, defining a
C    label to be placed at the top of the graph.  The string may not be
C    more than 40 characters long - if it is fewer than 40 characters
C    long, its last character must be a dollar sign.  (The dollar sign
C    is not a part of the label - it is stripped off.)  The character
C    string "CHAR(0)" may be used to indicate that the previous label,
C    whatever it was, should continue to be used.  The initial graph
C    label consists of blanks.
C
C In calls to the routine ANOTAT:
C
C -- XLAB and YLAB resemble GLAB (see above) and define labels for the
C    x and y axes.  The default x-axis label is the single character
C    X, the default y-axis label the single character Y.  Note that one
C    may use the string "CHAR(0)" to indicate that the x-axis (y-axis)
C    label is not to be changed from what it was previously.
C
C -- LBAC, if non-zero, specifies a new value for the AUTOGRAPH control
C    parameter 'BACKGROUND.', as follows:
C
C       1 - a perimeter background
C
C       2 - a grid background
C
C       3 - an axis background
C
C       4 - no background
C
C    The default value of 'BACKGROUND.' is 1.
C
C -- LSET, if non-zero, specifies a new value for the AUTOGRAPH control
C    parameter 'SET.'.  This parameter may be negated to suspend the
C    drawing of curves by the EZ... routines, so that a call to one of
C    them will produce only a background.  The absolute value of 'SET.'
C    affects the way in which AUTOGRAPH determines the position and
C    shape of the graph and the scaling of the axes, as follows:
C
C       1 - Restores the default values of the AUTOGRAPH parameters
C           in question.  AUTOGRAPH will set up an appropriate call
C           to the plot-package routine "SET", over-riding any prior
C           call to that routine.
C
C       2 - Tells AUTOGRAPH to use arguments 1-4 and 9 of the last
C           "SET" call.  Arguments 1-4 specify where the graph should
C           fall on the plotter frame, argument 9 whether the graph
C           is linear/linear, linear/log, etc.
C
C       3 - Tells AUTOGRAPH to use arguments 5-8 and 9 of the last
C           "SET" call.  Arguments 5-8 specify the scaling of the
C           axes, argument 9 whether the graph is linear/linear,
C           linear/log, etc.
C
C       4 - A combination of 2 and 3.  Arguments 1-4 of the last "SET"
C           call specify the position, arguments 5-8 the scaling, and
C           argument 9 the linear/log nature, of the graph.
C
C    (The plot-package routine "SET" is described in the NCAR Graphics
C    Manual; it is not a part of AUTOGRAPH.)
C
C    If the routine DISPLA is called with its argument LTYP non-zero,
C    the linear/log nature of the graph will be that specified by LTYP,
C    not that specified by the last "SET" call, no matter what the value
C    of the control parameter 'SET.'.
C
C    The default value of 'SET.' is 1.
C
C -- NDSH, if non-zero, specifies a new value of the AUTOGRAPH control
C    parameter 'DASH/SELECTOR.' (and therefore a new set of dashed-line
C    patterns), as described below.  Note:  The default value of the
C    dashed-line parameters is such that all curves will be drawn using
C    solid lines; if that is what you want, use a zero for NDSH.
C
C       If the value of 'DASH/SELECTOR.' is negative, curves produced
C       by subsequent calls to EZMY or EZMXY will be drawn using a
C       set of alphabetic dashed-line patterns.  The first curve drawn
C       by a given call will be labelled 'A', the second 'B', ..., the
C       twenty-sixth 'Z', the twenty-seventh 'A' again, and so on.
C       Curves drawn by calls to EZY and EZXY will be unaffected.
C
C       If the value of 'DASH/SELECTOR.' is positive, it must be less
C       than or equal to 26.  The next argument, DSHL, is an array
C       containing NDSH dashed-line patterns.  All curves produced by
C       subsequent calls to EZY, EZXY, EZMY, and EZMXY will be drawn
C       using the dashed-line patterns in (DSHL(I),I=1,NDSH) - the
C       first curve produced by a given call will have the pattern
C       specified by DSHL(1), the second that specified by DSHL(2),
C       the third that specified by DSHL(3), . . . the NDSH+1st that
C       specified by DSHL(1), . . . etc.  Each element of DSHL must
C       be a character string, in which a dollar sign stands for a
C       solid-line segment, a quote stands for a gap, and other
C       characters stand for themselves.  See the write-up of the
C       package "DASHCHAR".  Binary dashed-line patterns may not be
C       defined by means of a call to ANOTAT, only by means of calls
C       to lower-level routines.
C
C -- DSHL (if NDSH is greater than zero) is an array of dashed-line
C    patterns, as described above.
C
C In calls to the routine DISPLA:
C
C -- LFRA, if non-zero, specifies a new value for the AUTOGRAPH control
C    parameter 'FRAME.'.  Possible values are as follows:
C
C       1 - The EZ... routines do a frame advance after drawing.
C
C       2 - No frame advance is done by the EZ... routines.
C
C       3 - The EZ... routines do a frame advance before drawing.
C
C    The default value of 'FRAME.' is 1.
C
C -- LROW, if non-zero, specifies a new value for the AUTOGRAPH control
C    parameter 'ROW.'.  This parameter tells AUTOGRAPH how the argument
C    arrays XDRA and YDRA, in calls to the routines EZMY and EZMXY, are
C    subscripted, as follows:
C
C       If 'ROW.' is positive, this implies that the first subscript
C       of YDRA is a point number and the second subscript is a curve
C       number.  If 'ROW.' is negative, the order is reversed.
C
C       If the absolute value of 'ROW.' is 1, this implies that XDRA
C       is singly-subscripted, by point number only.  If the absolute
C       value of 'ROW.' is 2 or greater, this implies that XDRA is
C       doubly-subscripted, just like YDRA.
C
C    The default value of 'ROW.' is 1, spicifying that XDRA is singly-
C    subscripted and that YDRA is doubly-subscripted by point number
C    and curve number, in that order.
C
C -- LTYP, if non-zero, specifies new values for the AUTOGRAPH control
C    parameters 'X/LOGARITHMIC.' and 'Y/LOGARITHMIC.', which determine
C    whether the X and Y axes are linear or logarithmic.  Possible
C    values are as follows:
C
C       1 - x axis linear, y axis linear
C
C       2 - x axis linear, y axis logarithmic
C
C       3 - x axis logarithmic, y axis linear
C
C       4 - x axis logarithmic, y axis logarithmic
C
C    The default values of these parameters make both axes linear.
C
C    If the parameters 'X/LOGARITHMIC.' and 'Y/LOGARITHMIC.' are reset
C    by the routine DISPLA, they are given values which make them
C    immune to being reset when 'SET.' = 2, 3, or 4 (see the discussion
C    of the argument LSET, above).
C
C In calls to the routines AGSETP, AGSETF, AGSETI AGSETC, AGGETP,
C AGGETF, AGGETI, and AGGETC:
C
C -- TPGN is a character string identifying a group of AUTOGRAPH
C    control parameters.  It is of the form 'K1/K2/K3/ . . . /Kn.'.
C    Each Ki is a keyword.  The keyword K1 specifies a group of control
C    parameters, K2 a subgroup of that group, K3 a subgroup of that
C    subgroup, etc.  See the AUTOGRAPH write-up in the graphics manual
C    for a more complete description of these parameter-group names and
C    the ways in which they may be abbreviated.
C
C -- FURA is an array, from which control-parameter values are to be
C    taken (the routine AGSETP) or into which they are to be stored
C    (the routine AGGETP).  Note that the array is real; all of the
C    AUTOGRAPH parameters are stored internally as reals.
C
C -- LURA is the length of the user array FURA.
C
C -- FUSR is a variable, from which a single control parameter value is
C    to be taken (the routine AGSETF) or in which it is to be returned
C    (the routine AGGETF).  Note that the variable is real.
C
C -- IUSR is a variable, from which a single-control parameter value is
C    to be taken (the routine AGSETI) or in which it is to be returned
C    (the routine AGGETI).  Note that, since the control parameters are
C    stored internally as reals, each of the routines AGSETI and AGGETI
C    does a conversion - from integer to real or vice-versa.  Note also
C    that AGSETI and AGGETI should only be used for parameters which
C    have intrinsically integral values.
C
C -- CUSR is a character variable from which a character string is to
C    be taken (the routine AGSETC) or into which it is to be retrieved
C    (the routine AGGETC).  The control parameter affected by the call
C    contains the floating-point equivalent of an integer identifier
C    returned by the routine which stashes the character string and
C    tendered to the routine which retrieves it (sort of the automated
C    equivalent of a hat check).  Note that AGSETC and AGGETC should
C    only be used for parameters which intrinsically represent character
C    strings.
C
C In calls to the routine AGSTUP:
C
C -- XDRA is an array of x coordinates of user data - usually, but not
C    necessarily, the same data which will later be used in calls to
C    the routine AGCURV.
C
C -- NVIX is the number of vectors of data in XDRA - if XDRA is doubly-
C    dimensioned, NVIX would normally have the value of its second
C    dimension, if XDRA is singly-dimensioned, a 1.
C
C -- IIVX is the index increment between vectors in XDRA - if XDRA is
C    doubly-dimensioned, IIVX would normally have the value of its
C    first dimension, if XDRA is singly-dimensioned, a dummy value.
C
C -- NEVX is the number of elements in each data vector in XDRA - if
C    XDRA is doubly-dimensioned, NEVX would normally have the value of
C    its first dimension, if XDRA is singly-dimensioned, the value of
C    that single dimension.
C
C -- IIEX is the index increment between elements of a data vector in
C    XDRA - normally a 1.
C
C -- YDRA, NVIY, IIVY, NEVY, and IIEY are analogous to XDRA, NVIX,
C    IIVX, NEVX, and IIEX, but define y-coordinate data.
C
C In calls to the routine AGCURV:
C
C -- XVEC is a vector of x coordinate data.
C
C -- IIEX is the index increment between elements in XVEC.  AGCURV will
C    use XVEC(1), XVEC(1+IIEX), XVEC(1+2*IIEX), etc.
C
C -- YVEC is a vector of y coordinate data.
C
C -- IIEY is the index increment between elements in YVEC.  AGCURV will
C    use YVEC(1), YVEC(1+IIEY), YVEC(1+2*IIEY), etc.
C
C -- NEXY is the number of points defining the curve to be drawn.
C
C -- KDSH is a dashed-line selector.  Possible values are as follows:
C
C       If KDSH is zero, AUTOGRAPH will assume that the user has
C       called the routine DASHD (in the DASHCHAR package, which see)
C       to define the dashed-line pattern to be used.
C
C       If KDSH is less than zero and has absolute value M, AUTOGRAPH
C       will use the Mth (modulo 26) alphabetic dashed-line pattern.
C       Each of these patterns defines a solid line interrupted every
C       so often by a letter of the alphabet.
C
C       If KDSH is greater than zero and has the value M, AUTOGRAPH
C       will use the Mth (modulo N) dashed-line pattern in the group
C       of N dashed-line patterns defined by the AUTOGRAPH control
C       parameters in the group named 'DASH/PATTERNS.'.  The default
C       values of these parameters specify solid lines.
C
C In calls to the routines AGSAVE and AGRSTR:
C
C -- IFNO is the unit number associated with a file to which a single
C    unformatted logical record of data is to be written, or from which
C    such a record is to be read, by AUTOGRAPH.  The file is not rewound
C    before being written or read; positioning it properly is the user's
C    responsibility.
C
C In calls to the function AGBNCH:
C
C -- IDSH is a 16-bit binary dash pattern, the character equivalent of
C    which is to be returned as the value of AGBNCH.
C
C In calls to the function AGDSHN:
C
C -- IDSH is the number of the dash pattern parameter whose name is to
C    be returned as the value of the function AGDSHN.
C
C In calls to the routine AGUTOL:
C
C -- IAXS is the number of the axis.  The values 1, 2, 3, and 4 imply
C    the left, right, bottom, and top axes, respectively.
C
C -- FUNS is the value of the parameter 'AXIS/s/FUNCTION.' which may be
C    used to select the desired mapping function for axis IAXS.  It is
C    recommended that the default value (zero) be used to specify the
C    identity mapping.  A non-zero value may be integral (1., 2., etc.)
C    and serve purely to select the code to be executed or it may be the
C    value of a real parameter in the equations defining the mapping.
C
C -- IDMA specifies the direction of the mapping.  A value greater than
C    zero indicates that VINP is a value in the user system and that
C    VOTP is to be a value in the label system, a value less than zero
C    the opposite.
C
C -- VINP is an input value in one coordinate system along the axis.
C
C -- VOTP is an output value in the other coordinate system along the
C    axis.
C
C In calls to the routine AGCHAX:
C
C -- IFLG is zero if a particular object is about to be drawn, non-zero
C    if it has just been drawn.
C
C -- IAXS is the number of the axis being drawn.  The values 1, 2, 3,
C    and 4 indicate the left, right, bottom, and top axes, respectively.
C
C -- IPRT indicates the part of the axis being drawn.  Possible values
C    are as follows:
C
C    -- 1 implies the line of the axis.
C
C    -- 2 implies a major tick.
C
C    -- 3 implies a minor tick.
C
C    -- 4 implies the mantissa of a numeric label.
C
C    -- 5 implies the exponent of a numeric label.
C
C -- VILS is the value in the label system at the point where the part
C    is being drawn.  For IPRT = 1, VILS is zero.
C
C In calls to the routine AGCHCU:
C
C -- IFLG is zero if a particular object is about to be drawn, non-zero
C    if it has just been drawn.
C
C -- KDSH is the value with which AGCURV was called, as follows:
C
C       AGCURV called by   Value of KDSH
C       ----------------   ----------------------------------------
C       EZY                1
C       EZXY               1
C       EZMY               "n" or "-n", where n is the curve number
C       EZMXY              "n" or "-n", where n is the curve number
C       the user program   the user value
C
C In calls to the routine AGCHIL:
C
C -- IFLG is zero if a particular object is about to be drawn, non-zero
C    if it has just been drawn.
C
C -- LBNM is a character variable containing the name of the label being
C    drawn.
C
C -- LNNO is the number of the line being drawn.
C
C In calls to the routine AGCHNL:
C
C -- IAXS is the number of the axis being drawn.  The values 1, 2, 3,
C    and 4 imply the left, right, bottom, and top axes, respectively.
C
C -- VILS is the value to be represented by the numeric label, in the
C    label system for the axis.  The value of VILS must not be altered.
C
C -- CHRM, on entry, is a character string containing the mantissa of
C    the numeric label, as it will appear if AGCHNL makes no changes.
C    If the numeric label includes a "times" symbol, it is represented
C    by a blank in CHRM.  (See IPXM, below.)  CHRM may be modified.
C
C -- MCIM is the length of CHRM - the maximum number of characters that
C    it will hold.  The value of MCIM must not be altered.
C
C -- NCIM, on entry, is the number of meaningful characters in CHRM.  If
C    CHRM is changed, NCIM should be changed accordingly.
C
C -- IPXM, on entry, is zero if there is no "times" symbol in CHRM; if
C    it is non-zero, it is the index of a character position in CHRM.
C    If AGCHNL changes the position of the "times" symbol in CHRM,
C    removes it, or adds it, the value of IPXM must be changed.
C
C -- CHRE, on entry, is a character string containing the exponent of
C    the numeric label, as it will appear if AGCHNL makes no changes.
C    CHRE may be modified.
C
C -- MCIE is the length of CHRE - the maximum number of characters that
C    it will hold.  The value of MCIE must not be altered.
C
C -- NCIE, on entry, is the number of meaningful characters in CHRE.  If
C    CHRE is changed, NCIE should be changed accordingly.
C
C ---------------------------------------------------------------------
C T H E   A U T O G R A P H   C O D E
C ---------------------------------------------------------------------
C
C Following is the AUTOGRAPH code.  Routines appear in alphabetic order.
C
      SUBROUTINE AGAXIS (IAXS,QTST,QSPA,WCWP,HCWP,XBGA,YBGA,XNDA,YNDA,
     +                   QLUA,UBGA,UNDA,FUNS,QBTP,BASE,QJDP,WMJL,WMJR,
     +                   QMNT,QNDP,WMNL,WMNR,QLTP,QLEX,QLFL,QLOF,QLOS,
     +                   DNLA,WCLM,WCLE,RFNL,QCIM,QCIE,WNLL,WNLR,WNLB,
     +                                                           WNLE)
C
C The routine AGAXIS is used to draw, tick-mark, and label an axis or,
C if ITST is non-zero, to pre-compute the amount of space which will be
C required for numeric labels when the axis is actually drawn.  AGAXIS
C assumes that the last call to the plot-package routine SET was as
C follows (or the equivalent thereof):
C
C    CALL SET (XLCW,XRCW,YBCW,YTCW,0.,1.,0.,1.,1)
C
C where XLCW, XRCW, YBCW, and YTCW are the coordinates of the left,
C right, bottom, and top edges of the curve window, stated as fractions
C of the appropriate edge of the plotter frame.
C
C The arguments of AGAXIS are as follows:
C
C -- IAXS is the number of the axis being drawn - 1, 2, 3, or 4, meaning
C    the left, right, bottom, and top axes, respectively.
C
C -- ITST is an integer specifying what the caller wishes AGAXIS to do,
C    as follows:
C
C    -- If ITST .LT. 0, AGAXIS is to draw only the axis, nothing else.
C
C    -- If ITST .EQ. 0, AGAXIS is to draw, tick, and label the axis.
C
C    -- If ITST .GT. 0, AGAXIS is to pre-compute the amount of space
C       which will be required for numeric labels.  If the labels will
C       not fit in the space provided, AGAXIS is instructed to take
C       action as follows:
C
C       -- ITST .EQ. 1 - no action.
C
C       -- ITST .EQ. 2 - shrink the labels.
C
C       -- ITST .EQ. 3 - re-orient the labels.
C
C       -- ITST .EQ. 4 - shrink and/or re-orient the labels.
C
C -- ISPA is a 0 or a 1, specifying whether or not the axis itself is
C    to be drawn.  If ISPA .NE. 0, the axis is suppressed.  Tick marks
C    and/or labels may still be drawn.
C
C -- WCWP is the width of the curve window, in plotter units.
C
C -- HCWP is the height of the curve window, in plotter units.
C
C -- XBGA, YBGA, XNDA, and YNDA are the x and y coordinates of the ends
C    of the axis.  X coordinates are stated as fractions of the width,
C    y coordinates as fractions of the height, of the curve window.  The
C    axis to be drawn must be either horizontal or vertical (at an angle
C    of 0, 90, 180, or 270 degrees).  The left side, right side, begin-
C    ning, and end of the axis are defined from the viewpoint of a demon
C    standing at (XBGA,YBGA) and staring balefully toward (XNDA,YNDA).
C
C -- LLUA, UBGA, and UNDA define the mapping of the "user" coordinate
C    system (used for data-point coordinates) onto the axis.  If LLUA
C    is zero, the mapping is linear; if LLUA is non-zero, the mapping
C    is logarithmic.  UBGA is the user-system value at the beginning of
C    the axis, UNDA the value at the end of the axis.  The subroutine
C    AGFTOL, which needs these parameters, is actually passed LLUA,
C    UBEG=F(UBGA), and UDIF=F(UNDA)-F(UBGA), where F is the function
C    F(X)=X or the function F(X)=ALOG10(X), depending on LLUA.
C
C -- FUNS is a function-selector, to be used in calls to AGUTOL, which
C    defines the mappings from the user system to the label system and
C    vice-versa for each of the four axes.  The functions defined must
C    be continuous, monotonic, and bounded within the user-system range
C    (UBGA,UNDA) and a little bit outside that range.  The positions
C    of numeric labels and tick marks are chosen in the label system,
C    mapped to the user system, and then onto the axis.
C
C -- NBTP and BASE specify how major ticks are to be positioned in the
C    label coordinate system.  See the routine AGNUMB (arguments NBTP,
C    SBSE, and EXMU) for a description of these arguments.  Note that
C    NBTP .EQ. 0 or BASE .EQ. 0. suppresses both major tick marks and
C    their labels.  Note:  SBSE .EQ. +BASE or -BASE, as needed.
C
C -- QJDP is the major-tick-mark dash pattern (0. .LE. QJDP .LE. 65535.)
C    QJDP .LE. 0 suppresses major ticks.
C
C -- WMJL and WMJR are the distances to the left and right ends of the
C    major tick marks, stated as fractions of the shortest side of the
C    curve window.  Values .EQ. 0 may be used to suppress one or both
C    portions.  Values .GE. 1 may be used to extend a given portion all
C    the way to the edge of the curve window.  (See routine AGCTKO.)
C
C -- NMNT is the number of minor tick marks to be placed between each
C    pair of consecutive major tick marks.  NMNT .EQ. 0 suppresses them.
C
C -- QNDP, WMNL, and WMNR are analogous to QJDP, WMJL, and WMJR, but
C    specify minor-tick-mark characteristics.
C
C -- NLTP, NLEX, and NLFL specify the graphic form of numeric labels, as
C    described in the routine AGNUMB (which see).  Note that NLTP .LE. 0
C    suppresses numeric labels.
C
C -- NLOF and NLOS are first and second choices for the numeric label
C    orientation.  Both must be multiples of 90, specifying an angle
C    measured in degrees counter-clockwise from a vector running from
C    left to right in the curve window.  If ITST .EQ. 0, AGAXIS uses
C    NLOF if it is .GE. 0, NLOS otherwise, for the label orientation.
C    If ITST .NE. 0, AGAXIS initially makes both NLOF and NLOS positive.
C    Then, if ITST .GE. 3, NLOF may or may not be made negative.  (To
C    set the sign of NLOF or NLOS, AGAXIS adds or subtracts 360*K.)
C
C -- DNLA is the desired distance of numeric labels from the axis,
C    positive to the left, negative to the right, of the axis.  The
C    magnitude of DNLA is the size of the gap between the axis and the
C    nearest edge of a label, expressed as a fraction of the smaller
C    dimension of the curve window.  See also RFNL, below.
C
C -- WCLM and WCLE are the desired widths of characters in the mantissa
C    or the exponent, respectively, of numeric labels, expressed as a
C    fraction of the smaller dimension of the curve window.  See also
C    RFNL, below.
C
C -- RFNL is a reduction factor, used as a multiplier for DNLA, WCLM,
C    and WCLE.  If ITST .NE. 0, RFNL is initially set to 1. - then, if
C    ITST .EQ. 2 or 4, it is reset as necessary to shrink the labels.
C
C -- MCIM and MCIE specify the maximum number of characters in the
C    mantissa and exponent, respectively, of a numeric label.  These
C    are input parameters if ITST .EQ. 0, output parameters otherwise.
C
C -- WNLL, WNLR, WNLB, and WNLE are the widths of numeric-label strips
C    on the left side, on the right side, at the beginning, and at the
C    end, of the axis.  These are both input and output parameters of
C    AGAXIS.  On input, they specify the amount of space available for
C    numeric labels - on output, they specify the amount of space used
C    (if ITST .EQ. 0) or required (if ITST .NE. 0).  Each is stated as
C    a fraction of either the width or the height of the curve window,
C    depending on the orientation of the axis in the curve window.
C
C The following common block contains other AUTOGRAPH variables, both
C real and integer, which are not control parameters.  The only ones
C actually used here are ISLD, MWCM, MWCE, and MDLA.  ISLD is a solid-
C line dash pattern (sixteen one bits).  MWCM, MWCE, and MDLA specify
C the minimum allowed values of the width of a character in a label
C mantissa, the width of a character in a label exponent, and the
C distance of a label from the axis.  All are in plotter coordinate
C units.
C
      COMMON /AGORIP/ SMRL , ISLD , MWCL,MWCM,MWCE,MDLA,MWCD,MWDQ ,
     +                INIF
      SAVE /AGORIP/
C
C The AUTOGRAPH function AGFPBN is of type integer.
C
      INTEGER AGFPBN
C
C Local data required are as follows:
C
C BFRM is a buffer in which the routine AGNUMB returns the characters of
C a label mantissa.  CTMP holds a sub-string from an AGPWRT call.
C
      CHARACTER*40 BFRM
      CHARACTER*40 CTMP
C
C BFRE is a buffer in which the routine AGNUMB returns the characters of
C a label exponent.
C
      CHARACTER*5 BFRE
C
C XMJT, YMJT, XMNT, and YMNT are used to hold x and y offsets to the
C endpoints of left-of-label and right-of-label portions of major and
C minor tick marks.
C
      DIMENSION XMJT(4),YMJT(4),XMNT(4),YMNT(4)
C
C SMJP is the minimum distance allowed between major tick marks, in
C plotter coordinate units.
C
      DATA SMJP / 1. /
C
C FBGM, FBGP, FNDM, and FNDP are the coordinates of points a little on
C either side of the beginning and end of the axis, as fractions of the
C distance along the axis.
C
      DATA FBGM / -0.000001 /
      DATA FBGP / +0.000001 /
      DATA FNDM / +0.999999 /
      DATA FNDP / +1.000001 /
C
C HCFW is an arithmetic statement function specifying the height of a
C character as a function of its width (not counting "white space").
C The value of the multiplier was determined heuristically, by trying
C various values and seeing which gave the best results.
C
      HCFW(WDTH)=1.25*WDTH
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C This is the initialization section of AGAXIS.
C
C Unpack integer values from floating-point arguments.
C
      ITST=IFIX(QTST)
      ISPA=IFIX(QSPA)
      LLUA=IFIX(QLUA)
      NBTP=IFIX(QBTP)
      NMNT=IFIX(QMNT)
      NLTP=IFIX(QLTP)
      NLEX=IFIX(QLEX)
      NLFL=IFIX(QLFL)
      NLOF=IFIX(QLOF)
      NLOS=IFIX(QLOS)
      MCIM=IFIX(QCIM)
      MCIE=IFIX(QCIE)
C
C Initialize the local flags which specify what entities to draw, using
C values appropriate for the following quick exit.
C
      LDAX=1-ISPA
      LDNL=0
      LDMN=0
C
C If AGAXIS is to draw only the axis, exit immediately.
C
      IF (ITST.LT.0) GO TO 800
C
C If either NBTP or BASE is zeroed, exit immediately.
C
      IF (NBTP.EQ.0.OR.BASE.EQ.0.) GO TO 800
C
C Re-initialize the flag controlling the drawing of numeric labels.
C
      IF (NLTP.NE.0) LDNL=1
C
C If this is not a test run, skip.
C
      IF (ITST.EQ.0) GO TO 101
C
C This is a test run - exit if there are no numeric labels.
C
      IF (LDNL.EQ.0) GO TO 800
C
C This is a test run and the axis is to have numeric labels - initialize
C the numeric-label orientation and sizing parameters.  Clobber drawing.
C
      NLOF=MOD(NLOF+3600,360)
      NLOS=MOD(NLOS+3600,360)
      RFNL=1.
      MCIM=0
      MCIE=0
      LDMJ=0
      LDMN=0
C
C The main body of the initialization follows.
C
C Compute the length of the smaller side of the curve window, in the
C plotter coordinate system.
C
  101 SCWP=AMIN1(WCWP,HCWP)
C
C Compute a set of direction numbers for the axis, in the curve-window
C coordinate system (the change in x and y from the beginning to the
C end of the axis).
C
      XDNA=XNDA-XBGA
      YDNA=YNDA-YBGA
C
C Compute the length of the axis in the plotter coordinate system and
C its direction cosines.
C
      XDNP=XDNA*WCWP
      YDNP=YDNA*HCWP
      AXLP=SQRT(XDNP*XDNP+YDNP*YDNP)
      XDCA=XDNP/AXLP
      YDCA=YDNP/AXLP
C
C Compute the axis orientation angle, in degrees counter-clockwise.
C
      IAOR=MOD(IFIX(57.2957795130823*ATAN2(YDCA,XDCA)+3600.5),360)
C
C Compute the multiplicative constants required to convert a fraction of
C the axis length to a fraction of the width or height of the curve
C window (a distance in x or y).
C
      CFAX=AXLP/WCWP
      CFAY=AXLP/HCWP
C
C Compute the multiplicative constants required to convert a fraction of
C the axis length to a fraction of the along-axis and perpendicular-to-
C axis sides of the curve window.
C
      CFAA=ABS(XDCA*CFAX+YDCA*CFAY)
      CFAP=ABS(XDCA*CFAY+YDCA*CFAX)
C
C Compute the quantities (UBEG) and (UDIF) for AGFTOL.
C
      IF (LLUA.NE.0) GO TO 102
C
      UBEG=UBGA
      UDIF=UNDA-UBGA
      GO TO 103
C
  102 UBEG=ALOG10(UBGA)
      UDIF=ALOG10(UNDA)-UBEG
C
C SMJT and SMNT are fractions of the axis length and specify the minimum
C space which must be available between two major ticks before the major
C ticks themselves or the minor ticks between them, respectively, may be
C drawn.
C
  103 SMJT=SMJP/AXLP
      SMNT=SMJT*FLOAT(NMNT+1)
C
C Initialize the fractional numeric-label character heights.
C
      FHCM=0.
      FHCE=0.
C
C If the axis has no numeric labels, skip the following code.
C
      IF (LDNL.EQ.0) GO TO 104
C
C Zero the numeric-label offset.
C
      FNLO=0.
C
C The numeric-label parameters are computed by an internal procedure
C (which see, below).
C
      ASSIGN 104 TO JMP3
      GO TO 500
C
C If this is a test run, skip the following code.
C
  104 IF (ITST.NE.0) GO TO 200
C
C This is not a test run.  First, set up the tick-mark parameters.
C
C Compute the multiplicative constant required to convert a fraction of
C the smaller dimension of the grid to a fraction of the axis length.
C
      CSFA=SCWP/AXLP
C
C Compute the widths of the left and right portions of the numeric-label
C space as fractions of the axis length, affixing an appropriate sign.
C
      FNLL=-WNLL/CFAP
      FNLR=+WNLR/CFAP
C
C Compute a jump parameter to sort out the axis orientations.
C
      JAOR=1+IAOR/90
C
C The routine AGCTKO is used to compute the rest of the tick parameters.
C
      CALL AGCTKO (XBGA,YBGA,XDCA,YDCA,CFAX,CFAY,CSFA,JAOR,   1,QJDP,
     +             WMJL,WMJR,FNLL,FNLR,MJ12,MJ34,XMJT,YMJT)
C
      CALL AGCTKO (XBGA,YBGA,XDCA,YDCA,CFAX,CFAY,CSFA,JAOR,NMNT,QNDP,
     +             WMNL,WMNR,FNLL,FNLR,MN12,MN34,XMNT,YMNT)
C
C Set the flags controlling the drawing of tick marks.
C
      LDMJ=MJ12+MJ34
      LDMN=MN12+MN34
      LDLR=-(LDMJ+LDMN)
C
C If no numeric labels are to be drawn, skip the following code.
C
      IF (LDNL.EQ.0) GO TO 117
C
C Numeric labels are to be drawn.  Precompute parameters which will be
C used to position labels relative to the axis.
C
C Compute the widths and heights of the longest possible label mantissa
C and exponent, as fractions of the length of the axis.
C
      FWLM=FLOAT(MCIM)*FWCM
      FWLE=FLOAT(MCIE)*FWCE
      FHLM=FHCM
      FHLE=FHCE
      IF (MCIE.EQ.0) FHLE=0.
C
C Jump on the label-to-axis orientation.
C
      GO TO (105,106,107,108) , JLAO
C
C Label is at a 0-degree angle to the axis.
C
  105 FBLP=-FHLM
      GO TO 109
C
C Label is at a 90-degree angle to the axis.
C
  106 FBLA=0.
      FBLQ=-FWLM-FWLE
      GO TO 110
C
C Label is at a 180-degree angle to the axis.
C
  107 FBLP=FHLM+FHLE
      GO TO 109
C
C Label is at a 270-degree angle to the axis.
C
  108 FBLA=0.
      FBLQ=FWLM+FWLE
      GO TO 110
C
C Label is parallel to the axis.
C
  109 FNLW=FHLM+.5*FHLE
      FBLQ=0.
      GO TO 111
C
C Label is perpendicular to the axis.
C
  110 FNLW=FWLM+FWLE
      FBLP=0.
C
C If the labels will not fit in the space provided, clobber them.
C
  111 IF (.999999*FNLW.LT.FNLR-FNLL) GO TO 112
C
      LDNL=0
      GO TO 117
C
C Jump on the signed value of the numeric-label distance from the axis.
C
  112 IF (DNLA) 113,114,115
C
C Labels are to the right of the axis.
C
  113 FNLC=FDLA+.5*FNLW
      FBLP=FDLA+.5*ABS(FBLP-FHLE)
      FBLQ=FDLA+.5*ABS(FBLQ+FWLM-FWLE)
      GO TO 116
C
C Labels are centered on the axis.
C
  114 FNLC=0.
      FBLP=0.
      FBLQ=0.
      GO TO 116
C
C Labels are to the left of the axis.
C
  115 FNLC=-(FDLA+.5*FNLW)
      FBLP=-(FDLA+.5*ABS(FBLP))
      FBLQ=-(FDLA+.5*ABS(FBLQ-FWLM+FWLE))
C
  116 FNLO=.5*(FNLL+FNLR)-FNLC
C
C If the axis would pass through the offset labels, clobber it.
C
      IF (FNLL*FNLR.LT.0.) LDAX=0
C
C Jump to draw numeric labels and/or tick marks.
C
      GO TO 200
C
C No numeric labels are to be drawn.  If no tick marks are to be drawn
C either, exit.
C
  117 IF (LDLR.EQ.0) GO TO 800
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C The following code directs the process of tick-marking and labelling
C the axis, using the internal procedures which follow it.  If the
C label-coordinate-system value 0 maps onto the axis, tick-marking and
C labelling are done in two passes, one starting at 0 and proceeding
C in a positive direction and the other starting at 0 and proceeding
C in a negative direction.  If the label-coordinate-system value 0 does
C not map onto the axis, only one pass is required.
C
C First, determine the label-coordinate-system values VBGM and VNDP at
C the points FBGM and FNDP, a little beyond the ends of the axis.
C
  200 CALL AGFTOL (IAXS,1,FBGM,VBGM,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,BASE)
      CALL AGFTOL (IAXS,1,FNDP,VNDP,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,BASE)
C
C If zero falls on the axis, jump to the two-pass section of the code.
C
      IF (VBGM*VNDP.LE.0.) GO TO 201
C
C We may tick-mark and label the axis in a single pass.  Compute an
C appropriate starting value for the exponent/multiplier EXMU.
C
      SBSE=SIGN(BASE,VBGM)
      CALL AGFTOL (IAXS,2,FBGM,EBGM,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,SBSE)
      CALL AGFTOL (IAXS,2,FNDP,ENDP,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,SBSE)
      EXMU=AMIN1(EBGM,ENDP)
      ETMP=AMOD(EXMU,1.)
      IF (ETMP.NE.0.) EXMU=EXMU-ETMP+.5+SIGN(.5,EXMU)
C
C Set the numeric-label-space limits for the beginning and end of the
C axis.
C
      FNLB=FBGM-WNLB/CFAA-.5*(FHCM+FHCE)
      FNLE=FNDP+WNLE/CFAA+.5*(FHCM+FHCE)
C
C Jump to an internal procedure to tick-mark and label the axis.  Return
C from there to the termination section of AGAXIS.
C
      ASSIGN 800 TO JMP1
      GO TO 300
C
C Tick marks and labels must be done in two passes.  First, draw the
C tick mark and/or label at the zero position in the label system, using
C an internal procedure below.  A number of parameters must be preset.
C
  201 CALL AGFTOL (IAXS,-1,0.,FRAX,VLCS,LLUA,UBEG,UDIF,FUNS,NBTP,BASE)
C
C Determine whether label is to be drawn or not.
C
      LDLB=0
      IF (LDNL.EQ.0) GO TO 202
      LDLB=1
C
C The mantissa portion of the label consists of the single character 0.
C
      BFRM(1:1)='0'
      NCIM=1
      IPXM=0
C
C The label has no exponent portion.
C
      NCIE=0
C
C Allow the user to change the numeric label.
C
      CALL AGCHNL (IAXS,VLCS,BFRM,40,NCIM,IPXM,BFRE,5,NCIE)
C
C Compute the length of the mantissa, the exponent, and the whole label.
C
      FLLM=FLOAT(NCIM)*FWCM
      FLLE=FLOAT(NCIE)*FWCE
      FLLB=FLLM+FLLE
C
C The numeric-label space begins and ends at impossible values.
C
      FNLB=-10.
      FNLE=+10.
C
C Force the labeler to update FNLB, rather than FNLE.
C
      FDIR=1.
C
C Jump to an internal procedure to draw the label and/or the tick mark.
C
  202 ASSIGN 203 TO JMP2
      GO TO 400
C
C Save the position of the zero-point (FRAX, expressed as a fraction of
C the axis length) and preset the parameter DZRT, which is the minimum
C distance from the zero-point at which a major tick mark could occur,
C and the parameter DZRL, which is the minimum distance from the zero-
C point at which a label could occur.  Set the label-space limit FNLE.
C Preset the internal-procedure exit parameter JMP1.
C
  203 ASSIGN 205 TO JMP1
      FZRO=FRAX
      DZRT=AMAX1(SMJT,1.6*FLOAT(LDNL)*FHCM)
      IF (LDNL.EQ.0) GO TO 204
      DZRL=FNLB-FZRO
      FNLE=FNDP+WNLE/CFAA+.5*(FHCM+FHCE)
C
C Do the portion of the axis lying in the direction specified by DZRT.
C If it is too short, skip it entirely.
C
  204 FRAX=FZRO+DZRT
      IF (FRAX.LT.FBGM.OR.FRAX.GT.FNDP) GO TO JMP1 , (205,800)
C
C Find out whether BASE must be negated for this portion.
C
      CALL AGFTOL (IAXS,1,FRAX,VLCS,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,BASE)
      SBSE=SIGN(BASE,VLCS)
C
C Compute a starting value of the exponent/multiplier EXMU.
C
      CALL AGFTOL (IAXS,2,FRAX,EXMU,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,SBSE)
      EXMU=EXMU-AMOD(EXMU,1.)+.5+SIGN(.5,EXMU)
C
C Jump to an internal procedure to draw the tick marks and/or labels.
C
      GO TO 300
C
C Set up to do the second portion of the axis, then go do it.
C
  205 ASSIGN 800 TO JMP1
      DZRT=-DZRT
      IF (LDNL.EQ.0) GO TO 204
      FNLB=FBGM-WNLB/CFAA-.5*(FHCM+FHCE)
      FNLE=FZRO-DZRL
      GO TO 204
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C The following is an internal procedure, exited via the assigned-go-to
C variable JMP1.  Its purpose is to tick-mark and label a portion of the
C axis (perhaps the entire axis) at positions determined by consecutive
C values of the parameter EXMU.  It prevents tick marks from piling up
C or passing through the label space and prevents overlapping of labels.
C Tick marks are drawn alternately from left to right or vice-versa.
C
C The caller has provided an initial value of EXMU, but we must consider
C possible minor tick marks in the interval (EXMU-1.,EXMU).
C
  300 EXMU=EXMU-1.
C
C Compute FRAX, which is the fractional distance along the axis, and
C VLCS, which is the value in the label coordinate system corresponding
C to the current value of EXMU.
C
      CALL AGFTOL (IAXS,-2,EXMU,FRAX,VLCS,LLUA,UBEG,UDIF,FUNS,NBTP,SBSE)
C
C Move the current values of EXMU, FRAX, and VLCS to ELST, FLST, and
C VLST, specifying the last values of these parameters.  Then increment
C EXMU by 1. and recompute FRAX and VLCS.  (The loop through consecutive
C values of EXMU begins here.)
C
  301 ELST=EXMU
      FLST=FRAX
      VLST=VLCS
C
      EXMU=EXMU+1.
      CALL AGFTOL (IAXS,-2,EXMU,FRAX,VLCS,LLUA,UBEG,UDIF,FUNS,NBTP,SBSE)
C
C FDIR indicates the direction, FDST the magnitude, of step along axis.
C
      FDIR=FRAX-FLST
      FDST=ABS(FDIR)
C
C Draw minor tick marks, if any, in the interval (FLST,FRAX).
C
      IF (LDMN.EQ.0.OR.FDST.LT.SMNT) GO TO 304
C
C Use the dashed-line pattern for minor tick marks.
C
      CALL DASHDB (AGFPBN(QNDP))
C
C Minor tick marks are equally spaced in the label-coordinate system.
C
      VINC=(VLCS-VLST)/FLOAT(NMNT+1)
C
      DO 303 I=1,NMNT
        VMNT=VLST+VINC*FLOAT(I)
        CALL AGFTOL (IAXS,-1,VMNT,FMNT,DUMI,LLUA,UBEG,UDIF,FUNS,NBTP,
     +                                                             SBSE)
        IF (FMNT.LT.FBGP.OR.FMNT.GT.FNDM) GO TO 303
        XPAX=XBGA+FMNT*XDNA
        YPAX=YBGA+FMNT*YDNA
        LDLR=-LDLR
        IF (LDLR.LT.0) GO TO 302
        CALL AGCHAX (0,IAXS,3,VMNT)
        IF (MN12.NE.0) CALL LINED (XPAX+XMNT(1),YPAX+YMNT(1),
     +                             XPAX+XMNT(2),YPAX+YMNT(2))
        IF (MN34.NE.0) CALL LINED (XPAX+XMNT(3),YPAX+YMNT(3),
     +                             XPAX+XMNT(4),YPAX+YMNT(4))
        CALL AGCHAX (1,IAXS,3,VMNT)
        GO TO 303
  302   CALL AGCHAX (0,IAXS,3,VMNT)
        IF (MN34.NE.0) CALL LINED (XPAX+XMNT(4),YPAX+YMNT(4),
     +                             XPAX+XMNT(3),YPAX+YMNT(3))
        IF (MN12.NE.0) CALL LINED (XPAX+XMNT(2),YPAX+YMNT(2),
     +                             XPAX+XMNT(1),YPAX+YMNT(1))
        CALL AGCHAX (1,IAXS,3,VMNT)
  303 CONTINUE
C
C If the end of the axis has been reached, return to caller.
C
  304 IF (FRAX.LT.FBGM.OR.FRAX.GT.FNDP) GO TO JMP1 , (205,800)
C
C Draw the major tick mark and/or the numeric label at FRAX.
C
      IF (FDST.LT.SMJT) GO TO 301
      LDLB=0
      IF (LDNL.EQ.0) GO TO 305
      CALL AGNUMB (NBTP,SBSE,EXMU,NLTP,NLEX,NLFL,BFRM,40,NCIM,IPXM,BFRE,
     +                                                           5,NCIE)
      CALL AGCHNL (IAXS,VLCS,BFRM,40,NCIM,IPXM,BFRE,5,NCIE)
C
C If this is not a test run, mantissa and exponent length are checked.
C
      IF (ITST.EQ.0.AND.(NCIM.GT.MCIM.OR.NCIE.GT.MCIE)) GO TO 305
      LDLB=1
      FLLM=FLOAT(NCIM)*FWCM
      FLLE=FLOAT(NCIE)*FWCE
      FLLB=FLLM+FLLE
C
C Use the next internal procedure to draw the major tick and/or label.
C
  305 ASSIGN 301 TO JMP2
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C The following is an internal procedure, exited via the assigned-go-to
C variable JMP2.  Its purpose is to draw the major tick mark and/or the
C numeric label at a specified point on the axis or, if ITST is .NE. 0,
C to predict the amount of space which will be required for such items.
C
C Jump if no label is to be drawn.
C
  400 IF (LDLB.EQ.0.OR.NCIM.LE.0) GO TO 410
C
C See if the label will fit without overlapping another label.  To do
C this, first compute its fractional length along the axis (FLAA).
C
      GO TO (401,402,401,402) , JLAO
C
C Label is parallel to the axis.  Allow for inter-label spacing.
C
  401 FLAA=FLLB+FWCM
      GO TO 403
C
C Label is perpendicular to the axis.  Ignore exponent portion.
C
  402 FLAA=1.6*FHCM
C
C Compute the fractional coordinates of the endpoints of the label
C (along the axis) and see if it will fit in the available label space.
C
  403 FLBB=FRAX-.5*FLAA
      FLBE=FRAX+.5*FLAA
C
      IF (FLBB.GE.FNLB.AND.FLBE.LE.FNLE) GO TO 407
C
C Label will not fit.  Omit it or, if this is a test run, see if any
C remedial action is to be taken.
C
      LDLB=0
      IF (ITST.EQ.0) GO TO 411
C
C This is a test run and we have two consecutive labels which overlap.
C See what can be done about it.
C
      GO TO (424,404,406,404) , ITST
C
C We are allowed to shrink the labels.  See if they are minimum-size
C already.  If so, the only other possibility is to re-orient them.
C
  404 IF (IWCM.LE.MWCM.AND.IWCE.LE.MWCE.AND.IDLA.LE.MDLA) GO TO 405
C
C If not, shrink them by an amount based on the extent of the overlap,
C reset the parameters affected, and start from square one.
C
      RFNL=AMIN1(.9,FDST/(FDST+AMAX1(FNLB-FLBB,FLBE-FNLE)))*RFNL
      MCIM=0
      MCIE=0
      ASSIGN 200 TO JMP3
      GO TO 500
C
C If labels have already been shrunk to minimum size, see if we can
C re-orient them.  If not, at least continue with finding the maximum
C mantissa and exponent lengths.
C
  405 IF (ITST.NE.4) GO TO 424
C
C Try re-orienting the labels.  If this has already been tried, or it it
C would be pointless, skip it, but continue with finding the maximum
C mantissa and exponent lengths.
C
  406 IF (NLOF.LT.0.OR.NLOS.EQ.NLOF.OR.JLAO.EQ.2.OR.JLAO.EQ.4) GO TO 424
C
C If re-orienting makes sense, reset the appropriate parameters and
C start from square one.
C
      NLOF=NLOF-360
      RFNL=1.
      MCIM=0
      MCIE=0
      ASSIGN 200 TO JMP3
      GO TO 500
C
C Label will fit.  Update the label space limits for next time.
C
  407 IF (FDIR.GE.0.) GO TO 408
      FNLE=FLBB
      GO TO 409
  408 FNLB=FLBE
C
C If this is not just a test shot, go off and draw the tick mark/label.
C
  409 IF (ITST.EQ.0) GO TO 411
C
C If this is a test shot, update the maximum mantissa and exponent
C lengths being generated and exit from this internal procedure.
C
      MCIM=MAX0(MCIM,NCIM)
      MCIE=MAX0(MCIE,NCIE)
      GO TO 424
C
C No label is to be drawn.  If this is a test shot, exit from this
C internal procedure without drawing the tick mark.
C
  410 IF (ITST.NE.0) GO TO 424
C
C Compute x and y coordinates of current axis point.
C
  411 XPAX=XBGA+FRAX*XDNA
      YPAX=YBGA+FRAX*YDNA
C
C Jump if no major tick-mark is to be drawn.  Otherwise, set up the
C dash pattern for major tick-marks.
C
      IF (LDMJ.EQ.0) GO TO 414
      CALL DASHDB (AGFPBN(QJDP))
C
C Flip the left-to-right/right-to-left direction flag.
C
      LDLR=-LDLR
C
C Draw the first portion of the tick mark.
C
      IF (LDLR) 413,414,412
C
  412 IF (MJ12.NE.0) THEN
        CALL AGCHAX (0,IAXS,2,VLCS)
        CALL LINED (XPAX+XMJT(1),YPAX+YMJT(1),XPAX+XMJT(2),YPAX+YMJT(2))
        CALL AGCHAX (1,IAXS,2,VLCS)
      END IF
      GO TO 414
C
  413 IF (MJ34.NE.0) THEN
        CALL AGCHAX (0,IAXS,2,VLCS)
        CALL LINED (XPAX+XMJT(4),YPAX+YMJT(4),XPAX+XMJT(3),YPAX+YMJT(3))
        CALL AGCHAX (1,IAXS,2,VLCS)
      END IF
C
C Draw the label, if any.
C
  414 IF (LDLB.EQ.0.OR.NCIM.LE.0) GO TO 421
C
C Compute the distances from (XPAX,YPAX) to the beginning of the label -
C along the axis (FBLA) and perpendicular to the axis (FBLP).  Each is a
C directed distance whose magnitude represents a fraction of the length
C of the axis.  The values depend on the label/axis orientation and the
C distance of the label from the axis.  In some cases, these quantities,
C or portions of them, have already been computed.
C
      GO TO (415,416,417,418) , JLAO
C
C Label is at a 0-degree angle to the axis.
C
  415 FBLA=-.5*FLLB
      GO TO 419
C
C Label is at a 90-degree angle to the axis.
C
  416 FBLP=FBLQ+FLLM
      IF (DNLA.EQ.0.) FBLP=.5*FLLB
      GO TO 419
C
C Label is at a 180-degree angle to the axis.
C
  417 FBLA=.5*FLLB
      GO TO 419
C
C Label is at a 270-degree angle to the axis.
C
  418 FBLP=FBLQ-FLLM
      IF (DNLA.EQ.0.) FBLP=-.5*FLLB
C
C Draw the mantissa portion of the label (excluding the "X", if any).
C
  419 DEEX=FBLA*XDCA+(FBLP+FNLO)*YDCA
      DEEY=FBLA*YDCA-(FBLP+FNLO)*XDCA
      CALL AGCHAX (0,IAXS,4,VLCS)
      IF (IPXM.EQ.0) THEN
        CALL AGPWRT (XPAX+CFAX*DEEX,
     +               YPAX+CFAY*DEEY,BFRM,NCIM,IWCM,NLOR,-1)
      ELSE
        CALL AGPWRT (XPAX+CFAX*(DEEX+(FLLM-3.*FWCM)*XDCL),
     +               YPAX+CFAY*(DEEY+(FLLM-3.*FWCM)*YDCL),
     +               BFRM,IPXM-1,IWCM,NLOR,+1)
        CTMP=BFRM(IPXM+1:NCIM)
        CALL AGPWRT (XPAX+CFAX*(DEEX+(FLLM-2.*FWCM)*XDCL),
     +               YPAX+CFAY*(DEEY+(FLLM-2.*FWCM)*YDCL),
     +               CTMP,NCIM-IPXM,IWCM,NLOR,-1)
      END IF
      DEEX=DEEX+FLLM*XDCL
      DEEY=DEEY+FLLM*YDCL
C
C Draw the "X" portion of the mantissa, if it was left out above.
C
      IF (IPXM.EQ.0) GO TO 420
      DEEX=DEEX-2.5*FWCM*XDCL
      DEEY=DEEY-2.5*FWCM*YDCL
      CALL LINE (XPAX+CFAX*(DEEX-.3*FWCM*(XDCL-YDCL)),
     +           YPAX+CFAY*(DEEY-.3*FWCM*(YDCL+XDCL)),
     +           XPAX+CFAX*(DEEX+.3*FWCM*(XDCL-YDCL)),
     +           YPAX+CFAY*(DEEY+.3*FWCM*(YDCL+XDCL)))
      CALL LINE (XPAX+CFAX*(DEEX-.3*FWCM*(XDCL+YDCL)),
     +           YPAX+CFAY*(DEEY-.3*FWCM*(YDCL-XDCL)),
     +           XPAX+CFAX*(DEEX+.3*FWCM*(XDCL+YDCL)),
     +           YPAX+CFAY*(DEEY+.3*FWCM*(YDCL-XDCL)))
      DEEX=DEEX+2.5*FWCM*XDCL
      DEEY=DEEY+2.5*FWCM*YDCL
  420 CALL AGCHAX (1,IAXS,4,VLCS)
C
C Draw the exponent portion of the label (if it has one).
C
      IF (NCIE.EQ.0) GO TO 421
      DEEX=DEEX-.5*FHCM*YDCL
      DEEY=DEEY+.5*FHCM*XDCL
      CALL AGCHAX (0,IAXS,5,VLCS)
      CALL AGPWRT (XPAX+CFAX*DEEX,YPAX+CFAY*DEEY,BFRE,NCIE,IWCE,NLOR,-1)
      CALL AGCHAX (1,IAXS,5,VLCS)
C
C Draw the second portion of the tick mark, if any.
C
  421 IF (LDLR) 423,424,422
C
  422 IF (MJ34.NE.0) THEN
        CALL AGCHAX (0,IAXS,2,VLCS)
        CALL LINED (XPAX+XMJT(3),YPAX+YMJT(3),XPAX+XMJT(4),YPAX+YMJT(4))
        CALL AGCHAX (1,IAXS,2,VLCS)
      END IF
      GO TO 424
C
  423 IF (MJ12.NE.0) THEN
        CALL AGCHAX (0,IAXS,2,VLCS)
        CALL LINED (XPAX+XMJT(2),YPAX+YMJT(2),XPAX+XMJT(1),YPAX+YMJT(1))
        CALL AGCHAX (1,IAXS,2,VLCS)
      END IF
C
C Exit from internal procedure.
C
  424 GO TO JMP2 , (203,301)
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C The following is an internal procedure, exited via the assigned-go-to
C variable JMP3.  Its purpose is to compute all numeric-label parameters
C required by AGAXIS.
C
C Compute the desired label orientation and its direction cosines.
C
  500 NLOR=NLOF
      IF (NLOR.LT.0) NLOR=NLOS
C
      XDCL=COS(.017453292519943*FLOAT(NLOR))
      YDCL=SIN(.017453292519943*FLOAT(NLOR))
C
C Compute JLAO, which is a computed-go-to jump parameter specifying the
C label-to-axis orientation.
C
      JLAO=1+MOD(NLOR-IAOR+3600,360)/90
C
C Compute the width of a character in the label mantissa, the width of a
C character in the label exponent, and the distance of a label from the
C axis, in the plotter coordinate system.
C
      IWCM=MAX0(MWCM,IFIX(RFNL*ABS(WCLM)*SCWP+.5))
      IWCE=MAX0(MWCE,IFIX(RFNL*ABS(WCLE)*SCWP+.5))
      IDLA=MAX0(MDLA,IFIX(RFNL*ABS(DNLA)*SCWP+.5))
C
C Compute the same quantities as fractions of the axis length.
C
      FWCM=FLOAT(IWCM)/AXLP
      FWCE=FLOAT(IWCE)/AXLP
      FDLA=FLOAT(IDLA)/AXLP
C
C Compute character heights as fractions of the axis length.
C
      FHCM=HCFW(FWCM)
      FHCE=HCFW(FWCE)
C
C Return to internal-procedure caller.
C
      GO TO JMP3 , (104,200,801)
C
C *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C
C This is the termination section of AGAXIS.
C
C Update the parameters WNLL and WNLR to reflect the amount of space
C used/needed for numeric labels to the left and right of the axis.
C
  800 IF (LDNL.NE.0) GO TO 801
C
C No numeric labels occur on the axis.  Zero WNLL and WNLR and jump.
C
      WNLL=0.
      WNLR=0.
      GO TO 815
C
C Numeric labels do occur on the axis.  Compute the space required.
C
  801 GO TO (802,803,802,803) , JLAO
C
C Labels are parallel to the axis.
C
  802 FNLW=FHCM
      IF (MCIE.NE.0) FNLW=FNLW+.5*FHCE
      GO TO 804
C
C Labels are perpendicular to the axis.
C
  803 FNLW=FLOAT(MCIM)*FWCM+FLOAT(MCIE)*FWCE
C
C Jump on the numeric-label-distance-from-axis parameter DNLA.
C
  804 IF (DNLA) 805,806,807
C
C Labels are to the right of the axis.
C
  805 FNLL=-FDLA
      FNLR=+FDLA+FNLW
      GO TO 808
C
C Labels are centered on the axis.
C
  806 FNLL=+.5*FNLW
      FNLR=+.5*FNLW
      GO TO 808
C
C Labels are to the left of the axis.
C
  807 FNLL=+FDLA+FNLW
      FNLR=-FDLA
C
C Adjust FNLL and FNLR as implied by the numeric-label offset.
C
  808 FNLL=FNLL-FNLO
      FNLR=FNLR+FNLO
C
C If this is not a test run, jump to reset WNLL and WNLR.
C
      IF (ITST.EQ.0) GO TO 814
C
C If this is a test run, see if the labels will fit.  Jump if so.
C
      IF (CFAP*FNLL.LE.WNLL.AND.CFAP*FNLR.LE.WNLR) GO TO 814
C
C If the labels will not fit, we have a problem.  We may or may not be
C able to do anything about it, depending on ITST.
C
      GO TO (814,809,813,809) , ITST
C
C We are allowed to shrink the labels.  See if they are minimum-size
C already.  If so, the only other possibility is to re-orient them.
C
  809 IF (IWCM.LE.MWCM.AND.IWCE.LE.MWCE.AND.IDLA.LE.MDLA) GO TO 812
C
C If not, shrink them by an amount based on the extent of the problem,
C reset the parameters affected and see if the problem is solved.
C
      IF (WNLR+WNLL.GT.0.) GO TO 810
C
      RFNL=.000001*RFNL
      GO TO 811
C
  810 RFNL=AMIN1(.9,(WNLL+WNLR)/(CFAP*(FNLL+FNLR)))*RFNL
C
  811 ASSIGN 801 TO JMP3
      GO TO 500
C
C If labels have already been shrunk to minimum size, see if we can
C re-orient them.  If not, give up.
C
  812 IF (ITST.NE.3) GO TO 814
C
C Try re-orienting the labels.  If this has already been tried, or if it
C would be pointless, give up.
C
  813 IF (NLOF.LT.0.OR.NLOS.EQ.NLOF.OR.JLAO.EQ.1.OR.JLAO.EQ.3) GO TO 814
C
C If re-orienting makes sense, reset the parameters affected and see if
C the problem is solved.
C
      NLOF=NLOF-360
      RFNL=1.
      ASSIGN 801 TO JMP3
      GO TO 500
C
C Reset WNLL and WNLR for caller.
C
  814 WNLL=FNLL*CFAP
      WNLR=FNLR*CFAP
C
C If this is a test run, we are now done.
C
  815 IF (ITST.GT.0) GO TO 816
C
C Draw the axis, if it is to be drawn.
C
      IF (LDAX.EQ.0) GO TO 816
C
      CALL DASHDB (ISLD)
      CALL AGCHAX (0,IAXS,1,0.)
      CALL LINED (XBGA,YBGA,XNDA,YNDA)
      CALL AGCHAX (1,IAXS,1,0.)
C
C Pack up integer values which might have been changed into the
C corresponding floating-point arguments.
C
  816 QLOF=FLOAT(NLOF)
      QLOS=FLOAT(NLOS)
      QCIM=FLOAT(MCIM)
      QCIE=FLOAT(MCIE)
C
C Done.
C
      RETURN
C
      END
