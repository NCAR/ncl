C
C	$Id: gacolr.f,v 1.1.1.1 1992-04-17 22:31:18 ncargd Exp $
C
C-----------------------------------------------------------------------
C D E S C R I P T I O N   -   P A C K A G E   G R I D A L
C-----------------------------------------------------------------------
C
C PURPOSE                This package allows one to draw backgrounds
C                        for X/Y plots.  Included are routines for
C                        drawing grids, perimeters, and pairs of axes.
C
C USAGE                  Each user entry point in this package (GACOLR,
C                        GRID, GRIDAL, GRIDL, HALFAX, LABMOD, PERIM,
C                        PERIML, TICKS, and TICK4) is described below.
C                        Here, we discuss features of the package as a
C                        whole.
C
C                        Each of the routines GRID, GRIDL, HALFAX,
C                        PERIM, PERIML, and GRIDAL draws a background
C                        of some sort within the current GKS viewport,
C                        as follows:
C
C                          GRID draws an unlabelled grid.
C
C                          GRIDL draws a labelled grid.
C
C                          HALFAX draws a pair of intersecting axes.
C
C                          PERIM draws an unlabelled perimeter.
C
C                          PERIML draws a labelled perimeter.
C
C                          GRIDAL draws any of the above.
C
C                        Ticks, grid lines, and numeric labels are
C                        positioned as determined by the current GKS
C                        window and by the values of the two flags 'LS'
C                        and 'MI', in the SPPS package.  (The SPPS
C                        routine SET may be called to set the GKS
C                        viewport and window and the flags 'LS' and
C                        'MI'; the flag 'LS' determines whether the
C                        mappings of user "world" coordinates into the
C                        viewport are linear or logarithmic and the
C                        flag 'MI' whether the mappings are in the
C                        normal direction or are mirror-imaged.)
C
C                        The routines GACOLR, LABMOD, TICKS, and TICK4
C                        do no drawing.  Each is called to preset
C                        parameters which affect the behavior of a
C                        subsequent call to one of the background-
C                        drawing routines, as follows:
C
C                          GACOLR sets the color of background parts.
C
C                          LABMOD changes the appearance of labels.
C
C                          TICKS and TICK4 change the length and
C                          direction of ticks.
C
C ARGUMENTS              Several of the routines have arguments MJRX,
C                        MNRX, MJRY, and MNRY, which specify the number
C                        of major and minor divisions of the horizontal
C                        (X) and vertical (Y) axes of the current
C                        viewport.  These parameters have different
C                        meanings, depending on the current setting
C                        of the linear/log flag 'LS' in SPPS.
C
C                        If the axis is linear:  MJRX(Y) specifies the
C                        number of major divisions of the X(Y) axis and
C                        MNRX(Y) specifies the number of minor divisions
C                        within each major division.  In each case,
C                        the value specifies the number of spaces
C                        between grid lines or ticks rather than the
C                        number of lines or ticks.  There is always
C                        one more major division line or mark than the
C                        number of major divisions specified by MJRX(Y).
C                        Similarly, there is always one less minor
C                        division line or tick per major division
C                        than the number of minor divisions per major
C                        division specified by MNRX(Y).
C
C                        If the axis is logarithmic:  Each major
C                        division point occurs at a value 10**MJRX(Y)
C                        times the previous point.  For example, if
C                        the minimum X-axis value were 3., the maximum
C                        X-axis value 3000. and MJRX 1, then the major
C                        division points would be 3., 30., 300., and
C                        3000.  If MNRX(Y).LE.10, there are nine minor
C                        divisions within each major division.  For
C                        example, between 3. and 30., there would be
C                        minor division points at 6., 9., 12., ... 27.
C                        If MNRX(Y).GT.10., minor divisions are omitted.
C
C ENTRY POINTS           GALBEX, GRID, GRIDAL, GRIDL, HALFAX, LABMOD,
C                        PERIM, PERIML, TICKS, TICK4
C
C COMMON BLOCKS          GAREIN and GACHAR.
C
C REQUIRED ROUTINES      SETER and the SPPS package.
C
C REQUIRED GKS LEVEL     0A
C
C I/O                    Plots backgrounds.
C
C PRECISION              Single.
C
C LANGUAGE               FORTRAN 77.
C
C HISTORY                These routines have been a part of the NCAR
C                        System Plot Package for many years.  As part
C                        of the GKS effort, they were incorporated into
C                        a separate package.  The code here represents
C                        a total re-write in October, 1986.
C
C-----------------------------------------------------------------------
C D E S C R I P T I O N   -   S U B R O U T I N E   G A C O L R
C-----------------------------------------------------------------------
C
C PURPOSE                To set the color of various parts of the
C                        background.
C
C USAGE                  CALL GACOLR (KAXS,KLBL,KMJT,KMNT)
C
C ARGUMENTS              KAXS, KLBL, KMJT, and KMNT are the color
C                        indices of the desired colors of the axes,
C                        the labels, the major ticks/grid lines, and
C                        the minor ticks/grid lines, respectively.
C                        Values less than zero imply that no call is
C                        to be done to set the color before drawing
C                        items of the associated type.  The default
C                        value of each of the four parameters is -1.
C
C-----------------------------------------------------------------------
C D E S C R I P T I O N   -   S U B R O U T I N E   G R I D
C-----------------------------------------------------------------------
C
C PURPOSE                To draw an unlabelled grid.
C
C USAGE                  CALL GRID (MJRX,MNRX,MJRY,MNRY)
C
C ARGUMENTS              See the package description, above.
C
C-----------------------------------------------------------------------
C D E S C R I P T I O N   -   S U B R O U T I N E   G R I D A L
C-----------------------------------------------------------------------
C
C PURPOSE                A general entry point for all supported types
C                        of backgrounds.  Each of the other background-
C                        drawing routines is implemented by a call to
C                        GRIDAL.
C
C USAGE                  CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,IXLB,IYLB,
C                                     IGPH,XINT,YINT)
C
C ARGUMENTS              MJRX, MNRX, MJRY, and MNRY specify the major
C                        and minor divisions of the two axes, as
C                        described in the package description, above.
C
C                        IXLB and IYLB are integer flags, defined as
C                        follows:
C
C                          IXLB = -1  No X axis drawn.
C                                     No X-axis labels.
C
C                               =  0  X axis drawn.
C                                     No X-axis labels.
C
C                               =  1  X axis drawn.
C                                     X-axis labels.
C
C                          IYLB = -1  No Y axis drawn.
C                                     No Y-axis labels.
C
C                               =  0  Y axis drawn.
C                                     No Y-axis labels.
C
C                               =  1  Y axis drawn.
C                                     Y-axis labels.
C
C
C                        IGPH specifies the background type:
C
C                          IGPH      X axis      Y axis
C                          ----      ------      ------
C                           0        GRID        GRID
C                           1        GRID        PERIM
C                           2        GRID        HALFAX
C                           4        PERIM       GRID
C                           5        PERIM       PERIM
C                           6        PERIM       HALFAX
C                           8        HALFAX      GRID
C                           9        HALFAX      PERIM
C                          10        HALFAX      HALFAX
C
C                        XINT and YINT are the user "world" coordinates
C                        of the point of intersection of the two axes
C                        if IGPH equals 10.  For other values of
C                        IGPH for which one of the axes is of type
C                        HALFAX, XINT and/or YINT specify the position
C                        of that axis.
C
C-----------------------------------------------------------------------
C D E S C R I P T I O N   -   S U B R O U T I N E   G R I D L
C-----------------------------------------------------------------------
C
C PURPOSE                To draw a labelled grid.  Each major division
C                        is labelled with its numeric value.
C
C USAGE                  CALL GRIDL (MJRX,MNRX,MJRY,MNRY)
C
C ARGUMENTS              See the package description, above.
C
C-----------------------------------------------------------------------
C D E S C R I P T I O N   -   S U B R O U T I N E   H A L F A X
C-----------------------------------------------------------------------
C
C PURPOSE                To draw orthogonal axes intersecting at a
C                        specified point and with a specified set of
C                        labels.
C
C USAGE                  CALL HALFAX (MJRX,MNRX,MJRY,MNRY,XINT,YINT,
C                                     IXLB,IYLB)
C
C ARGUMENTS              All arguments are as defined for GRIDAL, above.
C                        In fact, the above call is equivalent to
C
C                          CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,IXLB,IYLB,
C                         +             10,XINT,YINT)
C
C-----------------------------------------------------------------------
C D E S C R I P T I O N   -   S U B R O U T I N E   L A B M O D
C-----------------------------------------------------------------------
C
C PURPOSE                To preset parameters controlling the appearance
C                        of labels drawn by GRIDAL, GRIDL, ... et al.
C                        LABMOD itself does no plotting and, in order
C                        to have any effect, must be called prior to
C                        the background-drawing routines for which it
C                        is presetting parameters.
C
C USAGE                  CALL LABMOD (FMTX,FMTY,NUMX,NUMY,ISZX,ISZY,
C                                     IXDC,IYDC,IXOR)
C
C ARGUMENTS              FMTX and FMTY are of type CHARACTER and contain
C                        format specifications for the X-axis and Y-axis
C                        numerical labels produced by GRIDAL, GRIDL,
C                        HALFAX, or PERIML.  The specification must
C                        begin with a left parenthesis and end with a
C                        right parenthesis and must not be more than
C                        ten characters long.  Conversions of types
C                        E, F, G, and I are allowed; for example, one
C                        might use FMTX='(F8.2)' and FMTY='(E10.0)'.
C                        The default for both formats is '(E10.3)'.
C
C                        NOTE:  I formats are allowed by this version
C                        of GRIDAL; they were not allowed by previous
C                        versions.
C
C                        NUMX, if non-zero, is the number of characters
C                        in each X-axis numeric label; if LBLX is a
C                        string produced by the format FMTX, then the
C                        label will be the substring LBLX(1:NUMX).  If
C                        NUMX is 0, then the label will be the substring
C                        LBLX(m:n), where LBLX(m:m) is the first non-
C                        blank character in LBLX, and LBLX(n:n) is the
C                        last non-blank character following LBLX(m:m).
C                        Using a non-zero NUMX causes the labels to be
C                        centered differently than if a zero value is
C                        used.  NUMY is defined similarly and applies
C                        to Y-axis labels.  The default value for both
C                        parameters is 0.
C
C                        ISZX and ISZY are character sizes for the
C                        labels, specified in plotter address units,
C                        just as for the SPPS routines PWRIT and WTSTR.
C                        The default value for both is 10.
C
C                        IXDC is the decrement, in plotter address
C                        units, from the left edge of the current
C                        viewport to the nearest X address of the
C                        label specified by FMTY, NUMY, and ISZY.
C                        For example, if the horizontal extent of the
C                        current viewport is defined by the normalized
C                        device coordinates .1 and .9, and if IXDC is
C                        60, and if there has been no call to the SPPS
C                        routine SETI, then labels on the Y axis will
C                        end at plotter coordinate 43 (.1*1023+1-60).
C                        Negative values may be used to put labels
C                        on the other side of the viewport; in the
C                        example given, changing IXDC to -878 (-.8*1023
C                        -60) would put the labels on the right side
C                        of the viewport, with their left edges 60
C                        plotter-coordinate units away from the edge
C                        of the viewport.  There are two special values
C                        of IXDC:
C
C                          If IXDC=0, the Y-axis labels will end 20
C                          plotter address units to the left of the
C                          viewport (equivalent to using IXDC=20).
C
C                          If IXDC=1, Y-axis labels will begin 20
C                          plotter address units to the right of the
C                          viewport (equivalent to using IXDC=-20-w,
C                          where w is the width of the viewport, in
C                          plotter address units).
C
C                        The default value of the X decrement is 20.
C
C                        When HALFAX is called or when GRIDAL is called
C                        with IGPH = 2, 6, or 10, IXDC is the distance
C                        from the Y axis, rather than from the minimum
C                        viewport coordinate, and the special values 0
C                        and 1 are equivalent to 20 and -20.
C
C                        IYDC is the decrement, in plotter address
C                        units, from the bottom edge of the current
C                        viewport to the nearest Y address of the
C                        label specified by FMTX, NUMX, and ISZX.
C                        Note that negative values may be used to put
C                        labels above the viewport.  There are two
C                        special values of IYDC:
C
C                          If IYDC=0, the top of the X-axis labels
C                          will be 20 plotter address units below the
C                          bottom edge of the viewport (equivalent to
C                          using IYDC=20).
C
C                          If IYDC=1, the bottom of the X-axis labels
C                          will be 20 plotter address units above the
C                          top edge of the viewport (equivalent to using
C                          IYDC=-20-h, where h is the height of the
C                          viewport, in plotter address units).
C
C                        The default value of the Y decrement is 20.
C
C                        When HALFAX is called or when GRIDAL is called
C                        with IGPH = 8, 9, or 10, IYDC is the distance
C                        from the X axis, rather than from the minimum
C                        viewport coordinate, and the special values 0
C                        and 1 are equivalent to 20 and -20.
C
C                        IXOR specifies the orientation of the X-axis
C                        labels:
C
C                          IXOR = 0    horizontal
C                               = 1    vertical
C
C                        The default orientation is horizontal.
C
C-----------------------------------------------------------------------
C D E S C R I P T I O N   -   S U B R O U T I N E   P E R I M
C-----------------------------------------------------------------------
C
C PURPOSE                To draw an unlabelled perimeter with inward-
C                        pointing tick marks.  The directions and
C                        lengths of tick marks may be changed by
C                        calling TICKS and/or TICK4 (which see, below).
C
C USAGE                  CALL PERIM (MJRX,MNRX,MJRY,MNRY)
C
C ARGUMENTS              See the package description, above.
C
C-----------------------------------------------------------------------
C D E S C R I P T I O N   -   S U B R O U T I N E   P E R I M L
C-----------------------------------------------------------------------
C
C PURPOSE                To draw a labelled perimeter with inward-
C                        pointing tick marks.  The directions and
C                        lengths of tick marks may be changed by
C                        calling TICKS and/or TICK4 (which see, below).
C
C USAGE                  CALL PERIML (MJRX,MNRX,MJRY,MNRY)
C
C ARGUMENTS              See the package description, above.
C
C-----------------------------------------------------------------------
C D E S C R I P T I O N   -   S U B R O U T I N E   T I C K S
C-----------------------------------------------------------------------
C
C PURPOSE                To allow program control of tick mark length
C                        and direction.  This routine has been
C                        superseded by TICK4, which should be used
C                        instead.
C
C USAGE                  CALL TICKS (LMJR,LMNR)
C
C ARGUMENTS              The above call is equivalent to
C
C                          CALL TICK4 (LMJR,LMNR,LMJR,LMNR)
C
C                        See the description of TICK4, below.
C
C-----------------------------------------------------------------------
C D E S C R I P T I O N   -   S U B R O U T I N E   T I C K 4
C-----------------------------------------------------------------------
C
C PURPOSE                To allow program control of tick mark length
C                        and direction.
C
C USAGE                  CALL TICK4 (LMJX,LMNX,LMJY,LMNY)
C
C ARGUMENTS              LMJX and LMNX are the lengths, in plotter
C                        address units, of major and minor ticks on
C                        the X axis.  The default values are 12 and 8.
C
C                        LMJY and LMNY are the lengths, in plotter
C                        address units, of major and minor ticks on
C                        the Y axis.  The default values are 12 and 8.
C
C                        By default, tick marks point inward.  Negative
C                        values of LMJX, LMNX, LMJY, and LMNY may be
C                        used to create outward-pointing tick marks.
C
C-----------------------------------------------------------------------
C C O D E   -   S U B R O U T I N E   G A C O L R
C-----------------------------------------------------------------------
C
      SUBROUTINE GACOLR (KAXS,KLBL,KMJT,KMNT)
C
C Declare the common block containing real or integer parameters.
C
        COMMON /GAREIN/ ICAX,ICLB,ICMJ,ICMN,ICWX,ICWY,IDCX,IDCY,IORX,
     +                  IMJX,IMJY,IMNX,IMNY,NCFX,NCFY
        SAVE   /GAREIN/
C
C Declare the block data "routine" external.  This should force it to
C be loaded.
C
        EXTERNAL GABLDT
C
C  The following is for gathering statistics on library use at NCAR.
C
        CALL Q8QST4 ('GRAPHX','GRIDAL','GACOLR','VERSION 01')
C
C Transfer the arguments to GRIDAL's common block.
C
        ICAX=KAXS
        ICLB=KLBL
        ICMJ=KMJT
        ICMN=KMNT
C
C Done.
C
        RETURN
C
      END
