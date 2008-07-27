C
C	$Id: srface.f,v 1.5 2008-07-27 00:17:26 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SRFACE (X,Y,Z,M,MX,NX,NY,S,STEREO)
C
C DIMENSION OF           X(NX),Y(NY),Z(MX,NY),M(2,NX,NY),S(6)
C ARGUMENTS
C
C PURPOSE                SRFACE draws a perspective picture of a
C                        function of two variables with hidden lines
C                        removed.  The function is approximated by a
C                        two-dimensional array of heights.
C
C USAGE                  If the following assumptions are met, use
C
C                             CALL EZSRFC (Z,M,N,ANGH,ANGV,WORK)
C
C                          assumptions:
C                              .the entire array is to be drawn,
C                              .the data are equally spaced (in the
C                               X-Y plane),
C                              .no stereo pairs,
C                              .scaling is chosen internally.
C
C                        If these assumptions are not met use
C
C                             CALL SRFACE (X,Y,Z,M,MX,NX,NY,S,
C                                          STEREO)
C
C ARGUMENTS
C
C ON INPUT               Z
C FOR EZSRFC               The M by N array to be drawn.
C
C                        M
C                          The first dimension of Z.
C
C                        N
C                          The second dimension of Z.
C
C                        ANGH
C                          Angle in degrees in the X-Y plane to the
C                          line of sight (counter-clockwise from
C                          the plus-X axis).
C
C                        ANGV
C                          Angle in degrees from the X-Y plane to
C                          the line of sight (positive angles are
C                          above the middle Z, negative below).
C
C                        WORK
C                          A scratch storage dimensioned at least
C                          2*M*N+M+N.
C
C ON OUTPUT              Z, M, N, ANGH, ANGV are unchanged.  WORK
C FOR EZSRFC             has been written in.
C
C
C ARGUMENTS
C
C ON INPUT               X
C FOR SRFACE               A linear array NX long containing the X
C                          coordinates of the points in the surface
C                          approximation, in increasing numerical order.
C                          See note, below.
C
C                        Y
C                          A linear array NY long containing the Y
C                          coordinates of the points in the surface
C                          approximation, in increasing numerical order.
C                          See note, below.
C
C                        Z
C                          An array MX by NY containing the surface
C                          to be drawn in NX by NY cells.
C                          Z(I,J) = F(X(I),Y(J)).  See note, below.
C
C                        M
C                          Scratch array at least 2*NX*NY words
C                          long.
C
C                        MX
C                          First dimension of Z.
C
C                        NX
C                          Number of data values in the X direction
C                          (the first subscript direction) in Z.
C                          to be plotted. When plotting an entire
C                          array, MX=NX.
C
C                        NY
C                          Number of data values in the Y direction
C                          (the second subscript direction) to be
C                          plotted.
C
C                        S
C                          S defines the line of sight.  The viewer's
C                          eye is at (S(1), S(2), S(3)) and the
C                          point looked at is at (S(4), S(5), S(6)).
C                          The eye should be outside the block with
C                          opposite corners (X(1), Y(1), ZMIN) and
C                          (X(NX), Y(NY), ZMAX) and the point looked
C                          at should be inside it.  For a nice
C                          perspective effect, the distance between
C                          the eye and the point looked at should be
C                          5 to 10 times the size of the block.  See
C                          note, below.
C
C                        STEREO
C                          Flag to indicate if stereo pairs are to
C                          be drawn.  0.0 means no stereo pair (one
C                          picture).  Non-zero means put out two
C                          pictures.  The value of STEREO is the
C                          relative angle between the eyes.  A value
C                          of 1.0 produces standard separation.
C                          Negative STEREO reverses the left and
C                          right figures.  See the documentation below
C                          for internal variable ISTP for additional
C                          information.
C
C ON OUTPUT              X, Y, Z, MX, NX, NY, S, STEREO are
C FOR SRFACE             unchanged.  M has been written in.
C
C NOTES                  . The range of Z compared with the range
C                          of X and Y determines the shape of the
C                          picture.  They are assumed to be in the
C                          same units and not wildly different in
C                          magnitude.  S is assumed to be in the
C                          same units as X, Y, and Z.
C                        . Picture size can be made relative to
C                          distance.  See comments in SETR.
C                        . TRN32S can be used to translate from 3
C                          space to 2 space.  See comments there.
C                        . Data with extreme discontinuities may
C                          cause visibility errors.  If this problem
C                          occurs, use a distant eye position
C                          away from the +Z axis.
C                        . The default line color is set to
C                          color index 1.  If the user wishes to
C                          change the line color, he can do so by
C                          defining color index 1 before calling
C                          SRFACE, or by putting the common block
C                          SRFINT in his calling program and
C                          defining and using color index ISRFMJ
C                          (defaulted to 1 in BLOCKDATA.)
C
C ENTRY POINTS           SRFACE, SRFGK, EZSRFC, SETR, DRAWS, TRN32S,
C                        CLSET, CTCELL, SRFABD
C
C COMMON BLOCKS          SRFACE, SRFINT, SRFBLK, PWRZIS, SRFIP1
C
C REQUIRED LIBRARY       The SPPS.
C ROUTINES
C
C REQUIRED GKS LEVEL     0A
C
C I/O                    Plots
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN
C
C HISTORY                Converted to FORTRAN 77 and GKS in March 1984.
C
C                        Prepared for SIGGRAPH, August 1976.
C
C                        Standardized in January 1973.
C
C                        Written in December 1971.  Replaced K.S.+G.
C                        algorithm called SOLIDS at NCAR.
C
C
C ALGORITHM              The data are processed from the near side of
C                        of the surface to the far side.  Visibility
C                        information is stored (see reference.)
C                        Highest so far is visible from above.
C
C REFERENCE              Wright, T.J., A Two Space Solution to the
C                        Hidden Line Problem for Plotting a Function
C                        of Two Variables.  IEEE Trans. Comp.,
C                        pp 28-33, January 1973.
C
C ACCURACY               If the ends of a line segment are visible,
C                        the middle is assumed visible.
C
C TIMING                 Proportional to NX*NY.
C
C
C INTERNAL PARAMETERS    name   default  function
C                        ----   -------  --------
C                        IFR        1    -1  Call FRAME first.
C                                         0  Do not call FRAME.
C                                        +1  Call FRAME when done.
C
C                        ISTP       0    STEREO type if STEREO
C                                        non-zero.
C                                        -1  Alternating frames,
C                                            slightly offset (for
C                                            movies,  IROTS = 0).
C                                         0  Blank frame between
C                                            for stereo slide.
C                                            IROTS = 1).
C                                        +1  Both on same frame.
C                                            (left picture to left
C                                            side.  IROTS = 0).
C
C                        IROTS      0     0  +Z in vertical plotting
C                                            direction (CINE mode).
C                                        +1  +Z in horizontal
C                                            plotting direction
C                                            (COMIC mode).
C
C                        IDRX       1    +1  Draw lines of constant
C                                            X.
C                                         0  Do not.
C
C                        IDRY       1    +1  Draw lines of constant
C                                            Y.
C                                         0  Do not.
C
C                        IDRZ       0    +1  Draw lines of constant
C                                            Z (contour lines).
C                                         0  Do not.
C
C                        IUPPER     0    +1  Draw upper side of
C                                            surface.
C                                         0  Draw both sides.
C                                        -1  Draw lower side.
C
C                        ISKIRT     0    +1  Draw a skirt around the
C                                            surface.
C                                            BOTTOM = HSKIRT.
C                                         0  Do not.
C
C                        NCLA       6    Approximate number of
C                                        levels of constant Z that
C                                        are drawn if levels are not
C                                        specified.  40 levels
C                                        maximum.
C
C                        THETA    .02    Angle, in radians, between
C                                        eyes for stereo pairs.
C
C                        HSKIRT    0.    Height of skirt
C                                            (if ISKIRT = 1).
C
C                        CHI       0.    Highest level of constant
C                                        Z.
C
C                        CLO       0.    Lowest level of constant Z.
C
C                        CINC      0.    Increment between levels.
C
C                          [If CHI, CLO, or CINC is zero, a nice
C                           value is generated automatically.]
C
C                        IOFFP     0     Flag to control use of special
C                                        value feature.  Do not have
C                                        both IOFFP=1 and ISKIRT=1.
C                                         0 Feature not in use
C                                        +1  Feature in use.  No lines
C                                            drawn to data points in Z
C                                            that are equal to SPVAL.
C
C                        SPVAL     0.    Special value used to mark un-
C                                        known data when IOFFP=1.
C
C
C
      DIMENSION       X(NX)      ,Y(NY)      ,Z(MX,NY)   ,M(2,NX,NY) ,
     1                S(6)
      DIMENSION       WIND(4)    ,VIEW(4)    ,LASF(13)
      COMMON /SRFINT/ ISRFMJ     ,ISRFMN     ,ISRFTX
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL SRFABD
C
      CALL Q8QST4 ('GRAPHX','SRFACE','SRFACE','VERSION 01')
C
C     THIS DRIVER SAVES THE CURRENT NORMALIZATION TRANSFORMATION
C     INFORMATION, DEFINES THE NORMALIZATION TRANSFORMATION
C     APPROPRIATE FOR SRFGK, CALLS SRFGK, AND RESTORES THE ORIGINAL
C     NORMALIZATION TRANSFORMATION.
C
C     Save current SET parameters.
C
      CALL GETSET (VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     +             WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
C
C
C
C
C
C     SET WINDOW AND VIEWPORT FOR SRFGK
C
      CALL SET(0.,1.,0.,1.,1.,1024.,1.,1024.,1)
C
C     SET LINE COLOR TO INDIVIDUAL (SAVE CURRENT SETTING)
C
      CALL GQASF (IER,LASF)
      LASFSV  = LASF(3)
      LASF(3) = 1
      CALL GSASF(LASF)
C
C     SET LINE COLOR INDEX TO COMMON VARIABLE ISRFMJ (SAVE
C     CURRENT SETTING)
C
      CALL GQPLCI (IER,LCISV)
      CALL GSPLCI (ISRFMJ)
C
C     DRAW PLOT
C
      CALL SRFGK (X,Y,Z,M,MX,NX,NY,S,STEREO)
C
C     RESTORE INITIAL LINE COLOR SETTINGS
C
      LASF(3) = LASFSV
      CALL GSASF(LASF)
      CALL GSPLCI (LCISV)
C
C     RESTORE ORIGINAL NORMALIZATION TRANSFORMATION
C
      CALL SET(VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     +         WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
C
C
      RETURN
      END
