C
C	$Id: isosrf.f,v 1.1.1.1 1992-04-17 22:31:23 ncargd Exp $
C
      SUBROUTINE ISOSRF (T,LU,MU,LV,MV,MW,EYE,MUVWP2,SLAB,TISO,IFLAG)
C
C DIMENSION OF           T(LU,LV,MW),EYE(3),SLAB(MUVWP2,MUVWP2)
C ARGUMENTS
C
C PURPOSE                ISOSRF draws a representation of the surface
C                        defined by the equation t(u,v,w)=tiso, where
C                        the function "t" is approximated by a three-
C                        dimensional array of data.  Contours created
C                        by taking slices in any of three directions
C                        through the surface are drawn with hidden
C                        portions of the contour lines removed.
C
C USAGE                  If the following assumptions are met, use
C
C                            CALL EZISOS (T,MU,MV,MW,EYE,SLAB,TISO)
C
C                               Assumptions:
C
C                                 -- All of the T array is to be used.
C                                 -- IFLAG is chosen internally.
C                                 -- FRAME is called by EZISOS.
C
C                        If the assumptions are not met, use
C
C                            CALL ISOSRF (T,LU,MU,LV,MV,MW,EYE,MUVWP2,
C                                         SLAB,TISO,IFLAG)
C
C ARGUMENTS
C
C ON INPUT               T
C                          Three-dimensional array of data approximating
C                          the function of three variables ("t").
C
C                        LU
C                          First dimension of T in the calling program.
C
C                        MU
C                          The number of data values of T to be
C                          processed in the U direction (the first
C                          subscript direction).  When processing the
C                          entire array, LU = MU (and LV = MV).
C
C                        LV
C                          Second dimension of T in the calling program.
C
C                        MV
C                          The number of data values of T to be
C                          processed in the V direction (the second
C                          subscript direction).
C
C                        MW
C                          The number of data values of T to be
C                          processed in the W direction (the third
C                          subscript direction).
C
C                        EYE
C                          The position of the eye in three-space.  T is
C                          considered to be in a box with opposite
C                          corners (1,1,1) and (MU,MV,MW).  The eye is
C                          at (EYE(1),EYE(2),EYE(3)), which must be
C                          outside the box that contains T.  While
C                          gaining experience with the routine, a good
C                          choice for EYE is (5.0*MU,3.5*MV,2.0*MW).
C
C                        MUVWP2
C                          The maximum of (MU,MV,MW)+2; that is,
C                          MUVWP2 = MAX(MU,MV,MW)+2.
C
C                        SLAB
C                          A work space used for internal storage.  SLAB
C                          must be at least MUVWP2*MUVWP2 words long.
C
C                        TISO
C                          The value "tiso" in the equation
C
C                            t(u,v,w)=tiso
C
C                          which defines the surface to be drawn.
C
C                        IFLAG
C                          This flag serves two purposes:
C
C                          .  First, the absolute value of IFLAG
C                             determines which types of lines are drawn
C                             to approximate the surface.  Three types
C                             of lines are considered:  lines of
C                             constant U, lines of constant V and lines
C                             of constant W.  The following table lists
C                             the types of lines drawn.
C
C                                                LINES OF CONSTANT
C                                                -----------------
C                             IABS(IFLAG)           U    V    W
C                             -----------          ---  ---  ---
C                                  1               NO   NO   YES
C                                  2               NO   YES  NO
C                                  3               NO   YES  YES
C                                  4               YES  NO   NO
C                                  5               YES  NO   YES
C                                  6               YES  YES  NO
C                             0, 7 OR MORE         YES  YES  YES
C
C                          .  Second, the sign of IFLAG determines what
C                             is inside and what is outside and so which
C                             lines are visible and what is done at the
C                             boundary of T.  For IFLAG:
C
C                             POSITIVE   T values greater than TISO are
C                                        assumed to be inside the solid
C                                        formed by the drawn surface.
C                             NEGATIVE   T values less than TISO are
C                                        assumed to be inside the solid
C                                        formed by the drawn surface.
C
C                             If the algorithm draws a cube, reverse the
C                             sign of IFLAG.
C
C ON OUTPUT              T,LU,MU,LV,MV,MW,EYE,MUVWP2,TISO and IFLAG are
C                        unchanged.  SLAB has been written in.
C
C NOTES                  .   Transformations can be achieved by
C                            adjusting scaling statement functions in
C                            ISOSRF, ISST3D, and ISGFXY.
C                        .   The hidden-line algorithm is not exact, so
C                            visibility errors can occur.
C                        .   Three-dimensional perspective character
C                            labeling of ISOSRF is possible by using
C                            the utility PWRZI.  For a description of
C                            the usage, see the PWRZI documentation.
C
C ENTRY POINTS           Principal routines:  ISOSRF and EZISOS.
C
C                        Parameter-access routines:  ISGETI, ISGETR,
C                        ISSETI, and ISSETR.
C
C                        Routines which are not currently advertised
C                        as user-calleable, but may be of interest to
C                        a knowledgeable user:  ISPLTF and ISTR32.
C
C                        Strictly internal routines:  ISFILL, ISGFXY,
C                        ISINIT, ISSMTH, ISST3D, ISTRCL, and ISZERO.
C
C                        A block data routine, in which default values
C                        of internal parameters are set:  ISBLDA.
C
C                        Also supported for historical reasons:  DRAWI,
C                        FRSTC, and TRN32I.
C
C COMMON BLOCKS          ISCOMN, PWRZ1I, TEMPRX
C
C REQUIRED LIBRARY       The ERPRT77 package and the SPPS.
C ROUTINES
C
C REQUIRED GKS LEVEL     0A
C
C I/O                    Plots surface.
C
C PRECISION              Single.
C
C LANGUAGE               FORTRAN 77.
C
C HISTORY                Developed for users of ISOSRFHR with smaller
C                        arrays.
C
C                        Bugs fixed and some improvements made in April
C                        and May of 1991.
C
C ALGORITHM              Cuts through the three-dimensional array are
C                        supplied with boundary values which ensure
C                        that all contour lines will be closed and then
C                        contoured by a routine which also marks a model
C                        of the plotter frame.  (Smoothing is optional.)
C                        The interiors of the contours in the model are
C                        then filled in and the result is ORed into
C                        a second model of the plotter frame that is
C                        used to determine the visibility of subsequent
C                        contour lines.
C
C TIMING                 Varies widely with size of T and the volume of
C                        the space enclosed by the surface drawn.
C
C ** NOTE **             Space requirements can be reduced by changing
C                        the sizes of the arrays ISCR and ISCA (found in
C                        the common block ISCOMN).  These need to hold
C                        512x256 bits apiece, so, on a 64-bit machine,
C                        ISCR and ISCA can be dimensioned 8x256 instead
C                        of the default 16x256.  On a 32-bit machine,
C                        16x256 will suffice.  On a 16-bit machine, the
C                        dimensions must be increased to 32x256.  If
C                        the first dimensions of ISCR and ISCA are
C                        changed, the value of the common variable LX
C                        (set in ISBLDA) must be changed to match.
C
C INTERNAL PARAMETERS    ISOSRF has several internal parameters.  The
C                        value of any of these parameters may be set
C                        by a call of the form
C
C                        CALL ISSETI (IPN,IVL)
C
C                        where IPN is a two-character string specifying
C                        the name of the parameter to be set and IVL is
C                        the desired integer value or by a call of the
C                        form
C
C                        CALL ISSETR (IPN,RVL)
C
C                        where IPN is as above and RVL is the desired
C                        real value.  The value of an internal parameter
C                        may be retrieved by one of the calls
C
C                        CALL ISGETI (IPN,IVL)
C                        CALL ISGETR (IPN,RVL)
C
C                        where IPN is as above, IVL is the returned
C                        integer value and RVL is the returned real
C                        value.
C
C                        The internal parameters are as follows:
C
C                        NAME  DEFAULT  FUNCTION
C                        ----  -------  --------------------------------
C
C                        'IU'     0     Number of extra slabs to be
C                                       interpolated between each pair
C                                       of slabs perpendicular to the
C                                       U axis.  Using a non-zero value
C                                       takes longer, but makes a better
C                                       picture.  Negate the value to
C                                       interpolate the extra slabs but
C                                       not draw them, which has the
C                                       effect of reducing errors due to
C                                       being able to see between slabs.
C                                       Attempting to give 'IU' a value
C                                       less than -10 will give it the
C                                       value -10; similarly, trying to
C                                       give it a value greater than 10
C                                       will give it the value 10.
C
C                        'IV'     0     Like 'IU', but applies to extra
C                                       slabs perpendicular to the V
C                                       axis.
C
C                        'IW'     0     Like 'IU', but applies to extra
C                                       slabs perpendicular to the W
C                                       axis.
C
C                        'RF'     1     Controls drawing of reference
C                                       planes and axes.  'RF' zero
C                                       means "don't draw them"; 'RF'
C                                       non-zero means "draw them".
C
C                        'RS'     0.    Zero turns off the "relative
C                                       size" feature; the 3D grid box
C                                       will be scaled to fill almost
C                                       the entire plotter frame.  Give
C                                       'RS' a non-zero value to turn on
C                                       the "relative size" feature and
C                                       specify that distance from which
C                                       the box, when viewed from the
C                                       direction which makes its image
C                                       largest, almost fills the frame.
C                                       Views from closer in will give a
C                                       bigger image and views from
C                                       farther out will give a smaller
C                                       image.  This is useful when
C                                       making movies with the "camera"
C                                       flying around the box.
C
C                        'SL'   .01     Segment length.  When contours
C                                       are smoothed, this parameter
C                                       specifies the approximate length
C                                       of the line segments used to
C                                       draw the smooth curves.  Values
C                                       outside the range from .0001 to
C                                       .1 will be mapped to the nearer
C                                       end of that range.
C
C                        'SM'     0     Screen model selector.  Zero
C                                       selects the coarse screen model
C                                       (128x128); non-zero selects a
C                                       finer screen model (256x256).
C                                       The former is faster; the latter
C                                       gives better pictures.
C
C                        'ST'     0.    Spline tension.  If zero, turns
C                                       off the smoothing of contours.
C                                       If non-zero, turns the smoother
C                                       on and determines the tension
C                                       on the splines used to do the
C                                       smoothing.  Use of values
C                                       greater than about 15 has been
C                                       observed to cause overflow on
C                                       some machines.
C
C                        'SV'     0     Special value.  The value zero
C                                       turns off the special-value
C                                       feature.  A non-zero real value
C                                       turns on the feature and gives
C                                       the special value itself.
C                                       Regions filled with this value
C                                       are treated as being outside the
C                                       object drawn.  We do not have
C                                       enough experience to say how
C                                       well this works in general;
C                                       preliminary experiments seem to
C                                       indicate that sometimes it works
C                                       well and sometimes it doesn't
C                                       (depending on the pattern of the
C                                       special values in the field).
C
C                        'VL'     0.    The position of the left edge
C                                       of the viewport in which the
C                                       iso-surface is to be displayed,
C                                       expressed as a fraction between
C                                       0 (the left edge of the frame)
C                                       and 1 (the right edge of the
C                                       frame).
C
C                        'VR'     0.    The position of the right edge
C                                       of the viewport in which the
C                                       iso-surface is to be displayed,
C                                       expressed as a fraction between
C                                       0 (the left edge of the frame)
C                                       and 1 (the right edge of the
C                                       frame).
C
C                        'VB'     0.    The position of the bottom edge
C                                       of the viewport in which the
C                                       iso-surface is to be displayed,
C                                       expressed as a fraction between
C                                       0 (the bottom edge of the frame)
C                                       and 1 (the top edge of the
C                                       frame).
C
C                        'VT'     0.    The position of the top edge
C                                       of the viewport in which the
C                                       iso-surface is to be displayed,
C                                       expressed as a fraction between
C                                       0 (the bottom edge of the frame)
C                                       and 1 (the top edge of the
C                                       frame).
C
C
      DIMENSION T(LU,LV,MW),EYE(3),SLAB(MUVWP2,MUVWP2)
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
C
C Force the BLOCK DATA routine to load.
C
      EXTERNAL ISBLDA
C
C Arithmetic statement function for taking an average of two reals.
C
      AVE(A,B) = (A+B)*.5
C
C Arithmetic statement functions for scaling.
C
      SU(UTEMP) = UTEMP
      SV(VTEMP) = VTEMP
      SW(WTEMP) = WTEMP
C
C  Initialize machine constants.
C
      IF (IDONE.EQ.0) THEN
        CALL ISINIT
        IDONE=1
      END IF
C
C In 3-space, we use variables U, V, W, IU, IV, IW, etc.  In 2-space,
C we use X, Y, IX, IY, etc.  Initialize.
C
      NU = MU
      NUP2 = NU+2
      NV = MV
      NVP2 = NV+2
      NW = MW
      NWP2 = NW+2
      FNU = NU
      FNV = NV
      FNW = NW
      SU1 = SU(1.)
      SV1 = SV(1.)
      SW1 = SW(1.)
      SUNU = SU(FNU)
      SVNV = SV(FNV)
      SWNW = SW(FNW)
      AVEU = AVE(SU1,SUNU)
      AVEV = AVE(SV1,SVNV)
      AVEW = AVE(SW1,SWNW)
      EYEU = EYE(1)
      EYEV = EYE(2)
      EYEW = EYE(3)
      NUVWP2 = MUVWP2
      TVAL = TISO
      NFLAG = IABS(IFLAG)
      IF (NFLAG.EQ.0 .OR. NFLAG.GE.8) NFLAG = 7
C
C Set up scaling.
C
      CALL ISST3D (EYE,1.,FNU,1.,FNV,1.,FNW)
C
C Bound lower and left edge of slab.
C
      EDGE = -SIGN(BIG,REAL(IFLAG))
C
      DO  40 IUVW=1,NUVWP2
        SLAB(IUVW,1) = EDGE
        SLAB(1,IUVW) = EDGE
   40 CONTINUE
C
C Slices perpendicular to the U axis (V/W slices):
C
      IF (NFLAG .LT. 4) GO TO 100
      CALL ISZERO
C
      ISLBT = -1
C
C Determine whether the viewing direction turns the contours over.
C
      IF (EYEU.LT.AVEU) THEN
        IFLIP=1
      ELSE
        IFLIP=2
      END IF
C
      IF (IFLAG.LT.0) IFLIP=3-IFLIP
C
C Bound upper and right edge of slab.
C
      DO  50 IV=2,NVP2
        SLAB(IV,NWP2) = EDGE
   50 CONTINUE
      DO  60 IW=2,NWP2
        SLAB(NVP2,IW) = EDGE
   60 CONTINUE
C
C Go through 3D array in the U direction.  IUEW stands for "IU, Either
C Way".  Pick IU based on the relative values of AVEU and EYEU.
C
      DO  90 IUEW=1,NU
        IU = IUEW
        IF (EYEU .GT. AVEU) IU = NU+1-IUEW
        U = IU
C
C Load this slice of T into SLAB.
C
        IF (SVAL.EQ.0.) THEN
          DO  71 IV=1,NV
            DO  70 IW=1,NW
              SLAB(IV+1,IW+1) = T(IU,IV,IW)
   70       CONTINUE
   71     CONTINUE
        ELSE
          DO  73 IV=1,NV
            DO  72 IW=1,NW
              IF (T(IU,IV,IW).NE.SVAL) THEN
                SLAB(IV+1,IW+1) = T(IU,IV,IW)
              ELSE
                SLAB(IV+1,IW+1) = EDGE
              END IF
   72       CONTINUE
   73     CONTINUE
        END IF
C
C Contour this slab.
C
        CALL ISTRCL (SLAB,NUVWP2,NVP2,NWP2,TVAL)
C
C Construct the visibility array.
C
        CALL ISFILL
C
C If interpolation is turned on, fill in intermediate slabs.
C
        IF (NINU.NE.0.AND.IUEW.NE.NU) THEN
          IF (NINU.LT.0) ISDR=1
          IQ=IU+1
          IF (EYEU.GT.AVEU) IQ=IU-1
          DO  85 IINU=1,ABS(NINU)
            Q=REAL(IINU)/REAL(ABS(NINU)+1)
            P=1.-Q
            U=P*REAL(IU)+Q*REAL(IQ)
            IF (SVAL.EQ.0.) THEN
              DO  82 IV=1,NV
                DO  81 IW=1,NW
                  SLAB(IV+1,IW+1)=P*T(IU,IV,IW)+Q*T(IQ,IV,IW)
   81           CONTINUE
   82         CONTINUE
            ELSE
              DO  84 IV=1,NV
                DO  83 IW=1,NW
                  IF (T(IU,IV,IW).NE.SVAL.AND.T(IQ,IV,IW).NE.SVAL) THEN
                    SLAB(IV+1,IW+1)=P*T(IU,IV,IW)+Q*T(IQ,IV,IW)
                  ELSE
                    SLAB(IV+1,IW+1)=EDGE
                  END IF
   83           CONTINUE
   84         CONTINUE
            END IF
            CALL ISTRCL (SLAB,NUVWP2,NVP2,NWP2,TVAL)
            CALL ISFILL
   85     CONTINUE
          ISDR=0
        END IF
C
   90 CONTINUE
C
C Slices perpendicular to the V axis (U/W slices):
C
  100 IF (MOD(NFLAG/2,2) .EQ. 0) GO TO 160
      CALL ISZERO
C
      ISLBT = 0
C
C Determine whether the viewing direction turns the contours over.
C
      IF (EYEV.GT.AVEV) THEN
        IFLIP=1
      ELSE
        IFLIP=2
      END IF
C
      IF (IFLAG.LT.0) IFLIP=3-IFLIP
C
C Bound upper and right edge of slab.
C
      DO 110 IU=2,NUP2
        SLAB(IU,NWP2) = EDGE
  110 CONTINUE
      DO 120 IW=2,NWP2
        SLAB(NUP2,IW) = EDGE
  120 CONTINUE
C
C Go through 3D array in the V direction.  IVEW stands for "IV, Either
C Way".  Pick IV based on the relative values of AVEV and EYEV.
C
      DO 150 IVEW=1,NV
        IV = IVEW
        IF (EYEV .GT. AVEV) IV = NV+1-IVEW
        V = IV
C
C Load this slice of T into SLAB.
C
        IF (SVAL.EQ.0.) THEN
          DO 131 IU=1,NU
            DO 130 IW=1,NW
              SLAB(IU+1,IW+1) = T(IU,IV,IW)
  130       CONTINUE
  131     CONTINUE
        ELSE
          DO 133 IU=1,NU
            DO 132 IW=1,NW
              IF (T(IU,IV,IW).NE.SVAL) THEN
                SLAB(IU+1,IW+1) = T(IU,IV,IW)
              ELSE
                SLAB(IU+1,IW+1) = EDGE
              END IF
  132       CONTINUE
  133     CONTINUE
        END IF
C
C Contour this slab.
C
        CALL ISTRCL (SLAB,NUVWP2,NUP2,NWP2,TVAL)
C
C Construct the visibility array.
C
        CALL ISFILL
C
C If interpolation is turned on, fill in intermediate slabs.
C
        IF (NINV.NE.0.AND.IVEW.NE.NV) THEN
          IF (NINV.LT.0) ISDR=1
          IQ=IV+1
          IF (EYEV.GT.AVEV) IQ=IV-1
          DO 145 IINV=1,ABS(NINV)
            Q=REAL(IINV)/REAL(ABS(NINV)+1)
            P=1.-Q
            V=P*REAL(IV)+Q*REAL(IQ)
            IF (SVAL.EQ.0.) THEN
              DO 142 IU=1,NU
                DO 141 IW=1,NW
                  SLAB(IU+1,IW+1)=P*T(IU,IV,IW)+Q*T(IU,IQ,IW)
  141           CONTINUE
  142         CONTINUE
            ELSE
              DO 144 IU=1,NU
                DO 143 IW=1,NW
                  IF (T(IU,IV,IW).NE.SVAL.AND.T(IU,IQ,IW).NE.SVAL) THEN
                    SLAB(IU+1,IW+1)=P*T(IU,IV,IW)+Q*T(IU,IQ,IW)
                  ELSE
                    SLAB(IU+1,IW+1)=EDGE
                  END IF
  143           CONTINUE
  144         CONTINUE
            END IF
            CALL ISTRCL (SLAB,NUVWP2,NUP2,NWP2,TVAL)
            CALL ISFILL
  145     CONTINUE
          ISDR=0
        END IF
C
  150 CONTINUE
C
C Slices perpendicular to the W axis (U/V slices):
C
  160 IF (MOD(NFLAG,2) .EQ. 0) GO TO 220
      CALL ISZERO
C
      ISLBT = 1
C
C Determine whether the viewing direction turns the contours over.
C
      IF (EYEW.LT.AVEW) THEN
        IFLIP=1
      ELSE
        IFLIP=2
      END IF
C
      IF (IFLAG.LT.0) IFLIP=3-IFLIP
C
C Bound upper and right edge of slab.
C
      DO 170 IU=2,NUP2
        SLAB(IU,NVP2) = EDGE
  170 CONTINUE
      DO 180 IV=2,NVP2
        SLAB(NUP2,IV) = EDGE
  180 CONTINUE
C
C Go through 3D array in the W direction.  IWEW stands for "IW, Either
C Way".  Pick IW based on the relative values of AVEW and EYEW.
C
      DO 210 IWEW=1,NW
        IW = IWEW
        IF (EYEW .GT. AVEW) IW = NW+1-IWEW
        W = IW
C
C Load this slice of T into SLAB.
C
        IF (SVAL.EQ.0.) THEN
          DO 191 IU=1,NU
            DO 190 IV=1,NV
              SLAB(IU+1,IV+1) = T(IU,IV,IW)
  190       CONTINUE
  191     CONTINUE
        ELSE
          DO 193 IU=1,NU
            DO 192 IV=1,NV
              IF (T(IU,IV,IW).NE.SVAL) THEN
                SLAB(IU+1,IV+1) = T(IU,IV,IW)
              ELSE
                SLAB(IU+1,IV+1) = EDGE
              END IF
  192       CONTINUE
  193     CONTINUE
        END IF
C
C Contour this slab.
C
        CALL ISTRCL (SLAB,NUVWP2,NUP2,NVP2,TVAL)
C
C Construct the visibility array.
C
        CALL ISFILL
C
C If interpolation is turned on, fill in intermediate slabs.
C
        IF (NINW.NE.0.AND.IWEW.NE.NW) THEN
          IF (NINW.LT.0) ISDR=1
          IQ=IW+1
          IF (EYEW.GT.AVEW) IQ=IW-1
          DO 205 IINW=1,ABS(NINW)
            Q=REAL(IINW)/REAL(ABS(NINW)+1)
            P=1.-Q
            W=P*REAL(IW)+Q*REAL(IQ)
            IF (SVAL.EQ.0.) THEN
              DO 202 IU=1,NU
                DO 201 IV=1,NV
                  SLAB(IU+1,IV+1)=P*T(IU,IV,IW)+Q*T(IU,IV,IQ)
  201           CONTINUE
  202         CONTINUE
            ELSE
              DO 204 IU=1,NU
                DO 203 IV=1,NV
                  IF (T(IU,IV,IW).NE.SVAL.AND.T(IU,IV,IQ).NE.SVAL) THEN
                    SLAB(IU+1,IV+1)=P*T(IU,IV,IW)+Q*T(IU,IV,IQ)
                  ELSE
                    SLAB(IU+1,IV+1)=EDGE
                  END IF
  203           CONTINUE
  204         CONTINUE
            END IF
            CALL ISTRCL (SLAB,NUVWP2,NUP2,NVP2,TVAL)
            CALL ISFILL
  205     CONTINUE
          ISDR=0
        END IF
C
  210 CONTINUE
C
C Draw reference-plane edges and the W axis.
C
  220 IF (IREF .EQ. 0) RETURN
      CALL ISTR32 (SU1,SV1,SW1,XT,YT,DUM,2)
      IF (EYEV .LT. SV1) GO TO 240
      CALL ISPLTF (XT,YT,1)
      DO 230 IX=1,NX
        RU=1.+REAL(NU-1)*(REAL(IX)/REAL(NX))
        CALL ISTR32 (SU(RU),SV1,SW1,XT,YT,DUM,2)
        CALL ISPLTF (XT,YT,2)
  230 CONTINUE
      GO TO 250
  240 CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,0)
      CALL ISTR32 (SUNU,SV1,SW1,XT,YT,DUM,2)
      CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,1)
  250 IF (EYEU .GT. SUNU) GO TO 270
      CALL ISPLTF (XT,YT,1)
      DO 260 IX=1,NX
        RV=1.+REAL(NV-1)*(REAL(IX)/REAL(NX))
        CALL ISTR32 (SUNU,SV(RV),SW1,XT,YT,DUM,2)
        CALL ISPLTF (XT,YT,2)
  260 CONTINUE
      GO TO 280
  270 CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,0)
      CALL ISTR32 (SUNU,SVNV,SW1,XT,YT,DUM,2)
      CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,1)
  280 IF (EYEV .GT. SVNV) GO TO 300
      CALL ISPLTF (XT,YT,1)
      DO 290 IX=1,NX
        RU=1.+REAL(NU-1)*(REAL(NX-IX)/REAL(NX))
        CALL ISTR32 (SU(RU),SVNV,SW1,XT,YT,DUM,2)
        CALL ISPLTF (XT,YT,2)
  290 CONTINUE
      GO TO 310
  300 CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,0)
      CALL ISTR32 (SU1,SVNV,SW1,XT,YT,DUM,2)
      CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,1)
  310 IF (EYEU .LT. SU1) GO TO 330
      CALL ISPLTF (XT,YT,1)
      DO 320 IX=1,NX
        RV=1.+REAL(NV-1)*(REAL(NX-IX)/REAL(NX))
        CALL ISTR32 (SU1,SV(RV),SW1,XT,YT,DUM,2)
        CALL ISPLTF (XT,YT,2)
  320 CONTINUE
      GO TO 340
  330 CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,0)
      CALL ISTR32 (SU1,SV1,SW1,XT,YT,DUM,2)
      CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,1)
  340 IF (EYEU.LE.SU1 .OR. EYEV.LE.SV1) GO TO 360
      CALL ISPLTF (XT,YT,1)
      DO 350 IX=1,NX
        RW=1.+REAL(NW-1)*(REAL(IX)/REAL(NX))
        CALL ISTR32 (SU1,SV1,SW(RW),XT,YT,DUM,2)
        CALL ISPLTF (XT,YT,2)
  350 CONTINUE
      RETURN
  360 CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,0)
      CALL ISTR32 (SU1,SV1,SWNW,XT,YT,DUM,2)
      CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,1)
      RETURN
      END
