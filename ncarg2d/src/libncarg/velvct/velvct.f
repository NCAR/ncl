C
C	$Id: velvct.f,v 1.1.1.1 1992-04-17 22:31:50 ncargd Exp $
C
      SUBROUTINE VELVCT (U,LU,V,LV,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
C
C SUBROUTINE VELVCT (U,LU,V,LV,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
C
C
C DIMENSION OF           U(LU,N),V(LV,N),SPV(2)
C ARGUMENTS
C
C PURPOSE                VELVCT draws a representation of a two-
C                        dimensional velocity field by drawing arrows
C                        from each data location.  The length of the
C                        arrow is proportional to the strength of the
C                        field at that location and the direction of
C                        the arrow indicates the direction of the flow
C                        at that location.
C
C USAGE                  If the following assumptions are met, use
C
C                               CALL EZVEC (U,V,M,N)
C
C                          Assumptions -
C
C                            --The whole array is processed.
C                            --The scale factor is chosen internally.
C                            --The perimeter is drawn.
C                            --FRAME is called after plotting.
C                            --There are no special values.
C
C                        If these assumptions are not met, use
C
C                               CALL VELVCT (U,LU,V,LV,M,N,FLO,HI,
C                                                 NSET,LENGTH,ISPV,SPV)
C
C ARGUMENTS
C
C ON INPUT               U,V
C
C                          The (origins of the) two-dimensional arrays
C                          containing the velocity field to be plotted.
C                          The vector at the point (I,J) has magnitude
C                          SQRT(U(I,J)**2+V(I,J)**2) and direction
C                          ATAN2(V(I,J),U(I,J)).  Other representations,
C                          such as (R,THETA), can be plotted by
C                          changing statement functions in this routine.
C
C                        LU
C
C                          The first dimension of U in the calling
C                          program.
C
C                        LV
C
C                          The first dimension of V in the calling
C                          program.
C
C                        M
C
C                          The number of data values to be plotted in
C                          the X-direction (the first subscript
C                          direction).  When plotting the entire array,
C                          LU = LV = M.
C
C                        N
C
C                          The number of data values to be plotted in
C                          the Y-direction (the second subscript
C                          direction).
C
C                        FLO
C
C                          The minimum vector magnitude to be shown.
C
C                        HI
C
C                          The maximum vector magnitude to be shown. (A
C                          value less than or equal to zero causes the
C                          maximum value of SQRT(U**2+V**2) to be used.)
C
C                        NSET
C
C                          Flag to control scaling -
C
C                          If NSET is zero, VELVCT establishes the
C                          window and viewport to properly
C                          scale plotting instructions to the standard
C                          configuration.  PERIM is called to draw a
C                          border.
C
C                          If NSET is greater than zero, VELVCT assumes
C                          that the user has established the window
C                          and viewport in such a way as to properly
C                          scale the plotting instructions generated
C                          by VELVCT.  PERIM is not called.
C
C                          If NSET is less than zero, VELVCT
C                          places the contour plot
C                          within the limits of the user's current
C                          window and viewport.  PERIM is not called.
C
C                        LENGTH
C
C                          The length, in Plotter Address Units (PAUs),
C                          of a vector having magnitude HI
C                          (or, if HI=0, the length in PAUs
C                          of the longest vector).  If LENGTH=0, a
C                          value is chosen such that the longest vector
C                          could just reach to the tail of the next
C                          vector.  If the horizontal and vertical
C                          resolutions of the plotter are different,
C                          LENGTH should be non-zero and specified as a
C                          horizontal distance.
C
C                        ISPV
C
C                          Flag to control the special value feature.
C
C                             0 means that the feature is not in use.
C
C                             1 means that if the value of
C                               U(I,J)=SPV(1) the vector will not be
C                               plotted.
C
C                             2 means that if the value of
C                               V(I,J)=SPV(2) the vector will not be
C                               plotted.
C
C                             3 means that if either U(I,J)=SPV(1) or
C                               V(I,J)=SPV(2) then the vector will not
C                               be plotted.
C
C                             4 means that if U(I,J)=SPV(1)
C                               and V(I,J)=SPV(2), the vector
C                               will not be plotted.
C
C                        SPV
C
C                        An array of length 2 which gives the value
C                        in the U array and the value in the V array
C                        which denote missing values.
C                        This argument is ignored if ISPV=0.
C
C
C ON OUTPUT              All arguments remain unchanged.
C
C NOTE                   The endpoints of each arrow drawn are (FX(X,Y),
C                        FY(X,Y)) and (MXF(X,Y,U,V,SFX,SFY,MX,MY),
C                        MYF(X,Y,U,V,SFX,SFY,MX,MY)) where X=I, Y=J,
C                        U=U(I,J), V=V(I,J), and SFX and SFY are scale
C                        factors.  Here I is the X-index and J is the
C                        Y-index.  (MX,MY) is the location of the tail.
C                        Thus the actual length of the arrow is
C                        SQRT(DX**2+DY**2) and the direction is
C                        ATAN2(DX,DY), where DX=MX-MXF(...) and
C                        DY=MY-MYF(...).
C
C ENTRY POINTS           VELVCT,EZVECT,DRWVEC,VELVEC,VELDAT
C
C COMMON BLOCKS          VEC1,VEC2
C
C I/O                    Plots the vector field.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN
C
C REQUIRED LIBRARY       GRIDAL and the SPPS
C ROUTINES
C
C REQUIRED GKS LEVEL     0A
C
C HISTORY                Written and standardized in November 1973.
C                        Revised in May, 1975, to include MXF and MYF.
C                        Revised in March, 1981, to fix certain errors;
C                        to use FL2INT and PLOTIT instead of MXMY,
C                        FRSTPT, and VECTOR; and to make the arrowheads
C                        narrower.  Converted to FORTRAN77 and GKS
C                        in July 1984.
C
C ALGORITHM              Each vector is examined, possibly transformed,
C                        then plotted.
C
C PORTABILITY            FORTRAN77
C
C ---------------------------------------------------------------------
C
C SPECIAL NOTE -
C
C Using this routine to put vectors on an arbitrary background drawn by
C SUPMAP is a bit tricky.  The arithmetic statement functions FX and FY
C are easy to replace.  The problem arises in replacing MXF and MYF.
C The following example may be helpful. (SUPMAP is an entry point in
C the EZMAP package.)
C
C Suppose that we have two arrays, CLON(36,9) and CLAT(36,9), which
C contain the E-W and N-S components of a wind flow field on the surface
C of the earth.  CLON(I,J) is the magnitude of the easterly flow.
C CLAT(I,J) is the magnitude of the northerly flow at a longitude (I-1)
C *10 degrees east of Greenwich and a latitude (J-1)*10 degrees north of
C the equator.  SUPMAP is to be used to draw a polar projection of the
C earth and VELVCT is to be used to superimpose vectors representing the
C flow field on it.  The following steps would be necessary:
C
C     1.  CALL SUPMAP (1,90.,0.,-90.,90.,90.,90.,90.,-4,10,0,1,IER)
C         to draw the map.
C
C     2.  CALL VELVCT (CLON,36,CLAT,36,36,9,0.,0.,1,50,0,0.) to put
C         vectors on it.  Notice that NSET has the value 1 to tell
C         VELVCT that SUPMAP has done the required SET call.
C
C     3.  In order to ensure that step 2 will work properly, delete
C         the arithmetic statement functions FX, FY, MXF, and MYF
C         from VELVCT and include the following functions.
C
C     FUNCTION FX(XX,YY)
C     CALL MAPTRN (10.*(YY-1.),10.*(XX-1.),X,Y)
C     FX=X
C     RETURN
C     END
C
C     FUNCTION FY(XX,YY)
C     CALL MAPTRN (10.*(YY-1.),10.*(XX-1.),X,Y)
C     FY=Y
C     RETURN
C     END
C
C     FUNCTION MXF(XX,YY,UU,VV,SFX,SFY,MX,MY)
C     CFCT=COS(.17453292519943*(YY-1.))
C     CALL MAPTRN(10.*(YY-1.)         ,10.*(XX-1.)              ,X1,Y1)
C     CALL MAPTRN(10.*(YY-1.)+1.E-6*VV,10.*(XX-1.)+1.E-6*UU/CFCT,X2,Y2)
C     U=((X2-X1)/SQRT((X2-X1)**2+(Y2-Y1)**2))*SQRT(UU**2+VV**2)
C     MXF=MX+IFIX(SFX*U)
C     RETURN
C     END
C
C     FUNCTION MYF(XX,YY,UU,VV,SFX,SFY,MX,MY)
C     CFCT=COS(.17453292519943*(YY-1.))
C     CALL MAPTRN(10.*(YY-1.)         ,10.*(XX-1.)              ,X1,Y1)
C     CALL MAPTRN(10.*(YY-1.)+1.E-6*VV,10.*(XX-1.)+1.E-6*UU/CFCT,X2,Y2)
C     V=((Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2))*SQRT(UU**2+VV**2)
C     MYF=MY+IFIX(SFY*V)
C     RETURN
C     END
C
C The basic notion behind the coding of the MXF and MYF functions is as
C follows.  Since UU and VV are the longitudinal and latitudinal components,
C respectively, of a velocity vector having units of distance over time,
C 1.E-6*UU/COS(latitude) and 1.E-6*VV represent the change in longitude
C and latitude, respectively, of a particle moving with the flow field
C for a very short period of time.  The routine MAPTRN is used to find
C the position of the particle's projection at the beginning and end of
C that tiny time slice and, therefore, the direction in which to draw
C the arrow representing the velocity vector so that it will be tangent
C to a projected flow line of the field at that point.  The values U
C and V are computed so as to give the arrow the length implied by UU
C and VV.  (The code ensures that SQRT(U**2+V**2) is equal to
C SQRT(UU**2+VV**2).)  The length of the arrow represents the magnitude
C of the velocity vector, unaffected by perspective.  The scaling set
C up by VELVCT will therefore be appropriate for the arrows drawn.
C
C This method is rather heuristic and has three inherent problems.
C First, the constant 1.E-6 may need to be made larger or smaller,
C depending on the magnitude of your U/V data.  Second, the north and
C south poles must be avoided.  At either pole, CFCT goes to zero,
C giving a division by zero; in a small region near the pole, the
C method may try to use MAPTRN with a latitude outside the range
C (-90,+90).  Third, the projection must be set up so as to avoid
C having vector basepoints at the exact edge of the map.  Vectors
C there will be of the correct length, but they may be drawn in the
C wrong direction (when the projected particle track determining the
C direction crosses the edge and reappears elsewhere on the map).
C With a little care, the desired results may be obtained.
C ---------------------------------------------------------------------
C
C DECLARATIONS -
C
      COMMON /VEC1/   ASH        ,EXT        ,ICTRFG     ,ILAB       ,
     +                IOFFD      ,IOFFM      ,ISX        ,ISY        ,
     +                RMN        ,RMX        ,SIDE       ,SIZE       ,
     +                XLT        ,YBT        ,ZMN        ,ZMX
C
      COMMON /VEC2/   BIG        ,INCX       ,INCY
C
C FORCE THE BLOCK DATA ROUTINE, WHICH SETS DEFAULT VARIABLES, TO LOAD.
C
      EXTERNAL        VELDAT
C
C ARGUMENT DIMENSIONS.
C
      DIMENSION       U(LU,N)    ,V(LV,N)    ,SPV(2)
        CHARACTER*10    LABEL
        REAL WIND(4), VIEW(4), IAR(4)
C
C ---------------------------------------------------------------------
C
C INTERNAL PARAMETERS OF VELVCT ARE AS FOLLOWS.  THE DEFAULT VALUES OF
C THESE PARAMETERS ARE DECLARED IN THE BLOCK DATA ROUTINE VELDAT.
C
C                        NAME   DEFAULT  FUNCTION
C                        ----   -------  --------
C
C                        BIG   R1MACH(2) CONSTANT USED TO INITIALIZE
C                                        POSSIBLE SEARCH FOR HI.
C
C                        EXT     0.25    THE LENGTHS OF THE SIDES OF THE
C                                        PLOT ARE PROPORTIONAL TO M AND
C                                        N WHEN NSET IS LESS THAN OR
C                                        EQUAL TO ZERO, EXCEPT WHEN
C                                        MIN(M,N)/MAX(M,N) IS LESS THAN
C                                        EXT, IN WHICH CASE A SQUARE
C                                        GRAPH IS PLOTTED.
C
C                        ICTRFG    1     FLAG TO CONTROL THE POSITION OF
C                                        THE ARROW RELATIVE TO  A BASE
C                                        POINT AT (MX,MY).
C
C                                        ZERO - CENTER AT (MX,MY)
C
C                                        POSITIVE - TAIL AT (MX,MY)
C
C                                        NEGATIVE -  HEAD AT (MX,MY)
C
C                        ILAB      0     FLAG TO CONTROL THE DRAWING OF
C                                        LINE LABELS.
C
C                                        ZERO - DO NOT DRAW THE LABELS
C
C                                        NON-ZERO - DRAW THE LABELS
C
C                        INCX      1     X-COORDINATE STEP SIZE FOR LESS
C                                        DENSE ARRAYS.
C
C                        INCY      1     Y-COORDINATE STEP SIZE.
C
C                        IOFFD     0     FLAG TO CONTROL NORMALIZATION
C                                        OF LABEL NUMBERS.
C
C                                        ZERO - INCLUDE A DECIMAL POINT
C                                        WHEN POSSIBLE
C
C                                        NON-ZERO - NORMALIZE ALL LABEL
C                                        NUMBERS BY ASH
C
C                        IOFFM     0     FLAG TO CONTROL PLOTTING OF
C                                        THE MESSAGE BELOW THE PLOT.
C
C                                        ZERO - PLOT THE MESSAGE
C
C                                        NON-ZERO - DO NOT PLOT IT
C
C                        RMN     160.    ARROW SIZE BELOW WHICH THE
C                                        HEAD NO LONGER SHRINKS, ON A
C                                        2**15 X 2**15 GRID.
C
C                        RMX    6400.    ARROW SIZE ABOVE WHICH THE
C                                        HEAD NO LONGER GROWS LARGER,
C                                        ON A 2**15 X 2**15 GRID.
C
C                        SIDE    0.90    LENGTH OF LONGER EDGE OF PLOT.
C                                        (SEE ALSO EXT.)
C
C                        SIZE    256.    WIDTH OF THE CHARACTERS IN
C                                        VECTOR LABELS, ON A 2**15 X
C                                        2**15 GRID.
C
C                        XLT     0.05    LEFT HAND EDGE OF THE PLOT.
C                                        (0 IS THE LEFT EDGE OF THE
C                                        FRAME, 1 THE RIGHT EDGE.)
C
C                        YBT     0.05    BOTTOM EDGE OF THE PLOT (0 IS
C                                        THE BOTTOM OF THE FRAME, 1 THE
C                                        TOP OF THE FRAME.)
C
C ---------------------------------------------------------------------
C
C INTERNAL FUNCTIONS WHICH MAY BE MODIFIED FOR DATA TRANSFORMATION -
C
C                        SCALE    COMPUTES A SCALE FACTOR USED IN THE
C                                 DETERMINATION OF THE LENGTH OF THE
C                                 VECTOR TO BE DRAWN.
C
C                        DIST     COMPUTES THE LENGTH OF A VECTOR.
C
C                        FX       RETURNS THE X INDEX AS THE
C                                 X-COORDINATE OF THE VECTOR BASE.
C
C                        MXF      RETURNS THE X-COORDINATE OF THE VECTOR
C                                 HEAD.
C
C                        FY       RETURNS THE Y INDEX AS THE
C                                 Y-COORDINATE OF THE VECTOR BASE.
C
C                        MYF      RETURNS THE Y-COORDINATE OF THE VECTOR
C                                 HEAD.
C
C                        VLAB     THE VALUE FOR THE VECTOR LABEL WHEN
C                                 ILAB IS NON-ZERO.
C
      SAVE
C     FX(XX,YY) = XX
C     FY(XX,YY) = YY
      DIST(XX,YY) = SQRT(XX*XX+YY*YY)
      MXF(XX,YY,UU,VV,SFXX,SFYY,MXX,MYY) = MXX+IFIX(SFXX*UU)
      MYF(XX,YY,UU,VV,SFXX,SFYY,MXX,MYY) = MYY+IFIX(SFYY*VV)
      SCALEX(MM,NN,INCXX,INCYY,HAA,XX1,XX2,YY1,YY2,XX3,XX4,YY3,YY4,
     1       LENN) = LENN/HAA
      SCALEY(MM,NN,INCXX,INCYY,HAA,XX1,XX2,YY1,YY2,XX3,XX4,YY3,YY4,
     1       LENN) = SCALEX(MM,NN,INCXX,INCYY,HAA,XX1,XX2,YY1,YY2,XX3,
     2                                                 XX4,YY3,YY4,LENN)
      VLAB(UU,VV,II,JJ) = DIST(UU,VV)
C
C ---------------------------------------------------------------------
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR.
C
      CALL Q8QST4 ('NSSL','VELVCT','VELVCT','VERSION  6')
C
C INITIALIZE AND TRANSFER SOME ARGUMENTS TO LOCAL VARIABLES.
C
      BIG = -R1MACH(2)
      MX = LU
      MY = LV
      NX = M
      NY = N
      GL = FLO
      HA = HI
      ISP = ISPV
      NC = 0
C
C COMPUTE CONSTANTS BASED ON THE ADDRESSABILITY OF THE PLOTTER.
C
      CALL GETUSV('XF',ISX)
      CALL GETUSV('YF',ISY)
      ISX = 2**(15-ISX)
      ISY = 2**(15-ISY)
      LEN = LENGTH*ISX
C
C SET UP THE SCALING OF THE PLOT.
C
      CALL GETSET (VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     +             WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
        X1 = VIEW(1)
        X2 = VIEW(2)
        Y1 = VIEW(3)
        Y2 = VIEW(4)
        X3 = WIND(1)
        X4 = WIND(2)
        Y3 = WIND(3)
        Y4 = WIND(4)
C
C
C
C
C
C
      IF (NSET) 101,102,106
C
  101 X3 = 1.
      X4 = FLOAT(NX)
      Y3 = 1.
      Y4 = FLOAT(NY)
      GO TO 105
C
  102 X1 = XLT
      X2 = XLT+SIDE
      Y1 = YBT
      Y2 = YBT+SIDE
      X3 = 1.
      Y3 = 1.
      X4 = FLOAT(NX)
      Y4 = FLOAT(NY)
      IF (AMIN1(X4,Y4)/AMAX1(X4,Y4) .LT. EXT) GO TO 105
C
      IF (NX-NY) 103,105,104
  103 X2 = XLT+SIDE*X4/Y4
      GO TO 105
  104 Y2 = YBT+SIDE*Y4/X4
C
  105 CALL SET(X1,X2,Y1,Y2,X3,X4,Y3,Y4,1)
      IF (NSET .EQ. 0) CALL PERIM (1,0,1,0)
C
C CALCULATE A LENGTH IF NONE PROVIDED.
C
  106 IF (LEN .NE. 0) GO TO 107
      CALL FL2INT(FX(1.,1.),FY(1.,1.),MX,MY)
      CALL FL2INT(FX(FLOAT(1+INCX),FLOAT(1+INCY)),
     +            FY(FLOAT(1+INCX),FLOAT(1+INCY)),LX,LY)
      LEN = SQRT((FLOAT(MX-LX)**2+FLOAT(MY-LY)**2)/2.)
C
C SET UP SPECIAL VALUES.
C
  107 IF (ISP .EQ. 0) GO TO 108
      SPV1 = SPV(1)
      SPV2 = SPV(2)
      IF (ISP .EQ. 4) SPV2 = SPV(1)
C
C FIND THE MAXIMUM VECTOR LENGTH.
C
  108 IF (HA .GT. 0.) GO TO 118
C
      HA = BIG
      IF (ISP .EQ. 0) GO TO 115
C
      DO 114 J=1,NY,INCY
         DO 113 I=1,NX,INCX
            IF (ISP-2) 109,111,110
  109       IF (U(I,J) .EQ. SPV1) GO TO 113
            GO TO 112
  110       IF (U(I,J) .EQ. SPV1) GO TO 113
  111       IF (V(I,J) .EQ. SPV2) GO TO 113
  112       HA = AMAX1(HA,DIST(U(I,J),V(I,J)))
  113    CONTINUE
  114 CONTINUE
      GO TO 126
C
  115 DO 117 J=1,NY,INCY
         DO 116 I=1,NX,INCX
            HA = AMAX1(HA,DIST(U(I,J),V(I,J)))
  116    CONTINUE
  117 CONTINUE
C
C BRANCH IF NULL VECTOR SIZE.
C
  126 IF (HA .LE. 0.) GO TO 125
C
C COMPUTE SCALE FACTORS.
C
  118 SFX = SCALEX(M,N,INCX,INCY,HA,X1,X2,Y1,Y2,X3,X4,Y3,Y4,LEN)
      SFY = SCALEY(M,N,INCX,INCY,HA,X1,X2,Y1,Y2,X3,X4,Y3,Y4,LEN)
      IOFFDT = IOFFD
      IF (GL.NE.0.0 .AND. (ABS(GL).LT.0.1 .OR. ABS(GL).GE.1.E5))
     1    IOFFDT = 1
      IF (HA.NE.0.0 .AND. (ABS(HA).LT.0.1 .OR. ABS(HA).GE.1.E5))
     1    IOFFDT = 1
      ASH = 1.0
      IF (IOFFDT .NE. 0)
     1    ASH = 10.**(3-IFIX(ALOG10(AMAX1(ABS(GL),ABS(HA)))-500.)-500)
      IZFLG = 0
C
C COMPUTE ZMN AND ZMX, WHICH ARE USED IN DRWVEC.
C
      ZMN = LEN*(GL/HA)
      ZMX = FLOAT(LEN)+.01
C
C DRAW THE VECTORS.
C
      DO 123 J=1,NY,INCY
         DO 122 I=1,NX,INCX
            UI = U(I,J)
            VI = V(I,J)
            IF (ISP-1) 121,119,120
  119       IF (UI-SPV1) 121,122,121
  120       IF (VI .EQ. SPV2) GO TO 122
            IF (ISP .GE. 3) GO TO 119
  121       X = I
            Y = J
            CALL FL2INT(FX(X,Y),FY(X,Y),MX,MY)
            LX = MAX0(1,MXF(X,Y,UI,VI,SFX,SFY,MX,MY))
            LY = MAX0(1,MYF(X,Y,UI,VI,SFX,SFY,MX,MY))
            IZFLG = 1
            IF (ILAB .NE. 0) CALL ENCD(VLAB(UI,VI,I,J),ASH,LABEL,NC,
     +                                                           IOFFDT)
            CALL DRWVEC (MX,MY,LX,LY,LABEL,NC)
  122    CONTINUE
  123 CONTINUE
C
      IF (IZFLG .EQ. 0) GO TO 125
C
      IF (IOFFM .NE. 0) GO TO 200
      WRITE(LABEL,'(E10.3)')HA
C
C     TURN OFF CLIPPING SO ARROW CAN BE DRAWN
C
      CALL GQCLIP(IER,ICLP,IAR)
      CALL GSCLIP(0)
      CALL DRWVEC (28768,608,28768+LEN,608,LABEL,10)
      IX = 1+(28768+LEN/2)/ISX
      IY = 1+(608-(5*ISX*MAX0(256/ISX,8))/4)/ISY
C
C
      XC = CPUX(IX)
      YC = CPUY(IY)
      CALL WTSTR (XC,YC,'MAXIMUM VECTOR',MAX0(256/ISX,8),0,0)
C
C
C
C     RESTORE CLIPPING
C
      CALL GSCLIP(ICLP)
C
C DONE.
C
      GOTO 200
C
C ZERO-FIELD ACTION.
C
  125 IX = 1+16384/ISX
        IY = 1+16384/ISY
C
C
        XC = CPUX(IX)
        YC = CPUY(IY)
      CALL WTSTR (XC,YC,
     +                             'ZERO FIELD',MAX0(960/ISX,8),0,0)
C
C
C RESTORE TRANS 1 AND LOG SCALING AND ORIGINAL TRANS NUMBER
C
  200 CONTINUE
      IF (NSET .LE. 0)
     +  CALL SET(VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     +           WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
C
C
      RETURN
      END
