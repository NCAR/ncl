C
C $Id: bccurv.f,v 1.6 2008-07-27 00:17:29 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE BCCURV(BXI,BYI,NO,XO,YO,NPTS)
C
C  PURPOSE
C    This subroutine calculates points along a Bezier curve whose
C    four control points are given in BXI and BYI.  In the default
C    case a recursive subdivision algorithm is implemented that
C    calculates the points along the curve to within a predefined
C    tolerance limit.
C
C  USAGE
C                Argument    Type     Mode    Dimension 
C                --------    ----     ----    ---------
C    CALL BCCURV(BXI,        Real     Input   4
C                BYI,        Real     Input   4
C                NO,         Integer  Input   
C                XO,         Real     Output  NO
C                YO,         Real     Output  NO
C                NPTS)       Integer  Output
C
C
C         BXI   --  The X coordinates for four Bezier control points.
C         BYI   --  The Y coordinates for four Bezier control points.
C         NO    --  The deminsion of the output arrays XO and YO.
C         XO    --  The interpolated X coordinates for the points along 
C                   the Bezier curve that is defined by the input control 
C                   points.
C         YO    --  The interpolated Y coordinates for the points along 
C                   the Bezier curve that is defined by the input control 
C                   points.
C         NPTS  --  The number of points returned in XO and YO.  NPTS
C                   must be less than NO or an error results.
C
C-------------------------------------------------------------------------------
C
C  UNDEF  --  A number to flag that fields have not been defined.
C  NLEV   --  The maximum number of iterations for curve subdivision,
C             i.e. the maximum number of times a curve will be subdivided
C             before calling the points in the subdivided curves the final
C             ones to represent the original curve.
C  MXCRV  --  Maximum number of curves resulting from the subdivision
C             process.
C
      include 'bccom.h'
C
      PARAMETER (UNDEF=2.**100, NLEV=8, MXCRV=2**(NLEV-1))
C
      DIMENSION BXI(4),BYI(4),XO(NO),YO(NO)
C
C  Dimension the arrays used to store the curves resulting from 
C  subdivision.  At each level of subdivision, the subdivided curves
C  computed at that level are stored in these arrays.
C
      DIMENSION BZPTSX(4,MXCRV),BZPTSY(4,MXCRV),BZX(4),BZY(4)
      DIMENSION TWIN(4),TVPT(4)
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL BCBKD
C
C  Set the flatness tolerance value EPS in user coordinates.
C
      CALL GQCNTN(IER,NTR)
      IF (IER .EQ. 8) THEN
        PRINT * , 'BCCURV -- GKS must be open before calling functions i
     +n the Bezier curve package.'
        STOP
      ENDIF
      CALL GQNT(NTR,IER,TWIN,TVPT)
      EPS = FRATIO*(TWIN(4)-TWIN(3))/(TVPT(4)-TVPT(3))
C
C  Calculate the required number of points along the curve by using
C  the actual cubic parametric equations for the Bezier curve.  This
C  is done only if the subdivision algorithm is being overridden.
C
      IF (NPPC .GT. 1) THEN
        IF (NO .LT. NPPC) THEN
          WRITE(6,510) NPPC,NO
  510     FORMAT(' BCCURV -- Number of points specified for interpolatio
     -n along the curve = ',I5,',',/'           exceeds the array dimens
     -ion reserved for them =',I5,'.'/)
        ENDIF
        CALL BCFCRV(BXI,BYI,NPPC,XO,YO)
        NPTS = NPPC
        GO TO 110
      ENDIF
C
C  Initialize the Bezier point arrays.
C
      DO 20 J=1,MXCRV
        DO 30 K=1,4
          BZPTSX(K,J) = UNDEF
          BZPTSY(K,J) = UNDEF
   30   CONTINUE
   20 CONTINUE
C
C  Specify the input Bezier points as the initial curve in the array
C  of subdivided curves.
C       
      DO 50 I=1,4
        BZPTSX(I,1) = BXI(I)
        BZPTSY(I,1) = BYI(I)
   50 CONTINUE
C
C  Continue curve subdivision until the flatness criterion is
C  satisfied or we are at the final level. 
C
      DO 60 LEVEL=1,NLEV-1
C
C  Number of curves possible at this level.
C
        NUMCRV = 2**(LEVEL-1)
C
C  Calculate the increment between the points that will be stored in 
C  the arrays if a curve satisfies the flatness criterion at this level.
C
        INC = MXCRV/(2**LEVEL)
C
C  Loop through the curves at this level and test for flatness;
C  NFLAT is a flag to indicate if all curves are flat at this level.
C
        NFLAT = 1
        DO 70 J=1,NUMCRV
          NDXO = 2*INC*(J-1)+1
          NDXL = NDXO
          NDXR = NDXL+INC
          IF (BZPTSX(1,NDXO) .NE. UNDEF) THEN
            DIST = BCFTOL(BZPTSX(1,NDXO),BZPTSY(1,NDXO))
            IF (DIST .GT. EPS) THEN
C
C  Curve is not flat, subdivide it.
C
              NFLAT = 0
              DO 100 K=1,4
                BZX(K) = BZPTSX(K,NDXO)
                BZY(K) = BZPTSY(K,NDXO)
  100         CONTINUE
              CALL BCSUBD(BZX,BZY,BZPTSX(1,NDXL),BZPTSY(1,NDXL),
     -                            BZPTSX(1,NDXR),BZPTSY(1,NDXR))
            ENDIF
          ENDIF
   70   CONTINUE
        IF (NFLAT .EQ. 1) GO TO 80
   60 CONTINUE
   80 CONTINUE
C
C  Collect the defined points into the output array.
C
      NPTS  = 1
      XO(1) = BZPTSX(1,1)
      YO(1) = BZPTSY(1,1)
      DO 90 I=1,MXCRV
        IF (BZPTSX(4,I) .NE. UNDEF) THEN
          NPTS = NPTS+1
          XO(NPTS) = BZPTSX(4,I)
          YO(NPTS) = BZPTSY(4,I)
        ENDIF
   90 CONTINUE
C
      IF (NO .LT. NPTS) THEN
        WRITE(6,510) NPTS,NO
        STOP
      ENDIF
C
  110 CONTINUE
      RETURN
C
      END
