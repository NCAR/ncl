C
C       $Id: ftex06.f,v 1.1 1998-02-06 19:40:56 fred Exp $
C
      PROGRAM FTEX06
C
C  Example of SURF1/SURF2.
C
      PARAMETER (NXI=11,NYI=17,NXO=31,NYO=21,IDTEMP=2*NYI+NXI)
C
      DIMENSION X(NXI),Y(NYI),Z(NXI,NYI)
      DIMENSION ZX1(NYI),ZXM(NYI),ZY1(NXI),ZYN(NXI)
      DIMENSION ZP(NXI,NYI,3),TEMP(IDTEMP)
      DIMENSION XO(NXO),YO(NYO),ZO(NXO,NYO)
C
C Declare a function ZF(U,V) that defines a surface.
C
      ZF(U,V)=.5+.25*SIN(-7.*U)+.25*COS(5.*V)
C
C Define the surface to be drawn.
C
      DO 104 I=1,NXI
         X(I) = REAL(I-1)/REAL(NXI-1)
         DO 103 J=1,NYI
            Y(J) = REAL(J-1)/REAL(NYI-1)
            Z(I,J)=ZF(X(I),Y(J))
 103     CONTINUE
 104  CONTINUE
C
C  Do SURF1 set up.
C
      SIGMA = 1.
      ISF   = 255
      CALL SURF1(NXI,NYI,X,Y,Z,NXI,ZX1,ZXM,ZY1,ZYN,
     +           ZXY11,ZXYM1,ZXY1N,ZXYMN,ISF,ZP,TEMP,SIGMA,IERR)
      IF (IERR .NE. 0) THEN
        PRINT *, 'Error return from SURF =',IERR
        STOP
      ENDIF
C
C  Get interpolated points using SURF2.
C
      TINCX = 1.0/(NXO-1)
      TINCY = 1.0/(NYO-1)
      DO 20 I=1,NXO
        XO(I) = (I-1)*TINCX
        DO 10 J=1,NYO
          YO(J) = (J-1)*TINCY
          ZO(I,J) = SURF2(XO(I),YO(J),NXI,NYI,X,Y,Z,NXI,ZP,SIGMA)
   10   CONTINUE
   20 CONTINUE
C
C  Draw plot.
C
      CALL DRWFT6(NXO,NYO,XO,YO,ZO)
C
      STOP
      END
      SUBROUTINE DRWFT6(NXO,NYO,XO,YO,ZO)
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL DRWTD3(NXO,NYO,XO,YO,ZO,3.85,2.75,1.65,6)
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      RETURN 
      END
      SUBROUTINE DRWTD3(NX,NY,X,Y,Z,S1,S2,S3,IST)
C
C  Procedure DRWTD3 uses the NCAR Graphics functions in Tdpack
C  to draw a surface plot of the data values in Z.
C
C  The point of observation is calculated from the 3D coordinate
C  (S1, S2, S3); the point looked at is the center of the surface.
C
C   NX     -  Dimension of the X-axis variable X.
C   NY     -  Dimension of the Y-axis variable Y.
C   X      -  An array of X-axis values.
C   Y      -  An array of Y-axis values.
C   Z      -  An array dimensioned for NX x NY containing data
C             values for each (X,Y) coordinate.
C   S1     -  X value for the eye position.
C   S2     -  Y value for the eye position.
C   S3     -  Z value for the eye position.
C   IST    -  A style index defining the colors used to shade the
C             surface as per:
C
C                    1  -  wire frame
C                    2  -  gray shades underneath; gray shades on top.
C                    3  -  gray shades underneath; red shades on top.
C                    4  -  gray shades underneath; green shades on top.
C                    5  -  gray shades underneath; blue shades on top.
C                    6  -  gray shades underneath; cyan shades on top.
C                    7  -  gray shades underneath; magenta shades on top.
C
C             If IST is positive, then a white backgound is used;
C             if IST is the negative of any of the above values, then
C             a black background is used.
C            
      PARAMETER (IERRF=6, LUNIT=2, IWKID=1, ITYPE=1)
      PARAMETER (MTRI=40000)
      DIMENSION X(NX),Y(NY),Z(NX,NY)
      DIMENSION RTRI(10,MTRI),RTWK(MTRI,2),ITWK(MTRI)
      DATA ANG1,ANG2,RMUL / -35.,25.,2.9 /
      COMMON /DSTDDT/ RTRI,RTWK,ITWK
C
C  Set the desired values of the shading parameters.  Values of SHDE
C  near 0 give brighter colors and values near 1 give pastel shades.
C  Values of SHDR near 0 give a narrow range of shades and values near
C  1 give a wide range of shades.
C
      DATA SHDE,SHDR / .1 , 0.8 /
      DATA DTOR / .017453292519943 /
C
      CALL GQOPS(ISTATE)
      IF (ISTATE .EQ. 0) THEN
        CALL GOPKS (6, 0)
        IF (ITYPE .EQ. 1) THEN
          CALL NGSETC('ME','srf.ncgm')
        ELSE IF ( (ITYPE .GE. 20) .AND. (ITYPE .LE. 31) ) THEN
          CALL NGSETC('ME','srf.ps')
        ENDIF
        CALL GOPWK (IWKID, LUNIT, ITYPE)
        CALL GACWK (IWKID)
      ENDIF
C
      IF (IST .LT. 0) THEN
        CALL GSCR(IWKID,0,0.,0.,0.)
        CALL GSCR(IWKID,1,1.,1.,1.)
      ELSE
        CALL GSCR(IWKID,0,1.,1.,1.)
        CALL GSCR(IWKID,1,0.,0.,0.)
      ENDIF
C
C  Find mins and maxs.
C
      XMIN = X(1)
      XMAX = X(1)
      DO 120 I=2,NX
        XMIN = MIN(XMIN,X(I))
        XMAX = MAX(XMAX,X(I))
  120 CONTINUE
      YMIN = Y(1)
      YMAX = Y(1)
      DO 125 I=2,NY
        YMIN = MIN(YMIN,Y(I))
        YMAX = MAX(YMAX,Y(I))
  125 CONTINUE
      ZMIN = Z(1,1)
      ZMAX = Z(1,1)
      DO 130 I=1,NX
        DO 140 J=1,NY
          ZMIN = MIN(ZMIN,Z(I,J))
          ZMAX = MAX(ZMAX,Z(I,J))
  140   CONTINUE
  130 CONTINUE
      XRNG = XMAX-XMIN
      YRNG = YMAX-YMIN
      ZRNG = ZMAX-ZMIN
      XMID = 0.5*(XMIN+XMAX)
      YMID = 0.5*(YMIN+YMAX)
      ZMID = 0.5*(ZMIN+ZMAX)
C
      CALL GSCR (IWKID,2,1.,0.,0.)
      CALL GSCR (IWKID,3,0.,1.,0.)
      CALL GSCR (IWKID,4,0.,0.,1.)
      CALL GSCR (IWKID,5,0.,1.,1.)
      CALL GSCR (IWKID,6,1.,0.,1.)
      CALL GSCR (IWKID,7,1.,1.,0.)
      CALL GSCR (IWKID,8,0.7,0.7,0.7)
C
      DO 101 ICOL=11,42
        P=1.-     REAL(ICOL-11)/31.
        Q=1.-SHDR*REAL(ICOL-11)/31.
        CALL GSCR (1,ICOL    ,     P,     P,     P)  !  gray scale
        CALL GSCR (1,ICOL+ 32,     Q,     Q,     Q)  !  white
        CALL GSCR (1,ICOL+ 64,     Q,SHDE*Q,SHDE*Q)  !  red
        CALL GSCR (1,ICOL+ 96,SHDE*Q,     Q,SHDE*Q)  !  green
        CALL GSCR (1,ICOL+128,SHDE*Q,SHDE*Q,     Q)  !  blue
        CALL GSCR (1,ICOL+160,SHDE*Q,     Q,     Q)  !  cyan
        CALL GSCR (1,ICOL+192,     Q,SHDE*Q,     Q)  !  magenta
  101 CONTINUE
C
C Define TDPACK rendering styles 0 through 7.  The indices 0-7 can 
C then be used as final arguments in calls to TDITRI, TDSTRI, and TDMTRI.
C
      XSL = 0.05*XRNG
      YSL = 0.05*YRNG
      ZSL = 0.00*ZRNG
      CALL TDSTRS (1,-1, 0, -1,  0, -1, 1, 0, XSL, YSL, ZSL)
      CALL TDSTRS (2,43,74, 43, 74, 1, 1, 0, XSL, YSL, ZSL)
      CALL TDSTRS (3,43,74, 75,106, 1, 1, 0, XSL, YSL, ZSL)
      CALL TDSTRS (4,43,74,107,138, 1, 1, 0, XSL, YSL, ZSL)
      CALL TDSTRS (5,43,74,139,170, 1, 1, 0, XSL, YSL, ZSL)
      CALL TDSTRS (6,43,74,171,202, 1, 1, 0, XSL, YSL, ZSL)
      CALL TDSTRS (7,43,74,203,234, 1, 1, 0, XSL, YSL, ZSL)
C
C Create the triangle list representing a surface.
C
      NTRI=0
      CALL TDSTRI (X,NX,Y,NY,Z,NX,RTRI,MTRI,NTRI,IST)
      IF (NTRI .EQ. MTRI) THEN
        PRINT * , 'Triangle list overflow in TDITRI'
        STOP
      END IF
C
C  Determine a default eye position if none is specified.
C
      IF (S1.EQ.0. .AND. S2.EQ.0. .AND. S3.EQ.0.) THEN
        R = RMUL*SQRT(XRNG*XRNG + YRNG*YRNG + ZRNG*ZRNG)
        XEYE = XMID+R*COS(DTOR*ANG1)*COS(DTOR*ANG2)
        YEYE = YMID+R*SIN(DTOR*ANG1)*COS(DTOR*ANG2)
        ZEYE = ZMID+R*SIN(DTOR*ANG2)
      ELSE
        XEYE = S1
        YEYE = S2
        ZEYE = S3
      ENDIF
C
C Initialize TDPACK.
C
      CALL TDINIT (XEYE, YEYE, ZEYE, XMID, YMID, ZMID,
     +                   XMID, YMID, ZMID+0.1*ZRNG, 0)
C
C Order the triangles.
C
      CALL TDOTRI (RTRI,MTRI,NTRI,RTWK,ITWK,1)
      IF (NTRI .EQ. MTRI) THEN
        PRINT * , 'TRIANGLE LIST OVERFLOW IN TDOTRI'
        STOP
      END IF
C
C  Draw the triangles.
C
      CALL TDDTRI (RTRI,MTRI,NTRI,ITWK)
      CALL FRAME
C
      IF (ISTATE .EQ. 0) THEN
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
      ENDIF
C
      RETURN
      END
