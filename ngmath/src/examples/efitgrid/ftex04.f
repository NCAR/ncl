
      PROGRAM FTEX04
C
C  Example of KURV1, KURV2, KURVD.
C
      PARAMETER (IDIM=11,IOUT=201,IDTEMP=9*IDIM)
      DIMENSION X(IDIM),Y(IDIM),TEMP(IDTEMP),U(IOUT),XO(IOUT),YO(IOUT),
     +          XS(IOUT),YS(IOUT),XD(IOUT),YD(IOUT),XDD(IOUT),YDD(IOUT)       
      DIMENSION XP(IDIM),YP(IDIM),S(IDIM)
C
      DATA X/ 3.,  4.,  9., 16., 21., 27., 34., 36., 34., 26., 18./
      DATA Y/ 2.4,  9.6, 14.4, 12.0,  9.6,  8.4, 
     +       13.2, 21.6, 30.0, 37.2, 38.4/
C
C  Do KURV1 set up.
C
      SIGMA = 1.
      ISF   = 3
      CALL KURV1(IDIM,X,Y,SLOP1,SLOP2,ISF,XP,YP,TEMP,S,SIGMA,IERR)
      IF (IERR .NE. 0) THEN
        PRINT *, 'Error return from KURV1 =',IERR
        STOP
      ENDIF
C
C  Get interpolated points using KURV2.
C
      TINC = 1.0/(IOUT-1)
      DO 10 I=1,IOUT
        U(I) = (I-1)*TINC
        CALL KURV2(U(I),XO(I),YO(I),IDIM,X,Y,XP,YP,S,SIGMA)
   10 CONTINUE
C
C  Get the derivatives.
C
      DO 20 I=1,IOUT
        CALL KURVD(U(I),XS(I),YS(I),XD(I),YD(I),XDD(I),YDD(I),
     +             IDIM,X,Y,XP,YP,S,SIGMA)
   20 CONTINUE
C
C  Draw plot.
C
      CALL DRWFT4(IDIM,X,Y,IOUT,XO,YO,U,XD,YD)
C
      STOP
      END
      SUBROUTINE DRWFT4(II,X,Y,IOUT,XO,YO,U,XD,YD)
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define a color table.
C
      CALL GSCR(IWKID, 0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID, 1, 0.0, 0.0, 0.0)
      CALL GSCR(IWKID, 2, 1.0, 0.0, 0.0)
      CALL GSCR(IWKID, 3, 0.0, 1.0, 0.0)
      CALL GSCR(IWKID, 4, 0.0, 0.0, 1.0)
C
C  Draw markers at original points.
C
      CALL BKGFT4(0.,40.,0.,40.,0.15,0.85,'Demo for KURV1/KURV2',0.035,
     +            0.5,0.93,0)
      CALL GRIDAL(4,5,4,5,1,1,10,0.,0.)
      CALL GSMKSC(2.)
      CALL GSPMCI(4)
      CALL GPM(II,X,Y)
C
C  Draw the interpolated curve
C
      CALL CURVE(XO,YO,IOUT)
      CALL FRAME
C
C  Plot the first derivatives of X and Y with respect to the parametric
C  variable U.
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PCSETI('FN',21)
      CALL PLCHHQ(0.5,0.95,'Derivatives from KURVD',0.035,0.,0.)
      CALL BKGFT4(0.,1.,-80.,80.,0.55,0.87,'dx/du',0.030,0.65,0.82,1)
      CALL GRIDAL(5,5,4,5,1,1,10,0.,-80.)
      CALL CURVE(U,XD,IOUT)
      CALL BKGFT4(0.,1.,-40.,80.,0.10,0.42,'dy/du',0.030,0.39,0.37,1)
      CALL GRIDAL(5,5,3,5,1,1,10,0.,-40.)
      CALL CURVE(U,YD,IOUT)
      CALL FRAME
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      RETURN
      END
      SUBROUTINE BKGFT4(XL,XR,YB,YT,YPB,YPT,LABEL,SIZL,POSXL,POSYL,IZL)
      DIMENSION XX(2),YY(2)
      CHARACTER*(*) LABEL
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PCSETI('FN',21)
      CALL PLCHHQ(POSXL,POSYL,LABEL,SIZL,0.,0.)
      CALL SET(0.17,0.87,YPB,YPT,XL,XR,YB,YT,1)
      IF (IZL .NE. 0) THEN
        XX(1) = XL
        XX(2) = XR
        YY(1) = 0.
        YY(2) = 0.
        CALL GSPLCI(2)
        CALL GPL(2,XX,YY)
        CALL GSPLCI(1)
      ENDIF
C 
      CALL GASETI('LTY',1)
      CALL PCSETI('FN',21)
      CALL GASETR('XLS',0.02)
      CALL GASETC('XLF','(F4.1)')
      CALL GASETR('YLS',0.02)
      CALL GASETC('YLF','(F5.1)')
      CALL GASETR('XMJ',0.02)
      CALL GASETR('YMJ',0.02)
C
      RETURN
      END
