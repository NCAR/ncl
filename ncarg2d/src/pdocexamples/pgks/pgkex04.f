
      PROGRAM PGKEX04
C
C  Illustrate normalization transformations.
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

      PARAMETER (ILD=121)
      DIMENSION PLX(ILD),PLY(ILD)
      DATA  RADC/.0174532/
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define color indices, color index 0 defines the background color.
C
      CALL GSCR(IWKID, 0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID, 1, 0.0, 0.0, 1.0)
      CALL GSCR(IWKID, 2, 1.0, 0.0, 0.2)
      CALL GSCR(IWKID, 3, 0.4, 0.0, 0.4)
C
C  Create the data for a spiral in the world coordinate square
C  bounded by (-10.,-10.) and (10.,10.) .
C
      J = 0
      DO 10 I=0,720,6
        SCALE = REAL(I)/75.
        J = J+1
        PLX(J) = SCALE*COS(REAL(I-1)*RADC)
        PLY(J) = SCALE*SIN(REAL(I-1)*RADC)
   10 CONTINUE
C
C  Define a normalization transformation that does not preserve
C  aspect ratio.  Draw the transformed spiral with a box bounding
C  the viewport.
C
      XL = -10.
      XR =  10.
      YB = -10.
      YT =  10.
      CALL GSVP(1,.55,.95,.4,.65)
      CALL GSWN(1,XL,XR,YB,YT)
      CALL GSELNT(1)
      CALL GSPLCI(1)
      CALL GPL(121,PLX,PLY)
      CALL BOX(XL,XR,YB,YT)
C
C  Draw an image representing the window to the left of the viewport.
C
      CALL GSVP(1,.05,.45,.3,.70)
      CALL GSWN(1,XL,XR,YB,YT)
      CALL GSELNT(1)
      CALL GSPLCI(1)
      CALL GPL(121,PLX,PLY)
      CALL BOX(XL,XR,YB,YT)
C
C  Draw dashed lines between the outlines.
C
      CALL DLINE(.05,.30,.55,.40)
      CALL DLINE(.45,.30,.95,.40)
      CALL DLINE(.05,.70,.55,.65)
      CALL DLINE(.45,.70,.95,.65)
C
C  Label the plot using Plotchar.
C
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',3)
      CALL PLCHHQ(.5,.83,'Normalization transformation',.035,0.,0.)
C
      CALL PCSETI('FN',21)
      CALL PCSETI('CC',3)
      CALL PLCHHQ(.07,.650,'Window',.022,0.,-1.)
      CALL PLCHHQ(.57,.625,'Viewport',.022,0.,-1.)
C
      CALL PCSETI('FN',9)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(.5,.22,'Normalization transformation defined by',
     +            .0275,0.,0.)
C
      CALL PCSETI('FN',29)
      CALL PCSETI('CC',3)
      CALL PLCHHQ(.50,.15,'CALL GSWN(1,-10., 10.,-10., 10.)',
     -            .015,0.,0.)
      CALL PLCHHQ(.50,.10,'CALL GSVP(1, .55, .95, .40, .65)',
     -            .015,0.,0.)
C
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
      SUBROUTINE BOX(XL,XR,YB,YT)
C
C  Draw a box with corner points (XL,YB) and (XR,YT).
C
      DIMENSION XA(5),YA(5)
C
      XA(1) = XL
      YA(1) = YB
      XA(2) = XR
      YA(2) = YB
      XA(3) = XR
      YA(3) = YT
      XA(4) = XL
      YA(4) = YT
      XA(5) = XA(1)
      YA(5) = YA(1)
C
      CALL GSPLCI(1)
      CALL GPL(5,XA,YA)
C
      RETURN
      END
      SUBROUTINE DLINE (X1,Y1,X2,Y2)
      DIMENSION XA(2),YA(2)
C
C  Draw a dashed line with color index 2 between the coordinates
C  (X1,Y1) and (X2,Y2) .
C
      CALL GSELNT(0)
      CALL GSPLCI(2)
      CALL GSLN(2)
      XA(1) = X1
      YA(1) = Y1
      XA(2) = X2
      YA(2) = Y2
      CALL GPL(2,XA,YA)
      CALL GSLN(1)
C
      RETURN
      END
