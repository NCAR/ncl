
      PROGRAM PGKEX16
C
C  Illustrate the fill algorithm determining what is inside
C  a polygon.
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

      PARAMETER (ID=11, IDM1=ID-1)
      DIMENSION XS(ID),YS(ID),XD(ID),YD(ID),XP(IDM1),YP(IDM1)
C
C  Coordinate data defining 10 points for a five-pointed star.
C  The ten points specify the tips of the star as well as the
C  points toward the center between the tips.  The first point
C  is set equal to the last point for the purposes of drawing
C  the outline of the points.
C
      DATA XS/ 0.00000, -0.22451, -0.95105, -0.36327, -0.58780,
     -         0.00000,  0.58776,  0.36327,  0.95107,  0.22453,
     -         0.00000                                         /
      DATA YS/ 1.00000,  0.30902,  0.30903, -0.11803, -0.80900,
     -        -0.38197, -0.80903, -0.11805,  0.30898,  0.30901,
     -         1.00000                                         /
C
C  Coordinates for labelling the stars.
C
      DATA XP/ .243,.180,.025,.138,.098,.243,.385,.345,.457,.320 /
      DATA YP/ .690,.540,.513,.415,.285,.340,.285,.415,.513,.540 /
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF,IDUM)
      CALL GOPWK (IWKID,LUNIT,IWTYPE)
      CALL GACWK (IWKID)
C
C  Define colors.
C
      CALL GSCR(IWKID, 0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID, 1, 0.0, 0.0, 1.0)
      CALL GSCR(IWKID, 2, 0.4, 0.0, 0.4)
      CALL GSCR(IWKID, 3, 1.0, 0.0, 0.0)
C
C  Draw the star with interior style solid;
C  use ten points in the fill area call.
C
      X = .25
      Y = .45
      SCL = .2
      DO 10 I=1,ID
        XD(I) = X+SCL*XS(I)
        YD(I) = Y+SCL*YS(I)
   10 CONTINUE
C
      CALL GSFAIS(1)
      CALL GSFACI(1)
      CALL GFA(10,XD,YD)
C
C  Label the points.
C
      DO 30 I=1,10
        CALL PLTNUM (XP(I),YP(I),I)
   30 CONTINUE
C
C  Draw lines connecting the coordinate points.
C
      CALL GSPLCI(3)
      CALL GSLWSC(4.)
      CALL GPL(ID,XD,YD)
C
C  Draw the star with interior style solid;
C  use only the five tips of the star as coordinates.
C
      X = .75
      Y = .45
      SCL = .2
      XD(1) = X+SCL*XS(1)
      YD(1) = Y+SCL*YS(1)
      XD(2) = X+SCL*XS(5)
      YD(2) = Y+SCL*YS(5)
      XD(3) = X+SCL*XS(9)
      YD(3) = Y+SCL*YS(9)
      XD(4) = X+SCL*XS(3)
      YD(4) = Y+SCL*YS(3)
      XD(5) = X+SCL*XS(7)
      YD(5) = Y+SCL*YS(7)
      CALL GFA(5,XD,YD)
C
C  Draw lines connecting the coordinate points.
C
      XD(6) = XD(1)
      YD(6) = YD(1)
      CALL GPL(6,XD,YD)
C
C  Label the points.
C
      CALL PLTNUM (XP(1)+.5,YP(1),1)
      CALL PLTNUM (XP(3)+.5,YP(3),4)
      CALL PLTNUM (XP(5)+.5,YP(5),2)
      CALL PLTNUM (XP(7)+.5,YP(7),5)
      CALL PLTNUM (XP(9)+.5,YP(9),3)
C
C  Label the plot using Plotchar.
C
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(.5,.91,'Filled areas',.035,0.,0.)
      CALL PLCHHQ(.5,.84,'What''s inside, what''s outside?',.035,0.,0.)
C
C  Close picture, deactivate and close the workstation, close GKS.
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
      SUBROUTINE PLTNUM(X,Y,NUM)
C
C  Plot the value of the integer NUM at coordinate location (X,Y)
C
      CHARACTER*2 LABEL
C
      WRITE(LABEL,100) NUM
  100 FORMAT(I2)
C
      CALL PCSETI('FN',22)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(X,Y,LABEL,.023,0.,0.)
C
      RETURN
      END
