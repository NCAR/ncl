
      PROGRAM PGKEX15
C
C  Illustrate filled area by replicating a basic filled area over a
C  large area to produce a pattern with perceptual multistability.
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
      CALL GOPKS (IERRF,IDUM)
      CALL GOPWK (IWKID,LUNIT,IWTYPE)
      CALL GACWK (IWKID)
C
C  Define colors.
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,1.)
      CALL GSCR(IWKID,2,.4,.0,.4)
C
C  Replicate the small filled area over the entire plot.
C
      DO 20 J=1,8,2
        DO 10 I=1,10,2
          CALL DRWSO(.05+(I-1)*.1,.14+(J-1)*.1,.1,0.)
          CALL DRWSO(.15+(I-1)*.1,.14+(J-1)*.1,.1,180.)
          CALL DRWSO(.05+(I-1)*.1,.24+(J-1)*.1,.1,180.)
          CALL DRWSO(.15+(I-1)*.1,.24+(J-1)*.1,.1,0.)
   10   CONTINUE
   20 CONTINUE
C
C  Label the plot using Plotchar.
C
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(.5,.94,'Filled areas',.035,0.,0.)
C
C  Terminate the picture, deactivate and close the workstation,
C  close GKS.
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
      SUBROUTINE DRWSO(X,Y,SCALE,ANGD)
C
C  Draw the fill area at coordinate (X,Y) at angle ANGD (in degrees)
C  and scaled by SCALE.  Using a higher level of GKS one could use
C  segment transformations to do the rotation, translation, and
C  scaling, but it is done directly here.
C
      PARAMETER (ID=16, H=1.)
      DIMENSION XA(ID),YA(ID),XB(ID),YB(ID)
C
C  Coordinates for the basic fill pattern.
C
      DATA XA /  0.000000,  0.130602,  0.500000,  0.315301,
     -           0.500000,  0.315301,  0.130602,  0.130602,
     -           0.000000, -0.130602, -0.130602, -0.315301,
     -          -0.500000, -0.315301, -0.500000, -0.130602 /
      DATA YA / -0.500000, -0.369398, -0.369398, -0.184699,
     -           0.000000,  0.184699,  0.000000,  0.369398,
     -           0.500000,  0.369398,  0.000000,  0.184699,
     -           0.000000, -0.184699, -0.369398, -0.369398 /
      DATA  RADC/.0174532/
      SAVE XA,YA
C
C  Convert the angle to radians.
C
      ANGR = RADC*ANGD
C
C  Translate, scale, and rotate the object so that its center is
C  at (X,Y).
C
      DO 20 K=1,ID
      XB(K) = X+SCALE*(XA(K)*COS(ANGR)-YA(K)*SIN(ANGR))
      YB(K) = Y+SCALE*(XA(K)*SIN(ANGR)+YA(K)*COS(ANGR))
   20 CONTINUE
      CALL GSFAIS(1)
      CALL GSFACI(1)
      CALL GFA(ID,XB,YB)
      RETURN
      END
