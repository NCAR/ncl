
      PROGRAM FGKGPM
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
C  Illustrate polymarkers.
C
      PARAMETER (ID=50)
      DIMENSION XM1(ID),YM1(ID),XM2(ID),YM2(ID),XM3(ID),YM3(ID)
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define the necessary color indices.
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,1.,1.,0.)
      CALL GSCR(IWKID,3,0.,1.,0.)
      CALL GSCR(IWKID,4,1.,1.,0.)
      CALL GSCR(IWKID,5,0.,1.,1.)
      CALL GSCR(IWKID,6,1.,0.,1.)
C
C  Marker 1, dot (fill a large circular dot with markers of type 1).
C
C
C   Position and radius of the dot outline.
C
      X0 = .5
      Y0 = .7
      R  = .08
      JL = 8
      AINC = R/REAL(JL)
      CALL GSMK(1)
      DO 10 J=0,JL
      Y = Y0+REAL(J)*AINC
      XE = X0+SQRT(MAX(R**2-(Y-Y0)**2,0.))
      X = X0
   20 CONTINUE
C
C   Fill the circle with dot markers using symmetries.
C	
      CALL GPM(1,X,Y)
      CALL GSPMCI(1)
      CALL GPM(1,X,2*Y0-Y)
      CALL GPM(1,2*X0-X,2*Y0-Y)
      CALL GPM(1,2*X0-X,Y)
      X = X+AINC
      IF (X .GT. XE) GO TO 10
      GO TO 20
   10 CONTINUE
C
C   Label the dot.
C
      CALL PCSETI('CD',1)
      CALL GSPLCI(1)
      CALL PLCHHQ(X0,Y0+R+.05,'Marker 1 (dots)',.018,0.,0.)
C
C  Marker 2, plus (make a plus from the plus markers.)
C
C
C   Center of big plus.
C
      X0 = .83
      Y0 = .5
      R  = .1
      JL = 7
      AINC = R/REAL(JL)
      DO 30 J=-JL,JL,1
      Y = Y0+REAL(J)*AINC
      IDX = J+JL+1
      XM1(IDX) = X0
      YM1(IDX) = Y
      X = X0+REAL(J)*AINC
      XM2(IDX) = X
      YM2(IDX) = Y0
   30 CONTINUE
      CALL GSMK(2)
      CALL GSPMCI(1)
C
C  Put plus markers along the two axes of the big plus.
C
      CALL GPM(2*JL+1,XM1,YM1)
      CALL GPM(2*JL+1,XM2,YM2)
C
C   Label the big plus.
C
      CALL PCSETI('CD',1)
      CALL GSPLCI(1)
      CALL PLCHHQ(X0,Y0+R+.05,'Marker 2 (plus signs)',.018,0.,0.)
C
C  Marker 3, asterisk (make an asterisk from the asterisk markers.)
C
      X0 = .7
      Y0 = .15
      R  = .1
      JL = 5
      AINC = R/REAL(JL)
      DO 40 J=-JL,JL,1
      Y = Y0+REAL(J)*AINC
      IDX = J+JL+1
      XM1(IDX) = X0
      YM1(IDX) = Y
      P = 0.5*SQRT(2.)*(Y-Y0)
      IF (Y .GE. 0.) THEN
        XM2(IDX) = X0+P
        YM2(IDX) = Y0+P
        XM3(IDX) = X0-P
        YM3(IDX) = Y0+P
      ELSE
        XM2(IDX) = X0-P
        YM2(IDX) = Y0-P
        XM3(IDX) = X0+P
        YM3(IDX) = Y0-P
      ENDIF
   40 CONTINUE
      CALL GSMK(3)
      CALL GSPMCI(1)
C
c Put asterisk markers along the axes of the big asterisk.
C
      CALL GPM(2*JL+1,XM1,YM1)
      CALL GPM(2*JL+1,XM2,YM2)
      CALL GPM(2*JL+1,XM3,YM3)
C
C   Label the big asterisk.
C
      CALL PCSETI('CD',1)
      CALL GSPLCI(1)
      CALL PLCHHQ(X0,Y0+R+.05,'Marker 3 (asterisks)',.018,0.,0.)
C
C  Marker 4, circle (make a big circle from the circle markers.)
C
      X0 = .3
      Y0 = .15
      R  = .1
      JL = 50
      RADINC = 2.*3.1415926/REAL(JL)
      DO 50 J=1,JL
      X = X0+R*COS(REAL(J)*RADINC)
      Y = Y0+R*SIN(REAL(J)*RADINC)
      XM1(J) = X
      YM1(J) = Y
   50 CONTINUE
      CALL GSMK(4)
      CALL GSPMCI(1)
      CALL GSMKSC(2.)
      CALL GPM(JL,XM1,YM1)
C
C   Label the big circle.
C
      CALL PCSETI('CD',1)
      CALL GSPLCI(1)
      CALL PLCHHQ(X0,Y0+R+.05,'Marker 4 (circles)',.018,0.,0.)
C
C  Marker 5, cross (make a big cross from the cross markers.)
C
      X0 = .17
      Y0 = .5
      R  = .1
      JL = 5
      AINC = R/REAL(JL)
      DO 60 J=-JL,JL,1
      Y = Y0+REAL(J)*AINC
      IDX = J+JL+1
      P = 0.5*SQRT(2.)*(Y-Y0)
      IF (Y .GE. 0.) THEN
        XM2(IDX) = X0+P
        YM2(IDX) = Y0+P
        XM3(IDX) = X0-P
        YM3(IDX) = Y0+P
      ELSE
        XM2(IDX) = X0-P
        YM2(IDX) = Y0-P
        XM3(IDX) = X0+P
        YM3(IDX) = Y0-P
      ENDIF
   60 CONTINUE
      CALL GSMK(5)
      CALL GSPMCI(1)
C
C  Plot cross markers along the axes of the big cross.
C
      CALL GSMKSC(1.)
      CALL GPM(2*JL+1,XM2,YM2)
      CALL GPM(2*JL+1,XM3,YM3)
C
C   Label the big cross.
C
      CALL PCSETI('CD',1)
      CALL GSPLCI(1)
      CALL PLCHHQ(X0,Y0+R+.05,'Marker 5 (crosses)',.018,0.,0.)
C
C  Draw a big circle in the center by applying a large marker size
C  scale factor to the circle marker.
C
      X0 = .5
      Y0 = .46
      CALL GSMK(4)
      CALL GSPMCI(1)
      CALL GSMKSC(15.)
      CALL GPM(1,X0,Y0)
      CALL PCSETI('CD',1)
      CALL GSPLCI(1)
      CALL PLCHHQ(X0,Y0+.028,'Circle',.015,0.,0.)
      CALL PLCHHQ(X0,Y0     ,'Scaled',.015,0.,0.)
      CALL PLCHHQ(X0,Y0-.028,'by 15.',.015,0.,0.)
C
C  Label the plot (PLOTCHAR strokes its characters with lines, so the
C  PLOTCHAR character attributes are controlled by the GKS polyline 
C  attributes).
C
      CALL GSPLCI(1)
      CALL PCSETI('CD',1)
      CALL GSLWSC(2.)
      CALL PLCHHQ(.5,.915,'Polymarkers',.025,0.,0.)
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
