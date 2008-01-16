
      PROGRAM MPEX07
C
C This program produces a stereographic view of the North Pole,
C with labelled meridians.
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Define the label for the top of the map.
C
      CHARACTER*32 PLBL
C
      DATA PLBL / 'MERIDIONAL LABELS ON A POLAR MAP' /
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Turn off the clipping indicator.
C
      CALL GSCLIP (0)
C
C Move the map a little to provide more room for labels.
C
      CALL MAPPOS (.075,.925,.05,.90)
C
C Use an elliptical (circular, in this case) perimeter.
C
      CALL MAPSTI ('EL',1)
C
C Show continents and international boundaries.
C
      CALL MAPSTC ('OU','PO')
C
C Use a stereographic projection, centered at the North Pole.
C
      CALL MAPROJ ('ST',90.,0.,-100.)
C
C Specify the angular distances to the edges of the map.
C
      CALL MAPSET ('AN',80.,80.,80.,80.)
C
C Draw the map.
C
      CALL MAPDRW
C
C Call a routine to label the meridians.  This routine is not
C a part of EZMAP; the code is given below.
C
      CALL MAPLBM
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.975,PLBL,32,2,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Advance the frame.
C
      CALL FRAME
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
C Done.
C
      STOP
C
      END
      SUBROUTINE MAPLBM
C
C This routine labels the meridians if and only if the current
C projection is azimuthal and centered at one of the poles, a
C circular boundary is being used, and the grid increment is an
C integral divisor of 180.  The routine was not thought general
C enough to include in EZMAP itself, but may nevertheless be of
C interest to users.
C
C Necessary local declarations.
C
      CHARACTER*2 PROJ
      CHARACTER*3 CHRS
      CHARACTER*4 CHLB
C
C See if the conditions required for MAPLBM to work are met.
C
C The projection must be azimuthal, ...
C
      CALL MAPGTC ('PR',PROJ)
      IF (PROJ.NE.'ST'.AND.PROJ.NE.'OR'.AND.PROJ.NE.'LE'.AND.
     +    PROJ.NE.'GN'.AND.PROJ.NE.'AE') RETURN
C
C the pole latitude must be +90 degrees or -90 degrees, ...
C
      CALL MAPGTR ('PT',PLAT)
      IF (ABS(PLAT).LT.89.9999) RETURN
C
C the perimeter must be elliptical, ...
C
      CALL MAPGTI ('EL',IELP)
      IF (IELP.EQ.0) RETURN
C
C the values used in the SET call must define a circle, ...
C
      CALL GETSET (FLEW,FREW,FBEW,FTEW,ULEW,UREW,VBEW,VTEW,LNLG)
      ILEW=KFPX(FLEW)
      IREW=KFPX(FREW)
      IBEW=KFPY(FBEW)
      ITEW=KFPY(FTEW)
      IF (ULEW+UREW.GT.0.0001.OR.VBEW+VTEW.GT.0.0001) RETURN
      IF (ULEW+VTEW.GT.0.0001.OR.VBEW+UREW.GT.0.0001) RETURN
C
C and the grid spacing must be an integral divisor of 180.
C
      CALL MAPGTR ('GR',GRID)
      IF (MOD(GRID,1.).NE.0.OR.
     +    MOD(180,INT(GRID)).NE.0) RETURN
C
C All conditions are satisfied.  Label the meridians.
C
C Collect the necessary information.
C
      IGRD=GRID
      CALL MAPGTR ('PN',PLON)
      CALL MAPGTR ('RO',ROTA)
      CALL MAPGTI ('LS',ICSZ)
      IF (ICSZ.EQ.0) THEN
        ICSZ=8
      ELSE IF (ICSZ.EQ.1) THEN
        ICSZ=12
      ELSE IF (ICSZ.EQ.2) THEN
        ICSZ=16
      ELSE IF (ICSZ.EQ.3) THEN
        ICSZ=24
      END IF
      WOCH=(REAL(  ICSZ)/REAL(IREW-ILEW))*(UREW-ULEW)
      HOCH=(REAL(2*ICSZ)/REAL(ITEW-IBEW))*(VTEW-VBEW)
      HOLB=HOCH/1.5
C
C Loop on the label values.
C
      DO 101 I=-180,179,IGRD
C
C Express the value of the longitude in a nice form.
C
        WRITE (CHRS,1001) ABS(I)
        NCHS=0
        IF (ABS(I).GE.100) THEN
          NCHS=NCHS+1
          CHLB(NCHS:NCHS)=CHRS(1:1)
        END IF
        IF (ABS(I).GE.10) THEN
          NCHS=NCHS+1
          CHLB(NCHS:NCHS)=CHRS(2:2)
        END IF
        NCHS=NCHS+1
        CHLB(NCHS:NCHS)=CHRS(3:3)
        IF (I.GT.-180.AND.I.LT.0) THEN
          NCHS=NCHS+1
          CHLB(NCHS:NCHS)='W'
        ELSE IF (I.GT.0.AND.I.LT.180) THEN
          NCHS=NCHS+1
          CHLB(NCHS:NCHS)='E'
        END IF
C
C Compute the width of the label.
C
       WOLB=REAL(NCHS)*WOCH
C
C Find the angle at which the labelled meridian lies on the plot.
C
        IF (PLAT.GT.0.) THEN
          ANGD=REAL(I-90)-PLON-ROTA
        ELSE
          ANGD=REAL(90-I)+PLON-ROTA
        END IF
C
C Reduce the angle to the range from -180 to +180 and
C find its equivalent in radians.
C
        ANGD=ANGD+(SIGN(180.,180.-ANGD)
     +            -SIGN(180.,ANGD+180.))
        ANGR=.017453292519943*ANGD
C
C Figure out where the end of the meridian is.
C
        XEND=UREW*COS(ANGR)
        YEND=VTEW*SIN(ANGR)
C
C Extend the meridian a little to make a tick mark.
C
        CALL LINE (XEND,YEND,1.015*XEND,1.015*YEND)
C
C Compute a center position for the label which puts its nearest
C edge at a fixed distance from the perimeter.  First, compute
C the components (DELX,DELY) of the vector from the center of the
C label box to the edge nearest the perimeter.
C
        IF      (ANGD.LT.-179.9999) THEN
          DELX=+0.5*WOLB
          DELY= 0.
        ELSE IF (ANGD.LT. -90.0001) THEN
          DELX=+0.5*WOLB
          DELY=+0.5*HOLB
        ELSE IF (ANGD.LT. -89.9999) THEN
          DELX= 0.0
          DELY=+0.5*HOLB
        ELSE IF (ANGD.LT.  -0.0001) THEN
          DELX=-0.5*WOLB
          DELY=+0.5*HOLB
        ELSE IF (ANGD.LT.  +0.0001) THEN
          DELX=-0.5*WOLB
          DELY= 0.0
        ELSE IF (ANGD.LT. +89.9999) THEN
          DELX=-0.5*WOLB
          DELY=-0.5*HOLB
        ELSE IF (ANGD.LT. +90.0001) THEN
          DELX= 0.0
          DELY=-0.5*HOLB
        ELSE IF (ANGD.LT.+179.9999) THEN
          DELX=+0.5*WOLB
          DELY=-0.5*HOLB
        ELSE
          DELX=+0.5*WOLB
          DELY= 0.0
        END IF
C
C Then, solve (for FMUL) the following equation:
C
C   SQRT((FMUL*XEND+DELX)**2+(FMUL*YEND+DELY)**2))=1.02*UREW
C
C which expresses the condition that the corner of the box
C nearest the circular perimeter should be at a distance of
C 1.02*(the radius of the perimeter) away from the center of
C the plot.
C
        A=XEND*XEND+YEND*YEND
        B=2.*(XEND*DELX+YEND*DELY)
        C=DELX*DELX+DELY*DELY-1.0404*UREW*UREW
C
        FMUL=(-B+SQRT(B*B-4.*A*C))/(2.*A)
C
C Draw the label.
C
        CALL PWRIT (FMUL*XEND,FMUL*YEND,CHLB,NCHS,ICSZ,0,0)
C
C End of loop.
C
  101 CONTINUE
C
C Done.
C
      RETURN
C
C Format
C
 1001 FORMAT (I3)
C
      END
