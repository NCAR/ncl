
      PROGRAM PGKEX21
C
C  Illustrate line joins, caps, and miter limits.
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWKID=1)
C
C  Specify certain key points on the plot (indices 1-3 for vertices 
C  of joins; 4-6 for vertices of caps; 7-8 for vertices of miter 
C  limits).
C
      DIMENSION X(8),Y(8)
C
      DATA X/0.20, 0.50, 0.80, 0.28, 0.28, 0.28, 0.75, 0.75/
      DATA Y/0.80, 0.67, 0.80, 0.35, 0.19, 0.03, 0.35, 0.14/
C
C  Open GKS.
C
      CALL GOPKS (IERRF, IDUM)
C
C  Open and activate a color PostScript workstation.
C
      CALL GOPWK (IWKID, 2, NGPSWK('PS','PORT','COLOR'))
      CALL GACWK (IWKID)
C
C  Define color indices.
C
      CALL GSCR(IWKID, 0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID, 1, 0.0, 0.0, 0.0)
C
C  Set workstation "1" as the one involved in subsequent NGSETI settings.
C
      CALL NGSETI('Workstation',1)
C
C  Line joins.
C
C 
C  Labels.
C
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',1)
      CALL PLCHHQ(.5,Y(1)+0.15,'Line joins',.032,0.,0.)
      CALL PLCHHQ(X(1),Y(1)+0.07,'miter',.025,0.,0.)
      CALL PLCHHQ(X(2),Y(1)+0.07,'round',.025,0.,0.)
      CALL PLCHHQ(X(3),Y(1)+0.07,'bevel',.025,0.,0.)
C
C  Loop through the three types of line join.
C
      DO 10 I=1,3
        CALL NGSETI('Joins',I-1)
        CALL DRAWV(X(I),Y(I),MOD(I-1,2),0,90.,0.7,1,25.)
        CALL DRAWV(X(I),Y(I),MOD(I-1,2),1,90.,0.7,0,1.)
   10 CONTINUE
C
C  Line caps.
C
C 
C  Labels.
C
      CALL PLCHHQ(X(4),Y(4)+0.15,'Line caps',.032,0.,0.)
      CALL PLCHHQ(X(4),Y(4)+0.06,'butt',.025,0.,0.)
      CALL PLCHHQ(X(4),Y(5)+0.06,'round',.025,0.,0.)
      CALL PLCHHQ(X(4),Y(6)+0.06,'square',.025,0.,0.)
C
C  Loop through the three types of line caps.
C
      DO 20 I=4,6
        CALL NGSETI('Caps',I-4)
        CALL DRAWV(X(I),Y(I),0,0,180.,0.75,1,25.)
        CALL DRAWV(X(I),Y(I),0,2,180.,0.75,0,1.)
   20 CONTINUE
C
C  Miter limits.
C 
C  Labels.
C
      CALL PLCHHQ(X(7),Y(7)+0.15,'Miter limits',.032,0.,0.)
      CALL PLCHHQ(X(7),Y(7)+0.07,'default (= 10.)',.025,0.,0.)
      CALL PLCHHQ(X(7),Y(8)+0.04,'limit = 1.',.025,0.,0.)
C
C  Set line join to miter.
C
      CALL NGSETI('Join',0)
C
C  Default.
C
      CALL DRAWV(X(7),Y(7),0,0,35.,0.5,1,20.)
C
C  Limit = 1.
C
      CALL NGSETR('Miter',1.)
      CALL DRAWV(X(8),Y(8),0,0,35.,0.5,1,20.)
C
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
      SUBROUTINE DRAWV(X,Y,IORIEN,IDOT,ANG,SCALE,ICOLOR,THICK)
C
C  Draw a "V" where:
C
C       (X,Y)   is the coordinate of the vertex.
C       IORIEN  flags whether the "V" is up (=1) or down (=0).
C       IDOT    flags whether dots are to be drawn at coordinates.
C               = 0 no dots.
C               = 1 dots at all coordinates
C               = 2 dots only at the end points
C       ANG     is the angle (in degrees) at the vertex of the "V".
C       SCALE   scales how big the "V" is.
C       ICOLOR  is the color index to be used for the lines.
C       THICK   is the linewidth scale factor.
C
      DIMENSION XV(3),YV(3)
C
      DATA  RADC,DSIZ/.0174532, 0.25/
C
      CALL GSPLCI(ICOLOR)
      CALL GSLWSC(THICK)
C
      BETA = RADC*(90.-0.5*ANG)
      XOFF = SCALE*DSIZ*COS(BETA)
      YOFF = SCALE*DSIZ*SIN(BETA)
      IF (IORIEN .EQ. 0) THEN
        SIGN =  1.
      ELSE
        SIGN = -1.
      ENDIF
C
      XV(1) = X-XOFF
      XV(2) = X
      XV(3) = X+XOFF
      YV(1) = Y-SIGN*YOFF
      YV(2) = Y
      YV(3) = YV(1)
C
      CALL GPL(3,XV,YV)
C
      IF (IDOT .EQ. 1) THEN
        CALL NGDOTS(XV,YV,3,0.005*THICK,ICOLOR)
      ELSE IF (IDOT .EQ. 2) THEN
        CALL NGDOTS(XV(1),YV(1),1,0.005*THICK,ICOLOR)
        CALL NGDOTS(XV(3),YV(3),1,0.005*THICK,ICOLOR)
      ENDIF
C
      RETURN
      END
