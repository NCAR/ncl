
      PROGRAM PGKEX02
C
C  Simple demo of GKS output primitives puts out sample 
C  POLYLINE, POLYMARKER, TEXT, FILL AREA and CELL ARRAY. 
C 
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
      REAL     ZZX(9), ZZYL(9), ZZYM(9), CIRX(9), CIRY(9)
      INTEGER  ICELLS(24,12) 
C 
      DATA  ZZX / -9.0, -8.0, -7.0, -6.0, -5.0, -4.0, -3.0, -2.0, -1.0/ 
      DATA  ZZYL/  6.5,  8.5,  6.5,  8.5,  6.5,  8.5,  6.5,  8.5,  6.5/ 
      DATA  ZZYM/ -1.0,  1.0, -1.0,  1.0, -1.0,  1.0, -1.0,  1.0, -1.0/
      DATA CIRX / 6.15, 5.26, 4.25, 3.59, 3.59, 4.25, 5.26, 6.15, 6.50/
      DATA CIRY / 8.46, 8.98, 8.80, 8.01, 6.99, 6.20, 6.02, 6.54, 7.50/
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF,IDUM) 
      CALL GOPWK (IWKID,LUNIT,IWTYPE)
      CALL GACWK (IWKID) 
C 
C  Define normalization transformation 1 and select it.
C
      CALL GSWN   (1, -10.0, 10.0, -10.0, 10.0)
      CALL GSVP   (1, 0.1, 0.9, 0.1, 0.9) 
      CALL GSELNT (1) 
C 
C  Draw a zig-zag POLYLINE. 
C
      CALL GPL (9, ZZX, ZZYL)
C
C  Set the marker type to 2 (plus sign) and draw markers.
C
      CALL GSMK (2)
      CALL GPM (9, ZZX, ZZYM)
C  
C  Set the fill area interior style to 1 (solid fill) and draw a 
C  solid filled nonagon.
C
      CALL GSFAIS (1)
      CALL GFA (9, CIRX, CIRY)
C 
C  Define 24x12 foreground/background checkerboard pattern. 
C
      DO 25 IX=1,24
        JX = MOD(IX,2)
        DO 20 IY=1,12 
          JY = MOD(IY,2)
          IF ((JX.EQ.1 .AND. JY.EQ.1) .OR. (JX.EQ.0 .AND. JY.EQ.0)) THEN
            ICELLS (IX,IY) = 1
          ELSE
            ICELLS (IX,IY) = 0
          ENDIF
   20   CONTINUE 
   25 CONTINUE 
C
C  Draw the checkerboard with CELL ARRAY. 
C
      CALL GCA (1.5,-1.25,8.5,1.25, 24, 12, 1, 1, 24, 12, ICELLS)
C 
C  Set the character height to 3% of the screen (.03*20.)
C
      CALL GSCHH (0.6) 
C
C  Set the text alignment to "center" in the horizontal and "half" in
C  the vertical.
C
      CALL GSTXAL (2, 3)
      XPOS =  0.0
      YPOS = -5.0 
C
C  Draw the text string. 
C
      CALL GTX (0.0, -7., 'Example text string')
C
C  Label the primitives.
C
      CALL GTX(-5.0,5.0,'Polyline')
      CALL GTX( 5.0,5.0,'Fill area')
      CALL GTX(-5.0,-2.5,'Polymarker')
      CALL GTX( 5.0,-2.5,'Cell array')
      CALL GTX( 0.0,-9.5,'Text')
C
      CALL FRAME
C
C  Deactive and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
