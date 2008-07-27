C
C	$Id: tgks0a.f,v 1.4 2008-07-27 00:59:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      PROGRAM TGKS0A
C
C  GKS sample program, level 0A.
C
C  Puts out sample POLYLINE, POLYMARKER, TEXT, FILL AREA and CELL ARRAY.
C
C  (Knowledge of device and environment assumed -- normal inquiry and
C   error checking are omitted from the sample program).
C
C
      REAL     ZZX(9), ZZYL(9),  ZZYM(9),  CIRX(9),  CIRY(9)
      INTEGER  ICELLS(24,12)
C
      DATA  ZZX / -9.0, -8.0, -7.0, -6.0, -5.0, -4.0, -3.0, -2.0, -1.0/
      DATA  ZZYL/  6.5,  8.5,  6.5,  8.5,  6.5,  8.5,  6.5,  8.5,  6.5/
      DATA  ZZYM/  1.5,  3.5,  1.5,  3.5,  1.5,  3.5,  1.5,  3.5,  1.5/
      DATA CIRX / 6.15, 5.26, 4.25, 3.59, 3.59, 4.25, 5.26, 6.15, 6.50/
      DATA CIRY / 8.46, 8.98, 8.80, 8.01, 6.99, 6.20, 6.02, 6.54, 7.50/
C
      DATA ISZ/0/
C
C
C                                Open GKS w/ logical unit 6 for errors.
      CALL GOPKS (6,ISZ)
C                                Open a wkstn of type 1, assigning 1
C                                for its id, 2 for connection id.
      CALL GOPWK (1, 2, 1)
C                                Activate the workstation.
      CALL GACWK (1)
C
C
C                                Define normalization transformation 1.
C                                Window in World Coordinates.
      CALL GSWN   (1, -10.0, 10.0, -10.0, 10.0)
C                                Viewport in Normalized Device Coords.
      CALL GSVP   (1, 0.1, 0.9, 0.1, 0.9)
C                                Select just-defined transformation.
      CALL GSELNT (1)
C
C
C                                Draw a zig-zag POLYLINE.
      CALL GPL (9, ZZX, ZZYL)
C
C
C               "+"              Set marker type.
      CALL GSMK (2)
C                                Draw a POLYMARKER.
      CALL GPM (9, ZZX, ZZYM)
C
C
C                solid           Set fill area interior style.
      CALL GSFAIS (1)
C                                Draw a solid circle w/FILL AREA.
      CALL GFA (9, CIRX, CIRY)
C
C
C                                Define 24x12 foreground/background
C                                checkerboard pattern.
      DO 25 IX=1,24
      JX = MOD(IX,2)
      DO 20 IY=1,12
      JY = MOD(IY,2)
      IF ((JX.EQ.1 .AND. JY.EQ.1) .OR. (JX.EQ.0 .AND. JY.EQ.0)) THEN
      ICELLS (IX,IY) = 1
      ELSE
      ICELLS (IX,IY) = 0
      ENDIF
20    CONTINUE
25    CONTINUE
C                                Draw the checkerboard w/ CELL ARRAY.
      CALL GCA (2.,1.5,8.,4.0, 24, 12, 1, 1, 24, 12, ICELLS)
C
C
C                                Set chr hgt to 2% of screen (.02*20).
      CALL GSCHH (0.4)
C                cen,hlf         Set alignment to center string on posn.
      CALL GSTXAL (2, 3)
      XPOS = 0.0
      YPOS = -5.0
C                                 Draw the text string.
      CALL GTX (XPOS, YPOS, 'Example string, centered in the center')
C                                 Flush metacode buffer.
      CALL GCLRWK(1,1)
C                                 Deactivate the workstation.
      CALL GDAWK (1)
C                                 Close the workstation.
      CALL GCLWK (1)
C                                 Close GKS
      CALL GCLKS
C
C
      STOP
      END
