C
C  This program produces five 9 x 9 color charts.  Blue and green
C  intensities are varied on each chart, and the red intensity is
C  varied between charts.  To produce the charts, color indices
C  1-84 are used.  If plotting on a device with fewer than 84 colors
C  available, unsatisfactory results will obtain.
C
      PROGRAM COEX01
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
      DIMENSION RGB(3,405)
C
C  Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Set fill area interior style to solid.
C
      CALL GSFAIS(1)
C
C  Define all permutations of RGB's needed.
C
      CALL COLSET(RGB)
C
C  Put out 5 color charts, varying the red value with each frame.
C
      DO 10 NPAGE=1,5
        CALL SETCOL(RGB,NPAGE,IWKID)
        CALL TTLBAR()
        CALL CUBE()
        CALL TITLE(NPAGE,IWKID)
        CALL FRAME
   10 CONTINUE
C
C  Close GKS.
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
C
      END
      SUBROUTINE COLSET(RGB)
C
C  Define the RGB color triples needed.  This is done by filling the
C  RGB array with all 405 permutations for a 9 x 9 color cube in 5
C  plots.  All values are normalized to fall in the range 0 to 1.
C
      DIMENSION RGB(3,405)
      DIMENSION CLRS(9)
C
      DATA CLRS(1),CLRS(2),CLRS(3)/   0.,  32.,  64./
      DATA CLRS(4),CLRS(5),CLRS(6)/  96., 128., 160./
      DATA CLRS(7),CLRS(8),CLRS(9)/ 192., 224., 255./
C
      INDEX = 1
      DO 10 I=1,5
        DO 20 J=1,9
          DO 30 K=1,9
            RGB (1,INDEX) = CLRS(2*I-1)/255.0
            RGB (2,INDEX) = CLRS(J)/255.0
            RGB (3,INDEX) = CLRS(K)/255.0
            INDEX = INDEX + 1
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
      RETURN
      END

      SUBROUTINE SETCOL(RGB,INDEX,IWKID)
C
C  Define color indices 3-84 to contain the desired colors.
C
      DIMENSION RGB(3,405)
      CALL GSCR(IWKID,0,0.,0.,0.)
      CALL GSCR(IWKID,1,1.,1.,1.)
      NDX = 81*(INDEX-1)+1
      DO 10 N=1,81
      CALL GSCR(IWKID,N+2,RGB(1,NDX),RGB(2,NDX),RGB(3,NDX))
      NDX = NDX + 1
   10 CONTINUE
C
      RETURN
      END
      SUBROUTINE CUBE()
C
C  Draw 81 color squares of size DELX x DELY.  Start at position
C  (X1,Y1).  Use blank space of GAPX and GAPY between the squares.
C
      PARAMETER (DELX=0.095, DELY=0.095)
      PARAMETER (GAPX=DELX-0.003, GAPY=DELY-0.003)
      DIMENSION CELLX(4), CELLY(4)
      DATA X1,Y1/0.13, 0.075/
C
C  Set the parameters for the starting point.
C
      X = X1
      Y = Y1
C
C  Start with color index 3 so that the foreground and background
C  color indices are not touched.
C
      INDCOL = 3
C
C  Put out 9 x 9 squares.
C
      DO 10 I=1,9
        DO 20 L=1,9
C
C  Define square.
C
          CELLX(1) = X
          CELLY(1) = Y
          CELLX(2) = X + GAPX
          CELLY(2) = Y
          CELLX(3) = X + GAPX
          CELLY(3) = Y + GAPY
          CELLX(4) = X
          CELLY(4) = Y + GAPY
C
C  Set color to index INDCOL and draw square.
C
          CALL GSFACI(INDCOL)
          CALL GFA(4,CELLX,CELLY)
          X = X + DELX
          INDCOL = INDCOL + 1
   20   CONTINUE
        Y = Y + DELY
        X = X1
   10 CONTINUE
C
      RETURN
      END
      SUBROUTINE TTLBAR()
C
C  Label the chart with the red intensity.
C
      DIMENSION CELLX(4), CELLY(4)
      DATA X1,Y1,XSIDE,YSIDE/ 0.70, 0.93, 0.28, 0.04/
C
C  Define the fill area.
C
      CELLX(1) = X1
      CELLY(1) = Y1
      CELLX(2) = X1 + XSIDE
      CELLY(2) = Y1
      CELLX(3) = X1 + XSIDE
      CELLY(3) = Y1 + YSIDE
      CELLX(4) = X1
      CELLY(4) = Y1 + YSIDE
C
C  Set the color and draw the fill area.
C
      CALL GSFACI(3)
      CALL GFA(4,CELLX,CELLY)
C
      RETURN
      END
      SUBROUTINE TITLE(NPAGE,IWKID)
C
C  Label the chart using white characters..
C
      DIMENSION AX1(3),AY1(3),AX2(3),AY2(3),IL(9)
      CHARACTER RVAL*16,CVAL*3,DVAL*4
      DATA AX1(1),AX1(2),AX1(3)/ .073, .048, .073/
      DATA AY1(1),AY1(2),AY1(3)/ .900, .800, .800/
      DATA AX2(1),AX2(2),AX2(3)/ .960, .860, .860/
      DATA AY2(1),AY2(2),AY2(3)/ .028, .010, .028/
      DATA DELX,DELY/.095,.095/
      DATA IL(1),IL(2),IL(3),IL(4),IL(5),IL(6),IL(7),IL(8),IL(9)
     1    /    3,    2,    2,    2,    1,    1,    1,    1,    1/
C
      CALL GSPLCI(1)
C
C  Print the title of each axis.
C
      CALL PLCHHQ(0.5,0.01,'Blue Axis' ,0.015,0.,0.)
      CALL PLCHHQ(0.05,0.5,'Green Axis',0.015,90.,0.)
C
C  Draw the arrow-line on each axis.
C
      CALL LINE(0.073,0.90,0.073,0.078)
      CALL GSFACI(1)
      CALL GFA(3,AX1,AY1)
C
      CALL LINE(0.15,0.028,0.96,0.028)
      CALL GSFACI(1)
      CALL GFA(3,AX2,AY2)
C
C  Print the red value for the frame at hand.
C
      ITMP = 64*(NPAGE-1)
      IF (NPAGE .EQ. 5) ITMP = 255
      RLV = REAL(ITMP)/255.
      WRITE(RVAL,100) ITMP,RLV
  100 FORMAT('RED = ',I3,' = ',F4.2)
      CALL PLCHHQ(.84,.95,RVAL,.014,0.,0.)
C
C  Print the Green values up the side.
C
      X = 0.10
      Y1 = 0.125
      Y2 = 0.108
      DO 20 I=1,9
        ITMP = 32*(I-1)
        IF (I .EQ. 9) ITMP=255
        WRITE(CVAL,110) ITMP
        RVL = REAL(ITMP)/255.
        WRITE(DVAL,120) RVL
  110   FORMAT(I3)
  120   FORMAT(F4.2)
        CALL PLCHHQ(X,Y1,CVAL(IL(I):3),.008,0.,0.)
        CALL PLCHHQ(X,Y2,DVAL,.008,0.,0.)
        Y1 = Y1+DELY
        Y2 = Y2+DELY
   20 CONTINUE
C
C  Print the Blue values across the bottom.
C
      Y1 = 0.060
      Y2 = 0.043
      X = 0.18
      DO 30 I=1,9
        ITMP = 32*(I-1)
        IF (I .EQ. 9) ITMP=255
        WRITE(CVAL,110) ITMP
        RVL = REAL(ITMP)/255.
        WRITE(DVAL,120) RVL
        CALL PLCHHQ(X,Y1,CVAL(IL(I):3),.008,0.,0.)
        CALL PLCHHQ(X,Y2,DVAL,.008,0.,0.)
        X = X+DELX
   30 CONTINUE
C
      RETURN
      END
