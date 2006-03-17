
      PROGRAM AGEX08
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
C Define the data arrays.
C
      REAL XDRA(101),YDRA(4,101)
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data arrays.
C
      DO 101 I=1,101
        XDRA(I)=-3.14159265358979+
     +            .062831853071796*REAL(I-1)
  101 CONTINUE
C
      DO 103 I=1,4
        FLTI=I
        BASE=2.*FLTI-1.
        DO 102 J=1,101
          YDRA(I,J)=BASE+.75*SIN(-3.14159265358979+
     +             .062831853071796*FLTI*REAL(J-1))
  102   CONTINUE
  103 CONTINUE
C
C Change the line-end character to a period.
C
      CALL AGSETC ('LINE/END.','.')
C
C Specify labels for x and y axes.
C
      CALL ANOTAT ('SINE FUNCTIONS OF T.','T.',0,0,0,' ')
C
C Use a half-axis background.
C
      CALL AGSETI ('BACKGROUND.',3)
C
C Move x axis to the zero point on the y axis.
C
      CALL AGSETF ('BOTTOM/INTERSECTION/USER.',0.)
C
C Specify base value for spacing of major ticks on x axis.
C
      CALL AGSETF ('BOTTOM/MAJOR/BASE.',1.)
C
C Run major ticks on x axis to edge of curve window.
C
      CALL AGSETF ('BOTTOM/MAJOR/INWARD.',1.)
      CALL AGSETF ('BOTTOM/MAJOR/OUTWARD.',1.)
C
C Position x axis minor ticks.
C
      CALL AGSETI ('BOTTOM/MINOR/SPACING.',9)
C
C Run the y axis backward.
C
      CALL AGSETI ('Y/ORDER.',1)
C
C Run plots full-scale in y.
C
      CALL AGSETI ('Y/NICE.',0)
C
C Have AUTOGRAPH scale x and y data the same.
C
      CALL AGSETF ('GRID/SHAPE.',.01)
C
C Use the alphabetic set of dashed-line patterns.
C
      CALL AGSETI ('DASH/SELECTOR.',-1)
C
C Tell AUTOGRAPH how the data arrays are dimensioned.
C
      CALL AGSETI ('ROW.',-1)
C
C Reverse the roles of the x and y arrays.
C
      CALL AGSETI ('INVERT.',1)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Draw the curves.
C
      CALL EZMXY (XDRA,YDRA,4,4,101,'EXAMPLE 8.')
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
C
      END
      SUBROUTINE BNDARY
C
C Routine to draw the plotter-frame edge.
C
      CALL PLOTIT (    0,    0,0)
      CALL PLOTIT (32767,    0,1)
      CALL PLOTIT (32767,32767,1)
      CALL PLOTIT (    0,32767,1)
      CALL PLOTIT (    0,    0,1)
      RETURN
      END
