
      PROGRAM AGEX11
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
C Create a scattergram.
C
      REAL XDRA(500),YDRA(500)
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data arrays.
C
      DO 101 I=1,500
        XDRA(I)=.5+(2.*(FRAN()-.5))**5
        YDRA(I)=.5+(2.*(FRAN()-.5))**5
  101 CONTINUE
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Suppress the frame advance.
C
      CALL AGSETI ('FRAME.',2)
C
C Suppress the drawing of curves by the EZ... routines.
C
      CALL AGSETI ('SET.',-1)
C
C Draw the background, using EZXY.
C
      CALL EZXY (XDRA,YDRA,500,'EXAMPLE 11 (SCATTERGRAM)$')
C
C Put a plus sign at each of the x-y positions.
C
      CALL POINTS (XDRA,YDRA,500,-2,0)
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
      STOP
C
      END
      FUNCTION FRAN()
C
C Pseudo-random-number generator.
C
        DOUBLE PRECISION X
        SAVE X
        DATA X / 2.718281828459045D0 /
        X=MOD(9821.D0*X+.211327D0,1.D0)
        FRAN=REAL(X)
        RETURN
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
