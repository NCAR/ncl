
      PROGRAM FSPPONTS
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
C Create a line plot with connected markers using points.
C
      REAL XDRA(20),YDRA(20)
      REAL X2DRA(20),Y2DRA(20)
      REAL X3DRA(20),Y3DRA(20)

      DATA NPTS/20/
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Set up a color table
C
      CALL DFCLRS (IWKID)
C
C Fill the data arrays.
C
      DO 101 I=1,NPTS
         XDRA(I)=SIN(REAL(I))
         YDRA(I)=COS(REAL(I))
         X2DRA(I)=XDRA(I) * .66
         Y2DRA(I)=YDRA(I) * .66
         X3DRA(I)=XDRA(I) * .3
         Y3DRA(I)=YDRA(I) * .3
 101  CONTINUE
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
      CALL EZXY (XDRA,YDRA,NPTS,'POINTS EXAMPLE$')
C
C Increase the size of the polymarkers
C
      CALL GSMKSC(2.0)
C
C Set the color of the polymarker
C
      CALL GSPLCI(7)
C
C Set the line width of the lines connecting the polymarkers.
C
      CALL GSLWSC(3.0)
C
C Put a plus sign at each of the x-y positions.
C
      CALL POINTS (XDRA,YDRA,NPTS,-2,1)
C
C Set the color of the polymarker
C
      CALL GSPLCI(9)
C
C Put a circle at each of the x2-y2 positions.
C
      CALL POINTS (X2DRA,Y2DRA,NPTS,-4,1)
C
C Put an asterix at each of the x3-y3 positions,
C and do not connect with lines.
C
      CALL POINTS (X3DRA,Y3DRA,NPTS,-3,0)
C
C Advance the frame.
C
      CALL FRAME
C
C Deactivate and close workstation, close GKS.
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

      SUBROUTINE DFCLRS (IWKID)
C
C Define a set of RGB color triples for colors 1 through 15.
C
      DIMENSION RGBV(3,15)
C
C Define the RGB color triples needed below.
C
      DATA RGBV / 0.00 , 0.00 , 0.00 ,
     +     0.70 , 0.70 , 0.70 ,
     +     0.75 , 0.50 , 1.00 ,
     +     0.50 , 0.00 , 1.00 ,
     +     0.00 , 0.00 , 1.00 ,
     +     0.00 , 0.50 , 1.00 ,
     +     0.00 , 1.00 , 1.00 ,
     +     0.00 , 1.00 , 0.60 ,
     +     0.00 , 1.00 , 0.00 ,
     +     0.70 , 1.00 , 0.00 ,
     +     1.00 , 1.00 , 0.00 ,
     +     1.00 , 0.75 , 0.00 ,
     +     1.00 , 0.38 , 0.38 ,
     +     1.00 , 0.00 , 0.38 ,
     +     1.00 , 0.00 , 0.00 /
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.
C
      CALL GSCR (IWKID,0,1.,1.,1.)
C
      DO 101 I=1,15
         CALL GSCR (IWKID,I,RGBV(1,I),RGBV(2,I),RGBV(3,I))
 101  CONTINUE
C
C Done.
C
      RETURN
C
      END

