C
C	$Id: agex12.f,v 1.2 1994-07-08 16:27:29 stautler Exp $
C
      PROGRAM XMPL12
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
C
C Create a sort of histogram.
C
      REAL XDRA(249),YDRA(249),WORK(204),IWRK(204)
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data arrays.  First, we define the histogram
C outline.  This will be used in the call to SFWRLD which
C fills in the area under the histogram.
C
      XDRA(1)=0.
      YDRA(1)=0.
C
      DO 101 I=2,100,2
        XDRA(I  )=XDRA(I-1)
        YDRA(I  )=EXP(-16.*(FLOAT(I/2)/50.-.51)**2)+.1*FRAN()
        XDRA(I+1)=XDRA(I-1)+.02
        YDRA(I+1)=YDRA(I)
  101 CONTINUE
C
      XDRA(102)=1.
      YDRA(102)=0.
C
C Define lines separating vertical boxes from each other.
C
      NDRA=102
C
      DO 102 I=3,99,2
        XDRA(NDRA+1)=1.E36
        YDRA(NDRA+1)=1.E36
        XDRA(NDRA+2)=XDRA(I)
        YDRA(NDRA+2)=0.
        XDRA(NDRA+3)=XDRA(I)
        YDRA(NDRA+3)=AMIN1(YDRA(I),YDRA(I+1))
        NDRA=NDRA+3
  102 CONTINUE
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Suppress the frame advance.
C
      CALL AGSETI ('FRAME.',2)
C
C Draw the graph, using EZXY.
C
      CALL EZXY (XDRA,YDRA,249,'EXAMPLE 12 (HISTOGRAM)$')
C
C Use the package SOFTFILL to fill the area defined by the
C data.
C
      CALL SFSETI ('AN',45)
      CALL SFSETR ('SP',.004)
      CALL SFWRLD (XDRA,YDRA,102,WORK,204,IWRK,204)
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
        DATA X / 2.718281828459045 /
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
