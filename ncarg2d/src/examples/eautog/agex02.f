C
C	$Id: agex02.f,v 1.2 1994-07-08 16:27:19 stautler Exp $
C
      PROGRAM EXMPL2
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
C
C Define the data arrays.
C
      REAL XDRA(4001),YDRA(4001)
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data arrays.
C
      DO 101 I=1,4001
        THETA=.0015707963267949*FLOAT(I-1)
        RHO=SIN(2.*THETA)+.05*SIN(64.*THETA)
        XDRA(I)=RHO*COS(THETA)
        YDRA(I)=RHO*SIN(THETA)
  101 CONTINUE
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Draw the graph, using EZXY.
C
      CALL EZXY (XDRA,YDRA,4001,'EXAMPLE 2 (EZXY)$')
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
