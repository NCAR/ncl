C
C	$Id: agex04.f,v 1.3 1995-06-14 13:56:05 haley Exp $
C
      PROGRAM EXMPL4
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
C
C Define the data arrays.
C
      REAL XDRA(201),YDRA(201,10)
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data arrays.
C
      DO 102 I=1,201
        XDRA(I)=-1.+.02*FLOAT(I-1)
        IF (I.GT.101) XDRA(I)=2.-XDRA(I)
        DO 101 J=1,10
          YDRA(I,J)=FLOAT(J)*
     +    SQRT(1.000000000001-XDRA(I)**2)/10.
          IF (I.GT.101) YDRA(I,J)=-YDRA(I,J)
  101   CONTINUE
  102 CONTINUE
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Draw the graph, using EZMXY.
C
      CALL EZMXY (XDRA,YDRA,201,10,201,'EXAMPLE 4 (EZMXY)$')
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
