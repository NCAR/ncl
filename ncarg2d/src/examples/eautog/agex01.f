C
C	$Id: agex01.f,v 1.2 1994-07-08 16:27:18 stautler Exp $
C
      PROGRAM EXMPL1
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
C
C Define the data array.
C
      REAL YDRA(1001)
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data array.
C
      DO 101 I=1,1001
        X=FLOAT(I)/20.
        YDRA(I)=10.*(X-1.)*(X-11.)*(X-21.)*(X-31.)*(X-41.)*
     +                             (X-51.)+2.E7*(FRAN()-.5)
  101 CONTINUE
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Draw the graph, using EZY.
C
      CALL EZY (YDRA,1001,'EXAMPLE 1 (EZY)$')
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
