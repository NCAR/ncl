C
C	$Id: agex03.f,v 1.2 1994-07-08 16:27:20 stautler Exp $
C
      PROGRAM EXMPL3
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
C
C Define the data array.
C
      REAL YDRA(100,2)
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data array.
C
      DO 101 I=1,100
        YDRA(I,1)=COS(3.14159265358979*FLOAT(I)/25.)*
     +                                    FLOAT(I)**2
        YDRA(I,2)=COS(3.14159265358979*FLOAT(I)/25.)*
     +                            10.**(.04*FLOAT(I))
  101 CONTINUE
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Draw the graph, using EZMY.
C
      CALL EZMY (YDRA,100,2,100,'EXAMPLE 3 (EZMY)$')
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
