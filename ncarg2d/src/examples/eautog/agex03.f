C
C	$Id: agex03.f,v 1.1.1.1 1992-04-17 22:33:08 ncargd Exp $
C
      PROGRAM EXMPL3
C
C Define the data array.
C
      REAL YDRA(100,2)
C
C Initialize GKS.
C
      CALL OPNGKS
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
      CALL CLSGKS
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
