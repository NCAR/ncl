C
C	$Id: ticks.f,v 1.1.1.1 1992-04-17 22:31:20 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C C O D E   -   S U B R O U T I N E   T I C K S
C-----------------------------------------------------------------------
C
      SUBROUTINE TICKS (LMJR,LMNR)
        CALL Q8QST4 ('GRAPHX','GRIDAL','TICKS','VERSION 01')
        CALL TICK4 (LMJR,LMNR,LMJR,LMNR)
        RETURN
      END
