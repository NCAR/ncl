C
C	$Id: gridl.f,v 1.1.1.1 1992-04-17 22:31:19 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C C O D E   -   S U B R O U T I N E   G R I D L
C-----------------------------------------------------------------------
C
      SUBROUTINE GRIDL (MJRX,MNRX,MJRY,MNRY)
        CALL Q8QST4 ('GRAPHX','GRIDAL','GRIDL','VERSION 01')
        CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,1,1,0,0.,0.)
        RETURN
      END
