C
C	$Id: grid.f,v 1.1.1.1 1992-04-17 22:31:18 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C C O D E   -   S U B R O U T I N E   G R I D
C-----------------------------------------------------------------------
C
      SUBROUTINE GRID (MJRX,MNRX,MJRY,MNRY)
        CALL Q8QST4 ('GRAPHX','GRIDAL','GRID','VERSION 01')
        CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,0,0,0,0.,0.)
        RETURN
      END
