C
C	$Id: perim.f,v 1.1.1.1 1992-04-17 22:31:19 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C C O D E   -   S U B R O U T I N E   P E R I M
C-----------------------------------------------------------------------
C
      SUBROUTINE PERIM (MJRX,MNRX,MJRY,MNRY)
        CALL Q8QST4 ('GRAPHX','GRIDAL','PERIM','VERSION 01')
        CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,0,0,5,0.,0.)
        RETURN
      END
