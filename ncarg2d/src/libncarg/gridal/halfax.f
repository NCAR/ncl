C
C	$Id: halfax.f,v 1.1.1.1 1992-04-17 22:31:19 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C C O D E   -   S U B R O U T I N E   H A L F A X
C-----------------------------------------------------------------------
C
      SUBROUTINE HALFAX (MJRX,MNRX,MJRY,MNRY,XINT,YINT,IXLB,IYLB)
        CALL Q8QST4 ('GRAPHX','GRIDAL','HALFAX','VERSION 01')
        CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,IXLB,IYLB,10,XINT,YINT)
        RETURN
      END
