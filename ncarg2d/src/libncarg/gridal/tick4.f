C
C $Id: tick4.f,v 1.4 1994-03-17 17:27:59 kennison Exp $
C
      SUBROUTINE TICK4 (LMJX,LMNX,LMJY,LMNY)
C
C Declare the common block containing real and integer parameters.
C
        COMMON /GAREIN/ ICAX,ICLB,ICMJ,ICMN,ILTY,IORX,NCFX,NCFY,RCWX,
     +                  RCWY,RDCX,RDCY,RMJX,RMJY,RMNX,RMNY,RWAX,RWLB,
     +                  RWMJ,RWMN
        SAVE   /GAREIN/
C
C Declare the block data "routine" external.  This should force it to
C be loaded.
C
        EXTERNAL GABLDT
C
C Check for an uncleared prior error.
C
        IF (ICFELL('TICK4 - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer the arguments to GRIDAL's common block.
C
        RMJX=REAL(LMJX)
        RMNX=REAL(LMNX)
        RMJY=REAL(LMJY)
        RMNY=REAL(LMNY)
C
C Done.
C
      RETURN
C
      END
