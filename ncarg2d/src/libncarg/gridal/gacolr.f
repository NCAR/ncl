C
C $Id: gacolr.f,v 1.4 1994-03-17 17:27:32 kennison Exp $
C
      SUBROUTINE GACOLR (KAXS,KLBL,KMJT,KMNT)
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
        IF (ICFELL('GACOLR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer the arguments to GRIDAL's common block.
C
        ICAX=KAXS
        ICLB=KLBL
        ICMJ=KMJT
        ICMN=KMNT
C
C Done.
C
        RETURN
C
      END
