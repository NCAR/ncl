C
C	$Id: tick4.f,v 1.3 1992-09-04 20:41:02 ncargd Exp $
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
