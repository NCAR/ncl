C
C $Id: labmod.f,v 1.4 1994-03-17 17:27:54 kennison Exp $
C
      SUBROUTINE LABMOD (FMTX,FMTY,NUMX,NUMY,ISZX,ISZY,IXDC,IYDC,IXOR)
C
        CHARACTER*(*) FMTX,FMTY
C
C Declare the common block containing real and integer parameters.
C
        COMMON /GAREIN/ ICAX,ICLB,ICMJ,ICMN,ILTY,IORX,NCFX,NCFY,RCWX,
     +                  RCWY,RDCX,RDCY,RMJX,RMJY,RMNX,RMNY,RWAX,RWLB,
     +                  RWMJ,RWMN
        SAVE   /GAREIN/
C
C Declare the common block containing character parameters.
C
        COMMON /GACHAR/ FNLX,FNLY
        CHARACTER*10    FNLX,FNLY
        SAVE   /GACHAR/
C
C Declare the block data "routine" external.  This should force it to
C be loaded.
C
        EXTERNAL GABLDT
C
C Check for an uncleared prior error.
C
        IF (ICFELL('LABMOD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer arguments to GRIDAL's common blocks.
C
        FNLX=FMTX
        FNLY=FMTY
        NCFX=NUMX
        NCFY=NUMY
        RCWX=REAL(ISZX)
        RCWY=REAL(ISZY)
        RDCX=REAL(IXDC)
        RDCY=REAL(IYDC)
        IORX=IXOR
C
C Done.
C
      RETURN
C
      END
