C
C	$Id: labmod.f,v 1.1.1.1 1992-04-17 22:31:19 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C C O D E   -   S U B R O U T I N E   L A B M O D
C-----------------------------------------------------------------------
C
      SUBROUTINE LABMOD (FMTX,FMTY,NUMX,NUMY,ISZX,ISZY,IXDC,IYDC,IXOR)
C
        CHARACTER*(*) FMTX,FMTY
C
C Declare the common block containing real or integer parameters.
C
        COMMON /GAREIN/ ICAX,ICLB,ICMJ,ICMN,ICWX,ICWY,IDCX,IDCY,IORX,
     +                  IMJX,IMJY,IMNX,IMNY,NCFX,NCFY
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
C  The following is for gathering statistics on library use at NCAR.
C
        CALL Q8QST4 ('GRAPHX','GRIDAL','LABMOD','VERSION 01')
C
C Transfer arguments to GRIDAL's common blocks.
C
        FNLX=FMTX
        FNLY=FMTY
        NCFX=NUMX
        NCFY=NUMY
        ICWX=ISZX
        ICWY=ISZY
        IDCX=IXDC
        IDCY=IYDC
        IORX=IXOR
C
C Done.
C
      RETURN
C
      END
