C
C	$Id: tick4.f,v 1.1.1.1 1992-04-17 22:31:20 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C C O D E   -   S U B R O U T I N E   T I C K 4
C-----------------------------------------------------------------------
C
      SUBROUTINE TICK4 (LMJX,LMNX,LMJY,LMNY)
C
C Declare the common block containing real or integer parameters.
C
        COMMON /GAREIN/ ICAX,ICLB,ICMJ,ICMN,ICWX,ICWY,IDCX,IDCY,IORX,
     +                  IMJX,IMJY,IMNX,IMNY,NCFX,NCFY
        SAVE   /GAREIN/
C
C Declare the block data "routine" external.  This should force it to
C be loaded.
C
        EXTERNAL GABLDT
C
C  The following is for gathering statistics on library use at NCAR.
C
        CALL Q8QST4 ('GRAPHX','GRIDAL','TICK4','VERSION 01')
C
C Transfer the arguments to GRIDAL's common block.
C
        IMJX=LMJX
        IMNX=LMNX
        IMJY=LMJY
        IMNY=LMNY
C
C Done.
C
      RETURN
C
      END
