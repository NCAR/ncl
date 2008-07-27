C
C $Id: gacolr.f,v 1.8 2008-07-27 00:17:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL GABLDT
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
