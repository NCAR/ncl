C
C $Id: tick4.f,v 1.8 2008-07-27 00:17:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL GABLDT
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
