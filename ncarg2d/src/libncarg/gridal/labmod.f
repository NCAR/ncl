C
C $Id: labmod.f,v 1.8 2008-07-27 00:17:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL GABLDT
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
