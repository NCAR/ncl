C
C $Id: tick43.f,v 1.6 2008-07-27 00:17:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TICK43 (MAGU,MINU,MAGV,MINV,MAGW,MINW)
      SAVE
      COMMON /TCK31/  TMAGU      ,TMINU      ,TMAGV      ,TMINV      ,
     1                TMAGW      ,TMINW
      TMAGU = MAGU
      TMINU = MINU
      TMAGV = MAGV
      TMINV = MINV
      TMAGW = MAGW
      TMINW = MINW
      RETURN
      END
