C
C $Id: tick43.f,v 1.2 1993-03-14 19:01:30 kennison Exp $
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
