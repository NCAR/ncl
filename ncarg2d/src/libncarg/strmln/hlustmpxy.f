C
C       $Id: hlustmpxy.f,v 1.2 1996-02-07 19:06:26 dbrown Exp $
C
C These routine stand between Streamline and the user call-back routines
C in stmpxy.f  When HLUs are not in use, this version of the routine
C gets loaded, so that STMPXY, STIMXY, and STMPTA get called. 
C When the HLUs are in use, they load another version; it either does 
C the appropriate thing for the purposes of the HLUs or calls STMPXY,
C STIMXY, and STMPTA as needed.
C
      SUBROUTINE HLUSTMPXY(XDA,YDA,XUS,YUS,IST)
C
      CALL STMPXY(XDA,YDA,XUS,YUS,IST)
C
      RETURN
C
      END
C
C -------------------------------------------------------------
C
      SUBROUTINE HLUSTIMXY(XUS,YUS,XDA,YDA,IST)
C
      CALL STIMXY(XUS,YUS,XDA,YDA,IST)
C
      RETURN
C
      END
C
C -------------------------------------------------------------
C
      SUBROUTINE HLUSTMPTA(XDA,YDA,XUS,YUS,XND,YND,DU,DV,TA,IST)
C
      CALL STMPTA(XDA,YDA,XUS,YUS,XND,YND,DU,DV,TA,IST)
C
      RETURN
C
      END
