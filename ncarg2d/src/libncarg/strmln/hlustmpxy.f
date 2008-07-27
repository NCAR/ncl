C
C       $Id: hlustmpxy.f,v 1.5 2008-07-27 00:17:27 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
