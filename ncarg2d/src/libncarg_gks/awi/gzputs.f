C
C	$Id: gzputs.f,v 1.5 2008-07-27 00:21:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZPUTS(ST,IER)
C
C  This subroutine fills the string character variable of the 
C  workstation interface common block and invokes the workstation
C  interface subroutine (N is the length of string ST).
C  If an error is returned from the workstation interface,
C  IER is set to that error number and a return is executed.
C
C
      include 'gkscom.h'
C
      CHARACTER*(*) ST
      N = LEN(ST)
      J = (N-1)/80
      STRL1 = N
      IF (J .EQ. 0) THEN
C
C  Case where there is no continuation.
C
        CONT = 0
        STRL2 = N
        STR(1:N) = ST(1:N)
        CALL GZTOWK
        IF (RERR .NE. 0) THEN
          IER = RERR
          RETURN
        ENDIF
      ELSE
C
C  Case with contination.
C
        CONT = 1
        STRL2 = 80
C
C  Loop through parts with continuation flag set.
C
        DO 200 K=1,J
          KPNT = (K-1)*80
          STR(1:80) = ST(KPNT+1:KPNT+80+1)
          IF (K .GT. 1) THEN
            IL2 = 0
            RL2 = 0
            IC2 = 0
          ENDIF
          CALL GZTOWK
          IF (RERR .NE. 0) THEN
            IER = RERR
            RETURN
          ENDIF
  200   CONTINUE
C
C  Put out last part of string with continuation flag set to last.
C
        CONT = 0
        STRL2 = N-J*80
        KPNT = J*80
        STR(1:STRL2) = ST(KPNT+1:KPNT+STRL2+1)
        CALL GZTOWK
      ENDIF
C
      RETURN
      END
