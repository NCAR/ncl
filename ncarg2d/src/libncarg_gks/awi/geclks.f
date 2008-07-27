C
C	$Id: geclks.f,v 1.5 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GECLKS
C
C  EMERGENCY CLOSE GKS
C
      include 'gkscom.h'
C
C  Update all open workstations.
C
      IF (OPS .GE. GWSOP) THEN
        IF (NOPWK .NE. 0) THEN
          DO 200 I=1,NOPWK
            CALL GUWK(SOPWK(I),1)
  200     CONTINUE
        ENDIF
      ENDIF
C
C  Close the open segment if in state GSGOP.
C
      IF (OPS .EQ. GSGOP) THEN
        CALL GQOPSG(IER,ISGNE)
        IF (IER .EQ. 0) THEN
          CALL GCLSG
        ENDIF
      ENDIF
C
C  Deactivate all active workstations.
C
      IF (OPS .EQ. GWSAC) THEN
        IF (NACWK .NE. 0) THEN
          NACWKT = NACWK
          DO 201 I=1,NACWKT
            CALL GDAWK(SACWK(NACWK))
  201     CONTINUE
        ENDIF
      ENDIF
C
C  Close all open workstations.
C
      IF (OPS .GE. GWSOP) THEN
        IF (NOPWK .NE. 0) THEN
          NOPWKT = NOPWK
          DO 202 I=1,NOPWKT
            CALL GCLWK(SOPWK(NOPWK))
  202     CONTINUE
        ENDIF
      ENDIF
C
C  Mark GKS closed.
C
      OPS = GGKCL
C
      RETURN
      END
