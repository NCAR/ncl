C
C	$Id: g01clw.f,v 1.2 1993-01-09 02:05:45 fred Exp $
C
      SUBROUTINE G01CLW
C
C CLEAR WORKSTATION
C
      include 'g01prm.h'
      include 'g01arq.h'
      include 'gksin.h'
      include 'g01wsl.h'
      include 'g01io.h'
      include 'g01ins.h'
      include 'g01ast.h'
      include 'g01adf.h'
      include 'g01opc.h'
      include 'gksenu.h'
C
      INTEGER  NBYTES
C
      IF (MDEMPT.EQ.GNEMPT .OR. ID(2).EQ.GALWAY) THEN
C
C  Put out END PICTURE element (CLASS, ID, LENGTH).
C
         NBYTES = 0
         CALL GPUTNI (CLENDP, IDENDP, NBYTES, RERR)
C
C  Flush buffer.
C
         CALL G01FLB (RERR)
         IF (RERR.NE.0)  GO TO 77
C
C  Reset all attribute deferral control variables.
C
         CALL GUPDVA
      ENDIF
C
C  Set WSL entry "DISPLAY SURFACE EMPTY" to "EMPTY"
C
      MDEMPT = GEMPTY
C
C  Set WSL entry "NEW FRAME ACTION NECESSARY AT UPDATE" to "NO".
C
      MNFRAM = GNO
C
C  If update state is pending, set current window and viewport
C  entries to those requested.
C
      IF (MTUS .EQ. GPEND) THEN
         CWINDO(1) = RWINDO(1)
         CWINDO(2) = RWINDO(2)
         CWINDO(3) = RWINDO(3)
         CWINDO(4) = RWINDO(4)
         CWKVP(1) = RWKVP(1)
         CWKVP(2) = RWKVP(2)
         CWKVP(3) = RWKVP(3)
         CWKVP(4) = RWKVP(4)
C
C  Set workstation update state to "NOT PENDING"
C
         MTUS = GNPEND
      ENDIF
C
C Clear picture name.
C
      MPNAME = ' '
C
 77   RETURN
      END
