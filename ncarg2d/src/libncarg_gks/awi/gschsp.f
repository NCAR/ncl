C
C	$Id: gschsp.f,v 1.2 1993-01-09 02:02:19 fred Exp $
C
      SUBROUTINE GSCHSP (CHSP)
C
C  SET CHARACTER SPACING
C
      INTEGER ESCHSP
      PARAMETER (ESCHSP=29)
C
      include 'gkscom.h'
C
      REAL CHSP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESCHSP,IER)
      IF (IER .NE. 0) RETURN
C
C  Set the current character spacing in the GKS state list.
C
      CCHSP = CHSP
C
C  Invoke the workstation interface.
C
      FCODE = 32
      CONT  = 0
      CALL GZROI(0)
      RL1   = 1
      RL2   = 1
      RX(1) = CHSP
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESCHSP,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
