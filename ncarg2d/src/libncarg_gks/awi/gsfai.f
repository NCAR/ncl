C
C	$Id: gsfai.f,v 1.2 1993-01-09 02:02:40 fred Exp $
C
      SUBROUTINE GSFAI (INDEX)
C
C  SET FILL AREA INDEX
C
C  Currently this subroutine does nothing since ctrans does not support
C  the CGM FILL BUNDLE INDEX element.
C
      INTEGER ESFAI
      PARAMETER (ESFAI=35)
C
      include 'gkscom.h'
C
      INTEGER INDEX
C
C  Remove this RETURN and uncomment the appropriate lines
C  to activate the subroutine.
C
      RETURN
C
C  Check if GKS is in the proper state.
C
C     CALL GZCKST(8,ESFAI,IER)
C     IF (IER .NE. 0) RETURN
C
C  Check that the index is positive (0A specific).
C
C     IF (INDEX.LE.0 .OR. INDEX.GT.5) THEN
C       ERS = 1
C       CALL GERHND(80,ESFAI,ERF)
C       ERS = 0
C       RETURN
C     ENDIF
C
C  Set the current index in the GKS state list.
C
C     CFAI = INDEX
C
C  Invoke the workstation interface.
C
C     FCODE = 37
C     CONT  = 0
C     CALL GZROI(0)
C     IL1   = 1
C     IL2   = 1
C     ID(1) = INDEX
C     CALL GZTOWK
C     IF (RERR.NE.0 .AND. RERR.NE.-109) THEN
C       ERS = 1
C       CALL GERHND(RERR,ESFAI,ERF)
C       ERS = 0
C     ENDIF
C
C     RETURN
      END
