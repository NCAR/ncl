C
C	$Id: gspa.f,v 1.2 1993-01-09 02:02:59 fred Exp $
C
      SUBROUTINE GSPA (SZX,SZY)
C
C  SET PATTERN SIZE
C
C  Currently this subroutine does nothing since ctrans does not support
C  the CGM PATTERN SIZE element.
C
      INTEGER ESPA
      PARAMETER (ESPA=39)
C
      include 'gkscom.h'
C
      REAL SZX,SZY
C
C  Remove this RETURN and uncomment appropriate lines to 
C  activate the subroutine.
C	
      RETURN
C
C  Check if GKS is in the proper state.
C
C     CALL GZCKST(8,ESPA,IER)
C     IF (IER .NE. 0) RETURN
C
C  Check that the size specifications are valid.
C
C     IF (SZX.LE.0. .OR. SZY.LE.0.) THEN
C       ERS = 1
C       CALL GERHND(87,ESPA,ERF)
C       ERS = 0
C       RETURN
C     ENDIF
C
C  Set the current pattern size variables
C  in the gks state list (these remain in world coordinates).
C
C     CPA(1) = SZX
C     CPA(2) = SZY
C
C  Transform pattern sizes to NDC space.
C
C     CALL GZW2NX(1,SZX,SZXN)
C     CALL GZW2NY(1,SZY,SZYN)
C
C  Invoke the workstation interface.
C
C     FCODE = 41
C     CONT  = 0
C     CALL GZROI(0)
C     RL1   = 1
C     RL2   = 1
C     RX(1) = SZXN
C     RY(1) = SZYN
C     CALL GZTOWK
C     IF (RERR .NE. 0) THEN
C       ERS = 1
C       CALL GERHND(RERR,ESPA,ERF)
C       ERS = 0
C     ENDIF
C
C     RETURN
      END
