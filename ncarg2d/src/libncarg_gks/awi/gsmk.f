C
C	$Id: gsmk.f,v 1.2 1993-01-09 02:02:55 fred Exp $
C
      SUBROUTINE GSMK (MTYPE)
C
C  SET MARKERTYPE
C
      INTEGER ESMK
      PARAMETER (ESMK=23)
C
      include 'gkscom.h'
C
      INTEGER MTYPE
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESMK,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the marker type is positive.
C
      IF (MTYPE.LE.0) THEN
        ERS = 1
        CALL GERHND(69,ESMK,ERF)
        ERS = 0
      RETURN
      ENDIF
C
C  Set the current marker type in the GKS state list.
C
      CMK = MTYPE
C
C  Invoke the workstation interface.
C
      FCODE = 26
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = MTYPE
      CALL GZTOWK
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,ESMK,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
