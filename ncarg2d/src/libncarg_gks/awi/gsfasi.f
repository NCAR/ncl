C
C	$Id: gsfasi.f,v 1.2 1993-01-09 02:02:45 fred Exp $
C
      SUBROUTINE GSFASI (STYLI)
C
C  SET FILL AREA STYLE INDEX
C
      INTEGER ESFASI
      PARAMETER (ESFASI=37)
C
      include 'gkscom.h'
C
      INTEGER STYLI
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESFASI,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the index is valid.
C
      IF (STYLI .LE. 0) THEN
        ERS = 1
        CALL GERHND(84,ESFASI,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current index in the GKS state list.
C
      CFASI = STYLI
C
C  Invoke the workstation interface.
C
      FCODE = 39
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = STYLI
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESFASI,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
