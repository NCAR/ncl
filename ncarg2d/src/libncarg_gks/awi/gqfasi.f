C
C	$Id: gqfasi.f,v 1.2 1993-01-09 02:00:18 fred Exp $
C
      SUBROUTINE GQFASI(ERRIND,STYLI)
C
C  INQUIRE FILL AREA STYLE INDEX
C
      include 'gkscom.h'
C
      INTEGER ERRIND,STYLI
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .EQ. 0) THEN
        STYLI = CFASI
      ELSE
        STYLI = -1
      ENDIF
C
      RETURN
      END
