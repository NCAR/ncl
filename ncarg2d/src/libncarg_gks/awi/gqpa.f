C
C	$Id: gqpa.f,v 1.2 1993-01-09 02:00:51 fred Exp $
C
      SUBROUTINE GQPA(ERRIND,PWX,PWY,PHX,PHY)
C
C  INQUIRE PATTERN SIZE
C
      include 'gkscom.h'
C
      INTEGER ERRIND
      REAL    PWX,PWY,PHX,PHY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND.EQ.0) THEN
C
C  Pattern width and height vectors will always be along the 
C  coordinate axes since no metafile interpretation functions 
C  are in the package.
C
        PWX = CPA(1)
        PWY = 0.
        PHX = 0.
        PHY = CPA(2)
      ELSE
        PWX = -1.
        PWY = -1.
        PHX = -1.
        PHY = -1.
      ENDIF
C
      RETURN
      END
