C
C	$Id: gopks.f,v 1.4 1996-10-07 19:14:15 fred Exp $
C
      SUBROUTINE GOPKS(ERRFIL,BUFA)
C
C  OPEN GKS
C
C  Force load of all BLOCKDATAs.
C
      EXTERNAL GKSBD,G01BKD,GWIBKD,GSEGDT
      INTEGER EOPKS
      PARAMETER (EOPKS=0)
      INTEGER ERRFIL,BUFA
C
      include 'gkscom.h'
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(1,EOPKS,IER)
      IF (IER .NE. 0) RETURN
C
C  Initialize the error state list and local names.
C
      CALL GZINES
C
C  Specify the error file in the GKS error state list.
C
      ERF = ERRFIL
C
C  Initialize the GKS state list.
C
      CALL GZINSL
C
C  Set the GKS operating state to GKSOP.
C
      OPS = GGKOP
C
      RETURN
      END
