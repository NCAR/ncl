C
C	$Id: gstxfp.f,v 1.2 1993-01-09 02:03:23 fred Exp $
C
      SUBROUTINE GSTXFP (FONT,PREC)
C
C  SET TEXT FONT AND PRECISION
C
      INTEGER ESTXFP
      PARAMETER (ESTXFP=27)
C
      include 'gkscom.h'
C
      INTEGER FONT,PREC
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESTXFP,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the font is not zero.
C
      IF (FONT .EQ. 0) THEN
        ERS = 1
        CALL GERHND(75,ESTXFP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check that the precision is valid.
C
      IF (PREC.LT.0 .OR. PREC.GT.2) THEN
        ERS = 1
        CALL GERHND(2000,ESTXFP,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current font and precision variables in the GKS state list.
C
      CTXFP(1) = FONT
      CTXFP(2) = PREC
C
C  Invoke the workstation interface.
C
      FCODE = 30
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = FONT
      ID(2) = PREC
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESTXFP,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
