C
C	$Id: gupasf.f,v 1.3 1993-03-19 01:28:25 fred Exp $
C
        SUBROUTINE GUPASF
C
C  Update aspect source flag context.
C
      include 'gksin.h'
      include 'g01arq.h'
      include 'g01ast.h'
      include 'g01adc.h'
C
      INTEGER  I
C
C  Compute change flags for each GKS ASF;  compute aggregate.
C  The CGM values for ASFs are reversed from those in GKS, so
C  we perform that complement here.
C
      ANYASF = .FALSE.
      DO 435 I=1,NGKASF
        MRASF(I) = 1-ID(I)
        ASFCHG(I) = MRASF(I).NE.MSASF(I)
        ANYASF = ANYASF .OR. ASFCHG(I)
  435 CONTINUE
C
C  Compute polyline aggregrate variable.
C  (Note that the logic relies on ASF pointers being a 
C   contiguous sequence).       
C
      DO 436 I=IALTYP,IAPLCI
        AGPEND(1) = AGPEND(1) .OR. ASFCHG(I)
  436 CONTINUE
C
C  Compute polymarker aggregrate variables.
C  (Note that the logic relies on ASF pointers being a
C   contiguous sequence).
C
      DO 437 I=IAMTYP,IAPMCI
        AGPEND(2) = AGPEND(2) .OR. ASFCHG(I)
  437 CONTINUE
C
C  Compute text aggregrate variables.
C  (Note that the logic relies on ASF pointers being a
C   contiguous sequence).
C
      DO 438 I=IATXFP,IATXCI
        AGPEND(3) = AGPEND(3) .OR. ASFCHG(I)
  438 CONTINUE
C
C  Compute fill area aggregrate variables.
C  (Note that the logic relies on ASF pointers being a
C   contiguous sequence).
C
      DO 439 I=IAFAIS,IAFACI
        AGPEND(4) = AGPEND(4) .OR. ASFCHG(I)
  439 CONTINUE
C
      RETURN
      END
