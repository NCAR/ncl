C
C	$Id: gcrsg.f,v 1.2 1993-01-09 01:58:06 fred Exp $
C
      SUBROUTINE GCRSG(SGNA)
C
C  CREATE SEGMENT
C
      INTEGER ECRSG
      PARAMETER (ECRSG=56)
C
      include 'gkscom.h'
C
      INTEGER SGNA
C
C  This subroutine is here solely as support for the SPPS GFLASn
C  entries.  Full segmentation is not a part of the NCAR GKS
C  package at this time.  The NCAR package is non-standard to the
C  extent that certain segmentation functions are supported, but
C  not all level 1 functions are supported.  This subroutine should
C  be considered a user entry point only by way of the GFLASn
C  calls--it should never be called directly by the user.
C
C
C  Check if GKS iS in the proper state.
C
      CALL GZCKST(3,ECRSG,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the segment name is valid.
C
      IF (SGNA.LT.0 .OR. SGNA.GT.99) THEN
        ERS = 1
        CALL GERHND(120,ECRSG,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if the segment name is already in use.
C
      DO 200 I=1,NUMSEG
        IF (SEGS(I) .EQ. SGNA) THEN
          ERS = 1
          CALL GERHND(121,ECRSG,ERF)
          ERS = 0
          RETURN
        ENDIF
  200 CONTINUE
C
C  Check if WISS is active.  Since NCAR GKS stores segments only 
C  in WISS, no segment is stored unless WISS is active.
C
      DO 10 I=1,NACWK
        CALL GQWKC(SACWK(I),IER,ICONID,ITYP)
        IF (ITYP .EQ. GWSS) THEN
          IWKIDX = I
          GO TO 20
        ENDIF
   10 CONTINUE
      ERS = 1
      CALL GERHND(-103,ECRSG,ERF)
      ERS = 0
      RETURN
   20 CONTINUE
C
C  Set GKS state to segment open.
C
      OPS = GSGOP
C
C  Add segment name to those in use; initialize the segment 
C  transformation to the identity.
C
      NUMSEG = NUMSEG+1
      SEGS(NUMSEG) = SGNA
      CURSEG = SGNA
      SEGNAM(NUMSEG) = ' '
      WRITE(SEGNAM(NUMSEG)(1:6),500) SGNA
      IF(SGNA .LT. 10) SEGNAM(NUMSEG)(5:5) = '0'
  500 FORMAT('GNFB',I2)
      DO 30 I=1,2
        DO 40 J=1,3
          SEGT(NUMSEG,I,J) = 0.
   40   CONTINUE
   30 CONTINUE
      SEGT(NUMSEG,1,1) = 1.
      SEGT(NUMSEG,2,2) = 1.
C
C  Invoke workstation interface, set CUFLAG to force consideration of
C  WISS only.
C
      CUFLAG = SACWK(IWKIDX)
      FCODE = 80
      CONT  = 0
      CALL GZROI(0)
      IL1 = 2
      IL2 = 2
      ID(1) = WCONID
      ID(2) = SGNA
      STR = SEGNAM(NUMSEG)
      CALL GZTOWK
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,ECRSG,ERF)
        ERS = 0
      ENDIF
      CUFLAG = -1
C
      RETURN
      END
