C
C	$Id: gcrsg.f,v 1.6 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
      IF (GSEGRT(1:4) .EQ. 'GSEG') THEN
C
C  Construct a temporary file name from the user id and process id.
C
        CALL GZGIDS(IPID,IUID)
        IPID = MOD(IPID,9999)
        IUID = MOD(IUID,9999)
        WRITE(SEGNAM(NUMSEG)(1:4),500)
        WRITE(SEGNAM(NUMSEG)(5:8),501) IUID
        WRITE(SEGNAM(NUMSEG)(9:12),501) IPID
        WRITE(SEGNAM(NUMSEG)(13:15),502) SGNA
  500   FORMAT('GSEG')
  501   FORMAT(I4)
  502   FORMAT('_',I2)
        DO 50 I=1,14
          IF(SEGNAM(NUMSEG)(I:I) .EQ. ' ') SEGNAM(NUMSEG)(I:I) = '0'
   50   CONTINUE
      ELSE
C
C  Construct the user-requested file name.
C
        LLEN = LEN(GSEGRT)
        DO 60 I=1,LLEN
        IF (GSEGRT(I:I).EQ.' ' .OR. GSEGRT(I:I).EQ.CHAR(0)) THEN
          ILEN = I-1
          GO TO 65
        ELSE
          SEGNAM(NUMSEG)(I:I) = GSEGRT(I:I)
        ENDIF
   60   CONTINUE
        ILEN = LLEN
   65   CONTINUE
        WRITE(SEGNAM(NUMSEG)(ILEN+1:ILEN+2),503) SGNA
        SEGNAM(NUMSEG)(ILEN+3:ILEN+3) = CHAR(0)
  503   FORMAT(I2)
        IF (SEGNAM(NUMSEG)(ILEN+1:ILEN+1) .EQ. ' ') THEN
          SEGNAM(NUMSEG)(ILEN+1:ILEN+1) = '0'
        ENDIF
      ENDIF
C
C  Initialize the segment transformation.
C
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
