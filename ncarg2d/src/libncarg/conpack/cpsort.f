C
C $Id: cpsort.f,v 1.5 1995-04-26 22:45:07 kennison Exp $
C
      SUBROUTINE CPSORT (RVAL,NVAL,IPER)
C
      DIMENSION RVAL(NVAL),IPER(NVAL)
C
C Given an array of NVAL reals in an array RVAL, this routine returns a
C permutation vector IPER such that, given I and J, 1.LE.I.LE.J.LE.NVAL,
C RVAL(IPER(I)).LE.RVAL(IPER(J)).
C
C A Shell sort is used.  Details of the algorithm may be found in the
C book "Algorithms" by Robert Sedgewick.
C
C Note:  Fred Clare wrote the original version of this routine.  I have
C adapted it for use in CONPACK; among other things, the error checking
C has been been removed because the calling routine does it.  (DJK)
C
      DO 10001 I=1,NVAL
        IPER(I)=I
10001 CONTINUE
C
      K=0
C
10002 CONTINUE
      IF (.NOT.(3*K+1.LT.NVAL)) GO TO 10003
        K=3*K+1
      GO TO 10002
10003 CONTINUE
C
10004 CONTINUE
      IF (.NOT.(K.GT.0)) GO TO 10005
C
        DO 10006 I=1,NVAL-K
C
          J=I
C
10007     CONTINUE
            IF (RVAL(IPER(J)).LE.RVAL(IPER(J+K))) GO TO 10008
            ITMP=IPER(J)
            IPER(J)=IPER(J+K)
            IPER(J+K)=ITMP
            J=J-K
            IF (J.LT.1) GO TO 10008
          GO TO 10007
10008     CONTINUE
C
10006   CONTINUE
C
        K=(K-1)/3
C
      GO TO 10004
10005 CONTINUE
C
C Done.
C
      RETURN
C
      END
