C
C $Id: sfsort.f,v 1.2 1994-03-17 20:58:53 kennison Exp $
C
      SUBROUTINE SFSORT (RVL,NVL,IOR,IPR)
C
      DIMENSION RVL(NVL),IPR(NVL)
C
C Given an array of NVL reals in an array RVL and an order flag
C IOR, this routine returns a permutation vector IPR such that, for
C every I and J such that 1.LE.I.LE.J.LE.NVL, if IOR is zero, then
C RVL(IPR(I)).LE.RVL(IPR(J)), else RVL(IPR(I)).GE.RVL(IPR(J)).
C
      DO 10001 I=1,NVL
      IPR(I)=I
10001 CONTINUE
C
      K=0
C
10002 CONTINUE
      IF (.NOT.(3*K+1.LT.NVL)) GO TO 10003
      K=3*K+1
      GO TO 10002
10003 CONTINUE
C
      IF (.NOT.(IOR.EQ.0)) GO TO 10004
C
10005 CONTINUE
      IF (.NOT.(K.GT.0)) GO TO 10006
C
      DO 10007 I=1,NVL-K
C
      J=I
C
10008 CONTINUE
      IF (RVL(IPR(J)).LE.RVL(IPR(J+K))) GO TO 10009
      ITM=IPR(J)
      IPR(J)=IPR(J+K)
      IPR(J+K)=ITM
      J=J-K
      IF (J.LT.1) GO TO 10009
      GO TO 10008
10009 CONTINUE
C
10007 CONTINUE
C
      K=(K-1)/3
C
      GO TO 10005
10006 CONTINUE
C
      GO TO 10010
10004 CONTINUE
C
10011 CONTINUE
      IF (.NOT.(K.GT.0)) GO TO 10012
C
      DO 10013 I=1,NVL-K
C
      J=I
C
10014 CONTINUE
      IF (RVL(IPR(J)).GE.RVL(IPR(J+K))) GO TO 10015
      ITM=IPR(J)
      IPR(J)=IPR(J+K)
      IPR(J+K)=ITM
      J=J-K
      IF (J.LT.1) GO TO 10015
      GO TO 10014
10015 CONTINUE
C
10013 CONTINUE
C
      K=(K-1)/3
C
      GO TO 10011
10012 CONTINUE
C
10010 CONTINUE
C
C Done.
C
      RETURN
C
      END
