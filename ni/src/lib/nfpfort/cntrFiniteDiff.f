      SUBROUTINE DCFINDIF(Q,R,NPTS,QMSG,RMSG,ICYC,IEND,QQ,RR,NPTS1,
     +                    DQDR,IER)

c perform a centered finite difference:
c .   dq/dr ==> partial(q)/partial(r) ==>
c .             dqdr(n) = (q(n+1)-q(n-1))/(r(n+1)-r(n-1))

c NCL: dqdr = center_finite_diff (q,r,rCyclic,opt)
c .           rCyclic = True/False
c .           opt     = unused
c .           wrapper: if rCyclic=True   set iend=0
c .           wrapper: if rCyclic=False  set iend=1

c Nomenclature:
c .   q     - input vector [variable to be differenced]
c .   r     - input vector [spatial or temporal coordinates]
c .   npts  - number of points
c .   qmsg  - msg value for q
c .   rmsg  - msg value for r [currently not used]
c .   icyc  - =1 if data are cyclic in the "r" coordinate
c .   iend  - only used if data are not cyclic in the "r" coordinate
c .           =1 if the end points of are to be approximated
c .              via a one-sided difference scheme
c .           =0 if the end points of are to be returned as rmsg
c .   dqdr  - result
c .   ier   - error code

      IMPLICIT NONE
c                                     input arguments
      INTEGER NPTS,NPTS1,ICYC,IEND
      DOUBLE PRECISION Q(NPTS),R(NPTS),QMSG,RMSG
c                                     output
      INTEGER IER
      DOUBLE PRECISION DQDR(NPTS)
      DOUBLE PRECISION QQ(0:NPTS1),RR(0:NPTS1)
      INTEGER N,NSTRT,NLAST

      IER = 0
c                                    not enough points
      IF (NPTS.LE.2) THEN
          IER = 1
      END IF
c                                    conflicting options
      IF (ICYC.EQ.1 .AND. IEND.NE.0) THEN
          IER = 2
      END IF
      IF (IER.NE.0) RETURN
c                                    initialize
      DO N = 1,NPTS
          DQDR(N) = QMSG
          QQ(N) = Q(N)
          RR(N) = R(N)
      END DO
c                                    add points if cyclic
      NSTRT = 2
      NLAST = NPTS - 1
      IF (ICYC.EQ.1) THEN
          NSTRT = 1
          NLAST = NPTS
          QQ(0) = QQ(NPTS)
          RR(0) = RR(1) - (RR(2)-RR(1))
          QQ(NPTS+1) = QQ(1)
          RR(NPTS+1) = RR(NPTS) + (RR(NPTS)-RR(NPTS-1))
      END IF
c                                    centered finite differences
      DO N = NSTRT,NLAST
          IF (QQ(N-1).NE.QMSG .AND. QQ(N+1).NE.QMSG) THEN
              DQDR(N) = (QQ(N+1)-QQ(N-1))/ (RR(N+1)-RR(N-1))
          END IF
      END DO
c                                    if not cyclic
c                                    how to treat end-points
c                                    3 Mar 2006 [added qmsg checks]
      IF (ICYC.NE.1 .AND. IEND.EQ.1) THEN
         IF (QQ(1).NE.QMSG .AND. QQ(2).NE.QMSG) THEN
             DQDR(1) = (QQ(2)-QQ(1))/ (RR(2)-RR(1))
         END IF
         IF (QQ(NPTS).NE.QMSG .AND. QQ(NPTS-1).NE.QMSG) THEN
             DQDR(NPTS) = (QQ(NPTS)-QQ(NPTS-1))/ (RR(NPTS)-RR(NPTS-1))
         END IF
      END IF

      RETURN
      END
