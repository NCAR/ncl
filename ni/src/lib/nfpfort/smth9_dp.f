      SUBROUTINE DSMTH9(X,WRK,NI,NJ,P,Q,XMSG,LWRAP,IER)
      DOUBLE PRECISION PO4
      DOUBLE PRECISION QO4
      DOUBLE PRECISION TERM1
      DOUBLE PRECISION TERM2

      LOGICAL LWRAP
      INTEGER NI,NJ,IER
      DOUBLE PRECISION X(NI,NJ),WRK(NI,NJ),P,Q,XMSG
c***********************************************************************
c modified j. olson routine
c***********************************************************************
c
c this routine does 9-point smoothing using the equation:
c
c   f0 = f0 + (p/4)*(f2+f4+f6+f8-4*f0) + (q/4)*(f1+f3+f5+f7-4*f0)
c
c  where the grid is:
c
c      1-------------8---------------7
c      |             |               |
c      |             |               |
c      |             |               |
c      |             |               |
c      2-------------0---------------6
c      |             |               |
c      |             |               |
c      |             |               |
c      |             |               |
c      3-------------4---------------5
c
c   arguments :
c .
c .   x        - 2-d input/output array
c .   wrk      - 2-d work array
c .   ni       - dimension
c .   nj       - dimension
c .   p        - first  weight (suggested value of  0.50)
c .   q        - second weight (suggested value of  0.25)
c .   xmsg     - value of missing points
c .   lwrap    - logical flag to include wraparound points in smoothing
c .              if lwrap = .true. , smooth left and right endpoints
c .              if lwrap = .false., no smoothing
c .
c  notes:
c
c       1)  if a point or any of its neighbors is missing, the point is
c           not smoothed
c       2)  this routine does not smooth the edges, just the interior
C*PL*ERROR* Comment line too long
c           with the exception that the left and right edges are smoothed
c           when "lwrap" is true.
c       3)  array has to have at least 3 points in each direction
c       4)  smoothed results are returned via the original input array
c
c***********************************************************************

c determine if size of array is sufficient

      IER = 0
      IF (NI.LT.3 .OR. NJ.LT.3) THEN
          IER = 1
          WRITE (*,FMT='(/,'' sub smth9: error'',2i5)') NI,NJ
          RETURN
      END IF

      PO4 = P/4.D0
      QO4 = Q/4.D0

c set the wrk array to msg

      DO I = 1,NI
          DO J = 1,NJ
              WRK(I,J) = XMSG
          END DO
      END DO

c are endpoints to be smoothed?

      IF (LWRAP) THEN
          NIB = 1
          NIE = NI
      ELSE
          NIB = 2
          NIE = NI - 1
      END IF
      NJB = 2
      NJE = NJ - 1

c  smooth

      DO J = NJB,NJE
          DO I = NIB,NIE
              JM1 = J - 1
              JP1 = J + 1
              IM1 = I - 1
              IP1 = I + 1
              IF (IM1.LT.1) IM1 = NI
              IF (IP1.GT.NI) IP1 = 1

              IF (X(I,J).EQ.XMSG .OR. X(IM1,JP1).EQ.XMSG .OR.
     +            X(IM1,J).EQ.XMSG .OR. X(IM1,JM1).EQ.XMSG .OR.
     +            X(I,JM1).EQ.XMSG .OR. X(IP1,JM1).EQ.XMSG .OR.
     +            X(IP1,J).EQ.XMSG .OR. X(IP1,JP1).EQ.XMSG .OR.
     +            X(I,JP1).EQ.XMSG) THEN
C original (unsmoothed) value
                  WRK(I,J) = X(I,J)
              ELSE
                  TERM1 = PO4* (X(IM1,J)+X(I,JM1)+X(IP1,J)+X(I,JP1)-
     +                    4.D0*X(I,J))
                  TERM2 = QO4* (X(IM1,JP1)+X(IM1,JM1)+X(IP1,JM1)+
     +                    X(IP1,JP1)-4.D0*X(I,J))
                  WRK(I,J) = X(I,J) + TERM1 + TERM2
              END IF
          END DO
      END DO

c transfer back to original array

      DO J = NJB,NJE
          DO I = NIB,NIE
              X(I,J) = WRK(I,J)
          END DO
      END DO

      RETURN
      END
