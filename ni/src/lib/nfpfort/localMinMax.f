      SUBROUTINE DLOCALMN(X,NI,NJ,XMSG,LWRAP,XI,YJ,NINJ,LOCMIN,
     +     DELTA,NMIN,IER)
      DOUBLE PRECISION XVAL

C input
      INTEGER LWRAP,NI,NJ,NINJ
C input
      INTEGER XI(NINJ),YJ(NINJ)
C input
      DOUBLE PRECISION X(NI,NJ),XMSG,DELTA
C output
      INTEGER NMIN,IER
C output
      DOUBLE PRECISION LOCMIN(NINJ)
c***********************************************************************
c
c this routine looks at 9 points
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
c .   ni       - dimension size ["x-axis": eg longitude]
c .   nj       - dimension size ["y-axis": eg latitude ]
c .   xmsg     - value of missing points
c .   lwrap    - flag to include wraparound points
c .              if lwrap = 1 , if data are cyclic in x
c .              if lwrap = 0 , data are not cyclic
c .   xi       - x-axis coordinates [eg, longitudes]
c .              The fortran subscripts have been adjusted for NCL
c .   yj       - y-axis coordinates [eg, latitudes ]
c .              The fortran subscripts have been adjusted for NCL
c .   ninj     - max number of points  (ni*nj is overkill but safe)
c .   locmin   - 1D array dimensioned (ninj)
c .   delta    - surrounding points must be different from the
c .              central value to be considered a local minimum
c .              Normally: delta = 0.0
c .   nmin     - actual number of local minima detected
c .
c  notes:
c
c       1)  if a point or any of its neighbors is missing, check is
c           not implemented
c       2)  this routine does not check the edges, just the interior
c           except when "lwrap" is true.
c       3)  array has to have at least 3 points in each direction
c
c  usage:
c     program main
c     implicit none
c     integer lwrap, ni, nj, ninj
c     parameter (ni=128, nj=64, ninj=ni*nj)
c     real z(ni,nj), zmsg, locmin(ninj)
c     integer xi(ni), yj(nj)
c     integer ier, n, m, nmin
c
c     read () z
c     ier   = 0
c     zmsg  = -999.
c     lwrap = 1
c     delta = 0.0
c     call localmin (z,ni,nj,zmsg,lwrap,xi,yj
c    *               ,ninj,locmin,delta,nmin,ier)
c
c     if (nmin.gt.0) then
c         do n=1, nmin
c            write (*,"(2i4,f10.2)") xi(n),jy(n),locmin(n)
c         end do
c     else
c         write (*,"('sub localmin: no local min found)")
c     end if
c     end
c***********************************************************************

c determine if size of array is sufficient

      IER = 0
      IF (NI.LT.3 .OR. NJ.LT.3) THEN
          IER = 1
          WRITE (*,FMT='(/,'' sub localmn: error'',2i5)') NI,NJ
          RETURN
      END IF

c initialize to missing and zero

      NMIN = 0
      DO N = 1,NINJ
          LOCMIN(N) = XMSG
          XI(N) = -999
          YJ(N) = -999
      END DO

c are endpoints to be included?

      IF (LWRAP.EQ.1) THEN
          NIB = 1
          NIE = NI
      ELSE
          NIB = 2
          NIE = NI - 1
      END IF
      NJB = 2
      NJE = NJ - 1

c  main loop

      DO J = NJB,NJE
          DO I = NIB,NIE
              JM1 = J - 1
              JP1 = J + 1
              IM1 = I - 1
              IP1 = I + 1
              IF (IM1.LT.1) IM1 = NI
              IF (IP1.GT.NI) IP1 = 1

              IF (X(I,J).NE.XMSG .AND. X(IM1,JP1).NE.XMSG .AND.
     +            X(IM1,J).NE.XMSG .AND. X(IM1,JM1).NE.XMSG .AND.
     +            X(I,JM1).NE.XMSG .AND. X(IP1,JM1).NE.XMSG .AND.
     +            X(IP1,J).NE.XMSG .AND. X(IP1,JP1).NE.XMSG .AND.
     +            X(I,JP1).NE.XMSG) THEN

                  XVAL = X(I,J) + DELTA

                  IF (X(IM1,JP1).GT.XVAL .AND. X(IM1,J).GT.XVAL .AND.
     +                X(IM1,JM1).GT.XVAL .AND. X(I,JM1).GT.XVAL .AND.
     +                X(IP1,JM1).GT.XVAL .AND. X(IP1,J).GT.XVAL .AND.
     +                X(IP1,JP1).GT.XVAL .AND. X(I,JP1).GT.X(I,J)) THEN
                      NMIN = NMIN + 1
c                                  subtract 1 for NCL subscripts
                      XI(NMIN) = I - 1
                      YJ(NMIN) = J - 1
                      LOCMIN(NMIN) = X(I,J)
                  END IF
              END IF
          END DO
      END DO

      RETURN
      END
c ----------------------------------------------------------
      SUBROUTINE DLOCALMX(X,NI,NJ,XMSG,LWRAP,XI,YJ,NINJ,LOCMAX,
     +     DELTA,NMAX,IER)
      DOUBLE PRECISION XVAL

C input
      INTEGER LWRAP,NI,NJ,NINJ
C input
      INTEGER XI(NINJ),YJ(NINJ)
C input
      DOUBLE PRECISION X(NI,NJ),XMSG,DELTA
C output
      INTEGER NMAX,IER
C output
      DOUBLE PRECISION LOCMAX(NINJ)
c***********************************************************************
c  see documentation for localmn
c***********************************************************************

c determine if size of array is sufficient

      IER = 0
      IF (NI.LT.3 .OR. NJ.LT.3) THEN
          IER = 1
          WRITE (*,FMT='(/,'' sub localmx: error'',2i5)') NI,NJ
          RETURN
      END IF

c initialize to missing and zero

      NMAX = 0
      DO N = 1,NINJ
          LOCMAX(N) = XMSG
          XI(N) = -999
          YJ(N) = -999
      END DO


c are endpoints to be included?

      IF (LWRAP.EQ.1) THEN
          NIB = 1
          NIE = NI
      ELSE
          NIB = 2
          NIE = NI - 1
      END IF
      NJB = 2
      NJE = NJ - 1

c  main loop

      DO J = NJB,NJE
          DO I = NIB,NIE
              JM1 = J - 1
              JP1 = J + 1
              IM1 = I - 1
              IP1 = I + 1
              IF (IM1.LT.1) IM1 = NI
              IF (IP1.GT.NI) IP1 = 1

              IF (X(I,J).NE.XMSG .AND. X(IM1,JP1).NE.XMSG .AND.
     +            X(IM1,J).NE.XMSG .AND. X(IM1,JM1).NE.XMSG .AND.
     +            X(I,JM1).NE.XMSG .AND. X(IP1,JM1).NE.XMSG .AND.
     +            X(IP1,J).NE.XMSG .AND. X(IP1,JP1).NE.XMSG .AND.
     +            X(I,JP1).NE.XMSG) THEN

                  XVAL = X(I,J) - DELTA

                  IF (X(IM1,JP1).LT.XVAL .AND. X(IM1,J).LT.XVAL .AND.
     +                X(IM1,JM1).LT.XVAL .AND. X(I,JM1).LT.XVAL .AND.
     +                X(IP1,JM1).LT.XVAL .AND. X(IP1,J).LT.XVAL .AND.
     +                X(IP1,JP1).LT.XVAL .AND. X(I,JP1).LT.X(I,J)) THEN
                      NMAX = NMAX + 1
c                                  subtract 1 for NCL subscripts
                      XI(NMAX) = I - 1
                      YJ(NMAX) = J - 1
                      LOCMAX(NMAX) = X(I,J)
                  END IF
              END IF
          END DO
      END DO

      RETURN
      END
