C NCLFORTSTART
      SUBROUTINE DRUNAVE(X,NPTS,NAVE,KOPT,XMSG,WORK,LWORK,IER)

C NCL function: x = runave (x,nave,kopt)

      INTEGER NPTS,NAVE,KOPT,IER
      DOUBLE PRECISION X(NPTS),XMSG
C NCLEND

      INTEGER LWORK
      DOUBLE PRECISION WORK(LWORK)

      NHALF = NAVE/2
      LWORK = NPTS + 2*NHALF

      CALL DRUNAVX77(X,NPTS,NAVE,KOPT,XMSG,WORK,LWORK,IER)

C NCL return (runave[x])
      RETURN
      END
C ---------------------------------------------------------
      SUBROUTINE DRUNAVX77(X,NPTS,NAVE,KOPT,XMSG,WORK,LW,IER)
      DOUBLE PRECISION XMSG
      DOUBLE PRECISION WGT
      DOUBLE PRECISION SUM

c same as runavx: slightly faster but larger memory for work

c this routine will perform a running average on x
c .   the end points may be treated in three different ways

c nomenclature:
c .   x         - on input the series to be smoothed
c .               on output the smoothed series
c .   npts      - no of pts in x
c .   nave     - no. of pts to be utilized in the running average
c .   kopt      - end-point option [prefer an odd number]
c .               kopt < 0 : utilize cyclic conditions
c .                          e.g.,  nave=3 at n=1 and n=npts
c .                          x(1)    = (x(npts)+x(1)+x(2)) / 3.
c .                          x(npts) = (x(npts-1)+x(npts)+x(1)) / 3.
c .                          e.g.,  nave=4 at n=1 and n=npts
c .                          x(1)    = (x(npts)+x(1)+x(2)+x(3)) / 4.
C*PL*ERROR* Comment line too long
c .                          x(npts) = (x(npts-2)+x(npts-1)+x(npts)+x(1)) / 4.
C*PL*ERROR* Comment line too long
c .               kopt = 0 : set unsmoothed beginning and end pts to xmsg
C*PL*ERROR* Comment line too long
c .                          e.g.,  nave=3 , x(1) = xmsg and x(npts) = xmsg
C*PL*ERROR* Comment line too long
c .                          e.g.,  nave=4 , x(1) = xmsg and x(2) = xmsg and
c .                                          x(npts) = xmsg
c .               kopt > 0 : utilize reflective (symmetric) conditions
c .                          e.g.,  nave=3 at n=1 and n=npts
c .                          x(1)    = (x(2)+x(1)+x(2)) / 3.
c .                          x(npts) = (x(npts-1)+x(npts)+x(npts-1)) /3.
c .                          e.g.,  nave=4 at n=1 and n=npts
c .                          x(1)    = (x(3)+x(2)+x(1)+x(2)) / 4.
C*PL*ERROR* Comment line too long
c .                          x(npts) = (x(npts-2)+x(npts-1)+x(npts)+x(npts-1)) /4.
c .   xmsg      - missing/special value
c .               also; if kopt=0 set end points to this special value
c .   work      - work vector
c .   lw        - length of work (=npts+2*N where N=nave/2)
c .   ier       - error flag
c .               ier = 0 : no errors
c .               ier = -11 : npts @ 0
c .               ier = -12 : npts < nave
c .               ier = -13 : nave is not odd

C do not need work this large
      DOUBLE PRECISION X(1:NPTS),WORK(LW)

      IER = 0
      IF (NPTS.LT.1) IER = -11
      IF (NAVE.GT.NPTS) IER = -12
      IF (IER.LT.0) RETURN

      IF (NAVE.LE.1) THEN
          DO N = 1,NPTS
C historical reasons
              WORK(N) = X(N)
          END DO
          RETURN
      END IF

      NAV = NAVE
      NAV2 = NAVE/2
      WGT = 1.D0/DBLE(NAV)
      LWORK = NPTS + 2*NAV2
C odd-even offset used below
      NOE = 0
      IF (MOD(NAVE,2).EQ.0) NOE = 1

      DO N = 1,LWORK
C preset to xmsg
          WORK(N) = XMSG
      END DO

      DO N = 1,NPTS
C stuff x into middle of work
          WORK(NAV2+N) = X(N)
      END DO
C pad work with appropriate values

      IF (KOPT.GT.0) THEN
          DO N = 1,NAV2
c c c        print *,"(nav2+1-n), (npts+nav2+n)=",
c c c+                (nav2+1-n), (npts+nav2+n)
              WORK(NAV2+1-N) = X(N+1)
              WORK(NPTS+NAV2+N) = X(NPTS-N)
          END DO
      ELSE IF (KOPT.LT.0) THEN
          DO N = 1,NAV2
c c c        print *,"(nav2+1-n), (npts+nav2+n)=",
c c c+                (nav2+1-n), (npts+nav2+n)
              WORK(NAV2+1-N) = X(NPTS+1-N)
              WORK(NPTS+NAV2+N) = X(N)
          END DO
      END IF

      IF (MOD(NAVE,2).EQ.0) THEN
c          print *," "
c          do n=1,lwork
c             write (*,"(' runavx77: ',3i4,3f8.2))") kopt,nav2,n,work(n)
c          end do
      END IF

      DO N = 1,NPTS
          KMSG = 0
          SUM = 0.D0
          NMID = N + NAV2 + NOE
          MSTRT = NMID - NAV2
          MLAST = MSTRT + NAV - 1
          DO M = MSTRT,MLAST
              IF (WORK(M).NE.XMSG) THEN
                  SUM = SUM + WORK(M)
              ELSE
                  KMSG = KMSG + 1
              END IF
          END DO

          IF (KMSG.EQ.0) THEN
C all values
              X(N) = SUM*WGT
          ELSE
C msg values encountered
              X(N) = XMSG
          END IF
      END DO

CDIR$ IVDEP
C return x in work(1 => npts)
c     do n=1,npts
C for historical reasons
c        work(n) = work(nav2+n)
c     enddo

      RETURN
      END
