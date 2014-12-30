C NCLFORTSTART
      SUBROUTINE kenststncl(xdata,n,s,z,prob,trend)
      IMPLICIT none
C                                     ! INPUT
      INTEGER n
      DOUBLE PRECISION    xdata(n)
      DOUBLE PRECISION    eps
C                                     ! OUTPUT
      INTEGER s
      DOUBLE PRECISION    z, prob, trend
C NCLEND
      INTEGER nslp                                ! LOCAL ==> INTERFACE
      DOUBLE PRECISION    slope(n*(n-1)/2)        ! MALLOC: Thiel-Sens
      LOGICAL             tieflg(n)               ! MALLOC

      eps    = 1.0d-5                             ! make optional
      nslp   = n*(n-1)/2

      call kenstst(xdata,n,s,z,prob,trend
     +            ,nslp,slope,tieflg,eps)

      return
      end

c-----------
      SUBROUTINE kenstst(xdata,n,s,z,prob,trend   ! standard
     +                  ,nslp,slope,tieflg,eps)   ! for NCL interface
      IMPLICIT none
C                                     ! INPUT
      INTEGER n
      DOUBLE PRECISION    xdata(n)
C                                     ! OUTPUT
      INTEGER s
      DOUBLE PRECISION    z, prob, trend

      INTEGER nslp                    ! FROM INTERFACE
      DOUBLE PRECISION    slope(nslp) ! MALLOC: Thiel-Sens
      DOUBLE PRECISION    eps
      LOGICAL tieflg(n)
C                                     ! LOCAL
      INTEGER j, k, nc, nt, ntie, ncrit
      DOUBLE PRECISION  diff, dave, var, vartie, zero

      zero = 0.0d0
      ncrit  =  10    ! series must be at least this length

      if (n.lt.ncrit) then
          s    = 0
          z    = zero
          prob = zero
          return
      end if

      eps     = 1d-5  ! closer than this is a tie
      vartie  = zero
      nc      = 0     ! total number of comparisons used (# of non-ties)
      s       = 0     ! S statistic

      DO j=1,nslp
         slope(j) = zero
      END DO

      DO j=1,n
         tieflg(j) = .false.
      END DO

      DO j=1,n-1

         ntie = 1     ! a value is always tied with itself

        DO k=j+1,n
           diff = xdata(k)-xdata(j)

C                     ! close enough for a tie?
           dave = (xdata(j)+xdata(k))/2.0d0  
           IF (diff.ne.0.0d0 .and. dave.ne.0.0d0) THEN
               if (abs(diff/dave) .lt. eps) diff = zero
           END IF

C                     ! counts
           nc = nc+1
           slope(nc) = diff/(k-j)

           IF (diff.gt.zero) THEN
               s = s+1
           ELSE IF (diff.lt.zero) THEN
               s = s-1
           ELSE
               ntie      = ntie + 1 
               tieflg(k) = .true.
           END IF
        END DO

        IF (ntie.ne.1 .and. .not.tieflg(j)) then
            vartie = vartie + ntie*(ntie-1d0)*(2d0*ntie+5d0)/18d0
c c c       write(*,'("ntie=",i4,"  vartie=",f8.1)') ntie, vartie
        END IF
      END DO
                                                   ! adjustment
      nt    = INT((1d0 + sqrt( 1d0 + 8d0*nc ))/2d0)
      var   = nt* (nt-1d0)* (2d0*nt + 5d0)/18d0    ! simple variance no ties
      var   = var - vartie                         ! adjust for ties

      IF (s.gt.zero) then
          z = (s-1d0)/sqrt(var)
      ELSEIF (s.lt.zero) then
          z = (s+1d0)/sqrt(var)
      ELSE
          z = zero
      END IF

      prob = erf(abs(z)/sqrt(2d0))

c Theil-Sen Trend Estimate: sort and take median 

      call dsortu(slope,nc)
      IF (mod(nc,2).eq.0) THEN
          trend = 0.5d0*(slope(nc/2)+slope(nc/2+1))
      ELSE
          trend = slope(nc/2+1)
      END IF

c c c      write(*,'(" n=",i3," s=",i5, " nc=",i6, "  nt=",i3
c c c     *         ," var=",f8.1," vartie=",f8.1," trend=",f10.5)') 
c c c     *         n, s, nc, nt, var, vartie, trend
    
      RETURN
      END 
