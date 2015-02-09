C--------------------------------------------------------------
C All the routines in this file were updated on 9 Feb 2015 to 
C reverse the dimensions of "clcntr"/"c" (from "k x n" to 
C "n x k") so that the calling C routine could have the 
C dimensions be K x N.
C--------------------------------------------------------------
C NCLFORTSTART
      subroutine kmns136 (dat, m, n, clcntr, k,  ic1, nc
     +                   ,iter, iseed, wss, ier)
      implicit none
c                            ! INPUT
      integer m, n, k, iter, iseed
      double precision dat(m,n)
c                            ! INPUT/OUTPUT
      integer ic1(m), nc(k), ier
      double precision clcntr(n,k), wss(k)
C NCLEND
c                            ! LOCAL WORK ARRAYS
      integer          ic2(m), ncp(k), itran(k), live(k)
      double precision an1(k), an2(k), d(m)
      integer nv, kk, mm

c Currently: two methods to set the seed
c . iseed=1 pick 1st k elements of the dat array
c . iseed=2 'randomly' sample the dat array

      do kk=1,k
         if (iseed.eq.1) then
             mm = kk
         else
             mm = m/kk
         end if
        do nv=1,n
           clcntr(nv,kk) = dat(mm,nv)
c c c      print *,"mm=", mm," kk=",kk,"  nv=",nv," clc=",clcntr(kk,nv) 
        end do
      end do

      call kmns (dat,m,n,clcntr,k,ic1,ic2,nc,an1,an2,ncp,d
     &          ,itran,live,iter,wss,ier )

      return
      end

c*********************************************************************72
      subroutine kmns ( a, m, n, c, k, ic1, ic2, nc, an1, an2, ncp, d
     &                ,itran, live, iter, wss, ifault )
c
cc KMNS carries out the K-means algorithm.
c
c  Discussion:
c
c    This routine attempts to divide M points in N-dimensional space into
c    K clusters so that the within cluster sum of squares is minimized.
c
c  Modified:
c
c    13 February 2008
c
c  Author:
c
c    Original FORTRAN77 version by John Hartigan, Manchek Wong.
c    Modifications by John Burkardt.
c
c  Reference:
c
c    John Hartigan, Manchek Wong,
c    Algorithm AS 136:
c    A K-Means Clustering Algorithm,
c    Applied Statistics,
c    Volume 28, Number 1, 1979, pages 100-108.
c
c  Parameters:
c
c    Input, double precision A(M,N), the points.
c
c    Input, integer M, the number of points.
c
c    Input, integer N, the number of spatial dimensions (aka, variables).
c
c    Input/output, double precision C(N,K), the cluster centers.
c
c    Input, integer K, the number of clusters.
c
c    Output, integer IC1(M), the cluster to which each point is assigned.
c
c    Workspace, integer IC2(M), used to store the cluster which each point
c    is most likely to be transferred to at each step.
c
c    Output, integer NC(K), the number of points in each cluster.
c
c    Workspace, double precision AN1(K).
c
c    Workspace, double precision AN2(K).
c
c    Workspace, integer NCP(K).
c
c    Workspace, double precision D(M).
c
c    Workspace, integer ITRAN(K).
c
c    Workspace, integer LIVE(K).
c
c    Input, integer ITER, the maximum number of iterations allowed.
c
c    Output, double precision WSS(K), the within-cluster sum of squares
c    of each cluster.
c
c    Output, integer IFAULT, error indicator.
c    0, no error was detected.
c    1, at least one cluster is empty after the initial assignment.  
c       A better set of initial cluster centers is needed.
c    2, the allowed maximum number off iterations was exceeded.
c    3, K is less than or equal to 1, or greater than or equal to M.
c
      implicit none

      integer k
      integer m
      integer n

      double precision a(m,n)
      double precision aa
      double precision an1(k)
      double precision an2(k)
      double precision c(n,k)
      double precision d(m)
      double precision da
      double precision db
      double precision dc
      double precision dt(2)
      integer i
      integer ic1(m)
      integer ic2(m)
      integer ifault
      integer ii
      integer ij
      integer il
      integer indx
      integer iter
      integer itran(k)
      integer j
      integer l
      integer live(k)
      integer nc(k)
      integer ncp(k)
      double precision r8_huge
      double precision temp
      double precision wss(k)

      ifault = 0

      if ( k .le. 1 .or. m .le. k ) then
        ifault = 3
        return
      end if
c
c  For each point I, find its two closest centers, IC1(I) and
c  IC2(I).  Assign the point to IC1(I).
c
      do i = 1, m

        ic1(i) = 1
        ic2(i) = 2

        do il = 1, 2
          dt(il) = 0.0D+00
          do j = 1, n
            da = a(i,j) - c(j,il)
            dt(il) = dt(il) + da * da
          end do
        end do

        if ( dt(2) .lt. dt(1) ) then
          ic1(i) = 2
          ic2(i) = 1
          temp = dt(1)
          dt(1) = dt(2)
          dt(2) = temp
        end if

        do l = 3, k

          db = 0.0D+00

          do j = 1, n
            dc = a(i,j) - c(j,l)
            db = db + dc * dc
          end do

          if ( db .lt. dt(2) ) then

            if ( dt(1) .le. db ) then
              dt(2) = db
              ic2(i) = l
            else
              dt(2) = dt(1)
              ic2(i) = ic1(i)
              dt(1) = db
              ic1(i) = l
            end if

          end if

        end do

      end do
c
c  Update cluster centers to be the average of points contained within them.
c
      do l = 1, k
        nc(l) = 0
        do j = 1, n
          c(j,l) = 0.0D+00
        end do
      end do

      do i = 1, m
        l = ic1(i)
        nc(l) = nc(l) + 1
        do j = 1, n
          c(j,l) = c(j,l) + a(i,j)
        end do
      end do
c
c  Check to see if there is any empty cluster at this stage.
c
      ifault = 1

      do l = 1, k

        if ( nc(l) .eq. 0 ) then
          ifault = 1
          return
        end if

      end do

      ifault = 0

      do l = 1, k

        aa = dble ( nc(l) )

        do j = 1, n
          c(j,l) = c(j,l) / aa
        end do
c
c  Initialize AN1, AN2, ITRAN and NCP.
c
c  AN1(L) = NC(L) / (NC(L) - 1)
c  AN2(L) = NC(L) / (NC(L) + 1)
c  ITRAN(L) = 1 if cluster L is updated in the quick-transfer stage,
c           = 0 otherwise
c
c  In the optimal-transfer stage, NCP(L) stores the step at which
c  cluster L is last updated.
c
c  In the quick-transfer stage, NCP(L) stores the step at which
c  cluster L is last updated plus M.
c
        an2(l) = aa / ( aa + 1.0D+00 )

        if ( 1.0D+00 .lt. aa ) then
          an1(l) = aa / ( aa - 1.0D+00 )
        else
          an1(l) = r8_huge ( )
        end if

        itran(l) = 1
        ncp(l) = -1

      end do

      indx = 0
      ifault = 2

      do ij = 1, iter
c
c  In this stage, there is only one pass through the data.   Each
c  point is re-allocated, if necessary, to the cluster that will
c  induce the maximum reduction in within-cluster sum of squares.
c
        call optra ( a, m, n, c, k, ic1, ic2, nc, an1, an2, ncp, d,
     &    itran, live, indx )
c
c  Stop if no transfer took place in the last M optimal transfer steps.
c
        if ( indx .eq. m ) then
          ifault = 0
          go to 150
        end if
c
c  Each point is tested in turn to see if it should be re-allocated
c  to the cluster to which it is most likely to be transferred,
c  IC2(I), from its present cluster, IC1(I).   Loop through the
c  data until no further change is to take place.
c
        call qtran ( a, m, n, c, k, ic1, ic2, nc, an1, an2, ncp, d,
     &    itran, indx )
c
c  If there are only two clusters, there is no need to re-enter the
c  optimal transfer stage.
c
        if ( k .eq. 2 ) then
          ifault = 0
          go to 150
        end if
c
c  NCP has to be set to 0 before entering OPTRA.
c
        do l = 1, k
          ncp(l) = 0
        end do

      end do

  150 continue
c
c  If the maximum number of iterations was taken without convergence,
c  IFAULT is 2 now.  This may indicate unforeseen looping.
c
      if ( ifault == 2 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'KMNS - Warning!'
        write ( *, '(a)' ) '  Maximum number of iterations reached'
        write ( *, '(a)' ) '  without convergence.'
      end if
c
c  Compute the within-cluster sum of squares for each cluster.
c
      do l = 1, k
        wss(l) = 0.0D+00
        do j = 1, n
          c(j,l) = 0.0D+00
        end do
      end do

      do i = 1, m
        ii = ic1(i)
        do j = 1, n
          c(j,ii) = c(j,ii) + a(i,j)
        end do
      end do

      do j = 1, n
        do l = 1, k
          c(j,l) = c(j,l) / dble ( nc(l) )
        end do
        do i = 1, m
          ii = ic1(i)
          da = a(i,j) - c(j,ii)
          wss(ii) = wss(ii) + da * da
        end do
      end do

      return
      end
      subroutine optra ( a, m, n, c, k, ic1, ic2, nc, an1, an2, ncp,
     &  d, itran, live, indx )

c*********************************************************************72
c
cc OPTRA carries out the optimal transfer stage.
c
c  Discussion:
c
c    This is the optimal transfer stage.
c
c    Each point is re-allocated, if necessary, to the cluster that
c    will induce a maximum reduction in the within-cluster sum of
c    squares.
c
c  Modified:
c
c    15 February 2008
c
c  Author:
c
c    Original FORTRAN77 version by John Hartigan, Manchek Wong.
c    Modifications by John Burkardt.
c
c  Reference:
c
c    John Hartigan, Manchek Wong,
c    Algorithm AS 136:
c    A K-Means Clustering Algorithm,
c    Applied Statistics,
c    Volume 28, Number 1, 1979, pages 100-108.
c
c  Parameters:
c
c    Input, double precision A(M,N), the points.
c
c    Input, integer M, the number of points.
c
c    Input, integer N, the number of spatial dimensions.
c
c    Input/output, double precision C(N,K), the cluster centers.
c
c    Input, integer K, the number of clusters.
c
c    Input/output, integer IC1(M), the cluster to which each point is assigned.
c
c    Input/output, integer IC2(M), used to store the cluster which each point
c    is most likely to be transferred to at each step.
c
c    Input/output, integer NC(K), the number of points in each cluster.
c
c    Input/output, double precision AN1(K).
c
c    Input/output, double precision AN2(K).
c
c    Input/output, integer NCP(K).
c
c    Input/output, double precision D(M).
c
c    Input/output, integer ITRAN(K).
c
c    Input/output, integer LIVE(K).
c
c    Input/output, integer INDX, the number of steps since a transfer took place.
c
      implicit none

      integer k
      integer m
      integer n

      double precision a(m,n)
      double precision al1
      double precision al2
      double precision alt
      double precision alw
      double precision an1(k)
      double precision an2(k)
      double precision c(n,k)
      double precision d(m)
      double precision da
      double precision db
      double precision dc
      double precision dd
      double precision de
      double precision df
      integer i
      integer ic1(m)
      integer ic2(m)
      integer indx
      integer itran(k)
      integer j
      integer l
      integer l1
      integer l2
      integer live(k)
      integer ll
      integer nc(k)
      integer ncp(k)
      double precision r2
      double precision r8_huge
      double precision rr
c
c  If cluster L is updated in the last quick-transfer stage, it
c  belongs to the live set throughout this stage.   Otherwise, at
c  each step, it is not in the live set if it has not been updated
c  in the last M optimal transfer steps.
c
      do l = 1, k
        if ( itran(l) .eq. 1) then
          live(l) = m + 1
        end if
      end do

      do i = 1, m

        indx = indx + 1
        l1 = ic1(i)
        l2 = ic2(i)
        ll = l2
c
c  If point I is the only member of cluster L1, no transfer.
c
        if ( 1 .lt. nc(l1)  ) then
c
c  If L1 has not yet been updated in this stage, no need to
c  re-compute D(I).
c
          if ( ncp(l1) .ne. 0 ) then
            de = 0.0D+00
            do j = 1, n
              df = a(i,j) - c(j,l1)
              de = de + df * df
            end do
            d(i) = de * an1(l1)
          end if
c
c  Find the cluster with minimum R2.
c
         da = 0.0D+00
          do j = 1, n
            db = a(i,j) - c(j,l2)
            da = da + db * db
          end do
          r2 = da * an2(l2)

          do l = 1, k
c
c  If LIVE(L1) <= I, then L1 is not in the live set.   If this is
c  true, we only need to consider clusters that are in the live set
c  for possible transfer of point I.   Otherwise, we need to consider
c  all possible clusters.
c
            if ( ( i .lt. live(l1) .or. i .lt. live(l2) ) .and.
     &             l .ne. l1 .and. l .ne. ll ) then

              rr = r2 / an2(l)

              dc = 0.0D+00
              do j = 1, n
                dd = a(i,j) - c(j,l)
                dc = dc + dd * dd
              end do

              if ( dc .lt. rr ) then
                r2 = dc * an2(l)
                l2 = l
              end if

            end if

          end do
c
c  If no transfer is necessary, L2 is the new IC2(I).
c
          if ( d(i) .le. r2 ) then

            ic2(i) = l2
c
c  Update cluster centers, LIVE, NCP, AN1 and AN2 for clusters L1 and
c  L2, and update IC1(I) and IC2(I).
c
          else

            indx = 0
            live(l1) = m + i
            live(l2) = m + i
            ncp(l1) = i
            ncp(l2) = i
            al1 = nc(l1)
            alw = al1 - 1.0D+00
            al2 = nc(l2)
            alt = al2 + 1.0D+00
            do j = 1, n
              c(j,l1) = ( c(j,l1) * al1 - a(i,j) ) / alw
              c(j,l2) = ( c(j,l2) * al2 + a(i,j) ) / alt
            end do
            nc(l1) = nc(l1) - 1
            nc(l2) = nc(l2) + 1
            an2(l1) = alw / al1
            if ( 1.0D+00 .lt. alw ) then
              an1(l1) = alw / ( alw - 1.0D+00 )
            else
              an1(l1) = r8_huge ( )
            end if
            an1(l2) = alt / al2
            an2(l2) = alt / ( alt + 1.0D+00 )
            ic1(i) = l2
            ic2(i) = l1

          end if

        end if

        if ( indx .eq. m ) then
          return
        end if

      end do
c
c  ITRAN(L) = 0 before entering QTRAN.   Also, LIVE(L) has to be
c  decreased by M before re-entering OPTRA.
c
      do l = 1, k
        itran(l) = 0
        live(l) = live(l) - m
      end do

      return
      end
      subroutine qtran ( a, m, n, c, k, ic1, ic2, nc, an1, an2, ncp,
     &  d, itran, indx )

c*********************************************************************72
c
cc QTRAN carries out the quick transfer stage.
c
c  Discussion:
c
c    This is the quick transfer stage.
c
c    IC1(I) is the cluster which point I belongs to.
c    IC2(I) is the cluster which point I is most likely to be
c    transferred to.
c
c    For each point I, IC1(I) and IC2(I) are switched, if necessary, to
c    reduce within-cluster sum of squares.  The cluster centers are
c    updated after each step.
c
c  Modified:
c
c    15 February 2008
c
c  Author:
c
c    Original FORTRAN77 version by John Hartigan, Manchek Wong.
c    Modifications by John Burkardt.
c
c  Reference:
c
c    John Hartigan, Manchek Wong,
c    Algorithm AS 136:
c    A K-Means Clustering Algorithm,
c    Applied Statistics,
c    Volume 28, Number 1, 1979, pages 100-108.
c
c  Parameters:
c
c    Input, double precision A(M,N), the points.
c
c    Input, integer M, the number of points.
c
c    Input, integer N, the number of spatial dimensions.
c
c    Input/output, double precision C(N,K), the cluster centers.
c
c    Input, integer K, the number of clusters.
c
c    Input/output, integer IC1(M), the cluster to which each point is assigned.
c
c    Input/output, integer IC2(M), used to store the cluster which each point
c    is most likely to be transferred to at each step.
c
c    Input/output, integer NC(K), the number of points in each cluster.
c
c    Input/output, double precision AN1(K).
c
c    Input/output, double precision AN2(K).
c
c    Input/output, integer NCP(K).
c
c    Input/output, double precision D(M).
c
c    Input/output, integer ITRAN(K).
c
c    Input/output, integer INDX, counts the number of steps since the
c    last transfer.
c
      implicit none

      integer k
      integer m
      integer n

      double precision a(m,n)
      double precision al1
      double precision al2
      double precision alt
      double precision alw
      double precision an1(k)
      double precision an2(k)
      double precision c(n,k)
      double precision d(m)
      double precision da
      double precision db
      double precision dd
      double precision de
      integer i
      integer ic1(m)
      integer ic2(m)
      integer icoun
      integer indx
      integer istep
      integer itran(k)
      integer j
      integer l1
      integer l2
      integer nc(k)
      integer ncp(k)
      double precision r2
      double precision r8_huge
c
c  In the optimal transfer stage, NCP(L) indicates the step at which
c  cluster L is last updated.   In the quick transfer stage, NCP(L)
c  is equal to the step at which cluster L is last updated plus M.
c
      icoun = 0
      istep = 0

   10 continue

        do i = 1, m

          icoun = icoun + 1
          istep = istep + 1
          l1 = ic1(i)
          l2 = ic2(i)
c
c  If point I is the only member of cluster L1, no transfer.
c
          if ( 1 .lt. nc(l1) ) then
c
c  If NCP(L1) < ISTEP, no need to re-compute distance from point I to
c  cluster L1.   Note that if cluster L1 is last updated exactly M
c  steps ago, we still need to compute the distance from point I to
c  cluster L1.
c
            if ( istep .le. ncp(l1) ) then

              da = 0.0D+00
              do j = 1, n
                db = a(i,j) - c(j,l1)
                da = da + db * db
              end do

              d(i) = da * an1(l1)

            end if
c
c  If NCP(L1) <= ISTEP and NCP(L2) <= ISTEP, there will be no transfer of
c  point I at this step.
c
            if ( istep .lt. ncp(l1) .or. istep .lt. ncp(l2) ) then

              r2 = d(i) / an2(l2)

              dd = 0.0D+00
              do j = 1, n
                de = a(i,j) - c(j,l2)
                dd = dd + de * de
              end do
c
c  Update cluster centers, NCP, NC, ITRAN, AN1 and AN2 for clusters
c  L1 and L2.   Also update IC1(I) and IC2(I).   Note that if any
c  updating occurs in this stage, INDX is set back to 0.
c
              if ( dd .lt. r2 ) then

                icoun = 0
                indx = 0
                itran(l1) = 1
                itran(l2) = 1
                ncp(l1) = istep + m
                ncp(l2) = istep + m
                al1 = nc(l1)
                alw = al1 - 1.0D+00
                al2 = nc(l2)
                alt = al2 + 1.0D+00
                do j = 1, n
                  c(j,l1) = ( c(j,l1) * al1 - a(i,j) ) / alw
                  c(j,l2) = ( c(j,l2) * al2 + a(i,j) ) / alt
                end do
                nc(l1) = nc(l1) - 1
                nc(l2) = nc(l2) + 1
                an2(l1) = alw / al1
                if ( 1.0D+00 .lt. alw ) then
                  an1(l1) = alw / ( alw - 1.0D+00 )
                else
                  an1(l1) = r8_huge ( )
                end if
                an1(l2) = alt / al2
                an2(l2) = alt / ( alt + 1.0D+00 )
                ic1(i) = l2
                ic2(i) = l1

              end if

            end if

          end if
c
c  If no re-allocation took place in the last M steps, return.
c
          if ( icoun .eq. m ) then
            return
          end if

        end do

      go to 10
      end
      function r8_huge ( )

c*********************************************************************72
c
cc R8_HUGE returns a "huge" R8.
c
c  Modified:
c
c    13 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_HUGE, a huge number.
c
      implicit none

      double precision r8_huge

      r8_huge = 1.0D+30

      return
      end
