c------------------------------------------------------------------
c     Computes double covariance or correlation matrix
c------------------------------------------------------------------
C NCLFORTSTART
      subroutine dcovarxy(x,y,xmsg,ymsg,cxy,n,m,lag,ncrit,iopt)
      implicit none
c                                                      INPUT
      integer n, m, lag, ncrit, iopt
      double precision x(n,m), y(n,m), xmsg, ymsg
c                                                      OUTPUT
      double precision cxy(m,m)
C NCLEND
c                                                      LOCAL 
      integer i, j, k
      double precision sumxx, sumyy, sumxy, sumx, sumy, nxy, xvar, yvar

      do i=1,m
        do j=1,m
           cxy(i,j) = xmsg
        end do
      end do

      do i=1,m
        do j=1,m
          nxy  = 0.0d0
          sumx = 0.0d0
          sumy = 0.0d0
          sumxy= 0.0d0
          sumxx= 0.0d0
          sumyy= 0.0d0

          do k=lag+1, n
            if (x(k,i).ne.xmsg .and. y(k-lag,j).ne.ymsg) then
                nxy   = nxy + 1
                sumx  = sumx  + x(k,i)
                sumy  = sumy  + y(k-lag,j)
                sumxy = sumxy + x(k,i)*y(k-lag,j)
                sumxx = sumxx + x(k,i)*x(k,i)     
                sumyy = sumyy + y(k-lag,j)*y(k-lag,j)
            end if
          end do
          if (nxy.gt.1d0 .and. nxy.ge.ncrit) then
              cxy(i,j) = (sumxy-(sumx*sumy)/nxy)/(nxy-1d0)

              if (iopt.eq.1 .and. 
     &            sumxx.gt.0.0d0 .and. sumyy.gt.0.0d0) then
                  xvar =  (sumxx-((sumx*sumx)/(nxy)) )/(nxy-1.)
                  yvar =  (sumyy-((sumy*sumy)/(nxy)) )/(nxy-1.)
                  cxy(i,j) = cxy(i,j)/(sqrt(xvar)*sqrt(yvar)) 
              end if

          end if
        end do
      end do      

      return
      end
