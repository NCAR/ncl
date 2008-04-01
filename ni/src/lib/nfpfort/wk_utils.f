C NCLFORTSTART
      subroutine wksmooth121 (vv,vn,nn,spv,dum)
      implicit none
      integer nn,vn
      double precision vv(vn), spv, dum(vn)
C NCLEND

c Smooths vv by passing it through a 1-2-1 filter.
c The first and last points are given 3-1 (1st) or 1-3 (last)
c weightings (Note that this conserves the total sum).
c The routine also skips-over missing data (spv)
c There are 'nn' pieces of useful information, which may be less
c than or equal to 'vn'.
  
      integer i

      i = 0
 10   continue
      i = i+1
      if (vv(i).eq.spv) then
          dum(i) = spv
      elseif(i.eq.1) then
          if(vv(i+1).ne.spv) then
             dum(i) = (3.0d0*vv(i)+vv(i+1))/4.0d0
          else
             dum(i) = vv(i)
          end if
      elseif(vv(i-1).eq.spv) then
          if(vv(i+1).ne.spv) then
             dum(i) = (3.0d0*vv(i)+vv(i+1))/4.0d0
          else
             dum(i) = vv(i)
          end if
      elseif(i.eq.nn.or.vv(i+1).eq.spv) then
          if(vv(i-1).ne.spv) then
             dum(i) = (vv(i-1)+3.0d0*vv(i))/4.0d0
          else
             dum(i) = vv(i)
          end if
      else
           dum(i) = (1.0d0*vv(i-1)+2.0d0*vv(i)+1.0d0*vv(i+1))/4.0d0
      end if
      if (i.ne.nn) go to 10

      do i = 1,nn
         vv(i) = dum(i)
      enddo
      end

C NCLFORTSTART
       subroutine wktapertozero(ts,N,nmi,nn,tp)
       implicit none
       integer  N, nmi, nn, tp
       double precision ts(N)
C NCLEND


c "taper" the first and last 'tp' members of 'ts' by multiplication by
c a segment of the cosine curve so that the ends of the series
c taper toward zero. This satisfies the
c periodic requirement of the FFT.
c Only the data from nmi to nmi+nn-1 is deemed useful, and
c the rest is set to zero. (There are nn points of useful data)
       integer i,j
       double precision Pi
c c c  parameter (Pi = 3.1415926)   !tp is number to taper on each end.

       Pi = 4.0d0*atan(1.0d0)

       if (N.lt.25.or.nn.lt.25) then
        print*,'wktapertozero: No tapering if less than 25 values!'
c c c   STOP
        return
       end if
       if (nmi+nn-1.gt.N) then
        print*,'wktapertozero: ERROR as nmi+nn-1 > N'
        STOP
       end if

       do i = 1,N
        j = i-nmi+1
        if (j.le.0.or.j.gt.nn) then
         ts(i) = 0.0d0
        elseif (j.le.tp) then
         ts(i) =  ts(i)*0.5d0*(1.0d0-COS((j-1)*Pi/dble(tp)))
        elseif (j.gt.(nn-tp).and.j.le.nn) then
         ts(i) =  ts(i)*0.5d0*(1.0d0-COS((nn-j)*Pi/dble(tp)))
        else
         ts(i) =  ts(i)
        end if
       enddo

       return
       end

C NCLFORTSTART
       subroutine wkdetrend(X2,nx,vmi,nv)
       implicit none
       integer  nx, nv, vmi
       double precision X2(nx)
C NCLEND

c Removes the linear-squares best fit line for the series.
c Only the data from vmi to vmi+nv-1 is deemed useful, and
c the rest is zeroed. (There are nv points of useful data)
c The line of best fit is given by X2'  =  a + bX1.
c The X1s are assumed to be evenly spaced ( i.e. X1 = dble(i) )
c
c m1  =  mean of X1
c m2  =  mean of X2
c m3  =  mean of (X1*X2)
c m4  =  mean of (X1*X1)
c m5  =  mean of (X2*X2)

       integer i
       real m1,m2,m3,m4,m5,r,a,b

       if (vmi+nv-1.gt.nx) then
        print*,'wkdtrend:ERROR as vmi+nv-1 > nx'
        STOP
       end if

       m1 = 0.0d0
       m2 = 0.0d0
       m3 = 0.0d0
       m4 = 0.0d0
       m5 = 0.0d0

       do i = vmi,vmi+nv-1
        m1  =  m1+dble(i)/dble(nv)
        m2  =  m2+X2(i)/dble(nv)
        m3  =  m3+(dble(i)*X2(i))/dble(nv)
        m4  =  m4+(dble(i)*dble(i))/dble(nv)
        m5  =  m5+(X2(i)*X2(i))/dble(nv)
       end do   

       r = (m3-m1*m2)/(sqrt(m4-m1**2)*sqrt(m5-m2**2))
       b = (m3-m1*m2)/(m4-m1**2)
       a = m2-b*m1

       do i = vmi,vmi+nv-1
        X2(i)  =  X2(i) - (a + b*dble(i))
       end do
c      print*,'Trend line has a = ',a,' b = ',b

       if (vmi.gt.1) then
           do i = 1,vmi-1
              X2(i)  =  0.0d0
           end do   
       end if
       if (vmi+nv.le.nx) then
           do i = vmi+nv,nx
              X2(i) =  0.0d0
           end do 
       end if

       return
       end              
