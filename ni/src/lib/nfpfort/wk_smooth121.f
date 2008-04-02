C NCLFORTSTART
      subroutine wksmooth121 (vv,vn,nn,spv,dum)
      implicit none
      integer nn,vn
      double precision vv(vn), spv, dum(vn)
C NCLEND

c NCL procedure:   wk_smooth121( x )

c vv  - input and output
c vn  - length of vv
c nn  - length of vv ... near as I can tell
c spv - missing value
c dum - work array same length as vv

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
      end do
      end
