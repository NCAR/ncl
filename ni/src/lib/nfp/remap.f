C NCLFORTSTART
      subroutine popremap 
     &          (dst_array, map_wts, dst_add ,src_add, src_array
     &          ,ndst,nlink,nw,nsrc,xmsg)

c written in f77 for the GNU f77 compiler for portability reasons

      implicit none
      integer  nlink, nw, ndst, nsrc
      real     map_wts(nw,nlink)
      real     dst_array(ndst)
      real     src_array(nsrc)
      integer  dst_add(nlink)
      integer  src_add(nlink)
      real     xmsg                 
C NCLEND
      integer  n

      do n=1,ndst
         dst_array(n) = 0.0                         ! initilize
      end do

      do n=1,nlink    
         if (src_array(src_add(n)).ne.xmsg) then    
             dst_array(dst_add(n)) = dst_array(dst_add(n)) + 
     &                               src_array(src_add(n))*map_wts(1,n)
         end if
      end do

      return
      end
