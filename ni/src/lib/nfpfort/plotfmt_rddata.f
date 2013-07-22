      subroutine plotfmt_rddata(istatus,nx,ny,slab)

      implicit none
      integer funit, nx, ny, istatus
      real slab(nx,ny)

      istatus = 1
      funit = 10
      read(unit=funit,err=1001,end=1001) slab

      istatus = 0
    
 1001 return
      end
