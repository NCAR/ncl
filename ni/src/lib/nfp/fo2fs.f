      subroutine fo2f (goff,ilon,jlat,greg,jlat1,work,lwork,wsave,
     +                 lsavex,ioff,ier)
 
      integer    ilon,jlat,jlat1,ioff    ! input    
      real       goff (ilon,jlat)
      integer    ier                     ! output
      real       greg(ilon,jlat1)
 
                                         ! local 
      real work(lwork)
      real wsave(lsavex)
      integer lwork, lsavex
      integer lsave
  
      ier = 0
      if (ilon.lt.4 .or. jlat.lt.3) ier = 10
      if ((jlat1-jlat).ne.1)        ier = ier+100
      if (ier.ne.0) return
 
      lsave = 2*(2*jlat1+ilon+16)
      lsave = (10*lsave)/9  
      call sshifti (ioff,ilon,jlat,lsave,wsave,ier)
      call sshifte (ioff,ilon,jlat,goff,greg,wsave,lsave,work,lwork,ier)
 
      return
      end
 
