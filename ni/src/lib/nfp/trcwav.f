c---------------------------------------------------------------------
c     subroutine trcwav(igridb,nlatb,nlonb,DB,wsave,lsave,work,lwork,
c    +                  dwork,ldwork,ier,JWAVE)
c
c *** author
c
c     John C, Adams (NCAR 1997), email: johnad@ncar.ucar.edu
c                                phone: 303-497-1213
c
c *** purpose
c
c     Given the scalar field DB on the sphere, subroutine trcwav
c     eliminates harmonic waves of DB with number greater than JWAVE.
c     So, for example, when transferring grid data from T63 to T42,
c     trcwav can be called with JWAVE=42 after calling trssph to eliminate
c     frequencies higher than 42.
c
c *** the arguments igridb,nlatb,nlonb,wsave,lsave,work,lwork,ier
c     for trcwav are the same as the corresponding arguments in trssph.
c     See the documentation of trssph for a description.  The last argument
c     JWAVE sets the wave truncation. The work space lengths in lsave and
c     lwork should be set as in trssph.  They are not checked in this code.
c
      subroutine trcwav(igridb,nlatb,nlonb,DB,wsave,lsave,work,
     +                  lwork,dwork,ldwork,ier,JWAVE)
      implicit none
      integer igridb(2),nlonb,nlatb
      integer lsave,lwork,ldwork,ier,JWAVE
      real db(*),wsave(*),work(*)
      double precision dwork(*)
      integer ig,igrdb,lb1,lb2,iab,ibb,lw,iw,nt,isym
      save

c
c     check input arguments
c
      ier = 6
      ig = igridb(1)
      if ((ig-1)*(ig+1)*(ig-2)*(ig+2).ne.0) return
      ier = 7
      ig = igridb(2)
      if (ig*(ig-1).ne.0) return
      ier = 8
      if (nlonb .lt.4) return
      ier = 9
      if (nlatb .lt.3) return
      ier = 0

      lb1 = min0(nlatb,(nlonb+2)/2)
      lb2 = (nlatb+1)/2
c
c     set pointers for spherical harmonic coefs
c
      iab = 1
      ibb = iab + lb1*nlatb
      igrdb = iabs(igridb(1))
c
c     set pointers for remaining work
c
      iw = ibb+lb1*nlatb
c
c     set remaining work space length in lw
c
      lw = lwork - iw

      if (igrdb .eq. 2) then
c
c     initialize wsave for gaussian synthesis

      call shsgci(nlatb,nlonb,wsave,lsave,dwork,ldwork,ier)
csp1  call shigc (nlatb,nlonb,wsave,lsave,dwork,ldwork,ier)
      if (ier.ne.0) then
c
c     flag failure in spherepack gaussian software
c
      ier = 12
      return
      end if
      else
c
c     initialize wsave for equally spaced synthesis
c
      call shseci(nlatb,nlonb,wsave,lsave,dwork,ldwork,ier)
      end if
c
c     transpose and/or reorder (co)latitude if necessary for DB
c     (arrays must have latitude (colatitude) as the first dimension
c     and run north to south for spherepack software)
c
      if (igridb(2) .eq. 0) call trsplat(nlonb,nlatb,DB,work)
      if (igridb(1) .gt. 0) call convlat(nlatb,nlonb,DB)

      nt = 1
      isym = 0
      if (igrdb .eq. 2) then
c
c     do spherical harmonic analysis of "adjusted" DB on gaussian grid
c
      call shagc(nlatb,nlonb,isym,nt,DB,nlatb,nlonb,work(iab),
     +work(ibb),lb1,nlatb,wsave,lsave,work(iw),lw,ier)
      else
c
c     do spherical harmonic analysis of "adjusted" DB on equally spaced grid
c
      call shaec(nlatb,nlonb,isym,nt,DB,nlatb,nlonb,work(iab),
     +work(ibb),lb1,nlatb,wsave,lsave,work(iw),lw,ier)
      end if
c
c     set DB wave number coefficients greater than JWAVE to zero
c
      call trcdb(lb1,nlatb,work(iab),work(ibb),JWAVE,ier)
C
C     regenerate DB with truncated coefficients
c
      if (igrdb .eq. 1) then
c
c     do spherical harmonic synthesis on nlatb by nlonb equally spaced grid
c
      call shsec(nlatb,nlonb,isym,nt,DB,nlatb,nlonb,work(iab),
     +work(ibb),lb1,nlatb,wsave,lsave,work(iw),lw,ier)
      else
c
c     do spherical harmonic synthesis on nlatb by nlonb gaussian grid
c
      call shsgc(nlatb,nlonb,isym,nt,DB,nlatb,nlonb,work(iab),
     +work(ibb),lb1,nlatb,wsave,lsave,work(iw),lw,ier)
      end if
c
c     DB is now latitude by longitude north to south array
c     restore DB to agree with flag igridb
c
      if (igridb(1) .gt. 0) call convlat(nlatb,nlonb,DB)
      if (igridb(2) .eq. 0) call trsplat(nlatb,nlonb,DB,work)

      return
      end
c -----------------------------------------------------
      subroutine trcdb(mb,nb,ab,bb,JWAVE,jer)
c
c     eliminate coefficients for wave numbers > JWAVE
c     triangular truncation at JWAVE
c
      implicit none
      integer mb,nb,JWAVE,jer
      real    ab(mb,nb),bb(mb,nb)

      integer j,m,n,iwave,jp,jw,ker,mer   ! local
      real    con                         ! local

c c c real, allocatable, dimension(:) :: twgt
      integer    ntwgt
      parameter (ntwgt=500)
      real  twgt(ntwgt)

      jer = 0

      iwave = iabs(jwave)
      if ((iwave+2).gt.nb) then  ! test for error
          jer = -8
          write (*,'(''SUB TRCDB/TRCWAV: jwave too large: ''
     *              ,''iwave,jwave,nb='',3i5)') iwave, jwave, nb
      elseif (nb.ne.mb) then
          jer = -8
          write (*,'(''SUB TRCDB/TRCWAV: nb .ne. mb: ''
     *              ,''nb,mb='',2i5)') nb,mb
      endif
      if (jer.ne.0) return

      do n=iwave+2,nb            ! perform triangular truncation
        do m=1,mb
           ab(m,n) = 0.0
           bb(m,n) = 0.0
        enddo
      enddo

      if (jwave.gt.0) return     ! must *not* want to taper
                                 ! --------------------------
                                 ! perform exponential tapering 
      jp = max0(iwave/10,1)      ! exponent; determines fall-off rate
      jw = jp*10                 ! coef which has wgt=exp(-1)

c c c write (*,'(''trcdb: jp,jw,jwave,iwave,nb,mb='',6i5)') 
c c c*                    jp,jw,jwave,iwave,nb,mb
      

c c c allocate (twgt(iwave+1), stat=ker)
      if ((iwave+1).gt.ntwgt) then
         write (*,'(''trcdb: ntwgt exceeded='',2i5)') ntwgt,(iwave+1)  
         return
      endif
          
      con = 1./real(jw*(jw+1))
      do j=0,iwave
         twgt(j+1) = exp(-(real(j*(j+1))*con)**jp)
c c c    write (*,'(''trcdb: j, twgt(j)='',2i5,1x,f15.7)') 
c c c*                       j, (j+1),twgt(j+1)   
      enddo
                                 ! now wgt the coef by the wgts
      do n=iwave+1,1,-1          ! traverse the diagonal
        do m=1,n
           ab(m,n) = ab(m,n)*twgt(n)
           bb(m,n) = bb(m,n)*twgt(n)
        enddo
      enddo

c c c deallocate (twgt, stat=mer)

      return
      end
