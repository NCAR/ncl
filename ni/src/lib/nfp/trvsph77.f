c--------------------------------------------------------------------------
      subroutine trvsph77 (intl,igrida,nlona,nlata,ua,va
     *                         ,igridb,nlonb,nlatb,ub,vb,jwave
     *                         ,work,lwork,wsave,lsave,dwork,ldwork,ier)
                        
      integer intl,igrida(2),nlona,nlata,igridb(2),nlonb,nlatb,jwave,ier
      integer lsave,lsvmin,lwork,lwkmin
      real ua(nlona,nlata),va(nlona,nlata)    ! input
      real ub(nlonb,nlatb),vb(nlonb,nlatb)    ! output
      data ientry /0/
      save ientry


      real work(lwork), wsave(lsave)
      double precision dwork(ldwork)
      ientry = ientry+1
c c c write (*,'(//,15(''-''),1x,'' sub trvsph77: ientry='',i3
c c c*             ,15(''-'') )')   ientry

c c c print *,' sub trvsph77: debug1: ', nlona,nlata,nlonb,nlatb,jwave    

c c c write (*,'('' sub trvsph77: jwave ='', i3)') jwave 
c c c write (*,'('' sub trvsph77: igrida='',2i3)') (igrida(i),i=1,2)
c c c write (*,'('' sub trvsph77: igridb='',2i3)') (igridb(i),i=1,2)
c c c write (*,'('' sub trvsph77: nlona,nlata='',2i3)') nlona,nlata
c c c write (*,'('' sub trvsph77: nlonb,nlatb='',2i3)') nlonb,nlatb
c c c write (*,'('' sub trvsph77:  ua(grida)='',i3,10f7.1)')
c c c*                          (j,(ua(i,j),i=1,10),j=1,nlata,nlata-1)

c call the f77 version with the full argument list
      
      iveca = 0
      ivecb = 0
      call trvsph(intl,igrida,nlona,nlata,iveca,ua,va
     +                ,igridb,nlonb,nlatb,ivecb,ub,vb
     +                ,wsave,lsave,lsvmin,work,lwork,lwkmin
     +                ,dwork,ldwork,ler)

c c c write (*,'('' sub trvsph77:  ub(gridb)='',i3,10f7.1)')
c c c*                          (j,(ub(i,j),i=1,10),j=1,nlatb,nlatb-1)
      if (ler.ne.0) then
          print *,' sub trvsph: ler=', ler, lsvmin, lwkmin
c c c     stop
      endif

c c c print *,'sub trvsph77: lsvmin, lwkmin=', lsvmin, lwkmin

      if (iabs(jwave).ne.0) then

c truncate the data at a specified truncation

          call trcwav(igridb,nlatb,nlonb,ub,wsave,lsave,work,lwork
     +               ,dwork,ldwork,ker,jwave )
          call trcwav(igridb,nlatb,nlonb,vb,wsave,lsave,work,lwork
     +               ,dwork,ldwork,ker,jwave )
c c c     print *,'sub trcwav: ker=',ker
c c c     print *,'sub trcwav: nlatb,nlonb,jwave=',nlatb,nlonb,jwave  

      endif
      
      return
      end
c-----------------------------------------------------------------
      subroutine f2foshv (uoff,voff,ilon,jlat,ureg,vreg,jlat1,
     +                    work,lwork,wsave,lsave,ioff,ier)

c NCL: f2foshv (ureg,vreg,uoff,voff)

c vector: fixed (regular) grid to fixed-offset (equal spaced)

c nomenclature:
c .   goff   - output grid. Data are in a fixed-offset grid    
c .            with no wraparound or cyclic pts.
c .            eg: a 5 degree grid ==> goff(72,36)
c .            where ilon=72 and jlat=36.
c .   greg   - input grid. data are on a fixed grid 
c .            The output grid will have pole pts.
c .            eg: for a 5 deg grid  greg(72,36).


      integer    ilon,jlat,jlat1     ! input    
      real       uoff (ilon,jlat)
     *          ,voff (ilon,jlat)
      real       ureg(ilon,jlat1)
     *          ,vreg(ilon,jlat1)
      integer    ier                 ! output

      ier = 0
      if (ilon.lt.4 .or. jlat.lt.3) ier = 10
      if ((jlat1-jlat).ne.1)        ier = ier+100
      if (ier.ne.0) return

      call vshifti (ioff,ilon,jlat,lsave,wsave,ier)
      call vshifte (ioff,ilon,jlat,uoff,voff,ureg,vreg
     *             ,wsave,lsave,work,lwork,ier)

      return
      end
