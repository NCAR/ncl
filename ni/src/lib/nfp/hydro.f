c ---------------------------------------------------------------
      subroutine hydro (p,tkv,zsfc,nlvl,zh,ier)
      implicit none

c NCL: zh = hydro (p,tkv,zsfc)

c use the hydrostatic eqn to get geopotential height
c .   it is assumed that p(1) and tkv(1) contain the
c .   surface quantities corresponding to zsfc.

c .   missing data not allowed
                                  ! input
      integer nlvl , ier          ! no. levels; error code 
      real p(nlvl)                ! pres (Pa)
     *   , tkv(nlvl)              ! temp (K) at each "p"
     *   , zsfc                   ! sfc geopotential height (gpm)
                                  ! output
      real zh(nlvl)               ! calculated geopotential (gpm)
                                  ! local
      integer nl 
      real    g, rdair, rdag, tvbar 

c     --------
      ier = 0
      if (nlvl.lt.1) then
          ier = -1             ! unrealistic nlvl value
          return
      endif
                               
      g     = 9.80665          ! gravity at 45 deg lat used by the WMO
      rdair = 287.04           ! gas const dry air (j/{kg-k})
      rdag  = rdair/g

c calculate geopotential height if initial z available [hydrostatic eq]

         zh(1)  = zsfc   
         do nl=2,nlvl
            tvbar  = (tkv(nl)*alog(p(nl)) + tkv(nl-1)*alog(p(nl-1)))
     *             /  alog(p(nl)*p(nl-1))  ! same as [ln(p(nl)+ln(p(nl-1)]     
            zh(nl) = zh(nl-1)+rdag*tvbar*alog(p(nl-1)/p(nl))
         enddo

      return
      end
