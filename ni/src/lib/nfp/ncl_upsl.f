c -----------------------------------------------------------------------
      real function pslhy1 (pres,z,tv,xmsg)
      implicit none

c Compute sea level pressure (Pa) via the hypsometric eqn
          
c NCL: slp = pslhy1 (pres,z,tv)    

                           ! input
      real pres            ! pressure on lowest vertical level (Pa)
      real z               ! Geopotential hgt of lowest vertical level (m)
      real tv              ! virtual temperature (K)
      real xmsg            ! missing value 

                           ! output
c     real pslhy1          ! sea level pressure (Pa)

                           ! local
      real g               ! gravity (m/s2)
      real rdair           ! gas const dry air (j/{kg-k})
      data g     /9.80665/ 
      data rdair /287.04/  

      if (pres.ne.xmsg .and. z.ne.xmsg .and. tv.ne.xmsg) then ! pres_msg, zmsg, tvmsg
         pslhy1 = pres*exp(g*z/(rdair*tv))
      else
         pslhy1 = xmsg                        ! pres_msg
      endif
          
      return
      end
c -----------------------------------------------------------------------
      real function pslhy2 (pres,z,t,w,xmsg)
      implicit none

c Compute sea level pressure (Pa) via the hypsometric eqn
          
c NCL: slp = pslhy2 (pres,z,t,w)

                           ! input
      real pres            ! pressure on lowest vertical level (Pa)
      real z               ! Geopotential hgt of lowest vertical level (m)
      real t               ! temperature (K)
      real w               ! mixing ratio (kg/kg) [specific hum couls also be used] 
      real xmsg            ! missing value 

                           ! output
c     real pslhy2          ! sea level pressure (Pa)

                           ! local
      real tv              ! virtual temperature (K)
      real g               ! gravity (m/s2)
      real rdair           ! gas const dry air (j/{kg-k})
      data g     /9.80665/ 
      data rdair /287.04/  

      if (pres.ne.xmsg .and. z.ne.xmsg .and. 
     *    t.ne.xmsg .and. w.ne.xmsg) then ! pres_msg, zmsg, tvmsg, wmsg
          tv     = t*(1.+0.608*w) 
          pslhy2 = pres*exp(g*z/(rdair*tv))
      else
          pslhy2 = xmsg                        ! pres_msg
      endif
          
      return
      end
c ----------------------------------------------------------------------------
      subroutine pslec (t, phis, ps, pres, mlon, nlat, psl)
      implicit none

c Compute sea level pressure using the ECMWF formulation.
c Uses same routine as CCM Processor's derived field: PSLECMWF

c NCL: slp = pslec (t,phis,ps,pres) 
                                     ! input
      integer mlon                   ! Number of longitude values
      integer nlat                   ! Number of latitude values

      real t(mlon,nlat)              ! Temperature of lowest vertical level (K)
      real phis(mlon,nlat)           ! Surface geopotential (m^2/s^2)
      real ps(mlon,nlat)             ! Surface pressure (Pascals)
      real pres(mlon,nlat)           ! Pressure on lowest vertical level (Pa)

                                     ! output
      real psl(mlon,nlat)            ! Sea Level Pressure (Pa)

                                     ! local
      integer nl                     ! latitude counter


c The CCM Processor routine cpslec processes one latitude slice at a time.

      do nl = 1,nlat
         call cpslec(pres(1,nl), phis(1,nl), ps(1,nl), t(1,nl)
     *              ,mlon, mlon, 1, psl(1,nl))
      end do

      return
      end
c ----------------------------------------------------------------------------
      subroutine pslhor(z, t, phis, ps, pres, lats, mlon,nlat,klev,psl,
     +                  pslu, zx, tx, presx)
      implicit none

c Compute sea level pressure (Pa)  using the ECMWF formulation and
c     Trenberth's horizontal correction.

c Uses same routine as CCM Processor's derived field: PSLHOR2.

c NCL: slp = pslhor (z,t,phis,ps,pres,lats) 

                                       ! input
      integer mlon                     ! Number of longitude values
      integer nlat                     ! Number of latitude values
      integer klev                     ! Number of vertical levels
    
      real z(mlon,nlat,klev)           ! Geopotential Height Units: meters
      real t(mlon,nlat,klev)           ! Temperature (K)
      real phis(mlon,nlat)             ! Surface geopotential (m^2/s^2)
      real ps(mlon,nlat)               ! Surface pressure (Pa)
      real pres(mlon,nlat,klev)        ! Pressure (Pa) 
      real lats(nlat)                  ! Latitudes (degrees)
    
                                       ! output
      real psl(mlon,nlat)              ! Sea Level Pressure (Pa)
    
                                       ! local (arrays auto allocation)
      integer ml,nl,kl                 ! loop counters
      real pslu(mlon,nlat)             ! psl from PSLEC, uncorrected
      real zx(mlon,klev)               ! temp
      real tx(mlon,klev)               ! temp
      real presx(mlon,klev)            ! temp


c The CCM Processor routine cpslec processes one latitude slice at a time.

      kl = 1
      do nl=1,nlat
         call cpslec(pres(1,nl,kl), phis(1,nl), ps(1,nl), t(1,nl,kl)       
     *              ,mlon, mlon, 1, pslu(1,nl) )
      end do

c Do the horizontal correction
c The CCM Processor routine cpslhor2 processes one latitude slice at a time
c   and expects data from top to bottom.

       do nl=1,nlat

        do kl=1,klev
         do ml=1,mlon
            zx(ml,kl)    = z(ml,nl,klev+1-kl)    ! must be in reverse order
            tx(ml,kl)    = t(ml,nl,klev+1-kl)    ! must be in reverse order
            presx(ml,kl) = pres(ml,nl,klev+1-kl) ! must be in reverse order
         enddo
        enddo

        call cpslhor2 (presx,zx,phis(1,nl),pslu(1,nl),ps(1,nl),tx
     *                ,mlon,mlon,klev,lats(nl),psl(1,nl) )
      end do

      return
      end
c ----------------------------------------------------------------------------
      subroutine cpslec(pres    ,phis    ,ps    ,t      ,idim   ,
     $                  imax    ,kmax    ,psl    )
c      
c       File:   /crestone/u0/ccmproc2/src/src/cpslec.F  (2312 bytes) 
c       Date:   Wed Dec  1 16:07:19 1993
c       Author: Lawrence Buja <ccmproc2@sunny>
c      
c       From:   inderf
c       Calls:  ** none **
c       Blocks: ** none **
c
c-----------------------------------------------------------------------
c     
c       CCM2 hybrid coord version using ECMWF formulation
c       Purpose:   Compute sea level pressure for a latitude line
c       Algorithm: See section 3.1.b in NCAR NT-396 "Vertical 
c                  Interpolation and Truncation of Model-Coordinate Data
c      
c-----------------------------Arguments---------------------------------
c
c Input
      real pres(idim,kmax) ! Atmospheric pressure (pascals)
      real phis(idim)      ! Surface geopotential (m**2/sec**2)
      real ps(idim)        ! Surface pressure (pascals)
      real T(idim,kmax)    ! Vertical slice of temperature (top to bot)
      integer idim         ! First dimension of phis, ps, and t
      integer imax         ! Number of longitude points to compute
      integer kmax         ! Number of levels in t (2nd dimension)
c
c Output
c
      real psl(idim)    ! Sea level pressures (pascals)
c
c-----------------------------Parameters--------------------------------
c     
      parameter (rd=287.04,g=9.80616,xlapse=6.5e-3)
      parameter (alpha=rd*xlapse/g)
c
c-----------------------------------------------------------------------
c
      do i=1,imax
        Tstar=T(i,kmax)*(1.+alpha*(ps(i)/pres(i,kmax)-1.))! pg 7 eq 5
  
        T0=Tstar + xlapse*phis(i)/g                         ! pg 8 eq 13
  
        if (Tstar.le.290.5  .and. T0.gt.290.5) then       ! pg 8 eq 14.1
          alph=rd/phis(i)*(290.5-Tstar)  
        else if (Tstar.gt.290.5  .and. T0.gt.290.5) then ! pg 8 eq 14.2
          alph=0.
          Tstar= 0.5 * (290.5 + Tstar)  
        else  
          alph=alpha  
        endif  
        if (Tstar.lt.255.) then  
           Tstar= 0.5 * (255. + Tstar)                    ! pg 8 eq 14.3
        endif
        beta = phis(i)/(rd*Tstar)
        psl(i)=ps(i)*exp( beta*(1.-alph*beta/2.+((alph*beta)**2)/3.))
      enddo
      return
      end
c ----------------------------------------------------------------------------
      subroutine cpslhor2(pres    ,z      ,phis    ,psl     ,ps    , 
     $                   t      ,idim    ,imax    ,kmax    ,dlat  ,
     $                   pslhor    )
c      
c       File:   /crestone/u0/ccmproc2/src/src/cpslhor.F  (12843 bytes) 
c       Date:   Wed Dec  8 13:42:48 1993
c       Author: Lawrence Buja <ccmproc2@sunny>
c      
c       From:   inderf
c       Calls:  ** none **
c       Blocks: ** none **
c
c-----------------------------------------------------------------------
c     
c       PSL via Trenberth's horizontal formulation
c       Purpose:   Compute sea level pressure for a latitude line
c       Algorithm: 
c                  
c Designation:  --Low Area----|-------High Area------|---Low Area--
c                               ___________________
c                              /                   
c                             /_ _ _ _ _ _ _ _ _ _ _
c                            /      2000 meters      
c                           /                         
c Surface:      ------------                           ------------ 
c Gridpoints:        X     X     X     X     X     X     X     X
c Counters:          A     B     C     D     E     F     G     H
c
c-----------------------------Arguments---------------------------------
c
c Input
      real pres(idim,kmax) ! Atmospheric pressure (pascals)
      real z(idim,kmax)    ! Geopotential of hybrid levels (m**2/sec**2)
      real phis(idim)      ! Surface geopotential (m**2/sec**2)
      real psl(idim)       ! Sea level pressures (pascals)
      real ps(idim)        ! Surface pressure (pascals)
      real T(idim,kmax)    ! Vertical slice of temperature (top to bot)
      integer idim         ! First dimension of phis, ps, and t
      integer imax         ! Number of longitude points to compute
      integer kmax         ! Number of levels in t (2nd dimension)
      real dlat            ! Current latitude (-90. <= dlat <= +90.00)
c
c Output
c
      real pslhor(idim)    ! PSL with Horiz correction (pascals)
c
c-----------------------------Parameters--------------------------------
c     
      parameter (rd=287.04,g=9.80616,xlapse=6.5e-3)
      parameter (alpha=rd*xlapse/g)
c
c--------------------------Local variables------------------------------
c
      integer iendpts(10,2)  ! Surrounding end points (2 on each bdry)
                             ! if region lon1 thru lon2 is > 2000meters
                             !  1st dim: Assume there are multiple high
                             !           areas on 1 latitude line.
                             !  2nd dim: (N,1)= 1st  lon of high area N
                             !           (N,2)= Last lon of high area N
      integer nend           ! Counter for first dimension of iendpts.
      logical Lstart         ! .True. = looking for high area
                             ! .False.= looking for low area
c
c-----------------------------------------------------------------------
c
      Zmin   = 2000.    ! Min height of a "high" area.
      Zmid   = 1250.    ! Max height to use PSL data for interpolation
      nend   = 0
      Lstart = .true.
      lastindex = 0
      do i=1,imax
        if (phis(i)/g .gt. Zmin  .and. ! This a high area and
     $      dlat .gt. -60.00   ) then  ! Latitude is .gt. -60.00
          if (Lstart) then             ! Is this 1st long of high area?
            if (i-lastindex.gt.3) then ! > 2 points away from last area
              nend = nend + 1          ! Increment counter.
              iendpts(nend,1) = i      ! Set beginning point index (i1)
              Lstart = .false.         ! In High area, search for low area
            else                       ! < 2 points from last high area.
              Lstart = .false.         !  merge last area into this area
            endif
          endif 
        else                         ! This is a low area
          pslhor(i) = psl(i)         ! PSL is not modified.
          if (.not. Lstart) then     ! 1st long of low area
            Lstart = .true.          ! In low area, search for high area
            iendpts(nend,2) = i - 1  ! Set end point index (i2)
            lastindx = i - 1
          endif 
        endif 
      enddo 
 
      do n = 1,nend
        i1 = iendpts(n,1)            ! Beginning index of high area
        i2 = iendpts(n,2)            ! Last index of high area
        do while (phis(i1)/g .gt. Zmid)
          i1 = i1 - 1
        enddo
        do while (phis(i2)/g .gt. Zmid)
          i2 = i2 + 1
        enddo
        i1 = i1 + 1
        i2 = i2 - 1
        do i=i1,i2                   ! loop over longitude from C to F
          do lev=1,kmax   ! Z Foes from 1=top to kmax=bottom
            if (z(i1-1,lev).gt.phis(i)/g)  levA=lev
            if (z(i2+1,lev).gt.phis(i)/g)  levF=lev
          enddo
c
c  zA is hgt of PHIS(i1) at point i1 minus 2. 
c
          zA = ( phis(i)/g      - z(i1-1,levA+1) ) /
     $         ( z(i1-1,levA)   - z(i1-1,levA+1) )
          zF = ( phis(i)/g      - z(i2+1,levF+1) ) /
     $         ( z(i2+1,levF)   - z(i2+1,levF+1) )
c
c PA is Pressure at the 2000 meter level at point i1 minus 2
c        (pres goes from 1=top down to kmax=bottom)
c
          pA = exp( log( pres(i1-1,levA+1) ) -
     $              log( pres(i1-1,levA+1) / pres(i1-1,levA) )*zA )  
          pF = exp( log( pres(i2+1,levF+1) ) -
     $              log( pres(i2+1,levF+1) / pres(i2+1,levF) )*zF )  
          
          dpA = psl(i1-1) - pA
          dpF = psl(i2+1) - pF
          factor = float(i - i1 ) /float( i2 - i1)
          pslhor(i) = ps(i) + ( factor * dpF + (1 - factor) * dpA )  
        enddo
      enddo

      return
      end

C======================================================================

      subroutine cpslhor(pres    ,z      ,phis    ,psl     ,ps    , 
     $                   t      ,idim    ,imax    ,kmax    ,dlat  ,
     $                   pslhor    )
c      
c       File:   /crestone/u0/ccmproc2/src/src/cpslhor.F  (6747 bytes) 
c       Date:   Tue Dec  7 13:52:48 1993
c       Author: Lawrence Buja <ccmproc2@sunny>
c      
c       From:   inderf
c       Calls:  ** none **
c       Blocks: ** none **
c
c-----------------------------------------------------------------------
c     
c       PSL via Trenberth's horizontal formulation
c       Purpose:   Compute sea level pressure for a latitude line
c       Algorithm: 
c                  
c Designation:  --Low Area----|-------High Area------|---Low Area--
c                               ___________________
c                              /                   
c                             /_ _ _ _ _ _ _ _ _ _ _
c                            /      2000 meters      
c                           /                         
c Surface:      ------------                           ------------ 
c Gridpoints:        X     X     X     X     X     X     X     X
c Counters:          A     B     C     D     E     F     G     H
c
c-----------------------------Arguments---------------------------------
c
c Input
      real pres(idim,kmax) ! Atmospheric pressure (pascals)
      real z(idim,kmax)    ! Geopotential of hybrid levels (m**2/sec**2)
      real phis(idim)      ! Surface geopotential (m**2/sec**2)
      real psl(idim)       ! Sea level pressures (pascals)
      real ps(idim)        ! Surface pressure (pascals)
      real T(idim,kmax)    ! Vertical slice of temperature (top to bot)
      integer idim         ! First dimension of phis, ps, and t
      integer imax         ! Number of longitude points to compute
      integer kmax         ! Number of levels in t (2nd dimension)
      real dlat            ! Current latitude (-90. <= dlat <= +90.00)
c
c Output
c
      real pslhor(idim)    ! PSL with Horiz correction (pascals)
c
c-----------------------------Parameters--------------------------------
c     
      parameter (rd=287.04,g=9.80616,xlapse=6.5e-3)
      parameter (alpha=rd*xlapse/g)
c
c--------------------------Local variables------------------------------
c
      integer iendpts(10,2)  ! Surrounding end points (2 on each bdry)
                             ! if region lon1 thru lon2 is > 2000meters
                             !  1st dim: Assume there are multiple high
                             !           areas on 1 latitude line.
                             !  2nd dim: (N,1)= 1st  lon of high area N
                             !           (N,2)= Last lon of high area N
      integer nend           ! Counter for first dimension of iendpts.
      logical Lstart         ! .True. = looking for high area
                             ! .False.= looking for low area
c
c-----------------------------------------------------------------------
c
      Zmin   = 2000.   
      nend   = 0
      Lstart = .true.
      lastindex = 0
      do i=1,imax
        if (phis(i)/g .gt. Zmin  .and. ! This a high area and
     $      dlat .gt. -60.00   ) then  ! Latitude is .gt. -60.00
          if (Lstart) then             ! Is this 1st long of high area?
            if (i-lastindex.gt.3) then ! > 2 points away from last area
              nend = nend + 1          ! Increment counter.
              iendpts(nend,1) = i      ! Set beginning point index (i1)
              Lstart = .false.         ! In High area, search for low area
            else                       ! < 2 points from last high area.
              Lstart = .false.         !  merge last area into this area
            endif
          endif 
        else                         ! This is a low area
          pslhor(i) = psl(i)         ! PSL is not modified.
          if (.not. Lstart) then     ! 1st long of low area
            Lstart = .true.          ! In low area, search for high area
            iendpts(nend,2) = i - 1  ! Set end point index (i2)
            lastindx = i - 1
          endif 
        endif 
      enddo 
 
      do n = 1,nend
        i1 = iendpts(n,1)            ! Beginning index of high area
        i2 = iendpts(n,2)            ! Last index of high area
        do i=i1,i2                   ! loop over longitude from C to F
          do lev=1,kmax   ! Z goes from 1=top to kmax=bottom
            if (z(i1-2,lev).gt.phis(i)/g)  levA=lev 
            if (z(i1-1,lev).gt.phis(i)/g)  levB=lev
            if (z(i2+1,lev).gt.phis(i)/g)  levG=lev
            if (z(i2+2,lev).gt.phis(i)/g)  levH=lev
          enddo
c
c  zA is hgt of PHIS(i1) at point i1 minus 2. 
c
          zA = ( phis(i)/g      - z(i1-2,levA+1) ) /
     $         ( z(i1-2,levA)   - z(i1-2,levA+1) )
          zB = ( phis(i)/g      - z(i1-1,levB+1) ) /
     $         ( z(i1-1,levB)   - z(i1-1,levB+1) )
          zG = ( phis(i)/g      - z(i2+1,levG+1) ) /
     $         ( z(i2+1,levG)   - z(i2+1,levG+1) )
          zH = ( phis(i)/g      - z(i2+2,levH+1) ) /
     $         ( z(i2+2,levH)   - z(i2+2,levH+1) )
c
c PA is Pressure at the 2000 meter level at point i1 minus 2
c        (pres goes from 1=top down to kmax=bottom)
c
          pA = exp( log( pres(i1-2,levA+1) ) -
     $              log( pres(i1-2,levA+1) / pres(i1-2,levA) )*zA )  
          pB = exp( log( pres(i1-1,levB+1) ) -
     $              log( pres(i1-1,levB+1) / pres(i1-1,levB) )*zB )  
          pG = exp( log( pres(i2+1,levG+1) ) -
     $              log( pres(i2+1,levG+1) / pres(i2+1,levG) )*zG )  
          pH = exp( log( pres(i2+2,levH+1) ) -
     $              log( pres(i2+2,levH+1) / pres(i2+2,levH) )*zH )  
          dpA = psl(i1-2) - pA
          dpB = psl(i1-1) - pB
          dpG = psl(i2+1) - pG
          dpH = psl(i2+2) - pH
          dpAB = ( dpA + dpB ) * 0.5
          dpGH = ( dpG + dpH ) * 0.5
c          if (Lprint) then
c             print *,' cpslhor.100: Levs= ',levA,levB,levG,levH
c             print *,' cpslhor.101: Zs=   ',zA,zB,zG,zH
c             print *,' cpslhor.102: Ps=   ',pA,pB,pG,pH
c             print *,' cpslhor.103: dps=  ',dpA,dpB,dpG,dpH,dpAB,dpGH
c           endif
          factor = ( i - i1 + 1.5 ) / ( i2 - i1 + 3)
          pslhor(i) = ps(i) + ( factor * dpGH + (1 - factor) * dpAB )  
        enddo
      enddo
      return
      end
