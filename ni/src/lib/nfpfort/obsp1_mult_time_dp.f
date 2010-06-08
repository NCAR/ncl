C NCLFORTSTART
      subroutine dobjanlx(plon,plat,pval,ntim,npts,grid,mlon,nlat
     &                   ,xmsg,pmsg ,rscan,nscan,glat,glon,smwgt,opt
     &                   ,zval,zlat,zlon,ip,work,lwork,ier)        
      implicit none
c                                                                               
c Objective Analysis via 'iterative improvement'  [driver]
c
c NCL: grid = objanalii(plon,plat,pval,rscan,glat,glon,opt)
c                                                                               
c nomenclature :                                                                
c .   plat,plon    - coordinates of observation in degrees                      
c .                  -90.<= plat =>90. ; {-180, 0}<= plon >=(180, 360}
c .   pval         - value of obs. [*] or ... [ntim][*]                                              
c .   grid         - array which will hold the interpolated grid                
c .   mlon,nlat    - no. of lon and lat points                                  
c .   glon,glat    - vectors containing the lat/lon coords of the grid          
c .                  longitudes must have same range as plon
c .   rscan        - vector containing scan radii (in degrees)                  
c .                  rscan(1)> rscan(2)> ...>rscan(nscan)                       
c .   nscan        - no. of scans to be performed (length of rscan)             
c .                  it is recommended that multiple scans be                   
c .                  performed. the max number of scans is currently            
c .                  10. this may easily be adjusted by changing the            
c .                  parameter nscmax and changing the length of the            
c .                  data statement for smwgt. this routine was tested          
c .                  with nscan=4 : rscan(1,2,3,4)=20.,15.,10.,7.5              
c .                  rscan(nscan) should be .ge. the grid spacing.              
c .   xmsg         - missing code for plat and plon
c .   pmsg         - missing code for pval
c .                  value to which grid points with no data                    
c .                  within rscan(1) degrees will be set                        
c .   opt          - option flag
c .   ier          - error code                                                 
c                                                                               
c                                                   INPUT
      integer   ntim,npts,mlon,nlat,nscan,lwork,ier
      double precision  plat(npts), plon(npts), pval(npts,ntim)
      double precision  rscan(nscan),glat(nlat),glon(mlon)
      double precision  smwgt(nscan), xmsg, pmsg
      integer ip(npts)
      double precision zval(npts,ntim),zlat(npts),zlon(npts)
      double precision work(lwork)
      logical   opt
c                                                   OUTPUT
      double precision  grid(mlon,nlat,ntim) 
C NCLEND
c                                                   DYNAMIC
c      double precision, allocatable, dimension(:,:) :: zval
c      double precision, allocatable, dimension(:)   :: zlat, zlon
c      integer, allocatable, dimension(:)            :: ip
c                                                   DEFAULT SETTING
      integer nscmax, kpts, k, n, nt, i, ker 
      parameter (nscmax=10)                                   
c
c 'smwgt' is being passed in by C driver here, so this 
c  code is commented out.
c
c      double precision smwgt(nscmax)
c completely arbitrary. use for 'blending' previous and current values
c      data smwgt/1.0d0, 0.85d0, 0.70d0, 0.50d0, 6*0.25d0/             

c sort the 'plat' into ascending latitude order: ip = permutation vector

C allocate this inside NCL C wrapper.
c      allocate (ip(npts), stat=ker)
      call dpsort(plat, npts, ip, 1, ker)

c eliminate bad or missing locations

C allocate these inside NCL C wrapper.
C      allocate (zlat(npts), zlon(npts), zval(npts,ntim), stat=ker)
      k = 0
      do n=1,npts
         i = ip(n)
         if (plat(i).ne.xmsg .and. plon(i).ne.xmsg .and. 
     &       abs(plat(i)).le.90.0d0 .and. plon(i).ge.-180.0d0 .and.
     &       plon(i).le.360.0d0 ) then
             k = k+1
             zlat(k) = plat(i)
             zlon(k) = plon(i)
             do nt=1,ntim
                zval(k,nt) = pval(i,nt)
             end do
         end if
      end do
      kpts = k

c opt if (opt .and. isatt(opt,"blend_wgt")) then
c opt     ier = 0
c opt     do n=1,nscan
c opt        smwgt(n) = opt@blend_wgt(n)
c opt        if (smwgt(n).lt.0.0do .or. smwgt(n).gt.1.0d0) ier = -77
c opt     end do
c opt     if (ier.lt.0) return
c opt end if

C allocate this inside NCL C wrapper.
C      lwork = 2*kpts
C      allocate (work(lwork), stat=ker)

      call dobjanl(zlon,zlat,zval,ntim,kpts,grid,mlon,nlat,pmsg
     &            ,rscan,nscan,work,lwork,glat,glon,smwgt,ier)

      return
      end
c -------------------------------------------------------------------
      subroutine dobjanl(plon,plat,pval,ntim,npts,grid,mlon,nlat,xmsg 
     &                  ,rscan,nscan,work,lwork,glat,glon,smwgt,ier) 
      implicit none
c                                                                               
c Objective Analysis vis 'iterative improvement'
c
c NCL: grid = objanal_ii(plon,plat,pval,rscan,glat,glon,opt)
c                                                                               
c nomenclature :                                                                
c .   plat,plon    - coordinates of observation in degrees                      
c .                  -90.<= plat =>90. ; {-180, 0}<= plon >=(180, 360}
c .   pval         - value of obs. [*] or ... [ntim][*]  

c NOTE: this assumes the input triplet is in ascending latitude order

c .   grid         - array which will hold the interpolated grid                
c .   mlon,nlat    - no. of lon and lat points                                  
c .   glon,glat    - vectors containing the lat/lon coords of the grid          
c .                  longitudes must have same range as plon
c .   rscan        - vector containing scan radii (in degrees)                  
c .                  rscan(1)> rscan(2)> ...>rscan(nscan)                       
c .   nscan        - no. of scans to be performed (length of rscan)             
c .                  it is recommended that multiple scans be                   
c .                  performed. the max number of scans is currently            
c .                  10. this may easily be adjusted by changing the            
c .                  parameter nscmax and changing the length of the            
c .                  data statement for smwgt. this routine was tested          
c .                  with nscan=4 : rscan(1,2,3,4)=20.,15.,10.,7.5              
c .                  rscan(nscan) should be .ge. the grid spacing.              
c .   xmsg         - value to which grid points with no data                    
c .                  within rscan(1) degrees will be set                        
c .   work         - work vector of length 2*npts                               
c .   lwork        - length of work vector = 2*npts or greater
c .   smwgt        - vector of length nscan containing blending weights 
c .                  0.0 < smwgt <=1.0
c .   iopt         - future option flag
c .   ier          - error code                                                 
c                                                                               
      integer   ntim,npts,mlon,nlat,nscan,lwork,iopt,ier
      double precision  plat(npts), plon(npts), pval(npts,ntim), xmsg 
      double precision  rscan(nscan),work(lwork),glat(nlat),glon(mlon)
      double precision  grid(mlon,nlat,ntim) 
      double precision  smwgt(nscan)
                                                               
      integer nscmax, n, nstrt, nlast, ns, nt, nl, ml, nptlat, nptscan  
      parameter (nscmax=10)                                   
      integer  nshold(nscmax) , nlhold(nscmax)                 
      double precision  coef, rad, radinv, sglat, cglat
     &                , wgt, toplat, botlat, dumy, swgt, val

                                                                                
      ier = 0                                             
      if (npts .lt.2) ier = ier+1                       
      if (mlon .lt.2 .or. nlat.lt.2) ier = ier+10        
      if (nscan.lt.1 .or. nscan.gt.nscmax) ier = ier+100
      if (ier.ne.0) return                             

      if (nscan.gt.1) then
          do n=2,nscan
             if (rscan(n).ge.rscan(n-1)) ier = -88 
          end do
          if (ier.lt.0) return
      end if
c                                                                               
      rad    = 4.0d0*atan(1.)/180.0d0                          
      radinv = 1.0d0/rad                                
c                                                                               
c set all the output grid points to msg                                                
c                                                                               
      do nt=1,ntim 
        do nl=1,nlat 
          do ml=1,mlon 
             grid(ml,nl,nt) = xmsg
          end do
        end do
      end do
c                                                                               
c circulate thru each grid pt                                                    
c                                                                               
      do nl=1,nlat                
         sglat = sin(glat(nl)*rad)      
         cglat = cos(glat(nl)*rad)     
c                                                                               
c isolate the observations within rscan(1,2,..nscan) deg of this lat            
c .   note:  1<=nstrt <=nlast <=npts                                            
c                                                                               
         nstrt = 1            
         nlast = npts        

        do ns=1,nscan          
           nshold(ns) = 0        
           nlhold(ns) = 0      

           toplat     = glat(nl)+rscan(ns)     
           botlat     = glat(nl)-rscan(ns)      
          do n=1,npts                        
             if (plat(n).gt.toplat) go to 18     
             if (plat(n).ge.botlat) then
                 if (nshold(ns).eq.0) nshold(ns) = n
                 nlhold(ns) = n                    
             end if
          end do
   18     continue
c c c     write(*,'(3f10.2,3i5)') botlat,glat(nl),toplat
c c c&                          , ns,nshold(ns),nlhold(ns)
        end do                      
   21   continue                   
        
c only do lon loop if there are obs within the lat bounds

      if (nshold(1).gt.0) then
        do ml=1,mlon            
           nstrt  = nshold(1)      
           nlast  = nlhold(1)     
           nptlat = nlast-nstrt+1     
c                                                                               
c determine the distance of all obs in this latitude band
c .   from the current grid point in degrees of latitude           
c                                                                               
          do n=nstrt,nlast              
             dumy    = sglat*sin(plat(n)*rad) + cglat*cos(plat(n)*rad)  
     &               * cos((plon(n)-glon(ml))*rad) 
             work(n) = acos( min(max( -1.0d0,dumy),1.0d0) )*radinv
c c c        work(n) = acos( min(max( -1.0d0,
c c c&                 sglat*sin(plat(n)*rad) + cglat*cos(plat(n)*rad)  
c c c&               * cos((plon(n)-glon(ml))*rad) 
c c c&                ),1.0d0) )*radinv
c c c        print * ,
c c c&       nl,ml,n,plat(n),plon(n),glat(nl),glon(ml),work(n)
          end do
c                                                                               
c use successively smaller scans to analyze smaller scale features               
c                                                                               
          do ns=1,nscan                    
             if (nshold(ns).gt.0) then
                 coef    = -4.0d0/rscan(ns)**2         
                 nstrt   = nshold(ns)              
                 nlast   = nlhold(ns)             
                 nptscan = nlast-nstrt+1      
c                                                                               
c compute the weights;  sum of wgt*obs; wgted average                           
c                                                                               
                 do nt=1,ntim
                    swgt = 0.0d0
                   do n=nstrt,nlast
                      if (work(n).gt.rscan(ns) .or.
     &                    pval(n,nt).eq.xmsg) then
                          work(n+npts) = 0.0d0
                      else
                          wgt  = exp(coef*work(n)**2)
                          work(n+npts) = pval(n,nt)*wgt         
                          swgt = swgt + wgt
                      end if
                   end do
 
                   if (swgt.gt.0.0d0) then
                       val = 0.0d0
                       do n=nstrt,nlast
                          val = val + work(npts+n)
                       end do
                       val = val/swgt                    
c                                                       ; blend
                       if (grid(ml,nl,nt).eq.xmsg) then     
                           grid(ml,nl,nt) = val
                       else
                           grid(ml,nl,nt) = smwgt(ns)*val         
     &                               +(1.0d0-smwgt(ns))*grid(ml,nl,nt) 
                       end if
                   end if
                 end do
             end if                                                
          end do
        end do
      end if

  100   continue
      end do
c                                                                               
      return 
      end
