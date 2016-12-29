;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
;                  diagnostics_cam.ncl                             
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
; gsn_code.ncl, gsn_csm.ncl, contributed.ncl  must be preloaded.
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
; October 2010: changed code to use _n functions. Less memory.  
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
;
;-------------------------------------------------------------
; Decompose a variable into symmetric and asymmetric parts
;-------------------------------------------------------------
undef("decompose2SymAsym")
function decompose2SymAsym( z, iret[1] )
; supports: z(lat,lon), z(time,lat,lon), z(time,lev,lat,lon) 

; antisymmetric part is stored in one hemisphere [eg: Northern Hemisphere]  
;          xOut( lat) = (x(lat)-x(-lat))/2
; symmetric part is stored in other hemisphere [eg: Southern Hemisphere]
;          xOut(-lat) = (x(lat)+x(-lat))/2

local dimz, rankz, nlat, nl, N2, N, nt, kl, ntim, klev, dec2SymAsym, PRTFLG
begin

  undef("dec2SymAsym")                                    ; private to this bloc
  function dec2SymAsym( x[*][*], nlat[1], N2[1], N[1] )   ; x(lat,lon)
  ; ===> local to decompose2SymAsym [~ f90 'contains' w PRIVATE attribute]
  ; This does the decomposition: called from driver 'decompose2SymAsym'.
  local xsa, nl
  begin
    xsa   = (/ x /)              ; x(lat,lon)
    do nl=0,N2-1
       xsa(       nl,:) = 0.5*(x(nlat-1-nl,:) + x(nl,:))   ; SH => symmetric
       xsa(nlat-1-nl,:) = 0.5*(x(nlat-1-nl,:) - x(nl,:))   ; NH => asymmetric
    end do

    return( xsa )
  end
  ; ==== 

  dimz  = dimsizes( z )
  rankz = dimsizes( dimz )

  if (rankz.ge.5) then
      print("decompose2SymAsym: currently supports up to 4D: rank="+rankz+"D")
      exit
  end if

  if (rankz.eq.1) then
      nlat = dimz(0)
  else
      nlat = dimz(rankz-2)
  end if
  
  N2   = nlat/2
  N    = N2
  if ((nlat%2).eq.1) then
       N = N2+1             ; offset to handle the Equator
  end if

  zSymAsym = (/ z /)        ; return array has same size/shape/type
                            ; values will be overwritten
  if (rankz.eq.1) then
      do nl=0,N2-1
       zSymAsym(       nl) = 0.5*(z(nlat-1-nl) + z(nl))   ; SH => symmetric
       zSymAsym(nlat-1-nl) = 0.5*(x(nlat-1-nl) - x(nl))   ; NH => asymmetric
      end do
      return( zSymAsym )
  end if

  if (rankz.eq.2) then
      zSymAsym = dec2SymAsym( z, nlat, N2, N )
  end if

  if (rankz.eq.3) then
      ntim = dimz(0)
      do nt=0,ntim-1
         zSymAsym(nt,:,:) = dec2SymAsym( z(nt,:,:), nlat, N2, N )
      end do
  end if

  if (rankz.eq.4) then
      ntim = dimz(0)
      klev = dimz(1)
      do nt=0,ntim-1
        do kl=0,klev-1
           zSymAsym(nt,kl,:,:) = dec2SymAsym( z(nt,kl,:,:), nlat, N2, N ) 
        end do
      end do
  end if

  zSymAsym@long_name = "antisymmetric & symmetric (separate hemispheres)"
  if (isatt(z,"units")) then
      zSymAsym@units = z@units
  end if
  copy_VarCoords( z, zSymAsym)
  
  if (iret.eq.0) 
      return( zSymAsym )                      ; original order
  else
      dNam = getvardims( zSymAsym )
      if (rankz.eq.3) then
          return( zSymAsym($dNam(1)$|:,$dNam(2)$|:,$dNam(0)$|:)) ; (lat,lon,time)
      end if
      if (rankz.eq.4) then
                                      ; return (lev,lat,lon,time)
          return( zSymAsym($dNam(1)$|:,$dNam(2)$|:,$dNam(3)$|:,$dNam(0)$|:)) 
      end if
  end if
end

;-------------------------------------------------------------
; Prewhiten the data: eg remove the annual cycle.
; Actually, this will remove all time periods less than
;           ththise corresponding to 'fCrit'.
; Note: The original fortran code provided by JET did not remove
;       the grid point means so .... rmvMeans=False
;       I assume that Matt Wheeler's code did that also.
;-------------------------------------------------------------
undef("rmvAnnualCycle")
function rmvAnnualCycle( z[*][*][*], spd[1], nDayTot[1]:integer
                       , rmvMeans[1]:logical \
                       , fCrit[1], iret[1]   )  ; z(time,lat,lon)
local dimz, ntim, dNam, zt, cf, xbar
begin
  dimz = dimsizes( z )
  ntim = dimz(0)

  dNam = getvardims( z )                        ; input dimension names
;; original code start ====================
;;zt   = z($dNam(1)$|:,$dNam(2)$|:,$dNam(0)$|:) ; reorder: time fastest varying
;;cf   = ezfftf( zt )                           
;;cf!0 = "cmplx"                                ; dimension clarity
;;cf!1 = "lat"
;;cf!2 = "lon"
;;cf!3 = "freq"
;;cf(:,:,:,{:fCrit}) = 0.0
;; original code end    ====================

  cf   = ezfftf_n(z, 0)                         ; [2] x [ntim/2] x [nlat] x [mlon]             
  cf!0 = "cmplx"                                ; dimension clarity
  cf!1 = "freq"
  cf!2 = "lat"                                  ; dNam(1)
  cf!3 = "lon"                                  ; dNam(2)
 
  cf&cmplx = (/ 0,1 /)
  cf&lat   = z&$dNam(1)$                        ; dNam(1)
  cf&lon   = z&$dNam(2)$                        ; dNam(2)

                                                ; create freq dim values
  freq     = fspan(1,nDayTot*spd/2,nDayTot*spd/2)/nDayTot
  freq!0   = "freq"                             ; clarity
  freq@units = "cycles/day"
  cf&freq  = freq                               ; assign values, needed for {..}

  cf(:,{:fCrit},:,:) = 0.0                      ; cf at all freq < fcrit   = 0.0
                                                ; eg       < 3/365 [6/730]
  if (rmvMeans) then                            ; remove means for each lat,lon
                                                ; cf@xbar = 0.0
      xbar    = cf@xbar                         ; explicit retrieve
      xbar    = 0.0                             ; set all means = 0.0
      cf@xbar = xbar                            ; reassign new values
  end if
                                                ; reconstruct 
  z = (/ ezfftb_n(cf,cf@xbar,1) /)             ; (/ ... /) avoid meta warning msg

  if (iret.eq.0) then                           ; return in original order
      return( z )                               ; (time,lat,lon)
  else                                          ; return in permuted order
      return( z($dNam(0)$|:,$dNam(1)$|:,$dNam(2)$|:) ) ; (lat,lon,time)
  end if
end

;-------------------------------------------------------------
; Special reordering to resolve the Progressive and Retrogressive waves 
; Reference: Hayashi, Y. 
;    A Generalized Method of Resolving Disturbances into 
;    Progressive and Retrogressive Waves by Space and  
;    Fourier and TimeCross Spectral Analysis
;    J. Meteor. Soc. Japan, 1971, 49: 125-128.
;-------------------------------------------------------------

undef("resolveWavesHayashi")
function resolveWavesHayashi ( varfft[*][*][*], nDayWin[1], spd[1] )  ; (2,mlon,nSampWin)
;
; Create array PEE(NL+1,NT+1) which contains the (real) power spectrum.
; all the following assume indexing starting with 0
; In this array, the negative wavenumbers will be from pn=0 to NL/2-1;
; The positive wavenumbers will be for pn=NL/2+1 to NL.
; Negative frequencies will be from pt=0 to NT/2-1
; Positive frequencies will be from pt=NT/2+1 to NT  .
; Information about zonal mean will be for pn=NL/2  .
; Information about time mean will be for pt=NT/2  .
; Information about the Nyquist Frequency is at pt=0 and pt=NT
;
; In PEE, define the 
; WESTWARD waves to be either +ve frequency
;          and -ve wavenumber or -ve freq and +ve wavenumber.
; EASTWARD waves are either +ve freq and +ve wavenumber 
;          OR -ve freq and -ve wavenumber.

; Note that frequencies are returned from fftpack are ordered like so
;    input_time_pos [ 0    1   2    3     4      5    6   7  ]
;    ouput_fft_coef [mean 1/7 2/7  3/7 nyquist -3/7 -2/7 -1/7]  
;                    mean,pos freq to nyq,neg freq hi to lo
;
; Rearrange the coef array to give you power array of freq and wave number east/west
; Note east/west wave number *NOT* eq to fft wavenumber see Hayashi '71 
; Hence, NCL's 'cfftf_frq_reorder' can *not* be used.
;
; For ffts that return the coefficients as described above, here is the algorithm
; coeff array varfft(2,n,t)   dimensioned (2,0:numlon-1,0:numtim-1)
; new space/time pee(2,pn,pt) dimensioned (2,0:numlon  ,0:numtim  ) 
;
; NOTE: one larger in both freq/space dims
; the initial index of 2 is for the real (indx 0) and imag (indx 1) parts of the array
;
;
;      if  |  0 <= pn <= numlon/2-1    then    | numlon/2 <= n <= 1
;          |  0 <= pt < numtim/2-1             | numtim/2 <= t <= numtim-1
;
;      if  |  0         <= pn <= numlon/2-1    then    | numlon/2 <= n <= 1
;          |  numtime/2 <= pt <= numtim                | 0        <= t <= numtim/2
;
;      if  |  numlon/2  <= pn <= numlon    then    | 0  <= n <= numlon/2
;          |  0         <= pt <= numtim/2          | numtim/2 <= t <= 0
;
;      if  |  numlon/2   <= pn <= numlon    then    | 0        <= n <= numlon/2
;          |  numtim/2+1 <= pt <= numtim            | numtim-1 <= t <= numtim/2

local dimvf, numlon, N, varspacetime, pee, wave, freq
begin
  dimvf  = dimsizes( varfft )
  mlon   = dimvf(1)
  N      = dimvf(2)

  varspacetime = new((/2,mlon+1,N+1/),typeof(varfft),1e20)

  varspacetime(:,:mlon/2-1,:N/2-1) = varfft(:,mlon/2:1,N/2:N-1)
  varspacetime(:,:mlon/2-1,N/2:)   = varfft(:,mlon/2:1,:N/2)
  varspacetime(:, mlon/2: ,:N/2)   = varfft(:,:mlon/2,N/2:0)
  varspacetime(:, mlon/2: ,N/2+1:) = varfft(:,:mlon/2,N-1:N/2)
 
;  Create the real power spectrum pee = sqrt(real^2+imag^2)^2
 
  pee       = new((/mlon+1,N+1/),typeof(varfft),1e20)
  pee       = (sqrt(varspacetime(0,:,:)^2+varspacetime(1,:,:)^2))^2

            ; add meta data for use upon return
  pee!0     = "wave"
  pee!1     = "freq"
            ; JET fortran code 'wavep1'
  wave      = ispan(-mlon/2,mlon/2,1)  
  wave!0    = "wave"
  wave&wave =  wave
            ; JET fortran code 'frqfftwin'
  freq      = fspan(-1*nDayWin*spd/2,nDayWin*spd/2,nDayWin*spd+1)/nDayWin
  freq!0    = "freq"
  freq&freq =  freq

  pee&wave  =  wave
  pee&freq  =  freq

  return( pee )
end
;---------------------------------------------------------------
; dispersion curves
;--------------------------------------------------------------
undef("genDispersionCurves")
procedure genDispersionCurves( nWaveType[1]:integer      \
                             , nEquivDepth[1]:integer    \
                             , nPlanetaryWave[1]:integer \
                             , rlat[1]:numeric           \
                             , Ahe[*]:numeric            \
                             , Afreq[*][*][*]:numeric    \
                             , Apzwn[*][*][*]:numeric    )
local pi, re, g, omega, U, Un, ll, Beta, maxwn, ww, ed, wn, he \
    , T, L, s, k, kn, del, deif, n, i, eif, P, dps, R, Rdeg, fillval 
begin
    pi    = 4.0*atan(1.0)
    re    = 6.37122e06     ; [m]   average radius of earth
    g     = 9.80665        ; [m/s] gravity at 45 deg lat used by the WMO
    omega = 7.292e-05      ; [1/s] earth's angular vel
    U     = 0.0
    Un    = 0.0   ; since Un = U*T/L
    ll    = 2.*pi*re*cos(abs(rlat))
    Beta  = 2.*omega*cos(abs(rlat))/re
    maxwn = nPlanetaryWave
    fillval = 1e20
    
    do ww = 1, nWaveType      ; wave type 
    
      do ed = 1, nEquivDepth  ; equivalent depth
        he = Ahe(ed-1)
        T = 1./sqrt(Beta)*(g*he)^(0.25)
        L = (g*he)^(0.25)/sqrt(Beta)
    
        do wn = 1, nPlanetaryWave     ; planetary wave number
    
          s  = -20.*(wn-1)*2./(nPlanetaryWave-1) + 20.
          k  = 2.*pi*s/ll
          kn = k*L
    
          ; Anti-symmetric curves
    
          if (ww.eq.1) then              ; MRG wave
            if (k.lt.0.) then
              del  = sqrt(1.+(4.*Beta)/(k^2*sqrt(g*he)))
              deif = k*sqrt(g*he)*(0.5-0.5*del)
            end if
            if (k.eq.0.) then
              deif = sqrt(sqrt(g*he)*Beta)
            end if
            if (k.gt.0.) then
              deif = fillval
            end if
          end if
          if (ww.eq.2) then              ; n=0 IG wave
            if (k.lt.0.) then
              deif = fillval
            end if
            if (k.eq.0.) then
              deif = sqrt(sqrt(g*he)*Beta)
            end if
            if (k.gt.0.) then
              del  = sqrt(1.+(4.0*Beta)/(k^2*sqrt(g*he)))
              deif = k*sqrt(g*he)*(0.5+0.5*del)
            end if
          end if
          if (ww.eq.3) then              ; n=2 IG wave
            n=2.
            del  = (Beta*sqrt(g*he))
            deif = sqrt((2.*n+1.)*del + (g*he)*k^2)
            ; do some corrections to the above calculated frequency.......
            do i = 1,5
             deif = sqrt((2.*n+1.)*del + (g*he)*k^2 + g*he*Beta*k/deif)
            end do
          end if
    
    
          ; symmetric curves
          if (ww.eq.4) then              ; n=1 ER wave
            n=1.
            if (k.lt.0.) then
             del  = (Beta/sqrt(g*he))*(2.*n+1.)
             deif = -Beta*k/(k^2 + del)
            else
             deif = fillval
            end if
          end if
          if (ww.eq.5) then              ; Kelvin wave
            deif = k*sqrt(g*he)
          end if
          if (ww.eq.6) then              ; n=1 IG wave
            n=1.
            del  = (Beta*sqrt(g*he))
            deif = sqrt((2.*n+1.)*del + (g*he)*k^2)
            ; do some corrections to the above calculated frequency.......
            do i=1,5
              deif = sqrt((2.*n+1.)*del + (g*he)*k^2 + g*he*Beta*k/deif)
            end do
          end if
        
          eif  = deif  ; + k*U since  U=0.0
          P    = 2.*pi/(eif*24.*60.*60.)
          dps  = deif/k
          R    = L
          Rdeg = (180.*R)/(pi*6.37e6)
    
          Apzwn(ww-1,ed-1,wn-1) = s
          if (deif.ne.fillval) then
            P = 2.*pi/(eif*24.*60.*60.)
            Afreq(ww-1,ed-1,wn-1) = 1./P
          else
            Afreq(ww-1,ed-1,wn-1) = fillval
          end if
        end do
      end do
    end do
end
;-------------------------------------------------------------
; Graphics Utility: Add temporal info the freq[y]- wave[x] contour 
;                   freq is cpd [cycles per day]
;-------------------------------------------------------------
undef("addHorVertLines")
procedure addHorVertLines(wks[1]:graphic, plot[1]:graphic \
                         ,x1[*],x2[*],y1[*],y2[*],y3[*],y4[*] )
; freq [y] axis:  Add horizontal lines that explicitly
;                 print time in days. This assumes the units
;                 of the freq axis are "cpd" [cycles per day]
local gsres, txres
begin
    gsres = True
    gsres@gsLineDashPattern = 1

    gsn_polyline(wks, plot, x1,y1,gsres)
    gsn_polyline(wks, plot, x1,y2,gsres)
    gsn_polyline(wks, plot, x1,y3,gsres)
    gsn_polyline(wks, plot, x2,y4,gsres)

    txres        = True
    txres@txJust = "CenterLeft"
    txres@txFontHeightF = 0.013

    gsn_text(wks, plot, "3 days" ,-14.7,.345,txres)
    gsn_text(wks, plot, "6 days" ,-14.7,.175,txres)
    gsn_text(wks, plot, "30 days",-14.7,.045,txres)
end

;----------------------------------------------------------- 
; Utility: Unix does not allow spaces in file names
;          When 5.0.1 becomes available, delete this and
;          use replaceSingleChar in contributed.ncl
;----------------------------------------------------------- 
undef("replaceChars")
procedure replaceChars (s[1]:string, oldStr[1]:string, newStr[1]:string )
local cOld, cNew, c, i
begin
  cOld = stringtochar( oldStr )
  cNew = stringtochar( newStr )

  c    = stringtochar( s )
  i    = ind( c.eq.cOld(0)  )
  if (.not.(any(ismissing(i)))) then
      c(i) = cNew(0)
      s    = chartostring( c )
  end if
end

;----------------------------------------------------------- 
; Graphics Utility: This info may be of use if users want 
;                   to set there own plot limits
; Activated if opt=True and (opt@debug=True or opt@Fig_3_statInfo=True)
;----------------------------------------------------------- 
undef("statAsymSym")
procedure statAsymSym (x[*][*]:numeric, name[1]:string )
local statx, work, nwork
begin
   statx    = new ( 7, typeof(x), 1e20) 
   statx(0) = avg(x)                ; mean
   statx(1) = stddev(x)             ; std. deviation
   work     = ndtooned(x) 
   nwork    = dimsizes(work)  
   qsort( work )                    ; sort into ascending order
   statx(2) = min ( work )
   statx(3) = work( nwork/4  )      ; lower quartile
   statx(4) = work( nwork/2  )      ; approx median
   statx(5) = work( nwork*3/4)      ; upper quartile
   statx(6) = max ( work )

   print(" ")
   print("   ===> Statistics: "+name+" <===")
   print("        Mean="+statx(0)  ) 
   print("      StdDev="+statx(1)  ) 
   print("         Min="+statx(2)  ) 
   print(" lowQuartile="+statx(3)  ) 
   print("      Median="+statx(4)  ) 
   print("highQuartile="+statx(5)  ) 
   print("         Max="+statx(6)  ) 
   print(" ")
end
   

;==================================================================
; ====> Create Wheeler-Kiladis Space-Time  plots.  <====
;==================================================================


;==================================================================
; Note_1: The full logitudinal domain is used. 
;         This means that every planetary
;         wavenumber will be represented.
; Note_2: Tapering in time is done to make the variable periodic.
;         
; The calculations are also only made for the latitudes
; between '-latBound' and 'latBound'. 
;
;********************   REFERENCES  *******************************
; Wheeler, M., G.N. Kiladis 
;    Convectively Coupled Equatorial Waves: 
;    Analysis of Clouds and Temperature in the 
;    Wavenumber-Frequency Domain
;    J. Atmos. Sci., 1999,  56: 374-399.
;---
; Hayashi, Y.
;    A Generalized Method of Resolving Disturbances into
;    Progressive and Retrogressive Waves by Space and
;    Fourier and TimeCross Spectral Analysis
;    J. Meteor. Soc. Japan, 1971, 49: 125-128.
;==================================================================
undef ("wkSpaceTime")
procedure wkSpaceTime (x[*][*][*]:numeric                  \
                      ,diro[1]:string                      \
                      ,caseName[1]:string                  \
                      ,varName[1]:string                   \
                      ,latBound[1]:numeric                 \
                      ,spd[1]:integer                      \
                      ,nDayWin[1]:integer                  \
                      ,nDaySkip[1]:integer                 \
                      ,opt[1]:logical)

local latN, latS, lonL, lonR, spd, fCrit, tim_taper        \
    , lon_taper, pltType, debug, minwav4smth, maxfrq4plt   \
    , minfrq4plt, maxfrq4plt, minwav4plt, maxwav4plt       \
    , fillVal, nMsg, dimx, ntim, nlat, mlon, nDayTot       \
    , nSampTot, nSampWin, nSampSkip, nWindow, N, dNam, work\
    , rmvMeans, xAS, q, peeAS, nl, ntStrt, ntLast, nw, nt  \
    , ml, psumanti, psumsym, wv, wkdir, caseName, pltFilTit\
    , frqfftwin, wavep1, minfrq, maxfrq, tmFontHgtF, pltTit\
    , tiFontHgtF, lbFontHgtF, txFontHgtF, res, freq        \
    , wavenumber, NWVN, dcres, txres, rlat, Ahe            \
    , nWaveType, nPlanetaryWave, nEquivDepth, Apzwn, Afreq \
    , asym, sym, x1, x2, y1, y2, y3, y4, wks, plot         \
    , psumb, psumsym_nolog, psumanti_nolog,  tt, smthlen   \
    , i, pt8cpd, spec, nCn, nExtra

  begin

  if (typeof(x).eq."float" .or. typeof(x).eq."double") then
      print(" ")
  else
      print("wkSpaceTime: input variable must be type float or double")
      print("             input variable type is "+typeof(x))
      exit
  end if

  debug   = False    ; default
  if (opt .and. isatt(opt, "debug") ) then
      debug = opt@debug
  end if

  if (isatt(x,"_FillValue")) then   ; Check for _FillValue .... not allowed
      nMsg = num(ismissing(x))
      if (nMsg.gt.0) then
          print("nMsg="+nMsg+"  User must preprocess to remove _FillValue")
          print("               FFTs do not allow missing values!!       ")
          exit
      end if
      delete(x@_FillValue)     ; avoid warning messages from fft
  end if

  if (debug) then
      printVarSummary( x )
      printMinMax( x, True )
  end if

;-------------------------------------------------------------------
; x sizes and dimension names
;-------------------------------------------------------------------

  dimx = dimsizes(x)
  ntim = dimx(0)               ; total number of temporal samples
  nlat = dimx(1)
  mlon = dimx(2)

  dNam = getvardims( x )

;-------------------------------------------------------------------
; check to make sure that "x" has full days of data
;-------------------------------------------------------------------

  if ((ntim%spd).ne.0) then
      nExtra = ntim%spd          
      print("nExtra="+nExtra+" input array must have complete days only ")
      exit
  end if

;-------------------------------------------------------------------
; Make input arguments into "internal" variables
;-------------------------------------------------------------------
  latN    = latBound
  latS    =-latBound ; make symmetric about the equator

  lonL    = 0        ; -180
  lonR    = 360      ;  180

  fCrit   = 1./nDayWin  ; remove all contributions 'longer'

  tim_taper  = 0.1   ; time taper      [0.1   => 10%]  
  lon_taper  = 0.0   ; longitude taper [0.0 for globe; only global supported]  

;-------------------------------------------------------------------
; Check for not allowed actions
;-------------------------------------------------------------------

  if (lon_taper.gt.0.0 .or. (lonR-lonL).ne.360.) then
      print("Code does currently allow lon_taper>0 or (lonR-lonL)<360")
      exit
  end if

;-------------------------------------------------------------------
; OPTIONS
;-------------------------------------------------------------------
  pltType = "ps"     ; default
  if (opt .and. isatt(opt, "pltType") ) then
      if (any(opt@pltType.eq.(/"ps", "eps", "x11", "ncgm", "png"/))) then
          pltType = opt@pltType
      end if
  end if

  pltTit   = caseName+"_"+varName      
  pltTitle = pltTit+" LOG[Power: "+latBound+"S-"+latBound+"N]"
  if (opt .and. isatt(opt, "pltTitle") ) then
      pltTitle = opt@pltTitle
  end if

  pltFilTit = pltTit
  replaceChars( pltFilTit, " ", "_") ; spaces not allowed unix file names

  pltColorMap = "amwg_blueyellowred"
  if (opt .and. isatt(opt, "pltColorMap") ) then
      pltColorMap = opt@pltColorMap
  end if


;-------------------------------------------------------------------
; Create required temporal sampling variables
;-------------------------------------------------------------------

  nDayTot   = ntim/spd         ; # of days (total) for input variable 
  nSampTot  = nDayTot*spd      ; # of samples (total)      
  nSampWin  = nDayWin*spd      ; # of samples per temporal window
  nSampSkip = nDaySkip*spd     ; # of samples to skip between window segments
                               ;   neg means overlap
  nWindow   = (nSampTot-nSampWin)/(nSampWin+nSampSkip)  + 1
  N         =  nSampWin        ; convenience [historical]         

  if (nDayTot.lt.nDayWin) then
      print("nDayTot="+nDayTot+" is less the nDayWin="+nDayWin)
      print("        This is not allowed !!       ")
      exit
  end if

;-------------------------------------------------------------------
; Remove dominant signals
; (a) Explicitly remove *long term* linear trend
;     For consistency with JET code keep the grid point means.
;     This necessitates that 'dtrend_msg' be used because 'dtrend'
;     always removes the mean(s).
; (b) All variations >= approx 'nDayWin' days if full year available
;-------------------------------------------------------------------

  dNam = getvardims( x )
;;work = x($dNam(1)$|:,$dNam(2)$|:,$dNam(0)$|:)    ; reorder  (lat,lon,time)
;;work = dtrend( work , False )   ; remove mean + overall long term temporal trend 
;;work = dtrend_msg(ispan(0,ntim-1,1)*1.0, work, False, False)  ; remove just  trend
;;if (isatt(work,"_FillValue")) then
;;    delete(work@_FillValue)                  ; dtrend_msg adds this att
;;end if
                                                         ; replace with detrended
;;x    = (/ work($dNam(0)$|:,$dNam(1)$|:,$dNam(2)$|:) /) ; values (time,lat,lon)
;;delete(work)
  x    = dtrend_msg_n(ispan(0,ntim-1,1)*1.0, x, False, False, 0)  ; remove just trend
  if (isatt(x,"_FillValue")) then
      delete(x@_FillValue)                     ; dtrend_msg adds this att
  end if

  if (nDayTot.ge.365) then                     ; rmv dominant signals
      rmvMeans = False                         ; original code did not remove
      x = rmvAnnualCycle(x, spd, nDayTot, rmvMeans, fCrit, 0)
  end if

  if (debug) then
      print("===> Post removal of trend and signal <===")
      printVarSummary( x )                     ; (time,lat,lon)
      printMinMax( x, True )
  end if

;-------------------------------------------------------------------
; Decompose to Symmetric and Asymmetric parts
;-------------------------------------------------------------------
  xAS  = decompose2SymAsym( x , 1 )          ; create Asym and Sym parts
  if (debug) then
      printVarSummary(xAS)                   ; xAS(lat,lon,time) [iret=1]
      printMinMax(xAS, True)
  end if

;-------------------------------------------------------------------
; Because there is the possibility of overlapping *temporal* segments,
; we must use a less efficient approach and detrend/taper
; each window segment as it arises. 
;          t0   t1   t2   t3   t4  .................. t(N)
; lon(0):  x00  x01  x02  x03  x04 .................. x0(N)
;     :    :   :   :   :   :                     :
; lon(M):  xM0  xM1  xM2  xM3  xM4 .................. xM(N)
;-------------------------------------------------------------------
; q     - temporary array to hold the 2D complex results
;         for each longitude/time (lon,time) window that is fft'd.
;         This is one instance [realization] of space-time decomposition.
;
; peeAS - symmetric and asymmetric power values in each latitude hemisphere.
;         Add extra lon/time to match JET 
;-------------------------------------------------------------------
  q      = new((/2,mlon,nSampWin/)       ,typeof(xAS), "No_FillValue")
  peeAS  = new((/nlat,mlon+1,nSampWin+1/),typeof(xAS), "No_FillValue")
  peeAS  = 0.0                       ; initialize 

;-------------------------------------------------------------------
; Operate on the xAS array
; NCL does not have a complex 2D FFT at this time.
; Perform a "poorman's" complex 2D FFT by looping over time and space.
; Loop over all latitudes and then perform summing/averaging 
; on the spectral results.
;-------------------------------------------------------------------

  do nl=0,nlat-1 

     ntStrt = 0
     ntLast = nSampWin-1
     if (debug) then
         print("==============> nl="+nl+" <==============")
     end if

    do nw=0,nWindow-1                        ; temporal window          
       if (debug .and. nl.eq.0) then         ; debug
           print("nw="+nw+"  ntStrt="+ntStrt+"   ntLast="+ntLast)
       end if
       work = dtrend( xAS(:,:,ntStrt:ntLast), False )    ; detrend temporal window
       work = taper ( work, tim_taper, 0)    ; taper in "time" [periodic] 
                                             ; work(nlat,mlon,N)
      do nt=0,nSampWin-1                     ; for each time perform complex fft in longitude
         q(:,:,nt) = cfftf( work(nl,:,nt), 0.0, 0)   ; space 
      end do                                  
       q = q/mlon                                    ; normalize by # lon samples

      do ml=0,mlon-1                         ; for each lon perform complex fft in time 
         q(:,ml,:) = cfftf( q(0,ml,:), q(1,ml,:), 0) ; time 
      end do
       q = q/nSampWin                                ; normalize by # time samples

;-------------------------------------------------------------------
; At this point 'q(2,mlon,nSampWin)' contains the 
; real and imaginary space-time spectrum for this latitude
; ---
; Use Hayashi method to resolve into Progressive [Eastward]
;     and Retrogressive [Westward] Waves.
;-------------------------------------------------------------------

       pee = resolveWavesHayashi( q, nDayWin, spd )        
       peeAS(nl,:,:) = peeAS(nl,:,:) + (pee/nWindow)   ; sum window contribution 
           
       ntStrt = ntLast+nSampSkip+1           ; set index for next temporal window
       ntLast = ntStrt+nSampWin-1    
    end do                                   ; nw (windows)

  end do                                     ; nl (lat)

  delete(work)
;-------------------------------------------------------------------
; Add meta data to the Hayashi space-time symmetric and asymmetric power
;-------------------------------------------------------------------

  peeAS!0     = "lat"
  peeAS!1     = "wave"
  peeAS!2     = "freq"
  peeAS&lat   = xAS&$dNam(1)$     ; nominally, xAS&lat
  peeAS&wave  = pee&wave
  peeAS&freq  = pee&freq      
  peeAS@long_name =  "symmetric and asymmetric components"

  if (debug) then
      printVarSummary( peeAS )                 
      printMinMax( peeAS , True)
  end if

  delete( [/ q, pee /] )          ; no longer needed

;-------------------------------------------------------------------
; now that we have the power array for sym and asym: use to
;    1) plot raw power spectrum (some smoothing)
;    2) derive and plot the background spectrum (lots of smoothing)
;    3) derive a denoised spectrum that is raw power/background power
;-------------------------------------------------------------------
; psumanti and psumsym will contain the symmetric and asymmetric power
; summed over latitude
;-------------------------------------------------------------------

  psumanti   = new((/dimsizes(peeAS&wave),dimsizes(peeAS&freq)/),typeof(peeAS), 1e20 )
  psumanti!0 = "wave"
  psumanti!1 = "freq"
  psumanti&wave = peeAS&wave
  psumanti&freq = peeAS&freq

  psumsym       = psumanti

  psumanti@long_name = "Asymmetric summed over lat"
  psumsym@long_name  = "Symmetric  summed over lat"

  if (nlat%2.eq.0)    ; use named dimensions to reorder
      psumanti = dim_sum_n_Wrap(peeAS(nlat/2:nlat-1,:,:), 0)
      psumsym  = dim_sum_n_Wrap(peeAS(0:nlat/2-1,:,:)   , 0)
    ;;psumanti = dim_sum_Wrap(peeAS(wave|:,freq|:,lat|nlat/2:nlat-1))
    ;;psumsym  = dim_sum_Wrap(peeAS(wave|:,freq|:,lat|0:nlat/2-1)   )
  else
      psumanti = dim_sum_n_Wrap(peeAS(nlat/2+1:nlat-1,:,:), 0)
      psumsym  = dim_sum_n_Wrap(peeAS(0:nlat/2,:,:)       , 0)
    ;;psumanti = dim_sum_Wrap(peeAS(wave|:,freq|:,lat|nlat/2+1:nlat-1))
    ;;psumsym  = dim_sum_Wrap(peeAS(wave|:,freq|:,lat|0:nlat/2)     )
  end if
    
;-------------------------------------------------------------------
; since summing over half the array (symmetric,asymmetric) the
; total variance is 2x the half sum
;-------------------------------------------------------------------
  psumanti = 2.0*psumanti
  psumsym  = 2.0*psumsym 

;-------------------------------------------------------------------
; set the mean to missing to match original code
;-------------------------------------------------------------------
;
  psumanti(:,{0.0}) = (/ psumanti@_FillValue /)  
  psumsym (:,{0.0}) = (/ psumsym@_FillValue /)

  if (debug) then
      printVarSummary( psumanti )         ; (wave,freq)
      printMinMax( psumsym , True)
  end if

;-------------------------------------------------------------------
; Apply smoothing to the spectrum. smooth over limited wave numbers
; Smoothing in frequency only (check if mean should be smoothed not smoothing now)
;--                                                                 
; Smoothing parameters set these larger than the plotting
; wavenumbers to avoid smoothing artifacts
;-------------------------------------------------------------------
  minwav4smth = -27
  maxwav4smth =  27
    
  do wv=minwav4smth,maxwav4smth
     wk_smooth121( psumanti({wv},N/2+1:N-1) )
     wk_smooth121( psumsym ({wv},N/2+1:N-1) )
  end do

;-------------------------------------------------------------------
; Log10 scaling 
;-------------------------------------------------------------------

  psumanti_nolog = psumanti
  psumsym_nolog  = psumsym

  psumanti       = log10(psumanti)
  psumsym        = log10(psumsym)

  psumanti@long_name = "log10(Asymmetric)"
  psumsym@long_name  = "log10(Symmetric)"

  if (debug) then
      printVarSummary( psumanti )             ; (wave,freq)                 
      printMinMax( psumanti , True)
      printVarSummary( psumsym )                 
      printMinMax( psumsym , True)
  end if

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;        PLOT CODE follows 
; --- set some 'plot variables
; set frequency maximum for plotting
; min(user specified frq, maxfrq in window)
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; The following allow DJS variable naming 
; to be used with original plot code.
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 ;caseName       = case
  wkdir          = diro

  frqfftwin      = peeAS&freq
  frqfftwin&freq = peeAS&freq

  wavep1         = peeAS&wave
  wavep1&wave    = peeAS&wave

  if (debug) then
      printVarSummary( frqfftwin )
      printMinMax( frqfftwin, True )
      printVarSummary( wavep1 )
      printMinMax( wavep1, True )
  end if
 
;-------------------------------------------------------------------
; plotting parameters freq and wavenumbers to plot
;-------------------------------------------------------------------
  minfrq4plt =  0.
  maxfrq4plt =  0.8
  minwav4plt = -15
  maxwav4plt =  15
    
  minfrq     = minfrq4plt
  maxfrq     = min((/maxfrq4plt,max(frqfftwin)/))

  fillVal    = 1e20           ; miscellaneous
    
;=============================================================
;             Start Common Graphics Resources
;=============================================================
   tmFontHgtF            = 0.015     ; not sure why
   tiFontHgtF            = 0.018
   lbFontHgtF            = 0.015
   txFontHgtF            = 0.013

   res = True
   res@gsnFrame          = False
   res@gsnMaximize       = True
   res@gsnPaperOrientation = "portrait"

   res@gsnLeftString     = "Westward"
   res@gsnRightString    = "Eastward"

  ;res@lbBoxMinorExtentF = 0.18
   res@lbLabelFontHeightF= lbFontHgtF
   res@lbOrientation     = "vertical"

   res@cnFillOn          = True
   if (opt .and. isatt(opt, "cnLinesOn") \
           .and. .not.opt@cnLinesOn) then   
       res@cnLinesOn     = False
   else
       res@cnLineThicknessF  = 0.5
   end if

   res@tmYLMode          = "Explicit"
   res@tmYLValues        = fspan(minfrq,maxfrq,9)
   res@tmYLLabels        = fspan(minfrq,maxfrq,9)
   res@tmYLMinorValues   = fspan(minfrq,maxfrq,17)

   res@tmYLLabelFontHeightF = tmFontHgtF
   res@tmXBLabelFontHeightF = tmFontHgtF

   res@tiXAxisString     = "Zonal Wave Number"
   res@tiXAxisFontHeightF= tiFontHgtF

   res@tiYAxisString     = "Frequency (cpd)"
   res@tiYAxisFontHeightF= res@tiXAxisFontHeightF
 
   if (.not.(pltTitle.eq."" .or. pltTitle.eq." ")) then
       res@tiMainString      = pltTitle
       res@tiMainFontHeightF = tiFontHgtF
   end if
   res@txFontHeightF     = tiFontHgtF

;------------------------------------------------------
; Create a list of variable names that have predefined
; contour intervals. 
;------------------------------------------------------
   varCnLevels = (/"FLUT" ,"OLR", "olr","U200","U850"   \
                  ,"PRECT","OMEGA500" /)

   if (any(varCnLevels.eq.varName)) then
       res@cnLevelSelectionMode = "ExplicitLevels"
   else
       res@cnLevelSelectionMode = "AutomaticLevels"
   end if
   
;-------------------------------
; horizontal dashed lines and text for frequency axis [plot only]
;-------------------------------

   freq       = frqfftwin({freq|minfrq:maxfrq})
   wavenumber = wavep1({wave|minwav4plt:maxwav4plt})
   NWVN       = dimsizes(wavenumber)         ; number of wavenumbers

   x1   = wavenumber 
   x1!0 = "wave"
   y1   = new (NWVN,float)
   y1!0 = "freq"
   y1   = 1./3.           ; 3 days
   y2   = y1
   y2   = 1./6.           ; 6 days
   y3   = y1
   y3   = 1./30.          ; 30 days 
   x2   = new(25,float)
   x2   = 0.0
   y4   = fspan(0.0,1.0,25)

;---------------------------------------------------------------
; dispersion: curves 
;---------------------------------------------------------------

   rlat           = 0.0
   Ahe            = (/50.,25.,12./)
   nWaveType      = 6
   nPlanetaryWave = 50
   nEquivDepth    = dimsizes(Ahe)
   Apzwn          = new((/nWaveType,nEquivDepth,nPlanetaryWave/),"double",fillVal)
   Afreq          = Apzwn
   genDispersionCurves(nWaveType, nEquivDepth, nPlanetaryWave, rlat, Ahe, Afreq, Apzwn )

;---------------------------------------------------------------
; dispersion curve and text plot resources
;---------------------------------------------------------------
   dcres = True
   dcres@gsLineThicknessF  = 2.0
   dcres@gsLineDashPattern = 0

   txres = True
   txres@txJust        = "CenterLeft"
   txres@txPerimOn     = True
   txres@txFontHeightF = 0.013
   txres@txBackgroundFillColor = "Background"
       
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; plotting params for fig 1; subset for plot
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
                                              ; reorder so freq is "y"
   asym       = psumanti({freq|minfrq:maxfrq},{wave|minwav4plt:maxwav4plt})
   asym!0     = "freq"
   asym&freq  =  freq 
   asym!1     = "wave"
   asym&wave  =  wavenumber
   asym@long_name = "Fig_1: log10(Asymmetric)"

   sym        = psumsym({freq|minfrq:maxfrq},{wave|minwav4plt:maxwav4plt})
   sym@long_name  = "Fig_1: log10(Symmetric)"

   if (debug) then
       printVarSummary(asym)
       printMinMax(asym, True)
       printVarSummary(sym)
       printMinMax(sym, True)
   end if

;------------------------------------------------------
; Fig 1: Pre-defined contour levels [15]  for selected variables [non-log10]
;------------------------------------------------------
   nCn = 15

   if (varName .eq. "FLUT" .or. varName .eq. "OLR" .or. varName .eq. "olr") then
       res@cnLevels = (/-1.2,-1.1,-1.0,-0.8,-0.6,-0.4,-0.2     \ ; unequal
                       , 0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.1,1.2/)
   end if
   if (varName .eq. "PRECT") then
       res@cnLevels = (/-18.2,-18.0,-17.8,-17.6,-17.5,-17.4,-17.3 \ ; unequal
                       ,-17.2,-17.1,-17.0,-16.9,-16.8,-16.7,-16.6,-16.5/)
   end if
   if (varName .eq. "U200") then
       res@cnLevels = fspan(-3.3, 0.9, nCn)
   end if
   if (varName .eq. "U850") then
       res@cnLevels = fspan(-3.25, 0.25, nCn)
   end if
   if (varName .eq. "OMEGA500") then
       res@cnLevels = fspan(-5.9, -4.5, nCn)
   end if

   if (opt .and. isatt(opt, "Fig_1")) then
       res@cnLevelSelectionMode = "ExplicitLevels"
       res@cnLevels = opt@Fig_1                    ; user specified limits
   end if
  
;------------------------------------------------------
; Fig 1: ANTI-SYMMETRIC
;------------------------------------------------------
  ;print("======> Fig 1a: ASYMMETRIC <=====")
    
   wks  = gsn_open_wks(pltType,wkdir+"/"+"Fig1.Asym."+pltFilTit)
   gsn_define_colormap(wks,pltColorMap)
   res@gsnCenterString = "Anti-Symmetric"
   plot = gsn_csm_contour(wks,asym,res)
   addHorVertLines(wks, plot, x1,x2,y1,y2,y3,y4 ) ; add dashed lines
   frame(wks)
   delete(wks)    ; not required 
    
;------------------------------------------------------
; Fig 1: SYMMETRIC
;------------------------------------------------------
  ;print("======> Fig 1b: SYMMETRIC <=====")
    
   wks  = gsn_open_wks(pltType,wkdir+"/"+"Fig1.Sym."+pltFilTit)
   gsn_define_colormap(wks,pltColorMap)
   res@gsnCenterString = "Symmetric"
   plot = gsn_csm_contour(wks,sym,res)
   addHorVertLines(wks, plot, x1,x2,y1,y2,y3,y4 ) ; add dashed lines
   frame(wks)
   delete(wks)

;------------------------------------------------------
; Is netCDF option set?
;------------------------------------------------------
   if (opt .and. isatt(opt,"netCDF") .and. opt@netCDF) then
       if (isatt(opt,"dirNetCDF")) then
           dirNetCDF = opt@dirNetCDF
       else
           dirNetCDF = "./" 
       end if
       if (isatt(opt,"filNetCDF")) then
           filNetCDF = opt@filNetCDF
       else
           filNetCDF = "SpaceTime."+varName+".nc"
       end if
       fNam      = dirNetCDF+filNetCDF
       system ("/bin/rm -f "+fNam)
        
       ncdf      = addfile(fNam, "c")

       ncdf->FIG_1_SYM  =  sym
       ncdf->FIG_1_ASYM = asym
   end if
    
;-----------------------------------------------------------------------------
; ******  now derive and plot the background spectrum (red noise) ************
; [1] Sum power over all latitude 
; [2] Put fill value in mean
; [3] Apply smoothing to the spectrum. This smoothing DOES include wavenumber zero.
;-----------------------------------------------------------------------------
  ;print("======> BACKGROUND <=====")
    
   psumb = dim_sum_n_Wrap(peeAS, 0)                  ; sum over all latitudes
 ;;psumb = dim_sum_Wrap(peeAS(wave|:,freq|:,lat|:))  ; sum over all latitudes
   psumb@long_name = "Background Spectrum"
    
   psumb@_FillValue       = fillVal    
   psumb(wave|:,freq|N/2) = fillVal
   psumb@_FillValue       = fillVal

   if (debug) then
       printVarSummary(psumb)             ; (wave,freq)      
       printMinMax(psumb, True)
   end if
    
   do tt = N/2+1,N
      smthlen = maxwav4smth-minwav4smth+1
      if (frqfftwin(tt).lt.0.1) then
        do i = 1,5
          wk_smooth121( psumb(freq|tt,{wave|minwav4smth:maxwav4smth}) )
        end do
      end if
      if (frqfftwin(tt).ge.0.1.and.frqfftwin(tt).lt.0.2) then
        do i = 1,10
          wk_smooth121( psumb(freq|tt,{wave|minwav4smth:maxwav4smth}) )
        end do
      end if
      if (frqfftwin(tt).ge.0.2.and.frqfftwin(tt).lt.0.3) then
        do i = 1,20
          wk_smooth121( psumb(freq|tt,{wave|minwav4smth:maxwav4smth}) )
        end do
      end if

      if (frqfftwin(tt).ge.0.3) then
        do i = 1,40
          wk_smooth121(psumb(freq|tt,{wave|minwav4smth:maxwav4smth}))
        end do
      end if
   end do
       
   do nw = minwav4smth,maxwav4smth ; smth frequency up to .8 cycles per day
      pt8cpd  = min((/closest_val(.8,frqfftwin),dimsizes(frqfftwin)-1/))
      smthlen = pt8cpd-(N/2+1)+1
     do i = 1,10
        wk_smooth121( psumb({nw},N/2+1:pt8cpd) )
     end do
   end do    

;----------------------------------------------------------------
; [1] Put fill value in mean (again)
; [2] SAVE the background spectrum for plotting Fig 3
; [3] LOGARITHMIC SCALING for plotting the background spectrum 
;----------------------------------------------------------------
    
   psumb(wave|:,freq|N/2) = fillVal
   psumb_nolog = psumb
   psumb       = log10(psumb)            ; LOG10
   psumb@long_name = "log10( background spec )"
   if (debug) then
       print(" ===> Post multiple smoothing by wk_smooth121 <===")
       printVarSummary(psumb_nolog)
       printMinMax(psumb_nolog, True)

       printVarSummary(psumb)
       printMinMax(psumb, True)
   end if
    
;----------------------------------------------------------------
; set up for plotting  [ subset of frequencies: not necessary]
;----------------------------------------------------------------
    
   freq      = frqfftwin({freq|minfrq:maxfrq})
   spec      = psumb({freq|minfrq:maxfrq},{wave|minwav4plt:maxwav4plt})

   if (debug) then
       printVarSummary(spec)                 ; (freq,wave)
       printMinMax(spec, True)
   end if
    
;----------------------------------------------------------------
; Fig 2: Predefined explicit contour levels BACKGROUND spectrum  [LOG10]
;----------------------------------------------------------------

   if (isatt(res,"cnLevels")) then
       delete(res@cnLevels)       ; allow size to change
   end if
   
   if (varName .eq. "FLUT" .or. varName .eq. "OLR" .or. varName .eq. "olr") then
       res@cnLevels = (/-1.2,-1.1,-1.0,-0.8,-0.6,-0.4,-0.2 \     ; unequal 15
                       , 0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.1,1.2/)
   end if
   if (varName .eq. "PRECT") then
       res@cnLevels = (/-18.2,-18.0,-17.8,-17.6,-17.5,-17.4,-17.3 \   ; unequal
                       ,-17.2,-17.1,-17.0,-16.9,-16.8,-16.7,-16.6,-16.5/)
   end if
   if (varName .eq. "U200") then
       res@cnLevels = fspan(-3.3, 0.9, nCn)
   end if
   if (varName .eq. "U850") then
       res@cnLevels = fspan(-3.25, 0.25, nCn)
   end if
   if (varName .eq. "OMEGA500") then
       res@cnLevels = fspan(-5.9,-4.5, nCn)
   end if

   if (opt .and. isatt(opt, "Fig_2")) then
       res@cnLevelSelectionMode = "ExplicitLevels"
       res@cnLevels = opt@Fig_2                   ; user specified 
   end if
   
   wks  = gsn_open_wks(pltType,wkdir+"/Fig2."+pltFilTit)
   gsn_define_colormap(wks,pltColorMap)

   res@gsnCenterString = "Background Power"
   plot = gsn_csm_contour(wks,spec,res)
   addHorVertLines(wks, plot, x1,x2,y1,y2,y3,y4 ) ; add dashed lines
   frame(wks)
    
;*********************************************************************************
; Fig 3a, 3b:  psum_nolog/psumb_nolog  [ratio]
;********************************************************************************
    
   psumsym_nolog  = psumsym_nolog /psumb_nolog   ; (wave,freq)
   psumanti_nolog = psumanti_nolog/psumb_nolog

   if (debug) then
       printVarSummary(psumanti_nolog)           ; (freq,wave)
       printMinMax(psumanti_nolog, True)
       printVarSummary(psumsym_nolog)            ; (freq,wave)
       printMinMax(psumsym_nolog, True)
   end if

;----------------------------------------------------------- 
; ANTI-SYMMETRIC: RATIO ( subset to plot )
;----------------------------------------------------------- 

   asym = psumanti_nolog({freq|minfrq:maxfrq},{wave|minwav4plt:maxwav4plt})
   sym  = psumsym_nolog({freq|minfrq:maxfrq},{wave|minwav4plt:maxwav4plt})

   if (debug) then
       printVarSummary(asym)                    ; (freq,wave)
       printMinMax(asym, True)
       printVarSummary(sym)                     ; (freq,wave)
       printMinMax(sym, True)
   end if

   if (isatt(res,"cnLevels")) then
       delete(res@cnLevels)                     ; allow size to change
   end if

   if (varName .eq. "FLUT" .or. varName .eq. "OLR".or. varName .eq. "olr") then
       res@cnLevels = fspan(0.3, 1.7, nCn)
   end if
   if (varName .eq. "U200") then
       res@cnLevels = fspan(0.4, 1.8, nCn)
   end if
   if (varName .eq. "U850") then
       res@cnLevels = fspan(0.4, 1.8, nCn)
   end if
   if (varName .eq. "PRECT") then
       res@cnLevels = (/0.6,0.7 ,0.8,0.9 ,1.0,1.1,1.15,1.2,1.25 \
                       ,1.3,1.35,1.4,1.45,1.5,1.6/)
   end if
   if (varName .eq. "OMEGA500") then
       res@cnLevels = (/0.6,0.7,0.8,0.9,1.0,1.1,1.15,1.2,1.25 \
                       ,1.3,1.35,1.4,1.45,1.5,1.6/)
   end if

   if (opt .and. isatt(opt, "Fig_3a")) then
       res@cnLevelSelectionMode = "ExplicitLevels"
       res@cnLevels = opt@Fig_3a                  ; user specified 
   end if
   
   wks  = gsn_open_wks(pltType,wkdir+"/Fig3.Asym."+pltFilTit)
   gsn_define_colormap(wks,pltColorMap)
   res@gsnCenterString = "Anti-Symmetric/Background"
   plot = gsn_csm_contour(wks,asym,res)
   addHorVertLines(wks, plot, x1,x2,y1,y2,y3,y4 ) ; add dashed lines

; draw dispersion line

   gsn_polyline(wks,plot,Apzwn(0,0,:),Afreq(0,0,:),dcres)
   gsn_polyline(wks,plot,Apzwn(0,1,:),Afreq(0,1,:),dcres)
   gsn_polyline(wks,plot,Apzwn(0,2,:),Afreq(0,2,:),dcres)
   gsn_polyline(wks,plot,Apzwn(1,0,:),Afreq(1,0,:),dcres)
   gsn_polyline(wks,plot,Apzwn(1,1,:),Afreq(1,1,:),dcres)
   gsn_polyline(wks,plot,Apzwn(1,2,:),Afreq(1,2,:),dcres)
   gsn_polyline(wks,plot,Apzwn(2,0,:),Afreq(2,0,:),dcres)
   gsn_polyline(wks,plot,Apzwn(2,1,:),Afreq(2,1,:),dcres)
   gsn_polyline(wks,plot,Apzwn(2,2,:),Afreq(2,2,:),dcres)

; dispersion labels

   gsn_text(wks,plot,"MRG",-10.0,.15,txres)
   gsn_text(wks,plot,"n=2 IG",-3.0,.58,txres)
   gsn_text(wks,plot,"n=0 EIG",9.5,.50,txres)
   gsn_text(wks,plot,"h=50",-10.0,.78,txres)
   gsn_text(wks,plot,"h=25",-10.0,.63,txres)
   gsn_text(wks,plot,"h=12",-10.0,.51,txres)

   frame(wks)
   delete(wks)             ; not required

;------------------------------------------------------------------
; SYMMETRIC
;------------------------------------------------------------------
   if (isatt(res,"cnLevels")) then
       delete(res@cnLevels)
   end if

   if (varName .eq. "FLUT" .or. varName .eq. "OLR".or. varName .eq. "olr") then
       res@cnLevels = (/.3,.4,.5,.6,.7,.8,.9,1.,1.1,1.2,1.4,1.7,2.,2.4,2.8/)
   end if
   if (varName .eq. "U200") then
       res@cnLevels = (/.4,.6,.8,1.,1.2,1.3,1.4,1.5,1.6,1.7,1.8,2.,2.2,2.4,2.6/)
   end if
   if (varName .eq. "U850") then
       res@cnLevels = (/.4,.6,.8,1.,1.2,1.3,1.4,1.5,1.6,1.7,1.8,2.,2.2,2.4,2.6/)
   end if
   if (varName .eq. "PRECT") then
       res@cnLevels = (/.6,.7,.8,.9,1.,1.1,1.15,1.2,1.25,1.3,1.35,1.4,1.45,1.5,1.6/)
   end if
   if (varName .eq. "OMEGA500") then
       res@cnLevels = (/.6,.7,.8,.9,1.,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2./)
   end if

   if (opt .and. isatt(opt, "Fig_3b")) then
       res@cnLevelSelectionMode = "ExplicitLevels"
       res@cnLevels = opt@Fig_3b                  ; user specified 
   end if

   wks  = gsn_open_wks(pltType, wkdir+"/Fig3.Sym."+pltFilTit)
   gsn_define_colormap(wks ,pltColorMap)
   res@gsnCenterString = "Symmetric/Background"
   plot = gsn_csm_contour(wks,sym,res)
   addHorVertLines(wks, plot, x1,x2,y1,y2,y3,y4 ) ; add dashed lines

   gsn_polyline(wks,plot,Apzwn(3,0,:),Afreq(3,0,:),dcres)
   gsn_polyline(wks,plot,Apzwn(3,1,:),Afreq(3,1,:),dcres)
   gsn_polyline(wks,plot,Apzwn(3,2,:),Afreq(3,2,:),dcres)
   gsn_polyline(wks,plot,Apzwn(4,0,:),Afreq(4,0,:),dcres)
   gsn_polyline(wks,plot,Apzwn(4,1,:),Afreq(4,1,:),dcres)
   gsn_polyline(wks,plot,Apzwn(4,2,:),Afreq(4,2,:),dcres)
   gsn_polyline(wks,plot,Apzwn(5,0,:),Afreq(5,0,:),dcres)
   gsn_polyline(wks,plot,Apzwn(5,1,:),Afreq(5,1,:),dcres)
   gsn_polyline(wks,plot,Apzwn(5,2,:),Afreq(5,2,:),dcres)

   gsn_text(wks,plot,"Kelvin",11.0,.40,txres)
   gsn_text(wks,plot,"n=1 ER",-10.7,.07,txres)
   gsn_text(wks,plot,"n=1 IG",-3.0,.45,txres)
   gsn_text(wks,plot,"h=50",-14.0,.78,txres)
   gsn_text(wks,plot,"h=25",-14.0,.60,txres)
   gsn_text(wks,plot,"h=12",-14.0,.46,txres)
   frame(wks)

   if (debug .or. (opt .and. isatt(opt,"Fig_3_statInfo") \
                       .and. opt@Fig_3_statInfo)       ) then
       statAsymSym( asym, "Fig_3a" )
       statAsymSym(  sym, "Fig_3b" )
   end if

;------------------------------------------------------
; Is netCDF option set?
;------------------------------------------------------
   if (opt .and. isatt(opt,"netCDF") .and. opt@netCDF) then
       ncdf->FIG_3_BACK = spec
       ncdf->FIG_3_SYM  =  sym
       ncdf->FIG_3_ASYM = asym
   end if

   if (debug) then
       print("********** FINISHED: Space-Time *****************")
   end if
end

;-------------------------------------------------------------
; CAM interface to wkSpaceTime 
;-------------------------------------------------------------
undef("wkSpaceTime_cam")
procedure wkSpaceTime_cam                    \  
                     ( diri[1]:string        \ ; input directory
                     , fili[*]:string        \ ; input file
                     , diro[1]:string        \ ; output directory [plots]
                     , caseName[1]:string    \ ; case name
                     , varName[1]:string     \ ; variable name
                     , latBound[1]:numeric   \ ; NH lat bound [positive]
                     , spd[1]:numeric        \ ; samples per day
                     , level[1]:numeric      \ ; if 4D ... what level?
                     , nDayWin[1]:integer    \ ; temporal window length [days]
                     , nDaySkip[1]:integer   \ ; days between time windows 
                     , opt[1]:logical        \ ; future options
                     )
;
;     ---- local declarations are *not* required ----
local latN, latS, lonL, lonR, SPD, tskip, debug, fillVal     \
          , nfil, f, rank, nMsg, x, dNam, flist, dNamx       \
          , TIME, pltId
begin 
  latN    = latBound
  latS    =-latBound ; make symmetric about the equator

  lonL    = 0        ; -180
  lonR    = 360      ;  180

                     ; OPTIONS
  debug   = False    ; default
  if (opt .and. isatt(opt, "debug") ) then
      debug = opt@debug
  end if

  SPD   = spd        ; SPD *actual* "samples per day" 
                     ; *after* possible 'decimation' 
  tskip = 1          ; used to skip samples on input
  if (opt .and. isatt(opt, "spdSkip") .and. opt@spdSkip.gt.1 ) then
      SPD     = spd/opt@spdSkip
      tskip   = max( (/1,SPD/) )      ; decimate
  end if

  fillVal    = 1e20           ; miscellaneous          

;-------------------------------------------------------------------
; Read data from file(s): maintain meta data and original dim names
;-------------------------------------------------------------------

  nfil = dimsizes(fili)
  f    = addfile (diri+fili(0), "r")
  rank = dimsizes( filevardimsizes(f,varName) )
  if (rank.lt.3 .or. rank.gt.4) then
      print("wkSpaceTime: only 3D and 4D supported: rank="+rank+"D")
      exit
  end if

  if (nfil.eq.1) then                    ; SINGLE FILE    
      if (rank.eq.3) then
          x  = f->$varName$(::tskip,{latS:latN},{lonL:lonR})
      else
          x  = f->$varName$(::tskip,{level},{latS:latN},{lonL:lonR})
      end if
  else                                   ; MULTIPLE FILES
      dNam   = getfilevardims(f,varName)
                                                 ; needed for *large*
      setfileoption("nc","SuppressClose",False)  ; number of files

      flist  = addfiles( diri+fili, "r")
                                             ; make TIME  [tedious]
      TIME           = flist[:]->$dNam(0)$   ; values
      if (isfilevaratt(flist[0],  dNam(0) ,  "units") ) then  
          TIME@units = flist[0]->$dNam(0)$@units   ; assign units attribute
      end if
      if (isfilevarcoord( flist[0], dNam(0), dNam(0) ) ) then
          TIME!0     = dNam(0)                     ; name the dimension
          TIME&$dNam(0)$ = TIME                    ; assign values [coord]
      end if

      if (rank.eq.3) then
          x   = flist[:]->$varName$(::tskip,{latS:latN},{lonL:lonR})
          x!0 = dNam(0)
          x!1 = dNam(1)
          x!2 = dNam(2)
      else
          x   = flist[:]->$varName$(::tskip,{level},{latS:latN},{lonL:lonR})
          x!0 = dNam(0)
          x!1 = dNam(2)
          x!2 = dNam(3)
      end if
                             ; NOT required but create meta data
      dNamx  = getvardims(x)            

      x&$dNamx(0)$ = TIME      ; assign coordinates
      x&$dNamx(1)$ = flist[0]->$dNamx(1)$({latS:latN})
      x&$dNamx(2)$ = flist[0]->$dNamx(2)$({lonL:lonR})
      if (isfilevaratt( flist[0], varName, "long_name")) then
          x@long_name = flist[0]->$varName$@long_name
      end if
      if (isfilevaratt( flist[0], varName, "units"    )) then  
          x@units = flist[0]->$varName$@units
      end if     
  end if
;-------------------------------------------------------------------
; Call the procedure that will do the calculations and plots.
;-------------------------------------------------------------------
  wkSpaceTime (x, diro, caseName, varName             \
              ,latBound, SPD, nDayWin, nDaySkip, opt  )  
end
;------------------------------------------------------- 
; L2 error norms
;------------------------------------------------------- 
 undef("l2_norma")        
 function l2_norma( x[*][*][*][*]:numeric, wy[*], wz )
 local dimx, dimwz, rankwz, ntim, l2a, nt, xzon, xzonc \
     , dif2, wyc, wywz, wywzs
 begin
   dimx   = dimsizes( x )
   dimwz  = dimsizes( wz )
   rankwz = dimsizes( dimwz )
   
   ntim   = dimx(0)
   l2a    = new ( ntim, typeof(x), getFillValue(x) )
                                                       ; constant all times
   wyc    =  conform(x(0,:,:,:), wy, 1)                ; (klev,nlat,mlon)
 
   if (rankwz.eq.1) then                               ; constant weight
       wywz  = wyc*conform( x(0,:,:,:), wz, 0 )        ; (klev,nlat,mlon)
       wywzs = sum(wywz)                               ; scalar
       delete( wyc )
   end if
 
   do nt=0,ntim-1
      xzon    = dim_avg( x(nt,:,:,:) )                 ; (klev,nlat)
      xzonc   = conform( x(nt,:,:,:), xzon, (/0,1/))   ; (klev,nlat,mlon)
      dif2    = (x(nt,:,:,:)-xzonc)^2                  ; (klev,nlat,mlon)
      if (rankwz.eq.1) then
          l2a(nt) = sqrt( sum(dif2*wywz)/wywzs  )      ; (nt)
      else                                             ; variable wgt (nt)
          wywz    = wyc*wz(nt,:,:,:)                   ; (klev,nlat,mlon)
          l2a(nt) = sqrt( sum(dif2*wywz)/sum(wywz)  )  ; (nt)
      end if
   end do
 
   l2a@long_name = "l2" 
   if (isatt(x, "units")) then
      l2a@units = x@units
   end if
   
   return( l2a ) 
 end
;------------------------------------------------------- 
; L2 error norms
;------------------------------------------------------- 
 undef("l2_normb")         
 function l2_normb( x[*][*][*][*]:numeric, wy[*], wz )
 local dimx, dimwz, rankwz, ntim, l2b, nt, xzon0, xzonc \
     , dif2, wyc, wywz, wywz
 begin
   dimx    = dimsizes( x )
   dimwz   = dimsizes( wz )
   rankwz  = dimsizes( dimwz )
                                                      ; time=0
   xzon0   = dim_avg( x(0,:,:,:) )                    ; (klev,nlat)
   xzon0c  = conform( x(0,:,:,:), xzon0, (/0,1/))     ; (klev,nlat,mlon)    
   wyc     = conform( x(0,:,:,:), wy, 1)              ; (klev,nlat,mlon)
 
   if (rankwz.eq.1) then
       wywz  = wyc*conform( x(0,:,:,:), wz, 0 )       ; (klev,nlat,mlon)
       wywzs = sum(wywz)                              ; scalar
       delete( wyc )
   end if
   
   ntim = dimx(0)
   l2b  = new ( ntim, typeof(x), getFillValue(x) )
 
   do nt=0,ntim-1
      dif2    = (x(nt,:,:,:)-xzon0c)^2                ; (klev,nlat,mlon)
      if (rankwz.eq.1) then
          l2a(nt) = sqrt( sum(dif2*wywz)/wywzs  )     ; (nt)
      else
          wywz    = wyc*wz(nt,:,:,:)                  ; (klev,nlat,mlon)
          l2b(nt) = sqrt( sum(dif2*wywz)/sum(wywz) )  ; (nt)
      end if
   end do
 
   l2b@long_name = "l2" 
   if (isatt(x, "units")) then
      l2b@units = x@units
   end if
   l2b@info = "x(:,:,:,:)-dimavg(x(0,:,:,:))"
   return( l2b ) 
 end
;------------------------------------------------------- 
; l2 norm written by Mark Taylor
; function  || T || on 2D horizontal array
;------------------------------------------------------- 
undef ("norml2")
function norml2 (varz[*][*]:numeric,gw[*]:numeric)
local s2, gs, varl
begin
  s2 =  dimsizes(varz)
  gs =  dimsizes(gw)

  if ( s2(0) .ne. gs(0) ) then
     print ("norml2: error: first dimension does not match weight dimension: " \
            + s2(0) + " " + gs(0))
  end if

  varl = ( gw # (varz^2) )/sum(gw)
  return( sqrt( sum(varl)/s2(1) ))
end

;------------------------------------------------------- 
; Energy
;------------------------------------------------------- 
 undef ("energy_a")
 function energy_a(u[*][*][*][*]:numeric,   v[*][*][*][*]:numeric\
                  ,t[*][*][*][*]:numeric,phis[*][*][*]:numeric   \
                  ,wy[*]:numeric, wz:numeric, opt[1]:integer )
;------------------------------------------------------- 
; energy  = SUM(0.5*(U^2+V^2) + cp*T + S )*wz
;-------------------------------------------------------
 local dimu, dimwz, rankwz, ntim, e, nt, x, wyc, wywz, wywzs
 begin
   dimu   = dimsizes( u )
   dimwz  = dimsizes( wz )
   rankwz = dimsizes( dimwz )
   
   ntim   = dimu(0)
   e      = new ( ntim, typeof(u), getFillValue(u) )
                                                    ; constant all times
   wyc    =  conform(u(0,:,:,:), wy, 1)             ; (klev,nlat,mlon)
 
   if (rankwz.eq.1) then                            ; constant weight
       wywz  = wyc*conform( u(0,:,:,:), wz, 0 )     ; (klev,nlat,mlon)
       wywzs = sum(wywz)                            ; scalar
       delete( wyc )
   end if

   a     = 6.371229e6   ; m            
   g     = 9.80616      ; m/s^2
   cp    = 1004.64      ; J/(kg-K)
   twopi = 8.*atan(1.0)

   do nt=0,ntim-1
      s = conform(u(nt,:,:,:), phis(nt,:,:), (/1,2/))
      x = (0.5*(u(nt,:,:,:)^2+v(nt,:,:,:)^2) + cp*t(nt,:,:,:) + s )
      if (rankwz.eq.1) then
          e(nt) = sum(x*wywz)
      else                                          ; variable wgt (nt)
          wywz  = wyc*wz(nt,:,:,:)                  ; (klev,nlat,mlon)
          e(nt) = sum(x*wywz)
      end if
   end do

   e = (twopi*a*a/g)*e                              ; total energy e(t)

   if (opt.eq.0) then
       e@long_name = "E: eqn 155" 
       e@units     = "J"                            ; ==>'kg m^2 / s^2'.
   else
       e = (e-e(0))/e(0)                            ; normalized
       e@long_name = "Normalized Energy Difference"     
       e@units     = "(E-E(0))/E(0)"                   
   end if
   return( e ) 
 end
; ====================================================
undef("mjo_ApplyLanczosWgt_LeftDim")
function mjo_ApplyLanczosWgt_LeftDim(x:numeric, wgt[*]:numeric, opt[1]:integer)
;++++
; The wgt_runave_n function made this function obsolete. 
; It is included for backward compatibility. It replaces *ALL* the
; reordering with the wgt_runave_n function.
;++++
; utility routine ... makes for cleaner code
; Reorder so time is rightmost; applies Lanczos weights, reorder back
;
local dimx, rank, dNam, xBand
begin
  return( wgt_runave_n_Wrap( x, wgt, opt, 0) )

;;dimx = dimsizes(x)
;;rank = dimsizes( dimx )

;;dNam = getvardims( x )

;;if (rank.eq.1) then
;;    return( wgt_runave_Wrap( x, wgt, opt) )
;;end if
;;if (rank.eq.2) then
;;    xBand = wgt_runave_Wrap( x($dNam(1)$|:,$dNam(0)$|:), wgt, opt)
;;    return( xBand($dNam(0)$|:,$dNam(1)$|:) ) 
;;end if
;;if (rank.eq.3) then
;;    xBand = wgt_runave_Wrap( x($dNam(1)$|:,$dNam(2)$|:,$dNam(0)$|:), wgt, opt)
;;    return( xBand($dNam(0)$|:,$dNam(1)$|:,$dNam(2)$|:) )
;;end if
;;if (rank.eq.4) then
;;    xBand = wgt_runave_Wrap( x($dNam(1)$|:,$dNam(2)$|:,$dNam(3)$|:,$dNam(0)$|:), wgt, opt)
;;    return (xBand($dNam(0)$|:,$dNam(1)$|:,$dNam(2)$|:,$dNam(3)$|:) )
;;end if
;;if (rank.eq.5) then
;;    xBand = wgt_runave_Wrap( x($dNam(1)$|:,$dNam(2)$|:,$dNam(3)$|:,$dNam(4)$|:,$dNam(0)$|:), wgt, opt)
;;    return (xBand($dNam(0)$|:,$dNam(1)$|:,$dNam(2)$|:,$dNam(3)$|:,$dNam(4)$|:) )
;;end if
end
; ====================================================
undef("apply_dtrend_LeftDim")
function apply_dtrend_LeftDim(x:numeric, wgt[*]:numeric, opt[1]:integer)
;++++
; The dtrend_n function made this function obsolete. 
; It is included for backward compatibility. It replaces *ALL* the
; reordering with the wgt_runave_n function.
;++++
;
; utility routine ... makes for cleaner code
; Reorder so time is rightmost; detrend the time series, reorder back
;
local xdtr
;;local dimx, rank, dNam, x_dtrend, xWork
begin
  xdtr = dtrend_n( x, False, 0)  
  copy_VarMeta(x, xdtr)
  return( xdtr ) 

;;dimx = dimsizes(x)
;;rank = dimsizes( dimx )
;;dNam = getvardims( x )

;;if (rank.eq.1) then
;;    x_dtrend = x                             ; retain meta data
;;    x_dtrend = dtrend( x, False) 
;;end if
;;if (rank.eq.2) then
;;    xWork = x($dNam(1)$|:,$dNam(0)$|:)       ; retain meta data
;;    xWork = dtrend( xWork, False)
;;    return( xWork($dNam(0)$|:,$dNam(1)$|:) ) 
;;end if
;;if (rank.eq.3) then
;;    xWork = x($dNam(1)$|:,$dNam(2)$|:,$dNam(0)$|:)
;;    xWork = dtrend( xWork, False)
;;    return( xWork($dNam(0)$|:,$dNam(1)$|:,$dNam(2)$|:) )
;;end if
;;if (rank.eq.4) then
;;    xWork = x($dNam(1)$|:,$dNam(2)$|:,$dNam(3)$|:,$dNam(0)$|:)
;;    xWork = dtrend( xWork, False)
;;    return (xWork($dNam(0)$|:,$dNam(1)$|:,$dNam(2)$|:,$dNam(3)$|:) )
;;end if
;;if (rank.eq.5) then
;;    xWork = x($dNam(1)$|:,$dNam(2)$|:,$dNam(3)$|:,$dNam(4)$|:,$dNam(0)$|:)
;;    xWork = dtrend( xWork, False)
;;    return (xWork )
;;end if
end
; ====================================================
undef ("band_pass_area_time")
function band_pass_area_time \
                 (x[*][*][*]:numeric, spd[1]:numeric   \
                 ,bpf[3]:numeric                       \
                 ,wy[*]:numeric, opt:logical)
local nMsg, dimx, dimwy, ntim, bpfStrt, bpfLast, bpfNwgt     \
    , dumy, save_FillValue, fca, fcb, ihp, sigma, xts    
begin
                                      ; error check
  nMsg = num( ismissing(x) )
  if (nMsg.gt.0) then
      print("band_pass_area_time: currently, missing data not allowed: nMsg="+nMsg)
      exit
  end if

  dimx    = dimsizes(x)
  dimwy   = dimsizes(wy)
  if (dimx(1).ne.dimwy(0)) then
      print("band_pass_area_time: sizes of y/lat dimension do not match")
      print("                     dimsizes(wy)="+dimwy)              
      print("                     dimx(1)=nlat="+dimx(1))              
      exit
  end if

  dimx    = dimsizes( x )
  ntim    = dimx(0)

  bpfStrt = bpf(0)                     ; days
  bpfLast = bpf(1)
  bpfNwgt = bpf(2)                     ; effective # weights 

  bpfNwgt = (bpfNwgt/2)*2 + 1          ; make sure it is odd

  if (bpfStrt.gt.bpfLast) then         ; safety check
      dumy    = bpfLast
      bpfLast = bpfStrt
      bpfStrt = dumy
  end if

  fca  = 1.0/(spd*bpfLast)             ; freq start/end
  fcb  = 1.0/(spd*bpfStrt)             ; bpf{Strt/Last}

  if (isatt(x,"_FillValue")) then 
      xts  = new( (/3,ntim/), "double", todouble(getFillValue(x)))
  else 
      xts  = new( (/3,ntim/), "double"); use default _FillValue
  end if
  xts!0 = "var"
                                       ; weighted area averages
  xts(1,:)  = wgt_areaave_Wrap(x, wy, 1., 0)    ; (time)

  if (opt .and. isatt(opt,"detrend") .and. opt@detrend) then
      if (isatt(xts,"_FillValue")) then
          save_FillValue = x@_FillValue
          delete(xts@_FillValue)           ; avoid annoying warning msg
      end if                               ; from dtrend

      xts(1,:) = dtrend(xts(1,:), False )  ; detrend 

      if (isvar("save_FillValue")) then
          xts@_FillValue = save_FillValue  ; reassign
      end if
  end if

  ihp      = 2                             ; bpf=>band pass filter
  sigma    = 1.0                           ; Lanczos sigma
  bpfwgt   = filwgts_lanczos (bpfNwgt, ihp, fca, fcb, sigma )

  xts(0,:) = wgt_runave_Wrap(xts(1,:), bpfwgt, 0)

  if (opt .and. isatt(opt,"Nrun")) then
      Nrun = (opt@Nrun/2)*2 + 1            ; make sure it is odd
  else
      Nrun = 101
  end if

  len  = (Nrun*spd)/2                      ; half total length
  do n = len,ntim-(len+1)
     xts(2,n) = variance(xts(0,n-len:n+len))  ; 'local' variance
  end do

  xts@bpfStrt = bpfStrt
  xts@bpfLast = bpfLast 
  xts@bpfNwgt = bpfNwgt

  xts@var_0   = "band pass"
  xts@var_1   = "raw areal means"
  xts@var_2   = "local variances: Nrun="+Nrun

  return( xts )
end
; =================================================================
undef("band_pass_area_time_plot")
procedure band_pass_area_time_plot(           \ 
          xts[3][*]:numeric, time[*]:numeric, \
          pltDir[1]:string, pltType[1]:string,\
          pltName[1]:string, opt[1]:logical)
begin
  date   = ut_calendar(time, -3)
  yrfrac = yyyymmddhh2yyyyFrac( date)
  delete(yrfrac@long_name)

  pltPath= pltDir+"/"+pltName
  
  wks  = gsn_open_wks(pltType , pltPath)   
  gsn_merge_colormaps(wks,"amwg_blueyellowred","BlAqGrYeOrReVi200") 

 ;gsn_draw_colormap(wks)                      ; draw color map

  plot  = new ( 3, "graphic")
 
; left variable
  res                         = True 
  res@gsnDraw                 = False
  res@gsnFrame                = False

  res@vpXF                    = 0.15   
  res@vpYF                    = 0.3     
  res@vpWidthF                = 0.65   
  res@vpHeightF               = 0.18

  res@tmXBLabelsOn            = False         ; do not draw bottom labels
  res@tmXBOn                  = False         ; no bottom tickmarks

  res@xyLineThicknessF        = 1.            ; 1 is default
  res@tiYAxisString           = "Raw areal Avg"
  plot(0) = gsn_csm_xy(wks,yrfrac,xts(1,:),res)

  res@gsnYRefLine             =  0.   
  res@gsnAboveYRefLineColor   = "Red"   
  res@gsnBelowYRefLineColor   = "Blue" 
  res@tiYAxisString           = "Band-Pass"
  plot(1) = gsn_csm_xy(wks,yrfrac,xts(0,:),res)

  delete(res@gsnAboveYRefLineColor)
  delete(res@gsnBelowYRefLineColor)
  delete(res@gsnYRefLine)

  res@tmXBLabelsOn            = True
  res@tmXBOn                  = True
  res@tmXBFormat              = "f"                 ; remove trailing .0s

  res@xyLineColor             = "green"         
  res@xyLineThicknessF        = 2.                  ; 1 is default

  yrfrac@long_name            = "date as fraction of year"
  res@tiYAxisString           = "Run Variance"
  plot(2) = gsn_csm_xy(wks,yrfrac,xts(2,:),res)
;************************************************
  resP                     = True                ; modify the panel plot
  if (opt .and. isatt(opt,"pltTitle")) then
      resP@txString        = opt@pltTitle
  else
      resP@txString        = xts@long_name+": Areal Averaged & Filtered: "  \
                            +xts@bpfStrt+"-"+xts@bpfLast+" days: nw="  \
                            +xts@bpfNwgt 
  end if
  resP@gsnMaximize         = True
  resP@gsnPaperOrientation = "portrait"          ; force portrait
  resP@gsnPanelBottom      = 0.05                ; add some space at bottom 
  gsn_panel(wks,plot,(/3,1/),resP)     

end
; +++++
undef ("band_pass_area_time_plot_cam")
procedure band_pass_area_time_plot_cam(          \ 
          xts[3][*]:numeric, date[*]:integer, datesec[*]:integer, \
          pltDir[1]:string, pltType[1]:string, pltName[1]:string)
begin
  ; place holder
end

;=========================================
undef ("band_pass_latlon_time")
function band_pass_latlon_time \
                 (x[*][*][*]:numeric, spd[1]:numeric   \
                 ,bpf[3]:numeric, opt:logical)

local nMsg, dimx, ntim, nlat, mlon, dNam, n, tim_taper \
    , bpfStrt, bpfLast, bpfNwgt, fca, fcb, ihp, sigma    \
    , bpfWgt, work, WORK, cf, WCF
begin
  nMsg = num( ismissing(x) )
  if (nMsg.gt.0) then
      print("band_pass_latlon_time: currently, missing data not allowed: nMsg="+nMsg)
      exit
  end if

  dimx    = dimsizes(x)
  ntim    = dimx(0)
  nlat    = dimx(1)
  mlon    = dimx(2)

  bpfStrt = bpf(0)                     ; days
  bpfLast = bpf(1)
  bpfNwgt = bpf(2)                     ; effective # weights

  bpfNwgt = (bpfNwgt/2)*2 + 1          ; make sure it is odd

  if (bpfStrt.gt.bpfLast) then         ; safety check
      dumy    = bpfLast
      bpfLast = bpfStrt
      bpfStrt = dumy
  end if

  fca  = 1.0/(spd*bpfLast)
  fcb  = 1.0/(spd*bpfStrt)

  dNam = getvardims(x)                 ; get dimension names
  do n=0,2                             ; only used if detrending
     if (ismissing(dNam(n))) then      ; or tapering in time
         x!n  = "dim"+n                ; assign name
         dNam =  x!n
     end if
  end do

  ihp    = 2                               ; bpf=>band pass filter
  sigma  = 1.0                             ; Lanczos sigma
  bpfwgt = filwgts_lanczos (bpfNwgt, ihp, fca, fcb, sigma )
;;printVarSummary(bpfwgt))

  work   = x
  tdim   = 0   ; time is assumed to be the leftmost dimension

  if (opt .and. isatt(opt,"fft") .and. opt@fft) then 

; Beginning with 6.2.0 there are ezfft_n / exfftb_n 

      if (isatt(work,"_FillValue")) then
          delete(work@_FillValue)          ; avoid annoying warning msg
      end if                               ; when calling ezfftf

                                           ; By default ... no detrend
      if (opt .and.(isatt(opt,"detrend") .and. opt@detrend)) then
          work  = dtrend_n(work, False, tdim) ; detrend array
          work@detrend = "data detrended in time"        
      end if
                                           ; By default ... no taper
      if (isatt(opt,"taper")) then
          tim_taper = opt@taper
      else
          tim_taper = 0.10                 ; default taper is 10%
      end if

      work       = taper_n(work, tim_taper, 0, tdim)  
      work@taper = "data tapered in time"        
                                           ; fft in time
      cf   = ezfftf_n (work, tdim)         ; cf(2,ntim/2,nlat,mlon)
     ;printVarSummary(cf)                  ;    0,   1  ,  2 ,  3
                                           ; map response of digitial
                                           ; filter to fft space
      fcf  = fspan(0, 0.5, ntim/2)         ; fft freq
      wcf  = linint1 (bpfwgt@freq, bpfwgt@resp, False, fcf, 0)
      WCF  = conform(cf, wcf, 1)
      delete(wcf)
                               
      cf   = cf*WCF                        ; apply response coef
      delete(WCF)
       
      work = ezfftb_n(cf, 0.0, 1)          ; fourier synthesis
      delete(cf)
      work@process  =  "FFT with digital respoonse mapped to FFT space"
  else
      work  = dtrend_n(work, False, 0 )   ; detrend
      work@detrend = "data detrended in time"        
      
      work   = wgt_runave_n_Wrap(work,bpfwgt,0,0)
      work@process  =  "wgt_runave"
  end if

  work@band_pass_start = bpfStrt
  work@band_pass_last  = bpfLast
  work@band_pass_Nwgts = bpfNwgt

  if (isatt(work,"long_name")) then
      work@long_name = "Band Pass: "+work@long_name
  else
      work@long_name = "Band Pass: "
  end if

  return(work)
end
; ----------------------------------------------------
undef ("band_pass_hovmueller")  
function band_pass_hovmueller(                         \
                  x[*][*][*]:numeric, spd[1]:numeric   \
                 ,bpf[3]:numeric, wy[*]:numeric, opt:logical)

local nMsg, dimx, ntim, nlat, mlon, dNam, n, saveFillV \
    , bpfStrt, bpfLast, bpfNwgt, fca, fcb, ihp, sigma    \
    , bpfWgt, wgty, work, WORK, cf, WCF
begin

  nMsg = num( ismissing(x) )            ; error check
  if (nMsg.gt.0) then
      print("band_pass_hovmueller: currently, missing data not allowed: nMsg="+nMsg)
      exit
  end if

  dimx    = dimsizes(x)                 
  dimwy   = dimsizes(wy)                ; size wy
  if (dimwy.gt.1 .and. dimx(1).ne.dimwy(0)) then
      print("band_pass_hovmueller: sizes of y/lat dimension do not match")
      print("                      dimsizes(wy)="+dimwy)
      print("                      dimx(1)     ="+dimx(1))
      exit
  end if
      
  if (dimwy.eq.1) then                  ; scalar
      wgty = new( dimwy, typeof(wy), getFillValue(wy))
  end if
  wgty = wy

  ntim = dimx(0)
  nlat = dimx(1)
  mlon = dimx(2)

  bpfStrt = bpf(0)                     ; days
  bpfLast = bpf(1)
  bpfNwgt = bpf(2)                     ; effective # weights

  bpfNwgt = (bpfNwgt/2)*2 + 1          ; make sure it is odd

  if (bpfStrt.gt.bpfLast) then         ; safety check
      dumy    = bpfLast
      bpfLast = bpfStrt
      bpfStrt = dumy
  end if

  fca  = 1.0/(spd*bpfLast)             ; frq corresponding to
  fcb  = 1.0/(spd*bpfStrt)             ; bpfStrt and bpfLast

  dNam = getvardims(x)                 ; get dimension names
  do n=0,2                             ; only used if detrending
     if (ismissing(dNam(n))) then     
         x!n  = "dim"+n              
         dNam =  x!n
     end if
  end do
                                       ; weighted average at each (lon,time)
  work = dim_avg_wgt_n_Wrap(x, wgty, 0, 1) 
;;work = dim_avg_wgt_Wrap(x($dNam(2)$|:, $dNam(0)$|:, $dNam(1)$|:), wgty, 0) 
                                       ; unweighted average at each (lon,time)
;;work = dim_avg_n_Wrap(x, 0, 1) 
;;work = dim_avg_Wrap(x($dNam(2)$|:, $dNam(0)$|:, $dNam(1)$|:)) 

                                       ; Remove overall mean 
  work = work - avg(work)   
                                       ; By default ... no detrend in time
  if (opt .and.(isatt(opt,"detrend") .and. opt@detrend)) then
      if (isatt(work,"_FillValue")) then
          saveFillV = work@_FillValue  ; avoid annoying warning msg
          delete(work@_FillValue)
      end if                           

      work  = dtrend(work, False )     ; detrend in time

      if (isvar("saveFillV")) then
          work@_FillValue = saveFillV  ; reassign 
      end if
      work@detrend = "data detrended in time"        
  end if

  ihp    = 2                               ; bpf=>band pass filter
  sigma  = 1.0                             ; Lanczos sigma
  bpfwgt = filwgts_lanczos (bpfNwgt, ihp, fca, fcb, sigma )

  work   = wgt_runave_n_Wrap(work,bpfwgt,0,0) ; apply filter to time
;;work   = wgt_runave_Wrap(work,bpfwgt,0) ; apply filter to time

  work@band_pass_start = bpfStrt
  work@band_pass_last  = bpfLast
  work@band_pass_Nwgts = bpfNwgt

;;printVarSummary(work)

  
  return(  work )  ; (time,lon)
;;return(  work($dNam(0)$|:, $dNam(2)$|:) )  ; (time,lon)
end
; ==============>  prototype plot procedure  <================= 
; create Hovmueller plots
; =============================================================
undef ("band_pass_hovmueller_plot")
procedure band_pass_hovmueller_plot( x[*][*]:numeric    \  ; (time,lon)
                                   , pltDir[1]:string   \
                                   , pltType[1]:string  \
                                   , pltName[1]:string  \
                                   , opt[1]:logical  )

local date, yrfrac, TIME, ntim, yyyy, mm, dd, hh, pltPath \
    , resH, hplt, nt, ntm, monNam, tValL, tLabL           \
    , tValR, tLabR
begin

  pltPath = pltDir+"/"+pltName

  wks     = gsn_open_wks(pltType , pltPath)
  gsn_define_colormap(wks,"amwg_blueyellowred") 

  resH                       = True     ; plot mods desired
  resH@cnFillOn              = True     ; turn on color fill
  resH@cnLinesOn             = False    ; turn of contour lines
  resH@cnLineLabelsOn        = False    ; turn of contour line labels

;; No longer needed in V6.1.0 and later
;;resH@gsnSpreadColors       = True     ; use full range of color map
;;resH@gsnSpreadColorStart   =  2     
;;resH@gsnSpreadColorEnd     = 17    
;;resH@lbLabelAutoStride     = True     ; let NCL figure spacing

  resH@pmLabelBarHeightF     = 0.1      ; default is taller

  resH@trYReverse            = True     ; reverse the y (time) axis
  resH@gsnMaximize           = True  
  resH@gsnPaperOrientation   = "portrait"

  symMinMaxPlt (x,16,False,resH)        ; nice symmetric range
                                        ; user settings?
  if (isatt(opt,"cnLevelSpacingF")) then
      resH@cnLevelSpacingF = opt@cnLevelSpacingF
  end if
  if (isatt(opt,"cnMinLevelValF")) then
      resH@cnMinLevelValF = opt@cnMinLevelValF
  end if
  if (isatt(opt,"cnMaxLevelValF")) then
      resH@cnMaxLevelValF = opt@cnMaxLevelValF
  end if
  if (isatt(opt,"cnMinLevelValF") .and. \  ; user should have -/+ and be =
      isatt(opt,"cnMaxLevelValF") ) then 
      if (abs(opt@cnMinLevelValF).ne.opt@cnMaxLevelValF) then
          print(" ")
          print("band_pass_hovmueller_plot: user min/max are not symmetric")
          print("                           color plot may be misleading  ")
          print("                           cnMinLevelValF="+opt@cnMinLevelValF)
          print("                           cnMaxLevelValF="+opt@cnMaxLevelValF)
          print(" ")
      end if
  end if

;;resH@tmLabelAutoStride     = True     ; don't let labels overlap

                                        ; TIME RELATED VARIABLES
  time   = x&time                       ; clarity
  date   = ut_calendar(x&time, -3)      ; gregorian calendar
  yrfrac = yyyymmddhh2yyyyFrac( date)
  delete(yrfrac@long_name)

  TIME   = ut_calendar(x&time, 0 )      ; gregorian calendar
  yyyy   = floattoint( TIME(:,0) )
  mm     = floattoint( TIME(:,1) )
  dd     = floattoint( TIME(:,2) )
 ;hh     = floattoint( TIME(:,3) )
  ntim   = dimsizes(date)
                                        ; Left axis
  if (opt .and. isatt(opt,"yearFraction")) then
      resH@tmYLMode              = "Manual"
      if (isatt(opt,"yearFractionSpacingF")) then
          resH@tmYLTickSpacingF  = opt@yearFractionSpacingF
      else
          resH@tmYLTickSpacingF  = 0.25    ; 0, .25, .50, .75
      end if
    ;;resH@tmYLTickStartF = yyyy(0)
    ;;resH@tmYLTickEndF   = yyyy(ntim-1)
      x&time = yrfrac
  else
      monNam = (/"Jan","Feb","Mar","Apr","May","Jun" \
                ,"Jul","Aug","Sep","Oct","Nov","Dec" /)
      tValL = new(ntim, typeof(x&time) , "No_FillValue") ; bigger than
      tLabL = new(ntim, "string", "No_FillValue")      ; needed
     ;tValR = tValL
     ;tLabR = tLabL
 
      if (opt .and. isatt(opt,"monthLabels")) then
          monthLabels = opt@monthLabels
      else
          monthLabels = (/1,4,7,10/)
      end if

      ntm   = -1
      do nt=0,ntim-1
         if (dd(nt).eq.1 .and. any(mm(nt).eq.monthLabels)) then
             ntm        = ntm + 1
             tValL(ntm) = x&time(nt)
             tLabL(ntm) = monNam(mm(nt)-1)
            ;tValR(ntm) = x&time(nt)
             if (dd(nt).eq.1 .and. mm(nt).eq.1) then
                 tLabL(ntm) = sprinti("%0.4i", yyyy(nt))+" "+tLabL(ntm)
            ;    tLabL(ntm) = yyyy(nt)+" "+tLabL(ntm)
            ;   ;tLabL(ntm) = tLabL(ntm)+"~C~"+yyyy(nt)
            ;    tLabR(ntm) = yyyy(nt)
            ;else
            ;    tLabR(ntm) = ""
             end if
         end if
      end do

      resH@tmYLMode              = "Explicit"
      resH@tmYLValues            = tValL(0:ntm)
      resH@tmYLLabels            = tLabL(0:ntm)
    
     ;resH@tmYRLabelsOn          = True              ; use for year
     ;resH@tmYUseLeft            = False
     ;resH@tmYRMode              = "Explicit"
     ;resH@tmYRValues            = tValR(0:ntm)
     ;resH@tmYRLabels            = tLabR(0:ntm)

  end if
 ;resH@cnLevelSpacingF =  20
 ;resH@cnMaxLevelValF  =  80
 ;resH@cnMinLevelValF  = -80
 ;resH@cnLevelSelectionMode = "ManualLevels"
				    
 ;hplt   = gsn_csm_hov(wks, x, resH)   
  hplt   = gsn_csm_contour(wks, x, resH)   

  if (isatt(resH,"tmYLMode") .and. resH@tmYLMode.eq."Manual") then
      x&time = time   ; reassign original time coordinate variable
  end if
end
; ====================================================
undef("mjo_spectra_season")
function mjo_spectra_season \
                 (x[*][*][*]:numeric, date[*]:integer, wy[*]:numeric \
                 ,seaName[1]:string, opt:logical)

; MJO CLIVAR: perform segment averaging
;             winter starts Nov 1  [180 days]
;             summer starts May 1  [180 days]
;             annual starts Jan 1  [365 days]

local nMsg, dimx, dimwy, ntim, save_FillValue, xts, nr, ns   \
    , nDay, nSea, iSea, mmdd, yyyy, mmddStrt, sdof, z1, smjo \
    , spec, d, sm, pct, xtsVar, xvari
begin
                                      ; error check
  nMsg = num( ismissing(x) )
  if (nMsg.gt.0) then
      print("mjo_spectra: currently, missing data not allowed: nMsg="+nMsg)
      exit
  end if

  dimx  = dimsizes(x)
  dimwy = dimsizes(wy)
  if (dimx(1).ne.dimwy(0)) then
      print("band_pass_area_time: sizes of y/lat dimension do not match")
      print("                     dimsizes(wy)="+dimwy)              
      print("                     dimx(1)=nlat="+dimx(1))              
      exit
  end if
  ntim = dimx(0)
  xts  = wgt_areaave_Wrap(x, wy, 1., 0)    ; (time)

  if (opt .and. isatt(opt,"detrend") .and. opt@detrend) then
      if (isatt(xts,"_FillValue")) then
          save_FillValue = x@_FillValue
          delete(xts@_FillValue)           ; avoid annoying warning msg
      end if                               ; from dtrend

      xts = dtrend(xts, False )  ; detrend 

      if (isvar("save_FillValue")) then
          xts@_FillValue = save_FillValue  ; reassign
      end if
  end if

  nDay = 180
  if (seaName.eq."winter") then
      mmddStrt = 1101                      ; Nov 1
  end if
  if (seaName.eq."summer") then
      mmddStrt =  501                      ; May 1
  end if
  if (seaName.eq."annual") then
      nDay     = 365
      mmddStrt = 101                       ; Jan 1
  end if

  yyyy = date/10000
  mmdd = date - yyyy*10000
  nSea = num(mmdd.eq.mmddStrt)             ; number of seasons
  iSea = ind(mmdd.eq.mmddStrt) 
  if (seaName.eq."winter") then
      nSea = nSea-1                        ; last season not complete
  end if

;*****************************************************************
; calculate spectrum via segment averaging
; MJO Clivar says "no" to detrending/tapering
;*****************************************************************
  if (opt .and. isatt(opt,"detrend_seg") ) then
      d   = opt@detrend_seg
  else
      d   = 0   ; detrending opt: 0=>remove mean 1=>remove mean and detrend
  end if
  if (opt .and. isatt(opt,"smooth_seg") ) then
      sm  = opt@smooth_seg
  else
      sm  = 0   ; smooth periodogram: 0 mean no smooth [pure periodgram]
  end if
  if (opt .and. isatt(opt,"taper_seg") ) then
      pct = opt@taper_seg
  else
      pct = 0.0 ; percent tapered: (0.0 <= pct <= 1.0) 0.10 common. 
  end if

  spec  = new ( nDay/2, typeof(x))
  spec  = 0.0
  z1    = 0.0                              
  xtsVar= 0.0

  do ns=0,nSea-1                           ; segment averaging
     iStrt  = iSea(ns) 
     iLast  = iSea(ns)+nDay-1 
     sdof   = specx_anal(xts(iStrt:iLast),d,sm,pct)
     spec   = spec + sdof@spcx
     xtsVar = xtsVar + sdof@xvari
     z1     = z1   + 0.5*log((1+sdof@xlag1)/(1-sdof@xlag1)) ; Fischer z-transform
  end do

  spec  = spec/nSea                         ; average spectra
  z1    = z1/nSea                           ; mean z
  xvari = xtsVar/nSea                       ; mean variance per segment

  smjo       = (/ sdof /)
  smjo       = 2*nSea                       ; # degrees freedom
  smjo@bw    = sdof@bw
  smjo@spcx  = spec
  smjo@xvari = xvari       
  smjo@frq   = sdof@frq
  smjo@xlag1 = (exp(2*z1)-1)/(exp(2*z1)+1)  ; match specx_anal 

  if (opt .and. isatt(opt,"smooth_seg") ) then
      smjo      = smjo*opt@smooth_seg
      smjo@bw   = (sdof@bw)*(opt@smooth_seg)
  end if

  return( smjo )
end
;*******************************************************************
undef("mjo_spectra")
procedure mjo_spectra \
                 (X[*][*][*]:numeric, date[*]:integer, wy[*]:numeric \
                 ,latS[*]:numeric, latN[*]:numeric                   \
                 ,lonL[*]:numeric, lonR[*]:numeric, nameRegion[*]:string \
                 ,pltDir[1]:string, pltType[1]:string                \
                 ,pltName[1]:string, opt[1]:logical)

; driver for MJO CLIVAR segment averaging

; MJO CLIVAR:  http://climate.snu.ac.kr/mjo_diagnostics/index.htm
;              Plot spectra with x-axis as frequency with log scaling and 
;              y-axis as power times frequency

local frqmnPlt, frqmxPlt, nameSeason, nSeason, r, resP, tiXAxisString \
    , ns, nr, plot, x, pltNameLocal, pltTypeLocal, pltPath, sMJO, splt\
    , ifrqmxPlt, nRegion, ciLow, ciHigh
begin

  frqmxPlt   = 0.15         ; graphics X -axis  [frequency]
  frqmnPlt   = 1./365.

  nameSeason = (/ "winter", "summer", "annual" /)
  nSeason = dimsizes(nameSeason)

  nRegion = dimsizes(nameRegion)

;*******************************************************************
; general graphics resources
;*******************************************************************
  r                 = True                      ; plot mods desired
  r@gsnDraw         = False                     ; do not draw
  r@gsnFrame        = False                     ; do not advance frame
  r@tiYAxisString   = "Power x freq"            ; yaxis
  r@xyLineThicknesses   = (/2, 2, 1, 1/)        ; Define line thicknesses 
  r@xyDashPatterns  = (/0, 0, 1, 1/)            ; Dash patterns 
  r@xyLineColors    = (/"foreground","red","blue","blue"/)
  r@vpHeightF       = 0.4               ; change aspect ratio of plot
  r@vpWidthF 	    = 0.7

  r@trXLog          = True                      ; default
  if (opt .and. isatt(opt,"logXAxis") .and. .not.opt@logXAxis) then
      delete(r@trXLog)                          ; linear x (freq) axis
  end if
  
  if (opt .and. isatt(opt,"periodXAxis") .and. .not.opt@periodXAxis) then
      tiXAxisString = "Frequency (cycles/day)"  ; x (freq) axis
  else                                          
      r@tmXBMode    = "Explicit"                ; default
      r@tmXBValues  = (/0.1, 0.05, 0.0333, 0.0167, 0.01, frqmnPlt/)
      r@tmXBLabels  = (/ 10,   20,   30  ,   60  ,  100,   365/)
      tiXAxisString = "Period (days)"           ; xaxis
  end if
                                                ; linear Y is default
  if (opt .and. isatt(opt,"logYAxis") .and. opt@logYAxis) then
      r@trYLog  = True
  end if

  resP                  = True             
  resP@gsnMaximize      = True            
  resP@gsnStringFontHeightF = 0.025   
  resP@gsnPanelBottom   = 0.05

  plot = new ( nSeason, "graphic")

  r@trXMaxF  = frqmxPlt                         ; X Axis [rightmost freq]

;;if (pltType.eq."png") then
;;    pltTypeLocal = "eps"
;;else
      pltTypeLocal = pltType                    ; historical reasons
;;end if

  ciLow  = 0.05                                 ; default
  ciHigh = 0.95
  if (opt .and. isatt(opt,"spcConLim")) then
      if (dimsizes(opt@spcConLim).ne.2) then
          print("mjo_spectra: spcConLim attribute must be size 2")
          print("             dimsizes(spcConLim)="+dimsizes(opt@spcConLim))
          exit
      else
          if (any(opt@spcConLim.lt.0 .or. opt@spcConLim.gt.1)) then
              print("mjo_spectra: 0 < spcConLim < 1 violation")
              print("spcConLim: "+opt@spcConLim)
              exit
          else
              ciLow  = opt@spcConLim(0)         ; user specified bounds
              ciHigh = opt@spcConLim(1)
          end if
      end if
  end if

  do nr=0,nRegion-1
                                                ;  extract region 
     x    = X(:,{latS(nr):latN(nr)},{lonL(nr):lonR(nr)}) 
     wgty = wy({latS(nr):latN(nr)})

     pltNameLocal = pltName+"_"+nameRegion(nr)        
     pltPath      = pltDir+pltNameLocal
     wks          = gsn_open_wks(pltTypeLocal,pltPath)        

    do ns=0,nSeason-1
       sMJO     = mjo_spectra_season(x, date, wgty, nameSeason(ns), opt)
       splt     = specx_ci (sMJO, ciLow,ciHigh)

       ifrqmxPlt= ind(sMJO@frq .ge. frqmnPlt .and. sMJO@frq .le. frqmxPlt)
       r@gsnCenterString = nameSeason(ns)

       if (isatt(r,"trXLog") .and. r@trXLog) then
           r@trXMinF  = frqmnPlt      
       end if

       if (isatt(r,"trYLog") .and. r@trYLog) then
           r@trYMinF  = 0.95*min(splt(:,ifrqmxPlt))
           r@trYMaxF  = 1.05*max(splt(:,ifrqmxPlt))
       end if

       if (ns.eq.(nSeason-1)) then
           r@tiXAxisString = tiXAxisString
       end if

      ;plot(ns) = gsn_csm_xy(wks,sMJO@frq(ifrqmxPlt), splt(:,ifrqmxPlt),r)
       work     = splt
       work     = work*conform(work,sMJO@frq,1)
       plot(ns) = gsn_csm_xy(wks,sMJO@frq(ifrqmxPlt), work(:,ifrqmxPlt),r)

       if (ns.eq.(nSeason-1)) then
           delete(r@tiXAxisString)
       end if

       delete([/sMJO, splt, work, ifrqmxPlt /])
    end do    
     resP@txString = nameRegion(nr)+": "+x@long_name
     gsn_panel(wks,plot,(/3,1/),resP)   

     if (pltTypeLocal.eq."png")
         if (isatt(opt,"pltConvert")) then 
             pltConvert = opt@pltConvert    ; convert options
         else
             pltConvert = "-trim +repage -border 8 -bordercolor white"  ; trim, nice border
         end if
         system("convert "+pltConvert+" "+pltPath+"."+pltTypeLocal+" "+pltPath+"."+pltTypeLocal)
     end if

     delete([/ x, wgty /])
  end do    

end

;*******************************************************************
undef("mjo_xcor_lag_season")
function mjo_xcor_lag_season(x[*]:numeric, y[*][*]:numeric, mxlag[1]:integer, opt[1]:logical) 
; cross correlations at assorted lags
local dNam, dimy, N, yNew, xNew, y_Lead_x, x_Lead_y, ccr
begin
  dNam     = getvardims(y)
  dimy     = dimsizes(y)
  N        = dimy(1)                         ; N = mlon or nlat

  yNew     = y($dNam(1)$|:,$dNam(0)$|:)
  xNew     = conform(yNew,x,1)                     

  y_Lead_x = esccr(xNew,yNew,mxlag)          ; (N,mxlag+1)
  x_Lead_y = esccr(yNew,xNew,mxlag)    

  ccr      = new ( (/N,2*mxlag+1/), float)    
  ccr(:,0:mxlag-1) = x_Lead_y(:,1:mxlag:-1)  ; "negative lag", -1 reverses order
  ccr(:,mxlag:)    = y_Lead_x(:,0:mxlag)     ; "positive lag"

  ccr      = where(ccr.gt. 1.0, 1.0, ccr)   
  ccr      = where(ccr.lt.-1.0,-1.0, ccr)     

  if (opt .and. isatt(opt,"smth9") ) then
      if (abs(opt@smth9).eq.0.25) then
          ccr  = smth9(ccr, 0.50, opt@smth9, False)
      else
          print("mjo_xcor_lag_season: opt@smth9 must be 0.25 or -0.25")
          print("              opt@smth9="+opt@smth9)
          print("              NO SMOOTHING PERFORMED")
      end if
  end if

  ccr!0          = dNam(1)
  ccr&$dNam(1)$  = y&$dNam(1)$
  ccr!1          = "lag"
  ccr&lag        = ispan(-mxlag,mxlag,1)
  ccr&lag@units  = "lag (days)"
  ccr@long_name  = "cross-correlation"

  return(ccr(lag|:,$dNam(1)$|:))            
end

;**********************************************************************
undef("mjo_xcor_lag_plot_ovly")
procedure mjo_xcor_lag_plot_ovly( \
                   ccr_a[*][*]:numeric, ccr_b[*][*]:numeric \
                  ,pltType[1]:string, pltDir[1]:string      \
                  ,pltName[1]:string, opt[1]:logical)
local pltPath, pltTypeLocal, res1, res2, CCR_a, CCR_b, plot_a, plot_b
begin

  pltPath = pltDir+"/"+pltName

;;if (pltType.eq."png") then
;;    pltTypeLocal = "eps"
;;else
      pltTypeLocal = pltType
;;end if

  wks  = gsn_open_wks(pltTypeLocal,pltPath)  
  if (opt .and. isatt(opt,"colorTable") .or. isatt(opt,"pltColorTable")) then
      if (isatt(opt,"colorTable")) then
          gsn_define_colormap(wks,opt@colorTable) 
      else
          gsn_define_colormap(wks,opt@pltColorTable) 
      end if
  else
     gsn_define_colormap(wks,"BlWhRe") 
  end if

  res1                      = True          ; color precip     
  res1@gsnDraw              = False
  res1@gsnFrame             = False
  res1@gsnMaximize          = True 
  res1@gsnPaperOrientation  = "portrait"

  res1@cnFillOn             = True          ; turn on color
  res1@cnLinesOn            = False

;;No longer needed in V6.1.0 and later
;;res1@gsnSpreadColors      = True          ; use full range of colormap
;;res1@lbLabelAutoStride    = True

  res1@cnLevelSelectionMode = "ManualLevels"; set manual contour levels
  res1@cnMinLevelValF       = -1.0          ; set min contour level
  res1@cnMaxLevelValF       =  1.0          ; set max contour level
  res1@cnLevelSpacingF      =  0.1          ; set contour spacing

  res1@cnLabelBarEndLabelsOn= True
  res1@cnLabelBarEndStyle   = "ExcludeOuterBoxes"

  res1@vpWidthF             = 0.6           ; change aspect ratio of plot
  res1@vpHeightF            = 0.4

 ;res1@gsnMajorLatSpacing   = 10            ; change maj lat tm spacing

  res1@tiYAxisString        = "lag (days)"
  if (isatt(opt,"tiMainString")) then
      res1@tiMainString     = opt@tiMainString
  end if
  if (isatt(opt,"gsnLeftString")) then
      res1@gsnLeftString    = opt@gsnLeftString
  end if
  if (isatt(opt,"gsnCenterString")) then
      res1@gsnCenterString  = opt@gsnCenterString
  end if
  if (isatt(opt,"gsnRightString")) then
      res1@gsnRightString   = opt@gsnRightString
  end if

;************************************************
; resource list for second data array
;************************************************
  res2                      = True              ; U
  res2@gsnDraw              = False
  res2@gsnFrame             = False
  res2@cnLevelSelectionMode = "ManualLevels"    ; set manual contour levels
  res2@cnMinLevelValF       = -1.0
  res2@cnMaxLevelValF       =  1.0
  res2@cnLevelSpacingF      =  0.1
  res2@cnLineLabelsOn       = True
  res2@gsnContourZeroLineThicknessF = 0. 	; Eliminate 0 line
  res2@gsnContourNegLineDashPattern = 1 	; negative contours dash pattern
  res2@cnLineThicknessF     = 2.                ; line thickness
  res2@cnInfoLabelOn        = False

  CCR_a = ccr_a             ; possible smooth and delete of attribute
  CCR_b = ccr_b
 
  if (opt .and. isatt(opt,"smth9") .and. abs(opt@smth9).eq.0.25) then
      CCR_a = smth9(CCR_a, 0.50, opt@smth9, False)
  end if
  delete(CCR_a@long_name)
  plot_a = gsn_csm_contour(wks,CCR_a,res1)       ; contour the variable

  if (opt .and. isatt(opt,"smth9") .and. abs(opt@smth9).eq.0.25) then
      CCR_b = smth9(CCR_b, 0.50, opt@smth9, False)
  end if
  delete(CCR_b@long_name)
  plot_b = gsn_csm_contour(wks,CCR_b,res2)       ; contour the variable

  overlay(plot_a, plot_b)
  draw(plot_a)     
  frame(wks)

  if (pltTypeLocal.eq."png") then
      if (isatt(opt,"pltConvert")) then 
          pltConvert = opt@pltConvert    ; convert options
      else
          pltConvert = "-trim +repage -border 8 -bordercolor white"  ; trim, nice border
      end if
      system("convert "+pltConvert+" "+pltPath+"."+pltTypeLocal+" "+pltPath+"."+pltTypeLocal)
  end if

end
;**********************************************************************
undef("mjo_xcor_lag_ovly_panel")
procedure mjo_xcor_lag_ovly_panel( \
                   ccr_a[*][*]:numeric, ccr_b[*][*]:numeric \
                  ,ccr_c[*][*]:numeric, ccr_d[*][*]:numeric \
                  ,pltType[1]:string, pltDir[1]:string      \
                  ,pltName[1]:string, opt[1]:logical)
local pltPath, pltTypeLocal, res1, res2, CCR1, CCR2 \
    , plt1, plt2, resP, plot
begin

  pltPath = pltDir+"/"+pltName

;;if (pltType.eq."png") then
;;    pltTypeLocal = "eps"
;;else
      pltTypeLocal = pltType
;;end if

  wks  = gsn_open_wks(pltTypeLocal,pltPath)  
  if (opt .and. isatt(opt,"colorTable") .or. isatt(opt,"pltColorTable")) then
      if (isatt(opt,"colorTable")) then
          gsn_define_colormap(wks,opt@colorTable) 
      else
          gsn_define_colormap(wks,opt@pltColorTable) 
      end if
  else
     gsn_define_colormap(wks,"BlWhRe") 
  end if

  res1                      = True          ; color precip     
  res1@gsnDraw              = False
  res1@gsnFrame             = False
  res1@gsnMaximize          = True 
  res1@gsnPaperOrientation  = "portrait"

  res1@cnFillOn             = True          ; turn on color
  res1@cnLinesOn            = False
;;No longer needed in V6.1.0 and later
;;res1@gsnSpreadColors      = True          ; use full range of colormap
  res1@cnLevelSelectionMode = "ManualLevels"; set manual contour levels
  res1@cnMinLevelValF       = -1.0          ; set min contour level
  res1@cnMaxLevelValF       =  1.0          ; set max contour level
  res1@cnLevelSpacingF      =  0.1          ; set contour spacing

  res1@cnLabelBarEndLabelsOn= True
  res1@cnLabelBarEndStyle   = "ExcludeOuterBoxes"
  res1@cnInfoLabelOn        = False

  res1@lbLabelBarOn         = False           ; turn off individual cb's

  res1@vpWidthF             = 0.6           ; change aspect ratio of plot
  res1@vpHeightF            = 0.4

 ;res1@gsnMajorLatSpacing   = 10            ; change maj lat tm spacing

  res1@tiYAxisString        = "lag (days)"
  if (isatt(opt,"tiMainString")) then
      res1@tiMainString     = opt@tiMainString
  end if
  if (isatt(opt,"gsnLeftString")) then
      res1@gsnLeftString    = opt@gsnLeftString
  end if
  if (isatt(opt,"gsnCenterString")) then
      res1@gsnCenterString  = opt@gsnCenterString
  end if
  if (isatt(opt,"gsnRightString")) then
      res1@gsnRightString   = opt@gsnRightString
  end if

;************************************************
; resource list for second data array
;************************************************
  res2                      = True              ; U
  res2@gsnDraw              = False
  res2@gsnFrame             = False
  res2@cnLevelSelectionMode = "ManualLevels"    ; set manual contour levels
  res2@cnMinLevelValF       = -1.0
  res2@cnMaxLevelValF       =  1.0
  res2@cnLevelSpacingF      =  0.1
  res2@cnLineLabelsOn       = True
  res2@gsnContourZeroLineThicknessF = 0. 	; Eliminate 0 line
  res2@gsnContourNegLineDashPattern = 1 	; negative contours dash pattern
 ;res2@cnLineThicknessF     = 2.                ; line thickness
  res2@cnInfoLabelOn        = False

  plot                      = new( 2, "graphic")

  do np=0,1
     if (np.eq.0) then
         CCR1 = ccr_a             ; possible smooth and delete of attribute
         CCR2 = ccr_b
     else
         CCR1 = ccr_c             ; possible smooth and delete of attribute
         CCR2 = ccr_d
     end if
    
     if (opt .and. isatt(opt,"smth9") .and. abs(opt@smth9).eq.0.25) then
         CCR1 = smth9(CCR1, 0.50, opt@smth9, False)
         CCR2 = smth9(CCR2, 0.50, opt@smth9, False)
     end if
     delete(CCR1@long_name)
     plt1 = gsn_csm_contour(wks,CCR1,res1)       ; contour the variable
   
     delete(CCR2@long_name)
     plt2 = gsn_csm_contour(wks,CCR2,res2)       ; contour the variable
   
     overlay(plt1, plt2)
     plot(np) = plt1

     delete(CCR1)                                 ; size may change
     delete(CCR2)
  end do

  resP                      = True                ; panel
  resP@lbLabelAutoStride    = True
  resP@gsnPanelLabelBar     = True                ; add common colorbar
 ;resP@lbLabelFontHeightF   = 0.007               ; make labels smaller
  if (isatt(opt,"txString")) then
      resP@txString = opt@txString
  end if
  gsn_panel(wks,plot,(/2,1/),resP)                ; now draw as one plot

  if (pltTypeLocal.eq."png") then
      if (isatt(opt,"pltConvert")) then 
          pltConvert = opt@pltConvert    ; convert options
      else
          pltConvert = "-trim +repage -border 8 -bordercolor white"  ; trim, nice border
      end if
      system("convert "+pltConvert+" "+pltPath+"."+pltTypeLocal+" "+pltPath+"."+pltTypeLocal)
  end if
end
;***********************************************************
undef("mjo_xcor_lag")
function mjo_xcor_lag (xio[*]:numeric , y[*][*]:numeric \ 
                      ,date[*]:integer, mxlag[1]:integer, seaName:string, opt[1]:logical) 

; driver to calculate mean seasonal lags
local ns, ntim, dimy, ntmy, my, nDay, mmddStrt, yyyy, mmdd, nSea, iSea, rtyp
    , rLag, nLag, rr, iStrt, iLast
begin

     ntim = dimsizes(xio) 
     dimy = dimsizes(y)
     ntmy = dimy(0)
     my   = dimy(1)

     if (ntim.ne.ntmy) then
         print("mjo_xcor_lag: time dimensions do not match")
         print("              ntim="+ntim+"  ntmy="+ntmy)
         exit
     end if
   
     nDay = 180
     if (seaName.eq."winter") then
         mmddStrt = 1101                      ; Nov 1
     end if
     if (seaName.eq."summer") then
         mmddStrt =  501                      ; May 1
     end if
     if (seaName.eq."annual") then
         nDay     = 365
         mmddStrt = 101                       ; Jan 1
     end if
   
     yyyy = date/10000
     mmdd = date - yyyy*10000
     nSea = num(mmdd.eq.mmddStrt)             ; number of 'seaNam' seasons
     iSea = ind(mmdd.eq.mmddStrt) 
     if (seaName.eq."winter") then
         nSea = nSea-1                        ; last season not complete
     end if
     
     rtyp = "float"
     if (typeof(xio).eq."double" .or. typeof(y).eq."double") then 
         rtyp = "double"
     end if

     nLag = 2*mxlag+1
     rz   = new ( (/nLag,my/), rtyp, getFillValue(y) )
     rz   = 0.0

     do ns=0,nSea-1                           ; seasonal averaging
        iStrt  = iSea(ns) 
        iLast  = iSea(ns)+nDay-1 
        if (iLast.gt.(ntim-1)) then
            break
        end if

        rLag  = mjo_xcor_lag_season (xio(iStrt:iLast), y(iStrt:iLast,:) ,mxlag, opt)

        rLag  = where(rLag.gt.0.999, 0.999, rLag)        ; avoid rounding issue
                                                         ; so (1-rLag) .ne. 0.0

        rz    = rz   + 0.5*log((1+rLag)/(1-rLag)) ; Fischer z-transform
     end do

     rz  = rz/nSea                           ; mean Fischer-z
     rz  = (exp(2*rz)-1)/(exp(2*rz)+1)       ; transform back  

     copy_VarMeta(rLag, rz)
     return( rz )
end

; ====================================================
undef("mjo_wavenum_freq_season")
function mjo_wavenum_freq_season \
                 (X[*][*]:numeric, date[*]:integer \
                 ,seaName[1]:string, opt:logical)
;
; For each 'seaName' (say, 'winter') over a number
; of different years, calculate the
; pooled space-time spectra

; MJO CLIVAR: wavenumber-frequency spectra 
;             winter starts Nov 1  [180 days]
;             summer starts May 1  [180 days]
;             annual starts Jan 1  [365 days]

local nMsg, dimX, ntim, mlon, x,  N, pow, xAvg, xSeason       \
    , nDay, ny, nYear, iSea, mmdd, yyyy, mmddStrt, work, work1\    
    , d, sm, pct, xVarSea, kSea, wave, freq, iStrt, iLast
begin
                                      ; error check
  nMsg = num( ismissing(X) )
  if (nMsg.gt.0) then
      print("mjo_wavenum_freq_season: currently, missing data not allowed: nMsg="+nMsg)
      exit
  end if

  dimX = dimsizes(X)

  ntim = dimX(0)                       ; total times
  mlon = dimX(1)

  x    = X                             ; local copy (time,lon)
                                       ; x may change
  if (isatt(x,"_FillValue")) then
      delete(x@_FillValue)             ; avoid annoying warning msg
  end if                               ; from dtrend

  nDay = 180
  if (seaName.eq."winter") then
      mmddStrt = 1101                      ; Nov 1
  end if
  if (seaName.eq."summer") then
      mmddStrt =  501                      ; May 1
  end if
  if (seaName.eq."annual") then
      nDay     = 365
      mmddStrt = 101                       ; Jan 1
  end if

  yyyy  = date/10000
  mmdd  = date - yyyy*10000
  nYear = num(mmdd.eq.mmddStrt)             ; number of seasons
  iSea  = ind(mmdd.eq.mmddStrt) 

;*****************************************************************
; For a specific season, calculate spectra via averaging 
; over each seasonal segment.
; MJO Clivar says "no" to detrending/tapering.
; Hence, the following are just 'place holders'
;*****************************************************************
  x       = dtrend_leftdim(x, False )       ; detrend overall series in time 

  work    = new((/2,mlon  ,nDay  /), typeof(x), "No_FillValue")  
          ; the +1 is for the 0-th  component 
  pow     = new((/  mlon+1,nDay+1/), typeof(x), "No_FillValue")  
  pow!0   = "wave"
  pow!1   = "frq"  
  pow     = 0.0
  
  xAvgSea = 0.0
  xVarSea = 0.0                             ; variance (raw)
  xVarTap = 0.0                             ; variance after tapering
  kSea    = 0                               ; count os seasons used
  N       = nDay                            ; convenience

  if (opt .and. isatt(opt,"taper_seg") ) then
      pct = opt@taper_seg   ; over-ride default
  else
      pct = 0.10            ; default; taper 10% of series 
  end if

; -------------------------------------------------------------------
; NCL does not have a complex 2D FFT at this time.
; Perform a "poorman's" complex 2D FFT by looping over time and space.
; Also, makes the code more clear (IMHO)
; -------------------------------------------------------------------

  do ny=0,nYear-1                           ; season (segment) averaging
     iStrt  = iSea(ny)                      ; start index for current season
     iLast  = iSea(ny)+nDay-1               ; last
     if (iLast.gt.(ntim-1)) then
         break
     end if
    ;print("iStrt="+iStrt+" ("+yyyy(iStrt)+mmdd(iStrt)+");  " \
    ;     +"iLast="+iLast+" ("+yyyy(iLast)+mmdd(iLast)+")"  )
     
     xSeason = x(iStrt:iLast,:)            ; keep meta data
     xAvg    = avg( xSeason )              ; season average all time/lon
     xSeason = xSeason - xAvg              ; remove season time-lon mean 
     xVarSea = xVarSea + variance( xSeason )    ; overall variance
     kSea    = kSea + 1
                                           
     xSeason = taper_leftdim( xSeason, pct, 0)  ; for each lon detrend in time
     xVarTap = xVarTap + variance( xSeason )    ; variance after tapering

    do nt=0,nDay-1                         ; each time perform complex fft(lon)
       work(:,:,nt) = cfftf( xSeason(nt,:), 0.0, 0)   ; space 
    end do                                  
     work = work/mlon                      ; normalize by # lon samples

    do ml=0,mlon-1                         ; each lon perform complex fft(time)
       work(:,ml,:) = cfftf( work(0,ml,:), work(1,ml,:), 0) ; time 
    end do
     work = work/nDay                      ; normalize by # time samples
                                           ; work(2,wave,freq)

                                           ; unscramble fft+ calculate power
     pow  = pow + resolveWavesHayashi( work, nDay, 1)     ; (wave,freq)
  end do

  xVarSea   = xVarSea/kSea               ; pooled seasonal variance
  xVarTap   = xVarTap/kSea               ; pooled seasonal variance
  pow       = pow/kSea                   ; pooled spectra
 
  if (isatt(X,"long_name")) then
      pow@long_name = x@long_name
  end if
            ; add meta data for use upon return
  pow!0     = "wave"
  pow!1     = "freq"
          
  wave      = ispan(-mlon/2,mlon/2,1) 
  wave!0    = "wave"
  wave@long_name= "zonal wavenumber"         ; for plot
  wave&wave =  wave
         
  freq      = fspan(-1*nDay/2,nDay/2,nDay+1)/nDay
  freq!0    = "freq"
  freq@long_name= "frequency (cycles/day)"   ; for plot
  freq@units= "cycles/day"
  freq&freq =  freq

  pow&wave  =  wave
  pow&freq  =  freq
   
  return( pow )
end
;--------------------------------------------------------------------
undef("mjo_wavenum_freq_season_plot")
procedure mjo_wavenum_freq_season_plot (wf[*][*],seaName[1]:string\
                     ,pltDir[1]:string, pltType[1]:string         \
                     ,pltName[1]:string, opt[1]:logical)
begin 
  pltPath= pltDir+"/"+pltName+"."+seaName
  
;;if (pltType.eq."png") then
;;    pltTypeLocal = "eps"
;;else
      pltTypeLocal = pltType
;;end if

  wks  = gsn_open_wks(pltTypeLocal, pltPath)  
  if (opt .and. isatt(opt,"colorTable") .or. isatt(opt,"pltColorTable")) then
      if (isatt(opt,"colorTable")) then
          gsn_define_colormap(wks,opt@colorTable) 
      else
          gsn_define_colormap(wks,opt@pltColorTable) 
      end if
  else
     gsn_define_colormap(wks,"prcp_2") 
  end if

  res                     = True          ; plot mods desired
  res@gsnFrame            = False
 ;res@gsnDraw             = False
  res@cnFillOn            = True          ; turn on color

;;No longer needed in V6.1.0 and later
;;res@gsnSpreadColors     = True          ; use full range of colormap
;;res@lbLabelAutoStride   = True
  
  if (isatt(opt,"tiMainString") .and. opt@tiMainString.ne."") then
      res@tiMainString    = opt@tiMainString
    else if (seaName.ne."") then
             res@tiMainString    = str_upper(seaName)
    end if
  end if
  if (isatt(opt,"gsnLeftString")) then
      res@gsnLeftString   = opt@gsnLeftString
  end if
  if (isatt(opt,"gsnCenterString")) then
      res@gsnCenterString = opt@gsnCenterString
  end if
  if (isatt(opt,"gsnRightString")) then
      res@gsnRightString  = opt@gsnRightString
  end if
  if (isatt(opt,"cnLinesOn")) then
      res@cnLinesOn    = opt@cnLinesOn
      if (.not.res@cnLinesOn) then
          res@cnLineLabelsOn = False
      end if
  end if
  if (isatt(opt,"cnLevelSelectionMode")) then
      res@cnLevelSelectionMode  = opt@cnLevelSelectionMode
  end if
  if (isatt(opt,"cnLevelSelectionMode")) then
      res@cnLevelSelectionMode  = opt@cnLevelSelectionMode
  end if
  if (isatt(opt,"cnMinLevelValF")) then
      res@cnMinLevelValF  = opt@cnMinLevelValF
  end if
  if (isatt(opt,"cnMaxLevelValF")) then
      res@cnMaxLevelValF  = opt@cnMaxLevelValF
  end if
  if (isatt(opt,"cnLevelSpacingF")) then
      res@cnLevelSpacingF  = opt@cnLevelSpacingF
  end if
  if (isatt(opt,"cnLevels")) then
      res@cnLevels  = opt@cnLevels
  end if
  if (isatt(opt,"cnLevels")) then
      res@cnLevels  = opt@cnLevels
  end if
  if (isatt(opt,"lbOrientation")) then
      res@lbOrientation = opt@lbOrientation
  end if
  
  NW = 6
  if (isatt(opt,"maxWavePlot")) then     ; wave [Y] axis
      NW  = opt@maxWavePlot
  end if

  fMin = -0.05  
  fMax =  0.05
  
  if (isatt(opt,"minFreqPlot")) then
      fMin  = opt@minFreqPlot
  end if 
  if (isatt(opt,"maxFreqPlot")) then
      fMax  = opt@maxFreqPlot
  end if
  
 ;res@trXMinF = fMin
 ;res@trXMaxF = fMax  
 
  WORK  = wf({0:NW},{fMin:fMax}) 
  
  if (opt .and. isatt(opt,"smth9") .and. opt@smth9) then
      WORK = smth9(WORK, 0.50,  0.25, False)
  end if

  izero = ind(WORK&freq .eq. 0.0)
  WORK(:,izero) = min(WORK)       ; 0th freq

  if (isatt(opt,"label_freq") .and. .not.opt@label_freq) then
      delete(WORK&freq@long_name)
  end if
  
  plot = gsn_csm_contour(wks, WORK , res)

  resp                  = True                      ; polyline mods desired
 ;resp@gsLineThicknessF = 2.0                       ; thickness of lines
  resp@gsLineDashPattern= 11

  tres       =  True
  tres@txFontHeightF = 0.0175
                                                    ; orig code rename
  if (opt .and. (isatt(opt,"dayLine") .or. isatt(opt,"dayLines"))) then
      if (opt .and. (isatt(opt,"dayLines"))) then
          day = min(opt@dayLines)
      else
          day = min(opt@dayLine)
      end if
  else
       day = 30         ; default 
  end if
  fline = 1./day

 ;resp@gsLineLabelString= day+"d"                   ; adds a line label string
  gsn_polyline(wks,plot,(/fline,fline/),(/ 0.,NW/), resp)      
  gsn_text(wks,plot, (day+"d"),fline+0.005,0.93*NW,tres)

                                                    ; orig code rename
  if (opt .and. (isatt(opt,"dayLine") .or. isatt(opt,"dayLines"))) then
      if (opt .and. (isatt(opt,"dayLines"))) then
          day = max(opt@dayLines)
      else
          day = max(opt@dayLine)
      end if
  else
       day = 80  
  end if
  fline = 1./day

 ;resp@gsLineLabelString= day+"d"                    ; adds a line label string
  gsn_polyline(wks,plot,(/fline,fline/),(/ 0.,NW/), resp)      
  gsn_text(wks,plot, (day+"d"),fline+0.005,0.93*NW,tres)

  frame(wks)

  if (pltTypeLocal.eq."png") then
      if (isatt(opt,"pltConvert")) then 
          pltConvert = opt@pltConvert    ; convert options
      else
          pltConvert = "-trim +repage -border 8 -bordercolor white"  ; trim, nice border
      end if
      system("convert "+pltConvert+" "+pltPath+"."+pltTypeLocal+" "+pltPath+"."+pltTypeLocal)
  end if
  
  if (pltType.eq."x11" .and. isatt(opt,"debug") .and. opt@debug) then 
      
      res@gsnCenterString = "wf"
      plot = gsn_csm_contour(wks, wf, res)
      
      res@gsnCenterString = "wf({-5:5},{-0.075:0.075})"
      plot = gsn_csm_contour(wks, wf({-5:5},{-0.075:0.075}), res)
  end if  
end  
;----------------------------------------------------------------
undef("mjo_cross")
function mjo_cross (X[*][*][*]:numeric, Y[*][*][*]:numeric \
                   ,segLen[1]:integer , segOverLap[1]:integer \
                   ,opt:logical)

local nMsgX, nMsgY, dimX, dimY, ntim, nlat, mlon, switc    \
    , x, y, pct, STC, stc, namStc, kseg, freq, wave        \
    , ntStrt, ntLast
begin
                                      ; error check
  dimX = dimsizes(X)
  dimY = dimsizes(Y)
  if (.not.all(dimX.eq.dimY)) then
      print("mjo_cross: X and Y must be same size")
      print("           dimX="+dimX+"   dimY="+dimY)
      exit
  end if

  nMsgX = num( ismissing(X) )
  nMsgY = num( ismissing(Y) )
 ;if ((nMsgX+nMsgY).gt.0) then  ; checked in builin function
 ;    print("mjo_cross: currently, missing data not allowed: nMsgX="+nMsgX+"  nMsgY="+nMsgY)
 ;    exit
 ;end if

  ntim = dimX(0)                       ; total times
  nlat = dimX(1)
  mlon = dimX(2)

  x    = X                             ; local copy (time,lat,lon)
  y    = Y            

  x    = decompose2SymAsym( x, 0)      ; decompose     
  y    = decompose2SymAsym( y, 0)
                                       ; x may change
  if (isatt(x,"_FillValue")) then
      delete(x@_FillValue)             ; avoid annoying warning msg
  end if                               ; from dtrend
  if (isatt(y,"_FillValue")) then
      delete(y@_FillValue)     
  end if                      

  x       = dtrend_leftdim(x, False )  ; detrend overall series in time 
  y       = dtrend_leftdim(y, False )   

  if (opt .and. isatt(opt,"taper_seg") ) then
      pct = opt@taper_seg   ; over-ride default
  else
      pct = 0.10            ; default; taper 10% of series 
  end if

  x       = taper_leftdim(x, pct, 0 )  ; taper in time
  y       = taper_leftdim(y, pct, 0 )  

; -------------------------------------------------------------------


  STC    = new ( (/16,segLen/2+1,mlon+1/), "double", 1d20)
  STC    = 0.0

  kseg   = 0
  ntStrt = 0
  switch = True
  do while (switch)
     ntLast = ntStrt + segLen-1 
     if (ntLast.gt.(ntim-1)) then
         switch = False
         break
     end if

     XX     = taper_leftdim( x(ntStrt:ntLast,:,:), pct, 0)
     YY     = taper_leftdim( y(ntStrt:ntLast,:,:), pct, 0)

     kseg   = kseg+1
     STC    = STC + mjo_cross_segment(XX,YY,0)

     ntStrt = ntStrt + abs(segOverLap)
  end do

  STC    = STC/kseg                          ; average

                                            ; use averaged power
  mjo_cross_coh2pha(STC, 0)                 ; to get coh2 / phase

  freq   = fspan(0,0.5,segLen/2+1)
  freq@long_term = " frq (cycles per day)"   ; for plot
  freq@units     = "cycles per day"
  freq!0 = "freq"
  wave   = fspan(-mlon/2,mlon/2,mlon+1)
  wave@long_term = "zonal wavenumber"
  wave@units     = "zonal wavenumber"
  wave!0 = "wave"

  STC!0  = "var"
  STC!1  = "freq"
  STC!2  = "wave"
  STC&freq = freq
  STC&wave = wave

  STC@varName  = (/"PEE1_SYM"   , "PEE1_ASY"    \
                  ,"PEE2_SYM"   , "PEE2_ASY"    \
                  ,"P12_SYM"    , "P12_ASY"     \
                  ,"Q12_SYM"    , "Q12_ASY"     \
                  ,"COH2_SYM"   , "COH2_ASY"    \
                  ,"PHAS_SYM"   , "PHAS_ASY"    \ 
                  ,"V1_SYM"     , "V1_ASY"      \ 
                  ,"V2_SYM"     , "V2_ASY"      /)

  STC@segmentLength  = segLen
  STC@segmentOverLap = segOverLap
  STC@segmentRepeat  = segLen-segOverLap
  STC@number_of_segments = kseg
  STC@dof            = 2.667*kseg     ; conservative estimate   
                                      ; 2.667 is for 1-2-1 smoother

  p = (/0.80, 0.85, 0.90, 0.925, 0.95, 0.99/); probability levels
  STC@prob       = p
  STC@prob_coh2  = 1.-(1.-p^(0.5*STC@dof -1))
         
  return( STC )
end
; -----------
undef("addHorVertLinesCross")
function addHorVertLinesCross(wks[1]:graphic, plot[1]:graphic \
                             ,nw[1], dumy[*]:graphic )
; freq [y] axis:  Add horizontal lines that explicitly
;                 print time in days. This assumes the units
;                 of the freq axis are "cpd" [cycles per day]
local gsres, txres, xx, dely, m\nwl, nwr
begin
    gsres = True
    gsres@gsLineDashPattern = 1

    nwl = -nw+3.5                   ; left
    nwr =  nw                       ; right
    dumy(0) = gsn_add_polyline(wks, plot, (/  0, 0/),(/ 0.0  , 0.5  /),gsres)
    dumy(1) = gsn_add_polyline(wks, plot, (/nwl,nwr/),(/ 1./80, 1./80/),gsres)
    dumy(2) = gsn_add_polyline(wks, plot, (/nwl,nwr/),(/ 1./20, 1./20/),gsres)
    dumy(3) = gsn_add_polyline(wks, plot, (/nwl,nwr/),(/ 1./10, 1./10/),gsres)
    dumy(4) = gsn_add_polyline(wks, plot, (/nwl,nwr/),(/ 1./5 , 1./5 /),gsres)
    dumy(5) = gsn_add_polyline(wks, plot, (/nwl,nwr/),(/ 1./3 , 1./3 /),gsres)

    txres         = True
    txres@txJust  = "CenterLeft"
    txres@txFontHeightF = 0.013

    xx            = -nw+0.3
    dely          = 0.000                                 ;    yy
    dumy(6) = gsn_add_text(wks, plot, "3 days" , xx ,(1./3 +dely),txres)
    dumy(7) = gsn_add_text(wks, plot, "5 days" , xx ,(1./5 +dely),txres)
    dumy(8) = gsn_add_text(wks, plot, "10 days", xx ,(1./10+dely),txres)
    dumy(9) = gsn_add_text(wks, plot, "20 days", xx ,(1./20+dely),txres)
    dumy(10)= gsn_add_text(wks, plot, "80 days", xx ,(1./80+dely),txres)

    return(plot)
end
; -----------
undef("mjo_elimIsolatedValues")
procedure mjo_elimIsolatedValues(c2, ph1, ph2, iopt)
; cosmetic and arbitrary ... eliminate isolated points ... 
; crude and not complete but 'good enough'
local npts, dimc2, nfreq, nwave, nf, nw
begin
  i = iopt
  if (iopt.le.0) then
      i = 1
  end if

  npts  = 0
  dimc2 = dimsizes(c2)
  nfreq = dimc2(0)
  nwave = dimc2(1)

  do nf=i,nfreq-i-1
    do nw=i,nwave-i-1
       if (.not.ismissing(c2(nf,nw))) then
           if (all(ismissing(c2(nf-i,nw  )).and.ismissing(c2(nf+i,nw  ))  .and.\
                   ismissing(c2(nf  ,nw-i)).and.ismissing(c2(nf  ,nw+i)))) then
               npts        = npts + 1
               c2(nf,nw)  = c2@_FillValue
               ph1(nf,nw) = ph1@_FillValue
               ph2(nf,nw) = ph2@_FillValue
           end if
       end if
    end do
     nw = 0               ; left (freq) boundary
     if (.not.ismissing(c2(nf,nw))) then
         if (all(ismissing(c2(nf-i,nw  )).and.ismissing(c2(nf+i,nw  )) .and. \
                 ismissing(c2(nf  ,nw+1)))) then
             npts        = npts + 1
             c2(nf,nw)  = c2@_FillValue
             ph1(nf,nw) = ph1@_FillValue
             ph2(nf,nw) = ph2@_FillValue
         end if
     end if
     nw = nwave-1         ; right (freq) boundary
     if (.not.ismissing(c2(nf,nw))) then
         if (all(ismissing(c2(nf-1,nw  )).and.ismissing(c2(nf+i,nw  )) .and. \
                 ismissing(c2(nf  ,nw-1)))) then
             npts        = npts + 1
             c2(nf,nw)  = c2@_FillValue
             ph1(nf,nw) = ph1@_FillValue
             ph2(nf,nw) = ph2@_FillValue
         end if
     end if

  end do

 ;print("***** elimIsolatedValues: npts = "+npts+" set to _FillValue")
end
; -----------
undef("mjo_cross_plot")
procedure mjo_cross_plot(STC[*][*][*]:numeric \
                        ,pltDir[1]:string, pltType[1]:string \
                        ,pltName[1]:string, opt[1]:logical   )
local pltPath, pltTypeLocal,wks, res
begin
  pltPath= pltDir+"/"+pltName
  
;;if (pltType.eq."png") then
;;    pltTypeLocal = "eps"
;;else
      pltTypeLocal = pltType
;;end if

  wks  = gsn_open_wks(pltTypeLocal, pltPath)  
  if (opt .and. isatt(opt,"colorTable") .or. isatt(opt,"pltColorTable")) then
      if (isatt(opt,"colorTable")) then
          gsn_define_colormap(wks,opt@colorTable) 
      else
          gsn_define_colormap(wks,opt@pltColorTable) 
      end if
  else
     gsn_define_colormap(wks,"amwg") 
  end if

  plot = new ( 2, "graphic")

  res                      = True          ; plot mods desired
  res@gsnDraw              = False
  res@gsnFrame             = False

  res@cnFillOn             = True          ; turn on color
  res@cnFillMode           = "RasterFill"  ; match WMO Clivar 

  res@gsnSpreadColors      = True          ; use full range of colormap
  if (opt .and. isatt(opt,"gsnSpreadColorStart") ) then
      res@gsnSpreadColorStart = opt@gsnSpreadColorStart
  end if
  if (opt .and. isatt(opt,"gsnSpreadColorEnd") ) then
      res@gsnSpreadColorEnd   = opt@gsnSpreadColorEnd
  end if
  res@cnLinesOn            = False
  res@cnLineLabelsOn       = False
  res@cnLevelSelectionMode = "ManualLevels"   
  res@cnMinLevelValF       =  0.05
  res@cnMaxLevelValF       =  0.65         ; correlation^2 = 0.8  
  res@cnLevelSpacingF      =  0.05
  res@cnInfoLabelOn        =  False 
  res@lbLabelBarOn         =  False        ; no individual label bars

  if (.not.opt .or. .not.isatt(opt,"pltPhase") .or. opt@pltPhase) then
      plotPhase = True
      res@vcRefMagnitudeF           = 1.0             ; define vector ref mag
      res@vcRefLengthF              = 0.01            ; define length of vec ref
      res@vcRefAnnoOrthogonalPosF   = -1.0            ; move ref vector
      res@vcRefAnnoArrowLineColor   = "black"         ; change ref vector color
      res@vcMinDistanceF            = 0.0075          ; thin out vectors
      res@vcMapDirection            = False           ; 
      res@vcRefAnnoOn               = False           ; do not draw
      res@gsnScalarContour          = True            ; contours desired
  else
      plotPhase = False
  end if

  res@gsnLeftString = "Coh^2: Symmetric"
  res@gsnRightString  = "10%="+sprintf("%3.2f", STC@prob_coh2(2))+" "\
                      +"  5%="+sprintf("%3.2f", STC@prob_coh2(4))

  if (opt .and. isatt(opt,"pltZonalWaveNumber") ) then
      nWavePlt  = opt@pltZonalWaveNumber
  else
      nWavePlt  = 15                            ; default
  end if

;---------------------------------------------------------------
; dispersion: curves 
;---------------------------------------------------------------
  rlat           = 0.0
  Ahe            = (/50.,25.,12./)
  nWaveType      = 6
  nPlanetaryWave = 50
  nEquivDepth    = dimsizes(Ahe)
  Apzwn          = new((/nWaveType,nEquivDepth,nPlanetaryWave/),"double",1e20)
  Afreq          = Apzwn
  genDispersionCurves(nWaveType, nEquivDepth, nPlanetaryWave, rlat, Ahe, Afreq, Apzwn )

;---------------------------------------------------------------
; dispersion curve and text plot resources
;---------------------------------------------------------------
   dcres = True
   dcres@gsLineThicknessF  = 2.0
   dcres@gsLineDashPattern = 0

   txres = True
  ;txres@txJust        = "CenterLeft"
   txres@txPerimOn     = True
   txres@txFontHeightF = 0.013
   txres@txBackgroundFillColor = "Background"

;---------------------------------------------------------------
; plot symmetric data      
;---------------------------------------------------------------
  n = 8

  c2s = STC(n,:,{-nWavePlt:nWavePlt})                     
  c2s@_FillValue = 1e20
  c2s(0,:)       = c2s@_FillValue 
  c2s = where(c2s.lt.0.05, c2s@_FillValue, c2s) ; mask

  n = 12
  phs1 = STC(n,:,{-nWavePlt:nWavePlt})
  phs1@long_name  = "symmetric phase-1"
  phs1@_FillValue = c2s@_FillValue
  phs1(0,:)       = phs1@_FillValue
  phs1 = where(c2s.lt.0.05, phs1@_FillValue, phs1) ; mask

  n = 14
  phs2 = STC(n,:,{-nWavePlt:nWavePlt})
  phs2@long_name  = "symmetric phase-2"
  phs2@_FillValue = c2s@_FillValue
  phs2(0,:)       = phs2@_FillValue
  phs2 = where(c2s.lt.0.05, phs2@_FillValue, phs2) ; mask

  if (opt .and. isatt(opt,"pltProb") ) then
      np  = ind(c2s@prob_coh2 .eq. opt@pltProb)
      if (.not.ismissing(np)) then
          c2s  = where(c2s.lt.STC@prob_coh2(np), c2s@_FillValue, c2s)
          phs1 = where(ismissing(c2s), phs1@_FillValue, phs1)
          phs2 = where(ismissing(c2s), phs2@_FillValue, phs2)
      end if
  end if

  if (opt .and. isatt(opt,"coh2Cutoff") ) then
      c2s  = where(c2s.lt.opt@coh2Cutoff, c2s@_FillValue, c2s)
  end if

  if (opt .and. isatt(opt,"phaseCutoff") ) then
      phs1 = where(c2s.lt.opt@phaseCutoff, phs1@_FillValue, phs1)
      phs2 = where(c2s.lt.opt@phaseCutoff, phs2@_FillValue, phs2)
  end if
  if (opt .and. isatt(opt,"elimIsoVals") .and. .not.opt@elimIsoVals) then
      print("mjo_cross_plot: no values eliminated")
  else
      mjo_elimIsolatedValues (c2s, phs1, phs2, 1)
      mjo_elimIsolatedValues (c2s, phs1, phs2, 2)
  end if
 
 ;res@gsnCenterString   = "var="+n+"  "+STC@varName(n)
  if (plotPhase) then
      scl_one = sqrt(1./(phs1^2 + phs2^2))
      phs1    = scl_one*phs1
      phs2    = scl_one*phs2
      plot(0) = gsn_csm_vector_scalar(wks, phs1, phs2, c2s, res)       
  else
      plot(0) = gsn_csm_contour(wks, c2s, res)       
  end if

  dums    = new (25, "graphic")    ; for _add_ *s*ymmetric graphical objects
  plot(0) = addHorVertLinesCross(wks, plot(0), nWavePlt, dums)

  dumdcs     = new ( 25, "graphic") ; dispersion curves symmetric (dcs)
  dumdcs( 0) = gsn_add_polyline(wks,plot(0),Apzwn(3,0,:),Afreq(3,0,:),dcres)
  dumdcs( 1) = gsn_add_polyline(wks,plot(0),Apzwn(3,1,:),Afreq(3,1,:),dcres)
  dumdcs( 2) = gsn_add_polyline(wks,plot(0),Apzwn(3,2,:),Afreq(3,2,:),dcres)
  dumdcs( 3) = gsn_add_polyline(wks,plot(0),Apzwn(4,0,:),Afreq(4,0,:),dcres)
  dumdcs( 4) = gsn_add_polyline(wks,plot(0),Apzwn(4,1,:),Afreq(4,1,:),dcres)
  dumdcs( 5) = gsn_add_polyline(wks,plot(0),Apzwn(4,2,:),Afreq(4,2,:),dcres)
  dumdcs( 6) = gsn_add_polyline(wks,plot(0),Apzwn(5,0,:),Afreq(5,0,:),dcres)
  dumdcs( 7) = gsn_add_polyline(wks,plot(0),Apzwn(5,1,:),Afreq(5,1,:),dcres)
  dumdcs( 8) = gsn_add_polyline(wks,plot(0),Apzwn(5,2,:),Afreq(5,2,:),dcres)

  dumdcs( 9) = gsn_add_text(wks,plot(0),"Kelvin",11.5,.40,txres)
  dumdcs(10) = gsn_add_text(wks,plot(0),"n=1 ER",-10.7,.07,txres)
  dumdcs(11) = gsn_add_text(wks,plot(0),"n=1 IG",-3.0,.45,txres)
  dumdcs(12) = gsn_add_text(wks,plot(0),"h=50",-14.0,.78,txres)
  dumdcs(13) = gsn_add_text(wks,plot(0),"h=25",-14.0,.60,txres)
  dumdcs(14) = gsn_add_text(wks,plot(0),"h=12",-14.0,.46,txres)

;---------------------------------------------------------------
; plot asymmetric data      
;---------------------------------------------------------------

  n = 9
  res@gsnLeftString   = "Coh^2: Asymmetric"
  res@gsnRightString  = "10%="+sprintf("%3.2f", STC@prob_coh2(2))+" "\
                      +"  5%="+sprintf("%3.2f", STC@prob_coh2(4))
  c2a = STC(n,:,{-nWavePlt:nWavePlt})
  c2a@_FillValue = 1e20
  c2a(0,:) = c2a@_FillValue 
  c2a = where(c2a.lt.0.05, c2a@_FillValue, c2a) ; mask

  n = 13
  pha1 = STC(n,:,{-nWavePlt:nWavePlt})
  pha1@long_name  = "asymmetric phase-1"
  pha1@_FillValue = c2a@_FillValue
  pha1(0,:)       = pha1@_FillValue
 ;pha1 = where(c2a.lt.0.05, pha1@_FillValue, pha1) ; mask

  n = 15
  pha2 = STC(n,:,{-nWavePlt:nWavePlt})
  pha2@long_name  = "asymmetric phase-2"
  pha2@_FillValue = c2a@_FillValue
  pha2(0,:)       = pha2@_FillValue
  pha2 = where(c2a.lt.0.05, pha2@_FillValue, pha2) ; mask

  if (opt .and. isatt(opt,"pltProb") ) then
      np  = ind(c2a@prob_coh2 .eq. opt@pltProb)
      if (.not.ismissing(np)) then
          c2a  = where(c2a.lt.STC@prob_coh2(np), c2a@_FillValue, c2s)
          pha1 = where(ismissing(c2a), pha1@_FillValue, pha1)
          pha2 = where(ismissing(c2a), pha2@_FillValue, pha2)
      end if
  end if

  if (opt .and. isatt(opt,"coh2Cutoff") ) then
      c2a  = where(c2a.lt.opt@coh2Cutoff, c2s@_FillValue, c2s)
  end if
  if (opt .and. isatt(opt,"phaseCutoff") ) then
      pha1 = where(c2a.lt.opt@phaseCutoff, pha1@_FillValue, pha1)
      pha2 = where(c2a.lt.opt@phaseCutoff, pha2@_FillValue, pha2)
  end if

  if (opt .and. isatt(opt,"elimIsoVals") .and. .not.opt@elimIsoVals) then
      mjo_elimIsolatedValues (c2a, pha1, pha2, 2)
      mjo_elimIsolatedValues (c2a, pha1, pha2, 1)
  end if

  if (plotPhase) then
      scl_one = sqrt(1./(pha1^2 + pha2^2))
      pha1    = scl_one*pha1
      pha2    = scl_one*pha2
      plot(1) = gsn_csm_vector_scalar(wks, pha1, pha2, c2a, res)       
  else
      plot(1) = gsn_csm_contour(wks, c2a, res)       
  end if

  duma    = new (25, "graphic")    ; for _add_ *a*symmetric graphical objects
  plot(1) = addHorVertLinesCross(wks, plot(1), nWavePlt, duma)

  dumdca     = new ( 25, "graphic") ; dispersion curves asymmetric (dcs)
  dumdca( 0) = gsn_add_polyline(wks,plot(1),Apzwn(0,0,:),Afreq(0,0,:),dcres)
  dumdca( 1) = gsn_add_polyline(wks,plot(1),Apzwn(0,1,:),Afreq(0,1,:),dcres)
  dumdca( 2) = gsn_add_polyline(wks,plot(1),Apzwn(0,2,:),Afreq(0,2,:),dcres)
  dumdca( 3) = gsn_add_polyline(wks,plot(1),Apzwn(1,0,:),Afreq(1,0,:),dcres)
  dumdca( 4) = gsn_add_polyline(wks,plot(1),Apzwn(1,1,:),Afreq(1,1,:),dcres)
  dumdca( 5) = gsn_add_polyline(wks,plot(1),Apzwn(1,2,:),Afreq(1,2,:),dcres)
  dumdca( 6) = gsn_add_polyline(wks,plot(1),Apzwn(2,0,:),Afreq(2,0,:),dcres)
  dumdca( 7) = gsn_add_polyline(wks,plot(1),Apzwn(2,1,:),Afreq(2,1,:),dcres)
  dumdca( 8) = gsn_add_polyline(wks,plot(1),Apzwn(2,2,:),Afreq(2,2,:),dcres)

  dumdca(10) = gsn_add_text(wks,plot(1),"MRG",-10.0,.15,txres)
  dumdca(11) = gsn_add_text(wks,plot(1),"n=2 IG",-3.0,.58,txres)
  dumdca(12) = gsn_add_text(wks,plot(1),"n=0 EIG",6.5,.40,txres)
  dumdca(13) = gsn_add_text(wks,plot(1),"h=50",-10.0,.78,txres)
  dumdca(14) = gsn_add_text(wks,plot(1),"h=25",-10.0,.63,txres)
  dumdca(15) = gsn_add_text(wks,plot(1),"h=12",-10.0,.51,txres)

  resP = True
  resP@gsnMaximize         = True
  resP@gsnPanelLabelBar    = True
 ;resP@lbLabelAutoStride   = True
  resP@lbLabelStride       = 2                   ; every other one
 ;resP@lbLabelFontHeightF  = 0.007               ; make labels smaller
  resP@cnLabelBarEndLabelsOn= True
  resP@cnLabelBarEndStyle   = "ExcludeOuterBoxes"

  resP@cnLevelSelectionMode = res@cnLevelSelectionMode
  resP@cnMinLevelValF       = res@cnMinLevelValF 
  resP@cnMaxLevelValF       = res@cnMaxLevelValF    ; correlation^2 = 0.8  
  resP@cnLevelSpacingF      = res@cnLevelSpacingF

  if (opt .and. isatt(opt,"txString") ) then
      resP@txString        = opt@txString
  end if
  if (opt .and. isatt(opt,"gsnPaperOrientation") ) then
      resP@gsnPaperOrientation = opt@gsnPaperOrientation
  end if

  if (opt .and. isatt(opt,"lbOrientation") ) then
      resP@lbOrientation =   "vertical"
      gsn_panel(wks,plot,(/2,1/),resP)
  else
      gsn_panel(wks,plot,(/1,2/),resP)
  end if

  if (pltTypeLocal.eq."png") then
      if (isatt(opt,"pltConvert")) then 
          pltConvert = opt@pltConvert    ; convert options
      else
          pltConvert = "-trim +repage -border 8 -bordercolor white"  ; trim, nice border
      end if
      system("convert "+pltConvert+" "+pltPath+"."+pltTypeLocal+" "+pltPath+"."+pltTypeLocal)
  end if
end
; ---------------------
undef ("mjo_phase_background")
function mjo_phase_background (wks:graphic , opt[1]:logical   )
begin
  if (opt .and. isatt(opt,"axisExtent") .and. opt@axisExtent.gt.0) then 
      axisExtent        = opt@axisExtent
  else
      axisExtent        =  4
  end if
  nPhase                =  8

  res                   = True   
  res@gsnDraw           = False
  res@gsnFrame          = False
 ;res@gsnPaperOrientation =  "portrait"

  res@vpXF              = 0.1            ; default=0.2
  res@vpYF              = 0.8            ; default=0.8

  res@trYMinF           = -axisExtent 
  res@trYMaxF           =  axisExtent 
  res@trXMinF           = -axisExtent 
  res@trXMaxF           =  axisExtent + 0.05 

  vpExtent              = 0.45           
  res@vpWidthF          = vpExtent       ; default=0.6
  res@vpHeightF         = vpExtent       ; default=0.6 

  res@tmXBFormat        = "f"            ; integer, no decimal
  res@tmYLFormat        = "f"            ; 3 rather than 3.0

  res@tmXBLabelDeltaF   = -0.75          ; move label closer to tm
  res@tmYLLabelDeltaF   = -0.75

  labelFontHeightF      = 0.0167         ; default=0.0167
  res@tiXAxisFontHeightF= labelFontHeightF 
  res@tiYAxisFontHeightF= labelFontHeightF
  res@tiDeltaF          =  1.25          ; default=1.5

  res@xyLineThicknessF  = 1

  rad   = 4.*atan(1.0)/180.
  if (opt .and. isatt(opt,"radius") .and. opt@radius.gt.0) then 
      radius = opt@radius
  else
      radius= 1.0                        ; unit length (default)
  end if
          
  nCirc = 361
  xCirc = radius*cos(fspan(0, 360, nCirc)*rad)
  yCirc = radius*sin(fspan(0, 360, nCirc)*rad)

  xCirc@long_name = "Phase 2 (Indian Ocean) Phase 3"
  yCirc@long_name = "Phase 1 (Western Hem, Africa) Phase 8"

 ;res@gsnCenterStringOrthogonalPosF = 
  res@gsnCenterStringFontHeightF    = labelFontHeightF
  res@gsnCenterString = "Phase 7 (Western Pacific) Phase 6"

  if (opt .and. isatt(opt,"tiMainString")) then 
      res@tiMainString = opt@tiMainString
  end if

  plt   = gsn_csm_xy (wks, xCirc, yCirc, res)  ; base plot

  txres               = True             
  txres@txFontHeightF = labelFontHeightF
  txres@txAngleF      = -90
  txid = gsn_create_text(wks, "Phase 5 (Maritime) Phase 4", txres)

  amres                  = True 
  amres@amParallelPosF   =  0.575  
  amres@amOrthogonalPosF =  0.0   
  amres@amJust           = "CenterCenter"
  ann3  = gsn_add_annotation(plt, txid, amres)

;************************************************
; Add the phase separator lines.
; Each line must be associated with a unique dummy variable. 
; Draw each line separately. Each line must contain two 
;      points (begin/end).
;************************************************
  plres                  = True      ; polyline mods desired
 ;plres@gsLineColor      = "red"     ; color of lines
  plres@gsLineThicknessF = 1.0       ; thickness of lines
  if (opt .and. isatt(opt,"gsLineDashPattern")) then 
      plres@gsLineDashPattern = opt@gsLineDashPattern
  else
      plres@gsLineDashPattern = 1
  end if

  c45  = radius*cos(45*rad)
  E    = axisExtent               ; convenience
  R    = radius

  phaLine      = new ( (/nPhase,4/), "float", "No_FillValue")
  phaLine(0,:) = (/   R,   E,   0,   0/) ; (xBegin,xEnd,yBegin,yEnd) 
  phaLine(1,:) = (/ c45,   E, c45,   E/) 
  phaLine(2,:) = (/   0,   0,   R,   E/) 
  phaLine(3,:) = (/-c45,  -E, c45,   E/) 
  phaLine(4,:) = (/  -R,  -E,   0,   0/) 
  phaLine(5,:) = (/-c45,  -E,-c45,  -E/) 
  phaLine(6,:) = (/   0,   0,  -R,  -E/) 
  phaLine(7,:) = (/ c45,   E,-c45,  -E/) 

  do i =0,nPhase-1
     plt@$unique_string("dum")$ = gsn_add_polyline(wks,plt \
                                 ,(/phaLine(i,0),phaLine(i,1)/) \
                                 ,(/phaLine(i,2),phaLine(i,3)/) ,plres)      
  end do

  return(plt)
end
