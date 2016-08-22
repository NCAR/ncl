;---
; It is important to note that the 'center' (aka: location, shift) parameter is 
; not the mean. However, it does represent the ‘center’ of the distribution.
; The 'scale' (aka, sigma, beta) parameter is not the standard deviation. However,
; it does govern the spread (size) of the deviations about the 'center'.
;---
undef("extval_gumbel")
function extval_gumbel(x[*]:numeric, scale[*]:numeric, center[*]:numeric, opt[1]:integer)
;
; Extreme Value Type I distribution ==> Gumbel Distribution
; The shape of the Gumbel model does not depend on the distribution parameters
; It is unbounded on the x-axis
;
; The Gumbel distribution is used to model the distribution of the maximum (or minimum) 
; of a number of samples of various distributions.
; The shape of the Gumbel model does not depend on the distribution parameters
; It is bounded on the left and has a heavy upper tail.
;
; See: http://www.itl.nist.gov/div898/handbook/eda/section3/eda366g.htm
;
; minmax <=0 results in MINIMA (smallest extreme value) 
; minmax > 0 results in MAXIMA (largest extreme value) 
;
; GEV Type I with shape=0
; Domain of attraction for many common distributions ( e.g., normal , exponential,
; gamma), not frequently found to fit ‘real world data ;
;
local nctr, nscl, nx, n, ix, z, work, pdf, cdf
begin
   nctr = dimsizes(center)
   nscl = dimsizes(scale)
   if (nctr.ne.nscl) then
       print("extval_gumbel: center and scale arguments must be the same size")
       print("extval_gumbel: nctr="+nctr+"  nscl="+nscl)
       exit
   end if
   nx   = dimsizes(x)

   if (typeof(x).eq."double" .or. \
       typeof(scale).eq."double" .or. typeof(center).eq."double") then
       pdf  = new( (/nscl,nx/), "double", -9999d0  )
   else
       pdf  = new( (/nscl,nx/), "float", -9999.0 )
   end if
   cdf  = pdf

   ix  = dim_pqsort_n(x, 1, 0) ; for plot reasons it is better 'x' is ascending 

   do n=0,nscl-1   
      if (scale(n).le.0) then
          print("extval_gumbel: shape <=0: scale="+scale(n))
          continue
      end if

      z  = (x(ix)-center(n))/scale(n)       ; minimum
      if (opt.ge.0) then                    ; maximum
          z  = -z
      end if

      work     =  exp(z)
      pdf(n,:) = (totype(1,typeof(x))/scale(n))*(work*exp(-work))  

      cdf(n,:) =  exp(-work)                ; maximum  
      if (opt.lt.0) then                 
          cdf(n,:) =  1-cdf(n,:)            ; minimum
      end if

      if (opt.ge.0) then                 ; maximum
          pdf@long_name = "Gumbel: Extreme Value Type I: Maxima; PDF"
          cdf@long_name = "Gumbel: Extreme Value Type I: Maxima; CDF"
      else
          pdf@long_name = "Gumbel: Extreme Value Type I: Minima; PDF"
          cdf@long_name = "Gumbel: Extreme Value Type I: Minima; CDF"
      end if

   end do

   pdf@center  = center
   pdf@scale   = scale

   cdf@center  = center
   cdf@scale   = scale

   if (nscl.eq.1) then
       return([/pdf(0,:), cdf(0,:)/])
   else
       return([/pdf, cdf/])
   end if
end
;---
undef("extval_frechet")
function extval_frechet(x[*]:numeric, shape[*]:numeric, scale[*]:numeric \
                                    ,center[*]:numeric, opt[1]:numeric)
;
; Extreme Value Type II distribution ==> Frechet Distribution
; The Fréchet model relates to MAXIMA (largest extreme value)
;
; http://www.mathwave.com/help/easyfit/html/analyses/distributions/frechet.html
;
; GEV type II with ξ > 0  ( Frechet, heavy tailed) 
; Fits found for precipitation, stream flow, economic damage, ..
;
; The Frechet distribution requires lower bound.
;
local nshp, nscl, nctr, nx, pdf, cdf, y, ix
begin
   nshp = dimsizes(shape)
   nscl = dimsizes(scale)
   nctr = dimsizes(center)
   if (nshp.ne.nscl .or. nshp.ne.nctr) then
       print("extval_frechet: shape, scale and center arguments must be the same size")
       print("extval_frechet: nshp="+nshp+"  nscl="+nctr+"  nscl="+nctr)
       exit
   end if
   
   nx   = dimsizes(x)
   if (typeof(x).eq."double" .or. \
       typeof(shape).eq."double" .or. typeof(scale).eq."double" .or. typeof(center).eq."double") then
       pdf  = new( (/nscl,nx/), "double", -9999d0  )
   else
       pdf  = new( (/nscl,nx/), "float", -9999.0 )
   end if
   cdf  = pdf

   if (any(x.le.0))  then
       print("extval_frechet: one or more x <= 0")
       exit
   end if

   ix = dim_pqsort_n(x, 1, 0)        ; for plot reasons it is better 'x' is asebding 

   do n=0,nscl-1   
       if (scale(n).le.0 .or. shape(n).le.0)  then
           print("extval_frechet: unexpected value(s): scale="+scale(n)+" shape="+shape(n))
           continue
       end if

       y  = scale(n)/(x(ix)-center(n))    

       cdf(n,:) = exp(-(y^shape(n)))
       pdf(n,:) = (shape(n)/scale(n))*((y^(shape(n)+1)) * cdf(n,:))
       pdf(n,:) = where(x.le.center(n), pdf@_FillValue, pdf(n,:))  ; undefined
   end do
   
   pdf@long_name = "PDF Frechet: Extreme Value Type II: Maxima"
   pdf@shape     = shape
   pdf@scale     = scale
   pdf@center    = center

   cdf@long_name = "CDF: Frechet: Extreme Value Type II: Maxima"
   cdf@shape     = shape
   cdf@scale     = scale
   cdf@center    = center

   if (nscl.eq.1) then
       return([/pdf(0,:), cdf(n,:)/])
   else
       return([/pdf, cdf/])
   end if
end
;---
undef("extval_weibull")
function extval_weibull(x[*]:numeric, shape[*]:numeric, scale[*]:numeric, center[*]:numeric, opt[1]:integer)
;
; Extreme Value Type III distribution ==> Weibull Distribution
; The distribution is defined for x>center
; The Weibull model relates to minima (smallest extreme value)
; 
; http://www.mathwave.com/help/easyfit/html/analyses/distributions/weibull.html
;
; GEV type III with ξ < 0 ( Weibull, bounded) Fits
; Fits found for temperature, wind speed, pollutants, sea level
;
local y, pdf, cdf, nshp, nscl, nctr, nx, n, ix
begin
  ;if (any(x.le.0))  then
  ;    print("extval_weibull: one or more x <= 0")
  ;    exit
  ;end if

   nshp = dimsizes(shape)
   nscl = dimsizes(scale)
   nctr = dimsizes(center)
   if (nshp.ne.nscl .or. nshp.ne.nctr) then
       print("extval_weibull: shape, scale and center arguments must be the same size")
       print("extval_weibull: nshp="+nshp+"  nscl="+nscl+"  nctr="+nctr)
       exit
   end if
   nx   = dimsizes(x)

   if (typeof(x).eq."double" .or. \
       typeof(shape).eq."double" .or. typeof(scale).eq."double" .or. typeof(center).eq."double") then
       pdf  = new( (/nscl,nx/), "double", -9999d0  )
   else
       pdf  = new( (/nscl,nx/), "float", -9999.0 )
   end if
   cdf  = pdf

   do n=0,nscl-1
      if (scale(n).le.0 .or. shape(n).le.0) then
          print("extval_weibull: shape <=0: shape="+shape(n)+"; scale="+scale(n) )
          continue
      end if

      ix       = dim_pqsort_n(x, 1, 0)  ; for plot reasons it is better 'x' is ascending
      y        = (x(ix)-center(n))/scale(n)

      pdf(n,:) = (shape(n)/scale(n))*(y^(shape(n)-1)) * exp(-(y^shape(n)))
      pdf(n,:) = where(x.le.center(n), 0, pdf(n,:))
      cdf(n,:) = 1-exp(-(y^shape(n)))
   end do

   pdf@long_name = "Weibull: Extreme Value Type III: PDF"
   pdf@shape     = shape
   pdf@scale     = scale
   pdf@center    = center

   cdf@long_name = "Weibull: Extreme Value Type III: CDF"
   cdf@shape     = shape
   cdf@scale     = scale
   cdf@center    = center

   if (nscl.eq.1) then
       return([/pdf(0,:), cdf(0,:)/])
   else
       return([/pdf, cdf/])
   end if
end
;================
undef("extval_pareto")
function extval_pareto(x[*]:numeric, shape[*]:numeric, scale[*]:numeric, location[*]:numeric \
                                   , ptype[1]:integer, opt[1]:integer)

; Pareto Distributions:
;   http://www.mathwave.com/help/easyfit/html/analyses/distributions/gen_pareto.html
;   http://www.mathwave.com/help/easyfit/html/analyses/distributions/pareto.html
;   http://www.mathwave.com/help/easyfit/html/analyses/distributions/pareto2.html
;
;   https://en.wikipedia.org/wiki/Pareto_distribution
;
local nscl, nshp, nloc, nx, pdf, cdf, n, con, conk, cons, shpy, pdf, cdf, y
begin
   nscl = dimsizes(scale)
   nshp = dimsizes(shape)
   nloc = dimsizes(location)
   nx   = dimsizes(x)

   pdf = new( (/nshp,nx/), typeof(x), getVarFillValue(x))
   cdf = pdf

   if (ptype.lt.0 .or. ptype.gt.2) then
       print("extval_pareto: non-valid ptype="+ptype)
       exit
   end if

   if (ptype.eq.0) then                       ; Generalized Pareto
       do n=0,nscl-1
          if (scale(n).le.0) then
              print("extval_pareto: Generalized: scale <=0: scale="+scale(n) )
              continue
          end if

          conk = 1.0/shape(n)                 ; convenience and clarity
          cons = 1.0/scale(n)
          y    = (x-location(n))/scale(n)    
          shpy = shape(n)*y

          if (shape(n).eq.0) then
              pdf(n,:)  = cons*exp(-y)
              cdf(n,:)  = 1-pdf(n,:)              
          else
              pdf(n,:)  = cons*(1+shpy)^(-1.0-conk)
              cdf(n,:)  = 1-(1+shpy)^(-conk)
          end if
        end do

        pdf@long_name = "Pareto: Generalized: PDF"
        pdf@shape     = shape
        pdf@scale     = scale
        pdf@location  = location

        cdf@long_name = "Pareto: Generalized: CDF"
        cdf@shape     = shape
        cdf@scale     = scale
        cdf@location  = location
   end if                                      ; ptype=0  

   if (ptype.eq.1) then                        ; Perato Type I
       do n=0,nscl-1
          if (shape(n).le.0 .or. scale(n).le.0) then
              print("extval_pareto: Type I: shape or scale <=0: shape="+shape(n)+"; scale="+scale(n) )
              continue
          end if

          xx       = where(x.le.0, pdf@_FillValue, x)      ; safety;  XX > 0
          con      = shape(n)*(scale(n)^shape(n))          ; numerator
          pdf(n,:) = where(xx.ge.scale(n), con/(xx^(shape(n)+1)), pdf@_FillValue) 
          cdf(n,:) = where(xx.lt.scale(n), 1, (1-(scale(n)/xx)^shape(n)))
        end do

        pdf@long_name = "Pareto: Type I: PDF"
        pdf@shape     = shape
        pdf@scale     = scale

        cdf@long_name = "Pareto: Type I: CDF"
        cdf@shape     = shape
        cdf@scale     = scale
   end if                                      ; ptype=1

   if (ptype.eq.2) then     ; Perato Type II; location ignored
       do n=0,nscl-1
          if (shape(n).le.0 .or. scale(n).le.0) then
              print("extval_pareto: Type II: shape or scale <=0: shape="+shape(n)+"; scale="+scale(n) )
              continue
          end if

          con       = shape(n) * (scale(n)^shape(n))    ; numerator
          pdf(n,:)  = where(x.ge.0, con/((x+scale(n))^(shape(n)+1)), pdf@_FillValue) 
          cdf(n,:)  = 1 - (scale(n)/(x+scale(n)))^shape(n)
      end do

      pdf@long_name = "Pareto: Type II: PDF"
      pdf@shape     = shape
      pdf@scale     = scale

      cdf@long_name = "Pareto: Type II: CDF"
      cdf@shape     = shape
      cdf@scale     = scale
   end if                                       ; ptype=2

   if (nscl.eq.1) then
       return([/pdf(0,:), cdf(0,:)/])
   else
       return([/pdf, cdf/])
   end if
end
;================
undef("extval_gev")
function extval_gev(x[*]:numeric, shape[*]:numeric, scale[*]:numeric, center[*]:numeric, opt[1]:integer)
;
; The generalized extreme value (GEV) distribution is a family of continuous probability 
; distributions developed within extreme value theory to combine the Gumbel, Fréchet 
; and Weibull families also known as type I, II and III extreme value distributions.
;
; The shape parameter ξ governs the distribution type :
; ... Type I   with ξ = 0 ( Gumbel,light tailed)
; ... Type II  with ξ > 0 ( Frechet, heavy tailed)
; ... Type III with ξ < 0 ( Weibull, bounded)
;
; The GEV distribution is often used as an approximation to model the maxima 
; of long (finite) sequences of random variables.
;;
local nshp, nscl, nctr, nx, pdf, cdf, y
begin
   nshp = dimsizes(shape)
   nscl = dimsizes(scale)
   nctr = dimsizes(center)    ; center
   if (nshp.ne.nscl .or. nshp.ne.nctr) then
       print("extval_gev: shape, scale  and shift arguments must be the same size")
       print("extval_gev: nshp="+nshp+"  nscl="+nscl+"  nctr="+nctr)
       exit
   end if

   nx   = dimsizes(x)
   if (typeof(x).eq."double" .or. \
       typeof(shape).eq."double" .or. typeof(scale).eq."double" .or. typeof(center).eq."double") then
       pdf  = new( (/nscl,nx/), "double", -9999d0  )
   else
       pdf  = new( (/nscl,nx/), "float", -9999.0 )
   end if
   cdf  = pdf

 ;;if (any(x.le.0))  then
 ;;    print("extval_gev: one or more x <= 0")
 ;;    exit
 ;;end if

   do n=0,nscl-1   
       if (scale(n).le.0)  then
           print("extval_gev: unexpected value(s): scale="+scale(n)+" shape="+shape(n))
           continue
       end if

       y   = (x-center(n))/scale(n)    ; temporary for clarity (debug)
       a   = 1+shape(n)*y
       q   = 1.0/scale(n)
       
       if (shape(n).ne.0) then
              p1       = -1-(1.0/shape(n)) ; work; clarity
              p2       = -1.0/shape(n)
              pdf(n,:) = q*exp(-(a^p2))*(a^p1) 
              cdf(n,:) = exp(-(a^p2))
       else
              pdf(n,:) = q*exp(-y-exp(-y))
              cdf(n,:) = exp(-exp(-y))
              pdf(n,:) = where(pdf(n,:).lt.0, pdf@_FillValue, pdf(n,:))
       end if
   end do
   pdf = where(pdf.lt.0, pdf@_FillValue, pdf)
   cdf = where(ismissing(pdf), cdf@_FillValue, cdf)

   pdf@long_name = "PDF: Generalized Extreme Value"
   pdf@shape     = shape
   pdf@scale     = scale
   pdf@center    = center

   cdf@long_name = "CDF: Generalized Extreme Value"
   cdf@shape     = shape
   cdf@scale     = scale
   cdf@center    = center

   if (nscl.eq.1) then
       return([/pdf(0,:), cdf(0,:)/])
   else
       return([/pdf, cdf/])
   end if
end
;----
undef("extval_gamma")
function extval_gamma(X[*]:numeric, shape[*]:numeric, scale[*]:numeric, center[*]:numeric, opt[1]:integer)
;
; Gamma Distribution
; The distribution is defined for x>center (location) ; scale>0 ; shape>0
; http://www.mathwave.com/help/easyfit/html/analyses/distributions/gamma.html
;
local ix, x, nx, pdf, cdf, nshp, nscl, nctr, n, ca, cb, cc, cd, cf, cg \
    , xc, numType, scl, shp, ctr
begin
   nshp = dimsizes(shape)
   nscl = dimsizes(scale)
   nctr = dimsizes(center)
   if (nshp.ne.nscl .or. nshp.ne.nctr) then
       print("extval_gamma: shape, scale and center arguments must be the same size")
       print("extval_gamma: nshp="+nshp+"  nscl="+nscl+"  nctr="+nctr)
       exit
   end if

;--- Force to highest numeric type

   nx   = dimsizes(X)
   if (typeof(X).eq."double" .or. \
       typeof(shape).eq."double" .or. typeof(scale).eq."double" .or. typeof(center).eq."double") then
       numType = "double"
       pdf     = new( (/nscl,nx/), numType, -9999d0  )
   else
       numType = "float"
       pdf     = new( (/nscl,nx/), numType, -9999.0 )
   end if
   cdf  = pdf

;--- For plot reasons it is better 'x' is ascending

   ix   = dim_pqsort_n(X, 1, 0)          ; indices of ascending values
   x    = totype(X(ix), numType)         ; 'x' is local and is ascending (plotting)
   x@_FillValue = pdf@_FillValue

   do n=0,nscl-1
      if (scale(n).le.0) then
          print("extval_gamma: scale<=0 : scale="+scale(n))
          continue
      end if
      if (shape(n).le.0) then
          print("extval_gamma: shape<=0 : shape="+shape(n))
          continue
      end if

      scl  = totype(scale(n) , numType) 
      shp  = totype(shape(n) , numType) 
      ctr  = totype(center(n), numType) 

      xc   = where(x .gt.ctr, (x-ctr), pdf@_FillValue)
      xc   = where(xc.gt.0  , xc,  xc@_FillValue)      ; avoid xc^(shp-1)  
                                ; clarity
      ca   = xc^(shp-1)         ; ca[*]
      cb   = exp(-xc/scl)       ; cb[*]
      cc   = gamma(shp)         ; cc[1]
      cd   = scl^shp            ; cd[1]
      cab  = ca*cb              ; cab[*]

      pdf(n,:) = cab/(cc*cd)

      cf   = conform(xc, shp, -1)                  ; [*] ... make scalar a [*]
      cg   = gammainc((xc/scl), cf)                ; [*]
   
    ;;cgc  = cg/cc                                 ; <== This is correct but ...
      cgc  = cg                                    ; Notn sure why this yields the CDF

      cdf(n,:) = where(.not.ismissing(pdf(n,:)), cgc, pdf@_FillValue)

    ;;print("=================")
    ;;print("n="+n+"  shape="+shape(n)+"  scale="+scale(n)+"  center="+center(n)+" cc="+cc+"  cd="+cd)
    ;;print("ca="+ca+"  cb="+cb+"  x="+x+"  pdf="+pdf(n,:)+"  cdf="+cdf(n,:))
   end do

   pdf@long_name = "Gamma: PDF"
   pdf@shape     = shape
   pdf@scale     = scale
   pdf@center    = center

   cdf@long_name = "Gamma:  CDF"
   cdf@shape     = shape
   cdf@scale     = scale
   cdf@center    = center

   if (nscl.eq.1) then
       return([/pdf(0,:), cdf(0,:)/])
   else
       return([/pdf, cdf/])
   end if
end
;----
undef("extval_recurrence_table_1d")
function extval_recurrence_table_1d(time[*]:numeric, x[*]:numeric, dims[*]:integer, opt[1]:logical)
;
; MAYBE this should be modified to return only:
; (5) exceedence probability rank
; (5) exceedence probability
; (6) recurrence interval  
;
local N, N1, ix, n, nstat, table, TABLE
begin
   N     = dimsizes(time)
   ix    = dim_pqsort_n(x, 1, dims)  

   nstat = 7
   if (typeof(x).eq."double" ) then
       table = new( (/N,nstat/), "double", 9999d0)
   else
       table = new( (/N,nstat/), "float", 9999.0)
   end if

   table(:,0) = totype(time, typeof(x))  
   table(:,1) = x 

   do n=0,N-1
      table(n,2) = ind(ix.eq.n)+1      ; Rank
   end do

   N1 = N+1.0
   table(:,3) =(table(:,2)/N1)         ; Cumulative Probability
   table(:,4) = N1 - table(:,2)        ; Exceedance Probability Rank (1 is highest)
   table(:,5) =(table(:,4)/N1)         ; Exceedance Probability
   table(:,6) = N1/table(:,4)          ; Exceedance Probability

   if (opt .and. isatt(opt,"rank_order") .and. opt@rank_order)
       TABLE = table
       TABLE(:,0) = table(ix,0) 
       TABLE(:,1) = table(ix,1) 
       TABLE(:,2) = table(ix,2) 
       TABLE(:,3) = table(ix,3) 
       TABLE(:,4) = table(ix,4) 
       TABLE(:,5) = table(ix,5) 
       TABLE(:,6) = table(ix,6) 
       return(TABLE)
   else
      return(table)
   end if
end
;----
undef("extval_recurrence_table_2d")
function extval_recurrence_table_2d(time[*]:numeric, x[*][*]:numeric, dims[*]:integer, opt[1]:logical)
; Driver for above

local dimx, ntim, npts, N, nstat, npt,table
begin
   dimx = dimsizes(x)          ; (ntim,npts)
   ntim = dimx(0)
   npts = dimx(1)
   N    = ntim    ; consistency with 1d version

   nstat = 7
   if (typeof(x).eq."float" .or. typeof(x).eq."double" ) then
       table = new( (/npts,ntim,nstat/), typeof(x), getVarFillValue(x))
   else
       table = new( (/npts,ntim,nstat/), "float", 1e10)
   end if

; BRUTE force loop

   do npt=0,npts-1
      table(npt,:,:) = (/ extval_recurrence_table_1d(time, x(:,npt), 0, False) /)
   end do

   copy_VarCoords(x(0,:), table(:,0,0))   ; space:  (npts,...)
   copy_VarCoords(x(:,0), table(0,:,0))   ; time :  ( 0 , : , 0)
   table!2 = "stat"

   table@long_name  = "Recurrence Table"
   table@info       = "Probability of Exceedance for Ranked data"
   table@NCL        = "extval_recurrence_table_2d"
   table@method     = "Weibull Version: Rank/(N+1)"

  return (table)
end
;----
undef("extval_recurrence_table_3d")
function extval_recurrence_table_3d(time[*]:numeric, x[*][*][*]:numeric, dims[*]:integer, opt[1]:logical)
; Driver for above

local dimx, ntim, nlat, mlon, N, nstat, nl, ml, table
begin
   dimx = dimsizes(x)
   ntim = dimx(0)
   nlat = dimx(1)
   mlon = dimx(2)
   N    = ntim    ; consistency with 1d version

   nstat = 7
   if (typeof(x).eq."float" .or. typeof(x).eq."double" ) then
       table = new( (/nlat,mlon,N,nstat/), typeof(x), getVarFillValue(x))
   else
       table = new( (/nlat,mlon,N,nstat/), "float", 1e10)
   end if

; BRUTE force loop

   do nl=0,nlat-1
     do ml=0,mlon-1
        table(nl,ml,:,:) = (/ extval_recurrence_table_1d(time, x(:,nl,ml), 0, False) /)
     end do
   end do

   copy_VarCoords(x(0,:,:), table(:,:,0,0))   ; space:  (lat,lon,...)
   copy_VarCoords(x(:,0,0), table(0,0,:,0))   ; time :  ( 0 , 0 , :, 0)
   table!3 = "stat"

   table@long_name  = "Recurrence Table"
   table@info       = "Probability of Exceedance for Ranked data"
   table@NCL        = "extval_recurrence_table_3d"
   table@method     = "Weibull Version: Rank/(N+1)"

  return (table)
end
;----
undef("extval_recurrence_table")
function extval_recurrence_table(time[*]:numeric, x:numeric, dims[*]:integer, opt[1]:logical)
;
; This invokes the appropriate function.
;
local dimx, rankx
begin
   dimx  = dimsizes(x)
   rankx = dimsizes(dimx)

   if (rankx.gt.3) then
       return("extval_recurrence_table: FATAL: only up to rank 3 curently supported; rankx="+rankx)
       exit
   end if

   if (rankx.eq.1) then
       return( extval_recurrence_table_1d(time, x, dims, opt) )
   end if 
   if (rankx.eq.2) then
       return( extval_recurrence_table_2d(time, x, dims, opt) )
   end if 
   if (rankx.eq.3) then
       return( extval_recurrence_table_3d(time, x, dims, opt) )
   end if 
end 
;----
undef("extval_recurrence_gev")
; DO NOT USE!  This function has **NOT** been tested. DO NOT USE!
function extval_recurrence_gev(time[*]:numeric, x[*]:numeric, dims[*]:integer \
                              ,shape[1]:numeric, scale[1]:numeric, location[1]:numeric\
                              ,opt[1]:logical)
; Use GEV parameters to estimate recurrence
; GEV => Generalized Extreme Value distribution
;
; Under the assumption of stationarity the return level is the same for all years, 
; giving rise to the notion of the return period .  The return period of a 
; particular event is the inverse of the probability that the event will be 
; exceeded in any given year , i .  e .  the 푚 - year return level is 
; associated with a return period of 푚 years
;
local N, N1, ix, n, rank, retval_gev
begin
   print("extval_recurrence_gev: DO NOT USE")
 ;;N     = dimsizes(x)
 ;;ix    = dim_pqsort_n(x, 1, 0)  
 ;;            
 ;;do n=0,N-1
 ;;   rank(n) = ind(ix.eq.n)+1      ; Rank
 ;;end do

 ;;retval_gev = location + (scale/shape)*((-log(1.0-rank))^(-shape)-1.0)
 ;;retval_gev@long_name = "Return Level"
   retval_gev = -999.9
   return(retval_gev)
end
;------
undef("extval_return_prob")
function extval_return_prob(Tr[*]:numeric, Nr[*]:numeric)
;
; Probability Pe that one or more floods occurring during any period 
; will exceed a given 'event' (eg, flood) threshold can be determined, 
; using the binomial distribution. 
;
; https://en.wikipedia.org/wiki/Return_period
; https://en.wikipedia.org/wiki/100-year_flood
; http://water.usgs.gov/edu/100yearflood.html

; The probability of at least one event that exceeds design limits during 
; the expected life of the structure is the complement of the probability 
; that no events occur which exceed design limits.
;
; What is the probability of having one or more occurrence 
;     of a 10-year event within the 10-year life of a projectt?
; Assume that you are considering buying a house just with
;      in the flood plain.  Flood plains are defined for return periods 
;      of 100 years.  What is the probability that one or more times 
;      you will be flooded within the 30-year life of a mortgage?
;
; Assumptions: values for 'Tr' are independent and stationary
; Tr  - return (event) period
; n   - period
; ------
; Let Tr  = 10  ... What is the probability one will occur for the next 8 (=n)
;     pre = 0.57
; ------
local kTr, kNr, Pre, Pref
begin
  kTr    = dimsizes(Tr)
  kNr    = dimsizes(Nr)

  if (kTr.eq.1 .or. kNr.eq.1) then    ;;if (isscalar(Tr) .and. isscaler(Nr)) then
      Pre = 1d0-(1d0-(1d0/Tr))^Nr
      if (kTr.eq.1) then
          Pre!0  = "Nr"
          Pre&Nr =  Nr
      else
          Pre!0  = "Tr"
          Pre&Tr =  Tr
      end if
  else 
      Pre = new( (/kTr,kNr/), "double")
      do k=0,kTr-1
         Pre(k,:) = 1d0-(1d0-(1d0/Tr(k)))^Nr
      end do
      Pre!0  = "Tr"
      Pre!1  = "Nr"
      Pre&Nr =  Nr
      Pre&Tr =  Tr
  end if

  Pre@long_name = "probability of return event"  ; exceedance
  Pre@info      = "design risk"
  if (typeof(Tr).eq."double" .or. typeof(Nr).eq."double") then
      return(Pre)
  else
      Pref = tofloat(Pre)
      copy_VarMeta(Pre, Pref)
      return( Pref )
  end if
end
;------
undef("extval_return_period")
function extval_return_period(Tr[*]:numeric, Pr[*]:numeric)
;
; Assumptions: values for 'Tr' are independent and stationary
; Tr  - return (event) period
; Pr  - probability (0-1)
; ------
; Pr = 0.95                 ; user specified 
; Ta = 100                  ; years (days ... whatever)
; pp = 1.0/Ta               ; clarity
; Nr = log(Pr)/log(1-pp)    ; Nr=5.1 years

; ------
local kTr, kPr, Nr, Nrf
begin
  kTr    = dimsizes(Tr)
  kPr    = dimsizes(Pr)

  if (any(Pr.le.0 .or. Pr.gt.1)) then
      print("extval_return_period: Pr must be 0 < Pr < 1: Pr="+Pr)
      exit
  end if

  if (any(Tr.le.0)) then
      print("extval_return_period: Tr must be > 0: Tr="+Tr)
      exit
  end if

  if (kTr.eq.1 .or. kPr.eq.1) then    ;;if (isscalar(Tr) .and. isscaler(Pr)) then
      pp = 1d0/Tr
      Nr = log(Pr)/log(1-pp)
      if (kTr.eq.1) then
          Nr!0  = "Pr"
          Nr&Pr =  Pr
      else
          Nr!0  = "Tr"
          Nr&Tr =  Tr
      end if
  else 
      Nr = new( (/kTr,kPr/), "double")
      do k=0,kTr-1
         pp      = 1d0/Tr
         Nr(k,:) = log(Pr)/log(1-pp)
      end do
      Nr!0  = "Pr"
      Nr!1  = "Tr"
      Nr&Pr =  Nr
      Nr&Tr =  Tr
  end if

  Nr@long_name = "period of return event"  
  if (isatt(Tr,"units")) then
      Nr@units = Tr@units
  end if

  if (typeof(Tr).eq."double" .or. typeof(Pr).eq."double") then
      return(Nr)
  else
      Nrf = tofloat(Nr)
      copy_VarMeta(Nr, Nrf)
      return( Nrf )
  end if
end
;===========
undef("extval_mlegam")
function extval_mlegam(x:numeric, dims[*]:integer, opt[1]:logical)
;
; Gamma Distribution: Maximim Likelihood Estimation of 
;     location, shape, scale parameters
;
; Reference:
; Wilks: Statistical Methods in the Atmospheric Sciences (2006); pp 95-102
;
; Int. J. Climatol.  27 : 935 – 944 (2007) 
; http://onlinelibrary.wiley.com/doi/10.1002/joc.1441/pdf
; Equations: 4-7
; pg: 938:
; "x(i) is equal to all non-zero values in the rainfall history, 
;  and the mean ( x) is the arithmetic mean of all non-zero values"
;  ... later ...     
; "the  gamma  distribution parameters are being estimated by 
;  fewer than 18 samples  (out  of  36 years) and  the  parameter  
;  reliability becomes quite suspect"

local X, Xavg, D, alpha, beta, s2, con, gam_mlw
begin
  X = x
  if (.not.isatt(x,"_FillValue")) then
      X@_FillValue = default_fillvalue(typeof(X))
  end if
  X = where(X.eq.0, X@_FillValue, X)  

  Xavg  = dim_avg_n(X, dims)                    ; average of non-zero values
 ;Xnum  = dim_num_n(.not.(ismissing(X)), dims)  ; number of non-missing values

  D     = log(Xavg) - dim_avg_n(log(X), dims)   ; eqn 4.40   ; Wilks Ref
  con   = 1/(4*D)                               ; clarity                   
  shape = con*(1+sqrt(1+(4*D/3)))               ; eqn 4.41
  scale = Xavg/shape                            ; eqn 4.42 (alpha)
  gam_mean  = shape*scale           
  gam_var   = shape*scale^2        
                                                ; gamma quantile function 
  prob  = (/0.50/)                              ; default (median)
  if (opt .and. isatt(opt,"prob")) then
      prob := opt@prob
  end if
  gam_quant = gam_mean +(scale/shape)*((-log(prob))^shape -1) ; eqn 4.56  
                                                              ; gam_mean ==> location 

  gam_mle = (/gam_mean, scale, shape, gam_var, gam_quant/)
  gam_mle@long_name = "gamma: maximum liklihood estimates"
  gam_mle@info      = "location, shape, scale, gam_var, gam_quant"
  gam_mle@NCL_tag   = "extval_mlegam"

  return(gam_mle)
end
