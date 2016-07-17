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

   pdf = new( (/nscl,nx/), typeof(x), getVarFillValue(x))
   cdf = pdf

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
local nshp, nscl, nx, pdf, cdf, y, ix
begin
   nshp = dimsizes(shape)
   nscl = dimsizes(scale)
   if (nshp.ne.nscl) then
       print("extval_frechet: shape and scale arguments must be the same size")
       print("extval_frechet: nshp="+nshp+"  nscl="+nscl)
       exit
   end if

   nx   = dimsizes(x)
   pdf  = new( (/nscl,nx/), typeof(x), getVarFillValue(x))
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

   cdf@long_name = "CDF: Frechet: Extreme Value Type II: Maxima"
   cdf@shape     = shape
   cdf@scale     = scale

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
local y, pdf, cdf, nshp, nscl, nsft, nx, n, ix
begin
  ;if (any(x.le.0))  then
  ;    print("extval_weibull: one or more x <= 0")
  ;    exit
  ;end if

   nshp = dimsizes(shape)
   nscl = dimsizes(scale)
   nsft = dimsizes(center)
   if (nshp.ne.nscl .or. nshp.ne.nsft) then
       print("extval_weibull: shape, scale and center arguments must be the same size")
       print("extval_weibull: nshp="+nshp+"  nscl="+nscl+"  nsft="+nsft)
       exit
   end if
   nx   = dimsizes(x)

   pdf = new( (/nscl,nx/), typeof(x), getVarFillValue(x))
   cdf = pdf

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
;=======================================
undef("extval_pareto")
;---
; Pareto: Not checked in any detail
;---
function extval_pareto(x[*]:numeric, shape[*]:numeric, scale[*]:numeric, center[*]:numeric, opt[1]:integer)
;
; Generalized Pareto Distribution
; http://www.mathwave.com/help/easyfit/html/analyses/distributions/pareto.html
; http://www.mathwave.com/articles/generalized_pareto_distribution.html
;
; The Generalized Pareto (GP) is a right-skewed distribution, 
; parameterized with a shape parameter, k, and a scale parameter, sigma. 
; k is also known as the "tail index" parameter, and can be positive, zero, or negative.k
;
; Like the exponential distribution, the generalized Pareto distribution is 
; often used to model the tails of another distribution. 
;
local y, q, arg, pwr, pdf, cdf, nshp, nscl, nsft, nx, n
begin
  ;if (any(x.le.0))  then
  ;    print("extval_pareto: x>0: xmin="+min(x))
  ;    exit
  ;end if

   nshp = dimsizes(shape)
   nscl = dimsizes(scale)
   nsft = dimsizes(center)
   if (nshp.ne.nscl) then
       print("extval_pareto: shape, scale and center arguments must be the same size")
       print("extval_pareto: nshp="+nshp+"  nscl="+nscl+"  nsft="+nsft)
       exit
   end if
   nx   = dimsizes(x)

   pdf = new( (/nscl,nx/), typeof(x), getVarFillValue(x))
   pdf = 0
   cdf = pdf

   do n=0,nscl-1
      if (scale(n).le.0) then
          print("extval_pareto: shape <=0: shape="+shape(n)+"; scale="+scale(n) )
          continue
      end if

      q = 1.0/scale(n)                          ; temporary variables for clarity                         
      y = (x-center(n))/scale(n)

      if (shape(n).ne.0) then
          pwr = (-1-(1/shape(n)))               ; clarity only; power
          arg =  1+shape(n)*y                   ; clarity
          pdf(n,:) = q*(arg^pwr)

          pwr = (-1/shape(n))
          cdf(n,:) = 1-(arg^pwr)
      else
          pdf(n,:) = q*exp(-y) 
          cdf(n,:) = 1-exp(-y)
      end if
   end do

   pdf@long_name = "Pareto: General: PDF"
   pdf@shape     = shape
   pdf@scale     = scale
   pdf@center    = center

   cdf@long_name = "Pareto: General: CDF"
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
local nshp, nscl, nx, pdf, cdf, y
begin
   nshp = dimsizes(shape)
   nscl = dimsizes(scale)
   nsft = dimsizes(center)    ; center
   if (nshp.ne.nscl .or. nshp.ne.nsft) then
       print("extval_gev: shape, scale  and sfift arguments must be the same size")
       print("extval_gev: nshp="+nshp+"  nscl="+nscl+"  nsft="+nsft)
       exit
   end if

   nx   = dimsizes(x)
   pdf  = new( (/nscl,nx/), typeof(x), getVarFillValue(x))
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
   if (typeof(x).eq."float" .or. typeof(x).eq."double" ) then
       table = new( (/N,nstat/), typeof(x), getVarFillValue(x))
   else
       table = new( (/N,nstat/), "float", 1e10)
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
local N, N1, ix, n, rank, retval
begin
   N     = dimsizes(x)
   ix    = dim_pqsort_n(x, 1, 0)  
               
   if (typeof(x).eq."float" .or. typeof(x).eq."double" ) then
       rank   = new( N, typeof(x), getVarFillValue(x))
       retval = new( N, typeof(x), getVarFillValue(x))
   else       ; integer, short, ...
       rank   = new( N, "float", 1e10)
       retval = new( N, "float", 1e10)
   end if

   do n=0,N-1
      rank(n) = ind(ix.eq.n)+1      ; Rank
   end do

   retval = location + (scale/shape)*((-log(1.0-rank))^(-shape)-1.0)
   retval@long_name = "Return Level"
   return(retval)
end
;------
undef("extval_prob_tresh")
function extval_prob_tresh(T:numeric, n[1]:numeric)
;
; Probability Pe that one or more floods occurring during any period 
; will exceed a given 'event' (eg, flood) threshold can be determined, 
; using the binomial distribution. 
;
; What is the probabili ty of having one or more occurrence 
;     of a 10-year event within the 10-year life of a projectt?
; Assume that you are considering buying a house just with
;      in the flood plain.  Flood plains are defined for return periods 
;      of 100 years.  What is the probability that one or more times 
;      you will be flooded within the 30-year life of a mortgage?
;
; Risk Analysis: design risk
;     https://wiki.umn.edu/pub/WilsonNotes/WebHome/risk.pdf
;
; Assumptions: values for 'T' are independent and stationary
;
; T  - event period
; n  - period

begin
;;pe = 1-(1-(1.0/T))^n
  if (typeof(T).eq."double") then
      pe = 1d0-(1d0-(1d0/T))^n
  else
      pe = 1.0-(1.0-(1.0/T))^n
  end if

  pe@long_name = "probability of event exceedance"
  pe@info      = "design risk"
  return (pe)
end
