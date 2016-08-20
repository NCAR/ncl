;===========================================================
; BOOTSTRAP: V2
;===========================================================
undef("bootstrap_optarg")
function bootstrap_optarg(z:numeric, nBoot[1]:integer, nDim:integer, opt[1]:logical)
; ----------------------------------------------------------
; ---******************Not called users*********************
; ----------------------------------------------------------
; ---This is called internally by various boot_*functions. 
; ---Common front-end to selected bootstrap functions
; ---Check Input and options; return extracted information
; ----------------------------------------------------------
local dimz, rankz, N, NN, NRL, zBoot, rseed1, rseed2, err, zType \
    , zFillValue, n0, n1
begin
  err      = 0
  err@long_name = "error code"
;----------------------------------------------
;--- dimension sizes and rank of the 'z' array
;----------------------------------------------
  dimz     = dimsizes(z)
  rankz    = dimsizes(dimz)
  if (rankz.gt.4) then
      err  = -20 
      print("bootstrap_optarg: rankz="+rankz+": rankz>4 not currently supported")
    ;;return([/ err, -999, -999, -999, -999.0, -999.0 /])
      exit
  end if

;----------------------------------------------
;--- dimension number for each variable        
;----------------------------------------------
  rank_nDim = dimsizes(dimsizes(nDim))

  n0 = nDim(0)
  if (rank_nDim.eq.1) then
      n1 = nDim(0)
  else
      n1 = nDim(1)
  end if
;----------------------------------------------
;--- Dimension size of the specified dimension
;----------------------------------------------
  N          = dimz(n0)               ; Most commonly, this is the 'time' dimension
  NN         = N                      ; Sub-sample size; most commonly NN=N
                                      ; Also, Regression segment Length; contiguous
                                      ;       used in bootstrap_regcoef
;----------------------------------------------
;--- Allocate memory for the bootstrapped array
;----------------------------------------------
  dimz(n0)   = nBoot                  ; 'trick'                                      

  zType      = typeof(z) 
  zFillValue = getFillValue(z)
  if (zType.eq."double" .or. zType.eq."float") then
      zBoot  = new (dimz, zType, zFillValue) 
  else
      zBoot  = new (dimz,"float", 1e20 )   ; integer, etc ... force 'float'
  end if

;----------------------------------------------
;--- Optional argumenta and error checking; (kinda) overkill but ...
;----------------------------------------------
  if (opt) then
      if (isatt(opt,"sample_size")) then
          if (typeof(opt@sample_size).ne."integer") then
              err = -14
              print("bootstrap_optarg: opt@sample_size must be integer: typeof(opt@sample_size)=" \
                    +typeof(opt@sample_size))
              exit
          end if

          NN = opt@sample_size      ; sub-sample size

          if (NN.gt.N) then
              err  = -13
              print("bootstrap_optarg: opt@sample_size="+NN+" is > sample size, N="+N) 
              exit
          end if
      end if

      if (isatt(opt,"rseed1")) then
          if (typeof(opt@rseed1).eq."integer") then
              rseed1 = opt@rseed1 
          else
              err = -12
              print("bootstrap_optarg: rseed1 must be integer: typeof(rseed1)="+typeof(opt@rseed1))
              exit
          end if
      end if

      if (isatt(opt,"rseed2")) then
          if (typeof(opt@rseed2).eq."integer") then
              rseed2 = opt@rseed2 
          else
              err = -11
              print("bootstrap_optarg: rseed2 must be integer: typeof(rseed2)="+typeof(opt@rseed2))
              exit
          end if
      end if

      if (isatt(opt,"rseed3")) then
          if (opt@rseed3.eq."clock") then
              rseed1 = toint(systemfunc(" date +%s"))
              rseed2 = toint((12345l*rseed1)%2147483398l)      ; NCL mod operator (%) 
          else
              err = -10
              print("bootstrap_optarg: rseed3 must be set to clock: opt@rseed3="+opt@rseed3)
              exit
          end if
      end if
  end if

  if (isvar("rseed1") .and. isvar("rseed2")) then  
      random_setallseed(rseed1, rseed2)     ; set seeds
  end if
;----------------------------------------------
;--- Return the needed information to the calling bootstrap function  
;----------------------------------------------
  return( [/dimz, rankz, N, NN, zBoot/] )
end
;===========================================================
undef("bootstrap_finish")
function bootstrap_finish(statName[1]:string, z:numeric, zBoot:numeric \
                         ,dimz[*]:integer, rankz[1]:integer            \
                         ,N[1]:integer, NN[1]:integer, nBoot[1]:integer)
; ----------------------------------------------------------
; ---******************Not called users*********************
; ----------------------------------------------------------
; ---This is called internally by the bootstrap function. 
; ---Common end to all bootstrap functions
; ---Basic stats on 'zBoot' and add coordinate and units information (if present) 
; ----------------------------------------------------------
local dimz, idz, dimBoot, zBootAvg, zBootStd, Z, ZAVG, ZSTD \
    , eps, zeps, zdenom, znumer 
begin
;----------------------------------------------
;--- Retrieve some info from input arguments
;----------------------------------------------
  dimz     = dimsizes(z)
  dimBoot  = dimsizes(zBoot)

;----------------------------------------------
;--- Calculate mean and std, dev. of the input arguments 
;----------------------------------------------
  if (statName.eq."correl") then                ; special case
 
      eps     = 1e-05
      zeps    = totype(eps, typeof(zBoot))

      zdenom  = 1.0-zBoot                       ; denominator of Z-transform
      zdenom  = where(zdenom.eq.0, zeps, zdenom); prevent division by 0

      znumer  = 1.0+zBoot
      znumer  = where(znumer.eq.0, zeps, znumer); prevent log(0)

      Z     = 0.5*log(znumer/zdenom)            ; Fischer z-transform of all bootstrapped correlations
      ZAVG  = dim_avg_n(Z, 0)                   ; avg Z
      ZSTD  = dim_stddev_n(Z, 0)                ; stddev Z
    ;;ZSE   = 1.0/sqrt(nBoot-1)                 ; SE of boot strapped values
    ;;NVAL  = cdft_t(rankHi,9999)               ; 2-sided; use 'big' number for normal dist   
    ;;ZLOW  = Z-NVAL*ZSE                        ; normal low:  2.5% 
    ;;ZHI   = Z+NVAL*ZSE                        ; normal hi ; 97.5%
                                                ; transform back to 'r' space
      zBootAvg = tanh(ZAVG)                     ; correlation
      zBootStd = tanh(ZSTD)                     ; not really correct ... place holder
    ;;rBootLow = tanh(ZLOW)                     ; =(exp(2*zlow)-1)/(exp(2*zlow)+1)
    ;;rBootHi  = tanh(ZHI)                      ; =(exp(2*zhi )-1)/(exp(2*zhi )+1)
      delete([/Z, ZAVG, ZSTD /])
  else
      zBootAvg = dim_avg_n(zBoot, 0)          
      zBootStd = dim_stddev_n(zBoot, 0) 
  end if
;----------------------------------------------
;--- Sort bootstrap values into ascending order 
;----------------------------------------------

  idz = dim_pqsort_n(zBoot, 2, 0)               ; sort bootstrap means into ascending order 
  
;----------------------------------------------
;---  Add meta data to returned variables 
;----------------------------------------------

  zBoot!0    = "boot"
  zBoot&boot = ispan(0,nBoot-1,1)               ; could also be (1,nBoot,1) 

  if (rankz.eq.2) then
      copy_VarCoords(z(0,:), zBoot(0,:))
      copy_VarCoords(z(0,:), zBootAvg)
      copy_VarCoords(z(0,:), zBootStd)
  end if

  if (rankz.eq.3) then
      copy_VarCoords(z(0,:,:), zBoot(0,:,:))
      copy_VarCoords(z(0,:,:), zBootAvg)
      copy_VarCoords(z(0,:,:), zBootStd)
  end if

  if (rankz.eq.4) then
      copy_VarCoords(z(0,:,:,:), zBoot(0,:,:,:))
      copy_VarCoords(z(0,:,:,:), zBootAvg)
      copy_VarCoords(z(0,:,:,:), zBootStd)
  end if

  if (isatt(z,"long_name")) then
      zBoot@long_name    = z@long_name
      zBootAvg@long_name = z@long_name 
      zBootStd@long_name = z@long_name 
  end if

  if (isatt(z,"units")) then
      zBoot@units    = z@units
      zBootAvg@units = z@units 
      zBootStd@units = z@units 
  end if

; add attributes data

  zBoot@bootstrap_info     = "bootstrap_"+statName+": Boot samples"
  zBootAvg@bootstrap_info  = "bootstrap_"+statName+": Average: "+nBoot+"  bootstrapped samples"
  zBootStd@bootstrap_info  = "bootstrap_"+statName+": Std. Deviation: "+nBoot+" bootstrapped samples" 

  return( [/ zBoot, zBootAvg, zBootStd /] )
end
;===========================================================
undef("bootstrap_indices")
function bootstrap_indices(K[1]:integer, KK[1]:integer, opt[1]:logical)
;-----------------------------------------------------------
; ---******************Not called users*********************
;-----------------------------------------------------------
;--- Utility used select all or sub-sample of total K 
;-----------------------------------------------------------
local sample_method, IW, k, kStrt, kLast, n
begin
  sample_method = 1                             ; Sample with replacement (default)
  if (opt) then
      if (isatt(opt,"sample_method")) then  
          if (typeof(opt@sample_method).eq."integer" .and.       \
              opt@sample_method.ge.0 .and. opt@sample_method.le.1) then
              sample_method = opt@sample_method
          else
              print("bootstrap_indices: something wrong: opt@sample_method="+ opt@sample_method)
              exit
          end if
      end if   ; opt@sample_method
  end if       ; opt

  IW = generate_sample_indices(K,sample_method) ; random indices from all

  if (K.eq.KK) then                                                    
      return(IW)                                ; default
  else if (opt .and. isatt(opt,"sequential") .and. opt@sequential) then
          do k=0,KK-1 
             kStrt = IW(k)                      ; random initial subscript
             kLast = kStrt+KK-1                 ; last subscript
             if (kLast.le.(K-1)) then
                 return(ispan(kStrt,kLast,1))   ; sequential indices 
             end if
          end do
       else
          return(IW(0:KK-1))                    ; 1st KK random indices
       end if
  end if    ; K=KK
end
;==============================================================
undef("resample_method")
function resample_method(iw,isamp,N,NN,opt)
;--------------------------------------------------------------
; ---******************Not called users*********************
;--------------------------------------------------------------
; utility used to specify *internal* resampling        
; IMHO ... do not use isamp=1 or isamp=2
;--------------------------------------------------------------
local jw
begin
  jw  = iw                             ; isamp=0 [x,y pairs]
  if (isamp.eq.0) then
      return(jw)
  end if

  if (isamp.eq.1) then
      jw = bootstrap_indices(N,NN,opt) ; isamp=1 [x,y independent]  
      return(jw)
  end if

  jw = iw(::-1)                        ; isamp=2 [randomized pairs values]
  return(jw)
end
;================================================================
;          FUNCTIONS (possibly) CALLED BY USERS
;================================================================
undef("bootstrap_print_list")
procedure bootstrap_print_list(x:list, opt_print[1]:integer)
;------------------------------------------------------------------------
;--- Utility used to print elements of a variable of type list
;--- Primarily used for debug but can be user called also
;------------------------------------------------------------------------
;
; Nomenclature
; x         - variable of type list created by one of the bootstrap functions
; opt_print - print format of information                                
;
local nList, n, List_Member
begin
  nList = ListCount (x)
  do n=0,nList-1
     List_Member := x[n]                          ; extract element from a list
     if (opt_print.eq.0) then
         print(List_Member)
     else if (opt_print.eq.1) then
         printVarSummary(List_Member)
     else if (opt_print.eq.2) then
         printMinMax(List_Member,0)
     else if (opt_print.eq.3) then
         printVarSummary(List_Member)
         printMinMax(List_Member,0)
         end if
       end if
      end if
     end if
  end do
end
;===========================================================
; bootstrapped information
;===========================================================
undef("bootstrap_estimate")
function bootstrap_estimate(xBoot:numeric, pc[1]:numeric, opt[1]:logical)
local dimBoot, nBoot, npc, xBoot_pc
begin
  if (pc.lt.0 .or. pc.gt.1) then
      print("bootstrap_estimate: illegal pc="+pc)
      exit
  end if

  dimBoot    = dimsizes(xBoot)
  rankBoot   = dimsizes(dimBoot)
  nBoot      = dimBoot(0)
  npc        = round(pc*(nBoot-1),3)    ; index for sorted array

  if (rankBoot.eq.1) then
      xBoot_pc   = xBoot(npc)    
  else if (rankBoot.eq.2) then
      xBoot_pc   = xBoot(npc,:)
  else if (rankBoot.eq.3) then
      xBoot_pc   = xBoot(npc,:,:)
  else if (rankBoot.eq.4) then
      xBoot_pc   = xBoot(npc,:,:,:)
     end if      ; 4
    end if       ; 3
   end if        ; 2
  end if         ; 1
  xBoot_pc@estimate = sprintf("%5.1f", (pc*100))+"%"
  return(xBoot_pc)
end
;================================================================
undef("bootstrap_stat")
function bootstrap_stat(z:numeric, stat[1]:integer \
                       ,nBoot[1]:integer, nDim[1]:integer, opt[1]:logical)
;-------------------------------------------------------------------------
; bootstrap of basic statistics derived from a variable ('univariate')
;
; Nomenclature:
; z     - input variable:  z(N), z(N,:), Z(N,:,:), Z(N,:,:,:)
;         N (usually 'time') is source sample size
; stat  - flag: =0 mean; =1 stddev; =2 variance
; nBoot - # of bootstrapped  estimates to be generated
; nDim  - dimension to be operated upon; currently nDim should be 0
; opt   - option (See function 'bootstrap_optarg') 
;-------------------------------------------------------------------------

local start, err, dimz, rankz, N, NN, zBoot \
     ,statName, iw, finish, zBootAvg, zBootStd
begin
;-------------------------------------------------------------------------
; bootstrap_optarg: extract 'opt' information and create th array
;-------------------------------------------------------------------------
  start = bootstrap_optarg(z, nBoot, nDim, opt)
  dimz    = start[0]
  rankz   = start[1]
  N       = start[2]
  NN      = start[3]      ; NN=N (most commonly)
  zBoot   = start[4]
  delete(start)

  if (stat.lt.0 .or. stat.gt.6) then
     err = -1
     print("bootstrap_stat: unrecognized option: stat="+stat)
     print("                currently stat=0 (mean): stat=1 (stddev); stat=2 (variance) are recognized")
     exit
  end if

; Bootstrap the appropriate statistic

  if (stat.eq.0) then
      statName = "mean"                    ; stat=0

      if (rankz.eq.1) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb) = dim_avg_n( z(iw), nDim) 
          end do
      end if

      if (rankz.eq.2) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:) = dim_avg_n(z(iw,:), nDim)
          end do
      end if

      if (rankz.eq.3) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:) = dim_avg_n(z(iw,:,:), nDim)
          end do
      end if

      if (rankz.eq.4) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:,:) = dim_avg_n(z(iw,:,:,:), nDim)
          end do
      end if
  end if

  if (stat.eq.1) then
      statName = "stddev"

      if (rankz.eq.1) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb) = dim_stddev_n( z(iw), nDim) 
          end do
      end if

      if (rankz.eq.2) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:) = dim_stddev_n(z(iw,:), nDim)
          end do
      end if

      if (rankz.eq.3) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:) = dim_stddev_n(z(iw,:,:), nDim)
          end do
      end if

      if (rankz.eq.4) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:,:) = dim_stddev_n(z(iw,:,:,:), nDim)
          end do
      end if
  end if

  if (stat.eq.2) then
      statName = "variance"

      if (rankz.eq.1) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb) = dim_variance_n( z(iw), nDim) 
          end do
      end if

      if (rankz.eq.2) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:) = dim_variance_n(z(iw,:), nDim)
          end do
      end if

      if (rankz.eq.3) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:) = dim_variance_n(z(iw,:,:), nDim)
          end do
      end if

      if (rankz.eq.4) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:,:) = dim_variance_n(z(iw,:,:,:), nDim)
          end do
      end if
  end if

  if (stat.eq.3) then
      statName = "median"

      if (rankz.eq.1) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb) = dim_median_n( z(iw), nDim) 
          end do
      end if

      if (rankz.eq.2) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:) = dim_median_n(z(iw,:), nDim)
          end do
      end if

      if (rankz.eq.3) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:) = dim_median_n(z(iw,:,:), nDim)
          end do
      end if

      if (rankz.eq.4) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:,:) = dim_median_n(z(iw,:,:,:), nDim)
          end do
      end if
  end if

  if (stat.eq.4) then
      statName = "min"

      if (rankz.eq.1) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb) = dim_min_n( z(iw), nDim) 
          end do
      end if

      if (rankz.eq.2) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:) = dim_min_n(z(iw,:), nDim)
          end do
      end if

      if (rankz.eq.3) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:) = dim_min_n(z(iw,:,:), nDim)
          end do
      end if

      if (rankz.eq.4) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:,:) = dim_min_n(z(iw,:,:,:), nDim)
          end do
      end if
  end if

  if (stat.eq.5) then
      statName = "max"

      if (rankz.eq.1) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb) = dim_max_n( z(iw), nDim) 
          end do
      end if

      if (rankz.eq.2) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:) = dim_max_n(z(iw,:), nDim)
          end do
      end if

      if (rankz.eq.3) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:) = dim_max_n(z(iw,:,:), nDim)
          end do
      end if

      if (rankz.eq.4) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:,:) = dim_max_n(z(iw,:,:,:), nDim)
          end do
      end if
  end if

  if (stat.eq.6) then
      statName = "sum"

      if (rankz.eq.1) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb) = dim_sum_n( z(iw), nDim) 
          end do
      end if

      if (rankz.eq.2) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:) = dim_sum_n(z(iw,:), nDim)
          end do
      end if

      if (rankz.eq.3) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:) = dim_sum_n(z(iw,:,:), nDim)
          end do
      end if

      if (rankz.eq.4) then 
          do nb=0,nBoot-1
             iw = bootstrap_indices(N,NN,opt)
             zBoot(nb,:,:,:) = dim_sum_n(z(iw,:,:,:), nDim)
          end do
      end if
  end if
           ; associate meta data with returned variables
  finish   = bootstrap_finish(statName, z, zBoot, dimz, rankz, N, NN, nBoot)

;;zBoot    = finish[0]
;;zBootAvg = finish[1]
;;zBootStd = finish[2]
;;return([/ zBoot, zBootAvg, zBootStd /]) 
  return([/ finish[0], finish[1], finish[2] /]) ; [/ zBoot, zBootAvg, zBootStd /]
end
;---------------------------------------------------------------
undef("bootstrap_regcoef")
function bootstrap_regcoef(x[*]:numeric, z:numeric, nBoot[1]:integer, nDim:integer, opt[1]:logical)

;;function bootstrap_regcoef(x[*]:numeric, z:numeric, nBoot[1]:integer, nDim[2]:integer, opt[1]:logical)
;;                       ,nBoot[1]:integer, nDim[1]:integer, opt[1]:logical)
;-------------------------------------------------------------------------
; bootstrap of regression coefficients (slopes)
; Nomenclature:
; x     - something like 'time'
; z     - input variable:  z(N), z(N,:), Z(N,:,:), Z(N,:,:,:)
;         N (usually 'time') is source sample size
; nBoot - # of bootstrapped  estimates to be generated
; nDim  - dimension to be operated upon; currently nDim should be 0
; opt   - option (See function 'bootstrap_optarg') 
;-------------------------------------------------------------------------
local start, err, dimz, rankz, N, NN, statName, rank_nDim, n0, n1 \
    , rcOrig, iw, rcBoot, rcBootAvg, rcBootStd
begin
  start = bootstrap_optarg(z, nBoot, nDim, opt)
  dimz    = start[0]
  rankz   = start[1]
  N       = start[2]
  NN      = start[3]      ; NN=N (most commonly)
  rcBoot  = start[4]
  delete(start)
print("N="+N+"   NN="+NN)

  statName = "regcoef"
  rank_nDim = dimsizes(dimsizes(nDim))
  
  n0 = nDim(0)
  if (rank_nDim.eq.1) then
      n1 = nDim(0)
  else
      n1 = nDim(1)
  end if

; Bootstrap 

  if (rankz.eq.1) then 
      do nb=0,nBoot-1
         iw = bootstrap_indices(N,NN,opt)
if (nb.eq.0) then
    print("===")
    print("N="+N+"   NN="+NN+"  dimsizes(iw)="+dimsizes(iw))
    print(opt)
end if
         rcBoot(nb) = (/ regCoef_n(x(iw), z(iw), n0, n1) /)    ; no attributes
      end do
  end if
if (nBoot.le.100) then
    print(rcBoot)
end if

  if (rankz.eq.2) then 
      do nb=0,nBoot-1
         iw = bootstrap_indices(N,NN,opt)
         rcBoot(nb,:) = (/ regCoef_n(x(iw), z(iw,:), n0, n1) /) 
      end do
  end if

  if (rankz.eq.3) then 
      do nb=0,nBoot-1
         iw = bootstrap_indices(N,NN,opt)
         rcBoot(nb,:,:) = (/ regCoef_n(x(iw), z(iw,:,:), n0, n1) /)
      end do
  end if

  if (rankz.eq.4) then 
      do nb=0,nBoot-1
         iw = bootstrap_indices(N,NN,opt)
         rcBoot(nb,:,:,:) = (/ regCoef_n(x(iw), z(iw,:,:,:), n0, n1) /)
      end do
  end if 
            ; associate meta data with returned variables
  finish   = bootstrap_finish(statName, z, rcBoot, dimz, rankz, N, NN, nBoot)

;;rcBoot    = finish[0]
;;rcBootAvg = finish[1]
;;rcBootStd = finish[2]
;;return([/ rcBoot, rcBootAvg, rcBootStd /]) 

  return([/ finish[0], finish[1], finish[2] /]) ; [/ rcBoot, rcBootAvg, rcBootStd /]
end
;---------------------------------------------------------------
undef("bootstrap_correl")
function bootstrap_correl(x:numeric, z:numeric, nBoot[1]:integer, nDim:integer, opt[1]:logical)
;-------------------------------------------------------------------------
; bootstrap of correlation coefficients 
; Nomenclature:
; x     - x(N) or match shape & size of Z
; z     - input variable:  z(N), z(N,:), Z(N,:,:), Z(N,:,:,:)
;         N (usually 'time') is source sample size
; nBoot - # of bootstrapped  estimates to be generated
; nDim  - dimension to be operated upon; currently nDim should be 0
; opt   - option (See function 'bootstrap_optarg') 
;-------------------------------------------------------------------------
; The following is *NOT* in effect
;-------------------------------------------------------------------------
;;<ul>
;;<li><em>opt</em>@resample_method: Set the resampling 'method' used for
;;<funcref uri="CONTRIB_FUNC">bootstrap_correl</funcref>. 
;;<li> <em>opt</em>@resample_method=0: Resample <em>x</em>, <em>y</em> pairs. (Default). 
;;<li> <em>opt</em>@resample_method=1: Resample <em>x</em> and <em>y</em> separately.
;;<li> <em>opt</em>@resample_method=2: Resample <em>x</em> and <em>y</em> via randomized pairs.
;;</ul>
;-------------------------------------------------------------------------
local start, err, dimz, rankz, N, NN, zBoot, idz, dimx, rankx, n0, n1, rank_nDim \
     ,ccBoot, ccBootAvg, ccBootStd, jw, iw, statname, isamp 
begin
  start = bootstrap_optarg(z, nBoot, nDim, opt)
  dimz    = start[0]
  rankz   = start[1]
  N       = start[2]
  NN      = start[3]      ; NN=N (most commonly)
  ccBoot  = start[4]
  delete(start)

  dimx    = dimsizes(x)
  rankx   = dimsizes(dimx)
  if (rankx.gt.rankz) then
      print("bootstrap_correl: rankx>rankz: unexpected: rankx="+rankx+"  rankz="+rankz)
      exit
  end if
  if (rankx.ne.1 .and. rankx.ne.rankz) then
      print("bootstrap_correl: rankx>1 and rankx.ne.rankz: unexpected: rankx="+rankx+"  rankz="+rankz)
      exit
  end if
  
  n0        = nDim(0)
  rank_nDim = dimsizes(dimsizes(nDim))
  if (rank_nDim.eq.1) then
      n1 = nDim(0)
  else
      n1 = nDim(1)
  end if

  statName = "correl"
  
;;ccOrig = escorc_n(x, z, nDim(0), nDim(0) )  ; original
;;ccOrigStdErr = sqrt(1-ccOrig^2)/sqrt(NN-2)  ; standard error of r statistic 
;;z      = 0.5*log((1+ccOrig)/(1-ccOrig))     ; Fischer z-transform 
;;zse    = 1.0/sqrt(NN-3)         ; standard error of z statistic
;;nval   = cdft_t(rankHi,9999)    ; 2-sided; use 'big' number for normal dist   
;;zLow   = z-nval*zse             ; normal low:  2.5% 
;;zHi    = z+nval*zse             ; normal hi ; 97.5%
;;                                ; transform back to 'r' space
;;rLow   = tanh(zLow)             ; =(exp(2*zlow)-1)/(exp(2*zlow)+1)
;;rHi    = tanh(zHi)              ; =(exp(2*zhi )-1)/(exp(2*zhi )+1)

; Bootstrap sample auto-correlations
; Which resampling 'strategy' (method)"

  isamp = 0
  if (opt) then 
      if (isatt(opt,"resample_method")) then
          isamp = opt@resample_method
      end if
  end if

  if (rankz.eq.1) then 
      if (rankx.eq.1) then
          do nb=0,nBoot-1
             if (isamp.eq.0) then
                 iw = bootstrap_indices(N,NN,opt)
                 ccBoot(nb) = escorc_n(x(iw), z(iw), n0, n1)  ; (iw), (iw) pairs
             else
                 jw = resample_method(iw,isamp,N,NN,opt)
                 ccBoot(nb) = escorc_n(x(iw), z(jw), n0, n1)
             end if
          end do
      else
          print("bootstrap_correl: rankz=1 and rankx.ne.1: rankx="+rankx+"  rankz="+rankz)
          exit
      end if
  end if

  if (rankz.eq.2) then 
      do nb=0,nBoot-1
         iw = bootstrap_indices(N,NN,opt)
        ;jw = resample_method(iw,isamp,N,NN,opt)

         if (rankx.eq.1) then
             ccBoot(nb,:) = escorc_n(x(iw)  , z(iw,:), n0, n1)
            ;ccBoot(nb,:) = escorc_n(x(iw)  , z(jw,:), n0, n1)
         else
             ccBoot(nb,:) = escorc_n(x(iw,:), z(iw,:), n0, n1)
            ;ccBoot(nb,:) = escorc_n(x(iw,:), z(jw,:), n0, n1)
         end if
      end do
  end if

  if (rankz.eq.3) then 
      do nb=0,nBoot-1
         iw = bootstrap_indices(N,NN,opt)
        ;jw = resample_method(iw,isamp,N,NN,opt)

         if (rankx.eq.1) then
             ccBoot(nb,:,:) = escorc_n(x(iw)    , z(iw,:,:), n0, n1)
            ;ccBoot(nb,:,:) = escorc_n(x(iw)    , z(jw,:,:), n0, n1)
         else
             ccBoot(nb,:,:) = escorc_n(x(iw,:,:), z(iw,:,:), n0, n1)
            ;ccBoot(nb,:,:) = escorc_n(x(iw,:,:), z(jw,:,:), n0, n1)
         end if
      end do
  end if

  if (rankz.eq.4) then 
      do nb=0,nBoot-1
         iw = bootstrap_indices(N,NN,opt)
        ;jw = resample_method(iw,isamp,N,NN,opt)

         if (rankx.eq.1) then
             ccBoot(nb,:,:,:) = escorc_n(x(iw)      , z(iw,:,:,:), n0, n1)
            ;ccBoot(nb,:,:,:) = escorc_n(x(iw)      , z(jw,:,:,:), n0, n1)
         else
             ccBoot(nb,:,:,:) = escorc_n(x(iw,:,:,:), z(iw,:,:,:), n0, n1)
            ;ccBoot(nb,:,:,:) = escorc_n(x(iw,:,:,:), z(jw,:,:,:), n0, n1)
         end if
      end do
  end if

           ; associate meta data with returned variables
  finish   = bootstrap_finish(statName, z, ccBoot, dimz, rankz, N, NN, nBoot)

;;ccBoot    = finish[0]   
;;ccBootAvg = finish[1]   ; 'z' average of correlation coef
;;ccBootStd = finish[2]
;;return([/ ccBoot, ccBootAvg, ccBootStd /]) 

  return([/ finish[0], finish[1], finish[2] /]) ; [/ ccBoot, ccBootAvg, ccBootStd /]
end
;---------------------------------------------------------------
undef("bootstrap_diff")
function bootstrap_diff(x:numeric, z:numeric, nBoot[1]:integer, nDim:integer, opt[1]:logical)
;-------------------------------------------------------------------------
; bootstrap of sample differences          
; Nomenclature:
; x     - x(NX) or match shape & size of Z
; z     - input variable:  z(NZ), z(NZ,:), Z(NZ,:,:), Z(NZ,:,:,:)
;         N (usually 'time') is source sample size
; nBoot - # of bootstrapped  estimates to be generated
; nDim  - dimension to be operated upon; currently nDim should be 0
; opt   - option (See function 'bootstrap_optarg') 
;-------------------------------------------------------------------------
local startz, err, dimz, rankz, NZ, NNZ, idz \
     ,startx, err, dimx, rankx, NX, NNX, idx \
     ,diffBoot, diffBootAvg, diffBootStd \
     ,xAvg, zAvg, xziw , diffOrig, iw, diffOrig, diffOrigStdErr, z, zse, zLow, zHi\
     ,nMidZ, rankMidZ
begin
  startz  = bootstrap_optarg(z, nBoot, nDim, opt)
  dimz     = startz[0]
  rankz    = startz[1]
  NZ       = startz[2]
  NNZ      = startz[3]     
  diffBoot = startz[4]
  delete(startz)

  startx   = bootstrap_optarg(x, nBoot, nDim, opt)
  dimx     = startx[0]
  rankx    = startx[1]
  NX       = startx[2]
  NNX      = startx[3]
  delete(startx)

; (possibly) override the NNX and NNZ with special x and y options 
; The documentation uses 'x' and 'y' (not 'x' and 'z')

  if (opt) then
      if (isatt(opt,"sample_size_x")) then
          NNX = min((/ opt@sample_size_x, NNX /))
      end if

      if (isatt(opt,"sample_size_y")) then
          NNZ = min((/ opt@sample_size_y, NNZ /))
      end if
  end if

  if (rankx.ne.rankz) then
      print("bootstrap_diff: rankx.ne.rankz: rankx="+rankx+"  rankz="+rankz)
      print("                maybe need to use conform or conform_dims to make ranks equal")
      exit
  end if

  statName = "diff"
  xAvg     = dim_avg_n(x, nDim(0) )  ; original
  zAvg     = dim_avg_n(z, nDim(0) )  ; original
  xzDiff   = xAvg-zAvg               ; original sample difference
  copy_VarCoords(zAvg, xzDiff)

; Bootstrap sample means

  if (rankz.eq.1 .and. rankx.eq.1) then 
      do nb=0,nBoot-1
         iwx  = bootstrap_indices(NX,NNX,opt)   
         iwz  = bootstrap_indices(NZ,NNZ,opt)
         xAve = dim_avg_n(x(iwx),  nDim(0))
         zAve = dim_avg_n(z(iwz),  nDim(0))
         diffBoot(nb) = xAve - zAve
      end do
  end if

  if (rankz.eq.2 .and. rankx.eq.2) then 
      do nb=0,nBoot-1
         iwx  = bootstrap_indices(NX,NX,opt)   
         iwz  = bootstrap_indices(NZ,NZ,opt)
         xAve = dim_avg_n(x(iwx,:),  nDim(0))
         zAve = dim_avg_n(z(iwz,:),  nDim(0))
         diffBoot(nb,:) = xAve - zAve
      end do
  end if

  if (rankz.eq.3) then 
      do nb=0,nBoot-1
         iwx  = bootstrap_indices(NX,NX,opt)   ; could be different sizes
         iwz  = bootstrap_indices(NZ,NZ,opt)
         xAve = dim_avg_n(x(iwx,:,:),  nDim(0))
         zAve = dim_avg_n(z(iwz,:,:),  nDim(0))
         diffBoot(nb,:,:) = xAve - zAve
      end do
  end if

  if (rankz.eq.4) then 
      do nb=0,nBoot-1
         iwx  = bootstrap_indices(NX,NX,opt)   ; could be different sizes
         iwz  = bootstrap_indices(NZ,NZ,opt)
         xAve = dim_avg_n(x(iwx,:,:,:),  nDim(0))
         zAve = dim_avg_n(z(iwz,:,:,:),  nDim(0))
         diffBoot(nb,:,:,:) = xAve - zAve
      end do
  end if
           ; associate meta data with returned variables
  finish   = bootstrap_finish(statName, z, diffBoot, dimz, rankz, NZ, NNZ, nBoot)
;;diffBoot    = finish[0]
;;diffBootAvg = finish[1]
;;diffBootStd = finish[2]
;;return([/diffBoot, diffBootAvg, diffBootStd /])

  return([/ finish[0], finish[1], finish[2] /]) 
end
;===============================================================
; the following is used for some bootstrap graphics
;===============================================================
undef("histogram_interp")
function histogram_interp(MidBarLocs[*]:numeric, BinLocs[*]:numeric, xHstMarkVals[*]:numeric)
local nMid, nBinLocs, MidBarVals, xHstBinLocs
begin
  nMid        = dimsizes(MidBarLocs)
  nBinLocs    = dimsizes(BinLocs)
  if (nBinLocs.ne.(nMid+1)) then
      print("histogram_interp: size mismatch:  nMid="+nMid+" nBinLocs="+nBinLocs)
      exit
  end if
  MidBarVals  = 0.5*(BinLocs(0:nBinLocs-2)+BinLocs(1:nBinLocs-1))

              ; simple linear interpoaltion
  xHstLocs    = linint1(MidBarVals,MidBarLocs, False,xHstMarkVals , 0)
  xHstLocs@long_name = "NDC coordinates of marker location(s)"

  return (xHstLocs)               
end
;---
               ; USER LEVEL FUNCTION; this could also be histogram_LINE or histogram_TICKMARK
undef("histogram_mark")      
procedure histogram_mark(wks[1]:graphic, hst[1]:graphic, xHstMarkLocs[*]:numeric, yHstMarkLocs[*]:numeric, pmHst[1]:logical)
local xHstLocs, pnred, nMarkX, nMarkY
begin
  nMarkX = dimsizes(xHstMarkLocs)
  nMarkY = dimsizes(yHstMarkLocs)
  if (nMarkX.ne.nMarkY) then
      print("histogram_mark: nMarkX must equal nMarkY:  nMarkX="+nMarkX+" nMarkY="+nMarkY)
      exit
  end if
           ; interpolate along the 'x' NDC coords
  xHstLocs = histogram_interp(hst@MidBarLocs, hst@BinLocs, xHstMarkLocs)

; local default resource settings

  pmres                = True         ;-- local
  pmres@gsClipOn       = False        ;-- allow to extend beyond the view port
  pmres@gsMarkerColor  = "magenta"    ;-- default marker color
  pmres@gsMarkerSizeF  =  0.0125      ;-- default; set size of marker
  pmres@gsMarkerThicknessF = 3.0      ;-- default is 1.0
 ;pmres@gsMarkerIndex  =   8          ;-- down triangle
  pmres@gsMarkerIndex  =   0          ;-- default; asterisk

; allow users to over-ride the local bootstrap
            
  if (pmHst .and. isatt(pmHst,"gsMarkerColor")) then
      pmres@gsMarkerColor  = pmHst@gsMarkerColor
  end if

  if (pmHst .and. isatt(pmHst,"gsMarkerSizeF")) then
      pmres@gsMarkerSizeF  = pmHst@gsMarkerSizeF
  end if

  if (pmHst .and. isatt(pmHst,"gsMarkerIndex")) then
      pmres@gsMarkerIndex  = pmHst@gsMarkerIndex
  end if

  if (pmHst .and. isatt(pmHst,"gsMarkerThicknessF")) then
      pmres@gsMarkerThicknessF  = pmHst@gsMarkerThicknessF
  end if

  if (pmHst .and. isatt(pmHst,"gsClipOn")) then   ; NCL default is True
      pmres@gsClipOn = pmHst@gsClipOn
  end if

;-- Add the marker(s)
  
  do n=0,nMarkX-1
     hst@$unique_string("hstMark_")$ = gsn_add_polymarker(wks, hst, xHstLocs(n), yHstMarkLocs(n) , pmres)  
  end do

;-- No return since this is a procedure

end

