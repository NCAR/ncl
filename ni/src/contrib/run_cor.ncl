undef ("run_cor")
function run_cor(x[*]:numeric,y[*]:numeric,time[*]:numeric,wSize[1]:integer)
;**********************************************************************
; User donated: Sabeerali(sebi) <sabeerl@gmail.com>
;**********************************************************************
; x,y : numeric array of 1D [eg: x(time) and y(time)]
; time: numeric array of 1D that represent time coordinate of x or y
;       time coordinate must have recognized by ut_calendar
; wSize: specifies window size.[e.g 11 , 15 , 21 , etc]
;
; x,y,time must have same size
;
;Sample usage:  x(time) and y(time).
;    cor_11=run_cor(x,y,time,11)
;    cor_15=run_cor(x,y,time,15)
;    cor_21=run_cor(x,y,time,21)
;************************************************************************
local aa,bb,i,cor,date,utc_date,year,k,N
begin
  N   = dimsizes(x)
  if (N.ne.dimsizes(y) .or. N.ne.dimsizes(time)) then
      print("run_cor: input array are different sizes: fatal")
      exit
  end if
  
  if (typeof(x).eq."double" .or. typeof(y).eq."double") then
      cor = new((/(N-(wSize-1))/),"double")
  else
      cor = new((/(N-(wSize-1))/),"float")
  end if

  date = new((/(N-(wSize-1))/),integer)
  utc_date = ut_calendar(time,0)
  year     = floattointeger(utc_date(:,0))

  k = (wSize-1)/2
  do j=0,N-(wSize)
     date(j)=year(k)+j
  end do

  do i=0,(N-wSize)
     aa     = x(i:i+(wSize-1))
     bb     = y(i:i+(wSize-1))
     cor(i) = escorc(aa,bb)
  end do

  cor!0="time"
  cor&time=date
  cor@longName = "running correlation"

  return(cor)
end

