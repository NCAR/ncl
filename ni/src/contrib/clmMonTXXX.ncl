undef("clmMonTXXX")
function clmMonTXXX(x:numeric, min_years:integer)
local x_dims, x_rank, \
      num_time, num_lev, num_lat, num_lon, num_months, \
      monAvg, monNum, \
      month, i

begin

; Define the number of months for the climatology calculation.
 num_months = 12

; Get the input array's dimension sizes and rank.
 x_dims = dimsizes(x)
 x_rank = dimsizes(x_dims)

; This function will work only on arrays up to and including four dimensions.
 if(x_rank.gt.4)then
   print("clmMonTXXX: Error: Expecting at most a 4D array!")
 end if

; Get the number of time steps.
 num_time = x_dims(0)

; If the number of time steps is not a multiple of 12, then emit an error.
 if(num_time%12.ne.0)then
   print("clmMonTXXX: Error: The time dimension's length must be a multiple of 12!")
   exit
 end if

; One-dimensional time series.
 if(x_rank.eq.1)then
   monAvg = new(num_months, typeof(x), getFillValue(x))
   monNum = new(num_months, integer)
   do month = 0, num_months - 1
     monAvg(month) = dim_avg_n(x(month:num_time-1:num_months), 0)
     monNum(month) = dim_sum_n(where(ismissing(x(month:num_time-1:num_months)), 0, 1), 0)
   end do
 end if

; Two-dimensional series.
 if(x_rank.eq.2)then
   num_points = x_dims(1)
   monAvg = new((/num_months, num_points/), typeof(x), getFillValue(x))
   monNum = new((/num_months, num_points/), integer)
   do month = 0, num_months - 1
     monAvg(month,:) = dim_avg_n(x(month:num_time-1:num_months,:), 0)
     monNum(month,:) = dim_sum_n(where(ismissing(x(month:num_time-1:num_months,:)), 0, 1), 0)
   end do
 end if

; Three-dimensional series.
 if(x_rank.eq.3)then
   num_lat  = x_dims(1)
   num_lon  = x_dims(2)
   monAvg = new((/num_months, num_lat, num_lon/), typeof(x), getFillValue(x))
   monNum = new((/num_months, num_lat, num_lon/), integer)
   do month = 0, num_months - 1
     monAvg(month,:,:) = dim_avg_n(x(month:num_time-1:num_months,:,:), 0)
     monNum(month,:,:) = dim_sum_n(where(ismissing(x(month:num_time-1:num_months,:,:)), 0, 1), 0)
   end do
 end if

; Four-dimensional series.
 if(x_rank.eq.4)then
   num_lev  = x_dims(1)
   num_lat  = x_dims(2)
   num_lon  = x_dims(3)
   monAvg = new((/num_months, num_lev, num_lat, num_lon/), typeof(x), getFillValue(x))
   monNum = new((/num_months, num_lev, num_lat, num_lon/), integer)
   do month = 0, num_months - 1
     monAvg(month,:,:,:) = dim_avg_n(x(month:num_time-1:num_months,:,:,:), 0)
     monNum(month,:,:,:) = dim_sum_n(where(ismissing(x(month:num_time-1:num_months,:,:,:)), 0, 1), 0)
   end do
 end if

; Mask out the monthly means where the climatology couldn't be calculated because of insufficient data .
 monAvg = where(monNum.ge.min_years, monAvg, getFillValue(monAvg))

; Copy the input variable's attributes.
 copy_VarAtts(x, monAvg)

; Add some meta of our own.
 monAvg@time_op_ncl = "Climatology: "+ (num_time/num_months) +" years"
 monAvg@info        = "function clmMonTXXX: chad_util.ncl"

; Copy the dimension names and coordinate variables.
 do i = 1, x_rank - 1
   if(.not.ismissing(x!i))then
     monAvg!i = x!i
     if(iscoord(x,x!i))then
       monAvg&$monAvg!i$ = x&$x!i$
     end if
   end if
 end  do

; Make the time variable "month".
 monAvg!0     = "month"
 monAvg&month = ispan(0,num_months-1,1)

; Copy the array with the number of observations that went into each monthly mean for each point or grid cell.
 monAvg@monNum = monNum

; There might be meta data which doesn't make sense anymore, like valid_min, valid_max, etc. Update it if necessary.
 if(isatt(monAvg, "valid_min"))then
   monAvg@valid_min = min(monAvg)
 end if

 if(isatt(monAvg, "valid_max"))then
   monAvg@valid_max = max(monAvg)
 end if

 if(isatt(monAvg, "valid_range"))then
   monAvg@valid_range = (/ min(monAvg), max(monAvg) /)
 end if

; Done
 return(monAvg)

end
