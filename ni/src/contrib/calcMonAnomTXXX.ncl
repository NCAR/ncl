undef("calcMonAnomTXXX")
function calcMonAnomTXXX(x:numeric, x_clm:numeric)
local num_months, num_time, \
      x_dims, x_rank, x_clm_dims, x_clm_rank, \
      xAnom

begin

; Define the number of months for the climatology calculation.
 num_months = 12

; Get the input arrays' dimensions and rank.
 x_dims = dimsizes(x)
 x_rank = dimsizes(x_dims)
 
 x_clm_dims = dimsizes(x_clm)
 x_clm_rank = dimsizes(x_clm_dims)

; This function will work only on arrays up to and including four dimensions.
 if(x_rank.gt.4.or.x_rank.gt.4)then
   print("calcMonAnomTXXX: Error: Expecting at most a 4D array!")
 end if

; Check if the non-time dimensions are conformable.
 if(x_rank.ne.1)then
   if(.not.all(x_dims(1:).eq.x_clm_dims(1:)))then
     print("calcMonAnomTXXX: Error: The input arrays' rightmost dimensions must be the same!")
     exit
   end if
 end if

; Get the number of time steps and copy the input array into the anomaly variable to preserve meta data.
 num_time  = x_dims(0)
 xAnom = x

; One-dimensional series
 if(x_rank.eq.1)then
   do year = 0, num_time - 1, num_months
     xAnom(year:year+num_months-1) = (/ x(year:year+num_months-1) - x_clm /)
   end do
 end if

; Two-dimensional series
 if(x_rank.eq.2)then
   do year = 0, num_time - 1, num_months
     xAnom(year:year+num_months-1,:) = (/ x(year:year+num_months-1,:) - x_clm /)
   end do
 end if

; Three-dimensional series
 if(x_rank.eq.3)then
   do year = 0, num_time - 1, num_months
     xAnom(year:year+num_months-1,:,:) = (/ x(year:year+num_months-1,:,:) - x_clm /)
   end do
 end if

; Four dimensional series
 if(x_rank.eq.4)then
   do year = 0, num_time - 1, num_months
     xAnom(year:year+num_months-1,:,:,:) = (/ x(year:year+num_months-1,:,:,:) - x_clm /)
   end do
 end if

; Add an attribute to show where the output variable came from. Change "chad_util.ncl" if you like since you don't have that file.
 xAnom@anomaly_op_ncl  = "Anomalies from Annual Cycle: calcMonAnomTXXX: chad_util.ncl" 

; There might be meta data which doesn't make sense anymore, like valid_min, valid_max, etc. Update it if necessary.
 if(isatt(xAnom, "valid_min"))then
   xAnom@valid_min = min(xAnom)
 end if

 if(isatt(xAnom, "valid_max"))then
   xAnom@valid_max = max(xAnom)
 end if

 if(isatt(xAnom, "valid_range"))then
   xAnom@valid_range = (/ min(xAnom), max(xAnom) /)
 end if

; Done
 return(xAnom)

end

