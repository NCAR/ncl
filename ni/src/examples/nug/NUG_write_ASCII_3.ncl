;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_write_ASCII_3.ncl
;
;  KMF
;-----------------------------------------------------------------
; This load command is not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
;-- Generate some dummy integer and float data.
   npts = 100
   i    = ispan(1,npts,1)
   j    = generate_unique_indices(npts)
   k    = generate_unique_indices(npts)
   x    = random_uniform(-10,10,npts)
   y    = random_uniform(0,1000.,npts)

   lines = "string_" + sprinti("%03i", j) + " " + \
           sprintf("%8.2f",x) +  " " + \
           sprinti("%4.0i", i) + " " + \
           sprintf("%8.1f",y) +  " " + \
           "    string_" + sprinti("%03i", k)

;---Write to a file
  asciiwrite("file3.txt",lines)
  
end
