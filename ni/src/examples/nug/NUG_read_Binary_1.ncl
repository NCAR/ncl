;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_read_Binary_1.ncl
;
;  KMF
;-----------------------------------------------------------------
begin

;-- path and file name
  diri  = "./"
  fname = "topo.bin"

;-- set byte order
  setfileoption("bin","ReadByteOrder","BigEndian")
  
;-- read binary file
  topo =  fbindirread(diri+fname,0,(/293,343/),"float")

;-- set some attributes
  topo@units     = "m"
  topo@long_name = "topography"

;-- print information
  printVarSummary(topo)

  print("Minimum value: "+min(topo))
  print("Maximum value: "+max(topo))
  
end
