;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_read_Binary_1.ncl
;
;  KMF
;-----------------------------------------------------------------
begin

;-- path and file name
  diri  = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "topo.bin"

  if (.not. fileexists(diri+fili)) then
     print("")
     print("You don't have the necessary data for this script. You can download it from:​")
     print("")
     print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     print("or use the wget command:")
     print("")
     print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+fili)
     print("")
     exit
  end if
  
;-- set byte order
  setfileoption("bin","ReadByteOrder","BigEndian")
  
;-- read binary file
  topo =  fbindirread(diri+fili,0,(/293,343/),"float")

;-- set some attributes
  topo@units     = "m"
  topo@long_name = "topography"

;-- print information
  printVarSummary(topo)

  print("Minimum value: "+min(topo))
  print("Maximum value: "+max(topo))
  
end
