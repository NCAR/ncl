;-----------------------------------------------------------------
;  NCL User Guide Example:   NCL_read_Binary_GrADS.ncl
;
;  KMF
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"  
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

  diri = "./"
  fili = "ps_grads_model.dat"
  
;-- read data
  setfileoption("bin", "ReadByteOrder", "LittleEndian")
  ps = fbindirread (diri+fili, 0, (/ 5, 46, 73 /), "float")

  print ("-- read GrADS binary data -- done")

  ps@long_name = "Surface Pressure"
  ps@units     = "Pa"
                
  printVarSummary(ps)
  print ("min(ps)="+min(ps))
  print ("max(ps)="+max(ps))

end
