;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_read_Binary_GrADS.ncl
;
;  KMF
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"  
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

  diri = "$NCARG_ROOT/lib/ncarg/data/nug/"
  fili = "ps_grads_model.dat"

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
