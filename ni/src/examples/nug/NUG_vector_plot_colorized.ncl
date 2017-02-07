;---------------------------------------------------
;  NCL User Guide Example:   NUG_vector_plot_colorized.ncl
;
;  KMF 30.10.14
;---------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  diri  = "./"
  fili  = "rectilinear_grid_2D.nc"

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
  
  uname = "u10"
  vname = "v10"
  tname = "tsurf"

;-- read the data
  f   =  addfile(diri+fili,"r")				    ;-- open file with read access
  u   =  f->$uname$(0,:,:)         			    ;-- first time step
  v   =  f->$vname$(0,:,:)         			    ;-- first time step
  t   =  f->$tname$(0,:,:)         			    ;-- first time step

;-- define the workstation (graphic will be written to a file)

  wks =  gsn_open_wks("png","NUG_vector_plot_colorized")

;-- set plot resources

  res                      =  True
  res@gsnMaximize          =  True			    ;-- maxmize plot in frame
  res@vcMinFracLengthF     =  1.0			    ;-- length of min vector as fraction of reference vector.
  res@vcRefMagnitudeF      =  3.0			    ;-- make vectors larger
  res@vcRefLengthF         =  0.045			    ;-- ref vec length
  res@vcGlyphStyle         = "CurlyVector"		;-- turn on curly vectors
  res@vcMinDistanceF       =  0.01			    ;-- thin out vectors
  res@tiMainString         = "NCL Doc Example:  vectors colorized by temperature" ;-- set title string
  res@vcLevelPalette       = "ncl_default"     		    ;-- choose color map

;-- draw the vectors

  plot = gsn_csm_vector_scalar_map(wks,u,v,t,res)

end
