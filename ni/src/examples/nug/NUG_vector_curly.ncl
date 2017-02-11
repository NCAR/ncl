;---------------------------------------------------
;  NCL User Guide Example:   NUG_vector_curly.ncl
;
;  KMF 30.10.14
;---------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  diri = "./"
  fili = "rectilinear_grid_2D.nc"

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
  
  f    =  addfile(diri+fili, "r")
  u    =  f->u10(0,:,:)         	  	    ;-- first time step
  v    =  f->v10(0,:,:)         		    ;-- first time step

;-- define the workstation (graphic will be written to a file)

  wks =  gsn_open_wks("png","NUG_vector_curly")

;-- set plot resources

  vres =  True
  vres@gsnMaximize          =  True
  vres@tiMainString         = "NCL Doc Example:  vector curly"
  vres@vcMinFracLengthF     =  1.0		    ;-- length of min vector as fraction of reference vector
  vres@vcRefMagnitudeF      =  3.0		    ;-- make vectors larger
  vres@vcRefLengthF         =  0.045		;-- ref vec length
  vres@vcGlyphStyle         = "CurlyVector"	;-- turn on curly vectors
  vres@vcMinDistanceF       =  0.01		    ;-- thin out vectors

;-- draw the vectors

  plot = gsn_csm_vector_map(wks,u,v,vres)

end
