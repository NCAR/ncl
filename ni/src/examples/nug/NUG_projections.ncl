;---------------------------------------------------
;  NCL User Guide Example:   NCL_projections.ncl
;
;  KMF 30.10.14
;---------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
   diri  = "./"
   file1 = "rectilinear_grid_2D.nc"
  
;-- read the data
   f   =  addfile(diri+file1,"r")
   var =  f->tsurf(0,:,:)

  proj = (/"Aitoff", "AzimuthalEquidistant", "CylindricalEqualArea",\
           "CylindricalEquidistant", "Gnomonic", "Hammer",\
           "LambertConformal", "LambertEqualArea", "Mercator",\
           "Mollweide", "Orthographic", "PseudoMollweide",\
           "Robinson", "RotatedMercator", "Satellite",\
           "Stereographic", "WinkelTripel" /)

;-- define the workstation (graphic will be written to a file)
   wks      =  gsn_open_wks("png","plot_projections")
   gsn_define_colormap(wks,"rainbow")
   
;-- set plot resources
   res                  = True             ; plot mods desired
   res@gsnMaximize      = True             ; maxmize plot in frame

   do i=0,16
      
      res@mpProjection      = proj(i)
      res@mpGridAndLimbOn   = True
      res@tiMainString     = "Projection: " + proj(i)

;-- draw the contour map
   plot = gsn_csm_contour_map(wks,var,res)
   end do

end
