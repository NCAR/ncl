;-----------------------------------------------------------------
;  NCL User Guide Example:  NCL_curvilinear_grid.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
   diri      = "./"
   fili      = "CNTASN_1m_200103_grid_T_curvilinear_grid.nc"
   f         =  addfile(diri+fili,"r")
   var       =  f->votemper(0,0,:,:)
   var@lat2d =  f->nav_lat
   var@lon2d =  f->nav_lon

  
;-- define the workstation (plot type and name)
   wks = gsn_open_wks("png","plot_curvilinear_grid")

;-- set resources
   res                       =  True
   res@gsnAddCyclic          =  False      ;-- data are not global, don't add lon cyclic point
   res@gsnMaximize           =  True

   res@cnFillOn              =  True            ;-- turn on contour fill
   res@cnMinLevelValF        =   5.
   res@cnMinLevelValF        =  25.
   res@cnLevelSpacingF       =   0.5
   res@cnLinesOn             =  False
   res@tiMainString          = "NCL UG Example: Curvilinear grid (NEMO)"  ;-- title string
   res@tiMainFontHeightF     =  0.02

   res@pmTickMarkDisplayMode = "Always"

;---Zoom in on map
   res@mpMinLatF             =  min(var@lat2d)
   res@mpMaxLatF             =  max(var@lat2d)
   res@mpMinLonF             =  min(var@lon2d)
   res@mpMaxLonF             =  max(var@lon2d)

;-- draw the contour map
   plot = gsn_csm_contour_map(wks,var,res)
end
