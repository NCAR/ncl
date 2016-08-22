;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_shapefile_plot_data.ncl
; 
;  This example requires the "DEU_adm1.shp" shapefile, which you
;  can download from:
;
;     http://www.gadm.org/country
;
;     (Select "Germany" from the list).
;
;  It also requires the RANGS database, see: 
;
;     http://www.ncl.ucar.edu/Document/Graphics/rangs.shtml
;  
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

undef("file_ERROR")
procedure file_ERROR(name)
begin
   print("")
   print("You don't have the necessary data for this script. You can download it from:​")
   print("")
   print("http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+name)
   print("")
   print("or use the wget command:")
   print("")
   print("wget http://www.ncl.ucar.edu/Document/Manuals/NCL_User_Guide/Data/"+name)
   print("")
   exit
end

undef("shp_file_ERROR")
procedure shp_file_ERROR(name)
begin
   print("")
   print("You don't have the necessary "+name+" shapefile data for this script. You can download it from:​")
   print("")
   print("http://www.gadm.org/country/")
   print("")
   print("Select 'Germany' from the country list and 'Shapefile' for the file format.")
   print("")
   exit
end

  
begin
   diri    = "./"
   shpname = "DEU_adm/DEU_adm1.shp"
   
;-- read data to display
   fili = "rectilinear_grid_2D.nc"
   
   if (.not. fileexists(diri+fili)) then
      file_ERROR(fili)
   end if
   if (.not. fileexists(diri+shpname)) then
      shp_file_ERROR(shpname)
   end if
  
  
   g    =  addfile(diri+fili,"r")
   var  =  g->tsurf(0,:,:)

;-- open workstation
   wks = gsn_open_wks("png","plot_shapefile_plus_data")

;-- set resources for the map
   res                     =  True
   res@gsnDraw             =  False                 ;-- don't draw the plot
   res@gsnFrame            =  False                 ;-- don't advance frame yet
   res@gsnMaximize         =  True                  ;-- maximize plot in frame
   res@gsnSpreadColors     =  True                  ;-- full color map
   res@gsnSpreadColorStart =  18                    ;-- start at color 18
   res@gsnSpreadColorEnd   =  -3


   res@cnFillOn            =  True
   res@cnLinesOn           =  False
   res@cnSmoothingOn       =  True
   res@cnLevelSelectionMode = "ManualLevels"
   res@cnMinLevelValF      =  270.0
   res@cnMaxLevelValF      =  285.0
   res@cnLevelSpacingF     =    1.0

;-- select coordinates for Germany
   res@mpFillOn            =  False
   res@mpLimitMode         = "Corners"
   res@mpLeftCornerLatF    =  47.
   res@mpRightCornerLatF   =  55.
   res@mpLeftCornerLonF    =  5.
   res@mpRightCornerLonF   =  16.
   res@mpDataBaseVersion   = "MediumRes"
   res@mpDataResolution    = "Medium"
   res@mpProjection        = "Mercator"

   res@tiMainString        = "NCL Doc Example: Using Shapefile for border lines"
   res@tiMainFontHeightF   =  0.015

;-- generate map, but don't draw it
   plot = gsn_csm_contour_map(wks,var,res)

;-- set resources for the polylines
   plres                   =  True
   plres@gsLineColor       = "black"

   id = gsn_add_shapefile_polylines(wks,plot,shpname,plres)

;-- draw the plot
   draw(plot)
   frame(wks)

end
