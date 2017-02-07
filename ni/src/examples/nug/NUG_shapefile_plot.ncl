;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_shapefile_plot.ncl
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

begin

;-- Check for required DEU shapefile
  diri = "./"
  fili = "DEU_adm/DEU_adm1.shp"

  if (.not. fileexists(diri+fili)) then
     print("")
     print("You don't have the necessary "+fili+" shapefile data for this script. You can download it from:​")
     print("")
     print("http://www.gadm.org/country/")
     print("")
     print("Select 'Germany' from the country list and 'Shapefile' for the file format.")
     print("")
     exit
  end if

;-- Check for required RANGS database
  rangs_dir  = ncargpath("rangs")
  rangs_file = "gshhs(0).rim"
  if(.not.fileexists(rangs_dir + "/" + rangs_file)) then
     print("")
     print("You don't have the necessary high-res map database for this script.")
     print("See this URL for information on how to download it.")
     print("")
     print("http://www.ncl.ucar.edu/Document/Graphics/rangs.shtml")
     print("")
     print("You can also set mpDataBaseVersion to MediumRes.")
     exit
  end if
  
;-- open workstation
   wks = gsn_open_wks("png","NUG_shapefile_plot")

;-- set resources for the map
   res                     =  True
   res@gsnDraw             =  False                 ;-- don't draw the plot
   res@gsnFrame            =  False                 ;-- don't advance frame yet
   res@gsnMaximize         =  True                  ;-- maximize plot in frame
   res@mpDataBaseVersion   = "HighRes"              ;-- use "MediumRes" if "HighRes" database not available
   res@mpDataResolution    = "Medium"
   res@mpProjection        = "Mercator"             ;-- change projection

;-- select coordinates for Germany
   res@mpLimitMode         = "Corners"
   res@mpLeftCornerLatF    =  47.
   res@mpRightCornerLatF   =  55.
   res@mpLeftCornerLonF    =  5.
   res@mpRightCornerLonF   =  16.

   res@tiMainString        = "NCL Doc Example: Shapefile plot"
   res@tiMainFontHeightF   =  0.015

;-- generate map, but don't draw it
   plot = gsn_csm_map(wks,res)                      ;-- draw map, but don't advance frame.

;-- set resources for the polylines
   plres                   =  True
   plres@gsLineColor       = "red"

   id = gsn_add_shapefile_polylines(wks,plot,diri+fili,plres)

;-- draw the plot
   draw(plot)
   frame(wks)

end
