;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_polar_NH.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

;---------------------------------------------------------------------
;-- procedure : polar_map_circle
;-- plot a circle around the polar map using width wsize and color col
;---------------------------------------------------------------------
undef("polar_map_circle")
procedure polar_map_circle(wks,plot:graphic,wsize:integer,col:string,offset:numeric)
local degrad,degrees,xcos,xsin,xcenter,ycenter,radius,xc,yc
begin
  getvalues plot     ;-- get viewport coordinates
    "vpXF"      : x
    "vpYF"      : y
    "vpWidthF"  : w
    "vpHeightF" : h
  end getvalues

  degrad  = 0.017453292519943
  degrees = ispan(0,360,1)
  xcos    = cos(degrad * degrees)
  xsin    = sin(degrad * degrees)
  xcenter = w/2 + x
  ycenter = h/2 + (y-h) 
  radius  = w/2 + offset
  xc      = xcenter + (radius * xcos)
  yc      = ycenter + (radius * xsin) 

;-- set resources for circle and plot
  lnres                  =  True
  lnres@gsLineColor      =  col
  lnres@gsLineThicknessF =  wsize
  gsn_polyline_ndc(wks,xc,yc,lnres)
end

;-----------------------------------------------------------
;-- main program
;-----------------------------------------------------------
begin

;-- read the data and define var

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
  u    =  f->tsurf(0,:,:)

;-- define the workstation (plot type and name)
  wks = gsn_open_wks("png" ,"NUG_polar_NH")
  gsn_define_colormap(wks,"ncl_default")

  res                          =  True
  res@gsnDraw                  =  False           ;-- don't draw the plot
  res@gsnFrame                 =  False           ;-- don't advance the frame
  res@gsnPolar                 = "NH"             ;-- show Northern Hemisphere
  res@gsnPolarLabelSpacing     =  15              ;-- grid spacing
  res@gsnPolarLabelDistance    =  1.1             ;-- default is 1.04
  res@gsnPolarLabelFontHeightF =  0.015
  res@gsnPolarLabelFont        = "helvetica-bold"
  res@gsnSpreadColorStart      =  14              ;-- color index start
  res@gsnSpreadColorEnd        =  -8              ;-- color index end

  res@cnFillOn                 =  True

  res@tiMainString             = "NCL Doc Example: Polar Plot (NH)"

;-- draw the plot
  plot = gsn_csm_contour_map_polar(wks,u,res)    ;-- from contributed.ncl library
  draw(plot)                                     ;-- draw plot

;-- draw a circle around the map of plot: wsize=10, col="black", offset=0
;-- use function polar_map_circle from above
  polar_map_circle(wks, plot, 7, "black", 0)
  
;-- advance the frame
  frame(wks)

end


