load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin

  cdf_file = addfile("$NCARG_ROOT/lib/ncarg/data/cdf/contour.cdf","r")

  temp = cdf_file->T(0,0,:,:)               ; temperature
  Z    = cdf_file->Z(0,0,:,:)               ; geopotential height
  pres = cdf_file->Psl(0,:,:)               ; pressure at mean sea level
  lat  = cdf_file->lat                      ; latitude
  lon  = cdf_file->lon                      ; longitude

  temp = temp - 273.15                      ; Convert Kelvin -> Celsius
  pres = pres * 0.01                        ; Convert Pa -> mb
  temp@units = "(C)"                        ; Change units to reflect
  pres@units = "(mb)"                       ; conversion done.

  xwks = gsn_open_wks("x11","gsun02n")      ; Open an X11 workstation.

  plot = gsn_contour(xwks,temp,False)       ; Draw a contour plot.

;----------- Begin second plot -----------------------------------------

  resources                 = True       ; Indicate you want to set some
                                         ; resources.

  resources@cnMonoLineColor = False      ; Turn off the drawing of
                                         ; contours lines in one color.

  resources@tiMainString    = "Temperature (C)"  ; Create a title.

  plot = gsn_contour(xwks,temp,resources)  ; Draw a contour plot.

;----------- Begin third plot -----------------------------------------

  resources@cnFillOn          = True        ; Turn on contour line fill.
  resources@cnMonoFillPattern = False       ; Turn off using a single fill
                                            ; pattern.
  resources@cnMonoFillColor   = True
  resources@cnMonoLineColor   = True

  resources@tiXAxisString    = lon@long_name
  resources@tiYAxisString    = lat@long_name
  resources@sfXArray         = lon
  resources@sfYArray         = lat

  plot = gsn_contour(xwks,temp,resources)   ; Draw a contour plot.

;---------- Begin fourth plot ------------------------------------------

  resources@cnMonoFillPattern     = True     ; Turn solid fill back on.
  resources@cnMonoFillColor       = False    ; Use multiple colors.
  resources@cnLineLabelsOn        = False    ; Turn off line labels.
  resources@cnInfoLabelOn         = False    ; Turn off informational
                                             ; label.
  resources@cnLinesOn             = False    ; Turn off contour lines.

  resources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.
  resources@lbPerimOn             = False    ; Turn off perimeter on
                                             ; label bar.

  resources@tiMainString    = Z@long_name
  resources@tiMainFont      = 26
  resources@tiXAxisFont     = 26
  resources@tiYAxisFont     = 26

  plot = gsn_contour(xwks,Z,resources)    ; Draw a contour plot.

;---------- Begin fifth plot ------------------------------------------

  cmap = (/(/0.,0.,0./),(/1.,1.,1./),(/.1,.1,.1/),(/.15,.15,.15/),\
         (/.2,.2,.2/),(/.25,.25,.25/),(/.3,.3,.3/),(/.35,.35,.35/),\
         (/.4,.4,.4/),(/.45,.45,.45/),(/.5,.5,.5/),(/.55,.55,.55/),\
         (/.6,.6,.6/),(/.65,.65,.65/),(/.7,.7,.7/),(/.75,.75,.75/),\
         (/.8,.8,.8/),(/.85,.85,.85/)/)

  gsn_define_colormap(xwks,cmap)          ; Define a new color map.

  resources@tiMainString    = pres@long_name

  plot = gsn_contour(xwks,pres,resources) ; Draw a contour plot.

  print(temp(2:5,7:9))             ; Print subset of "temp" variable.

  print(temp!0)                    ; Print the dimension names for the
  print(temp!1)                    ; first two dimensions of T.
  print(temp@long_name)            ; Print "long_name" and "units"
  print(temp@units)                ; attributes of "T".
  print(temp&lat)                  ; Print coordinate variables "lat"
  print(temp&lon)                  ; and "lon".

  ascii_file = "data.asc"                 ; Create name of ASCII file.
  system("/bin/rm -f " + ascii_file)      ; Remove ASCII file.
  asciiwrite(ascii_file,temp(7:3:2,0:4))  ; Write part of temp to ASCII 
                                          ; file.
  delete(plot)       ; Clean up.
  delete(temp)
  delete(resources)
end
