load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsun_scripts.ncl"

begin

  cdf_file = addfile("$NCARG_ROOT/lib/ncarg/data/cdf/contour.cdf","r")

  temp = cdf_file->T(0,0,:,:) - 273.15      ; Convert temperature from 
                                            ; kelvin to celsius.
  Z    = cdf_file->Z(0,0,:,:)               ; geopotential height
  pres = cdf_file->Psl(0,:,:)               ; pressure at mean sea level
  pres = pres * 0.01                        ; Convert from Pa to mb.

  lat  = cdf_file->lat                      ; Get latitude values.
  lon  = cdf_file->lon                      ; Get longitude values.

  xwks = ncl_open_wks("ncgm","gsun02n")    ; Open an X11 workstation.

  plot = ncl_contour(xwks,temp,False)       ; Draw a contour plot.

; Begin second plot.

  resources                 = True       ; Indicate we want to set some
                                         ; resources.

  resources@cnMonoLineColor = False      ; Turn off the drawing of
                                         ; contours lines in one color.

  resources@tiMainString    = "Temperature (C)"  ; Create a title.

  plot = ncl_contour(xwks,temp,resources)  ; Draw a contour plot.

; Begin third plot.

  resources@cnFillOn          = True        ; Turn on contour line fill.
  resources@cnMonoFillPattern = False       ; Turn off using a single fill
                                            ; pattern.
  resources@cnMonoFillColor   = True
  resources@cnMonoLineColor   = True

  resources@tiXAxisString    = lon@long_name
  resources@tiYAxisString    = lat@long_name
  resources@sfXArray         = lon
  resources@sfYArray         = lat

  plot = ncl_contour(xwks,temp,resources)   ; Draw a contour plot.

; Begin fourth plot.

  resources@cnMonoFillPattern     = True     ; Turn solid fill back on.
  resources@cnMonoFillColor       = False    ; Use multiple colors.
  resources@cnLineLabelsOn        = False    ; Turn off line labels.
  resources@cnInfoLabelOn         = False    ; Turn off informational
                                             ; label.
  resources@cnLinesOn             = False    ; Turn off contour lines.

  resources@pmLabelBarDisplayMode = "always" ; Turn on labelbar.
  resources@lbPerimOn             = False    ; Turn off perimeter on
                                             ; labelbar.

  resources@tiMainString    = Z@long_name
  resources@tiMainFont      = 26
  resources@tiXAxisFont     = 26
  resources@tiYAxisFont     = 26
  plot = ncl_contour(xwks,Z,resources)    ; Draw a contour plot.

; Begin fifth plot.

  cmap = (/(/0.,0.,0./),(/1.,1.,1./),(/.1,.1,.1/),(/.15,.15,.15/),\
         (/.2,.2,.2/),(/.25,.25,.25/),(/.3,.3,.3/),(/.35,.35,.35/),\
         (/.4,.4,.4/),(/.45,.45,.45/),(/.5,.5,.5/),(/.55,.55,.55/),\
         (/.6,.6,.6/),(/.65,.65,.65/),(/.7,.7,.7/),(/.75,.75,.75/),\
         (/.8,.8,.8/),(/.85,.85,.85/)/)

  ncl_define_colormap(xwks,cmap)          ; Define a new color map.

  resources@tiMainString    = pres@long_name
  plot = ncl_contour(xwks,pres,resources) ; Draw a contour plot.

  print(temp(2:5,7:9))                    ; Print subset of "temp"
                                          ; variable.

  print(cdf_file&level)                   ; Print coordinate variable
                                          ; "level".
  print(cdf_file->T@long_name)            ; Print "long_name" and "units"
  print(cdf_file->T@units)                ; attributes of "T".


  ascii_file = "data.asc"                 ; Create name of ASCII file.
  system("/bin/rm -f " + ascii_file)      ; Remove ASCII file.
  asciiwrite(ascii_file,temp(7:3:2,0:4))  ; Write part of temp to ASCII 
                                          ; file.
end
