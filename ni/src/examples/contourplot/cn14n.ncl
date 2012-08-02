;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn14n.ncl
;
;   Author:     Bob Lackman
;               National Center for Atmospheric Research
;               PO 3000, Boulder, Colorado
;
;   Date:       Fri Feb 16, 1996
;
;   Description:    Reads 2 netCDF files, U and V winds, and produces
;                   a contour plot of the wind speed.
;
;   Notes:      - A semicolon on a line turns the rest of the line
;                 into a comment.
;

begin           ; Every ncl script begins with the statement "begin".
;
;------------    Read some data to be plotted.  -----------------
;
; Read NCL created NetCDF file containing U and V wind components.
;
path = ncargpath("data")
file1 = addfile(path+"/cdf/941110_UV.cdf","r")     ; read U and V
;print (file1)                                ; look at the file contents
u = file1->u
v = file1->v
spd = sqrt(u*u+v*v)             ; compute the wind speed
;
;   ------------- Define a color map  ---------------------
;
; Generate a new color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
ncolors = 16
cmap = (/ (/1.00,1.00,1.00/), \  ; white
         (/0.00,0.00,0.00/), \  ; black
         (/0.00,0.00,1.00/), \  ; blue
         (/0.20,0.56,0.80/), \  ; sky blue
         (/0.00,1.00,1.00/), \  ; cyan
         (/0.50,0.00,1.00/), \  ; blue magenta
         (/0.00,1.00,0.00/), \  ; green
         (/0.14,0.56,0.14/), \  ; forest green
         (/1.00,1.00,0.00/), \  ; yellow
         (/1.00,0.50,0.00/), \  ; orange
         (/1.00,0.00,1.00/), \ ; magenta
         (/1.00,0.00,0.00/), \ ; red
         (/0.65,0.16,0.16/), \ ; brown
         (/0.86,0.58,0.44/), \ ; tan
         (/0.66,0.66,0.66/), \ ; light gray
         (/0.40,0.40,0.40/) /) ; dark gray
;
; Change the output to an X11 window
;
wks_type = "x11"
;                 ncl supports if-then and if-then-else logic.
;
  if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
    wks = create "cn14Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "./cn14n.ncgm"      ; name the ncgm
      "wkColorMap" : cmap                ; assign the new color map
    end create
  end if
  if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
    wks = create "cn14Work" windowWorkstationClass defaultapp
      "wkColorMap" : cmap                     ; assign the new color map
    end create
  end if
  if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
    wks = create "cn14Work" psWorkstationClass defaultapp
      "wkPSFileName" : "./cn14n.ps"      ; name the PostScript file
      "wkColorMap" : cmap              ; assign the new color map
    end create
  end if
  if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
    wks = create "cn14Work" pdfWorkstationClass defaultapp
      "wkPDFFileName" : "./cn14n.pdf"      ; name the PDF file
      "wkColorMap" : cmap              ; assign the new color map
    end create
  end if
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wks = create "cn14Work" documentWorkstationClass defaultapp
      "wkFileName" : "./cn14n"      ; name the PDF file
      "wkFormat" : wks_type
      "wkColorMap" : cmap              ; assign the new color map
    end create
  end if
  if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wks = create "cn14Work" imageWorkstationClass defaultapp
      "wkFileName" : "./cn14n"      ; name the PDF file
      "wkFormat" : wks_type
      "wkColorMap" : cmap              ; assign the new color map
    end create
  end if
;
;    Create the data object needed by ContourPlot.
;
cndata = create "cndata" scalarFieldClass defaultapp
"sfDataArray" : spd                             ; the data to plot is spd
"sfXCStartV" :  file1->lon(0)                   ; longitude range of file1
"sfXCEndV"   :  file1->lon(filevardimsizes(file1,"lon")-1)
"sfYCStartV" :  file1->lat(0)                   ; latitude range of file1
"sfYCEndV"   :  file1->lat(filevardimsizes(file1,"lat")-1)
end create
;
; Create the ContourPlot object and assign data to it
;
cn_plot = create "cn_plot" contourPlotClass wks
;
; Let's set a bunch of resources this time
;
"vpXF" : .10                               ; define the viewport location
"vpYF" : .80
"vpWidthF" : .80                           ; and size
"vpHeightF" : .40
 "cnScalarFieldData" : cndata              ; assign the data object
 "cnFillOn" : True                       ; turn on contour fills
 "cnFillColors": (/15,14,2,3,4,7,6,8,9,10,11,13,12,0/)  ; contour fill colors
;
; blk,drk gry,lt gry,blue,sky bl,cyan,frst grn,grn,yel,or,mag,red,tan,brwn,wh
;
; Set the range and spacing of the contour levels.
;
 "cnLevelSelectionMode" : "ManualLevels"
 "cnMinLevelValF" : 2.0
 "cnMaxLevelValF" : 28.0
 "cnLevelSpacingF" : 3.0
;
; Turn off the contour lines and labels
;
 "cnLinesOn" : False
 "cnLineLabelsOn" : False
 "cnHighLabelsOn" : False
 "cnLowLabelsOn" : False
 "cnInfoLabelOn" : False
;
;  Set resources of the inherited Title objects
;
 "tiXAxisString"       : "Longitude"
 "tiXAxisFont"         : "helvetica"
 "tiXAxisFontHeightF"  : .02
 "tiYAxisString"       : "Latitude"
 "tiYAxisFont"         : "helvetica"
 "tiYAxisFontHeightF"  : .02
 "tiMainString"        : "Wind Speed"
 "tiMainFont"          : "helvetica"
 "tiMainFontHeightF"   : .03
;
;  Set resources of the inherited TickMark object
;
 "tmXBMode" : "EXPLICIT"
 "tmXBValues" : (/-180.,-120.,-60.,0,60,120,180/)
 "tmXBLabels" : (/"180W","120W","60W","0","60E","120E","180E"/)
 "tmYLMode" : "EXPLICIT"
 "tmYLValues" : (/-90, -60,-30,0,30,60,90/)
 "tmYLLabels" : (/"90S","60S","30S","EQ","30N","60N","90N"/)
 "tmXBLabelFontHeightF" :  .016         ; set x axis scale size
 "tmYLLabelFontHeightF" :  .016         ; set y axis scale size
 "tmXBLabelFont"        : "times-roman"
 "tmYLLabelFont"        : "times-roman"
 "tmXBMinorOn"          : False
 "tmYLMinorOn"          : False
;
 "pmLabelBarDisplayMode" : "ALWAYS"     ; turn on the overlay labelbar
 "pmLabelBarHeightF" : .15              ; set the labelbar height
 "pmLabelBarWidthF" : .8                ;                  width
 "pmLabelBarSide" : "bottom"            ; location and orientation
;
 "lbOrientation" : "horizontal"
 "lbBoxLinesOn" : 0                     ; no lines between labelbar colors
 "lbLabelsOn" : True                  ; turn on the labels
 "lbPerimOn" : False                  ; turn off the box around the labelbar
 "lbAutoManage"       : False         ; allows us to control label sizes
 "lbLabelFontHeightF" : 0.015           ; set the label size
 "lbLabelFont"        : "times-roman"   ;               font
end create
;
map = create "map" mapPlotClass wks
   "vpXF" : .10                         ; define the viewport location
   "vpYF" : .80
   "vpWidthF" : .80                     ; and size
   "vpHeightF" : .40
   "mpFillOn"  :  True                ; fill the map
   "mpLandFillColor"        : "Black"
   "mpOceanFillColor"       : "Transparent"
   "mpInlandWaterFillColor" : "Black"
   "mpGridLineColor"        : "LightGray"
   "mpGeophysicalLineColor" : "Black"
   "mpGeophysicalLineThicknessF" : 1.
end create
draw(cn_plot)                                   ; draw the contour plot
draw(map)
frame(wks)                                      ; advance the frame
end                                             ; ncl scripts end with "end"
