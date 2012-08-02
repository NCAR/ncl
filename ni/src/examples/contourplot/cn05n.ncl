;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1993                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn05n.ncl
;
;   Author:     Tim Scheitlin
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;
;   Date:       Wed Apr 12 10:40:27 MDT 1995
;
;   Description: Demonstrates how to create a map plot animation with a 
;		 contour overlay and labelbar annotation.
;

; The PLOT_ALL_DATA flag controls whether or not all 31 days of data is 
; read and plotted.  Setting the flag to TRUE will cause this script to 
; plot 31 frames.  Otherwise, the script will read only 3 days worth of 
; data and plot 3 frames.
PLOT_ALL_DATA=False

function gen_colormap( )
begin
        ;This procedure explicitly sets the red, green, and blue
        ;intensities for each element in a colormap array.
 
        ; Set the color map
        ; Entry 0 is the background color.
 
        ; Entry 1 is the foreground color.
        ; The continental outlines in the map object use this color
        ; as a default.
        return( (/ (/.000,.000,.000/), \
                (/.000,.000,.000/), \
                (/.700,.700,.700/), \
                (/.650,.650,.700/), \
                (/.610,.600,.700/), \
                (/.550,.550,.700/), \
                (/.560,.500,.700/), \
                (/.450,.450,.700/), \
                (/.420,.400,.700/), \
                (/.350,.350,.700/), \
                (/.300,.300,.700/), \
                (/.250,.250,.700/), \
                (/.200,.200,.700/), \
                (/.150,.150,.700/), \
                (/.100,.100,.700/), \
                (/.050,.050,.700/), \
                (/.000,.000,.700/), \
                (/.000,.050,.700/), \
                (/.000,.100,.700/), \
                (/.000,.150,.700/), \
                (/.000,.200,.700/), \
                (/.000,.250,.700/), \
                (/.000,.300,.700/), \
                (/.000,.350,.700/), \
                (/.000,.400,.700/), \
                (/.000,.450,.600/), \
                (/.000,.500,.500/), \
                (/.000,.550,.400/), \
                (/.000,.600,.300/), \
                (/.000,.650,.200/), \
                (/.000,.700,.100/), \
                (/.000,.725,.000/), \
                (/.000,.690,.000/), \
                (/.030,.685,.000/), \
                (/.060,.680,.000/), \
                (/.100,.575,.000/), \
                (/.130,.570,.000/), \
                (/.160,.565,.000/), \
                (/.550,.550,.000/), \
                (/.555,.545,.000/), \
                (/.560,.530,.000/), \
                (/.565,.485,.000/), \
                (/.570,.420,.000/), \
                (/.675,.375,.000/), \
                (/.680,.330,.000/), \
                (/.690,.300,.000/), \
                (/.700,.285,.000/), \
                (/.700,.270,.000/), \
                (/.700,.260,.000/), \
                (/.700,.240,.000/), \
                (/.700,.180,.000/), \
                (/.700,.130,.000/), \
                (/.700,.120,.000/), \
                (/.700,.100,.000/), \
                (/.700,.090,.000/), \
                (/.750,.090,.000/), \
                (/.800,.090,.000/), \
                (/.830,.070,.000/), \
                (/.870,.050,.000/), \
                (/.900,.030,.000/), \
                (/.950,.010,.000/), \
                (/.990,.000,.000/), \
                (/1.00,.000,.000/), \
                (/1.00,.000,.000/) /) )
end

begin
; Get current location of data directory
datadir=ncargpath("data")
datafile=datadir + "/cdf/meccatemp.cdf"

; Load a file record from the netCDF file as a read only data set.
mfile =  addfile (datafile,"r")

;
; Default is to create an NCGM file.
;
wks_type = "ncgm"

if (str_lower(wks_type).eq."ncgm")
;
; Create an ncgm workstation.
;
  wks = create "cn05Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "./cn05n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wks = create "cn05Work" windowWorkstationClass defaultapp
    "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  wks = create "cn05Work" psWorkstationClass defaultapp
    "wkPSFileName" : "./cn05n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  wks = create "cn05Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "./cn05n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF workstation.
;
  wks = create "cn05Work" documentWorkstationClass defaultapp
    "wkFileName" : "./cn05n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG workstation.
;
  wks = create "cn05Work" imageWorkstationClass defaultapp
    "wkFileName" : "./cn05n"
    "wkFormat" : wks_type
  end create
end if


; Create a 64x3 array for the colormap (the first dimension is arbitrary).
; The second dimension represents each component of an RGB triple.


; Assign the colormap to the workstation.
setvalues wks
; Generate a colormap.
   "wkColorMap" : gen_colormap()
end setvalues

; Create a scalar field object that will be used as the
; dataset for the contour object.
field1 = create "field1" scalarFieldClass defaultapp
	; Read the first temperature field.
        "sfDataArray" : mfile->t(0,:,:)  
	; Assign the X and Y start and end coordinates.
        "sfXCStartV" : mfile->lon(0)  
        "sfXCEndV": mfile->lon(filevardimsizes(mfile,"lon") - 1)
        "sfYCStartV" : mfile->lat(0)
        "sfYCEndV": mfile->lat(filevardimsizes(mfile,"lat") - 1)
end create


; Create an array that will contain the indices into the 
; colormap defined earlier. Assign the indices, skipping the
; first two colormap entries that contain the default background
; and foreground color. 
;
fillindices = ispan(2,63,1)

; Create a contour plot object.
con1 = create "con1" contourPlotClass wks
         ; Assign the data that was read earlier.
         "cnScalarFieldData" : field1
         ; Assign the colormap fill indices
         "cnFillColors": fillindices
         ; Set the range and spacing of the contour levels.
         "cnLevelSelectionMode" : "ManualLevels"
         "cnMinLevelValF" :  195.0
         "cnMaxLevelValF" :  328.0
         "cnLevelSpacingF" : 2.25
         ; Turn on contour fills.
         "cnFillOn" : True
         ; Turn off the contour lines and labels.
         "cnLinesOn" : False
         "cnLineLabelsOn" : False
         "cnHighLabelsOn" : False
         "cnLowLabelsOn" : False
         "cnInfoLabelOn" : False
         ; Turn on the overlay labelbar.
         "pmLabelBarDisplayMode" : "ALWAYS"
         ; Set the labelbar size
         "pmLabelBarHeightF" : .15
         "pmLabelBarWidthF" : .6
         ; Set the location and orientation of the labelbar.
         "pmLabelBarSide" : "bottom"
         "lbOrientation" : "horizontal"
         ; Set the lablebar title, font, and color.
         "lbTitleString" : "Day 1"
         "lbTitleFont" : 22  ; Helvetica-bold
         "lbTitleFontColor" : "PaleGreen4"
         ; Turn off the labelbar perimeter box 
         "lbPerimOn" : False
         ; Turn off lines that separate each color in the labelbar.
         "lbBoxLinesOn" : 0
         ; Turn off labelbar labels
         "lbLabelsOn" : False
end create

; Create map object
map = create "map" mapPlotClass wks
	; Allow the map to be stretched along 
	; the horizontal and vertical view axes.
	"mpShapeMode" : "FreeAspect"
        ; Set the viewport position and size. This will
        ; stretch the map along its axes.
        "vpXF": .03
        "vpYF": .90
        "vpWidthF" : .94
        "vpHeightF" : .68
        ; Set the center of projection.
        "mpCenterLatF": 0.0
        "mpCenterLonF": 150.0
        ; Set the projection type.
        "mpProjection": "CYLINDRICALEQUIDISTANT"
        ; Turn off grid and limb lines, labels, and permimeter.
        "mpGridAndLimbOn": False
        "mpLabelsOn": False
        "mpPerimOn": False
        ; Turn on main title and set its value, font, and color.
        "pmTitleDisplayMode" : "Always"
        "tiMainString" : "January Global Surface Temperature"
        "tiMainFont" : 22
        "tiMainFontColor" : "PaleGreen4"
end create

; Overlay function used to assign the object "con1" to
; the overlay base

overlay(map,con1)

; Create two labels (high and low values) for labelbar.
lbarlo = create "lbarlo" textItemClass wks
        "txString": "195 K"
        "txPosXF": .05
        "txPosYF": .03
        "txFont": 22
        "txFontColor": "PaleGreen4"
        "txFontHeightF": .03
        "txJust": "CENTERLEFT"
end create

lbarhi = create "lbarhi" textItemClass wks
        "txString": "328 K"
        "txPosXF": .85
        "txPosYF": .03
        "txFont": 22
        "txFontColor": "PaleGreen4"
        "txFontHeightF": .03
        "txJust": "CENTERLEFT"
end create

; Draw all objects.
print("Plotting Day " + 1)
draw (map)
draw (lbarlo)
draw (lbarhi)
frame(wks)

; Loop on remaining time steps

 if ( PLOT_ALL_DATA ) then
       NFRAMES=dimsizes(mfile->time) - 1
 else
       NFRAMES=2
 end if
 do i = 1, NFRAMES
         print("Plotting Day " + (i+1))
         ; Read the next data field (next day).
         setvalues field1
                 "sfDataArray" : mfile->t(i,:,:)
         end setvalues
         d=i+1
         ; Increment day string.
         setvalues con1
		 "lbTitleString": "Day " + d
         end setvalues
         ; draw another plot.
         draw(map)
         draw(lbarlo)
         draw(lbarhi)
         frame(wks)
 end do

 if ( .not.PLOT_ALL_DATA ) then

  print("To plot all 31 days in this animation, edit the NCL script, and set the PLOT_ALL_DATA flag to TRUE.")

 end if
end
