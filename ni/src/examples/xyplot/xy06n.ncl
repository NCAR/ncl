
;      $Id: xy06n.ncl,v 1.12 2010-03-15 22:49:25 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy06n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Mon Feb 13 09:24:47 MST 1995
;;
;; Description:   This example shows how to do several things:
;;
;;                1) How to create an XyPlot object with multiple
;;                   lines using the CoordArrays and Data objects.
;;                2) How to change the data to create a different
;;                   plot, without having to create a new data object
;;                   (using the NhlSetValues call).
;;                3) How to overlay a Legend object and to tweak
;;                   several kinds of resources (see "xy06.res").
;;                4) How to use ncl interface routines to open
;;                   and access a netCDF file.
;;                5) How to use a kludgy method for using the resource
;;                   file to specify which stations you want to observe.

;
; Begin NCL script
;
begin


;
; Create variables for stations information. "NSTATIONS" is the number
; of stations we want data vales for, and "spv" is the special value
; for missing values.  If you want to plot data values for
; other cities, change the "stations" variable accordingly, and
; also change "NSTATIONS" if you change the number of stations.
;

NHOURS = 24
NSTATIONS = 8
NCOLORS = NSTATIONS+2

spv = -9999.0

;
; Create variable for holding data values.
;
temp = new((/NSTATIONS,NHOURS/),float,spv)
pres = new((/NSTATIONS,NHOURS/),float,spv)
wspd = new((/NSTATIONS,NHOURS/),float,spv)

;
; We need to initialize a non-constant dummy array for our Data
; object, otherwise we'll get error messages.
;
temp(0,:) = ispan(0,NHOURS-1,1)

;
; Modify the color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
cmap = (/ (/0.00,0.00,0.00/), \
	  (/1.00,1.00,1.00/), \
	  (/0.00,0.00,1.00/), \
	  (/0.00,1.00,0.00/), \
	  (/0.00,1.00,0.75/), \
	  (/0.50,0.50,0.63/), \
	  (/1.00,0.00,0.00/), \
	  (/0.75,0.38,0.25/), \
	  (/0.75,0.00,0.75/), \
	  (/1.00,1.00,0.00/) /)


;
; Default is to display output to an X workstation
;
wks_type = "x11"

;
; Create application object.  The application name is used to 
; determine the name of the resource file which will be "xy06.res"
; in this case.
; 
appid = create "xy06" appClass defaultapp
   "appDefaultParent" : True
   "appUsrDir" : "./"
end create

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  xworkid = create "xy06Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy06n.ncgm"
      "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation
;
  xworkid = create "xy06Work" windowWorkstationClass defaultapp
    "wkPause" : True
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xworkid = create "xy06Work" psWorkstationClass defaultapp
    "wkPSFileName" : "xy06n.ps"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xworkid = create "xy06Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "xy06n.pdf"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  xworkid = create "xy06Work" documentWorkstationClass defaultapp
    "wkFileName" : "xy06n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  xworkid = create "xy06Work" imageWorkstationClass defaultapp
    "wkFileName" : "xy06n"
    "wkFormat" : wks_type
    "wkColorMap" : cmap
  end create
end if

;
; Define a dummy Data object.  We do this so a DataSpec object is
; created automatically and then we can use a NhlFGetValues call to
; get the names of the stations we want data values for.
;
dataid = create "xyData" coordArraysClass defaultapp
    "caYArray" : temp(0,:)
end create

;
; The id for this dummy Data object will now become the resource
; value for "xyCoordData".
;
plotid = create "xyPlot1" xyPlotClass xworkid
    "xyCoordData": dataid
end create

;
; Get the DataSpec object id.
;
getvalues plotid
   "xyCoordDataSpec" : dspec
end getvalues

;
; Get station id names that have been set in resource file.  This
; is a round-about way of doing things, but it makes it convenient to
; be able to specify the stations we want.  We used the
; 'xyExplicitLegendLabels' resource, because we also want to use the 
; station names to label the lines in the legend.
;
getvalues dspec(0)
   "xyExplicitLegendLabels" : station_abrev
end getvalues

numids = dimsizes(station_abrev)

;
; Set date.
;
date = "950318"

;
; Define procedure for getting the daily data values from a particular
; station.
;

temp@_FillValue = spv
pres@_FillValue = spv
wspd@_FillValue = spv
filedir = ncargpath("data")
do j = 0,NHOURS-1
  if((j.lt.10))
    filename = filedir + "/cdf/" + date + "0" + j + "_sao.cdf"
  else
    filename = filedir + "/cdf/" + date + j + "_sao.cdf"
  end if
; 
; Open data file containing grid of global data values.
;
  tmpfile = addfile(filename,"r")
  rdstations = chartostring(tmpfile->id)
  do l = 0, numids-1
    i = ind(rdstations.eq.station_abrev(l))
    print("station " + station_abrev(l))
    if(.not.all(ismissing(i)))
      print("loaded hour " + j)
      temp(l,j) = (/tmpfile->T(i(0))/)
      pres(l,j) = (/tmpfile->PSL(i(0))/)
      wspd(l,j) = (/tmpfile->SPD(i(0))/)
    else
      print("missing hour " + j)
      temp(l,j) = spv
      pres(l,j) = spv
      wspd(l,j) = spv
    end if
    delete(i)
  end do
  delete(rdstations)
  delete(tmpfile)
end do

;
; Define the Data object.  Since only the Y values are specified here,
; each Y value will be paired with its integer array index.  The data
; id from this object will become the value for the XyPlot resource
; "xyCoordData".
;
dataid = create "xyData" coordArraysClass noparent
    "caYArray" : temp
    "caYMissingV" : spv
end create

;
; The id for this Data object is now the resource value for
; xyCoordData.  Tweak some XyPlot resources in the resource file
; ("xy06.res").
; 
plotid = create "xyPlot1" xyPlotClass xworkid
    "xyCoordData": dataid
end create

;
; Draw the plot.
; 
draw(plotid)
frame(xworkid)

;
; Change the data in our Data object.  Notice we use setvalues
; instead of create, so our data object will have the same
; name as when we originally created it, "xyData".
;
setvalues dataid
    "caYArray" : pres
end setvalues

;
; Create another XyPlot object with this new Data object.  We have to
; create another object instead of just changing the current one,
; because we want to change some resource values to title our axes
; differently.
;
plotid = create "xyPlot2" xyPlotClass xworkid
    "xyCoordData": dataid
end create

;
; Draw the plot.
; 
draw(plotid)
frame(xworkid)

;
; Change the data in our Data object.  Notice we use setvalues
; instead of create, so our data object will have the same
; name as when we originally created it, "xyData".
;
setvalues dataid
    "caYArray" : wspd
end setvalues

;
; Create another XyPlot object with this new Data object.  We have to
; create another object instead of just changing the current one,
; because we want to change some resource values to title our axes
; differently.
;
plotid = create "xyPlot3" xyPlotClass xworkid
    "xyCoordData": dataid
end create

;
; Draw the plot.
; 
draw(plotid)
frame(xworkid)

;
; End NCL script
;
end

