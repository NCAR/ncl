
;      $Id: xy06n.ncl,v 1.6 1995-06-22 21:09:38 haley Exp $
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
pressure = new((/NSTATIONS,NHOURS/),float,spv)
wind_speed = new((/NSTATIONS,NHOURS/),float,spv)

;
; We need to initialize a non-constant dummy array for our Data
; object, otherwise we'll get error messages.
;
do  j = 0,NHOURS-1
   temp(0,j) = j
end do

;
; Modify the color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
cmap = new( (/NCOLORS,3/), float)
cmap(0,:) = (/0.00,0.00,0.00/)
cmap(1,:) = (/1.00,1.00,1.00/)
cmap(2,:) = (/0.00,0.00,1.00/)
cmap(3,:) = (/0.00,1.00,0.00/)
cmap(4,:) = (/0.00,1.00,0.75/)
cmap(5,:) = (/0.50,0.50,0.63/)
cmap(6,:) = (/1.00,0.00,0.00/)
cmap(7,:) = (/0.75,0.38,0.25/)
cmap(8,:) = (/0.75,0.00,0.75/)
cmap(9,:) = (/1.00,1.00,0.00/)


;
; Default is to display output to an X workstation
;
NCGM=0
X11=1
PS=0

;
; Create application object.  The application name is used to 
; determine the name of the resource file which will be "xy06.res"
; in this case.
; 
appid = create "xy06" appClass defaultapp
   "appDefaultParent" : "True"
   "appUsrDir" : "./"
end create

if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
  xworkid = create "xy06Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy06n.ncgm"
      "wkColorMap" : cmap
  end create
else
  if (X11 .eq. 1) then
;
; Create an X workstation
;
    xworkid = create "xy06Work" xWorkstationClass defaultapp
      "wkPause" : "True"
      "wkColorMap" : cmap
    end create
  else
    if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
      xworkid = create "xy06Work" psWorkstationClass defaultapp
        "wkPSFileName" : "xy06n.ps"
        "wkColorMap"   : cmap
     end create
    end if
  end if
end if

;
; Define a dummy Data object.  We do this so a DataSpec object is
; created automatically and then we can use a NhlFGetValues call to
; get the names of the stations we want data values for.
;
dataid = create "xyData" coordArraysClass defaultapp
    "caYArray" : temp
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

temp@_FillValue = spv
pressure@_FillValue = spv
wind_speed@_FillValue = spv
; 
; Open data file containing grid of global data values.
;
filedir = ncargpath("data")
filename = filedir + "/cdf/950318_sao.cdf"
tmpfile = addfile(filename,"r")

;
; Get station ids, temperatures, pressures, and wind speeds.
;
rdstations = tmpfile->id
tdata = tmpfile->T
pdata = tmpfile->PSL
vdata = tmpfile->SPD

; Loop through each hour of the day.
;
do j = 0,NHOURS-1
; 
; Get number of station ids.
;
  isize = dimsizes(tmpfile->id(:,j,0))
;
; Loop through each station we want to get values for.
;
  do l = 0, numids-1
    do i = 0, (isize-1)
      if(rdstations(i,j,:) .eq. station_abrev(l)) 
        break
      end if
    end do
;
; If found the station we wanted, then get the temperature, pressure,
; and wind speed; otherwise, assign a missing value.
;
    if(i .lt. isize) then
      print("station " + station_abrev(l))
      print("loaded hour " + j)
      temp(l,j) = tdata(i,j)
      pressure(l,j) = pdata(i,j)
      wind_speed(l,j) = vdata(i,j)
    end if
  end do
end do

;
; Define the Data object.  Since only the Y values are specified here,
; each Y value will be paired with its integer array index.  The data
; id from this object will become the value for the XyPlot resource
; "xyCoordData".
;
dataid = create "xyData" coordArraysClass defaultapp
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
    "caYArray" : pressure
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
    "caYArray" : wind_speed
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

