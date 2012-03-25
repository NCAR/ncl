;
;      $Id: xy10n.ncl,v 1.8 2010-03-15 22:49:25 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy10n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Fri May  5 13:44:42 MDT 1995
;;
;; Description:   This example shows how to overlay an XyPlot
;;                on a MapPlot.

;
; Begin NCL script
;
begin

;
; Define procedure for getting the lat/lon data values from a particular
; station.
;
procedure LatLonValues(
   lat[*]:float,
   lon[*]:float,
   cdf_file:string,
   spvlt:float,
   spvln:float
)
local i,
      tmpfile,
      filename, 
      rec_len
begin
    filedir = ncargpath("data")
    filename = filedir + "/cdf/" + cdf_file
; 
; Open data file containing grid of global data values.
;
    tmpfile = addfile(filename,"r");
    rec_len = dimsizes(tmpfile->id(:,0))
    spvlt = tmpfile->lat@_FillValue
    spvln = tmpfile->lon@_FillValue
;
; Read in lat array
;
    lat(0:rec_len-1) = tmpfile->lat
    lon(0:rec_len-1) = tmpfile->lon

;
; Filter out of range values using _FillValue feature
;
    lat = mask(lat,(lat.gt.-90.and.lat.lt.90),True)
    lat@_FillValue = spvlt

    lon = mask(lon,(lon.gt.-180.0.and.lon.lt.180.0),True)
    lon@_FillValue = spvln
    
end

;
; Create variable for holding data values.
;
lat = new(3000,float)
lon = new(3000,float)

;
; Create application object.  The application name is used to 
; determine the name of the resource file which will be "xy10.res"
; in this case.
; 
appid = create "xy10" appClass defaultapp
   "appDefaultParent" : "True"
   "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
  xworkid = create "xy10Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy10n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation
;
  xworkid = create "xy10Work" windowWorkstationClass defaultapp
    "wkPause" : "True"
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xworkid = create "xy10Work" psWorkstationClass defaultapp
    "wkPSFileName" : "xy10n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xworkid = create "xy10Work" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "xy10n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  xworkid = create "xy10Work" documentWorkstationClass defaultapp
    "wkFileName" : "xy10n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  xworkid = create "xy10Work" imageWorkstationClass defaultapp
    "wkFileName" : "xy10n"
    "wkFormat" : wks_type
  end create
end if

;
; Call procedure to get data values.
;
cdf_file = "95031800_sao.cdf"
spvlt = -9999.
spvln = -9999.

LatLonValues(lat,lon,cdf_file,spvlt,spvln)

;
; Define the Data object.
;
dataid = create "xyData" coordArraysClass defaultapp
    "caXArray" : lon
    "caYArray" : lat
    "caXMissingV" : spvln
    "caYMissingV" : spvlt
end create

;
; The id for this Data object is now the resource value for
; xyCoordData.  Tweak some XyPlot resources in the resource file
; ("xy10.res").
; 
xyid1 = create "xyPlot1" xyPlotClass xworkid
    "xyCoordData": dataid
end create

;
; Plot all of the station ids.
; 
draw(xyid1)
frame(xworkid)

;
; Create a second XyPlot object with a different name so we can
; change the "tr" resources to limit the plot within the mainland
; United States.
;
xyid2 = create "xyPlot2" xyPlotClass xworkid
    "xyCoordData": dataid
end create

;
; Plot station ids over mainland United States only.  This is done
; using resources in the "xy10.res" resource file.
;
draw(xyid2)
frame(xworkid)

;
; Create two MapPlots, one of the world and one of the United States 
; (projection parameters are set up in the "xy10.res" resource file).
;
mpid1 = create "mpPlot1" mapPlotClass xworkid end create
mpid2 = create "mpPlot2" mapPlotClass xworkid end create

; Draw the two map plots. 
;
draw(mpid1)
frame(xworkid)

draw(mpid2)
frame(xworkid)

;
; Overlay the first XyPlot object on the first MapPlot object.  This
; will plot station ids over the world.
;
overlay(mpid1,xyid1)
draw(mpid1)
frame(xworkid)
    
;
; Overlay the second XyPlot object on the second MapPlot object.  This
; will plot station ids over mainland United States only.
;
overlay(mpid2,xyid2)
draw(mpid2)
frame(xworkid)

;
; End NCL script
;
end

