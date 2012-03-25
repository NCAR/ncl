;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn09n.ncl
;
;   Author:     Ethan Alpert
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;           Converted to NCL by Ed Stautler
;
;   Date:       Fri Apr 28 11:37:28 MDT 1995
;
;   Description:    Reads a netCDF file and produces a series of
;                   surface pressure contour plots.
;

begin

;
; Open up data file containing surface pressure data for
; entire globe
;
filedir = ncargpath("data")
filename = filedir + "/cdf/contour.cdf"
a = addfile(filename,"r")

;
; set color table
;
cmap = (/ (/0.0, 0.0, 0.0/), \
	 (/1.0, 1.0, 1.0/), \
	 (/0.0, 0.0, 0.0/), \
	 (/1.0, 0.0, 0.0/), \
	 (/0.0, 1.0, 0.0/), \
	 (/0.0, 0.0, 1.0/), \
	 (/1.0, 1.0, 0.0/), \
	 (/0.0, 1.0, 1.0/), \
	 (/1.0, 0.0, 1.0/), \
	 (/0.5, 0.0, 0.0/), \
	 (/0.5, 1.0, 1.0/), \
	 (/0.0, 0.0, 0.5/), \
	 (/1.0, 1.0, 0.5/), \
	 (/0.5, 0.0, 1.0/), \
	 (/1.0, 0.5, 0.0/), \
	 (/0.0, 0.5, 1.0/), \
	 (/0.5, 1.0, 0.0/), \
	 (/0.5, 0.0, 0.5/), \
	 (/0.5, 1.0, 0.5/), \
	 (/1.0, 0.5, 1.0/), \
	 (/0.0, 0.5, 0.0/), \
	 (/0.5, 0.5, 1.0/), \
	 (/1.0, 0.0, 0.5/) /)

wks_type = "ncgm"

if (str_lower(wks_type).eq."ncgm")
;
; Open NCGM workstation.
;
  wks = create "cn09Work" ncgmWorkstationClass defaultapp
    "wkMetaName" : "cn09n.ncgm"
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation.
;
  wks = create "cn09Work" windowWorkstationClass defaultapp
      "wkColorMap" : cmap
      "wkPause" : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Open PS workstation.
;
  wks = create "cn09Work" psWorkstationClass defaultapp
    "wkColorMap" : cmap
    "wkPSFileName" : "cn09n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Open PDF workstation.
;
  wks = create "cn09Work" pdfWorkstationClass defaultapp
    "wkColorMap" : cmap
    "wkPDFFileName" : "cn09n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Open cairo PS/PDF workstation.
;
  wks = create "cn09Work" documentWorkstationClass defaultapp
    "wkColorMap" : cmap
    "wkFileName" : "cn09n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Open cairo PNG workstation.
;
  wks = create "cn09Work" imageWorkstationClass defaultapp
    "wkColorMap" : cmap
    "wkFileName" : "cn09n"
    "wkFormat" : wks_type
  end create
end if

;
; Read in surface pressure and convert it to millibars
;
p = a->Psl/100.0
frtime = a->frtime

;
; Creates scalar field configured with first time step
; of pressure data
;
field1 = create "field1" scalarFieldClass defaultapp
    "sfDataArray" : p(0,:,:) 
    "sfMissingValueV" : p@_FillValue
    "sfXCStartV" : a->lon(0)
    "sfXCEndV" : a->lon(filevardimsizes(a,"lon")- 1)
    "sfYCStartV" : a->lat(0)
    "sfYCEndV" : a->lat(filevardimsizes(a,"lat") - 1)
end create

;
; Creates contour object using manual level spacing and solid
; color fill
;
con1 = create "con1" contourPlotClass wks
    "cnScalarFieldData" : field1
    "cnLevelSelectionMode" : "ManualLevels"
    "cnMinLevelValF" : 960.0
    "cnMaxLevelValF" : 1040.0
    "cnLevelSpacingF" : 5.0
    "cnMaxLevelCount" : 25
    "cnInfoLabelOn" : "OFF"
    "cnHighLabelsOn" : "OFF"
    "cnLowLabelsOn" : "OFF"
    "cnLineLabelsOn" : False
    "cnLinesOn" : False
    "cnFillOn" : True
    "tiMainString" : "Forecast Time " + 0
end create

; 
; Draw first frame
;
draw(con1)
frame(wks)

;
; Loop and draw remaining frames reseting the scalar field object
; with a new array for each iteration
;
do i = 1, dimsizes(frtime) - 1 
    setvalues field1
        "sfDataArray" : p(i,:,:)
    end setvalues
    setvalues con1
        "tiMainString" : "Forecast Time " + frtime(i)
    end setvalues
    draw(con1)
    frame(wks)
end do

delete(a)
delete(con1)
delete(wks)
delete(field1)
delete(i)
delete(p)
delete(frtime)

end
