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

NCGM=1

if (NCGM.eq.1) then
;
; Open NCGM workstation and name the metafile "contour00.ncgm"
;
    wks = create "cn09Work" ncgmWorkstationClass noparent
        "wkMetaName" : "cn09n.ncgm"
    end create
else
;
; Create an X workstation.
;
    wks = create "cn09Work" xWorkstationClass defaultapp
        "wkPause" : "True"
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
field1 = create "field1" scalarFieldClass noparent
    "sfDataArray" : p(0,:,:) 
    "sfMissingValueV" : p@_FillValue
    "sfXCStartV" : a->lon(0)
    "sfXCEndV" : a->lon(dimsizes(a->lon)- 1)
    "sfYCStartV" : a->lat(0)
    "sfYCEndV" : a->lat(dimsizes(a->lat) - 1)
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
    "cnLineLabelsOn" : "False"
    "cnLinesOn" : "False"
    "cnFillOn" : "True"
    "tiMainString" : "Forcast Time " + 0
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
        "tiMainString" : "Forcast Time " + frtime(i)
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
