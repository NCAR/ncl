;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tm03n.ncl
;
;      Author:         Bob Lackman (converted by Ed Stautler)
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 06 18:31:18 MDT 1995
;
;      Description:    Demonstrates the TickMark Object
;                      with reversed and log y axis.
;

;
; Begin NCL script.
;
begin

level = (/ 1000, 850, 700, 500, 400, 300, 250, 200, 150, 100 /)

;
; Define label strings for EXPLICIT mode tick mark placement
;
labels = (/ "90:S:o:N:S", "60:S:o:N:S", "30:S:o:N:S", "EQ", \
        "30:S:o:N:N", "60:S:o:N:N", "90:S:o:N:N" /)
;
; Specify data locations for above labels
;
labellocs = (/ -90.0, -60.0, -30.0, 0.0, 30.0, 60.0, 90.0 /)
        
;
; Create an application context. Set the app dir to the current
; directory so the application looks for a resource file in the working
; directory. In this example the resource file supplies the plot title
; only.
;
appid = create "tm03" appClass defaultapp
    "appDefaultParent" : True
    "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation.
;
NCGM=0
X11=1
PS=0

if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
  wid = create "tm03Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "tm03n.ncgm"
  end create
else
  if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
    wid = create "tm03Work" xWorkstationClass defaultapp
      "wkPause" : True
    end create
  else
    if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
      wid = create "tm03Work" psWorkstationClass defaultapp
        "wkPSFileName" : "tm03n.ps"
      end create
    end if
  end if
end if

pid = create "TickMarks" tickMarkClass wid
    "vpXF" : .2
    "vpYF" : .8
    "vpWidthF" : .6
    "vpHeightF" : .6
    "tmYLDataTopF" : 100.0
    "tmYLDataBottomF" : 1000.0
    "tmXBDataRightF" : 90.0
    "tmXBDataLeftF" : -90.0
    "tmYLStyle" : "IRREGULAR"
    "tmXBMode" : "EXPLICIT"
    "tmXBMinorOn" : False
    "tmXBValues" : labellocs

;
; Array "level" contains original grid point data locations in Y direction.
; Providing the grid points to the TickMark object as the control points
; for the IRREGULAR style transformation, means that these points will be
; evenly spaced along the Y axis. Since this is how CONPACK thinks the
; points are spaced, the tick marks will correctly correspond with the 
; data coordinates. See the HLU User's Guide for a complete
; discussion of IRREGULAR style transformations.
;
    "tmXBLabels" : labels
    "tmYLIrregularPoints" : level
end create

draw(pid)
frame(wid)
delete(pid)
delete(wid)
delete(appid)

;
; End NCL script.
;
end
