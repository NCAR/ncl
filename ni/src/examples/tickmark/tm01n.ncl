;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tm01n.ncl
;
;      Author:         Bob Lackman (converted by Ed Stautler)
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 06 18:31:18 MDT 1995
;
;      Description:    Demonstrates the TickMark Object
;                      defaults.
;

;
; Begin NCL script.
;
begin

;
; Create an application context. Set the app dir to the current
; directory so the application looks for a resource file in the working
; directory. In this example the resource file supplies the plot title
; only.
;
appid = create "tm01" appClass defaultapp
    "appUsrDir" : "./"
    "appDefaultParent" : True
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
  wid = create "tm01Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "tm01n.ncgm"
  end create
else
  if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
    wid = create "tm01Work" xWorkstationClass defaultapp
      "wkPause" : True
    end create
  else
    if (PS .eq. 1) then
;
; Create an ncgmWorkstation object.
;
      wid = create "tm01Work" psWorkstationClass defaultapp
        "wkPSFileName" : "tm01n.ps"
      end create
    end if
  end if
end if

;
; Specify the viewport extent of the object.
;
pid = create "TickMarks" tickMarkClass wid
    "vpXF" : .2
    "vpYF" : .8
    "vpWidthF" : .6
    "vpHeightF" : .6
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
