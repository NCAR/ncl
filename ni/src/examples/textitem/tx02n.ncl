;
;   $Id: tx02n.ncl,v 1.7 1995-04-07 10:54:34 boote Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tx02n.ncl
;
;      Author:         Bob Lackman
;                  National Center for Atmospheric Research
;                      PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 06 18:31:18 MDT 1995
;
;      Description:    Demonstrates the TextItem Object
;                      writing "NCAR Graphics" in a filled
;                      colored font.  Turn on the bounding box.
;

;
; Begin NCL script.
;
begin

;
; Create an application context. Set the app dir to the current directory
; so the application looks for a resource file in the working directory.
; In this example the resource file supplies the plot title only.
;
appid = create "tx02" appClass defaultapp
    "appDefaultParent" : "True"
    "appUsrDir" : "./"
end create

;
; Default is to display output to an X workstation
;
NCGM=0

if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
  wid = create "tx02Work" ncgmWorkstationClass defaultapp
      "wkMetaName"         : "tx02n.ncgm"
  end create
else
;
; Create an XWorkstation object.
;
  wid = create "tx02Work" xWorkstationClass defaultapp
      "wkPause" : "True"
  end create
end if

;
; Create TextItem object.
;
pid = create "TextItems" textItemClass wid end create

draw(pid)
frame(wid)

;
; End NCL script.
;
end
