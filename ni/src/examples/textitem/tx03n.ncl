;
;    $Id: tx03n.ncl,v 1.5 1995-03-22 21:45:52 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tx03n.ncl
;
;      Author:         Bob Lackman
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;      Date:           Fri Jan 06 18:31:18 MDT 1995
;
;      Description:    Demonstrates the TextItem Object
;                      Writes "NCAR Graphics" in a series of
;                      114 different colors. (The default colormap.)
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
appid = create "tx03" appLayerClass defaultapp
    "appDefaultParent" : "True"
    "appUsrDir" : "./"
end create

;
; Default is to create a metafile.
;
NCGM=1

if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
  wid = create "tx03Work" ncgmWorkstationLayerClass defaultapp
      "wkMetaName"         : "tx03n.ncgm"
  end create
else
;
; Create an XWorkstation object.
;
  wid = create "tx03Work" xWorkstationLayerClass defaultapp
      "wkPause" : "True"
  end create
end if

;
; Create 114 plots varying the fill color of the text bounding box
; to all entries of the default workstation color map.
;

do i=1,114
    pid = create "TextItems" textItemLayerClass wid
        "txBackgroundFillColor" : i
    end create

    draw(pid);
    frame(wid);
end do

;
; End NCL script.
;
end
