;
;    $Id: tx03n.ncl,v 1.7 1995-05-01 16:41:49 haley Exp $
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
;                      different colors (using the default colormap.)
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
appid = create "tx03" appClass defaultapp
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
  wid = create "tx03Work" ncgmWorkstationClass defaultapp
      "wkMetaName"         : "tx03n.ncgm"
  end create
else
;
; Create an XWorkstation object.
;
  wid = create "tx03Work" xWorkstationClass defaultapp
      "wkPause" : "True"
  end create
end if

;
; Get the number of colors in the default color table.
;
getvalues wid
	"wkColorMapLen":  num_colors
end getvalues

;
; Create multiple plots varying the fill color of the text bounding box
; to all entries of the default workstation color map.
;

do i=1,num_colors
    pid = create "TextItems" textItemClass wid
        "txBackgroundFillColor" : i
    end create

    draw(pid);
    frame(wid);
end do

;
; End NCL script.
;
end
