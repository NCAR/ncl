;
;   $Id: tx05n.ncl,v 1.5 1995-04-27 19:42:45 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tx05n.ncl
;
;      Author:         Fred Clare
;                      National Center for Atmospheric Research
;                      PO 3000, Boulder, Colorado
;
;      Date:           Wed Mar 15 12:38:49 MST 1995
;
;      Description:    Demonstrates the TextItem object with text having
;                      various heights and at various angles.
;

;
; Begin NCL script.
;
begin

;
; Create an X11 workstation, defining the desired colors.
;
cmap = new( (/3, 3/), float)
cmap(0,:) = (/1.0, 1.0, 1.0/)
cmap(1,:) = (/0.0, 0.0, 1.0/)
cmap(2,:) = (/0.4, 0.0, 0.4/)
;
; Default is to display output to an X workstation
;
NCGM=0

if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
  workstation_id = create "tx05Work" ncgmWorkstationClass defaultapp
      "wkMetaName"         : "tx05n.ncgm"
      "wkColorMap"         : cmap
  end create
else
;
; Create an XWorkstation object.
;
    workstation_id = create "tx05Work"  xWorkstationClass defaultapp
      "wkPause"            : True
      "wkColorMap"         : cmap
    end create
end if

;
; Create a TextItem object.
;
text_item_id = create "tx05TextItem" textItemClass workstation_id
  "txFont"        : 22
  "txString"      : "NCAR"
end create

;
;  Draw a string with various heights and at various angles.
;
dtr=0.017453292519943
angle = 0.
do while (angle .lt. 136.)
  x_coord = 0.3 + 0.4*cos(dtr*angle)
  y_coord = 0.2 + 0.4*sin(dtr*angle)
  height  = 0.0005*(136.-angle)

  setvalues text_item_id
    "txAngleF" : angle
    "txPosXF" : x_coord
    "txPosYF" : y_coord
    "txFontHeightF" : height
  end setvalues

  draw(text_item_id)

  angle   = angle + (210.*height > 1.)
end do

;
; Text strings at certain specific angles.
;
setvalues text_item_id
  "txAngleF" : 180.
  "txPosXF" : 0.25
  "txPosYF" : 0.34
  "txFontHeightF" : 0.04
  "txFont" : 22
  "txFontColor" : 1
  "txString" : "NCAR"
end setvalues
draw (text_item_id)

setvalues text_item_id
  "txAngleF" : 0.
  "txPosYF" : 0.4
  "txFontHeightF" : 0.03
  "txFontColor" : 2
  "txString" : "180 degrees"
end setvalues
draw (text_item_id)

setvalues text_item_id
  "txAngleF" : -45.
  "txPosXF" : 0.7
  "txPosYF" : 0.6
  "txFontHeightF" : 0.04
  "txFontColor" : 1
  "txString" : "NCAR"
end setvalues
draw (text_item_id)

setvalues text_item_id
  "txAngleF" : 0.
  "txPosXF" : 0.73
  "txPosYF" : 0.65
  "txFontHeightF" : 0.03
  "txFontColor" : 2
  "txJust" : 1
  "txString" : "-45 degrees"
end setvalues
draw (text_item_id)

;
; Label the plot.
;
setvalues text_item_id
  "txAngleF"      : 0.0
  "txFont"        : 25
  "txJust"        : 1
  "txFontColor"   : 2
  "txFontHeightF" : 0.05
  "txPosXF"       : 0.2
  "txPosYF"       : 0.84
  "txString"      : "Text heights &"
end setvalues
draw(text_item_id)

setvalues text_item_id
  "txPosYF"       : .76
  "txString"      : "Text angles"
end setvalues

draw(text_item_id)

frame(workstation_id)

delete(text_item_id)
delete(workstation_id)

;
; End NCL script.
;
end
