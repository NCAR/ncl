;
;	$Id: tx06n.ncl,v 1.1 1995-03-20 21:29:04 fred Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tx06n.ncl
;
;      Author:         Fred Clare
;                      National Center for Atmospheric Research
;                      PO 3000, Boulder, Colorado
;
;      Date:           Thu Mar 16 16:01:13 MST 1995
;
;      Description:    Demonstrates TextItem text justifications.
;

;
; Begin NCL script.
;
begin

;
; Create an X11 workstation, defining the desired colors.
;
cmap = new( (/3, 3/), float)
cmap(0,:) = (/0.0, 0.0, 1.0/)
cmap(1,:) = (/1.0, 0.0, 0.4/)
cmap(2,:) = (/0.4, 0.0, 0.4/)
workstation_id = create "tx05Work"  xWorkstationLayerClass defaultapp
  "wkColorMap"         : cmap
  "wkBackgroundColor"  : (/ 1., 1., 1. /)
end create

;
; Create a TextItem object.
;
text_item_id = create "tx06TextItem" textItemLayerClass workstation_id
end create

labels = (/                                                  \
            "Top Left"  ,"Center Left" , "Bottom Left"  ,    \
            "Top Center" ,"Center Center", "Bottom Center",  \
            "Top Right"  ,"Center Right" , "Bottom Right"    \
          /)

;
;  Draw a string with various heights and at various angles.
;
char_height = 0.035
Xmarker_height = .03
do just = 0, 8
  y_coord =  0.08*just+0.1
  if ( (just % 3) .eq. 0) then
    y_coord = y_coord + 0.45*char_height
  end if
  if ( (just % 3) .eq. 2) then
    y_coord = y_coord - 0.45*char_height
  end if
  setvalues text_item_id
    "txString" : labels(just)
    "txFontColor" : 1
    "txJust" : just
    "txFont" : 22
    "txPosXF" : 0.5
    "txFontHeightF" : char_height
    "txPosYF" : y_coord
  end setvalues
  draw(text_item_id)

;
;  Mark the justification point.
;
  setvalues text_item_id
    "txString" : "X"
    "txFontColor" : 2
    "txJust" : 4
    "txFont" : 22
    "txFontHeightF" : Xmarker_height
  end setvalues
  draw(text_item_id)
end do

;
; Label the plot.
;
setvalues text_item_id
  "txFont" : 25
  "txJust" : 4
  "txFontColor" : 3
  "txFontHeightF" : 0.045
  "txPosXF" : 0.5
  "txPosYF" : 0.92
  "txString" : "Text Justifications"
end setvalues
draw(text_item_id)

setvalues text_item_id
  "txPosXF" : 0.5+0.5*char_height
  "txPosYF" : 0.84
  "txString" : " - Marks the justification point"
end setvalues
draw(text_item_id)

bb = NhlGetBB(text_item_id)
setvalues text_item_id
  "txFont" : 22
  "txFontColor" : 2
  "txFontHeightF" : Xmarker_height
  "txPosXF" : bb(2)
  "txPosYF" : 0.837
  "txJust" : 7
  "txString" : "X"
end setvalues
draw(text_item_id)

frame(workstation_id)

delete(text_item_id)
delete(workstation_id)

;
; End NCL script.
;
end
