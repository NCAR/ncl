;
;   $Id: tx07n.ncl,v 1.2 1995-03-22 21:46:02 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           tx07n.ncl
;
;      Author:         Fred Clare
;                      National Center for Atmospheric Research
;                      PO 3000, Boulder, Colorado
;
;      Date:           Mon Mar 20 14:16:13 MST 1995
;
;      Description:    Demonstrates TextItem text spacings and
;                      aspect ratios.
;

;
; Begin NCL script.
;
begin

;
; Create a workstation, defining the desired colors.
;
cmap = new( (/3, 3/), float)
cmap(0,:) = (/0.0, 0.0, 1.0/)
cmap(1,:) = (/0.4, 0.0, 0.4/)
cmap(2,:) = (/1.0, 0.0, 0.0/)

;
; Default is to display output to an X workstation
;
NCGM=0

if (NCGM .eq. 1) then
    workstation_id = create "tx07Work"  ncgmWorkstationLayerClass defaultapp
      "wkMetaName"         : "tx07n.ncgm"
      "wkColorMap"         : cmap
      "wkBackgroundColor"  : (/ 1., 1., 1. /)
    end create
else
;
; Create an XWorkstation object.
;
  workstation_id = create "tx07Work"  xWorkstationLayerClass defaultapp
    "wkPause"            : True
    "wkColorMap"         : cmap
    "wkBackgroundColor"  : (/ 1., 1., 1. /)
  end create
end if

;
; Create two TextItem objects to be used for the example string
; and for the label for the example sting.
;
example_id = create "Example string" textItemLayerClass workstation_id
end create
label_id = create "Label" textItemLayerClass workstation_id
end create

cspacings = (/ "0.0"  ,"1.5" , "0.6" /)
ispacings = (/  0.0   , 1.5  ,  0.6  /)
caspects = (/ "0.7", "2.0" /)
iaspects = (/  0.7 ,  2.0  /)

;
;  Set up example string.
;
setvalues example_id
  "txString" : "NCAR Graphics"
  "txFontColor" : 1
  "txFont" : 25
  "txPosXF" :  0.5
  "txFontHeightF" : .05
end setvalues

;
;  Set up label string.
;
setvalues label_id
  "txFontColor" : 2
  "txFont" : 21
  "txPosXF" :  0.5
  "txFontHeightF" : .04
end setvalues

;
;  Spacings
;
do i = 0,2
  ypos = 0.83 - 0.17 * i
  setvalues label_id
    "txString" : "txConstantSpacingF = " + cspacings(i)
    "txPosYF" : ypos
  end setvalues
  draw(label_id)
  setvalues example_id
    "txConstantSpacingF" : ispacings(i)
    "txPosYF" : ypos - 0.07
  end setvalues
  draw(example_id)
end do

;
;  Aspect ratios
;
do i = 0,1
  ypos = 0.32 - 0.17 * i 
  setvalues label_id
    "txString" : "txFontAspectF = " + caspects(i)
    "txPosYF" : ypos
  end setvalues
  draw(label_id)
  setvalues example_id
    "txConstantSpacingF" : 0.0
    "txPosYF" : ypos - 0.07
    "txFontAspectF" : iaspects(i)
  end setvalues
  draw(example_id)
end do

;
; Plot title.
;
setvalues label_id
  "txFontColor" : 3
  "txFont" : 25
  "txPosYF" : 0.92
  "txFontHeightF" : 0.045
  "txString" : "Text Spacings and Aspect Ratios"
end setvalues
draw(label_id)

frame(workstation_id)

delete(example_id)
delete(label_id)
delete(workstation_id)

;
; End NCL script.
;
end
