;
;      $Id: tx04n.ncl,v 1.11 2010-03-15 22:49:24 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  File:       tx04c.c
;
;  Author:     Jeff Boote
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;  Date:       Tue Jan 24 10:44:51 MST 1995
;
;  Description:         Demonstrates TextItem object.
;

;
; Begin NCL script.
;
begin

;
; Create an application context. Set the app dir to the current directory
; so the application looks for the resource file the directory it executes
; from. 
;
app_id = create "tx04" appClass defaultapp
    "appDefaultParent" : True
    "appUsrDir" : "./"
end create
    
;
; Default output is to an X workstation. 
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
    workstation_id = create "tx04Work"  ncgmWorkstationClass defaultapp
      "wkMetaName" : "tx04n.ncgm"
    end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
  workstation_id = create "tx04Work"  windowWorkstationClass defaultapp
    "wkPause"            : True
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  workstation_id = create "tx04Work"  psWorkstationClass defaultapp
    "wkPSFileName" : "tx04n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  workstation_id = create "tx04Work"  pdfWorkstationClass defaultapp
    "wkPDFFileName" : "tx04n.pdf"
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  workstation_id = create "tx04Work"  documentWorkstationClass defaultapp
    "wkFileName" : "tx04n"
    "wkFormat" : wks_type
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  workstation_id = create "tx04Work"  imageWorkstationClass defaultapp
    "wkFileName" : "tx04n"
    "wkFormat" : wks_type
  end create
end if

;
; This is the only creation of a text object for this entire program.
;
text_item_id = create "tx04TextItem" textItemClass workstation_id
    "txJust" : 4
    "txFont" : 25
    "txFontHeightF" : .03
    "txString" : "TextItem - VARIOUS CAPABILITIES"
    "txPosXF" : .5
    "txPosYF" : .95
end create

draw(text_item_id)

setvalues text_item_id
    "txFontHeightF" : .015
    "txPosXF" : .7
    "txPosYF" : .86
    "txString" : "HIGH-QUALITY CHARACTERS USED BELOW"
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .7
    "txPosYF" : .5
    "txString" : "MEDIUM-QUALITY CHARACTERS USED BELOW"
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .58
    "txPosYF" : .81
        "txString" : "NhlNtxString"
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosYF" : .46
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .82
    "txPosYF" : .81
    "txString" : "RESULT"
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosYF" : .46
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .82
    "txPosYF" : .78
        "txString" : "------"
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosYF" : .43
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .58
        "txPosYF" : .78
        "txString" : "------------"
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosYF" : .43
end setvalues

draw(text_item_id)

;
; Below are examples using the function control characters. In each iteration
; of the loop a new string is set using NhlSetValues. In order to demonstrate
; what the value of NhlNtxString is the function code is temporarily changed 
; causing the entire contents of NhlNtxString to be drawn
;

do i=0,11
    if (i .eq. 0)
        setvalues text_item_id
            "txString" : "~L~A"
        end setvalues
    end if
    if (i .eq. 1)
        setvalues text_item_id
            "txString" : "~IGL~A"
        end setvalues
    end if
    if (i .eq. 2)
        setvalues text_item_id
            "txString" : "A~S~2~N~+B~S~2~N~"
        end setvalues
    end if
    if (i .eq. 3)
        setvalues text_item_id
            "txString" : "A~S~B"
        end setvalues
    end if
    if (i .eq. 4)
        setvalues text_item_id
            "txString" : "A~SPU~B"
        end setvalues
    end if
    if (i .eq. 5)
        setvalues text_item_id
            "txString" : "~GIU~+"
        end setvalues
    end if
    if (i .eq. 6)
        setvalues text_item_id
            "txString" : "~1045~"
        end setvalues
    end if
    if (i .eq. 7)
        setvalues text_item_id
            "txString" : "10~S~10~S~100"
        end setvalues
    end if
    if (i .eq. 8)
        setvalues text_item_id
            "txString" : "X~B1~2~S1~3"
        end setvalues
    end if
    if (i .eq. 9)
        setvalues text_item_id
            "txString" : "X~B1~2~S~3~N~Y~S~2"
        end setvalues
    end if
    if (i .eq. 10)
        setvalues text_item_id
            "txString" : "X~S~A~B~1~NN~ABC"
        end setvalues
    end if
    if (i .eq. 11)
        setvalues text_item_id
            "txString" : "1.3648~L1~410~S~-13"
        end setvalues
    end if

;
; Change function code so full string will be drawn
;
    setvalues text_item_id
        "txFuncCode" : "$"
        "txFontHeightF" : .01
        "txPosXF" : .58
        "txPosYF" : .78 - (i+1) * .02
    end setvalues

    draw(text_item_id)

;
; Change function code back so result string will be drawn
;
    setvalues text_item_id
        "txFuncCode" : "~"
        "txPosXF" : .82
    end setvalues

    draw(text_item_id)
;
; Change to medium font quality and set function code so full string will be
; drawn
;       
    setvalues text_item_id
        "txFontQuality" : "Medium"
        "txFuncCode" : "$"
        "txPosXF" : .58
        "txPosYF" : .42 - (i+1) * .02
    end setvalues

    draw(text_item_id)
;
; Change function code back to get result string to draw
;
    setvalues text_item_id
        "txFuncCode" : "~"
        "txPosXF" : .82
    end setvalues

    draw(text_item_id)

    setvalues text_item_id
        "txFontQuality" : "High"
    end setvalues
end do

; 
; Examples of setting font height.
;
setvalues text_item_id
    "txPosXF" : .25
    "txPosYF" : .86
    "txFontHeightF" : .01
    "txString" : "NhlNtxFontHeightF = .01"
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .25
    "txPosYF" : .84
    "txFontHeightF" : .015
    "txString" : "NhlNtxFontHeightF = .015"
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .25
    "txPosYF" : .80
    "txFontHeightF" : .02
    "txString" : "NhlNtxFontHeightF = .02"
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .25
    "txPosYF" : .75
    "txFontHeightF" : .025
    "txString" : "NhlNtxFontHeightF = .025"
end setvalues

draw(text_item_id)

;
; Examples setting the rotation angle of the text
;

setvalues text_item_id
    "txPosXF" : .26
    "txPosYF" : .47
    "txFontHeightF" : .015
    "txJust" :1
    "txString" : "NhlNtxAngleF = 0.0"
    "txAngleF" : 0.0
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .26
    "txPosYF" : .5
    "txString" : "NhlNtxAngleF = 45.0"
    "txAngleF" : 45.0
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .23
    "txPosYF" : .50
    "txString" : "NhlNtxAngleF = 90.0"
    "txAngleF" : 90.0
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .20
    "txPosYF" : .5
    "txString" : "NhlNtxAngleF = 135.0"
    "txAngleF" : 135.0
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .20
    "txPosYF" : .47
    "txString" : "NhlNtxAngleF = 180.0"
    "txAngleF" : 180.0
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .20
    "txPosYF" : .44
    "txString" : "NhlNtxAngleF = 225.0"
    "txAngleF" : 225.0
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .23
    "txPosYF" : .44
    "txString" : "NhlNtxAngleF = 270.0"
    "txAngleF" :270.0
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txPosXF" : .26
    "txPosYF" : .44
    "txString" : "NhlNtxAngleF = 315.0"
    "txAngleF" :315.0
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txAngleF" : 0.0
    "txPosXF" : .25
    "txPosYF" : .15
    "txJust" : 4
    "txString" : "NhltxJust = 4"
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txAngleF" : 0.0
    "txPosXF" : .25
    "txPosYF" : .18
    "txJust" : 1
    "txString" : "NhltxJust = 1"
end setvalues

draw(text_item_id)

setvalues text_item_id
    "txAngleF" : 0.0
    "txPosXF" : .25
    "txPosYF" : .12
    "txJust" : 7
    "txString" : "NhlNtxJust = 7"
end setvalues

draw(text_item_id)

;
; End of first frame
;
frame(workstation_id)

;
; Remainder of calls demonstrate the various fonts available for use
; with the text item. Note that the NhlNtxString values demonstrate a
; mid-string font change
;
j = 0.027

setvalues text_item_id
    "txFuncCode" : "$"
    "txFont" : 0
    "txJust" : 4
    "txFontHeightF" : .02
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "pwritx database : $F21$NhlNtxFont = 0"
end setvalues
j = j + 0.027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 1
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "default : $F21$NhlNtxFont = 1"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 2
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "cartographic roman : $F21$NhlNtxFont = 2"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 3
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "cartographic greek : $F21$NhlNtxFont = 3"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 4
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "simplex roman: $F21$NhlNtxFont = 4"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 5
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "simplex greek: $F21$NhlNtxFont = 5"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 6
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "simplex script: $F21$NhlNtxFont = 6"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 7
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "complex roman: $F21$NhlNtxFont = 7"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 8
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "complex greek: $F21$NhlNtxFont = 8"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 9
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "complex script: $F21$NhlNtxFont = 9"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 10
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "complex script: $F21$NhlNtxFont = 10"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 11
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "complex cyrillic: $F21$NhlNtxFont = 11"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 12
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "duplex roman: $F21$NhlNtxFont = 12"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 13
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "triplex roman: $F21$NhlNtxFont = 13"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 14
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "triplex italic: $F21$NhlNtxFont = 14"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 15
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "gothic german: $F21$NhlNtxFont = 15"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 16
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "gothic english: $F21$NhlNtxFont = 16"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 17
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "gothic italian: $F21$NhlNtxFont = 17"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 18
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "math symbols: $F21$NhlNtxFont = 18"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 19
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "symbols set1: $F21$NhlNtxFont = 19"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 20
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "symbols set2: $F21$NhlNtxFont = 20"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 21
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "helvetica: $F21$NhlNtxFont = 21"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 22
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "helvetica bold: $F21$NhlNtxFont = 22"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 25
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "times-roman : $F21$NhlNtxFont = 25"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 26
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "times-roman bold : $F21$NhlNtxFont = 26"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 29
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "courier : $F21$NhlNtxFont = 29"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 30
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "courier bold : $F21$NhlNtxFont = 30"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 33
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "greek : $F21$NhlNtxFont = 33"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 34
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "math-symbols : $F21$NhlNtxFont = 34"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 35
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "text-symbols : $F21$NhlNtxFont = 35"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 36
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "weather1 : $F21$NhlNtxFont = 36"
end setvalues
j = j + .027

draw(text_item_id)

setvalues text_item_id
    "txFont" : 37
    "txPosXF" : .5
    "txPosYF" : .95 - j
    "txString" : "weather2 : $F21$NhlNtxFont = 37"
end setvalues

draw(text_item_id)
;
; End of second and final frame
;
frame(workstation_id)

;
; clean up and close hlu library
;

delete(text_item_id)
delete(workstation_id)
delete(app_id)

;
; End NCL script.
;
end
