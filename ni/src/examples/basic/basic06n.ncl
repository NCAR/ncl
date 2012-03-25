; $Id: basic06n.ncl,v 1.8 2010-03-15 22:49:23 haley Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         basic06n.ncl
;
;   Author:       Fred Clare
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;
;   Date:         Wed May 24 12:54:47 MDT 1995
;
;   Description:  This NCL script demonstrates how to position 
;                 objects on an output device and how to change 
;                 their sizes.  A simple color table is also defined 
;                 and used for changing the color of a curve in an 
;                 XyPlot.  The script begins with two procedures - 
;                 one for drawing plot objects and one for drawing 
;                 text objects.
;

procedure draw_plot(id:graphic, x:float, y:float, scale:float)

;
; This procedure takes the plot object with identifier "id" and 
; draws it centered at coordinate (x,y) and scaled by "scale".  
; The original plot object is returned unchanged.
;

begin
  getvalues id
    "vpXF": x_ref
    "vpYF": y_ref
    "vpWidthF": width_ref
    "vpHeightF": height_ref
  end getvalues

  setvalues id
    "vpXF": x - 0.5*width_ref*scale
    "vpYF": y + 0.5*height_ref*scale
    "vpWidthF": width_ref*scale
    "vpHeightF": height_ref*scale
  end setvalues

  draw(id)

  setvalues id
    "vpXF": x_ref
    "vpYF": y_ref
    "vpWidthF": width_ref
    "vpHeightF": height_ref
  end setvalues

end

procedure draw_text(id:graphic, x:float, y:float, height:float)

;
; This procedure takes the text string in the object identified by "id"
; and draws it centered at coordinate (x,y) with a height of "height".
;

begin
  getvalues id
    "txPosXF": xpos
    "txPosYF": ypos
    "txFontHeightF": fheight
  end getvalues

  setvalues id
    "txPosXF": x
    "txPosYF": y
    "txFontHeightF": height
  end setvalues

  draw(id)

  setvalues id
    "txPosXF": xpos
    "txPosYF": ypos
    "txFontHeightF": fheight
  end setvalues

end

;
; Main program.  
;

;
; Define a simple color map (color index 0 defines the background color).
;

cmap = (/ (/1.0, 1.0, 1.0/), \
	 (/0.0, 0.0, 1.0/), \
	 (/0.0, 1.0, 0.0/), \
	 (/1.0, 0.0, 0.0/) /)

;
; Set the display. Default is to display output to an X workstation.
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object with default colormap.
;
  xwork_id = create "simple" ncgmWorkstationClass defaultapp
      "wkMetaName" : "basic06n.ncgm"
      "wkColorMap"         : cmap
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X11 output workstation and use the above color map.
;
  xwork_id = create "simple" windowWorkstationClass defaultapp
    "wkPause" : "True"
    "wkColorMap"         : cmap
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
  xwork_id = create "simple" psWorkstationClass defaultapp
    "wkPSFileName"   : "basic06n.ps"
    "wkColorMap"     : cmap
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
  xwork_id = create "simple" pdfWorkstationClass defaultapp
    "wkPDFFileName"   : "basic06n.pdf"
    "wkColorMap"     : cmap
  end create
end if
if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
  xwork_id = create "simple" documentWorkstationClass defaultapp
    "wkFileName" : "basic06n"
    "wkFormat"   : wks_type
    "wkColorMap" : cmap
  end create
end if
if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
  xwork_id = create "simple" imageWorkstationClass defaultapp
    "wkFileName" : "basic06n"
    "wkFormat"   : wks_type
    "wkColorMap" : cmap
  end create
end if

;
; Create data for an XyPlot
;

data_id = create "xyData" coordArraysClass defaultapp
    "caXArray": (/0.0, 0.1, 0.5, 0.9, 1.0, 0.9, 0.5, 0.1, 0.0/)
    "caYArray": (/0.5, 0.9, 1.0, 0.9, 0.5, 0.1, 0.0, 0.1, 0.5/)
end create

;
; Create a simple XyPlot object with no labels or borders.  The
; parent for this object is xwork_id, hence it will be sent to
; the workstation identified by xwork_id when the NCL draw procedure
; is invoked on it.
;

box_id = create "Box" xyPlotClass xwork_id
    "tmXBBorderOn": "False"
    "tmXTBorderOn": "False"
    "tmYLBorderOn": "False"
    "tmYRBorderOn": "False"
    "tmXBOn": "False"
    "tmXTOn": "False"
    "tmYLOn": "False"
    "tmYROn": "False"
    "vpXF": 0.
    "vpYF": 1.
    "vpWidthF": 1.
    "vpHeightF": 1.
end create

;
; Create a TextItem object.
;

text_id = create "Text" textItemClass xwork_id
    "txPosXF": 0.5
    "txPosYF": 0.5
    "txFont": 26
end create

;
; Add the data identified by data_id to the XyPlot.
;

dataspec = NhlAddData(box_id,"xyCoordData",data_id)

;
; Draw three labeled boxes at different sizes and in different positions
; and with different colors.
;

do i=1,3
  xpos = -0.05*i^2 + 0.5*i - 0.20
  ypos = 1.-xpos

;
; Specify a text string and its color.
;
  setvalues text_id
    "txString": "Box" + i
    "txFontColor": 4-i 
  end setvalues

;
; Set the XyPlot curve color.
;
  setvalues dataspec
    "xyMonoLineColor": "True"
    "xyLineColor": i
  end setvalues

;
; Draw box and text.
;
  draw_plot(box_id, xpos, ypos, 0.36-0.09*(i-1))
  draw_text(text_id, xpos, ypos, 0.08-0.02*(i-1))
end do

frame(xwork_id)
