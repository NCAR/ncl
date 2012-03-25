; $Id: basic08n.ncl,v 1.5 2010-03-15 22:49:23 haley Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1995                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         basic08n.ncl
;
;   Author:       David Brown
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;
;   Date:         Wed May 24 12:54:47 MDT 1995
;
;   Description:  This NCL script demonstrates how to draw
;		  a plot object including any annotations
;		  outside its viewport within a predefined 
;		  bounded area of NDC space. The procedure 
;		  draw_bounded_plot is intended to be useful in any
;		  context where it is desired to keep an entire plot
;		  within predetermined boundaries.
;

procedure draw_bounded_plot(id:graphic, keep_aspect: logical,
			left:float, right:float, bottom:float, top:float)

;
; This procedure takes the plot object with identifier "id" and 
; draws it within the NDC boundaries represented by left, right,
; top, and bottom. If keep_aspect is True the aspect ratio of the plot
; is preserved: the plot fills the extent of the limiting dimension and
; is centered within the extent of the other dimension. If keep_aspect is
; False, the aspect ratio is distorted in order to fill as much of the
; space as possible, given certain limitations in the ability of some
; HLU objects to distort themselves to any arbitrary aspect ratio.
;

local x_save,y_save,width_save,height_save,
      bb,bb_height,bb_width,frame_height,frame_width,
      factor,x_off,y_off,x,y,width,height
begin

  getvalues id
    "vpXF": x_save
    "vpYF": y_save
    "vpWidthF": width_save
    "vpHeightF": height_save
  end getvalues

  bb = NhlGetBB(id)
  bb_height = bb(0) - bb(1)
  bb_width = bb(3) - bb(2)
  frame_height = top - bottom
  frame_width = right - left
  x = x_save
  y = y_save
  height = height_save
  width = width_save

  if (.not. keep_aspect) then
	factor = frame_width / bb_width
	width = width * factor
	x_off = (x - bb(2)) * factor
	x = left + x_off
	factor = frame_height / bb_height
	height = height * factor
	y_off = (y - bb(0)) * factor
	y = top + y_off
	setvalues id
		"vpXF": x
    		"vpYF": y
		"vpWidthF": width
		"vpHeightF": height
	end setvalues
	getvalues id
		"vpXF": x
		"vpYF": y
		"vpWidthF": width
		"vpHeightF": height
	end getvalues
	bb = NhlGetBB(id)
	bb_height = bb(0) - bb(1)
	bb_width = bb(3) - bb(2)
  end if
	
  if (bb_height / bb_width .lt. frame_height / frame_width) then
;	width is limiting dimension

	factor = frame_width / bb_width
	width = width * factor
	height = height * factor
	x_off = (x - bb(2)) * factor
	y_off = (y - bb(0)) * factor
	x = left + x_off
	bb_height = bb_height * factor
	y = top + y_off - 0.5 * (frame_height - bb_height)
  else
;	height is limiting dimension

	factor = frame_height / bb_height
	height = height * factor
	width = width * factor
	x_off = (x - bb(2)) * factor
	y_off = (y - bb(0)) * factor
	bb_width = bb_width * factor
	x = left + x_off + 0.5 * (frame_width - bb_width)
	y = top + y_off
  end if

  setvalues id
    "vpXF": x
    "vpYF": y
    "vpWidthF": width
    "vpHeightF": height
  end setvalues

  draw(id)

  setvalues id
    "vpXF": x_save
    "vpYF": y_save
    "vpWidthF": width_save
    "vpHeightF": height_save
  end setvalues

end

begin
;
; Main program.  
;

  wks_type = "x11"

  if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object with default colormap.
;
    wid = create "basic08" ncgmWorkstationClass defaultapp
        "wkMetaName" : "basic08n.ncgm"
    end create
  end if
  if (str_lower(wks_type).eq."x11") then
;
; Create an X11 output workstation and use the above color map.
;
    wid = create "basic08" windowWorkstationClass defaultapp
      "wkPause" : "True"
    end create
  end if
  if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
    wid = create "basic08" psWorkstationClass defaultapp
      "wkPSFileName"   : "basic08n.ps"
    end create
  end if
  if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
    wid = create "basic08" pdfWorkstationClass defaultapp
      "wkPDFFileName"   : "basic08n.pdf"
    end create
  end if
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wid = create "basic08" documentWorkstationClass defaultapp
      "wkFileName" : "basic08n"
      "wkFormat"   : wks_type
    end create
  end if
  if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wid = create "basic08" imageWorkstationClass defaultapp
      "wkFileName" : "basic08n"
      "wkFormat"   : wks_type
    end create
  end if

;
;
; create a LogLinPlot with a viewport that fill the viewspace. This will
; be used for drawing immediate mode polylines indicating the intended
; boundary of each plot object.
;

  ll_id = create "loglin" logLinPlotClass wid
	"vpXF" : 0.0
 	"vpYF" : 1.0
	"vpHeightF" : 1.0
	"vpWidthF" : 1.0
  end create

;
; Set GraphicStyle resources to modify the immediate mode line attributes.
;
  getvalues wid
    "wkDefGraphicStyleId": gid
  end getvalues

  setvalues gid
	"gsLineColor" : 2
	"gsLineDashPattern" : 1
  end setvalues

;
; Create an empty ContourPlot with a Title, a LabelBar, and a Legend.
; Note that the viewport is square and covers the whole NDC space, 
; meaning that if the plot were drawn as created, all annotations exterior
; to the viewport would be outside the viewspace and therefore clipped.
; 

  cn_id = create "contour" contourPlotClass wid
	"pmLabelBarDisplayMode" : "always"
	"pmLegendDisplayMode" : "always"
	"tiMainString" : "bounded contour plot"
	"vpXF" : 0.0
 	"vpYF" : 1.0
	"vpHeightF" : 1.0
	"vpWidthF" : 1.0
  end create

;
; The first frame illustrates drawing the plot with a 5% margin around
; the viewable area. Draw an immediate mode line indicating the boundary 
; that defines the margin
;

  x = (/ 0.05, 0.95, 0.95, 0.05, 0.05 /)
  y = (/ 0.05, 0.05, 0.95, 0.95, 0.05 /)
  NhlNDCPolyline(ll_id,gid,x,y)

;
; Draw the plot with the desired boundary parameters.
;
  draw_bounded_plot(cn_id, True, 0.05, 0.95, 0.05, 0.95)
  frame(wid)

;
; The second frame illustrates use of the draw_bounded_plot procedure
; to place several plots with varying aspect ratios in a single frame.
;
  x = (/ 0.025, 0.475, 0.475, 0.025, 0.025 /)
  y = (/ 0.525, 0.525, 0.975, 0.975, 0.525 /)
  NhlNDCPolyline(ll_id,gid,x,y)

;
; Set the ContourPlot viewport so that the width is twice the height.
; (The absolute numbers are not important here, only the ratio matters.)
;

  setvalues cn_id
	"vpWidthF" : 0.6
	"vpHeightF" : 0.3
	"tiMainString" : "width is limiting dimension"
  end setvalues

;
; Draw the plot with the desired boundary parameters.
;
  draw_bounded_plot(cn_id, True, 0.025, 0.475, 0.525, 0.975)

;
; Draw an immediate mode line indicating the desired boundary of the
; second plot.
;

  x = (/ 0.525, 0.975, 0.975, 0.525, 0.525 /)
  y = (/ 0.525, 0.525, 0.975, 0.975, 0.525 /)
  NhlNDCPolyline(ll_id,gid,x,y)

;
;
; Set the ContourPlot viewport so that the height is twice the width.
;

  setvalues cn_id
	"vpWidthF" : 0.3
	"vpHeightF" : 0.6
	"tiMainString" : "height is limiting dimension"
  end setvalues

;
; Draw the plot with the desired boundary parameters.
;

  draw_bounded_plot(cn_id, True, 0.525, 0.975, 0.525, 0.975)

;
; Draw an immediate mode line indicating the desired boundary of the
; third plot.
;
  x = (/ 0.125, 0.875, 0.875, 0.125, 0.125 /)
  y = (/ 0.1, 0.1, 0.4, 0.4, 0.1 /)
  NhlNDCPolyline(ll_id,gid,x,y)

;
; For this plot the aspect ratio is distorted in order to fill as much as 
; possible of the desired area. Note that the space is not completely filled.
; This is because a number of factors affecting the final aspect ratio,
; such as the text size used for titles, are determined based on only
; one of the viewport's dimensions. 
;
  setvalues cn_id
	"tiMainString" : "distort aspect ratio to fill area"
	"vpHeightF" : 1.0
	"vpWidthF" : 1.0
  end setvalues

;
; Draw the plot with the desired boundary parameters.
;

  draw_bounded_plot(cn_id, False, 0.125, 0.875, 0.1, 0.4)
  frame(wid)

;
; clean up 
;
  delete(x)
  delete(y)
  delete(wid)

end
