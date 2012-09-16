;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                     Copyright (C)  1998                               ;
;        University Corporation for Atmospheric Research                ;
;                     All Rights Reserved                               ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         nm15n.ncl
;
;   Author:       Fred Clare
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;   Date:         Wed Nov 18 16:42:45 MST 1998
;
;   Description:  Demo of extrapolation into data sparse regions
;                 using c_csa1xs.
;

load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"

begin

;----------- Define original data and initial weighting array -----------

  xi = (/0.0, 0.1, 0.2,  0.3,  0.4, 0.5, 0.6, 0.9, 1.0/)
  yi = (/0.0, 0.2, 0.4, -0.4, -1.0, 0.2, 0.5, 0.4, 0.0/)

  npts   = 101
  xo     = fspan(0.0,1.0,npts)  ; Create the output X coordinate array.

  xx = (/0.0, 1.0/) ; Create data for a polyline for marking
  yy = (/0.0, 0.0/) ; the Y = 0.0 line in each graph.

;--- Calculate approximated function values using different wieghts.

  smth = 0.0
  yo1 = csa1xs(xi,yi,-1.,8,smth,0,xo)
  smth = 1.0
  yo2 = csa1xs(xi,yi,-1.,8,smth,0,xo)

;---- Open workstation, define color map ---------------------------

  wks_type = "ncgm"
  wks = gsn_open_wks(wks_type,"nm15n")

  cmap = (/                 \
            (/1.,1.,1./),   \
            (/0.,0.,0./),   \
            (/1.,0.,0./),   \
            (/0.,0.,1./)    \
         /)
  gsn_define_colormap(wks,cmap)      

;---- Specify resources ---------------------------

  txres = True   ; Set up some variables for 
  xyres = True   ; creating three different
  gsres = True   ; resource lists.

  xyres@gsnFrame               = False     ; Don't advance the frame.

  xyres@tmXTBorderOn           = False     ; Don't draw top axis.
  xyres@tmXTOn                 = False     ; Don't draw top axis tick marks.
  xyres@tmBorderThicknessF     = 1.0       ; Default thickness is 2.0

  xyres@tmXBLabelFont          = 21        ; Change X label font.
  xyres@tmXBLabelFontHeightF   = 0.025     ; Change font size.
  xyres@tmXBMajorLengthF       = 0.015     ; Default is 0.02.
  xyres@tmXBMajorThicknessF    = 1.0       ; Default is 2.
  xyres@tmXBMinorLengthF       = 0.0075    ; Default is 0.01.
  xyres@tmXBMinorPerMajor      = 4         ; # of minor tick marks per major.
  xyres@tmXBMode               = "Manual"  ; Set tick mark mode.
  xyres@tmXBTickStartF         = 0.0
  xyres@tmXBTickEndF           = 1.0
  xyres@tmXBTickSpacingF       = 0.2
  xyres@tmXMajorGridThicknessF = 1.0       ; Default is 2.0

  xyres@tmYLLabelFont          = 21        ; See explanations for X axes
  xyres@tmYLLabelFontHeightF   = 0.025     ; resources.
  xyres@tmYLMajorLengthF       = 0.015
  xyres@tmYLMajorThicknessF    = 1.0
  xyres@tmYLMinorPerMajor      = 0
  xyres@tmYLMode               = "Manual"
  xyres@tmYLTickStartF         = -2.0
  xyres@tmYLTickEndF           =  1.0
  xyres@tmYLTickSpacingF       =  1.0
  xyres@tmYRBorderOn           = False     ; Don't draw right axis.
  xyres@tmYROn                 = False     ; Don't draw right axis tick marks.

  xyres@trXMinF   =  0.0
  xyres@trXMaxF   =  1.0                   ; Specify data limits.
  xyres@trYMinF   = -2.0
  xyres@trYMaxF   =  1.0

  xyres@vpHeightF = 0.28                   ; Specify size and location of plot.
  xyres@vpWidthF  = 0.80
  xyres@vpXF      = 0.13
  xyres@vpYF      = 0.86

;--------- First graph (note unexpected oscillation in the curve) ----------

  xy = gsn_xy(wks,xo,yo1,xyres)      ; Plot the approximzted curve created
                                     ; with equal weights.

  xyres@xyMarkLineMode = "Markers"   ; Set line mode to "Markers".
  xyres@xyMarkerColor  = 3           ; Set markers to blue.
  xyres@xyMarkerSizeF  = 0.025       ; Make markers larger.

  xy = gsn_xy(wks,xi,yi,xyres)       ; Mark the original points with blue Xs.

  txres@txFont        = 21           ; Change the default font.
  txres@txFontHeightF = 0.038        ; Set the font height.
  gsn_text(wks,xy,"smth = 0.",0.17,-1.20,txres)

  gsres@gsLineColor = 2              ; Set polyline color to red.
  gsn_polyline(wks,xy,xx,yy,gsres)   ; Draw polyline at Y=0.

;----------- Second graph, on same picture --------------------------------

  xyres@vpYF             = 0.43      ; Set Y location of plot.
  xyres@xyMarkLineMode   = "Lines"   ; Set line mode to "Lines".

  xy = gsn_xy(wks,xo,yo2,xyres)      ; Plot the approximated curve 
                                     ; created using a weight of 0.5 
                                     ; to the second input point.

  xyres@xyMarkLineMode   = "Markers" ; Set line mode to "Markers".
  xy = gsn_xy(wks,xi,yi,xyres)       ; Mark original points.

  txres@txFontHeightF = 0.038        ; Set font height.
  gsn_text(wks,xy,"smth = 1.",0.17,-1.20,txres)

  gsn_polyline(wks,xy,xx,yy,gsres)   ; Draw polyline at Y=0.

;------------- Main title ------------------------------------------

  txres@txFontHeightF = 0.04         ; Change the font height.
  gsn_text_ndc(wks,"~F21~Data sparse areas",.5,.95,txres) 

;------------- Plot and clean up  -----------------------------------
  frame(wks) 

  delete(xy)
  delete(txres)
  delete(gsres)
  delete(xyres)

end
