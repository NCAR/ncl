;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1998                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         nm13n.ncl
;
;   Author:       Fred Clare
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;   Date:         Thu Nov 12 11:49:40 MST 1998
;
;   Description:  This program illustrates the use of csa1s.
;

load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"

begin

;----------- Define original data --------------------------------------

  xi = (/0.0, 0.1,  0.2,  0.3, 0.5,  0.6, 0.65,  0.8,  0.9, 1./)
  yi = (/0.0, 0.8, -0.9, -0.9, 0.9,  1.0, 0.90, -0.8, -0.8, 0./)

  npts   = 101
  xo     = fspan(0.0,1.0,npts)  ; Create the output X coordinate array.

  xx = (/0.0, 1.0/) ; Create data for a polyline for marking
  yy = (/0.0, 0.0/) ; the Y = 0.0 line in each graph.

;--- Calculate approximated function values using differing number of knots.

  knots = 4
  yo4 = csa1s(xi,yi,knots,xo)
  knots = 7
  yo7 = csa1s(xi,yi,knots,xo)
  knots = 9
  yo9 = csa1s(xi,yi,knots,xo)

;---- Open workstation, define color map ---------------------------

  wks_type = "ncgm"

  wks = gsn_open_wks(wks_type,"nm13n")

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
  xyres@tmYLTickStartF         = -1.2
  xyres@tmYLTickEndF           =  1.2
  xyres@tmYLTickSpacingF       =  0.6
  xyres@tmYRBorderOn           = False     ; Don't draw right axis.
  xyres@tmYROn                 = False     ; Don't draw right axis tick marks.

  xyres@trXMaxF   =  1.0                   ; Specify data limits.
  xyres@trXMinF   =  0.0
  xyres@trYMaxF   =  1.2
  xyres@trYMinF   = -1.2

  xyres@vpHeightF = 0.20                   ; Specify size and location of plot.
  xyres@vpWidthF  = 0.80
  xyres@vpXF      = 0.13
  xyres@vpYF      = 0.88

;----------- First graph --------------------------------------------

  xy = gsn_xy(wks,xo,yo4,xyres)      ; Plot the approximated curve created
                                     ; using 4 knots.

  xyres@xyMarkLineMode = "Markers"   ; Set line mode to "Markers".
  xyres@xyMarkerColor  = 3           ; Set markers to blue.
  xyres@xyMarkerSizeF  = 0.025       ; Make markers larger.

  xy = gsn_xy(wks,xi,yi,xyres)       ; Mark the original points with blue Xs.

  txres@txFont        = 21           ; Change the default font.
  txres@txFontHeightF = 0.03         ; Set the font height.
  gsn_text(wks,xy,"knots = 4",0.25,1.0,txres) ; Label the plot.

  gsres@gsLineColor = 2              ; Set polyline color to red.
  gsn_polyline(wks,xy,xx,yy,gsres)   ; Draw polyline at Y=0.

;----------- Second graph, on same picture --------------------------------

  xyres@vpYF             = 0.58      ; Set Y location of plot.
  xyres@xyMarkLineMode   = "Lines"   ; Set line mode to "Lines".

  xy = gsn_xy(wks,xo,yo7,xyres)      ; Plot the approximzted curve 
                                     ; created using 7 knots.

  xyres@xyMarkLineMode   = "Markers" ; Set line mode to "Markers".
  xy = gsn_xy(wks,xi,yi,xyres)       ; Mark original points.

  txres@txFontHeightF = 0.03         ; Set font height.
  gsn_text(wks,xy,"knots = 7",0.25,1.0,txres) ; Label the plot.

  gsn_polyline(wks,xy,xx,yy,gsres)   ; Draw polyline at Y=0.


;----------- Third graph, on same picture --------------------------------

  xyres@vpYF             = 0.28      ; Set Y location of plot.
  xyres@xyMarkLineMode   = "Lines"   ; Set line mode.

  xy = gsn_xy(wks,xo,yo9,xyres)      ; Plot the approximated curve created
                                     ; using 9 knots.

  xyres@xyMarkLineMode   = "Markers" ; Set line mode.
  xy = gsn_xy(wks,xi,yi,xyres)       ; Plot the original points with markers.

  txres@txFontHeightF = 0.03         ; Set font height.
  gsn_text(wks,xy,"knots = 9",0.25,1.0,txres) ; Label the plot.

  gsn_polyline(wks,xy,xx,yy,gsres)   ; Draw polyline at Y=0.

;------------- Main title ------------------------------------------

  txres@txFontHeightF = 0.04         ; Change the font height.
  gsn_text_ndc(wks,"Demo for csa1s",.5,.95,txres) 

;------------- Plot and clean up  -----------------------------------
  frame(wks) 

  delete(xy)
  delete(txres)
  delete(gsres)
  delete(xyres)

end
