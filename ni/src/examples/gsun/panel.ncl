;
; vpwidth     : width of the plot only, and not all of its annotations
; plot_width  : total width of plot with all of its annotations
; total_width : plot_width plus white space on both sides
;
procedure panel(plot[*]:graphic,dims[2]:float)
local xwsp, ywsp, rows, cols, xrows, xcols, top, bottom, left, right, \
vpx, vpy, vpwidth, vpheight, bb, plot_width, plot_height, dx, dy, dux, duy, \
total_width, total_height, col_scale, row_scale, scale, xpos, ypos, xsp, ysp, \
new_plot_height, new_plot_width, new_total_height, new_total_width, \
i, j, old_vpx, old_vpy, new_vpx, new_vpy, old_vpwidth, old_vpheight, \
new_vpheight, new_vpwidth
begin
  rows = floattointeger(dims(0))
  cols = floattointeger(dims(1))
  xrows = dims(0)
  xcols = dims(1)

  getvalues plot(0)
    "vpXF"      : vpx       ; Get original view port coordinates.
    "vpYF"      : vpy
    "vpWidthF"  : vpwidth
    "vpHeightF" : vpheight
  end getvalues

  bb = NhlGetBB(plot(0))   ; Get bounding box of plot with
  top    = bb(0)           ; all of its annotations.
  bottom = bb(1)
  left   = bb(2)
  right  = bb(3)

  plot_width  = right - left     ; Calculate total width of plot.
  plot_height = top - bottom     ; Calculate total height of plot.

  xwsp = 0.05
  ywsp = 0.05
  if(isatt(plot,"PercentXSpace"))
    xwsp = plot@PercentXSpace
    if(xwsp.le.0.or.xwsp.ge.1.)
      print("Attribute PercentXSpace must be between 0 and 1.")
      print("Defaulting to 0.05.")
      xwsp = 0.05
    end if
  end if
  if(isatt(plot,"PercentYSpace"))
    ywsp = plot@PercentYSpace
    if(ywsp.le.0.or.ywsp.ge.1.)
      print("Attribute PercentYSpace must be between 0 and 1.")
      print("Defaulting to 0.05.")
      ywsp = 0.05
    end if
  end if

  dx = xwsp * plot_width         ; White space is a percentage of total
  dy = ywsp * plot_height        ; width and height.

  total_width  = 2.*dx + plot_width   ; Calculate total width and height
  total_height = 2.*dy + plot_height  ; with white space added.

;
; We want:
;
;   ncols * col_scale * total_width  <= 1.0
;   nrows * row_scale * total_height <= 1.0
;
; By taking the minimum of these two, we get the scale
; factor that we need to fit all plots on a page.
;
  col_scale = min((/1./(xcols*total_width),1./))
  row_scale = min((/1./(xrows*total_height),1./))
  scale = min((/col_scale,row_scale/))

  new_plot_width  = scale*plot_width    ; Calculate new width
  new_plot_height = scale*plot_height   ; and height.

  dx = xwsp * new_plot_width   ; Calculate new white space.
  dy = ywsp * new_plot_height

  new_total_width  = 2.*dx + new_plot_width   ; Calculate new total width
  new_total_height = 2.*dy + new_plot_height  ; and height with white space.

  xsp = 1. - new_total_width*cols   ; Calculate total amt of white space
  ysp = 1. - new_total_height*rows  ; left in both X and Y directions.

  dux = scale * (vpx - left) ; Calculate distance from plot's left position
                             ; to its leftmost annotation
  duy = scale * (top - vpy) ; Calculate distance from plot's top position
                            ; to its topmost annotation.

  xpos =     xsp/2.+new_total_width*ispan(0,cols-1,1)   ; Calculate X/Y
  ypos = 1.-(ysp/2.+new_total_height*ispan(0,rows-1,1)) ; pos of each plot.

;
; Loop through each plot and draw it in the new scaled-down size.
;
  do i = 0,rows-1
    do j = 0,cols-1
      if(i*cols+j .lt. dimsizes(plot))
        if(.not.ismissing(plot(i*cols+j)))
          getvalues plot(i*cols+j)
            "vpXF"      : old_vpx
            "vpYF"      : old_vpy
            "vpWidthF"  : old_vpwidth
            "vpHeightF" : old_vpheight
          end getvalues

          new_vpx      = xpos(j) + dx + dux
          new_vpy      = ypos(i) - dy - duy
          new_vpwidth  = scale*old_vpwidth 
          new_vpheight = scale*old_vpheight

          setvalues plot(i*cols+j)
            "vpXF"      : new_vpx
            "vpYF"      : new_vpy
            "vpWidthF"  : new_vpwidth
            "vpHeightF" : new_vpheight
          end setvalues

          draw(plot(i*cols+j))
          setvalues plot(i*cols+j)
            "vpXF"      : old_vpx
            "vpYF"      : old_vpy
            "vpWidthF"  : old_vpwidth
            "vpHeightF" : old_vpheight
          end setvalues

        end if
      else
        return
      end if
    end do
  end do
end
