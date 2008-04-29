;
; This is a convenient script for quickly drawing a colormap so you
; can see what it looks like.
;
; You must run this script with:
;
;   ncl 'cmap="xxx"' draw_cmap.ncl
;
; where "xxx" is the name of the colormap, like "uniform".
;

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin
  if(.not.isvar("cmap")) then
    print("Error: you must indicate a color map with the 'cmap' variable.")
  end if

  wks = gsn_open_wks("x11","colormaps")

  gsn_define_colormap(wks,cmap)
  gsn_draw_colormap(wks)
end
