;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_map_default.ncl
;
;  KMF
;-----------------------------------------------------------------
begin

  wks = gsn_open_wks("png","plot_map_default")   ;-- define the workstation

  map = gsn_csm_map(wks, False)                  ;-- draw the map

end
