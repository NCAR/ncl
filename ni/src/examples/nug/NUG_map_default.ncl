;-----------------------------------------------------------------
;  NCL User Guide Example:   NUG_map_default.ncl
;
;  KMF
;-----------------------------------------------------------------
; These load commands are not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 

begin

  wks = gsn_open_wks("png","NUG_map_default")   ;-- define the workstation

  map = gsn_csm_map(wks, False)                  ;-- draw the map

end
