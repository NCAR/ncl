load "gsn_code.ncl"
load "gsn_csm.ncl"

begin
  wks = gsn_open_wks("x11","gsn_cgd")

  mpres = True
  mpres@mpFillOn  = True
  mpres@gsnDraw   = False
  mpres@gsnFrame  = False
  mp = new(4,graphic)
  mp(0) = gsn_map(wks,"CylindricalEquidistant",mpres)
  mp(1) = gsn_map(wks,"CylindricalEquidistant",mpres)
  mp(2) = gsn_map(wks,"CylindricalEquidistant",mpres)
  mp(3) = gsn_map(wks,"CylindricalEquidistant",mpres)

  res = True
  res@gsnPanelFigureStrings = (/"1a","1b","2a","2b"/)
  res@amJust = "topright"
  gsn_panel(wks,mp,(/2,2/),res)
end
