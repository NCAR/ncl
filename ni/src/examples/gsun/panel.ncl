load "gsn_code.ncl"

begin
  wks = gsn_open_wks("x11","gsn_cgd")

  mpres = True
  mpres@gsnDraw   = False
  mpres@gsnFrame  = False
  
  mp0 = gsn_map(wks,"CylindricalEquidistant",mpres)

  mpres@mpCenterLonF = 180
  mp1 = gsn_map(wks,"CylindricalEquidistant",mpres)

  res = True
;  gsn_panel_save(wks,(/mp0,mp1/),(/2,1/),res)
  gsn_panel(wks,(/mp0,mp1/),(/2,1/),res)
  output_gif((/mp0,mp1/),"output.gif",700,1)
end
