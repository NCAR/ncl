load "gsn_code.ncl"

begin
  wks = gsn_open_wks("x11","gsn_cgd")

  mpres = True
  mpres@gsnDraw   = False
  mpres@gsnFrame  = False
  
  mp0 = gsn_map(wks,"CylindricalEquidistant",mpres)
  mp1 = mp0
  mp2 = mp0
  mp3 = mp0

  res = True
  gsn_panel_spec(wks,(/mp0,mp1/),(/2,1/),res)
  gsn_panel(wks,(/mp0,mp1/),(/2,1/),res)

end
