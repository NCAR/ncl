load "gsn_code.ncl"

begin
  datadir=ncargpath("data")
  datafile=datadir + "/cdf/meccatemp.cdf"
  ncfile =  addfile (datafile,"r")

  wks = gsn_open_wks("x11","gsn_cgd")
  setvalues wks
    "wkColorMap":(/(/1.000,1.000,1.000/), (/.000,.000,.000/), \
                   (/.700,.700,.700/),    (/.650,.650,.700/), \
                   (/.610,.600,.700/),    (/.550,.550,.700/), \
                   (/.560,.500,.700/),    (/.450,.450,.700/), \
                   (/.420,.400,.700/),    (/.350,.350,.700/), \
                   (/.300,.300,.700/),    (/.250,.250,.700/), \
                   (/.200,.200,.700/),    (/.150,.150,.700/), \
                   (/.100,.100,.700/),    (/.050,.050,.700/), \
                   (/.000,.000,.700/),    (/.000,.050,.700/), \
                   (/.000,.100,.700/),    (/.000,.150,.700/), \
                   (/.000,.200,.700/),    (/.000,.250,.700/), \
                   (/.000,.300,.700/),    (/.000,.350,.700/), \
                   (/.000,.400,.700/),    (/.000,.450,.600/), \
                   (/.000,.500,.500/),    (/.000,.550,.400/), \
                   (/.000,.600,.300/),    (/.000,.650,.200/), \
                   (/.000,.700,.100/),    (/.000,.725,.000/), \
                   (/.000,.690,.000/),    (/.030,.685,.000/), \
                   (/.060,.680,.000/),    (/.100,.575,.000/), \
                   (/.130,.570,.000/),    (/.160,.565,.000/), \
                   (/.550,.550,.000/),    (/.555,.545,.000/), \
                   (/.560,.530,.000/),    (/.565,.485,.000/), \
                   (/.570,.420,.000/),    (/.675,.375,.000/), \
                   (/.680,.330,.000/),    (/.690,.300,.000/), \
                   (/.700,.285,.000/),    (/.700,.270,.000/), \
                   (/.700,.260,.000/),    (/.700,.240,.000/), \
                   (/.700,.180,.000/),    (/.700,.130,.000/), \
                   (/.700,.120,.000/),    (/.700,.100,.000/), \
                   (/.700,.090,.000/),    (/.750,.090,.000/), \
                   (/.800,.090,.000/),    (/.830,.070,.000/), \
                   (/.870,.050,.000/),    (/.900,.030,.000/), \
                   (/.950,.010,.000/),    (/.990,.000,.000/), \
                   (/1.00,.000,.000/),    (/1.00,.000,.000/) /)
  end setvalues

  mpres = True
  mpres@sfXCStartV      = ncfile->lon(0)  
  mpres@sfXCEndV        = ncfile->lon(dimsizes(ncfile->lon)-1)
  mpres@sfYCStartV      = ncfile->lat(0)
  mpres@sfYCEndV        = ncfile->lat(dimsizes(ncfile->lat)-1)
  mpres@cnFillOn        = True
  mpres@cnFillColors    = ispan(2,63,2)
  mpres@pmLabelBarSide  = "bottom"
  mpres@cnMinLevelValF  =  195.0
  mpres@cnMaxLevelValF  =  328.0
  mpres@cnLevelSpacingF = 5.50
  mpres@cnLinesOn       = False
  mpres@cnLineLabelsOn  = False
  mpres@cnHighLabelsOn  = False
  mpres@cnLowLabelsOn   = False
  mpres@cnInfoLabelOn   = False
  mpres@tiMainString    = ":F26:January Global Surface Temperature - Day 1"
  mpres@gsnDraw         = False
  mpres@gsnFrame        = False
;  mpres@pmLabelBarDisplayMode = "Always"
  mp1 = gsn_contour_map(wks,ncfile->t(0,:,:),mpres)

  mpres@tiMainString = ":F26:January Global Surface Temperature - Day 2"
  mp2 = gsn_contour_map(wks,ncfile->t(1,:,:),mpres)

  mpres@tiMainString = ":F26:January Global Surface Temperature - Day 3"
  mp3 = gsn_contour_map(wks,ncfile->t(2,:,:),mpres)

  mpres@tiMainString = ":F26:January Global Surface Temperature - Day 4"
  mp4 = gsn_contour_map(wks,ncfile->t(3,:,:),mpres)

  gsn_panel(wks,(/mp1,mp2,mp3/),(/3,1/),False)

  getvalues mp1@contour
    "tiMainFontHeightF" : fheight
  end getvalues

  setvalues mp1@contour
    "tiMainOn" : False
  end setvalues

  setvalues mp2@contour
    "tiMainOn" : False
  end setvalues

  setvalues mp3@contour
    "tiMainOn" : False
  end setvalues

  res@gsnFrame = False
  res = True
  res@gsnPanelBottom = .06
  res@gsnPanelTop = .94
  gsn_panel(wks,(/mp1,mp2,mp3/),(/3,1/),res)
  txres = True
  txres@txFontHeightF = fheight
  gsn_text_ndc(wks,":F26:January Global Surface Temperature - Days 1-4",.5,.95,txres)

  getvalues mp1@contour
    "cnLevels" : levels
  end getvalues

  frame(wks)

  lbres = True
  lbres@lbOrientation  = "horizontal"
  lbres@lbFillColors   = ispan(2,63,2)
  lbres@lbPerimOn      = False
  lbres@lbBoxLinesOn   = False
  lbres@lbWidthF  =  .6
  lbres@lbHeightF  =  .1
  gsn_labelbar_ndc(wks,dimsizes(levels),levels,.1,.6,lbres)

  delete(lbres)
  lbres = True
  lbres@lbPerimOn = False         ; Turn off labelbar perimeter.
  lbres@lbLabelFont = "helvetica" ; Change font to helvetica.
  gsn_labelbar_ndc(wks,3,(/"one","two","three"/),0.2,0.8,lbres)
  frame(wks)
end
