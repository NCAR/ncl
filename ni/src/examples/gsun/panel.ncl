load "gsn_code.ncl"
load "gsn_csm.ncl"

begin
  datadir=ncargpath("data")
  datafile=datadir + "/cdf/meccatemp.cdf"
  ncfile =  addfile (datafile,"r")

  t0 = ncfile->t(0,::4,::4)
  t1 = ncfile->t(1,::4,::4)
  t2 = ncfile->t(2,::4,::4)
  t3 = ncfile->t(3,::4,::4)

  type = "ps"
;   type@wkOrientation  = "portrait"
;   type@wkDeviceLowerX  =   0
;   type@wkDeviceLowerY  =  0
;   type@wkDeviceUpperX  = 700
;   type@wkDeviceUpperY  = 700
;  wks = gsn_open_wks(type,"gsn_cgd")
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
  mpres@sfXCStartV      = t0&lon(0)
  mpres@sfXCEndV        = t0&lon(dimsizes(t0&lon)-1)
  mpres@sfYCStartV      = t0&lat(0)
  mpres@sfYCEndV        = t0&lat(dimsizes(t0&lat)-1)
  mpres@cnFillOn        = True
  mpres@cnFillColors    = ispan(2,63,2)
  mpres@cnMinLevelValF  =  195.0
  mpres@cnMaxLevelValF  =  328.0
  mpres@cnLevelSpacingF = 5.50
  mpres@cnLinesOn       = False
  mpres@cnLineLabelsOn  = False
  mpres@cnHighLabelsOn  = False
  mpres@cnLowLabelsOn   = False
  mpres@cnInfoLabelOn   = False
  mpres@gsnDraw         = False
  mpres@gsnFrame        = False
  mpres@lbLabelBarOn    = False
  mp = new(4,graphic)
  mp(0) = gsn_csm_contour_map(wks,t0,mpres)

  mp(1) = gsn_csm_contour_map(wks,t1,mpres)

  mp(2) = gsn_csm_contour_map(wks,t2,mpres)

  mp(3) = gsn_csm_contour_map(wks,t3,mpres)

  res = True
  res@gsnPanelLabelBar = True
  res@lbLabelStride = 2
  res@gsnPanelFigureStrings = (/"one","two","three"/)
  res@amJust = "bottomleft"
  res@txFont = "helvetica-bold"
  gsn_panel(wks,mp,(/2,2/),res)
end
