load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsun_scripts.ncl"

begin
  N = 40
  z = new((/N,N/),float)
  len_dims = new((/2/),integer)
;
; These are the geographical areas we want to fill.
;
  fill_specs = (/"water","land"/)
;
; These are the geographical areas we want to mask.
;
  mask_specs = (/\
  "algeria","angola","angola-exclave-called-cabinda","benin","botswana",\
  "burundi","cameroon","central-african-republic","chad","congo","djibouti",\
  "egypt","equatorial-guinea","ethiopia","gabon","gambia","ghana","guinea",\
  "guinea-bissau","ivory-coast","kenya","lesotho","liberia","libya",\
  "madagascar","malawi","mali","mauritania","mauritius","morocco",\
  "mozambique","namibia","niger","nigeria","rwanda","senegal","sierra-leone",\
  "somalia","south-africa","sudan","swaziland","tanzania","togo","tunisia",\
  "uganda","upper-volta","western-sahara","zaire","zambia","zimbabwe"/)

  wks = ncl_open_wks("ncgm","gsun08n")

  cmap = (/(/1.00,1.00,1.00/),(/0.00,0.00,0.00/),(/0.70,0.70,0.70/),(/0.75,0.50,1.00/),(/0.50,0.00,1.00/),(/0.00,0.00,1.00/),(/0.00,0.50,1.00/),(/0.00,1.00,1.00/),(/0.00,1.00,0.60/),(/0.00,1.00,0.00/),(/0.70,1.00,0.00/),(/1.00,1.00,0.00/),(/1.00,0.75,0.00/),(/1.00,0.38,0.38/),(/1.00,0.00,0.38/),(/1.00,0.00,0.00/)/)
  ncl_define_colormap(wks,cmap)
  len_dims(0) = N
  len_dims(1) = N
  filedir = getenv("NCARG_ROOT") + "/lib/ncarg/data/asc/cn12n.asc"
  z = asciiread(filedir,len_dims,"float")

  resources = True
  resources@mpMaskAreaSpecifiers =   mask_specs
  resources@mpFillAreaSpecifiers =   fill_specs
  resources@mpAreaMaskingOn =              1
  resources@mpFillBoundarySets =           "NoBoundaries"
  resources@mpFillOn =                     True
  resources@mpSpecifiedFillColors =        (/7,2/)
  resources@mpEllipticalBoundary =         True
  resources@mpProjection =                 "Orthographic"
  resources@mpCenterLatF =                 20.0
  resources@mpCenterLonF =                 14.0
  resources@mpLabelsOn =                   False
  resources@mpGridSpacingF =               10
  resources@mpGridLineColor =              2
  resources@mpGridLineThicknessF =         1.1
  resources@mpGridMaskMode =               "MaskLand"

  map = ncl_map(wks,"orthographic",resources)

  resources@sfXCStartV = -18.
  resources@sfXCEndV = 52.
  resources@sfYCStartV = -35.
  resources@sfYCEndV = 38.
  resources@pmLabelBarDisplayMode =  "always"
  resources@cnFillOn = True
  resources@cnFillColors = (/3,4,5,6,8,9,10,11,12,13,14,15/)
  resources@cnFillDrawOrder =              "predraw"
  resources@cnLineDrawOrder =              "predraw"

  plot = ncl_contour_map(wks,z,resources)

;  delete(wks)
end
