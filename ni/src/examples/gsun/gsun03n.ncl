load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsun_scripts.ncl"

begin
  nlat =  64
  nlon = 129
  grid_dims = (/nlat,nlon/)

  filedir = ncargpath("data") + "/bin/"
  filename = filedir + "ex01B1_uv300.hs.mdl"

  num_records = fbinnumrec(filename)         ; Get the number of Fortran
                                             ; records in this file.

  num_timesteps = num_records/2              ; 2 records per timestep

  u = new( (/num_timesteps,nlat,nlon/), float )
  v = new( (/num_timesteps,nlat,nlon/), float )

  u(0,:,:) = fbinrecread(filename,0,grid_dims,"float")
  v(0,:,:) = fbinrecread(filename,1,grid_dims,"float")
  u(1,:,:) = fbinrecread(filename,2,grid_dims,"float")
  v(1,:,:) = fbinrecread(filename,3,grid_dims,"float")

  u@units = "m/sec"                         ; Define a "units" attribute
  v@units = "m/sec"                         ; for both u and v.

  lat = fspan(-88.,88.,nlat)
  lon = fspan(-180.,180.,nlon)
  u!1 = "lat"
  u!2 = "lon"
  v!1 = "lat"
  v!2 = "lon"
  umsk = mask(u,(u.gt.0),True)
  vmsk = mask(v,(v.gt.0),True)

  wks = ncl_open_wks("ncgm","gsun03n") ; open an X11 workstation

  resources = True
  resources@vpWidthF  = 0.7
  resources@vpHeightF = 0.7 * dimsizes(u(0,:,0))/dimsizes(u(0,0,:))

  resources@vfXCStartV = -180.
  resources@vfXCEndV   =  180.
  resources@vfYCStartV =  -88.
  resources@vfYCEndV   =   88.

  resources@tiYAxisString = "latitude"
  resources@tiXAxisString = "longitude"
  titles = (/"January wind", "July wind"/)

  resources@vcLineArrowThicknessF =  2.0

  do i=0,num_timesteps-1
    resources@tiMainString = titles(i)

    vector = ncl_vector(wks,u(i,::4,::4),v(i,::4,::4),resources)

    resources@tiMainString = titles(i) + " (with masked values)"
    vector_mask = ncl_vector(wks,umsk(i,::4,::4),vmsk(i,::4,::4),\
                             resources)

  end do   ; End of "do" statement.

  resources@Draw = False
  resources@Frame = False
  resources@vcMonoLineArrowColor = False     ; Indicate we want color
                                             ; vectors.
  resources@mpGridAndLimbOn  =     False
  vector_map = ncl_vector_map(wks,u(0,::2,::2),v(0,::2,::2),resources)

                                             ; Set some resources to
                                             ; take effect after the
                                             ; plot is created.
  setvalues vector_map@vector
    "tiMainString"         : titles(0)
    "tiMainFont"           : 25
    "tiXAxisFont"          : 25
    "tiYAxisFont"          : 25
    "tiMainFontHeightF"    : 0.035
    "tiXAxisFontHeightF"   : 0.026
    "tiYAxisFontHeightF"   : 0.026
  end setvalues

  draw(vector_map)
  frame(wks)

  hdf_filename = "ex01B1_uv300.hs.mdl.hdf"
  system("/bin/rm -f " + hdf_filename)
  hdf_file = addfile(hdf_filename,"c")

  hdf_file@title = "Wind vectors in January and July at 300 mb"
  hdf_file->ujan = u(0,:,:)
  hdf_file->vjan = v(0,:,:)
  hdf_file->ujul = u(1,:,:)
  hdf_file->vjul = v(1,:,:)
  hdf_file&lat = lat
  hdf_file&lon = lon
end
