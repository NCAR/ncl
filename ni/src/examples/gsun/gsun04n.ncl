load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin

  data_dir = ncargpath("data")
  grb_file = addfile(data_dir + "/grb/ced1.lf00.t00z.eta.grb","r")

  names = getfilevarnames(grb_file)  ; Get the variable names in the
  print(names)                       ; GRIB file and print them out.

  atts = getfilevaratts(grb_file,names(0)) ; Get the variable attributes and
  dims = getfilevardims(grb_file,names(0)) ; dimension names from the GRIB
  print(atts)                              ; file and print them out.
  print(dims)

  wks = gsn_open_wks("x11","gsun04n") ; Open an X11 workstation.

;----------- Begin first plot -----------------------------------------

  resources             = True

  uvar = grb_file->U_GRD_6_ISBL(0,:,:) ; Store two GRIB variables to 
  vvar = grb_file->V_GRD_6_ISBL(0,:,:) ; local NCL variables.

  if(isatt(uvar,"units").eq.True) ; Check if "uvar" has an attribute "units".
    resources@tiMainString = "GRD_6_ISBL (u,v " + uvar@units + ")"
  else
    resources@tiMainString = "GRD_6_ISBL"
  end if              ; The space is required between "end" and "if".
  resources@tiMainFont    = "Times-Roman"
  resources@tiXAxisString = "streamlines"

  plot = gsn_streamline(wks,uvar(::2,::2),vvar(::2,::2),resources) ; Create and
                                                 ; draw a streamline plot.
;----------- Begin second plot -----------------------------------------

  delete(uvar) ; Delete uvar and vvar since you are going to use
  delete(vvar) ; them again, but with different sized arrays.

  uvar = grb_file->U_GRD_6_TRO ; Store two different GRIB file variables
  vvar = grb_file->V_GRD_6_TRO ; to uvar and vvar.

  if(isatt(uvar,"units"))
    resources@tiMainString = "GRD_6_TRO (u,v " + uvar@units + ")"
  else
    resources@tiMainString = "GRD_6_TRO"
  end if

  resources@tiXAxisFont   = "Times-Roman"  ; Change the default font used.
  resources@tmXBLabelFont = "Times-Roman"
  resources@tmYLLabelFont = "Times-Roman"

  resources@stLineColor = "green"  ; Change streamlines to green.

  plot = gsn_streamline(wks,uvar(::2,::2),vvar(::2,::2),resources) ; Draw streamline plot.

;----------- Begin third plot -----------------------------------------

  getvalues plot                        ; Retrieve some resource values.
    "stArrowLengthF"    : arrowlength
    "stMinLineSpacingF" : spacing
  end getvalues
    
  resources@stMinLineSpacingF = spacing * 2.0     ; Set some resources based
  resources@stArrowLengthF    = arrowlength * 2.0 ; on resources you retrieved.
  resources@stLineColor       = "red"             ; Change line colors back to 
  resources@stLineThicknessF  = 1.5               ; red.

  uvar = grb_file->U_GRD_6_GPML(0,:,:) ; Get new GRIB variables.
  vvar = grb_file->V_GRD_6_GPML(0,:,:)

  if(isatt(uvar,"units"))
    resources@tiMainString = "GRD_6_GPML (u,v " + uvar@units + ")"
  else
    resources@tiMainString = "GRD_6_GPML"
  end if

  plot = gsn_streamline(wks,uvar(::2,::2),vvar(::2,::2),resources) ; Draw streamline plot.

  delete(uvar) ; Remove some variables we don't need any more.
  delete(vvar)
  delete(plot)

;----------- Write GRIB data to netCDF file --------------------------

  cdf_filename = "ced1.lf00.t00z.eta.nc"
  system("/bin/rm -f " + cdf_filename)
  cdf_file = addfile(cdf_filename,"c") ; Create a new netCDF file.

  cdf_file@title = "data from a GRIB file" ; Add some global attributes to
  cdf_file@date = systemfunc("date")       ; the netCDF file.

  do i = 0,dimsizes(names)-1
    names_char = stringtochar(names(i))
    if(names_char(0:3).eq."PRES") then   ; Only write variables that start
      print(names(i))                    ; with "PRES".
      cdf_file->$names(i)$ = grb_file->$names(i)$
    end if
    delete(names_char)
  end do                       ; The space is required b/w "end" and "do".
end
