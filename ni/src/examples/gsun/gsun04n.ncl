load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsun_scripts.ncl"

;***********************************************************************;
; gsun04n.ncl : streamline and contour plots                            ;
;                                                                       ;
; This example opens a GRIB file and creates some streamline and        ;
; contour plots from its data. This example also writes some of the     ;
; GRIB data to a netCDF file. To find out more about GRIB files, see the;
; URL:                                                                  ;
;                                                                       ;
;       http://ncardata.ucar.edu/docs/grib                              ;
;                                                                       ;
; Please note that you cannot *write* GRIB files, but only read them.   ;
; For more explanation on this, go to the URL:                          ;
;                                                                       ;
;  http://ngwww.ucar.edu/ngdoc/ng/ref/ncl/NclFormatSupport.html#GRIB    ;
;                                                                       ;
; To find out more about the GRIB file being opened, this example uses  ;
; NCL inquiry functions like isatt, getfilevarnames, getfilevaratts,    ;
; getfilevardims, and isfilevar to determine what variables and         ;
; attributes are in the file.                                           ;
;***********************************************************************;

begin

;***********************************************************************;
; Open a GRIB file to read from and a netCDF file to write to.          ;
;***********************************************************************;

  data_dir = ncargpath("data")
  grb_file = addfile(data_dir + "/grb/ced1.lf00.t00z.eta.grb","r")

  cdf_filename = "ced1.lf00.t00z.eta.cdf"
  system("/bin/rm -f " + cdf_filename)
  cdf_file = addfile(cdf_filename,"c")

;***********************************************************************;
; Add global attributes to the netCDF file called "title" and "date".   ;
; The function "systemfunc" executes a system command and returns the   ;
; output in a string.                                                   ;
;***********************************************************************;

  cdf_file@title = "data from a GRIB file"  
  cdf_file@date = systemfunc("date")

;***********************************************************************;
; Retrieve the list of variable names in our GRIB file with the         ;
; "getfilevarnames" function and print them out.                        ;
;***********************************************************************;

  names = getfilevarnames(grb_file)
  print(names)

;***********************************************************************;
; Get the list of attributes and dimension names for the first variable ;
; in the GRIB file and print them out (just to show how this is done).  ;
;***********************************************************************;

  atts = getfilevaratts(grb_file,names(0))
  dims = getfilevardims(grb_file,names(0))
  print(atts)
  print(dims)

;***********************************************************************;
; Even though we've seen the list of variable names, let's assume we're ;
; not sure which variables are in the file. All we know is that if any  ;
; of the variables that we list below are in the file, we want a        ;
; streamline plot of it.                                                ;
;***********************************************************************;

  uvars_we_want = (/"U_GRD_6_TRO","U_GRD_6_ISBL","U_GRD_6_GPML",\
                   "U_GRD_7_TRO"/)

  vvars_we_want = (/"V_GRD_6_TRO","V_GRD_6_ISBL","V_GRD_6_GPML",\
                   "V_GRD_7_TRO"/)

  wks = ncl_open_wks("ncgm","gsun04n")

;***********************************************************************;
; Loop through the variables that we want and generate a streamline plot;
; of each one if it exists.                                             ;
;                                                                       ;
; NCL contains "if-else-end if" statements just like C and Fortran.     ;
; "if" statements in NCL do not need a "then", and there is no "elseif" ;
; equivalent.  Also, make sure to put a space between "end" and "if"    ;
; since it won't recognize "endif".                                     ;
;                                                                       ;
; The "any" function returns True if any of the input is True.          ;
;***********************************************************************;

  if(any(isfilevar(grb_file,uvars_we_want)))
    do i = 0,dimsizes(uvars_we_want)-1

;***********************************************************************;
; "isfilevar" checks to see if the specified file contains the          ;
; requested variable.                                                   ;
;***********************************************************************;

    if(isfilevar(grb_file,uvars_we_want(i)).and.\
       isfilevar(grb_file,vvars_we_want(i)))

;***********************************************************************;
; If the variable has three dimensions, then the first dimension is a   ;
; level and we'll just select the first level to plot.                  ;
;                                                                       ;
; If you plan to access an NCL file variable several times, it's a good ;
; idea to save to a local NCL variable first, partly for efficiency,    ;
; and partly to save yourself some typing.                              ;
;***********************************************************************;

      if(dimsizes(dimsizes(grb_file->$uvars_we_want(i)$)).eq.3)
        uvar = grb_file->$uvars_we_want(i)$(0,:,:)
        vvar = grb_file->$vvars_we_want(i)$(0,:,:)
      else 
        uvar = grb_file->$uvars_we_want(i)$
        vvar = grb_file->$vvars_we_want(i)$
      end if

;***********************************************************************;
; If the variable in question has an attribute called "units", then use ;
; use this to create part of a title. Otherwise just use the name of the;
; variable as the title. The font type used for each label is changed   ;
; from the default.                                                     ;
;***********************************************************************;

      resources             = True
      resources@tiMainFont  = "times-roman"
      resources@tiXAxisFont = "times-roman"
      resources@tiYAxisFont = "times-roman"

      if(isatt(uvar,"units"))
        resources@tiMainString = uvars_we_want(i) + " (" + uvar@units + ")"
      else
        resources@tiMainString = uvars_we_want(i)
      end if

      if(isatt(uvar,"long_name"))
        resources@tiXAxisString = uvar@long_name
        resources@tiYAxisString = vvar@long_name
      else
        resources@tiXAxisString = "U"
        resources@tiYAxisString = "V"
      end if

;***********************************************************************;
; Check the variable to see if it has a missing value set. If it does,  ;
; then check the variable's values to see if any of them are missing    ;
; values. We don't do anything with this knowledge, but it just shows   ;
; how to check for missing values if you should ever need to do so.     ;
;***********************************************************************;

        if(isatt(uvar,"_FillValue")) 
          print("missing value for " + uvars_we_want(i) + " = " + \
                  uvar@_FillValue)
        end if
        if(any(ismissing(uvar))) 
          print( uvars_we_want(i) + " contains missing values")
        else
          print( uvars_we_want(i) + " contains no missing values")
        end if

;***********************************************************************;
; No streamline resources are set in this case, but FYI, streamline     ;
; resources start with "st".  To see a list of available streamline     ;
; resources, go to the URL:                                             ;
;                                                                       ;
;  http://ngwww.ucar.edu/ngdoc/ng/ref/hlu/obj/StreamlinePlot.res.html   ;
;                                                                       ;
;***********************************************************************;

        plot = ncl_streamline(wks,uvar,vvar,resources) ; Create/draw
                                                      ; streamline plot.

;***********************************************************************;
; Write these variables to the netCDF file that we opened. We'll give   ;
; the netCDF variables the same name as the GRIB variables.             ;
;***********************************************************************;

        cdf_file->$uvars_we_want(i)$ = uvar
        cdf_file->$vvars_we_want(i)$ = vvar

;***********************************************************************;
; Remove the variables "uvar" and "vvar" since we are going to possibly ;
; get a new variable. This is necessary because the next time you get a ;
; variable it could be a different type or a different size, and NCL    ;
; can't change a variable's size in this manner.                        ;
;***********************************************************************;

        delete(uvar)
        delete(vvar)

      else
;***********************************************************************;
; The variables don't exist in our GRIB file, so print out a message to ;
; that effect.                                                          ;
;***********************************************************************;

        print(uvars_we_want(i) + " and " + vvars_we_want(i) + \
              " are not variables in the GRIB file.")
      end if
    end do

;***********************************************************************;
; Print out what's in our netCDF file.  If none of the variables that   ;
; we requested above were in our GRIB file, then the only thing in our  ;
; netCDF file should be the title.                                      ;
;***********************************************************************;

    print(cdf_file)
  end if

;***********************************************************************;
; Now loop through all of the GRIB file variables and create contour    ;
; plots of the ones that start with "PRES".  First we have to convert   ;
; each string to a character array so we can pick out the first four    ;
; characters and test them.                                             ;
;***********************************************************************;

  resources@tiMainFuncCode  = "~"  ; Change the function code to
                                   ; something other than a colon,
                                   ; because we have a colon as part of
                                   ; our main title.
  resources@tiXAxisString   = "X"
  resources@tiYAxisString   = "Y"

  charnames = stringtochar(names)
  do i = 0,dimsizes(names)-1
    if(charnames(i,0:3).eq."PRES")
        var = grb_file->$names(i)$
        if(isatt(var,"units").and.var@units .eq. "Pa") 
          var = var * 0.01   ; If our units are Pa, convert them to mb.
          var@units = "mb"
        end if

        if(all(isatt(var,(/"long_name", "units"/))))
          resources@tiMainString = names(i) + ": " + var@long_name + \
                                   " (" + var@units + ")"
        else
          resources@tiMainString = vars_we_want(i)
        end if

;***********************************************************************;
; Set the resources for filling the contours, turning off the           ;
; informational label string, and creating a labelbar.                  ;
;***********************************************************************;

        resources@cnFillOn              = True
        resources@cnInfoLabelOn         = False
        resources@cnLineLabelsOn        = False
        resources@pmLabelBarDisplayMode = "always"
        resources@lbPerimOn             = False
        plot = ncl_contour(wks,var,resources)    ; Create/draw contour plot.

        delete(var)

    end if
  end do

end
