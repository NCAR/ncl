load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsun_scripts.ncl"

;***********************************************************************;
; gsun07n.ncl : contour plot animation                                  ;
;                                                                       ;
; This example opens a netCDF file of storm data and produces a contour ;
; over a map animation. It also shows how to use ncl_polyline,          ;
; ncl_polymarker, and ncl_text to draw primitives on a plot.            ;
;                                                                       ;
; This example introduces the use of a "resource file" for setting      ;
; resources.  In the call to "ncl_open_wks", the second argument will be;
; the name of your resource file, with a ".res" appended.  If there is  ;
; no file by this name, then NCL will not try to load it in. In this    ;
; example, the resource file is called "gsun07n.res".  Only resources   ;
; that don't depend on programmatic results can be set in a resource    ;
; file. The rest of the resources will be set in this script, as you'll ;
; see below.                                                            ;
;                                                                       ;
; Resources in a resource file are set much in the same way that X      ;
; resources in a .Xdefaults or .Xresources file are set.  For now, you  ;
; can just look at the file "gsun07n.res" to get a pretty good idea     ;
; of how resource files work.  There are some comments in the           ;
; "gsun07n.res to help explain some of the concepts. Or, you can get    ;
; more information than you probably need at the URL:                   ;
;                                                                       ;
;    http://ngwww.ucar.edu/ngdoc/ng/ug/hlu/hluimplmt/howrescwk.html     ;
;                                                                       ;
;***********************************************************************;

begin

;***********************************************************************;
; Define two arrays giving the approximate center of all 50 states in   ;
; the United States.                                                    ;
;***********************************************************************;

  slat = (/33.0, 65.0, 34.7, 35.0, 37.5, 39.0, 41.6, 39.0, 28.5, 32.5, \
           20.0, 43.5, 40.2, 40.0, 42.0, 38.5, 37.4, 31.2, 45.5, 39.2, \
           42.3, 44.0, 46.0, 32.5, 38.5, 47.0, 41.5, 39.8, 43.2, 39.7, \
           34.7, 43.0, 35.5, 47.5, 40.2, 35.6, 44.0, 40.8, 41.7, 34.0, \
           44.5, 36.0, 32.0, 39.5, 44.2, 37.6, 47.5, 38.5, 44.5, 43.0 /)

  slon = (/-86.5,-152.0,-111.5, -92.5,-120.5,-105.8, -72.6, -75.5, \
           -82.0, -83.0,-157.0,-114.0, -89.2, -86.0, -93.2, -98.2, \
           -84.5, -92.5, -69.0, -76.5, -72.0, -85.0, -94.5, -89.5, \
           -92.5,-109.5, -99.5,-117.0, -71.6, -74.5,-106.0, -75.0, \
           -79.5,-100.5, -82.5, -97.5,-120.2, -77.6, -71.5, -80.5, \
          -100.5, -86.5,-100.0,-111.5, -72.5, -78.6,-120.5, -80.8, \
           -89.5,-107.5 /)

  wks = ncl_open_wks("ncgm","gsun07n") ; Open an X11 workstation.

;***********************************************************************;
; Define a color table.                                                 ;
;***********************************************************************;

  cmap = (/(/0.00,0.00,0.00/),(/1.00,1.00,1.00/),(/.600,.600,.600/),\
           (/0.00,0.00,0.00/),(/1.00,1.00,1.00/),(/.400,.400,.400/),\
           (/1.00,1.00,0.00/),(/1.00,.000,.000/),(/.950,.010,.000/),\
           (/.870,.050,.000/),(/.800,.090,.000/),(/.700,.090,.000/),\
           (/.700,.120,.000/),(/.700,.180,.000/),(/.700,.260,.000/),\
           (/.700,.285,.000/),(/.680,.330,.000/),(/.570,.420,.000/),\
           (/.560,.530,.000/),(/.550,.550,.000/),(/.130,.570,.000/),\
           (/.060,.680,.000/),(/.000,.690,.000/),(/.000,.700,.100/),\
           (/.000,.600,.300/),(/.000,.500,.500/),(/.000,.400,.700/),\
           (/.000,.300,.700/),(/.000,.200,.700/),(/.000,.100,.700/),\
           (/.000,.000,.700/),(/.100,.100,.700/),(/.200,.200,.700/),\
           (/.300,.300,.700/),(/.420,.400,.700/),(/.560,.500,.700/),\
           (/.610,.600,.700/),(/.700,.700,.700/)/)

  ncl_define_colormap(wks,cmap)

;***********************************************************************;
; Open netCDF file and then read temperature and lat/lon data.          ;
;***********************************************************************;

  datadir  = ncargpath("data")
  datafile = datadir + "/cdf/meccatemp.cdf"
  cdffile  = addfile (datafile,"r")

  temp = cdffile->t - 273.15  ; Read temperature and convert to celsius.
  lon  = cdffile->lon
  lat  = cdffile->lat
  nlat = dimsizes(lat)
  nlon = dimsizes(lon)

;***********************************************************************;
; Restrict the contour plot to an area bounded by a rectangle specified ;
; in lat/lon coordinates. We do this by setting the temperature values  ;
; to a missing value wherever they fall outside the defined rectangle.  ;
;***********************************************************************;

  cminlat =   28.
  cminlon = -120.
  cmaxlat =   51.
  cmaxlon =  -90.
  temp@_FillValue = -999.   ; Define the missing-value value.
  do i = 0,nlon-1
    do j = 0,nlat-1
      if(lon(i).lt.cminlon.or.lon(i).gt.cmaxlon.or. \
         lat(j).lt.cminlat.or.lat(j).gt.cmaxlat) then
        temp(:,j,i) = temp@_FillValue
      end if
    end do
  end do

  clon = (/cminlon,cmaxlon,cmaxlon,cminlon,cminlon/) ; Define a rectangle
  clat = (/cminlat,cminlat,cmaxlat,cmaxlat,cminlat/) ; of lat/lon 
                                                     ; coordinates.

;***********************************************************************;
; The only resources we are setting here are the ones that depend on    ;
; programmatic values.  The others are set in a resource file called    ;
; "gsun07.res".                                                         ;
;***********************************************************************;

  resources                 = True
  resources@sfXCStartV      = lon(0)
  resources@sfXCEndV        = lon(nlon-1)
  resources@sfYCStartV      = lat(0)
  resources@sfYCEndV        = lat(nlat-1)

;***********************************************************************;
; We want to draw some polymarkers, lines, and text on our plot before  ;
; we advance the frame, so set "Frame" to False.                        ;
;***********************************************************************;

  resources@Frame           = False

  contour = ncl_contour_map(wks,temp(0,:,:),resources)

;***********************************************************************;
; Draw a polyline around the area that we are limiting our contours to, ;
; and draw a polymarker at the center of each state in the United       ;
; States. ncl_polyline and ncl_polymarker take as arguments the         ;
; workstation to draw to, the plot whose data space you want to draw in,;
; the x,y coordinates of the line, and a set of resources (optional).   ;
; The x,y coordinates must be in the same data space as the data in the ;
; plot, so in this case, we are giving both routines lat/lon            ;
; coordinates.                                                          ;
;                                                                       ;
; Notice that we have defined polymarkers for all states, but if the    ;
; polymarker falls outside the limits of our projection, it won't get   ;
; drawn.                                                                ;
;                                                                       ;
; Resources for changing polymarkers and polylines start with "gs"      ;
; (stands for GraphicStyle) and are documented at the URL:              ;
;                                                                       ;
;  http://ngwww.ucar.edu/ngdoc/ng/ref/hlu/obj/GraphicStyle.res.html     ;
;                                                                       ;
;***********************************************************************;

  ncl_polyline(wks,contour,clon,clat,False)
  ncl_polymarker(wks,contour,slon,slat,False)

;***********************************************************************;
; Draw some text at the bottom of the plot using "ncl_text".  This      ;
; procedure is different from ncl_polyline and ncl_polytext in that the ;
; x,y coordinates for drawing the text should be specified in viewport  ;
; coordinates and not data space coordinates.  The viewport goes from 0 ;
; to 1 in both directions, where 0,0 is the lower left corner. By       ;
; default, the text is centered about the x position. Text resources    ;
; start with "tx" and are defined at the URL:
;                                                                       ;
;    http://ngwww.ucar.edu/ngdoc/ng/ref/hlu/obj/TextItem.res.html       ;
;                                                                       ;
; The text resources in this example are defined in the "gsun07.res"    ;
; file.                                                                 ;
;                                                                       ;
; Notice the syntax ":S:o:N:".; The ':' character is a special "function;
; code" notifying NCL that we want to go into a different text mode.  In;
; this case, we follow the colon with "S:" to indicate we want to go    ;
; into superscript mode before drawing an 'o' character (as a degree    ;
; symbol).  The ":N:" puts us back in normal text mode.  For other      ;
; special characters that you can get this way, see the URL:            ;
;                                                                       ;
;    http://ngwww.ucar.edu/ngdoc/ng/ref/hlu/FunctionCodes.html          ;
;                                                                       ;
;***********************************************************************;

  text = "Contours limit to rectangle bounded by (120:S:o:N:W, 90:S:o:N:W) and (28:S:o:N:N, 51:S:o:N:N)"

  ncl_text(wks,text,.5,.1,False)

  frame(wks)      ; Now advance the frame.

;***********************************************************************;
; Loop through the rest of the time values in the netCDF file and create;
; an animation of the contour plot we just set up.  The only thing we   ;
; need to change in each contour plot is the data, so we use "setvalues";
; to do this, which is much more efficient than calling ncl_contour_map ;
; for each plot.                                                        ;
;***********************************************************************;
 
  ntime = dimsizes(cdffile->time)
  do k=1,ntime-1
    setvalues contour@data
      "sfDataArray" : temp(k,:,:)
    end setvalues
    draw(contour)
    ncl_polyline(wks,contour,clon,clat,False)
    ncl_polymarker(wks,contour,slon,slat,False)
    ncl_text(wks,text,.5,.1,False)
    frame(wks)
  end do

end
