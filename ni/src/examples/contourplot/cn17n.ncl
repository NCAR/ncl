;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1996                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn17n.ncl
;
;   Author:     Bob Lackman
;               National Center for Atmospheric Research
;               PO 3000, Boulder, Colorado
;
;  Date:       Mon Apr 28 09:17:09 MST 1997
;
;  Description:  This example is somewhat similar to the LLU example
;                "cpex10", which draws contours bands within a circle
;                on a satellite map projection. It also shows how to
;                use the NhlDataPolyline and NhlDataPolymarker routines
;                to draw lines and markers on a map projection.
;                
;                This example uses the AnnoManager class to label each
;                of the United States with a two-letter mnemonic. The
;                labeling is not done in the map projection, however,
;                so the text is not part of the map.  To show how you
;                *can* get your text to be part of the map, this example
;                has a second frame which mixes the LLUs and the HLUs
;                to achieve this affect (since this *is* doable in the
;                LLUs).
;
begin

  NCLS = 100
  NCOLORS = 23
  NDIM = 50
  NCIRC = 100
  zdat = new((/NCLS,NCLS/),float)
;
; Define the state-labelling data.
;
  name = (/"AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL",\
           "IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",\
           "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",\
           "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"/)

  slat = (/33.0, 65.0, 34.7, 35.0, 37.5, 39.0, 41.6, 39.0, 28.5, 32.5, 20.0, \
           43.5, 40.2, 40.0, 42.0, 38.5, 37.4, 31.2, 45.5, 39.2, 42.3, 44.0, \
           46.0, 32.5, 38.5, 47.0, 41.5, 39.8, 43.2, 39.7, 34.7, 43.0, 35.5, \
           47.5, 40.2, 35.6, 44.0, 40.8, 41.7, 34.0, 44.5, 36.0, 32.0, 39.5, \
           44.2, 37.6, 47.5, 38.5, 44.5, 43.0 /)

  slon = (/-86.5, -152.0, -111.5, -92.5, -120.5, -105.8, -72.6, -75.5, -82.0,\
           -83.0, -157.0, -114.0, -89.2, -86.0, -93.2, -98.2, -84.5, -92.5,\
           -69.0, -76.5, -72.0, -85.0, -94.5, -89.5, -92.5, -109.5, -99.5,\
           -117.0, -71.6, -74.5, -106.0, -75.0, -79.5, -100.5, -82.5, -97.5,\
           -120.2, -77.6, -71.5, -80.5, -100.5, -86.5, -100.0, -111.5, -72.5,\
           -78.6, -120.5, -80.8, -89.5, -107.5 /)
;
; Create an application object.
;
  appid = create "cn17" appClass defaultapp
    "appUsrDir" : "./"
    "appDefaultParent" : "True"
  end create
;
; Set the color map.
;
  cmap = new((/NCOLORS,3/),float)
  cmap(0,:) = (/0.00, 0.00, 0.00/)
  cmap(1,:) = (/1.00, 1.00, 1.00/)
  cmap(2,:) = (/.6, .6, .6 /)
  cmap(3,:) = (/0., 0., 0. /)
  cmap(4,:) = (/1., 1, 1./)
  cmap(5,:) = (/.4, .4, .4/)
  cmap(6,:) = (/1., 1, 0./)
  ii = ispan(8,NCOLORS,1)
  cmap(7:NCOLORS-1,0) = (NCOLORS-ii)/15.
  cmap(7:NCOLORS-1,1) = 0
  cmap(7:NCOLORS-1,2) = (ii-8)/15.
  delete(ii)
;
; Output to an X11 workstation
;
wks_type = "x11"

  if (str_lower(wks_type).eq."ncgm")
;
; Create an ncgmWorkstation object.
;
    wid = create "cn17Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "./cn17n.ncgm"
      "wkColorMap" : cmap
    end create
end if
  if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
    wid = create "cn17Work" windowWorkstationClass defaultapp
      "wkColorMap" : cmap
        "wkPause" : "True"
    end create
  end if
  if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
    wid = create "cn17Work" psWorkstationClass defaultapp
      "wkPSFileName" : "./cn17n.ps"
      "wkColorMap" : cmap
    end create
  end if
  if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
    wid = create "cn17Work" pdfWorkstationClass defaultapp
      "wkPDFFileName" : "./cn17n.pdf"
      "wkColorMap" : cmap
    end create
  end if
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wid = create "cn17Work" documentWorkstationClass defaultapp
      "wkFileName" : "./cn17n"
      "wkFormat" : wks_type
      "wkColorMap" : cmap
    end create
  end if
  if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wid = create "cn17Work" imageWorkstationClass defaultapp
      "wkFileName" : "./cn17n"
      "wkFormat" : wks_type
      "wkColorMap" : cmap
    end create
  end if
;
; Create a "great" circle in lat/lon coordinates. We don't want to draw
; any contour lines outside of this circle.
;
  cminlon = -115.
  cmaxlon = -95.
  cminlat = 32.
  cmaxlat = 48.
  ctrlat = 40.
  ctrlon = -105.
  xlonrng = cmaxlon - cminlon
  xlatrng = cmaxlat - cminlat
  ii = ispan(0,NCIRC-1,1)
  clon = ctrlon + 7.*cos((ii*6.28)/(NCIRC-1))
  clat = ctrlat + 7.*sin((ii*6.28)/(NCIRC-1))
  delete(ii)
;
; Generate some dummy data to contour later.
;
  miss_val = 1.e12
  xlonstp = xlonrng/(NCLS-1);
  xlatstp = xlatrng/(NCLS-1);
  ii = ispan(0,NCLS-1,1)
  x = (1.*ii)/(NCLS-1)
  xlon = cminlon + ii * xlonstp
  dist = new((/NCLS/),float)
  do j = 0,NCLS-1
      xlat = cminlat + j * xlatstp
      dist = sqrt((ctrlat-xlat)*(ctrlat-xlat) + (ctrlon-xlon)*(ctrlon-xlon))
;
; If xlat/xlon falls outside of circle, then we don't
; want to contour this location.
;
      if (num(dist.le.7.0).ne.0) then
        y = (1.*j)/(NCLS-1)
        inds = ind(dist.le.7.0)
        zdat(j,(/inds/)) = x((/inds/))^2 + y^2 + x((/inds/))*y \
                           + sin(9.*x((/inds/)))*cos(9.*y)
        delete(inds)
      end if
      zdat(j,ind(dist.gt.7.0)) = miss_val
  end do
  delete(dist)
;
; AnnoManager objects allow the PlotManager to manipulate any View
; class object as an annotation a uniform fashion. They allow
; the user to set the View object's size and location relative to
; the viewport of a Plot. They may be located relative to one
; of the viewport sides, or, as in this example, aligned with the 
; plot's data space (amTrackData is set True in the resource file).
;
; Create a TextItem for each place name to be included on the map.
; Collect the object ids into an array.
;
  text_ids = new(NDIM,graphic)
  do i=0,NDIM-1
    text_ids(i) = create name(i) textItemClass wid
      "txString": name(i)
    end create
  end do
;
; Create a MapPlot object.
;
  mpid = create "MapPlot" mapPlotClass wid
    "mpSatelliteAngle1F": 7.*57.2957795130823*asin(1./1.3)/8.
    "pmAnnoViews":  text_ids
  end create
;
; Retrieve the ids of the AnnoManager objects created by the PlotManager
; and then set their location in data coordinate space. The AnnoManager
; objects are arranged in the same order as the TextItems in the
; pmAnnoViews resource.
;
    getvalues mpid
      "pmAnnoManagers" : am_ids;
    end getvalues
    num_am_ids = dimsizes(am_ids)

    do i = 0,num_am_ids-1
      setvalues am_ids(i)
        "amDataXF" : slon(i)
        "amDataYF" : slat(i)
      end setvalues
   end do
;
; create an sst data object
;
  dataid = create "DataItem" scalarFieldClass noparent
    "sfDataArray" :     zdat
    "sfMissingValueV" : miss_val
    "sfXCStartV"  :     cminlon
    "sfXCEndV"    :     cmaxlon
    "sfYCStartV"  :     cminlat
    "sfYCEndV"    :     cmaxlat
  end create
;
; Create a ContourPlot Object
;
  cnid = create "ContourPlot" contourPlotClass wid
    "cnScalarFieldData" : dataid
    "tiMainString" : "Satellite view of contour bands in a limited area (using HLUS)"
  end create
;
; Overlay ContourPlot on MapPlot and draw.
;
  overlay(mpid,cnid)
  draw(mpid)
;
; Retrieve the GraphicStyle object that was created for us when
; we created the Workstation object.  We can then draw polylines
; and polymarkers on our MapPlot on behalf of the GraphicStyle object.
    getvalues wid
      "wkDefGraphicStyleId":  gsid
    end getvalues
;
; Draw circle around our contours.
;
    NhlDataPolyline(mpid,gsid,clon,clat)
;
; Draw a polymarker at the position of Boulder, Colorado (where
; NCAR is located).
;
    NhlDataPolymarker(mpid,gsid,ctrlon,ctrlat)
;
; Advance the frame.
;
  frame(wid)
  delete(wid)
end
