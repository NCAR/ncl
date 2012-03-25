;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       cn13n.ncl
;
;   Author:     Mary Haley
;               National Center for Atmospheric Research
;               PO 3000, Boulder, Colorado
;
;   Date:       Tue Nov 28 10:17:44 MST 1995
;
;  Description:  This example emulates LLU example "mpex10".  It shows
;                how to do inverse map tranformations and raster contour
;                plots.
;
;                Notice how quick the NCL version of this example 
;                runs because of the use of NCL's array arithmetic.

begin

      
  NCLS = 300
  NCOLORS = 66
  DTOR = .017453292519943
;
; Declare variable for defining color map.
;
  cmap = new( (/NCOLORS, 3/), float)
;
;  Colors for contour fill areas varying from blue to red.
;
  ii = ispan(1,NCOLORS-2,1)
  cmap( 0,:) = (/0.00, 0.00, 0.00/)
  cmap( 1,:) = (/1.00, 1.00, 1.00/)
  cmap(2:NCOLORS-1,0) = (1.0*(ii-1))/((NCOLORS-3)*1.0)
  cmap(2:NCOLORS-1,1) = 0.
  cmap(2:NCOLORS-1,2) = (1.0*((NCOLORS-2)-ii))/((NCOLORS-3)*1.0)
;
; Create an application object.
;
  appid = create "cn13" appClass defaultapp
      "appUsrDir" : "./"
      "appDefaultParent" : True
  end create
;
; Default is to display output to an X workstation
;
wks_type = "x11"

  if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
    wid = create "cn13Work" ncgmWorkstationClass defaultapp
       "wkColorMap"         : cmap
       "wkMetaName" : "./cn13n.ncgm"
    end create
  end if
  if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
    wid = create "cn13Work" windowWorkstationClass defaultapp
      "wkColorMap"         : cmap
      "wkPause" : True
    end create
  end if
  if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
    wid = create "cn13Work" psWorkstationClass defaultapp
     "wkPSFileName" : "./cn13n.ps"
     "wkColorMap"         : cmap
    end create
  end if
  if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
    wid = create "cn13Work" pdfWorkstationClass defaultapp
     "wkPDFFileName" : "./cn13n.pdf"
     "wkColorMap"         : cmap
    end create
  end if
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wid = create "cn13Work" documentWorkstationClass defaultapp
     "wkFileName" : "./cn13n"
     "wkFormat" : wks_type
     "wkColorMap"         : cmap
    end create
  end if
  if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wid = create "cn13Work" imageWorkstationClass defaultapp
     "wkFileName" : "./cn13n"
     "wkFormat" : wks_type
     "wkColorMap"         : cmap
    end create
  end if
;
; Create a MapPlot object.
;
  mpid = create "MapPlot" mapPlotClass wid
    "mpProjection":   "Orthographic"
    "mpCenterLatF":   40.
    "mpCenterLonF":   -105.
    "mpCenterRotF":   0.
  end create
;
; Calculate nice range of x,y values, and then get their
; corresponding lon,lat values.
;
  delete(ii)
  ii = new((/NCLS,NCLS/),float)
  ii = onedtond(ispan(0,NCLS-1,1),(/NCLS,NCLS/))

  ii!0 = "x"
  ii!1 = "y"

  x = .05+.90*(ii+.5)/NCLS
  x@_FillValue = 1e12
  y = .05+.90*(ii(y | :,x | : )+.5)/NCLS
  y@_FillValue = 1e12
 
  do i=0,NCLS-1
    NhlNDCToData(mpid,x(i,:),y(i,:),x(i,:),y(i,:))
  end do

;
; Now create a cell array.
;
  icra = 2 + ((.25*(1.+cos(DTOR*10.* y))+.25*(1.+sin(DTOR*10.*x ))*cos(DTOR* y))*(NCOLORS-2))
  icra@_FillValue = 1e12

  icra = icra < NCOLORS-1

;
; Create a scalar field object.
;
  dataid = create "DataItem" scalarFieldClass defaultapp
    "sfDataArray" : icra
    "sfMissingValueV" : icra@_FillValue
  end create
;
; Create a contour object
;
  cnid = create "ContourPlot" contourPlotClass wid
    "cnScalarFieldData":      dataid
  end create

;
; Draw MapPlot on ContourPlot.
;
  draw(cnid)
  draw(mpid)
  frame(wid)
;
; Destroy the workstation object and exit.
;
  delete(wid)
  delete(x)
  delete(y)
  delete(icra)
  delete(ii)
end
