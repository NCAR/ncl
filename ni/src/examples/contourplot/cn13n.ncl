;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

begin

  function max(x:float,y:float)
  begin
    if ( x.gt.y ) then
      return(x)
    else
      return(y)
    end if
  end

  function min(x:float,y:float)
  begin
    if ( x.lt.y ) then
      return(x)
    else
      return(y)
    end if
  end
      
  NCLS = 300
  NC = NCLS*NCLS
  NCOLORS = 66
  DTOR = .017453292519943
  icra = new((/NCLS,NCLS/),integer)
  x = new((/NC/),float)
  y = new((/NC/),float)
;
; Declare variable for defining color map.
;
  cmap = new( (/NCOLORS, 3/), float)
;
;  Colors for contour fill areas varying from blue to red.
;
  cmap( 0,:) = (/0.00, 0.00, 0.00/)
  cmap( 1,:) = (/1.00, 1.00, 1.00/)
  do i = 1,NCOLORS-2
    cmap(i+1,0) = (1.0*(i-1))/((NCOLORS-3)*1.0)
    cmap(i+1,1) = 0.
    cmap(i+1,2) = (1.0*((NCOLORS-2)-i))/((NCOLORS-3)*1.0)
  end do
;
; Create an application object.
;
  appid = create "cn13" appClass defaultapp
      "appUsrDir" : "./"
      "appDefaultParent" : "True"
  end create
;
; Default is to display output to an X workstation
;
  NCGM=0
  X11=1
  PS=0

  if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
      wid = create "cn13Work" ncgmWorkstationClass defaultapp
         "wkColorMap"         : cmap
         "wkMetaName" : "./cn13n.ncgm"
      end create
  else
    if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
      wid = create "cn13Work" xWorkstationClass defaultapp
          "wkColorMap"         : cmap
          "wkPause" : "True"
      end create
    else
      if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
        wid = create "cn13Work" psWorkstationClass defaultapp
         "wkPSFileName" : "./cn13n.ps"
         "wkColorMap"         : cmap
        end create
      end if
    end if
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
  l = 0
  do i = 0,NCLS-1
    do j = 0,NCLS-1
      x(l) = .05+.90*(i+.5)/NCLS
      y(l) = .05+.90*(j+.5)/NCLS
      l=l+1
    end do
  end do

  NhlNDCToData(mpid,x,y,x,y)
;
; Now create a cell array.
;
  l = 0
  do i = 0,NCLS-1
    do j = 0,NCLS-1
      if (x(l) .eq. 1.e12) then
        icra(j,i) = 0
      else
        dval=.25*(1.+cos(DTOR*10.*y(l)))+.25*(1.+sin(DTOR*10.*x(l)))*cos(DTOR*y(l))
        icra(j,i) = floattoint(max(2,floattoint(min(NCOLORS-1,2+floattoint(dval*(NCOLORS-2))))))
      end if
      l = l+1
    end do
  end do
;
; Create a scalar field object.
;
  dataid = create "DataItem" scalarFieldClass defaultapp
    "sfDataArray" : icra(:,:)
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
end
