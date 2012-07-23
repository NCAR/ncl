;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1997                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       nm02n.ncl
;
;   Author:     Fred Clare (converted to NCL by Mary Haley)
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       Mon Dec 15 10:48:08 MST 1997
;
;   Description: How to compute aspects and slopes.
;
begin
;
; Define constants
;
  RAD2DEG = 57.29578
  NUMXOUT = 21
  NUMYOUT = 21
;
;  Coordinate data are defined as random numbers between
;  -0.2 and 1.2. and are explicitly defined here for uniformity
;  across platforms.
; 
  x = (/ 1.16,  0.47,  0.29,  0.72,  0.52,  1.12,  0.33,  0.20,  0.30, \
 0.78,  0.92,  0.52,  0.44,  0.22, -0.10,  0.11,  0.59,  1.13, \
 0.68,  1.11,  0.93,  0.29,  0.74,  0.43,  0.87,  0.87, -0.10, \
 0.26,  0.85,  0.00, -0.02,  1.01, -0.12,  0.65,  0.39,  0.96, \
 0.39,  0.38,  0.94, -0.03, -0.17,  0.00,  0.03,  0.67, -0.06, \
 0.82, -0.03,  1.08,  0.37,  1.02, -0.11, -0.13,  1.03,  0.61, \
 0.26,  0.18,  0.62,  0.42,  1.03,  0.72,  0.97,  0.08,  1.18, \
 0.00,  0.69,  0.10,  0.80,  0.06,  0.82,  0.20,  0.46,  0.37, \
 1.16,  0.93,  1.09,  0.96,  1.00,  0.80,  0.01,  0.12,  1.01, \
 0.48,  0.79,  0.04,  0.42,  0.48, -0.18,  1.16,  0.85,  0.97, \
 0.14,  0.40,  0.78,  1.12,  1.19,  0.68,  0.65,  0.41,  0.90, \
 0.84, -0.11, -0.01, -0.02, -0.10,  1.04,  0.58,  0.61,  0.12, \
-0.02, -0.03,  0.27,  1.17,  1.02,  0.16, -0.17,  1.03,  0.13, \
 0.04, -0.03,  0.15,  0.00, -0.01,  0.91,  1.20,  0.54, -0.14, \
 1.03,  0.93,  0.42,  0.36, -0.10,  0.57,  0.22,  0.74,  1.15, \
 0.40,  0.82,  0.96,  1.09,  0.42,  1.13,  0.24,  0.51,  0.60, \
 0.06,  0.38,  0.15,  0.59,  0.76,  1.16,  0.02,  0.86,  1.14, \
 0.37,  0.38,  0.26,  0.26,  0.07,  0.87,  0.90,  0.83,  0.09, \
 0.03,  0.56, -0.19,  0.51,  1.07, -0.13,  0.99,  0.84,  0.22/)

  y = (/ -0.11,  1.07,  1.11, -0.17,  0.08,  0.09,  0.91,  0.17, -0.02, \
  0.83,  1.08,  0.87,  0.46,  0.66,  0.50, -0.14,  0.78,  1.08, \
  0.65,  0.00,  1.03,  0.06,  0.69, -0.16,  0.02,  0.59,  0.19, \
  0.54,  0.68,  0.95,  0.30,  0.77,  0.94,  0.76,  0.56,  0.12, \
  0.05, -0.07,  1.01,  0.61,  1.04, -0.07,  0.46,  1.07,  0.87, \
  0.11,  0.63,  0.06,  0.53,  0.95,  0.78,  0.48,  0.45,  0.77, \
  0.78,  0.29,  0.38,  0.85, -0.10,  1.17,  0.35,  1.14, -0.04, \
  0.34, -0.18,  0.78,  0.17,  0.63,  0.88, -0.12,  0.58, -0.12, \
  1.00,  0.99,  0.45,  0.86, -0.15,  0.97,  0.99,  0.90,  0.42, \
  0.61,  0.74,  0.41,  0.44,  1.08,  1.06,  1.18,  0.89,  0.74, \
  0.74, -0.06,  0.00,  0.99,  0.03,  1.00, -0.04,  0.24,  0.65, \
  0.12,  0.13, -0.09, -0.05,  1.03,  1.07, -0.02,  1.18,  0.19, \
  0.03, -0.03,  0.86,  1.12,  0.38,  0.72, -0.20, -0.08, -0.18, \
  0.32,  0.13, -0.19,  0.93,  0.81,  0.31,  1.09, -0.03,  1.01, \
 -0.17,  0.84, -0.11,  0.45,  0.18,  0.23,  0.81,  0.39,  1.09, \
 -0.05,  0.58,  0.53,  0.96,  0.43,  0.48,  0.96, -0.03,  1.13, \
  1.16,  0.16,  1.15,  0.57,  0.13,  0.71,  0.35,  1.04,  0.62, \
  1.03,  0.98,  0.31,  0.70,  0.97,  0.87,  1.14,  0.08,  1.19, \
  0.88,  1.00,  0.51,  0.03,  0.17,  1.01,  0.44,  0.17, -0.11/)

  z = (x-0.25)*(x-0.25) + (y-0.50)*(y-0.50)

  u = new((/NUMXOUT,NUMYOUT/),float)
  v = new((/NUMXOUT,NUMYOUT/),float)

  xc = 1./(NUMXOUT-1.) 
  xo = ispan(0,NUMXOUT-1,1) * xc

  yc = 1./(NUMYOUT-1.) 
  yo = ispan(0,NUMXOUT-1,1) * yc
;
;  Turn on gradient estimate calculations, flag calculation of
;  aspects and slopes, set flag to return aspects and slopes in
;  radians.
;
  nnsetp("SDI - compute slopes and aspects",1)
  nnsetp("IGR - use gradient estimates",1)
  nnsetp("RAD - return results in radians",1)
;
;  Do the interpolation.
;
  out = natgrids(x, y, z, xo, yo)
;
; Create an application object.
;
  appid = create "nm03" appClass defaultapp
    "appUsrDir" : "./"
    "appDefaultParent" : True
  end create
;
; Create our workstation object and then retrieve the GKS workstation
; id.
;
  wks_type = "ncgm"

  if (str_lower(wks_type).eq."ncgm") then
;
; Create an ncgmWorkstation object.
;
    wid = create "natgrids" ncgmWorkstationClass defaultapp
        "wkMetaName" : "nm03n.ncgm"
    end create
  end if
  if (str_lower(wks_type).eq."x11") then
;
; Create an X11 workstation.
;
    wid = create "natgrids" windowWorkstationClass defaultapp
      "wkPause" : "True"
    end create
  end if
  if (str_lower(wks_type).eq."oldps") then
;
; Create an older-style PostScript workstation.
;
    wid = create "natgrids" psWorkstationClass defaultapp
      "wkPSFileName" : "nm03n.ps"
    end create
  end if
  if (str_lower(wks_type).eq."oldpdf") then
;
; Create an older-style PDF workstation.
;
    wid = create "natgrids" pdfWorkstationClass defaultapp
      "wkPDFFileName" : "nm03n.pdf"
    end create
  end if
  if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS/PDF Workstation object.
;
    wid = create "natgrids" documentWorkstationClass defaultapp
      "wkFileName" : "nm03n"
      "wkFormat" : wks_type
    end create
  end if
  if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG Workstation object.
;
    wid = create "natgrids" imageWorkstationClass defaultapp
      "wkFileName" : "nm03n"
      "wkFormat" : wks_type
    end create
  end if

  getvalues wid
    "wkGksWorkId" : gkswid
  end getvalues

;
;  Draw the surface plot.
;
  drwsrfc(gkswid,xo,yo,out,10.,-25.,50.)

;
;  Get the aspects.
;
  do i=0,NUMXOUT-1
    do j=0,NUMYOUT-1
       rtmp = nngetaspects(i,j)
       u(j,i) = rtmp
       v(j,i) = rtmp
    end do
  end do
  u = sin(u)
  v = cos(v)

;
; Create a VectorField object; then use its id as the value of
; the 'vcVectorFieldData' resource when creating the VectorPlot object.
;
  vfid = create "vectorfield" vectorFieldClass defaultapp
    "vfUDataArray":  u
    "vfVDataArray":  v
  end create

  vcid = create "VectorPlot" vectorPlotClass wid
    "vcVectorFieldData":  vfid
  end create
;
; Plot the aspects as a vector plot.
;
  draw(vcid)
  frame(wid)
;
;  Get the slopes.
;
  do i = 0,NUMXOUT-1
    do j = 0,NUMYOUT-1
      rtmp = nngetslopes(i, j)
      u(j,i) = rtmp
    end do
  end do
  u = u * RAD2DEG
;
; Create a ScalarField object; then use its id as the value of
; the 'cnScalarFieldData' resource when creating the ContourPlot object.
;
  dataid = create "data" scalarFieldClass defaultapp
    "sfDataArray":  u
  end create

  cnid = create "ContourPlot" contourPlotClass wid
    "cnScalarFieldData":  dataid
  end create
;
;  Plot the slopes as a contour plot.
;
  draw(cnid)
  frame(wid)

end

