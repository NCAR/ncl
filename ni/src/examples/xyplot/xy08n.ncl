;
;      $Id: xy08n.ncl,v 1.3 1996-04-22 23:43:32 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                     ;
;                Copyright (C)  1995                                  ;
;        University Corporation for Atmospheric Research              ;
;                All Rights Reserved                                  ;
;                                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       xy08n.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Tue Apr  4 13:02:35 MDT 1995
;;
;;  Description: This example is similar to the ncargex Autograph
;;               example "agex13".  It shows how to use Irregular
;;               points to change the transformation of your plot.
;;               It also shows how to read an ascii data file using
;;               "asciiread".
;;
;;               The "CoordArrays" object is used to set up the data.
;;               (The C version uses the "CoordArrTable" object which
;;               is not available in Fortran or NCL.)
;;
;;
;
; Begin NCL script.
;
begin

;
; Define procedure for setting up data arrays.
;
procedure assigndata(
    xc[*]:float,
    x[*][*]:float,
    y[*][*]:float,
    len[*]:integer,
    npts:integer,
    ncurve:integer
) 
local
    i,
    j,
    l
begin
  l = 0
  do i = 0, ncurve-1
    do j = 0,len(i)*2-1,2
        x(i,j/2) = xc(l)
        y(i,j/2) = xc(l+1)
        l = l + 2
    end do
  end do
end

;
; Create variables to contain data.
;
ncurve = 3
npts = 61
npts2 = 220
xcoord = new(npts2,float)
xdra = new((/ncurve,npts/),float)
ydra = new((/ncurve,npts/),float)
len = new(ncurve,integer)
len = (/12,37,61/)
;
; Create array to contain dataitem ids.
;
dataid = new(ncurve,graphic)

;
; Initialize some data for the XyPlot object.
; 
;
; Read the input data
;
len_dims = new(1,integer)
len_dims(0) = npts2
xcoord = asciiread("xy08.asc",len_dims,"float")

do i = 0, npts2-1
  xcoord(i) = 2. ^ ((xcoord(i)-15.)/2.5)
end do

assigndata(xcoord,xdra,ydra,len,npts,ncurve)

;
; Initialize tickmark values
;
explicit_values = new(14,float)

do i=0,13
  explicit_values(i) = 2.^(i-6)
end do

;
; Modify the color map.  Color indices '0' and '1' are the background
; and foreground colors respectively.
;
ncolors = 3
cmap = new( (/ncolors,3/), float)
cmap(0,:) = (/1.00,1.00,1.00/)
cmap(1,:) = (/0.00,0.00,0.00/)
cmap(2,:) = (/0.00,0.00,1.00/)

;
; Create Application object.  The Application object name is used to
; determine the name of the resource file, which is "xy08.res" in this
; case.
; 
appid = create "xy08" appClass defaultapp
    "appDefaultParent" : "True"
    "appUsrDir" : "./"
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
  xworkid = create "xy08Work" ncgmWorkstationClass defaultapp
      "wkMetaName" : "xy08n.ncgm"
      "wkColorMap" : cmap
  end create
else
  if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
    xworkid = create "xy08Work" xWorkstationClass defaultapp
      "wkPause" : "True"
      "wkColorMap" : cmap
    end create
  else
    if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
      xworkid = create "xy08Work" psWorkstationClass defaultapp
        "wkPSFileName" : "xy08n.ps"
        "wkColorMap" : cmap
      end create
    end if
  end if
end if

;
; Create the CoordArrays which defines the data for the XyPlot
; object. The id from this object will become the value for the XyPlot
; resource, "xyCoordData".
;
do i=0,ncurve-1
  dataid(i) = create "xyData"+i coordArraysClass defaultapp
    "caYArray": ydra(i,0:len(i)-1)
    "caXArray": xdra(i,0:len(i)-1)
  end create
end do

;
; Create the XyPlot object and tweak some of the tickmark, title and
; view port resources (some in the "xy08.res" resource file).
;
plotid = create "xyPlot" xyPlotClass xworkid
    "tmXBValues":  explicit_values
    "tmYLValues":  explicit_values
    "xyXIrregularPoints":  explicit_values
    "xyYIrregularPoints":  explicit_values
    "xyCoordData": dataid
end create

;
; Draw the plot.
; 
draw(plotid)
frame(xworkid)

;
; End NCL script.
;
end
