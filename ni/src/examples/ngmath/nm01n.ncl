;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1997                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       nm01n.ncl
;
;   Author:     Fred Clare (converted to NCL by Mary Haley)
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       Mon Dec 15 10:38:27 MST 1997
;
;   Description: Simple example of natural neighbor linear interpolation.
;

begin
  ISLIM = 6
  NumXOut = 21
  NumYOut = 21
;
; Define the input data arrays.
;
  x = (/0.00, 1.00, 0.00, 1.00, 0.40, 0.75/)
  y = (/0.00, 0.00, 1.00, 1.00, 0.20, 0.65/)
  z = (/0.00, 0.00, 0.00, 0.00, 1.25, 0.80/)
;
; Define the output grid.
;
  xc = 1./(NumXOut-1.) 
  xo = ispan(0,NumXOut-1,1) * xc

  yc = 1./(NumYOut-1.) 
  yo = ispan(0,NumYOut-1,1) * yc
;
; Do the gridding.
;
  out = natgrids(x, y, z, xo, yo)
;
; Create a workstation object.
;
  NCGM=1
  X11=0
  PS=0

  if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
    wid = create "natgrids" ncgmWorkstationClass defaultapp
        "wkMetaName" : "nm01n.ncgm"
    end create
  else
    if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
      wid = create "natgrids" xWorkstationClass defaultapp
        "wkPause" : "True"
      end create
    else
      if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
        wid = create "natgrids" psWorkstationClass defaultapp
          "wkPSFileName" : "nm01n.ps"
        end create
      end if
    end if
  end if
;
; Retrieve the GKS workstation ID so we can use low-level calls to
; draw the surface.
;
  getvalues wid
    "wkGksWorkId" : gkswid
  end getvalues
;
; Draw the surface.
;
  drwsrfc(gkswid,xo,yo,out,15.,-25.,90.)
;
; Show how to do the above, only using double precision.
; First we need to remove our float variables and redeclare 
; them as double precision.
;
  delete(x)
  delete(y)
  delete(z)
  delete(xo)
  delete(yo)
  delete(out)
;
; Create double precision variables.
;
  x = new((/ISLIM/),double)
  y = new((/ISLIM/),double)
  z = new((/ISLIM/),double)
  xo = new((/NumXOut/),double)
  yo = new((/NumXOut/),double)

  x = (/0.00, 1.00, 0.00, 1.00, 0.40, 0.75/)
  y = (/0.00, 0.00, 1.00, 1.00, 0.20, 0.65/)
  z = (/0.00, 0.00, 0.00, 0.00, 1.25, 0.80/)
;
; Define the output grid.
;
  xc = 1./(NumXOut-1.) 
  xo = ispan(0,NumXOut-1,1) * xc

  yc = 1./(NumYOut-1.) 
  yo = ispan(0,NumYOut-1,1) * yc
;
; Do the gridding.
;
  out = natgridd(x, y, z, xo, yo)
;
; Draw the surface.
;
  xp = doubletofloat(xo)
  yp = doubletofloat(yo)
  outp = doubletofloat(out)
    
  drwsrfc(gkswid,xp,yp,outp,15.,-25.,90.)
;
; Destroy the objects created and exit.
;
  delete(wid)
end

