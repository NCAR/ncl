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
;   Description:   Simple example of natural neighbor linear regridding.
;
begin
  NUMXOUT = 21
  NUMYOUT = 21

  x = (/0.00, 1.00, 0.00, 1.00, 0.40, 0.75/)
  y = (/0.00, 0.00, 1.00, 1.00, 0.20, 0.65/)
  z = (/0.00, 0.00, 0.00, 0.00, 1.25, 0.80/)

  xc = 1./(NUMXOUT-1.) 
  xi = ispan(0,NUMXOUT-1,1) * xc

  yc = 1./(NUMYOUT-1.) 
  yi = ispan(0,NUMYOUT-1,1) * yc

  nnsetp("igr",1)
  zi = natgrids(x, y, z, xi, yi)

  NCGM=1
  X11=0
  PS=0

  if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
    wid = create "natgrids" ncgmWorkstationClass defaultapp
        "wkMetaName" : "nm02n.ncgm"
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
          "wkPSFileName" : "nm02n.ps"
        end create
      end if
    end if
  end if

  getvalues wid
    "wkGksWorkId" : gkswid
  end getvalues

  drwsrfc(gkswid,xi,yi,zi,15.,-25.,90.)
end

