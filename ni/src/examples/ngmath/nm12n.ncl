;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1998                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:         nm12n.ncl
;
;   Author:       Fred Clare
;                 National Center for Atmospheric Research
;                 PO 3000, Boulder, Colorado
;
;   Date:         Tue Jan 20 12:49:41 MST 1998
;
;   Description:  This program illustrates the use of ftsurf.
;

begin

;
;  Create the input arrays.
;
nxi = 11
nyi = 17

xi = fspan(0.,1.,nxi)
yi = fspan(0.,1.,nyi)
zi = new((/nxi,nyi/),float)

do i=0,nxi-1
    zi(i,:) = sin(-7.*xi(i)) + cos(5.*yi)
end do
zi = .5 + .25 * zi

;
;  Set up the output grid.
;
nxo = 31
nyo = 21
xo = fspan(0.,1.,nxo)
yo = fspan(0.,1.,nyo)

;
;  Interpolate.
;
zo = ftsurf(xi,yi,zi,xo,yo)


;
; Create workstation object.
;
  NCGM=1
  X11=0
  PS=0
 
  if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
    wid = create "nm12Work" ncgmWorkstationClass defaultapp
        "wkMetaName" : "nm12n.ncgm"
    end create
  else
    if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
      wid = create "nm12Work" xWorkstationClass defaultapp
        "wkPause" : True
      end create
    else
      if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
        wid = create "dsgrid" psWorkstationClass defaultapp
          "wkPSFileName" : "nm12n.ps"
        end create
      end if
     end if
  end if
 
;
;  Draw plot.
;
rho = 3.
theta = 36.
phi = 67.
tdez2d(wid, xo, yo, zo, rho, theta, phi, -6)
frame(wid)

end
