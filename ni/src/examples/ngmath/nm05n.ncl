;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1997                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       nm05n.ncl
;
;   Author:     Fred Clare (converted to NCL by Mary Haley)
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       Tue Dec 16 10:18:13 MST 1997
;
begin

  NUM = 6
  NX = 61
  NY = 61

;
; Input data points and values.
;
  xi = (/0.00, 1.00, 0.00, 1.00, 0.40, 0.75/)
  yi = (/0.00, 0.00, 1.00, 1.00, 0.20, 0.65/)
  zi = (/0.00, 0.00, 0.00, 0.00, 1.25, 0.80/)
  xeye =  3.3
  yeye = -3.3
  zeye =  3.3

  xo = new((/NX/),float)
  yo = new((/NY/),float)

;
; Create the output grid.
;
  xinc = 1./(NX-1) 
  yinc = 1./(NY-1) 
  ii = fspan(0.,60.,NX)
  xo = xinc * ii
  yo = yinc * ii
;
; Exponent equals 0.5
;
  dssetr("exp", 0.5)
  xexp = new(1,float)
  dsgetr("exp",xexp)
  if (xexp.ne.0.5) then
    print("xexp should equal 0.5")
  end if
  output = dsgrid2s(xi, yi, zi, xo, yo)
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
    wid = create "dsgrid" ncgmWorkstationClass defaultapp
        "wkMetaName" : "nm05n.ncgm"
    end create
  else
    if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
      wid = create "dsgrid" xWorkstationClass defaultapp
        "wkPause" : "True"
      end create
    else
      if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
        wid = create "dsgrid" psWorkstationClass defaultapp
          "wkPSFileName" : "nm05n.ps"
        end create
      end if
    end if
  end if

  getvalues wid
    "wkGksWorkId" : gkswid
  end getvalues

  drwtd2(gkswid, xo, yo, output, xeye, yeye, zeye, -6)

;
; Exponent equals 1.0
;
  dssetr("exp", 1.0)
  output = dsgrid2s(xi, yi, zi, xo, yo )

  drwtd2(gkswid, xo, yo, output, xeye, yeye, zeye, -6)

;
; Exponent equals 5.0
;
  dssetr("exp", 5.0)
  output = dsgrid2s(xi, yi, zi, xo, yo)

  drwtd2(gkswid, xo, yo, output, xeye, yeye, zeye, -6)

end
