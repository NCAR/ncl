;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1997                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       nm08n.ncl
;
;   Author:     Fred Clare (converted to NCL by Mary Haley)
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;   Date:       Mon Dec 15 10:52:56 MST 1997
;
;   Description:
;
begin
  NUMXOUT = 26
  NUMYOUT = 26

  x = (/0.00,0.15,0.10,0.35,0.50,0.55,0.55,0.65,0.85,0.80,1.00/)
  y = (/0.50,0.80,0.30,0.60,0.35,1.00,0.00,0.55,0.80,0.25,0.45/)
  z = (x-0.25)*(x-0.25) + (y-0.50)*(y-0.50)

  xinc = 1./(NUMXOUT-1.) 
  xi = ispan(0,NUMXOUT-1,1) * xinc

  yinc = 1./(NUMYOUT-1.)
  yi = ispan(0,NUMYOUT-1,1) * yinc

  nnseti("ADF",1)
  nnsetc("ALG","example8.dat")
  nnseti("IGR",0)
  zi = natgrids(x, y, z, xi, yi)
end

