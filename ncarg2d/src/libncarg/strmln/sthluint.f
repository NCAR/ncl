C
C       $Id: sthluint.f,v 1.4 2000-08-22 15:06:43 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
C This module contains interface routines for StreamlinePlot
C
      SUBROUTINE STGETMAPINFO (IMP,ITR,VNL,DFM,XMN,XMX,YMN,YMX,
     +     XDL,XDH,YDL,YDH)
C
C The sole purpose of this routine is to make information from the
C mapping common block available to hlustmpxy, hlustimxy and hlustmpta
C
C --------------------------------------------------------------------
C
C The mapping common block: made available to user mapping routines
C
      COMMON /STMAP/
     +                IMAP       ,LNLG       ,INVX       ,INVY       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                XGDS       ,YGDS       ,NXCT       ,NYCT       ,
     +                ITRT       ,FW2W       ,FH2H       ,
     +                DFMG       ,VNML       ,RBIG       ,IBIG
C
      SAVE /STMAP/
C
C Math constants
C
      PARAMETER (PDTOR  = 0.017453292519943,
     +           PRTOD  = 57.2957795130823,
     +           P1XPI  = 3.14159265358979,
     +           P2XPI  = 6.28318530717959,
     +           P1D2PI = 1.57079632679489,
     +           P5D2PI = 7.85398163397448) 
C
      IMP = IMAP
      ITR = ITRT
      VNL = VNML
      DFM = DFMG
      XMN = WXMN
      XMX = WXMX
      YMN = WYMN
      YMX = WYMX
      XDL = MIN(XLOV,XHIV)
      XDH = MAX(XLOV,XHIV)
      YDL = MIN(YLOV,YHIV)
      YDH = MAX(YLOV,YHIV)
C
C Done.
C
      RETURN
C
      END
