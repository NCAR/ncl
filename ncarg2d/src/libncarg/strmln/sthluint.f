C
C       $Id: sthluint.f,v 1.5 2008-07-27 00:17:28 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
