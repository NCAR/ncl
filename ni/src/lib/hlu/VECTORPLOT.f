C
C $Id: VECTORPLOT.f,v 1.1 1995-11-21 20:19:02 dbrown Exp $
C
C****************************************************************
C								*
C			Copyright (C)  1994			*
C	University Corporation for Atmospheric Research		*
C			All Rights Reserved			*
C								*
C****************************************************************
C
C      File:            VECTORPLOT.f
C
C      Author:          David I. Brown
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Fri Sep 29 14:13:34 MDT 1995
C
C      Description:     
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVGETMAPINFO (IMP,ITR,DMX,SXD,SYD,XMN,XMX,YMN,YMX)
C
C The sole purpose of this routine is to make information from the
C mapping common block available to the C version of vvumxy
C
      COMMON /VVMAP/
     +                IMAP       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                SXDC       ,SYDC       ,NXCT       ,NYCT       ,
     +                RLEN       ,LNLG       ,INVX       ,INVY       ,
     +                ITRT       ,IWCT       ,FW2W       ,FH2H       ,
     +                DVMN       ,DVMX       ,RBIG       ,IBIG
C
      SAVE /VVMAP/
C
      IMP = IMAP
      ITR = ITRT
      DMX = DVMX
      SXD = SXDC
      SYD = SYDC
      ITR = ITRT
      XMN = WXMN
      XMX = WXMX
      YMN = WYMN
      YMX = WYMX
C
C Done.
C
      RETURN
C
      END
