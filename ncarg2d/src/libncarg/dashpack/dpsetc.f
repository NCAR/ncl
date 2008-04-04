C
C $Id: dpsetc.f,v 1.6 2008-04-04 21:02:45 kennison Exp $
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
      SUBROUTINE DPSETC (PNAM,CVAL)
C
C This routine is used to set the value of an internal parameter of
C type CHARACTER.
C
        CHARACTER*(*) PNAM,CVAL
C
C Declare the character common block.
C
        COMMON /DPCMCH/ CHDP,CHRB,CHRG,CHRS
          CHARACTER*256 CHDP
          CHARACTER*1 CHRB,CHRG,CHRS
        SAVE   /DPCMCH/
C
C Declare the real/integer common block.
C
        COMMON /DPCMRI/ ANGF,DBPI,EPSI,IDPI,IDPS,ILTL,INDP,IPCF,ISBF,
     +                  ISCF,LCDP,RLS1,RLS2,RMFS,TENS,WCHR,WGAP,WSLD
        SAVE   /DPCMRI/
C
C Declare a temporary variable in which to form error messages.
C
        CHARACTER*39 CTMP
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL DPBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPSETC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Because most changes in internal parameters of DASHPACK result in
C changing the way in which the current dash pattern is interpreted,
C force a buffer dump before changing anything and ensure that, the
C next time DPDRAW is called, the dash pattern element descriptors
C will be regenerated.
C
        CALL DPSMTH (0.,0.,2)
        IF (ICFELL('DPSETC',2).NE.0) RETURN
C
        IDPI=0
C
C Change the specified parameter or log an error.
C
        IF (LEN(PNAM).LT.3) THEN
          CTMP='DPSETC - PARAMETER NAME TOO SHORT - '//PNAM
          CALL SETER (CTMP(1:38),3,1)
          RETURN
        ELSE IF (PNAM(1:3).EQ.'CRB'.OR.PNAM(1:3).EQ.'crb') THEN
          CHRB=CVAL
        ELSE IF (PNAM(1:3).EQ.'CRG'.OR.PNAM(1:3).EQ.'crg') THEN
          CHRG=CVAL
        ELSE IF (PNAM(1:3).EQ.'CRS'.OR.PNAM(1:3).EQ.'crs') THEN
          CHRS=CVAL
        ELSE IF (PNAM(1:3).EQ.'DPT'.OR.PNAM(1:3).EQ.'dpt') THEN
          LCDP=MAX(1,MIN(256,LEN(CVAL)))
          CHDP=CVAL
        ELSE
          CTMP='DPSETC - PARAMETER NAME NOT KNOWN - '//PNAM
          CALL SETER (CTMP(1:39),4,1)
          RETURN
        END IF
C
C Done.
C
        RETURN
C
      END
