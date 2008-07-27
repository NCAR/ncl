C
C $Id: dprset.f,v 1.5 2008-07-27 00:16:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DPRSET
C
C This routine restores the default values of DASHPACK parameters.
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
C Set all values.  For descriptions of the parameters being set, see
C the BLOCK DATA routine DPBLDAX.
C
        ANGF=360.
        CHDP='$$$$$$$$$$$$$$$$'
        CHRB='|'
        CHRG='_'
        CHRS='$'
        DBPI=.01
        EPSI=.000001
        IDPI=0
        IDPS=0
        ILTL=0
        INDP=65535
        IPCF=0
        ISBF=1
        ISCF=0
        LCDP=16
        RLS1=.5
        RLS2=0.
        RMFS=1.
        TENS=-1.
        WCHR=.01
        WGAP=.005
        WSLD=.005
C
C Done.
C
        RETURN
C
      END
