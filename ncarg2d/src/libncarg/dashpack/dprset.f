C
C $Id: dprset.f,v 1.2 2000-07-12 16:23:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
C the BLOCK DATA routine DPBLDA.
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
