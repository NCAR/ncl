C
C	$Id: dashbd.f,v 1.2 2000-07-12 16:22:59 haley Exp $
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
      BLOCKDATA DASHBD
C
C DASHBD IS USED TO INITIALIZE VARIABLES IN NAMED COMMON.
C
      COMMON /DASHD1/  ISL,  L,  ISIZE,  IP(100),  NWDSM1,  IPFLAG(100)
     1                 ,MNCSTR, IGP
C
      COMMON /DDFLAG/ IFCFLG
C
      COMMON /DCFLAG/ IFSTFL
C
      COMMON /CFFLAG/ IVCTFG
C
      COMMON /DSAVE3/ IXSTOR,IYSTOR
C
      COMMON/INTPR/IPAU,FPART,TENSN,NP,SMALL,L1,ADDLR,ADDTB,MLLINE,
     1    ICLOSE
      SAVE
C
C IFSTFL CONTROLS THAT FRSTD IS CALLED BEFORE VECTD IS CALLED (IN CFVLD)
C WHENEVER DASHDB OR DASHDC HAVE BEEN CALLED.
C
      DATA IFSTFL /1/
C
C IVCTFG INDICATES IF VECTD IS BEING CALLED OR LASTD (IN CFVLD)
C
      DATA IVCTFG /1/
C
C ISL IS A FLAG FOR AN ALL SOLID PATTERN (+1) OR AN ALL GAP PATTERN (-1)
C
      DATA ISL /1/
C
C IGP IS AN INTERNAL PARAMETER. IT IS DESCRIBED IN THE DOCUMENTATION
C TO THE DASHED LINE PACKAGE.
C
      DATA IGP /9/
C
C MNCSTR IS THE MAXIMUM NUMBER OF CHARACTERS ALLOWED IN A HOLLERITH
C STRING PASSED TO DASHDC.
C
      DATA MNCSTR /15/
C
C
C
C  INTERNAL PARAMETERS
C
      DATA IPAU/3/
      DATA FPART/1./
      DATA TENSN/2.5/
      DATA NP/100/
      DATA SMALL/128./
      DATA L1/70/
      DATA ADDLR/2./
      DATA ADDTB/2./
      DATA MLLINE/384/
      DATA ICLOSE/6/
C
C IFCFLG IS THE FIRST CALL FLAG FOR SUBROUTINE DASHDB OR DASHDC.
C  1 = FIRST CALL TO DASHDB OR DASHDC.
C  2 = DASHDB OR DASHDC HAS BEEN CALLED BEFORE.
C
      DATA IFCFLG /1/
C
C IXSTOR AND IYSTOR CONTAIN THE CURRENT PEN POSITION. THEY ARE
C INITIALIZED TO AN IMPOSSIBLE VALUE.
C
      DATA IXSTOR,IYSTOR /-9999,-9999/
C
      END
