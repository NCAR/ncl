C
C	$Id: capfnt.h,v 1.2 2000-07-11 21:29:47 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
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
C License along with this library; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C

      COMMON /CAPFNT/ CHSTRT, CHSEND, CHRWDT, CHRSCH, FNTRHT,
     1		      FNTTOP, FNTCAP, FNTHLF, FNTBAS, FNTBOT,
     2		      FNTTYP, FNTSCH, CORXST, CORXLN, CORYST,
     3		      CORYLN, CORPST, CORPLN, PBEGST, PBEGLN,
     4		      PENDST, PENDLN, CORSCH, NEWCLS, CPNTRS,
     5                CPNLST, CSTRKS
      INTEGER         CHRSM1, CHRSM2
      PARAMETER       (CHRSM1=128, CHRSM2=5121)
      INTEGER         CHSTRT, CHSEND, CHRWDT, 
     1                CHRSCH(11)    , FNTRHT, FNTTOP,
     2                FNTCAP, FNTHLF, FNTBAS, FNTBOT, FNTTYP,
     3                FNTSCH(9)     , CORXST, CORXLN,
     4                CORYST, CORYLN, CORPST, CORPLN,
     5                PBEGST, PBEGLN, PENDST, PENDLN,
     6                CORSCH(10)    ,
     7                NEWCLS(300)   ,
     8                CPNTRS(CHRSM1), CPNLST        ,
     9                CSTRKS(CHRSM2)
C
C  XLCN   --  Bit offset to extract X coordinates from fontcap.
C  YLCN   --  Bit offset to extract Y coordinates from fontcap.
C  PLCN   --  Bit offset to extract PEN control bit from fontcap.
C  FNTMAX --  Number of fonts obtained from FONT LIST (defaults to 1).
C  FNTS   --  An array of pointers associating indices obtained from
C             FONT INDEX elements with the internally supported fonts.
C             Consult the documentation in BLOCKDATA TRNDAT for a
C             list of supported fonts.
C  FLEFT  --  An array of character left values for proportionally
C             spaced fonts.
C  MXFLST --  The maximum number of strings accepted in a FONT LIST
C             element.
C  NSFNTS --  The number of distinct fonts supported internally.
C
      PARAMETER (NSFNTS=20,MXFLST=300)
      COMMON /FNTCOM/ XLCN  , YLCN  , PLCN  , FNTMAX, 
     1                FNTS  , FLEFT , SFLENS
      INTEGER         FLEFT(CHRSM1) , FNTS(MXFLST)  ,
     1                XLCN, YLCN    , PLCN  , FNTMAX
      CHARACTER*30 SFONTS(NSFNTS)
      COMMON /SUPFNT/ SFONTS
C
C  Size of common CAPFNT in words.
C
      INTEGER WRLEN
      PARAMETER (WRLEN=5600)
