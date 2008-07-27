C
C	$Id: capfnt.h,v 1.4 2008-07-27 12:23:41 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
