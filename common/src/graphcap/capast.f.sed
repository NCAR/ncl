C
C	$Id: capast.f.sed,v 1.3 2008-07-27 12:23:43 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CAPAST
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA CAPASTX
C
C  Define the ASCII conversion tables.
C
      COMMON /CAPASC/ ASCTB1, ASCTB2
      INTEGER TBLLE1, TBLWT1, INTVAL, TBLLE2, CORDVL
      PARAMETER (TBLLE1=41, TBLWT1=3, INTVAL=-1, TBLLE2=94, CORDVL=-7)
      CHARACTER*1 ASCTB1(TBLLE1, TBLWT1), ASCTB2(TBLLE2)
      COMMON /CAPASI/ ASCVL1, ASCVL2
      INTEGER ASCVL1(TBLLE1), ASCVL2(TBLLE2)
C
      INTEGER II, JJ
C
C      ASCTB1
C        Table containing all 3-character sequences
C        which have special meaning to the translator.
C        This includes the special ASCII characters
C        with ADE values between 0 and 31 (decimal).
C
      DATA ((ASCTB1(II, JJ),JJ=1,TBLWT1),II=1,TBLLE1)/
     1 'R','L',' ',
     1 'M','A','D','X','Y','C','X','C',' ','Y','C',' ','V','C',' ',
     1 'I','N','T','N','U','L','S','O','H','S','T','X','E','T','X',
     1 'E','O','T',
     2 'E','N','Q','A','C','K','B','E','L','B','S',' ','H','T',' ',
     3 'L','F',' ','V','T',' ','F','F',' ','C','R',' ','S','O',' ',
     4 'S','I',' ','D','L','E','D','C','1','D','C','2','D','C','3',
     5 'D','C','4','N','A','K','S','Y','N','E','T','B','C','A','N',
     6 'E','M',' ','S','U','B','E','S','C','F','S',' ','G','S',' ',
     7 'R','S',' ','U','S',' ','S','P','C','D','E','L'/
C
C      ASCTB2
C        Table of all ASCII characters with ADE values
C        between 33 and 126 (decimal).
C
#if defined(mips) && defined(ultrix)
      DATA ASCTB2 /'!','"','#','$','%','&',"'",'(',')','*','+',',',
#else
      DATA ASCTB2 /'!','"','#','$','%','&','''','(',')','*','+',',',
#endif
     1 '-','.','/','0','1','2','3','4','5','6','7','8','9',':',';',
     2 '<','=','>','?','@','A','B','C','D','E','F','G','H','I','J',
     3 'K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y',
#if defined(u370) && defined(unix)
     4 'Z','[','\',']','^','_','`','a','b','c','d','e','f','g','h',
#else
     4 'Z','[','\\',']','^','_','`','a','b','c','d','e','f','g','h',
#endif
     5 'i','j','k','l','m','n','o','p','q','r','s','t','u','v','w',
     6 'x','y','z','{','|','}','~'/
C
C      ASCVL1
C        A table containing decimal equivalents to the
C        characters in ASCTB1.  Special negative values
C        are assigned to the strings which are significant
C        to the translator, but are not part of the ASCII
C        character set.
      DATA ASCVL1/
     1 -7,
     1 -6,-5,-4,-3,-2,
     1 -1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
     1 20,21,22,23,24,25,26,27,28,29,30,31,32,127/
C
C      ASCVL2
C        A table containing decimal equivalents for the
C        ASCII characters in table ASCTB2.
C
      DATA ASCVL2/33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,
     2 51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,
     3 72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,
     4 93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,
     5 110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,
     6 125,126/
C
      END
