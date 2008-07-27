C
C	$Id: gqtxx.f,v 1.8 2008-07-27 00:21:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQTXX(WKID,PX,PY,STRX,ERRIND,CPX,CPY,
     +           TXEXPX,TXEXPY)
C
C  INQUIRE TEXT EXTENT
C
C  Number of NCAR fonts.
C
      PARAMETER (NFNT=31, NUMX=100)
C
      include 'gkscom.h'
C
      INTEGER WKID,ERRIND
C
C  Font metric data for NCAR fonts (font right, top, cap, base, bot, and
C  widths).
C
      INTEGER FMETRX(NUMX,NFNT)
      REAL    PX,PY,CPX,CPY,TXEXPX(4),TXEXPY(4)
      REAL TWIND(4),TVIEW(4)
      CHARACTER*(*) STRX
C
C  Legal NCAR font numbers.
C
      DIMENSION IFNTNM(NFNT)
      DATA IFNTNM/   1, -2, -3, -4, -5, -6, -7, -8, -9,-10,-11,-12,-13,
     +             -14,-15,-16,-17,-18,-19,-20,-21,-22,-25,-26,-29,-30,
     +             -33,-34,-35,-36,-37/
C
      DATA (FMETRX(LL,1),LL=1,NUMX)/
     +  27,  44,  35,   8,   0,  27,  27,  27,  27,  27,
     +  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,
     +  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,
     +  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,
     +  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,
     +  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,
     +  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,
     +  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,
     +  27,  27,  27,  27,  27,  27,  27,  27,  27,  27,
     +  27,  27,  27,  27,  27,  27,  27,  27,  27,  27/
      DATA (FMETRX(LL,2),LL=1,NUMX)/
     + 254, 137, 131, 123, 120,   8,   5,   8,  11,  10,
     +   0,  12,   4,   7,   7,  10,  12,   5,  12,   5,
     +  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,
     +  10,   5,   5,   5,  12,   0,   9,   0,  10,  10,
     +  11,  10,  10,   9,  11,  10,   4,   7,  10,   9,
     +  12,  10,  11,  10,  11,  10,  10,  10,  11,  10,
     +  12,  10,  10,  10,   0,   0,   0,   0,   0,   0,
     +   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     +   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     +   0,   0,   0,   0,   0,   0,   0,   4,  27,  27/
      DATA (FMETRX(LL,3),LL=1,NUMX)/
     + 254, 137, 131, 123, 120,   8,   5,   8,  11,  10,
     +   0,  12,   4,   7,   7,  10,  12,   5,  12,   5,
     +  10,  10,  10,  10,  10,  10,  10,  10,  10,  10,
     +  10,   5,   5,   5,  12,   0,   9,   0,  10,  10,
     +  10,  10,  10,  12,   9,  10,   4,   0,  10,  10,
     +  12,  10,  11,  10,  11,  10,  10,  10,  10,   0,
     +  11,  10,  12,  10,   0,   0,   0,   0,   0,   0,
     +   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     +   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     +   0,   0,   0,   0,   0,   0,   0,   4,  27,  27/
      DATA (FMETRX(LL,4),LL=1,NUMX)/
     + 254, 143, 139, 118, 111,  16,  10,  16,  21,  20,
     +  24,  26,   8,  14,  14,  16,  26,  10,  26,  10,
     +  22,  20,  20,  20,  20,  20,  20,  20,  20,  20,
     +  20,  10,  10,  24,  26,  24,  18,   0,  18,  21,
     +  21,  21,  19,  18,  21,  22,   8,  16,  21,  17,
     +  24,  22,  22,  21,  22,  21,  20,  16,  22,  18,
     +  24,  20,  18,  20,   0,  14,   0,  16,   0,  12,
     +  19,  19,  18,  19,  18,  12,  19,  19,   8,  10,
     +  17,   8,  30,  19,  19,  19,  19,  13,  17,  12,
     +  19,  16,  22,  17,  16,  17,   0,   8,  27,  27/
      DATA (FMETRX(LL,5),LL=1,NUMX)/
     + 254, 143, 139, 118, 111,  16,  10,  16,  21,  20,
     +  24,  26,   8,  14,  14,  16,  26,  10,  26,  10,
     +  22,  20,  20,  20,  20,  20,  20,  20,  20,  20,
     +  20,  10,  10,  24,  26,  24,  18,   0,  18,  21,
     +  20,  18,  19,  20,  17,  22,   8,  21,  21,  18,
     +  24,  22,  22,  22,  22,  21,  18,  16,  18,  18,
     +  20,  18,  22,  20,   0,  14,   0,  16,   0,  12,
     +  21,  19,  18,  18,  16,  20,  19,  20,  11,  22,
     +  18,  16,  21,  18,  17,  22,  17,  18,  20,  20,
     +  20,  16,  23,  16,  23,  15,   0,   8,  27,  27/
      DATA (FMETRX(LL,6),LL=1,NUMX)/
     + 254, 143, 139, 118, 106,  16,  10,  16,  21,  20,
     +   0,  26,   8,  14,  14,  16,  26,  10,  26,  10,
     +  22,  20,  20,  20,  20,  20,  20,  20,  20,  20,
     +  20,  10,  10,  24,  26,  24,  18,   0,  20,  23,
     +  20,  23,  20,  20,  23,  24,  17,  15,  24,  19,
     +  33,  24,  21,  25,  22,  25,  20,  19,  24,  23,
     +  28,  24,  23,  21,   0,  14,   0,  16,   0,  12,
     +  16,  14,  11,  16,  10,   8,  15,  15,   7,   7,
     +  14,   8,  25,  18,  14,  15,  15,  13,  11,   9,
     +  15,  15,  21,  16,  15,  14,   0,   8,  27,  27/
      DATA (FMETRX(LL,7),LL=1,NUMX)/
     + 254, 143, 139, 118, 111,  16,  10,  16,  21,  20,
     +  24,  25,   8,  14,  14,  16,  26,  10,  26,  10,
     +  22,  20,  20,  20,  20,  20,  20,  20,  20,  20,
     +  20,  10,  10,  24,  26,  24,  18,  27,  20,  22,
     +  21,  22,  21,  20,  23,  24,  11,  15,  22,  18,
     +  25,  23,  22,  22,  22,  22,  20,  19,  24,  20,
     +  24,  20,  21,  20,  14,  14,  14,  22,   0,  12,
     +  20,  21,  19,  21,  19,  13,  19,  22,  11,  11,
     +  21,  11,  33,  22,  20,  21,  20,  17,  17,  15,
     +  22,  18,  24,  20,  19,  18,  14,   8,  14,  27/
      DATA (FMETRX(LL,8),LL=1,NUMX)/
     + 254, 143, 139, 118, 111,  16,  10,  16,  21,  20,
     +  24,  25,   8,  14,  14,  16,  26,  10,  26,  10,
     +  22,  20,  20,  20,  20,  20,  20,  20,  20,  20,
     +  20,  10,  10,  24,  26,  24,  18,   0,  20,  22,
     +  20,  20,  21,  21,  18,  24,  11,  23,  22,  20,
     +  25,  23,  22,  24,  22,  22,  21,  19,  19,  18,
     +  22,  22,  23,  20,  14,  14,  14,  22,   0,  12,
     +  23,  21,  18,  19,  18,  22,  20,  22,  12,  22,
     +  20,  20,  23,  20,  18,  22,  19,  19,  21,  20,
     +  20,  17,  23,  17,  23,  18,  14,   0,  14,  27/
      DATA (FMETRX(LL,9),LL=1,NUMX)/
     + 254, 143, 139, 118, 106,  16,  11,  18,   0,  21,
     +   0,  26,   9,  15,  15,  17,  26,  11,  26,  11,
     +  22,  21,  21,  21,  21,  21,  21,  21,  21,  21,
     +  21,  11,  11,   0,  26,   0,  21,   0,  23,  24,
     +  21,  23,  19,  21,  22,  24,  16,  17,  24,  18,
     +  28,  23,  21,  23,  21,  24,  20,  18,  22,  21,
     +  23,  20,  22,  21,   0,   0,   0,   0,   0,   0,
     +  16,  14,  12,  16,  12,   9,  16,  15,   8,   8,
     +  14,   8,  25,  18,  14,  15,  16,  14,  12,   8,
     +  16,  15,  21,  16,  16,  13,  14,   0,  14,  27/
      DATA (FMETRX(LL,10),LL=1,NUMX)/
     + 254, 143, 139, 118, 111,  16,  11,  18,   0,  21,
     +   0,  26,   9,  15,  15,  17,  26,  11,  26,  11,
     +  22,  21,  21,  21,  21,  21,  21,  21,  21,  21,
     +  21,  11,  11,   0,  26,   0,  21,  27,  20,  24,
     +  21,  23,  23,  22,  22,  26,  13,  18,  23,  20,
     +  27,  25,  22,  23,  22,  24,  23,  21,  25,  20,
     +  26,  22,  21,  22,   0,   0,   0,   0,   0,   0,
     +  21,  19,  18,  21,  18,  15,  20,  21,  13,  13,
     +  20,  12,  33,  23,  18,  21,  20,  17,  17,  14,
     +  23,  20,  29,  20,  21,  20,  14,   0,  14,  27/
      DATA (FMETRX(LL,11),LL=1,NUMX)/
     + 254, 143, 139, 118, 111,  16,  10,  16,  21,  20,
     +  24,  25,   8,  14,  14,  16,  26,  10,  26,  10,
     +  22,  20,  20,  20,  20,  20,  20,  20,  20,  20,
     +  20,   0,   0,   0,  24,  22,  26,  21,  20,  22,
     +  22,  18,  24,  21,  31,  20,  24,  24,  25,  25,
     +  24,  22,  24,  22,  21,  19,  21,  25,  20,  24,
     +  23,  33,  33,  30,  21,  21,  31,  22,   0,   0,
     +  20,  20,  20,  18,  23,  19,  27,  18,  22,  20,
     +  22,  23,  22,  20,  22,  21,  19,  19,  18,  21,
     +  20,  22,  22,  31,  31,  26,  17,  19,  29,  21/
      DATA (FMETRX(LL,12),LL=1,NUMX)/
     + 254, 143, 139, 118, 111,  16,  11,  18,   0,  19,
     +   0,  25,   9,  14,  14,  16,  25,  11,  25,  11,
     +  23,  20,  20,  20,  20,  20,  20,  20,  20,  20,
     +  20,  11,  11,   0,  25,   0,  19,   0,  20,  20,
     +  21,  21,  19,  18,  21,  22,   9,  17,  21,  17,
     +  24,  22,  22,  20,  22,  20,  20,  17,  22,  20,
     +  26,  20,  19,  20,   0,   0,   0,   0,   0,   0,
     +  20,  20,  18,  20,  18,  14,  20,  20,   9,   9,
     +  19,   9,  31,  20,  19,  20,  20,  14,  17,  11,
     +  20,  16,  24,  18,  16,  18,  17,  19,  29,  21/
      DATA (FMETRX(LL,13),LL=1,NUMX)/
     + 254, 143, 139, 118, 111,  16,  11,  18,   0,  20,
     +   0,  26,   9,  14,  14,  16,  25,  11,  25,  11,
     +  23,  20,  20,  20,  20,  20,  20,  20,  20,  20,
     +  20,  11,  11,   0,  25,   0,  19,   0,  20,  22,
     +  21,  22,  21,  20,  23,  24,  12,  16,  22,  18,
     +  26,  24,  22,  22,  22,  22,  20,  20,  24,  20,
     +  24,  20,  22,  20,   0,   0,   0,   0,   0,   0,
     +  20,  21,  19,  21,  19,  14,  19,  23,  12,  13,
     +  22,  12,  34,  23,  20,  21,  20,  17,  17,  15,
     +  23,  18,  24,  20,  19,  18,  17,  19,  29,  21/
      DATA (FMETRX(LL,14),LL=1,NUMX)/
     + 254, 143, 139, 118, 111,  16,  11,  20,   0,  21,
     +   0,  26,  10,  16,  16,  17,  25,  11,  25,  11,
     +  23,  21,  21,  21,  21,  21,  21,  21,  21,  21,
     +  21,  11,  11,   0,  25,   0,  21,   0,  20,  24,
     +  21,  23,  23,  22,  22,  26,  14,  19,  23,  20,
     +  28,  25,  22,  23,  22,  24,  23,  22,  25,  20,
     +  26,  22,  22,  22,   0,   0,   0,   0,   0,   0,
     +  22,  19,  18,  22,  18,  16,  21,  22,  13,  13,
     +  22,  12,  35,  24,  20,  22,  21,  18,  17,  14,
     +  24,  20,  30,  22,  22,  20,  17,  19,  29,  21/
      DATA (FMETRX(LL,15),LL=1,NUMX)/
     + 254, 143, 139, 118, 111,  16,  11,  18,   0,  20,
     +   0,  26,   9,  14,  14,  16,  25,  11,  25,  11,
     +  23,  20,  20,  20,  20,  20,  20,  20,  20,  20,
     +  20,  11,  11,   0,  25,   0,  19,   0,  24,  26,
     +  24,  26,  24,  24,  26,  25,  25,  25,  26,  23,
     +  32,  28,  28,  27,  28,  28,  27,  25,  22,  28,
     +  33,  24,  26,  24,   0,   0,   0,   0,   0,   0,
     +  17,  17,  13,  17,  13,  13,  17,  17,  10,  10,
     +  14,  10,  26,  18,  17,  17,  17,  14,  11,  12,
     +  18,  17,  25,  15,  17,  14,  16,  17,  17,  21/
      DATA (FMETRX(LL,16),LL=1,NUMX)/
     + 254, 143, 139, 118, 111,  16,  12,  18,   0,  20,
     +   0,  26,   9,  14,  14,  16,  25,  12,  25,  12,
     +  23,  20,  20,  20,  20,  20,  20,  20,  20,  20,
     +  20,  12,  12,   0,  25,   0,  18,   0,  22,  24,
     +  24,  23,  22,  23,  25,  24,  19,  20,  24,  22,
     +  28,  25,  26,  22,  26,  24,  23,  24,  24,  23,
     +  27,  22,  23,  20,   0,   0,   0,   0,   0,   0,
     +  17,  18,  14,  17,  14,  13,  18,  18,  10,  10,
     +  17,  10,  26,  18,  18,  18,  18,  14,  16,  10,
     +  18,  18,  26,  19,  18,  18,  16,  17,  17,  21/
      DATA (FMETRX(LL,17),LL=1,NUMX)/
     + 254, 143, 139, 118, 111,  16,  11,  18,   0,  20,
     +   0,  26,   9,  14,  14,  16,  25,  11,  25,  11,
     +  23,  20,  20,  20,  20,  20,  20,  20,  20,  20,
     +  20,  11,  11,   0,  25,   0,  19,   0,  26,  26,
     +  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,
     +  26,  26,  26,  26,  26,  26,  26,  26,  26,  26,
     +  26,  26,  26,  26,   0,   0,   0,   0,   0,   0,
     +  17,  17,  12,  16,  13,  12,  17,  18,  10,  10,
     +  17,  10,  26,  18,  16,  17,  17,  13,  16,  10,
     +  18,  18,  26,  18,  18,  15,  16,  17,  17,  21/
      DATA (FMETRX(LL,18),LL=1,NUMX)/
     + 254, 143, 139, 118, 107,  16,  10,  16,  21,  21,
     +  24,  25,   8,  14,  14,  16,  26,  10,  26,  10,
     +  22,  20,  20,  20,  20,  20,  20,  20,  20,  20,
     +  20,  10,  10,  24,  26,  24,  18,  27,  24,  24,
     +  26,   4,   0,  28,  10,   8,  14,  28,  24,  24,
     +  16,  16,  14,  14,   8,  14,  24,  24,  22,  10,
     +  26,  26,  26,  27,  14,  14,  14,  22,  19,  12,
     +  25,  24,  12,  20,  10,  10,  10,  10,  22,  24,
     +  24,  24,  24,  24,  26,  16,  26,  16,  19,  20,
     +  33,  24,  24,  25,  24,  16,  14,   0,  14,  22/
      DATA (FMETRX(LL,19),LL=1,NUMX)/
     + 254, 143, 139, 118, 105,  16,  12,  18,  22,  24,
     +  24,  24,  24,  24,  24,  10,  10,   6,  10,  10,
     +  22,  10,  12,   3,   4,   4,  10,  22,  20,  14,
     +  40,  28,  24,  14,   0,  14,  24,  14,  10,   0,
     +  10,  11,  11,  11,  11,  28,  28,  14,  16,  14,
     +  16,  16,  20,  16,  20,  22,  20,   5,  28,  28,
     +  28,  28,  14,  12,  14,  12,  16,   8,  10,   9,
     +  10,   9,  12,   7,  10,  12,  14,  18,  18,  20,
     +  12,  14,  14,  22,  16,  16,  16,  16,  18,   2,
     +   4,   8,  10,  14,  22,  34,  44,  40,  34,  22/
      DATA (FMETRX(LL,20),LL=1,NUMX)/
     + 254, 143, 139, 118, 102,  23,  22,  22,  33,  33,
     +  11,  22,  26,  24,  24,  35,  35,  13,  17,  19,
     +  28,  25,  22,  19,  19,  23,  22,  19,  25,  20,
     +  24,  24,  25,  24,  24,  24,  23,  24,  24,  25,
     +  22,  24,  24,  24,   8,  16,  16,  20,  20,  17,
     +  16,  16,  16,  26,  16,  16,  16,  29,  33,  28,
     +   8,  16,  16,  20,  20,  17,  16,  16,  16,  26,
     +  16,  28,  16,  33,  16,  34,  31,  11,  11,  14,
     +  30,  11,  11,  22,  16,  16,  16,  16,  18,   2,
     +   4,   8,  10,  14,  22,  34,  44,  40,  34,  22/
      DATA (FMETRX(LL,21),LL=1,NUMX)/
     + 195, 168, 140,   0, -42,  53,  53,  68, 107, 107,
     + 171, 128,  43,  64,  64,  74, 112,  53,  64,  53,
     +  54, 107, 107, 107, 107, 107, 107, 107, 107, 107,
     + 107,  53,  53, 112, 112, 112, 143, 195, 128, 128,
     + 139, 139, 128, 118, 150, 139,  54,  96, 128, 107,
     + 160, 139, 149, 128, 150, 139, 128, 118, 139, 128,
     + 182, 128, 128, 117,  53,  53,  53,  90, 107,  43,
     + 107, 107,  96, 107, 107,  53, 107, 107,  43,  43,
     +  96,  43, 160, 107, 107, 107, 107,  64,  96,  54,
     + 107,  96, 138,  96,  96,  96,  64,  50,  64, 112/
      DATA (FMETRX(LL,22),LL=1,NUMX)/
     + 534, 418, 400,   0,-121, 151, 182, 260, 305, 305,
     + 487, 396, 152, 182, 182, 213, 320, 152, 182, 152,
     + 152, 305, 305, 305, 305, 305, 305, 305, 305, 305,
     + 305, 182, 182, 320, 320, 320, 335, 534, 396, 396,
     + 397, 396, 366, 335, 426, 396, 152, 305, 397, 335,
     + 456, 396, 426, 366, 426, 396, 366, 335, 396, 366,
     + 517, 366, 366, 335, 182, 152, 182, 320, 305, 152,
     + 305, 335, 305, 335, 305, 182, 335, 335, 152, 152,
     + 305, 152, 487, 335, 335, 335, 335, 213, 305, 182,
     + 335, 305, 426, 305, 305, 274, 213, 153, 213, 320/
      DATA (FMETRX(LL,23),LL=1,NUMX)/
     + 570, 480, 400,   0,-131, 151, 201, 246, 302, 302,
     + 503, 470, 201, 201, 201, 302, 341, 151, 201, 151,
     + 168, 302, 302, 302, 302, 302, 302, 302, 302, 302,
     + 302, 168, 168, 341, 341, 341, 268, 556, 436, 403,
     + 403, 436, 369, 336, 436, 436, 201, 235, 436, 369,
     + 537, 436, 436, 336, 436, 403, 336, 369, 436, 436,
     + 570, 436, 436, 369, 201, 168, 201, 283, 304, 201,
     + 270, 302, 268, 302, 268, 201, 302, 302, 168, 168,
     + 304, 168, 470, 302, 302, 302, 304, 201, 234, 168,
     + 302, 302, 436, 302, 302, 268, 290, 121, 290, 327/
      DATA (FMETRX(LL,24),LL=1,NUMX)/
     + 582, 480, 400,   0,-123, 150, 195, 325, 293, 293,
     + 586, 488, 195, 195, 195, 293, 334, 147, 195, 147,
     + 163, 293, 293, 293, 293, 293, 293, 293, 293, 293,
     + 293, 195, 195, 334, 334, 334, 293, 545, 423, 391,
     + 423, 423, 391, 359, 456, 456, 228, 293, 456, 391,
     + 553, 423, 456, 358, 456, 423, 326, 391, 423, 423,
     + 586, 423, 423, 391, 195, 163, 195, 340, 293, 195,
     + 293, 326, 260, 326, 260, 195, 293, 326, 163, 195,
     + 326, 163, 488, 326, 293, 326, 326, 260, 228, 195,
     + 326, 293, 423, 293, 293, 260, 231, 129, 231, 305/
      DATA (FMETRX(LL,25),LL=1,NUMX)/
     + 432, 480, 400,   0,-112, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414/
      DATA (FMETRX(LL,26),LL=1,NUMX)/
     + 447, 480, 400,   0,-111, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414,
     + 414, 414, 414, 414, 414, 414, 414, 414, 414, 414/
      DATA (FMETRX(LL,27),LL=1,NUMX)/
     + 556, 480, 400,   0,-129, 151, 201, 246, 302, 302,
     + 503, 470, 201, 201, 201, 302, 341, 151, 201, 151,
     + 168, 302, 302, 302, 302, 302, 302, 302, 302, 302,
     + 302, 168, 168, 341, 341, 341, 268, 556, 427, 395,
     + 427, 364, 365, 453, 357, 434, 197, 374, 428, 407,
     + 527, 428, 428, 455, 439, 330, 351, 362, 409, 260,
     + 455, 382, 471, 362, 201, 168, 201, 283, 304, 201,
     + 374, 325, 326, 293, 260, 309, 244, 358, 195, 358,
     + 325, 342, 342, 309, 326, 326, 309, 326, 356, 260,
     + 342, 368, 407, 292, 407, 293, 290, 121, 290, 327/
      DATA (FMETRX(LL,28),LL=1,NUMX)/
     + 606, 480, 400,   0,-144, 151, 325, 512, 326, 390,
     + 423, 297, 423, 326, 260, 325, 618, 585, 358, 585,
     + 358, 237, 326, 244, 326, 326, 422, 293, 273, 325,
     + 326, 326, 326, 593, 130, 560, 390, 488, 407, 471,
     + 585, 455, 455, 488, 455, 455, 423, 423, 423, 423,
     + 423, 323, 323, 455, 423, 151, 151, 151, 488, 326,
     + 148, 423, 358, 358, 618, 585, 358, 585, 358, 293,
     + 195, 151, 151, 151, 423, 151, 151, 151, 151, 151,
     + 151, 151, 151, 151, 151, 151, 195, 162, 230, 326,
     + 358, 151, 151, 151, 151, 151, 151, 151, 151, 151/
      DATA (FMETRX(LL,29),LL=1,NUMX)/
     + 528, 480, 400,   0,-143, 151, 151, 302, 151, 151,
     + 151, 151, 302, 151, 109, 268, 302, 201, 201, 151,
     + 151, 151, 151, 302, 302, 151, 151, 273, 151, 201,
     + 151, 268, 302, 593, 151, 151, 151, 151, 201, 201,
     + 201, 201, 201, 201, 201, 201, 151, 201, 201, 151,
     + 201, 201, 201, 151, 151, 477, 477, 538, 151, 151,
     + 151, 151, 151, 151, 151, 151, 151, 151, 151, 151,
     + 151, 477, 477, 475, 151, 151, 151, 151, 151, 151,
     + 151, 464, 326, 514, 493, 151, 151, 151, 151, 151,
     + 151, 151, 151, 151, 151, 151, 151, 151, 151, 151/
      DATA (FMETRX(LL,30),LL=1,NUMX)/
     +1390,1150, 800,   0,-175,1040,1040,1040,1390,1040,
     + 971,1156, 583, 583, 525,1231,1014,1014,1014, 562,
     +1096, 834, 834, 887, 654, 564, 514, 622, 694, 694,
     +1270, 775, 775, 775,1230,1080,1036, 936,1036,1036,
     + 936,1036, 925, 936, 925, 936,1247,1020,1090,1090,
     +1020,1020,1090,1090,1020,1020, 347, 617, 347, 683,
     + 347, 701,1065,1065, 426, 426, 426, 696, 426, 796,
     + 426, 800,1065,1065, 468, 468, 468, 758, 468, 802,
     + 468, 802,1040, 990, 990, 884, 560, 560, 560, 560,
     + 560, 560, 560, 560, 560, 560, 560,1223,1223,1244/
      DATA (FMETRX(LL,31),LL=1,NUMX)/
     +1254,1450, 800,   0,-111,1040,1244, 887, 887, 848,
     + 887, 848, 667,1244,1244, 887, 848, 680,1074,1172,
     +1094, 650, 962, 962,1009, 990,1062,1062,1062,1062,
     +1062,1009,1009, 962, 990,1062,1062, 962, 962,1060,
     + 920,1060,1060,1060, 650, 950,1184,1076,1076,1094,
     +1076,1094,1076,1076,1094,1040,1040,1040,1040,1040,
     +1040,1040,1040,1040,1040, 963,1099, 840, 963,1254,
     + 963,1099, 840, 963,1040,1040,1040,1040,1040,1040,
     +1040,1040,1040,1040,1040,1040,1040,1040,1040,1040,
     +1040,1040,1040,1040,1040,1040,1040,1040,1040,1040/
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that the workstation identifier is valid.
C
      CALL GZCKWK(20,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check for invalid workstation categories (we are allowing for
C  category MO and return the extent box for the NCAR fonts there
C  if the font number is valid).  Also, the extent box for NCAR fonts
C  is returned for OUTPUT workstations.
C
      CALL GQWKC(WKID,ERRIND,ICONID,ITYPE)
      IF (ERRIND .NE. 0) GO TO 100
      CALL GQWKCA(ITYPE,ERRIND,ICAT)
      IF (ERRIND .NE. 0) GO TO 100
      IF (ICAT.EQ.GINPUT .OR. ICAT.EQ.GWISS .OR. ICAT.EQ.GMI) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -293
      CALL GZROI(0)
      IL1   = 6
      IL2   = 6
      RL1   = 7
      RL2   = 7
      ID(1) = WKID
C
C  Individual settings.
C
      ID(2) = CTXFP(1)
      ID(3) = CTXFP(2)
      ID(4) = CTXP
      ID(5) = CTXAL(1)
      ID(6) = CTXAL(2)
C
C  Bundled settings.
C
      ID(7) = CTXI
      ID(8) = CTXFPA
      ID(9) = CCHXPA
      ID(8) = CCHSPA
      RX(1) = CCHXP
      RX(2) = CCHSP
      RX(3) = CCHH
      RX(4) = CCHUP(1)
      RX(5) = CCHUP(2)
      CALL GZW2NX(1,PX,PXD)
      CALL GZW2NY(1,PY,PYD)
      RX(6) = PXD
      RX(7) = PYD
C
C  We compute the extent rectangle based on the font metrics of 
C  the NCAR fonts at stroke precision.  We would normally make the
C  call CALL GZIQWK(ITYPE,WKID) here.  If and when we are using fonts
C  supplied by a particular workstation this call will have to be made.
C
      DO 10 I=1,NFNT
      IF (ID(2) .EQ. IFNTNM(I)) THEN
        INDX = I
        GO TO 20
      ENDIF
   10 CONTINUE
C
C  Font number is not a valid NCAR font number.
C
      GO TO 100     
   20 CONTINUE
C
C  Compute the scale factor based on the charecter height.
C
      FTOP = FMETRX(2,INDX)
      FCAP = FMETRX(3,INDX)
      FBAS = FMETRX(4,INDX)
      FBOT = FMETRX(5,INDX)
      FHLF = .5*(FCAP+FBAS)
      HSCALE = CCHH/(FCAP-FBAS)
C
C  Compute the scale factor based on the aspect ratio.
C
      CALL GQCNTN(IER,NTOLD)
      CALL GQNT(NTOLD,IER,TWIND,TVIEW)
      A1 = (TWIND(2)-TWIND(1))/(TWIND(4)-TWIND(3))
      A2 = (TVIEW(2)-TVIEW(1))/(TVIEW(4)-TVIEW(3))
      ALPHA = A2/A1
      BASEX =  ALPHA*CCHUP(2)
      BASEY = -ALPHA*CCHUP(1)
      UPLEN  = SQRT(REAL(CCHUP(1)**2+CCHUP(2)**2))
      BASLEN = SQRT(REAL(BASEX**2+BASEY**2))
      ASCALE = 1.
      BASXN  = BASEX/BASLEN
      BASYN  = BASEY/BASLEN
C
C  Number of characters in the input string.
C
      ICOUNT = LEN(STRX)
C
C  Compute the character spacing in world coordinates.
C
      CWCSPC = CCHSP*(FCAP-FBAS)*HSCALE
C
C  Calculate the width and height of the text extent box.
C
      IF (CTXP.EQ.0 .OR. CTXP.EQ.1) THEN
C
C  Path is right or left.
C
        XP = 0.
        XMN = 0.
        XMX = -1.E12
        DO 30 I=1,ICOUNT
          JNDX = ICHAR(STRX(I:I))
          IF (JNDX.LT.32 .OR. JNDX.GT.126) JNDX = 32
          JNDX = JNDX-26
C
C  Set width of undefined characters to the width of a blank.
C
          IF (FMETRX(JNDX,INDX) .EQ. 0) 
     +                 FMETRX(JNDX,INDX) = FMETRX(6,INDX)
          CWDTH = FMETRX(JNDX,INDX)*HSCALE*ASCALE*CCHXP
          XMX = MAX(XMX,XP+CWDTH)
          XP = XP+CWDTH
          IF (I .EQ. ICOUNT) GO TO 40
          XP = XP+CWCSPC*ASCALE
   30   CONTINUE
   40   CONTINUE
        XL = XMN
        XR = XMX
      ELSE IF(CTXP.EQ.2 .OR. CTXP.EQ.3) THEN
C
C  Path is up or down.
C
        IMX = 0
        DO 50 I=1,ICOUNT
          JNDX = ICHAR(STRX(I:I))-26
          IWDTH = FMETRX(JNDX,INDX)
          IMX = MAX(IMX,IWDTH)
   50   CONTINUE
        XL = 0.
        XR = REAL(IMX)*HSCALE*ASCALE*CCHXP
        XP = 0.
        XMN = 1.E12
        XMX = -1.E12
        HGT = REAL(FTOP-FBOT)*HSCALE
        DO 60 I=1,ICOUNT
          XMN = MIN(XMN,XP)
          XMX = MAX(XMX,XP+HGT)
          XP = XP+HGT
          IF (I .EQ. ICOUNT) GO TO 70
          XP = XP+CWCSPC
   60   CONTINUE
   70   CONTINUE
        YB = XMN
        YT = XMX
      ENDIF
C
C  Run through all possible text paths and alignments to calculate the
C  text extent rectangle and concatenation point before rotation.
C
      IF (CTXP.EQ.0 .OR. CTXP.EQ.1) THEN
C
C  Path is right or left.
C
        IF (CTXAL(1) .EQ. 1) THEN
C
C  Horizontal alignment is left.
C
          TXEXPX(1) = PX
          TXEXPX(2) = PX+(XR-XL)
          TXEXPX(3) = TXEXPX(2)
          TXEXPX(4) = TXEXPX(1)
          CPX = TXEXPX(2)+CWCSPC*ASCALE
          CPY = PY
        ELSE IF (CTXAL(1) .EQ. 2) THEN
C
C  Horizontal alignment is center.
C
          TXEXPX(1) = PX-.5*(XR-XL)
          TXEXPX(2) = PX+.5*(XR-XL)
          TXEXPX(3) = TXEXPX(2)
          TXEXPX(4) = TXEXPX(1)
          CPX = PX
          CPY = PY
        ELSE IF (CTXAL(1) .EQ. 3) THEN
C
C  Horizontal alignment is right.
C
          TXEXPX(1) = PX-(XR-XL)
          TXEXPX(2) = PX
          TXEXPX(3) = TXEXPX(2)
          TXEXPX(4) = TXEXPX(1)
          CPX = TXEXPX(1)-CWCSPC*ASCALE
          CPY = PY
        ENDIF
C
        IF (CTXAL(2) .EQ. 1) THEN
C
C  Vertical alignment is top.
C
          TXEXPY(1) = PY-REAL(FTOP-FBOT)*HSCALE
          TXEXPY(2) = TXEXPY(1)
          TXEXPY(3) = PY
          TXEXPY(4) = TXEXPY(3)
        ELSE IF (CTXAL(2) .EQ. 2) THEN
C
C  Vertical alignment is cap.
C
          TXEXPY(1) = PY-REAL(FCAP-FBOT)*HSCALE
          TXEXPY(2) = TXEXPY(1)
          TXEXPY(3) = PY+REAL(FTOP-FCAP)*HSCALE
          TXEXPY(4) = TXEXPY(3)
        ELSE IF (CTXAL(2) .EQ. 3) THEN
C
C  Vertical alignment is half.
C
          TXEXPY(1) = PY-REAL(FHLF-FBOT)*HSCALE
          TXEXPY(2) = TXEXPY(1)
          TXEXPY(3) = PY+REAL(FTOP-FHLF)*HSCALE
          TXEXPY(4) = TXEXPY(3)
        ELSE IF (CTXAL(2) .EQ. 4) THEN
C
C  Vertical alignment is base.
C
          TXEXPY(1) = PY-REAL(FBAS-FBOT)*HSCALE
          TXEXPY(2) = TXEXPY(1)
          TXEXPY(3) = PY+REAL(FTOP-FBAS)*HSCALE
          TXEXPY(4) = TXEXPY(3)
        ELSE IF (CTXAL(2) .EQ. 5) THEN
C
C  Vertical alignment is bottom.
C
          TXEXPY(1) = PY
          TXEXPY(2) = TXEXPY(1)
          TXEXPY(3) = PY+REAL(FTOP-FBOT)*HSCALE
          TXEXPY(4) = TXEXPY(3)
        ENDIF
      ELSE IF (CTXP.EQ.2 .OR. CTXP.EQ.3) THEN
C
C  Path is up or down.
C
        IF (CTXAL(1) .EQ. 1) THEN
C
C  Horizontal alignment is left.
C
          TXEXPX(1) = PX
          TXEXPX(2) = PX+(XR-XL)
          TXEXPX(3) = TXEXPX(2)
          TXEXPX(4) = TXEXPX(1)
        ELSE IF (CTXAL(1) .EQ. 2) THEN
C
C  Horizontal alignment is center.
C
          XHWD = .5*(XL+XR)
          TXEXPX(1) = PX-XHWD
          TXEXPX(2) = PX+XHWD
          TXEXPX(3) = TXEXPX(2)
          TXEXPX(4) = TXEXPX(1)
        ELSE IF (CTXAL(1) .EQ. 3) THEN
C
C  Horizontal alignment is right.
C
          TXEXPX(1) = PX-(XR-XL)
          TXEXPX(2) = PX
          TXEXPX(3) = TXEXPX(2)
          TXEXPX(4) = TXEXPX(1)
        ENDIF
C
        IF (CTXAL(2) .EQ. 1) THEN
C
C  Vertical alignment is top.
C
          TXEXPY(1) = PY-(YT-YB)
          TXEXPY(2) = TXEXPY(1)
          TXEXPY(3) = PY
          TXEXPY(4) = TXEXPY(3)
          CPX = PX
          CPY = TXEXPY(1)-ASCALE*CWCSPC
        ELSE IF (CTXAL(2) .EQ. 2) THEN
C
C  Vertical alignment is cap.
C
          TXEXPY(1) = PY-(YT-YB+REAL(FTOP-FCAP)*HSCALE)
          TXEXPY(2) = TXEXPY(1)
          TXEXPY(3) = PY+REAL(FTOP-FCAP)*HSCALE
          TXEXPY(4) = TXEXPY(3)
          CPX = PX
          CPY = TXEXPY(1)-ASCALE*CWCSPC
        ELSE IF (CTXAL(2) .EQ. 3) THEN
C
C  Vertical alignment is half.
C
          YHLF = .5*(YT-YB)
          TXEXPY(1) = PY-YHLF
          TXEXPY(2) = TXEXPY(1)
          TXEXPY(3) = PY+YHLF
          TXEXPY(4) = TXEXPY(3)
          CPX = PX
          CPY = PY
        ELSE IF (CTXAL(2) .EQ. 4) THEN
C
C  Vertical alignment is base.
C
          TXEXPY(1) = PY-REAL(FBAS-FBOT)*HSCALE
          TXEXPY(2) = TXEXPY(1)
          TXEXPY(3) = TXEXPY(1)+(YT-YB)
          TXEXPY(4) = TXEXPY(3)
          CPX = PX
          CPY = TXEXPY(3)+ASCALE*CWCSPC
        ELSE IF (CTXAL(2) .EQ. 5) THEN
C
C  Vertical alignment is bottom.
C
          TXEXPY(1) = PY
          TXEXPY(2) = TXEXPY(1)
          TXEXPY(3) = PY+(YT-YB)
          TXEXPY(4) = TXEXPY(3)
          CPX = PX
          CPY = TXEXPY(3)+ASCALE*CWCSPC
        ENDIF
      ENDIF
C
C  Rotate the extent box and concatenation point.
C
      XT =  CCHUP(2)
      YT = -CCHUP(1)
      RR =  SQRT(XT*XT+YT*YT)
      COST = XT/RR
      SINT = YT/RR
C  80 CONTINUE
      RASCAL = 1./ASCALE
      DO 90 J=1,4
        DX = RASCAL*(TXEXPX(J)-PX)
        DY = TXEXPY(J)-PY
        TXEXPX(J) = PX+ASCALE*(DX*BASXN-DY*BASYN)
        TXEXPY(J) = PY+DX*BASYN+DY*BASXN
   90   CONTINUE
C
      DX = RASCAL*(CPX-PX)
      DY = CPY-PY
      CPX = PX+ASCALE*(DX*BASXN-DY*BASYN)
      CPY = PY+DX*BASYN+DY*BASXN
C
      RETURN
C
C  Interface call.
C
C     IF (LEN(STRX) .GT. 128) THEN
C       RERR = -110
C       GO TO 100
C     ENDIF
C     CALL GZIQWK(ITYPE,WKID)
C     IF (RERR .NE. 0) THEN
C       ERRIND = RERR
C       GOTO 100
C     ENDIF
C
C  Receive desired information in NDC space, and convert to WC.
C
C     CALL GZN2WX(1,RX(8),CPX)
C     CALL GZN2WY(1,RX(9),CPY)
C     CALL GZN2WX(1,RX(10),TXEXPX(1))
C     CALL GZN2WX(1,RX(11),TXEXPX(2))
C     CALL GZN2WX(1,RX(12),TXEXPX(3))
C     CALL GZN2WX(1,RX(13),TXEXPX(4))
C     CALL GZN2WY(1,RX(14),TXEXPY(1))
C     CALL GZN2WY(1,RX(15),TXEXPY(2))
C     CALL GZN2WY(1,RX(16),TXEXPY(3))
C     CALL GZN2WY(1,RX(17),TXEXPY(4))
C     RETURN
C
  100 CONTINUE
      CPX = 1.E20
      CPY = 1.E20
      TXEXPX(1) =  0.
      TXEXPX(2) =  0.
      TXEXPX(3) =  0.
      TXEXPX(4) =  0.
      TXEXPY(1) =  0.
      TXEXPY(2) =  0.
      TXEXPY(3) =  0.
      TXEXPY(4) =  0.
      RETURN
      END
