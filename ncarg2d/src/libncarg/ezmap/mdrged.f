C
C $Id: mdrged.f,v 1.8 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGED (IRGL,IPID,ILAT,ILON,ICWP,XCRA,YCRA,NCRA,MCRA)
C
        REAL XCRA(MCRA),YCRA(MCRA)
C
C This routine is called to see if the polygon defined by the input
C arguments is on a list of known self-intersecting polygons in the
C RANGS/GSHHS database and, if so, to edit it to fix the problem.
C
C Declare all the arrays defining the editing commands.
C
        PARAMETER    (NPOL=137,NINS=641)
C
        CHARACTER*19 PNAM(NPOL)
        INTEGER      IBEG(NPOL)
        INTEGER      ISPX(NPOL)
        INTEGER      ISPY(NPOL)
C
        INTEGER      INTS(NINS)
C
C TNAM is a variable in which to construct a polygon name to be tested
C against the ones in the edit list.
C
        CHARACTER*19 TNAM
C
C ALPH is a variable containing an upper-case alphabet.
C
        CHARACTER*26 ALPH
C
C Define the contents of the edit list.  These data statements are made
C up in an automated way.
C
        DATA PNAM(   1) / '0-000004-152-293-1A' /
        DATA IBEG(   1) /        1 /
        DATA INTS(   1) /    -5048 /
        DATA INTS(   2) /    -5047 /
        DATA ISPX(   1) /      556 /
        DATA ISPY(   1) /    40250 /
        DATA INTS(   3) /        0 /
        DATA PNAM(   2) / '0-000033-089-293-0A' /
        DATA IBEG(   2) /        4 /
        DATA INTS(   4) /      -70 /
        DATA INTS(   5) /      -69 /
        DATA INTS(   6) /       68 /
        DATA INTS(   7) /   654213 /
        DATA INTS(   8) /   598333 /
        DATA ISPX(   2) /   999444 /
        DATA ISPY(   2) /   656389 /
        DATA INTS(   9) /        0 /
        DATA PNAM(   3) / '0-000135-062-304-0A' /
        DATA IBEG(   3) /       10 /
        DATA INTS(  10) /      -54 /
        DATA INTS(  11) /       53 /
        DATA INTS(  12) /   214283 /
        DATA INTS(  13) /   566649 /
        DATA ISPX(   3) /     3056 /
        DATA ISPY(   3) /   685556 /
        DATA INTS(  14) /        0 /
        DATA PNAM(   4) / '0-000173-156-231-0A' /
        DATA IBEG(   4) /       15 /
        DATA INTS(  15) /      -55 /
        DATA ISPX(   4) /     3333 /
        DATA ISPY(   4) /   197222 /
        DATA INTS(  16) /        0 /
        DATA PNAM(   5) / '0-000177-136-282-0A' /
        DATA IBEG(   5) /       17 /
        DATA INTS(  17) /      -66 /
        DATA INTS(  18) /      -65 /
        DATA INTS(  19) /       64 /
        DATA INTS(  20) /   314801 /
        DATA INTS(  21) /   185974 /
        DATA ISPX(   5) /   704722 /
        DATA ISPY(   5) /      833 /
        DATA INTS(  22) /        0 /
        DATA PNAM(   6) / '0-000234-105-347-0A' /
        DATA IBEG(   6) /       23 /
        DATA INTS(  23) /     -119 /
        DATA INTS(  24) /     -118 /
        DATA INTS(  25) /     -117 /
        DATA INTS(  26) /      116 /
        DATA INTS(  27) /   215435 /
        DATA INTS(  28) /   149825 /
        DATA ISPX(   6) /     6111 /
        DATA ISPY(   6) /   507222 /
        DATA INTS(  29) /        0 /
        DATA PNAM(   7) / '0-000239-095-006-0A' /
        DATA IBEG(   7) /       30 /
        DATA INTS(  30) /     -126 /
        DATA INTS(  31) /     -125 /
        DATA INTS(  32) /      124 /
        DATA INTS(  33) /   478774 /
        DATA INTS(  34) /   432081 /
        DATA ISPX(   7) /   713889 /
        DATA ISPY(   7) /   997778 /
        DATA INTS(  35) /        0 /
        DATA PNAM(   8) / '0-000239-097-009-0A' /
        DATA IBEG(   8) /       36 /
        DATA INTS(  36) /       -9 /
        DATA INTS(  37) /        8 /
        DATA INTS(  38) /    30140 /
        DATA INTS(  39) /   870382 /
        DATA ISPX(   8) /     2222 /
        DATA ISPY(   8) /   868611 /
        DATA INTS(  40) /        0 /
        DATA PNAM(   9) / '0-000239-098-006-0A' /
        DATA IBEG(   9) /       41 /
        DATA INTS(  41) /     -160 /
        DATA INTS(  42) /     -159 /
        DATA INTS(  43) /      158 /
        DATA INTS(  44) /   378222 /
        DATA INTS(  45) /   658894 /
        DATA ISPX(   9) /   773056 /
        DATA ISPY(   9) /     4444 /
        DATA INTS(  46) /        0 /
        DATA PNAM(  10) / '0-000279-122-046-0A' /
        DATA IBEG(  10) /       47 /
        DATA INTS(  47) /     -253 /
        DATA INTS(  48) /     -252 /
        DATA INTS(  49) /      251 /
        DATA INTS(  50) /   874445 /
        DATA INTS(  51) /     4908 /
        DATA ISPX(  10) /     1389 /
        DATA ISPY(  10) /   615278 /
        DATA INTS(  52) /        0 /
        DATA PNAM(  11) / '0-000444-104-355-0A' /
        DATA IBEG(  11) /       53 /
        DATA INTS(  53) /     -109 /
        DATA INTS(  54) /     -108 /
        DATA INTS(  55) /      107 /
        DATA INTS(  56) /   519013 /
        DATA INTS(  57) /   242999 /
        DATA ISPX(  11) /     4444 /
        DATA ISPY(  11) /   138333 /
        DATA INTS(  58) /        0 /
        DATA PNAM(  12) / '0-000479-157-153-0A' /
        DATA IBEG(  12) /       59 /
        DATA INTS(  59) /       12 /
        DATA INTS(  60) /   907556 /
        DATA INTS(  61) /   497511 /
        DATA ISPX(  12) /   994444 /
        DATA ISPY(  12) /   536111 /
        DATA INTS(  62) /        0 /
        DATA PNAM(  13) / '0-000504-152-045-0A' /
        DATA IBEG(  13) /       63 /
        DATA INTS(  63) /      -39 /
        DATA INTS(  64) /       38 /
        DATA INTS(  65) /   126431 /
        DATA INTS(  66) /     1873 /
        DATA ISPX(  13) /     2778 /
        DATA ISPY(  13) /   229444 /
        DATA INTS(  67) /        0 /
        DATA PNAM(  14) / '0-000678-113-111-0A' /
        DATA IBEG(  14) /       68 /
        DATA INTS(  68) /      145 /
        DATA INTS(  69) /  -383612 /
        DATA INTS(  70) /  -471112 /
        DATA INTS(  71) /     -149 /
        DATA INTS(  72) /     -148 /
        DATA ISPX(  14) /   996111 /
        DATA ISPY(  14) /    85556 /
        DATA INTS(  73) /        0 /
        DATA PNAM(  15) / '0-000688-161-099-0A' /
        DATA IBEG(  15) /       74 /
        DATA INTS(  74) /      -56 /
        DATA INTS(  75) /       55 /
        DATA INTS(  76) /   560327 /
        DATA INTS(  77) /   446810 /
        DATA ISPX(  15) /   997500 /
        DATA ISPY(  15) /   656111 /
        DATA INTS(  78) /        0 /
        DATA PNAM(  16) / '0-000773-073-034-0A' /
        DATA IBEG(  16) /       79 /
        DATA INTS(  79) /     -154 /
        DATA INTS(  80) /      153 /
        DATA INTS(  81) /   146111 /
        DATA INTS(  82) /   347408 /
        DATA ISPX(  16) /   862500 /
        DATA ISPY(  16) /     2778 /
        DATA INTS(  83) /        0 /
        DATA PNAM(  17) / '0-000781-113-089-0A' /
        DATA IBEG(  17) /       84 /
        DATA INTS(  84) /      -25 /
        DATA ISPX(  17) /   131667 /
        DATA ISPY(  17) /   999167 /
        DATA INTS(  85) /        0 /
        DATA PNAM(  18) / '0-000823-151-238-0A' /
        DATA IBEG(  18) /       86 /
        DATA INTS(  86) /     -106 /
        DATA INTS(  87) /      105 /
        DATA INTS(  88) /   806834 /
        DATA INTS(  89) /   793467 /
        DATA ISPX(  18) /   256667 /
        DATA ISPY(  18) /   999722 /
        DATA INTS(  90) /        0 /
        DATA PNAM(  19) / '0-000870-144-077-0A' /
        DATA IBEG(  19) /       91 /
        DATA INTS(  91) /     -402 /
        DATA ISPX(  19) /     2500 /
        DATA ISPY(  19) /   911111 /
        DATA INTS(  92) /        0 /
        DATA PNAM(  20) / '0-001596-160-278-0A' /
        DATA IBEG(  20) /       93 /
        DATA INTS(  93) /      -27 /
        DATA INTS(  94) /      -26 /
        DATA ISPX(  20) /   998333 /
        DATA ISPY(  20) /   643056 /
        DATA INTS(  95) /        0 /
        DATA PNAM(  21) / '0-001704-098-010-0A' /
        DATA IBEG(  21) /       96 /
        DATA INTS(  96) /      -50 /
        DATA INTS(  97) /      -49 /
        DATA INTS(  98) /       48 /
        DATA INTS(  99) /   138494 /
        DATA INTS( 100) /   510388 /
        DATA ISPX(  21) /      556 /
        DATA ISPY(  21) /   346389 /
        DATA INTS( 101) /        0 /
        DATA PNAM(  22) / '0-003087-105-000-1A' /
        DATA IBEG(  22) /      102 /
        DATA INTS( 102) /      -58 /
        DATA INTS( 103) /      -57 /
        DATA INTS( 104) /      -56 /
        DATA INTS( 105) /       55 /
        DATA INTS( 106) /   298390 /
        DATA INTS( 107) /   810693 /
        DATA ISPX(  22) /   118611 /
        DATA ISPY(  22) /   996111 /
        DATA INTS( 108) /        0 /
        DATA PNAM(  23) / '0-003091-107-358-0A' /
        DATA IBEG(  23) /      109 /
        DATA INTS( 109) /      -22 /
        DATA INTS( 110) /       21 /
        DATA INTS( 111) /   885510 /
        DATA INTS( 112) /    59095 /
        DATA ISPX(  23) /   946667 /
        DATA ISPY(  23) /     3889 /
        DATA INTS( 113) /        0 /
        DATA PNAM(  24) / '0-003128-101-350-0A' /
        DATA IBEG(  24) /      114 /
        DATA INTS( 114) /      114 /
        DATA INTS( 115) /   718419 /
        DATA INTS( 116) /    66791 /
        DATA ISPX(  24) /   999167 /
        DATA ISPY(  24) /   418056 /
        DATA INTS( 117) /        0 /
        DATA PNAM(  25) / '0-003128-101-351-0A' /
        DATA IBEG(  25) /      118 /
        DATA INTS( 118) /       -7 /
        DATA INTS( 119) /       -6 /
        DATA INTS( 120) /        5 /
        DATA INTS( 121) /    14018 /
        DATA INTS( 122) /   424180 /
        DATA ISPX(  25) /     5000 /
        DATA ISPY(  25) /   425000 /
        DATA INTS( 123) /        0 /
        DATA PNAM(  26) / '0-003845-104-355-0A' /
        DATA IBEG(  26) /      124 /
        DATA INTS( 124) /      -66 /
        DATA INTS( 125) /       65 /
        DATA INTS( 126) /   828889 /
        DATA INTS( 127) /   647247 /
        DATA ISPX(  26) /   995000 /
        DATA ISPY(  26) /   888056 /
        DATA INTS( 128) /        0 /
        DATA PNAM(  27) / '0-003845-105-356-0A' /
        DATA IBEG(  27) /      129 /
        DATA INTS( 129) /       -7 /
        DATA INTS( 130) /       -6 /
        DATA ISPX(  27) /    97778 /
        DATA ISPY(  27) /     1944 /
        DATA INTS( 131) /        0 /
        DATA PNAM(  28) / '0-004139-119-267-0A' /
        DATA IBEG(  28) /      132 /
        DATA INTS( 132) /     -182 /
        DATA INTS( 133) /     -181 /
        DATA INTS( 134) /     -180 /
        DATA ISPX(  28) /   204722 /
        DATA ISPY(  28) /   972778 /
        DATA INTS( 135) /        0 /
        DATA PNAM(  29) / '0-004503-125-092-0A' /
        DATA IBEG(  29) /      136 /
        DATA INTS( 136) /     -132 /
        DATA INTS( 137) /     -131 /
        DATA INTS( 138) /     -130 /
        DATA INTS( 139) /     -129 /
        DATA ISPX(  29) /   999167 /
        DATA ISPY(  29) /   638889 /
        DATA INTS( 140) /        0 /
        DATA PNAM(  30) / '0-005129-136-061-0A' /
        DATA IBEG(  30) /      141 /
        DATA INTS( 141) /      -47 /
        DATA INTS( 142) /      -46 /
        DATA INTS( 143) /      -45 /
        DATA INTS( 144) /      -44 /
        DATA ISPX(  30) /   993889 /
        DATA ISPY(  30) /   129444 /
        DATA INTS( 145) /        0 /
        DATA PNAM(  31) / '0-005199-101-003-0A' /
        DATA IBEG(  31) /      146 /
        DATA INTS( 146) /     -149 /
        DATA INTS( 147) /      148 /
        DATA INTS( 148) /   559304 /
        DATA INTS( 149) /   755876 /
        DATA ISPX(  31) /   558889 /
        DATA ISPY(  31) /   756944 /
        DATA INTS( 150) /        0 /
        DATA PNAM(  32) / '0-005227-104-001-0A' /
        DATA IBEG(  32) /      151 /
        DATA INTS( 151) /      -36 /
        DATA INTS( 152) /      -35 /
        DATA ISPX(  32) /      556 /
        DATA ISPY(  32) /   621667 /
        DATA INTS( 153) /        0 /
        DATA PNAM(  33) / '0-005331-100-106-0A' /
        DATA IBEG(  33) /      154 /
        DATA INTS( 154) /     -143 /
        DATA INTS( 155) /     -142 /
        DATA INTS( 156) /     -141 /
        DATA ISPX(  33) /   888611 /
        DATA ISPY(  33) /   629444 /
        DATA INTS( 157) /        0 /
        DATA PNAM(  34) / '0-005992-105-000-0A' /
        DATA IBEG(  34) /      158 /
        DATA INTS( 158) /     -131 /
        DATA INTS( 159) /     -130 /
        DATA INTS( 160) /        2 /
        DATA INTS( 161) /   752749 /
        DATA INTS( 162) /   347649 /
        DATA ISPX(  34) /   753333 /
        DATA ISPY(  34) /   350278 /
        DATA INTS( 163) /        0 /
        DATA PNAM(  35) / '0-007124-152-239-0A' /
        DATA IBEG(  35) /      164 /
        DATA INTS( 164) /     -109 /
        DATA INTS( 165) /     -108 /
        DATA ISPX(  35) /   345278 /
        DATA ISPY(  35) /   771111 /
        DATA INTS( 166) /        0 /
        DATA PNAM(  36) / '0-007184-126-116-0A' /
        DATA IBEG(  36) /      167 /
        DATA INTS( 167) /      -37 /
        DATA INTS( 168) /      -36 /
        DATA INTS( 169) /      -35 /
        DATA INTS( 170) /      -34 /
        DATA ISPX(  36) /   155000 /
        DATA ISPY(  36) /      278 /
        DATA INTS( 171) /        0 /
        DATA PNAM(  37) / '0-007345-131-075-0A' /
        DATA IBEG(  37) /      172 /
        DATA INTS( 172) /     -107 /
        DATA INTS( 173) /     -106 /
        DATA INTS( 174) /     -105 /
        DATA INTS( 175) /     -104 /
        DATA INTS( 176) /     -103 /
        DATA ISPX(  37) /   217500 /
        DATA ISPY(  37) /   893889 /
        DATA INTS( 177) /        0 /
        DATA PNAM(  38) / '0-008537-130-108-0A' /
        DATA IBEG(  38) /      178 /
        DATA INTS( 178) /      -93 /
        DATA INTS( 179) /      -92 /
        DATA ISPX(  38) /   815556 /
        DATA ISPY(  38) /   937500 /
        DATA INTS( 180) /        0 /
        DATA PNAM(  39) / '0-009592-153-038-0A' /
        DATA IBEG(  39) /      181 /
        DATA INTS( 181) /      -18 /
        DATA INTS( 182) /      -17 /
        DATA INTS( 183) /      -16 /
        DATA INTS( 184) /      -15 /
        DATA ISPX(  39) /     7778 /
        DATA ISPY(  39) /   193611 /
        DATA INTS( 185) /        0 /
        DATA PNAM(  40) / '0-010027-129-064-0A' /
        DATA IBEG(  40) /      186 /
        DATA INTS( 186) /      -79 /
        DATA INTS( 187) /      -78 /
        DATA INTS( 188) /      -77 /
        DATA ISPX(  40) /   871667 /
        DATA ISPY(  40) /   913611 /
        DATA INTS( 189) /        0 /
        DATA PNAM(  41) / '0-010543-128-114-0A' /
        DATA IBEG(  41) /      190 /
        DATA INTS( 190) /       -6 /
        DATA INTS( 191) /       -5 /
        DATA INTS( 192) /       -4 /
        DATA ISPX(  41) /     2778 /
        DATA ISPY(  41) /   357500 /
        DATA INTS( 193) /        0 /
        DATA PNAM(  42) / '0-012371-130-140-0A' /
        DATA IBEG(  42) /      194 /
        DATA INTS( 194) /      -65 /
        DATA INTS( 195) /      -64 /
        DATA INTS( 196) /      -63 /
        DATA INTS( 197) /       -3 /
        DATA INTS( 198) /       -2 /
        DATA ISPX(  42) /   842222 /
        DATA ISPY(  42) /   483611 /
        DATA INTS( 199) /        0 /
        DATA PNAM(  43) / '0-014541-141-069-0A' /
        DATA IBEG(  43) /      200 /
        DATA INTS( 200) /      -55 /
        DATA INTS( 201) /      -54 /
        DATA INTS( 202) /       -3 /
        DATA INTS( 203) /       -2 /
        DATA ISPX(  43) /   667500 /
        DATA ISPY(  43) /   192778 /
        DATA INTS( 204) /        0 /
        DATA PNAM(  44) / '0-015249-142-081-0A' /
        DATA IBEG(  44) /      205 /
        DATA INTS( 205) /      -52 /
        DATA INTS( 206) /       51 /
        DATA INTS( 207) /   799336 /
        DATA INTS( 208) /   488861 /
        DATA ISPX(  44) /   788611 /
        DATA ISPY(  44) /   482222 /
        DATA INTS( 209) /        0 /
        DATA PNAM(  45) / '0-015808-158-036-0A' /
        DATA IBEG(  45) /      210 /
        DATA INTS( 210) /      -51 /
        DATA INTS( 211) /      -50 /
        DATA ISPX(  45) /   373611 /
        DATA ISPY(  45) /   272222 /
        DATA INTS( 212) /        0 /
        DATA PNAM(  46) / '0-015816-138-061-0A' /
        DATA IBEG(  46) /      213 /
        DATA INTS( 213) /      -25 /
        DATA INTS( 214) /      -24 /
        DATA INTS( 215) /      -23 /
        DATA INTS( 216) /      -23 /
        DATA ISPX(  46) /   998889 /
        DATA ISPY(  46) /   876667 /
        DATA INTS( 217) /        0 /
        DATA PNAM(  47) / '0-016121-159-070-0A' /
        DATA IBEG(  47) /      218 /
        DATA INTS( 218) /      -49 /
        DATA INTS( 219) /      -48 /
        DATA INTS( 220) /      -47 /
        DATA ISPX(  47) /    24444 /
        DATA ISPY(  47) /   705278 /
        DATA INTS( 221) /        0 /
        DATA PNAM(  48) / '0-017966-142-071-0A' /
        DATA IBEG(  48) /      222 /
        DATA INTS( 222) /      -45 /
        DATA INTS( 223) /       -4 /
        DATA INTS( 224) /       -3 /
        DATA INTS( 225) /       -2 /
        DATA ISPX(  48) /   250556 /
        DATA ISPY(  48) /   523333 /
        DATA INTS( 226) /        0 /
        DATA PNAM(  49) / '0-018157-034-292-0A' /
        DATA IBEG(  49) /      227 /
        DATA INTS( 227) /      -44 /
        DATA INTS( 228) /      -43 /
        DATA ISPX(  49) /   731389 /
        DATA ISPY(  49) /   938611 /
        DATA INTS( 229) /        0 /
        DATA PNAM(  50) / '0-018393-135-082-0A' /
        DATA IBEG(  50) /      230 /
        DATA INTS( 230) /      -44 /
        DATA INTS( 231) /      -43 /
        DATA ISPX(  50) /   123611 /
        DATA ISPY(  50) /   548611 /
        DATA INTS( 232) /        0 /
        DATA PNAM(  51) / '0-019281-139-075-0A' /
        DATA IBEG(  51) /      233 /
        DATA INTS( 233) /      -42 /
        DATA INTS( 234) /      -41 /
        DATA INTS( 235) /      -40 /
        DATA INTS( 236) /      -39 /
        DATA ISPX(  51) /   902778 /
        DATA ISPY(  51) /   769167 /
        DATA INTS( 237) /        0 /
        DATA PNAM(  52) / '0-019283-141-080-0A' /
        DATA IBEG(  52) /      238 /
        DATA INTS( 238) /      -42 /
        DATA INTS( 239) /      -41 /
        DATA INTS( 240) /      -40 /
        DATA INTS( 241) /      -39 /
        DATA INTS( 242) /      -38 /
        DATA ISPX(  52) /   779167 /
        DATA ISPY(  52) /   175000 /
        DATA INTS( 243) /        0 /
        DATA PNAM(  53) / '0-027252-096-124-0A' /
        DATA IBEG(  53) /      244 /
        DATA INTS( 244) /      -30 /
        DATA INTS( 245) /      -29 /
        DATA INTS( 246) /      -28 /
        DATA ISPX(  53) /   806111 /
        DATA ISPY(  53) /   968611 /
        DATA INTS( 247) /        0 /
        DATA PNAM(  54) / '0-034840-096-124-0A' /
        DATA IBEG(  54) /      248 /
        DATA INTS( 248) /      -23 /
        DATA INTS( 249) /      -22 /
        DATA INTS( 250) /       -2 /
        DATA ISPX(  54) /   727778 /
        DATA ISPY(  54) /   263611 /
        DATA INTS( 251) /        0 /
        DATA PNAM(  55) / '0-038832-159-087-1A' /
        DATA IBEG(  55) /      252 /
        DATA INTS( 252) /       -2 /
        DATA ISPX(  55) /   970278 /
        DATA ISPY(  55) /   521667 /
        DATA INTS( 253) /        0 /
        DATA PNAM(  56) / '0-044615-141-112-0A' /
        DATA IBEG(  56) /      254 /
        DATA INTS( 254) /      -19 /
        DATA INTS( 255) /      -18 /
        DATA INTS( 256) /       -2 /
        DATA ISPX(  56) /   206389 /
        DATA ISPY(  56) /   231389 /
        DATA INTS( 257) /        0 /
        DATA PNAM(  57) / '0-044619-162-124-0A' /
        DATA IBEG(  57) /      258 /
        DATA INTS( 258) /      -19 /
        DATA INTS( 259) /       18 /
        DATA INTS( 260) /   730728 /
        DATA INTS( 261) /   652222 /
        DATA ISPX(  57) /   732778 /
        DATA ISPY(  57) /   652222 /
        DATA INTS( 262) /        0 /
        DATA PNAM(  58) / '0-057542-155-011-1A' /
        DATA IBEG(  58) /      263 /
        DATA INTS( 263) /       13 /
        DATA INTS( 264) /   963326 /
        DATA INTS( 265) /   711521 /
        DATA ISPX(  58) /   970000 /
        DATA ISPY(  58) /   708750 /
        DATA INTS( 266) /        0 /
        DATA PNAM(  59) / '0-059036-084-119-0A' /
        DATA IBEG(  59) /      267 /
        DATA INTS( 267) /      -15 /
        DATA INTS( 268) /      -14 /
        DATA ISPX(  59) /   291667 /
        DATA ISPY(  59) /   500556 /
        DATA INTS( 269) /        0 /
        DATA PNAM(  60) / '0-063364-092-125-0A' /
        DATA IBEG(  60) /      270 /
        DATA INTS( 270) /      -14 /
        DATA INTS( 271) /      -13 /
        DATA ISPX(  60) /   374167 /
        DATA ISPY(  60) /   308333 /
        DATA INTS( 272) /        0 /
        DATA PNAM(  61) / '0-063378-169-018-1A' /
        DATA IBEG(  61) /      273 /
        DATA INTS( 273) /       14 /
        DATA INTS( 274) /   698632 /
        DATA INTS( 275) /   758722 /
        DATA INTS( 276) /       13 /
        DATA INTS( 277) /   702623 /
        DATA INTS( 278) /   753728 /
        DATA INTS( 279) /      -12 /
        DATA INTS( 280) /      -11 /
        DATA INTS( 281) /       -6 /
        DATA INTS( 282) /       -5 /
        DATA INTS( 283) /       -4 /
        DATA INTS( 284) /       -3 /
        DATA INTS( 285) /       -2 /
        DATA ISPX(  61) /   696611 /
        DATA ISPY(  61) /   761250 /
        DATA INTS( 286) /        0 /
        DATA PNAM(  62) / '0-063402-152-006-1A' /
        DATA IBEG(  62) /      287 /
        DATA INTS( 287) /       14 /
        DATA INTS( 288) /   497507 /
        DATA INTS( 289) /   505389 /
        DATA INTS( 290) /       -4 /
        DATA INTS( 291) /       -3 /
        DATA INTS( 292) /       -2 /
        DATA ISPX(  62) /   494972 /
        DATA ISPY(  62) /   503694 /
        DATA INTS( 293) /        0 /
        DATA PNAM(  63) / '0-074230-152-006-1A' /
        DATA IBEG(  63) /      294 /
        DATA INTS( 294) /       12 /
        DATA INTS( 295) /   824283 /
        DATA INTS( 296) /   794197 /
        DATA INTS( 297) /       -2 /
        DATA ISPX(  63) /   824083 /
        DATA ISPY(  63) /   793333 /
        DATA INTS( 298) /        0 /
        DATA PNAM(  64) / '0-074722-150-022-1A' /
        DATA IBEG(  64) /      299 /
        DATA INTS( 299) /       12 /
        DATA INTS( 300) /   170551 /
        DATA INTS( 301) /   161222 /
        DATA INTS( 302) /       -2 /
        DATA ISPX(  64) /   168361 /
        DATA ISPY(  64) /   160389 /
        DATA INTS( 303) /        0 /
        DATA PNAM(  65) / '0-089851-157-013-1A' /
        DATA IBEG(  65) /      304 /
        DATA INTS( 304) /        8 /
        DATA INTS( 305) /   907655 /
        DATA INTS( 306) /   409090 /
        DATA ISPX(  65) /   905806 /
        DATA ISPY(  65) /   407472 /
        DATA INTS( 307) /        0 /
        DATA PNAM(  66) / '0-089858-155-011-1A' /
        DATA IBEG(  66) /      308 /
        DATA INTS( 308) /        6 /
        DATA INTS( 309) /    38285 /
        DATA INTS( 310) /    27857 /
        DATA ISPX(  66) /    35833 /
        DATA ISPY(  66) /    26639 /
        DATA INTS( 311) /        0 /
        DATA PNAM(  67) / '0-089875-155-024-1A' /
        DATA IBEG(  67) /      312 /
        DATA INTS( 312) /      -10 /
        DATA INTS( 313) /        9 /
        DATA INTS( 314) /   126948 /
        DATA INTS( 315) /   687273 /
        DATA ISPX(  67) /   133139 /
        DATA ISPY(  67) /   685389 /
        DATA INTS( 316) /        0 /
        DATA PNAM(  68) / '0-089880-159-015-1A' /
        DATA IBEG(  68) /      317 /
        DATA INTS( 317) /      -10 /
        DATA INTS( 318) /       -9 /
        DATA INTS( 319) /        8 /
        DATA INTS( 320) /   806967 /
        DATA INTS( 321) /   190547 /
        DATA ISPX(  68) /   816611 /
        DATA ISPY(  68) /   188722 /
        DATA INTS( 322) /        0 /
        DATA PNAM(  69) / '0-089888-149-265-1A' /
        DATA IBEG(  69) /      323 /
        DATA INTS( 323) /      -10 /
        DATA INTS( 324) /        9 /
        DATA INTS( 325) /    50630 /
        DATA INTS( 326) /    76467 /
        DATA ISPX(  69) /    53639 /
        DATA ISPY(  69) /    76056 /
        DATA INTS( 327) /        0 /
        DATA PNAM(  70) / '0-089905-134-014-1A' /
        DATA IBEG(  70) /      328 /
        DATA INTS( 328) /      -10 /
        DATA INTS( 329) /        9 /
        DATA INTS( 330) /   878622 /
        DATA INTS( 331) /   240397 /
        DATA ISPX(  70) /   883278 /
        DATA ISPY(  70) /   237056 /
        DATA INTS( 332) /        0 /
        DATA PNAM(  71) / '0-101789-095-000-1A' /
        DATA IBEG(  71) /      333 /
        DATA INTS( 333) /      -10 /
        DATA ISPX(  71) /   654139 /
        DATA ISPY(  71) /   816222 /
        DATA INTS( 334) /        0 /
        DATA PNAM(  72) / '0-161847-147-011-1A' /
        DATA IBEG(  72) /      335 /
        DATA INTS( 335) /        5 /
        DATA INTS( 336) /   908287 /
        DATA INTS( 337) /   304116 /
        DATA INTS( 338) /       -8 /
        DATA INTS( 339) /       -3 /
        DATA INTS( 340) /        2 /
        DATA INTS( 341) /   910545 /
        DATA INTS( 342) /   304606 /
        DATA ISPX(  72) /   913278 /
        DATA ISPY(  72) /   305389 /
        DATA INTS( 343) /        0 /
        DATA PNAM(  73) / '0-161855-150-026-1A' /
        DATA IBEG(  73) /      344 /
        DATA INTS( 344) /       -7 /
        DATA INTS( 345) /       -6 /
        DATA INTS( 346) /       -5 /
        DATA INTS( 347) /        4 /
        DATA INTS( 348) /   747417 /
        DATA INTS( 349) /   418736 /
        DATA ISPX(  73) /   751583 /
        DATA ISPY(  73) /   417083 /
        DATA INTS( 350) /        0 /
        DATA PNAM(  74) / '0-170632-153-019-1A' /
        DATA IBEG(  74) /      351 /
        DATA INTS( 351) /        4 /
        DATA INTS( 352) /   897674 /
        DATA INTS( 353) /   522557 /
        DATA ISPX(  74) /   900028 /
        DATA ISPY(  74) /   519528 /
        DATA INTS( 354) /        0 /
        DATA PNAM(  75) / '0-174934-150-024-1A' /
        DATA IBEG(  75) /      355 /
        DATA INTS( 355) /       -6 /
        DATA INTS( 356) /        5 /
        DATA INTS( 357) /    93904 /
        DATA INTS( 358) /    29577 /
        DATA ISPX(  75) /    98250 /
        DATA ISPY(  75) /    27944 /
        DATA INTS( 359) /        0 /
        DATA PNAM(  76) / '0-184887-167-022-1A' /
        DATA IBEG(  76) /      360 /
        DATA INTS( 360) /        3 /
        DATA INTS( 361) /   111881 /
        DATA INTS( 362) /   112292 /
        DATA ISPX(  76) /   109917 /
        DATA ISPY(  76) /   110417 /
        DATA INTS( 363) /        0 /
        DATA PNAM(  77) / '0-184923-161-053-1A' /
        DATA IBEG(  77) /      364 /
        DATA INTS( 364) /       -5 /
        DATA INTS( 365) /        4 /
        DATA INTS( 366) /   219878 /
        DATA INTS( 367) /   259574 /
        DATA ISPX(  77) /   225028 /
        DATA ISPY(  77) /   258694 /
        DATA INTS( 368) /        0 /
        DATA PNAM(  78) / '0-184945-155-023-1A' /
        DATA IBEG(  78) /      369 /
        DATA INTS( 369) /        3 /
        DATA INTS( 370) /   684264 /
        DATA INTS( 371) /   770993 /
        DATA ISPX(  78) /   684889 /
        DATA ISPY(  78) /   769556 /
        DATA INTS( 372) /        0 /
        DATA PNAM(  79) / '0-184965-155-025-1A' /
        DATA IBEG(  79) /      373 /
        DATA INTS( 373) /       -5 /
        DATA INTS( 374) /        4 /
        DATA INTS( 375) /   267078 /
        DATA INTS( 376) /   345413 /
        DATA ISPX(  79) /   273361 /
        DATA ISPY(  79) /   342917 /
        DATA INTS( 377) /        0 /
        DATA PNAM(  80) / '0-184982-153-021-1A' /
        DATA IBEG(  80) /      378 /
        DATA INTS( 378) /       -6 /
        DATA INTS( 379) /       -5 /
        DATA INTS( 380) /        4 /
        DATA INTS( 381) /   234907 /
        DATA INTS( 382) /   332102 /
        DATA INTS( 383) /        2 /
        DATA INTS( 384) /   232464 /
        DATA INTS( 385) /   334518 /
        DATA ISPX(  80) /   231583 /
        DATA ISPY(  80) /   335389 /
        DATA INTS( 386) /        0 /
        DATA PNAM(  81) / '0-185056-150-027-1A' /
        DATA IBEG(  81) /      387 /
        DATA INTS( 387) /       -6 /
        DATA ISPX(  81) /    18250 /
        DATA ISPY(  81) /   457889 /
        DATA INTS( 388) /        0 /
        DATA PNAM(  82) / '0-185100-150-021-1A' /
        DATA IBEG(  82) /      389 /
        DATA INTS( 389) /       -5 /
        DATA ISPX(  82) /   541583 /
        DATA ISPY(  82) /    85389 /
        DATA INTS( 390) /        0 /
        DATA PNAM(  83) / '0-185131-150-021-1A' /
        DATA IBEG(  83) /      391 /
        DATA INTS( 391) /       -6 /
        DATA ISPX(  83) /    53306 /
        DATA ISPY(  83) /   442028 /
        DATA INTS( 392) /        0 /
        DATA PNAM(  84) / '0-185193-148-021-1A' /
        DATA IBEG(  84) /      393 /
        DATA INTS( 393) /       -5 /
        DATA ISPX(  84) /   996667 /
        DATA ISPY(  84) /   371250 /
        DATA INTS( 394) /        0 /
        DATA PNAM(  85) / '0-185251-150-018-1A' /
        DATA IBEG(  85) /      395 /
        DATA INTS( 395) /        3 /
        DATA INTS( 396) /   477535 /
        DATA INTS( 397) /   457482 /
        DATA ISPX(  85) /   478333 /
        DATA ISPY(  85) /   456278 /
        DATA INTS( 398) /        0 /
        DATA PNAM(  86) / '0-187373-155-011-1A' /
        DATA IBEG(  86) /      399 /
        DATA INTS( 399) /       -4 /
        DATA INTS( 400) /        3 /
        DATA INTS( 401) /   866097 /
        DATA INTS( 402) /   184602 /
        DATA ISPX(  86) /   863333 /
        DATA ISPY(  86) /   185389 /
        DATA INTS( 403) /        0 /
        DATA PNAM(  87) / '0-187456-149-030-1A' /
        DATA IBEG(  87) /      404 /
        DATA INTS( 404) /       -4 /
        DATA INTS( 405) /        3 /
        DATA INTS( 406) /    49141 /
        DATA INTS( 407) /   866068 /
        DATA ISPX(  87) /    50833 /
        DATA ISPY(  87) /   867444 /
        DATA INTS( 408) /        0 /
        DATA PNAM(  88) / '0-187631-150-020-1A' /
        DATA IBEG(  88) /      409 /
        DATA INTS( 409) /       -5 /
        DATA INTS( 410) /        2 /
        DATA INTS( 411) /   545814 /
        DATA INTS( 412) /   388348 /
        DATA ISPX(  88) /   545000 /
        DATA ISPY(  88) /   389583 /
        DATA INTS( 413) /        0 /
        DATA PNAM(  89) / '0-187701-150-028-1A' /
        DATA IBEG(  89) /      414 /
        DATA INTS( 414) /       -4 /
        DATA ISPX(  89) /    53222 /
        DATA ISPY(  89) /   508722 /
        DATA INTS( 415) /        0 /
        DATA PNAM(  90) / '0-187713-153-021-1A' /
        DATA IBEG(  90) /      416 /
        DATA INTS( 416) /       -4 /
        DATA INTS( 417) /        3 /
        DATA INTS( 418) /   970082 /
        DATA INTS( 419) /   280301 /
        DATA ISPX(  90) /   969167 /
        DATA ISPY(  90) /   281667 /
        DATA INTS( 420) /        0 /
        DATA PNAM(  91) / '0-187754-153-022-1A' /
        DATA IBEG(  91) /      421 /
        DATA INTS( 421) /       -5 /
        DATA INTS( 422) /        2 /
        DATA INTS( 423) /    60823 /
        DATA INTS( 424) /   307186 /
        DATA ISPX(  91) /    59944 /
        DATA ISPY(  91) /   307917 /
        DATA INTS( 425) /        0 /
        DATA PNAM(  92) / '1-000000-111-106-1A' /
        DATA IBEG(  92) /      426 /
        DATA INTS( 426) /       -6 /
        DATA INTS( 427) /        5 /
        DATA INTS( 428) /   768515 /
        DATA INTS( 429) /     5333 /
        DATA ISPX(  92) /   767833 /
        DATA ISPY(  92) /     1611 /
        DATA INTS( 430) /        0 /
        DATA PNAM(  93) / '1-000000-115-061-1A' /
        DATA IBEG(  93) /      431 /
        DATA INTS( 431) /      -92 /
        DATA ISPX(  93) /     9889 /
        DATA ISPY(  93) /   212861 /
        DATA INTS( 432) /        0 /
        DATA PNAM(  94) / '1-000017-143-350-1A' /
        DATA IBEG(  94) /      433 /
        DATA INTS( 433) /     -398 /
        DATA ISPX(  94) /    30778 /
        DATA ISPY(  94) /   570806 /
        DATA INTS( 434) /        0 /
        DATA PNAM(  95) / '1-008472-110-106-1A' /
        DATA IBEG(  95) /      435 /
        DATA INTS( 435) /        9 /
        DATA INTS( 436) /   866839 /
        DATA INTS( 437) /   820754 /
        DATA ISPX(  95) /   854528 /
        DATA ISPY(  95) /   819972 /
        DATA INTS( 438) /        0 /
        DATA PNAM(  96) / '1-012327-118-277-1A' /
        DATA IBEG(  96) /      439 /
        DATA INTS( 439) /        9 /
        DATA INTS( 440) /   330858 /
        DATA INTS( 441) /   877244 /
        DATA ISPX(  96) /   325389 /
        DATA ISPY(  96) /   877472 /
        DATA INTS( 442) /        0 /
        DATA PNAM(  97) / '1-012837-118-277-1A' /
        DATA IBEG(  97) /      443 /
        DATA INTS( 443) /        8 /
        DATA INTS( 444) /   346484 /
        DATA INTS( 445) /   888387 /
        DATA INTS( 446) /        9 /
        DATA INTS( 447) /   342455 /
        DATA INTS( 448) /   889241 /
        DATA ISPX(  97) /   334583 /
        DATA ISPY(  97) /   889167 /
        DATA INTS( 449) /        0 /
        DATA PNAM(  98) / '1-021796-098-281-1A' /
        DATA IBEG(  98) /      450 /
        DATA INTS( 450) /       -5 /
        DATA INTS( 451) /        4 /
        DATA INTS( 452) /    94957 /
        DATA INTS( 453) /   476288 /
        DATA ISPX(  98) /   100972 /
        DATA ISPY(  98) /   482250 /
        DATA INTS( 454) /        0 /
        DATA PNAM(  99) / '1-022827-159-223-1A' /
        DATA IBEG(  99) /      455 /
        DATA INTS( 455) /       -8 /
        DATA INTS( 456) /        7 /
        DATA INTS( 457) /   847202 /
        DATA INTS( 458) /   114206 /
        DATA ISPX(  99) /   862917 /
        DATA ISPY(  99) /   109417 /
        DATA INTS( 459) /        0 /
        DATA PNAM( 100) / '1-028620-117-262-1A' /
        DATA IBEG( 100) /      460 /
        DATA INTS( 460) /        4 /
        DATA INTS( 461) /   625193 /
        DATA INTS( 462) /   287187 /
        DATA ISPX( 100) /   622111 /
        DATA ISPY( 100) /   282472 /
        DATA INTS( 463) /        0 /
        DATA PNAM( 101) / '1-029783-119-270-1A' /
        DATA IBEG( 101) /      464 /
        DATA INTS( 464) /        2 /
        DATA INTS( 465) /    13503 /
        DATA INTS( 466) /   282582 /
        DATA ISPX( 101) /     2861 /
        DATA ISPY( 101) /   280806 /
        DATA INTS( 467) /        0 /
        DATA PNAM( 102) / '1-035314-113-282-1A' /
        DATA IBEG( 102) /      468 /
        DATA INTS( 468) /        2 /
        DATA INTS( 469) /   400095 /
        DATA INTS( 470) /   806824 /
        DATA INTS( 471) /        1 /
        DATA INTS( 472) /  -396877 /
        DATA INTS( 473) /  -811199 /
        DATA ISPX( 102) /   395417 /
        DATA ISPY( 102) /   812500 /
        DATA INTS( 474) /        0 /
        DATA PNAM( 103) / '1-038864-149-030-1A' /
        DATA IBEG( 103) /      475 /
        DATA INTS( 475) /       -3 /
        DATA INTS( 476) /        2 /
        DATA INTS( 477) /   250173 /
        DATA INTS( 478) /   976929 /
        DATA ISPX( 103) /   212472 /
        DATA ISPY( 103) /   970806 /
        DATA INTS( 479) /        0 /
        DATA PNAM( 104) / '1-059965-152-292-1A' /
        DATA IBEG( 104) /      480 /
        DATA INTS( 480) /       -5 /
        DATA INTS( 481) /        4 /
        DATA INTS( 482) /   709789 /
        DATA INTS( 483) /   121795 /
        DATA ISPX( 104) /   710167 /
        DATA ISPY( 104) /   120750 /
        DATA INTS( 484) /        0 /
        DATA PNAM( 105) / '1-060873-120-270-1A' /
        DATA IBEG( 105) /      485 /
        DATA INTS( 485) /        2 /
        DATA INTS( 486) /   207628 /
        DATA INTS( 487) /    45156 /
        DATA ISPX( 105) /   178750 /
        DATA ISPY( 105) /    44972 /
        DATA INTS( 488) /        0 /
        DATA PNAM( 106) / '1-062090-127-284-1A' /
        DATA IBEG( 106) /      489 /
        DATA INTS( 489) /       -5 /
        DATA INTS( 490) /        4 /
        DATA INTS( 491) /   658745 /
        DATA INTS( 492) /   922170 /
        DATA ISPX( 106) /   657889 /
        DATA ISPY( 106) /   919972 /
        DATA INTS( 493) /        0 /
        DATA PNAM( 107) / '1-062286-057-151-1A' /
        DATA IBEG( 107) /      494 /
        DATA INTS( 494) /       -4 /
        DATA INTS( 495) /        3 /
        DATA INTS( 496) /   741165 /
        DATA INTS( 497) /   115439 /
        DATA ISPX( 107) /   722056 /
        DATA ISPY( 107) /   119944 /
        DATA INTS( 498) /        0 /
        DATA PNAM( 108) / '2-000003-071-122-1A' /
        DATA IBEG( 108) /      499 /
        DATA INTS( 499) /      -12 /
        DATA INTS( 500) /       -9 /
        DATA ISPX( 108) /   369556 /
        DATA ISPY( 108) /   882500 /
        DATA INTS( 501) /        0 /
        DATA PNAM( 109) / '2-004265-157-092-0A' /
        DATA IBEG( 109) /      502 /
        DATA INTS( 502) /      -12 /
        DATA INTS( 503) /       -6 /
        DATA ISPX( 109) /    64722 /
        DATA ISPY( 109) /    35000 /
        DATA INTS( 504) /        0 /
        DATA PNAM( 110) / '2-005341-146-281-1A' /
        DATA IBEG( 110) /      505 /
        DATA INTS( 505) /        5 /
        DATA INTS( 506) /    98004 /
        DATA INTS( 507) /    66464 /
        DATA INTS( 508) /       -2 /
        DATA ISPX( 110) /   173833 /
        DATA ISPY( 110) /    85111 /
        DATA INTS( 509) /        0 /
        DATA PNAM( 111) / '2-007331-111-088-1A' /
        DATA IBEG( 111) /      510 /
        DATA INTS( 510) /       -3 /
        DATA INTS( 511) /        2 /
        DATA INTS( 512) /   586171 /
        DATA INTS( 513) /   744601 /
        DATA ISPX( 111) /   573551 /
        DATA ISPY( 111) /   763053 /
        DATA INTS( 514) /        0 /
        DATA PNAM( 112) / '2-010256-062-153-1A' /
        DATA IBEG( 112) /      515 /
        DATA INTS( 515) /       -2 /
        DATA ISPX( 112) /   376278 /
        DATA ISPY( 112) /   245806 /
        DATA INTS( 516) /        0 /
        DATA PNAM( 113) / '2-012022-083-138-1A' /
        DATA IBEG( 113) /      517 /
        DATA INTS( 517) /       -2 /
        DATA ISPX( 113) /   583778 /
        DATA ISPY( 113) /   234139 /
        DATA INTS( 518) /        0 /
        DATA PNAM( 114) / '2-013880-108-272-1A' /
        DATA IBEG( 114) /      519 /
        DATA INTS( 519) /       -4 /
        DATA ISPX( 114) /    72583 /
        DATA ISPY( 114) /    41556 /
        DATA INTS( 520) /        0 /
        DATA PNAM( 115) / '2-015016-090-103-1A' /
        DATA IBEG( 115) /      521 /
        DATA INTS( 521) /       -5 /
        DATA INTS( 522) /        2 /
        DATA INTS( 523) /   915601 /
        DATA INTS( 524) /   785276 /
        DATA ISPX( 115) /   889556 /
        DATA ISPY( 115) /   791694 /
        DATA INTS( 525) /        0 /
        DATA PNAM( 116) / '2-017367-121-278-1A' /
        DATA IBEG( 116) /      526 /
        DATA INTS( 526) /       -4 /
        DATA ISPX( 116) /   523779 /
        DATA ISPY( 116) /    25817 /
        DATA INTS( 527) /        0 /
        DATA PNAM( 117) / '2-017402-087-279-1A' /
        DATA IBEG( 117) /      528 /
        DATA INTS( 528) /       -5 /
        DATA INTS( 529) /        2 /
        DATA INTS( 530) /   924091 /
        DATA INTS( 531) /    81039 /
        DATA ISPX( 117) /   894556 /
        DATA ISPY( 117) /   107500 /
        DATA INTS( 532) /        0 /
        DATA PNAM( 118) / '2-020526-100-122-0A' /
        DATA IBEG( 118) /      533 /
        DATA INTS( 533) /       -5 /
        DATA INTS( 534) /        4 /
        DATA INTS( 535) /   901596 /
        DATA INTS( 536) /   270278 /
        DATA ISPX( 118) /   901667 /
        DATA ISPY( 118) /   271667 /
        DATA INTS( 537) /        0 /
        DATA PNAM( 119) / '3-000005-018-347-1A' /
        DATA IBEG( 119) /      538 /
        DATA INTS( 538) /       -8 /
        DATA ISPX( 119) /   788167 /
        DATA ISPY( 119) /   641028 /
        DATA INTS( 539) /        0 /
        DATA PNAM( 120) / '3-000014-148-082-0A' /
        DATA IBEG( 120) /      540 /
        DATA INTS( 540) /       -7 /
        DATA INTS( 541) /       -6 /
        DATA INTS( 542) /       -5 /
        DATA INTS( 543) /       -4 /
        DATA INTS( 544) /       -3 /
        DATA INTS( 545) /       -2 /
        DATA INTS( 546) /        1 /
        DATA INTS( 547) / -1000001 /
        DATA INTS( 548) /  -160012 /
        DATA INTS( 549) /        2 /
        DATA INTS( 550) /  -883057 /
        DATA INTS( 551) /  -202779 /
        DATA INTS( 552) /        3 /
        DATA INTS( 553) /  -946390 /
        DATA INTS( 554) /  -292779 /
        DATA INTS( 555) /        4 /
        DATA INTS( 556) /  -420834 /
        DATA INTS( 557) /  -335279 /
        DATA INTS( 558) /        5 /
        DATA INTS( 559) /  -368567 /
        DATA INTS( 560) /  -404291 /
        DATA INTS( 561) /        6 /
        DATA INTS( 562) /  -114168 /
        DATA INTS( 563) /  -567779 /
        DATA INTS( 564) /        7 /
        DATA INTS( 565) /       -1 /
        DATA INTS( 566) /  -575591 /
        DATA ISPX( 120) /   955833 /
        DATA ISPY( 120) /   169444 /
        DATA INTS( 567) /        0 /
        DATA PNAM( 121) / '3-000014-148-082-0B' /
        DATA IBEG( 121) /      568 /
        DATA INTS( 568) /       -6 /
        DATA INTS( 569) /       -5 /
        DATA INTS( 570) /       -4 /
        DATA INTS( 571) /       -3 /
        DATA INTS( 572) /       -2 /
        DATA INTS( 573) /       -1 /
        DATA ISPX( 121) /   883056 /
        DATA ISPY( 121) /   202778 /
        DATA INTS( 574) /        0 /
        DATA PNAM( 122) / '3-000015-065-047-1A' /
        DATA IBEG( 122) /      575 /
        DATA INTS( 575) /       -6 /
        DATA INTS( 576) /        5 /
        DATA INTS( 577) /    79195 /
        DATA INTS( 578) /    19750 /
        DATA ISPX( 122) /    89611 /
        DATA ISPY( 122) /    28278 /
        DATA INTS( 579) /        0 /
        DATA PNAM( 123) / '3-000018-162-249-1A' /
        DATA IBEG( 123) /      580 /
        DATA INTS( 580) /       -8 /
        DATA INTS( 581) /        7 /
        DATA INTS( 582) /   484951 /
        DATA INTS( 583) /   510674 /
        DATA ISPX( 123) /   969083 /
        DATA ISPY( 123) /   429333 /
        DATA INTS( 584) /        0 /
        DATA PNAM( 124) / '3-000030-098-358-0A' /
        DATA IBEG( 124) /      585 /
        DATA INTS( 585) /       15 /
        DATA INTS( 586) /   760429 /
        DATA INTS( 587) /   688668 /
        DATA INTS( 588) /      -12 /
        DATA ISPX( 124) /   853333 /
        DATA ISPY( 124) /   924722 /
        DATA INTS( 589) /        0 /
        DATA PNAM( 125) / '3-000032-152-028-0A' /
        DATA IBEG( 125) /      590 /
        DATA INTS( 590) /       -5 /
        DATA INTS( 591) /        4 /
        DATA INTS( 592) /   470522 /
        DATA INTS( 593) /    22463 /
        DATA ISPX( 125) /   310000 /
        DATA ISPY( 125) /    70560 /
        DATA INTS( 594) /        0 /
        DATA PNAM( 126) / '3-000045-102-105-0A' /
        DATA IBEG( 126) /      595 /
        DATA INTS( 595) /        4 /
        DATA INTS( 596) /   570137 /
        DATA INTS( 597) /   266800 /
        DATA ISPX( 126) /   986389 /
        DATA ISPY( 126) /   244722 /
        DATA INTS( 598) /        0 /
        DATA PNAM( 127) / '3-000055-117-068-0A' /
        DATA IBEG( 127) /      599 /
        DATA INTS( 599) /       30 /
        DATA INTS( 600) /   201621 /
        DATA INTS( 601) /   304865 /
        DATA ISPX( 127) /   107500 /
        DATA ISPY( 127) /    96111 /
        DATA INTS( 602) /        0 /
        DATA PNAM( 128) / '3-000076-111-031-0A' /
        DATA IBEG( 128) /      603 /
        DATA INTS( 603) /       -5 /
        DATA INTS( 604) /        4 /
        DATA INTS( 605) /   173637 /
        DATA INTS( 606) /   654192 /
        DATA ISPX( 128) /   329720 /
        DATA ISPY( 128) /   810280 /
        DATA INTS( 607) /        0 /
        DATA PNAM( 129) / '3-000081-157-229-0A' /
        DATA IBEG( 129) /      608 /
        DATA INTS( 608) /        3 /
        DATA INTS( 609) /   714144 /
        DATA INTS( 610) /   206096 /
        DATA ISPX( 129) /   722222 /
        DATA ISPY( 129) /   221667 /
        DATA INTS( 611) /        0 /
        DATA PNAM( 130) / '3-000093-155-053-0A' /
        DATA IBEG( 130) /      612 /
        DATA INTS( 612) /        5 /
        DATA INTS( 613) /  -477239 /
        DATA INTS( 614) /  -377960 /
        DATA INTS( 615) /        5 /
        DATA INTS( 616) /   179609 /
        DATA INTS( 617) /   348466 /
        DATA ISPX( 130) /   352222 /
        DATA ISPY( 130) /   335556 /
        DATA INTS( 618) /        0 /
        DATA PNAM( 131) / '3-000252-142-283-0A' /
        DATA IBEG( 131) /      619 /
        DATA INTS( 619) /       -4 /
        DATA ISPX( 131) /   143056 /
        DATA ISPY( 131) /   971944 /
        DATA INTS( 620) /        0 /
        DATA PNAM( 132) / '3-000415-148-357-1A' /
        DATA IBEG( 132) /      621 /
        DATA INTS( 621) /      -10 /
        DATA INTS( 622) /        9 /
        DATA INTS( 623) /   208422 /
        DATA INTS( 624) /   938057 /
        DATA ISPX( 132) /    29139 /
        DATA ISPY( 132) /   960806 /
        DATA INTS( 625) /        0 /
        DATA PNAM( 133) / '3-000628-041-285-1A' /
        DATA IBEG( 133) /      626 /
        DATA INTS( 626) /      -10 /
        DATA INTS( 627) /       -9 /
        DATA INTS( 628) /       -8 /
        DATA INTS( 629) /       -7 /
        DATA INTS( 630) /       -5 /
        DATA INTS( 631) /       -4 /
        DATA INTS( 632) /       -3 /
        DATA ISPX( 133) /   162444 /
        DATA ISPY( 133) /   882889 /
        DATA INTS( 633) /        0 /
        DATA PNAM( 134) / '3-002163-138-261-0A' /
        DATA IBEG( 134) /      634 /
        DATA INTS( 634) /       -8 /
        DATA ISPX( 134) /    72778 /
        DATA ISPY( 134) /    77500 /
        DATA INTS( 635) /        0 /
        DATA PNAM( 135) / '4-000612-037-285-1A' /
        DATA IBEG( 135) /      636 /
        DATA INTS( 636) /       -4 /
        DATA ISPX( 135) /      722 /
        DATA ISPY( 135) /   953306 /
        DATA INTS( 637) /        0 /
        DATA PNAM( 136) / '4-000963-035-288-1A' /
        DATA IBEG( 136) /      638 /
        DATA INTS( 638) /       -4 /
        DATA ISPX( 136) /     3306 /
        DATA ISPY( 136) /   777917 /
        DATA INTS( 639) /        0 /
        DATA PNAM( 137) / '4-001025-165-281-1A' /
        DATA IBEG( 137) /      640 /
        DATA INTS( 640) /       -4 /
        DATA ISPX( 137) /     8111 /
        DATA ISPY( 137) /   884111 /
        DATA INTS( 641) /        0 /
C
        DATA ALPH / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
C
        IALP=1
C
C Form the name of the polygon to search for in the edit list.
C
  101   WRITE (TNAM,'(I1,"-",I6,"-",I3,"-",I3,"-",I1,A1)')
     +                       IRGL,IPID,ILAT+90,ILON,ICWP,ALPH(IALP:IALP)
C
        DO 102 I=1,19
          IF (TNAM(I:I).EQ.' ') TNAM(I:I)='0'
  102   CONTINUE
C
C Search the edit list, using a binary-halving technique.
C
        IPLO=1
        IPHI=NPOL+1
C
  103   IPIM=(IPLO+IPHI)/2
C
        IF      (PNAM(IPIM).LE.TNAM) THEN
          IPLO=IPIM
        ELSE IF (PNAM(IPIM).GT.TNAM) THEN
          IPHI=IPIM
        END IF
C
        IF (IPHI-IPLO.GT.1) GO TO 103
C
C If the desired polygon is not in the list, there are no edits to be
C done; quit.
C
        IF (PNAM(IPLO).NE.TNAM) RETURN
C
C Found it.  Pull out the pointer to the list of edits for the polygon.
C
        IOEC=IBEG(IPLO)
C
C If the current polygon is not the one that needed editing, quit.
C
        DO 104 I=1,NCRA
          IF (XCRA(I).NE.0..AND.XCRA(I).NE.1..AND.
     +        YCRA(I).NE.0..AND.YCRA(I).NE.1.) THEN
            IF (NINT(1000000.*XCRA(I)).NE.ISPX(IPLO).OR.
     +          NINT(1000000.*YCRA(I)).NE.ISPY(IPLO)) THEN
              IALP=IALP+1
              GO TO 101
            END IF
            GO TO 105
          END IF
  104   CONTINUE
C
        IALP=IALP+1
        GO TO 101
C
C Do the edits.
C
  105   IF (INTS(IOEC).LT.0) THEN
C
C A negative edit command means to delete a point.
C
          IPTD=MAX(1,MIN(NCRA,ABS(INTS(IOEC))))
C
C Note that we take pains to ensure that the first and last points
C of the polygon remain identical.
C
          IF (IPTD.EQ.1.OR.IPTD.EQ.NCRA) THEN
            XCRA(1)=XCRA(NCRA-1)
            YCRA(1)=YCRA(NCRA-1)
          ELSE
            DO 106 I=IPTD,NCRA-1
              XCRA(I)=XCRA(I+1)
              YCRA(I)=YCRA(I+1)
  106       CONTINUE
          END IF
C
C Reduce the number of points by one.
C
          NCRA=NCRA-1
C
C Jump back to get the next edit command.
C
          IOEC=IOEC+1
          GO TO 105
C
        ELSE IF (INTS(IOEC).GT.0) THEN
C
C A positive edit command means to replace a point (change its
C coordinates to specified values) or to insert a point after
C a specified point.  For the former, the coordinates given will
C be positive; for the latter, negative.
C
          IXVL=INTS(IOEC+1)
          IYVL=INTS(IOEC+2)
C
          IF (IXVL.GT.0) THEN
C
C Replace.  Note that we take pains to ensure that the first and last
C points of the polygon remain identical.
C
            IPTR=MAX(1,MIN(NCRA,INTS(IOEC)))
C
            IF (IPTR.EQ.1.OR.IPTR.EQ.NCRA) THEN
              XCRA(   1)=REAL(IXVL)/1000000.
              YCRA(   1)=REAL(IYVL)/1000000.
              XCRA(NCRA)=REAL(IXVL)/1000000.
              YCRA(NCRA)=REAL(IYVL)/1000000.
            ELSE
              XCRA(IPTR)=REAL(IXVL)/1000000.
              YCRA(IPTR)=REAL(IYVL)/1000000.
            END IF
C
C Jump back to get the next edit command.
C
            IOEC=IOEC+3
            GO TO 105
C
          ELSE
C
C Insert.
C
            IF (NCRA.LT.MCRA) THEN
C
              IPTI=MAX(1,MIN(NCRA,INTS(IOEC)))
C
              IF (IPTI.EQ.NCRA) IPTI=1
C
              DO 107 I=NCRA+1,IPTI+2,-1
                XCRA(I)=XCRA(I-1)
                YCRA(I)=YCRA(I-1)
  107         CONTINUE
C
C Increase the number of points by one.
C
              NCRA=NCRA+1
C
              XCRA(IPTI+1)=-REAL(IXVL+1)/1000000.
              YCRA(IPTI+1)=-REAL(IYVL+1)/1000000.
C
            END IF
C
C Jump back to get the next edit command.
C
            IOEC=IOEC+3
            GO TO 105
C
          END IF
C
        END IF
C
C Done.
C
        RETURN
C
      END
