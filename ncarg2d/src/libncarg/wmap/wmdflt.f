C
C	$Id: wmdflt.f,v 1.9 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMDFLT()
C
C  Sets all attributes to their default values.  All values should
C  agree with those set in BLOCKDATA WMBLDAX.
C
      include 'wmcomn.h'
C
      SYMWID =  0.03250
      BEGDST =  0.01500
      ENDDST =  0.01500
      BETDST =  0.04500
      CRVLEN =  0.00000
      SLOPE1 =  0.00000
      SLOPE2 =  0.00000
      SLOPEL =  0.00000
      SLOPER =  0.00000
      RLINWD =  8.00000
      SLINWD =  0.00275
      WSIZEW =  0.01400
      WSIZEC =  0.01050
      WSIZET =  0.01650
      WSIZER =  0.00800
      WSIZES =  0.02000
      ARWSIZ =  0.03500
      ARWLEN =  1.00000
      ARWDIR =  0.00000
      CDOTSZ =  0.00600
      CTYMRG =  0.00200
      TMPMRG =  0.00100
      WBSHFT =  0.03500
      WBFTIC =  0.33000
      WBDIST =  0.10000
      WBCLMR =  0.25000
      WBBANG = 62.00000
      WBXL   =  0.00000
      WBXR   =  0.00000
      WBYB   =  0.00000
      WBYT   =  0.00000
      WBBASE =  0.30000
      WBLSIZ =  0.17000
      DLINWD =  2.00000
C
      IWBBAS =   0
      IWBDIR =   0
      IBGCTY =   0
      IFGTRG =   1
      IDOTCO =   1
      IDOTBG =   0
      IFRONT =   2
      IARNDX =   1
      ISLFLG =   3
      IALOFT =   0
      IWDTYP =   0
      ICOLOR =   1
      NPTSBZ =  51
      MAXSYM = 200
      ISUNC1 =   2
      ISUNC2 =   3
      ISUNC3 =   1
      ISUNC4 =   1
      ICLDC1 =   2
      ICLDC2 =   1
      ICLDC3 =   1
      ILTNC1 =   2
      ILTNC2 =   1
      ILTNC3 =   1
      NUMSYO =  -1
      IWARMC =   1
      ICOLDC =   1
      ITRO1C =   1
      ITRO2C =   1
      IHIGC1 =   1
      IHIGC2 =   0
      IHIGC3 =   1
      IHIGC4 =   1
      ILOWC1 =   0
      ILOWC2 =   1
      ILOWC3 =   0
      IRGLC1 =   1
      IRGLC2 =   0
      IRGLC3 =   1
      IRGLC4 =   1
      IRGLC5 =  -1
      IAROWC =   1
      IARSHC =  -1
      IAROUC =  -1
      IRLLSC =  -1
      IRLOUC =  -1
      IRLBKC =  -1
C
      DO 10 I=1,ISDIM
        ISTYPE(I) = 2
   10 CONTINUE
C
      END
