C
C $Id: mdgdsp.f,v 1.2 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      REAL FUNCTION MDGDSP (ALAT,ALON,BLAT,BLON)
C
        REAL ALAT,ALON,CALT,SALT,CALN,SALN
        REAL BLAT,BLON,CBLT,SBLT,CBLN,SBLN
C
        REAL XCOA,YCOA,ZCOA
        REAL XCOB,YCOB,ZCOB
C
        REAL DIST
C
        REAL DTOR,RTOD
C
        DATA DTOR / .017453292519943E0 /
        DATA RTOD / 57.2957795130823E0 /
C
        CALT=COS(DTOR*ALAT)
        SALT=SIN(DTOR*ALAT)
        CALN=COS(DTOR*ALON)
        SALN=SIN(DTOR*ALON)
C
        CBLT=COS(DTOR*BLAT)
        SBLT=SIN(DTOR*BLAT)
        CBLN=COS(DTOR*BLON)
        SBLN=SIN(DTOR*BLON)
C
        XCOA=CALT*CALN
        YCOA=CALT*SALN
        ZCOA=SALT
C
        XCOB=CBLT*CBLN
        YCOB=CBLT*SBLN
        ZCOB=SBLT
C
        DIST=SQRT((XCOA-XCOB)**2+(YCOA-YCOB)**2+(ZCOA-ZCOB)**2)
C
        MDGDSP=2.E0*RTOD*ASIN(DIST/2.E0)
C
        RETURN
C
      END
