C
C $Id: mdgddp.f,v 1.2 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION MDGDDP (ALAT,ALON,BLAT,BLON)
C
        DOUBLE PRECISION ALAT,ALON,CALT,SALT,CALN,SALN
        DOUBLE PRECISION BLAT,BLON,CBLT,SBLT,CBLN,SBLN
C
        DOUBLE PRECISION XCOA,YCOA,ZCOA
        DOUBLE PRECISION XCOB,YCOB,ZCOB
C
        DOUBLE PRECISION DIST
C
        DOUBLE PRECISION DTOR,RTOD
C
        DATA DTOR / .017453292519943D0 /
        DATA RTOD / 57.2957795130823D0 /
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
        MDGDDP=2.D0*RTOD*ASIN(DIST/2.D0)
C
        RETURN
C
      END
