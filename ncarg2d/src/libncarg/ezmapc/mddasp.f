C
C $Id: mddasp.f,v 1.2 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      REAL FUNCTION MDDASP (GEOG)
C
        REAL GEOG(2)
C
        REAL DTOR,RTOD
C
        REAL ALAT,ALON,CALT,SALT,CALN,SALN
        REAL BLAT,BLON,CBLT,SBLT,CBLN,SBLN
C
        REAL XCOA,YCOA,ZCOA
        REAL XCOB,YCOB,ZCOB
C
        REAL DIST
C
        DATA DTOR /  .017453292519943E0 /
        DATA RTOD /  57.2957795130823E0 /
C
        DATA ALAT /  1.11701072128000E0 /
        DATA ALON / -2.65290046304000E0 /
C
        BLAT=GEOG(2)
        BLON=GEOG(1)

        CALT=COS(ALAT)
        SALT=SIN(ALAT)
        CALN=COS(ALON)
        SALN=SIN(ALON)
C
        CBLT=COS(BLAT)
        SBLT=SIN(BLAT)
        CBLN=COS(BLON)
        SBLN=SIN(BLON)
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
        MDDASP=2.E0*RTOD*ASIN(DIST/2.E0)
C
        RETURN
C
      END
