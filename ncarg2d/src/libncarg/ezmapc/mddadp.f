C
C $Id: mddadp.f,v 1.1 2001-08-16 23:12:44 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      DOUBLE PRECISION FUNCTION MDDADP (GEOG)
C
        DOUBLE PRECISION GEOG(2)
C
        DOUBLE PRECISION DTOR,RTOD
C
        DOUBLE PRECISION ALAT,ALON,CALT,SALT,CALN,SALN
        DOUBLE PRECISION BLAT,BLON,CBLT,SBLT,CBLN,SBLN
C
        DOUBLE PRECISION XCOA,YCOA,ZCOA
        DOUBLE PRECISION XCOB,YCOB,ZCOB
C
        DOUBLE PRECISION DIST
C
        DATA DTOR /  .017453292519943D0 /
        DATA RTOD /  57.2957795130823D0 /
C
        DATA ALAT /  1.11701072128000D0 /
        DATA ALON / -2.65290046304000D0 /
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
        MDDADP=2.D0*RTOD*ASIN(DIST/2.D0)
C
        RETURN
C
      END
