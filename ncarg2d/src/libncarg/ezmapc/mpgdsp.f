C
C $Id: mpgdsp.f,v 1.2 2000-07-12 16:23:49 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
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
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      REAL FUNCTION MPGDSP (ALAT,ALON,BLAT,BLON)
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
        MPGDSP=2.E0*RTOD*ASIN(DIST/2.E0)
C
        RETURN
C
      END
