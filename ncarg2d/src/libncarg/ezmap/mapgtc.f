C
C $Id: mapgtc.f,v 1.13 2000-08-22 15:03:32 haley Exp $
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
      SUBROUTINE MAPGTC (WHCH,CVAL)
C
      CHARACTER*(*) WHCH,CVAL
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,SLOG,
     +                PNTS(200),IDOS(4)
      SAVE   /MAPCM3/
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW,GRLA,
     +                GRLO,GRPO
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE   /MAPCM4/
C
      COMMON /MAPCM5/ DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(12),PDCL(12)
      CHARACTER*2     DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
      SAVE   /MAPCM5/
C
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE   /MAPSAT/
C
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE   /MAPDPS/
C
      IF (ICFELL('MAPGTC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
      IF (     WHCH(1:2).EQ.'AR') THEN
        CVAL=LDCT(ILTS)
      ELSE IF (WHCH(1:2).EQ.'OU') THEN
        CVAL=DDCT(NOUT+1)
      ELSE IF (WHCH(1:2).EQ.'PR') THEN
        CVAL=PDCT(JPRJ+1)
        IF (JPRJ.EQ.3.AND.ABS(SALT).GT.1.) CVAL=PDCT(12)
      ELSE IF (WHCH(1:2).EQ.'ar') THEN
        CVAL=LDCL(ILTS)
      ELSE IF (WHCH(1:2).EQ.'ou') THEN
        CVAL=DDCL(NOUT+1)
      ELSE IF (WHCH(1:2).EQ.'pr') THEN
        CVAL=PDCL(JPRJ+1)
        IF (JPRJ.EQ.3.AND.ABS(SALT).GT.1.) CVAL=PDCL(12)
      ELSE
        GO TO 901
      END IF
C
C Done.
C
      RETURN
C
C Error exits.
C
  901 CALL MAPCEM ('MAPGTC - UNKNOWN PARAMETER NAME ',WHCH,2,1)
      CVAL=' '
      RETURN
C
      END
