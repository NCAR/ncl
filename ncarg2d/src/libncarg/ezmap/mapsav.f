C
C $Id: mapsav.f,v 1.9 2000-07-12 16:23:19 haley Exp $
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
      SUBROUTINE MAPSAV (IFNO)
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
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE   /MAPCMA/
C
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE   /MAPCMC/
C
      COMMON /MAPCMQ/ ICIN(8)
      SAVE   /MAPCMQ/
C
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE   /MAPSAT/
C
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE   /MAPDPS/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPSAV - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Write a record containing all the user-settable parameters.
C
      WRITE (IFNO,ERR=901)        NOUT,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,
     +                            PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,
     +                            PLTR,GRID,IDSH,IDOT,LBLF,PRMF,ELPF,
     +                            XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,
     +                            ILCW,DPLT,DDTS,IGI1,IGI2,NOVS,SALT,
     +                            SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,
     +                            RCSB,DSNA,DCSA,DSNB,DCSB,ICIN,GRLA,
     +                            GRLO,GRPO
C
C Done.
C
      RETURN
C
C Error exits.
C
  901 CALL SETER ('MAPSAV - ERROR ON WRITE',2,1)
      RETURN
C
      END
