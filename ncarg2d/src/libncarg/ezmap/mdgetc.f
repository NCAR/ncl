C
C $Id: mdgetc.f,v 1.10 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDGETC (WHCH,CVAL)
C
        CHARACTER*(*) WHCH
        CHARACTER*(*) CVAL
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM3/  ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,
     +                   SLOG,PNTS(200),IDOS(4)
        INTEGER          ITPN,NOUT,NPTS,IGID,IDLS,IDRS,IDOS
        REAL             BLAG,SLAG,BLOG,SLOG,PNTS
        SAVE   /MAPCM3/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCM5/  DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(19),
     +                   PDCL(19)
        CHARACTER*2      DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
        SAVE   /MAPCM5/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
        IF (ICFELL('MDGETC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
        IF (     WHCH(1:2).EQ.'AR') THEN
          CVAL=LDCT(ILTS)
        ELSE IF (WHCH(1:2).EQ.'OU') THEN
          CVAL=DDCT(NOUT+1)
        ELSE IF (WHCH(1:2).EQ.'PR') THEN
          CVAL=PDCT(JPRJ+1)
          IF (JPRJ.EQ.3.AND.ABS(SALT).GT.1.D0) CVAL=PDCT(17)
        ELSE IF (WHCH(1:2).EQ.'ar') THEN
          CVAL=LDCL(ILTS)
        ELSE IF (WHCH(1:2).EQ.'ou') THEN
          CVAL=DDCL(NOUT+1)
        ELSE IF (WHCH(1:2).EQ.'pr') THEN
          CVAL=PDCL(JPRJ+1)
          IF (JPRJ.EQ.3.AND.ABS(SALT).GT.1.D0) CVAL=PDCL(17)
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
  901   CALL MDPCEM ('MDGETC - UNKNOWN PARAMETER NAME ',WHCH,2,1)
        CVAL=' '
        RETURN
C
      END
