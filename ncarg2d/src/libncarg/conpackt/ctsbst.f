C
C $Id: ctsbst.f,v 1.1 2003-05-28 15:44:33 kennison Exp $
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
      SUBROUTINE CTSBST (CHSI,CHSO,NCHO)
C
      CHARACTER*(*) CHSI,CHSO
C
C The routine CTSBST is called to perform substitution of numeric values
C for parameter names.  The contents of the string CHSI are copied to
C the string CHSO.  Certain substrings of the form '$xxx$' are replaced
C by strings representing numeric values; in particular, '$DVA$' is
C replaced by a string representing the numeric value of DVAL.  The
C length of the resulting string is returned as the value of NCHO.
C
C
C Declare all of the CONPACKT common blocks.
C
C
C CTCOM1 contains integer and real variables.
C
      COMMON /CTCOM1/ ANCF,ANHL,ANIL,ANLL,CDMX,CHWM,CINS,CINT(10)
      COMMON /CTCOM1/ CINU,CLDB(256),CLDL(256),CLDR(256)
      COMMON /CTCOM1/ CLDT(256),CLEV(256),CLWA(258),CXCF
      COMMON /CTCOM1/ CXIL,CYCF,CYIL,DBLF,DBLM,DBLN,DBLV,DFLD,DMAX
      COMMON /CTCOM1/ DMIN,DOPT,DVAL,EPSI,FNCM,GRAV,GRSD,GSDM,HCHL
      COMMON /CTCOM1/ HCHS,HLSR,IAIA(258),IAIB(256),IBCF,IBHL
      COMMON /CTCOM1/ IBIL,IBLL,ICAF,ICCF,ICCL(258),ICFF,ICHI
      COMMON /CTCOM1/ ICHL,ICIL,ICLL(256),ICLO,ICLP(256),ICLS
      COMMON /CTCOM1/ ICLU(258),ICLV,ICLW,IDUF,IGCL,IGLB,IGRM
      COMMON /CTCOM1/ IGRN,IGVS,IHCF,IHLE,IIWS(2),IIWU,ILBC
      COMMON /CTCOM1/ IMPF,INCX(8),INCY(8),INHL,INIL,INIT,INLL
      COMMON /CTCOM1/ IOCF,IOHL,IOLL,IPAI,IPCF,IPIC,IPIE,IPIL,IPLL
      COMMON /CTCOM1/ IRWS(4),IRWU,ISET,IWSO,JODP,JOMA,JOTZ
      COMMON /CTCOM1/ LCTM,LEA1,LEA2,LEA3,LEE1,LEE2,LEE3,LINS
      COMMON /CTCOM1/ LINT(10),LINU,LIWK,LIWM,LIWS(2),LNLG,LOEN
      COMMON /CTCOM1/ LOPN,LOTN,LRWC,LRWG,LRWK,LRWM,LRWS(4)
      COMMON /CTCOM1/ LSDD,LSDL,LSDM,LTCF,LTHI,LTIL,LTLO,MIRO
      COMMON /CTCOM1/ NCLB(256),NCLV,NDGL,NEDG,NEXL,NEXT,NEXU
      COMMON /CTCOM1/ NLBS,NLSD,NLZF,NOMF,NOVS,NPNT,NR04,NSDL
      COMMON /CTCOM1/ NSDR,NTRI,OORV,PITH,SCFS,SCFU,SEGL,T2DS
      COMMON /CTCOM1/ UCMN,UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL
      COMMON /CTCOM1/ UWDR,UWDT,WCCF,WCHL,WCIL,WCLL,WLCF,WLHL,WLIL
      COMMON /CTCOM1/ WLLL,WOCH,WODA,WTCD,WTGR,WTNC,WTOD,WWCF,WWHL
      COMMON /CTCOM1/ WWIL,WWLL,XLBC,XMAX,XMIN,XVPL,XVPR,XWDL,XWDR
      COMMON /CTCOM1/ YLBC,YMAX,YMIN,YVPB,YVPT,YWDB,YWDT,ZMAX,ZMIN
C
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      EQUIVALENCE (IRWS(3),IR03),(LRWS(3),LR03)
      EQUIVALENCE (IRWS(4),IR04),(LRWS(4),LR04)
      SAVE   /CTCOM1/
C
C CTCOM2 holds character parameters.
C
      COMMON /CTCOM2/ CHEX,CLBL(256),CLDP(258),CTMA,CTMB,FRMT
      COMMON /CTCOM2/ TXCF,TXHI,TXIL,TXLO
      CHARACTER*13 CHEX
      CHARACTER*64 CLBL
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*64 TXCF
      CHARACTER*32 TXHI
      CHARACTER*128 TXIL
      CHARACTER*32 TXLO
      SAVE   /CTCOM2/
C
C Find the length of the input character string.
C
      NCHI=LEN(CHSI)
C
C Find the length of the output character-string variable, blank-fill
C it, and initialize the count of characters put into it.
C
      MCHO=LEN(CHSO)
      CHSO=' '
      NCHO=0
C
C Do the copy.  Each time a dollar sign is encountered, see if it
C introduces one of the parameter names to be replaced and, if so,
C do the replacement.
C
      KCHI=0
10001 CONTINUE
      IF (.NOT.(KCHI.LT.NCHI)) GO TO 10002
        KCHI=KCHI+1
        IF (.NOT.(NCHO.LT.MCHO)) GO TO 10003
          NCHO=NCHO+1
          CHSO(NCHO:NCHO)=CHSI(KCHI:KCHI)
          IF (.NOT.(CHSI(KCHI:KCHI).EQ.'$'.AND.KCHI+4.LE.NCHI)) GO TO 10
     +004
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'DVA')) GO TO 10005
              VALU=DVAL
              L10007=    1
              GO TO 10007
10006         CONTINUE
            GO TO 10008
10005       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'DMN')) GO TO 10009
              VALU=DMIN
              L10007=    2
              GO TO 10007
10010         CONTINUE
            GO TO 10008
10009       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'DMX')) GO TO 10011
              VALU=DMAX
              L10007=    3
              GO TO 10007
10012         CONTINUE
            GO TO 10008
10011       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'CIU')) GO TO 10013
              VALU=CINU
              L10015=    1
              GO TO 10015
10014         CONTINUE
            GO TO 10008
10013       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'CMN')) GO TO 10016
              IF (.NOT.(NCLV.LE.0)) GO TO 10017
                VALU=0.
              GO TO 10018
10017         CONTINUE
                VALU=CLEV(ICLP(1))
10018         CONTINUE
              L10015=    2
              GO TO 10015
10019         CONTINUE
            GO TO 10008
10016       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'CMX')) GO TO 10020
              IF (.NOT.(NCLV.LE.0)) GO TO 10021
                VALU=0.
              GO TO 10022
10021         CONTINUE
                VALU=CLEV(ICLP(NCLV))
10022         CONTINUE
              L10015=    3
              GO TO 10015
10023         CONTINUE
            GO TO 10008
10020       CONTINUE
            IF (.NOT.(CHSI(KCHI+1:KCHI+3).EQ.'SFU')) GO TO 10024
              VALU=SCFU
              LMSD=-10000
              IEXP=1
              LEXP=0
              IOMA=1
              IODP=1
              IOTZ=1
              L10026=    1
              GO TO 10026
10025         CONTINUE
10008       CONTINUE
10024       CONTINUE
10004     CONTINUE
10003   CONTINUE
      GO TO 10001
10002 CONTINUE
C
C Done.
C
      RETURN
C
C The following internal procedure determines whether to treat $CIU$,
C $CMN$, and $CMX$ as unrounded or rounded numbers.
C
10015 CONTINUE
        IF (.NOT.(ICLS.LT.0)) GO TO 10027
          L10007=    4
          GO TO 10007
10028     CONTINUE
        GO TO 10029
10027   CONTINUE
          L10031=    1
          GO TO 10031
10030     CONTINUE
10029   CONTINUE
      GO TO (10014,10019,10023) , L10015
C
C The following internal procedure is used to handle numbers known not
C to have been rounded to nice values.
C
10007 CONTINUE
        IF (CHSI(KCHI+4:KCHI+4).NE.'U') VALU=VALU/SCFU
        LMSD=LSDL
        IEXP=NEXU
        LEXP=NEXL
        IOMA=JOMA
        IODP=JODP
        IOTZ=JOTZ
        L10026=    2
        GO TO 10026
10032   CONTINUE
      GO TO (10006,10010,10012,10028) , L10007
C
C The following internal procedure is used to handle numbers which are
C likely to have been rounded to nice values, so that it is probably a
C good idea to trim off trailing zeroes.
C
10031 CONTINUE
        IF (CHSI(KCHI+4:KCHI+4).NE.'U') VALU=VALU/SCFU
        LMSD=LSDL
        IEXP=NEXU
        LEXP=NEXL
        IOMA=JOMA
        IODP=JODP
        IOTZ=1
        L10026=    3
        GO TO 10026
10033   CONTINUE
      GO TO (10030) , L10031
C
C The following internal procedure generates, in the output string, the
C representation of a numeric value.  It then updates the pointers into
C the input and output character strings.
C
10026 CONTINUE
        CALL CTNUMB (VALU,NDGL,LMSD,IEXP,LEXP,CHEX(1:LEA1),
     +               CHEX(LEA1+1:LEA1+LEA2),
     +               CHEX(LEA1+LEA2+1:LEA1+LEA2+LEA3),
     +               LEE1,LEE2,LEE3,IOMA,IODP,IOTZ,
     +               CHSO(NCHO:MCHO),NCHS,NDGS,IEVA)
        NCHO=NCHO+NCHS-1
        KCHI=KCHI+4
        IF (CHSI(KCHI:KCHI).NE.'$') KCHI=KCHI+1
      GO TO (10025,10032,10033) , L10026
C
      END
