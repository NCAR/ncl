C
C $Id: cphllb.f,v 1.12 2002-02-06 23:22:04 kennison Exp $
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
      SUBROUTINE CPHLLB (ZDAT,RWRK,IWRK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*)
C
C CPHLLB generates the high and low labels for the contour field; the
C quantities defining the labels are added to the lists in real
C workspaces 3 and 4.
C
C A point (I,J) is defined to be a high (low) if ZDAT(I,J) is greater
C than (less than) every other field value within a certain neighborhood
C of (I,J).  The neighborhood is defined by the values of the parameters
C 'HLX' and 'HLY'.
C
C
C Declare all of the CONPACK common blocks.
C
C
C CPCOM1 contains integer and real variables.
C
      COMMON /CPCOM1/ ANCF,ANHL,ANIL,ANLL,CDMX,CHWM,CINS,CINT(10)
      COMMON /CPCOM1/ CINU,CLDB(256),CLDL(256),CLDR(256)
      COMMON /CPCOM1/ CLDT(256),CLEV(256),CLWA(259),CXCF
      COMMON /CPCOM1/ CXIL,CYCF,CYIL,DBLF,DBLM,DBLN,DBLV,DFLD,DOPT
      COMMON /CPCOM1/ EPSI,FNCM,GRAV,GRSD,GSDM,HCHL,HCHS,IAIA(259)
      COMMON /CPCOM1/ IAIB(256),IBCF,IBHL,IBIL,IBLL,ICAF,ICCF
      COMMON /CPCOM1/ ICCL(259),ICFF,ICHI,ICHL,ICIL,ICLL(256)
      COMMON /CPCOM1/ ICLO,ICLP(256),ICLS,ICLU(259),ICLV,ICLW
      COMMON /CPCOM1/ IDUF,IGCL,IGLB,IGRM,IGRN,IGVS,IHCF,IHLE,IHLX
      COMMON /CPCOM1/ IHLY,IIWS(2),IIWU,ILBC,IMPF,INCX(8),INCY(8)
      COMMON /CPCOM1/ INHL,INIL,INIT,INLL,IOCF,IOHL,IOLL,IPAI,IPCF
      COMMON /CPCOM1/ IPIC,IPIE,IPIL,IPLL,IRWS(4),IRWU,ISET,IWSO
      COMMON /CPCOM1/ IZD1,IZDM,IZDN,IZDS,JODP,JOMA,JOTZ,LCTM,LEA1
      COMMON /CPCOM1/ LEA2,LEA3,LEE1,LEE2,LEE3,LINS,LINT(10),LINU
      COMMON /CPCOM1/ LIWK,LIWM,LIWS(2),LNLG,LRWC,LRWG,LRWK
      COMMON /CPCOM1/ LRWM,LRWS(4),LSDD,LSDL,LSDM,LTCF,LTHI
      COMMON /CPCOM1/ LTIL,LTLO,MIRO,NCLB(256),NCLV,NDGL,NEXL
      COMMON /CPCOM1/ NEXT,NEXU,NLBS,NLSD,NLZF,NOMF,NOVS,NR04,NSDL
      COMMON /CPCOM1/ NSDR,OORV,PITH,SCFS,SCFU,SEGL,SVAL,T2DS,T3DS
      COMMON /CPCOM1/ UCMN,UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL
      COMMON /CPCOM1/ UWDR,UWDT,UXA1,UXAM,UYA1,UYAN,WCCF,WCHL,WCIL
      COMMON /CPCOM1/ WCLL,WLCF,WLHL,WLIL,WLLL,WOCH,WODA,WTCD,WTGR
      COMMON /CPCOM1/ WTNC,WTOD,WWCF,WWHL,WWIL,WWLL,XAT1,XATM,XLBC
      COMMON /CPCOM1/ XVPL,XVPR,XWDL,XWDR,YAT1,YATN,YLBC,YVPB,YVPT
      COMMON /CPCOM1/ YWDB,YWDT,ZDVL,ZMAX,ZMIN
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      EQUIVALENCE (IRWS(3),IR03),(LRWS(3),LR03)
      EQUIVALENCE (IRWS(4),IR04),(LRWS(4),LR04)
      SAVE   /CPCOM1/
C
C CPCOM2 holds character parameters.
C
      COMMON /CPCOM2/ CHEX,CLBL(256),CLDP(259),CTMA,CTMB,FRMT
      COMMON /CPCOM2/ TXCF,TXHI,TXIL,TXLO
      CHARACTER*13 CHEX
      CHARACTER*64 CLBL
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*64 TXCF
      CHARACTER*32 TXHI
      CHARACTER*128 TXIL
      CHARACTER*32 TXLO
      SAVE   /CPCOM2/
C
C ZVAL is an arithmetic statement function which, given an integer
C index I between 1 and IZDM*IZDN, has as its value the Ith element
C (in column-wise order) of the array ZDAT.
C
      ZVAL(I)=ZDAT(MOD(I,IZDM)+1,I/IZDM+1)
C
C If the text strings for high and low labels are blank, do nothing.
C
      IF (TXHI(1:LTHI).EQ.' '.AND.TXLO(1:LTLO).EQ.' ') RETURN
C
C Zero the value of a flag that is incremented whenever a problematic
C pair of near-by equal values is seen in the field.
C
      IFLG=0
C
C Compute constants needed to get from data-index coordinates to user
C coordinates.
C
      RZDM=(XATM-XAT1)/REAL(IZDM-1)
      RZDN=(YATN-YAT1)/REAL(IZDN-1)
C
C Compute the value of the angle at which the labels are written, in
C radians, and the sine and cosine of that angle.
C
      ANLB=.017453292519943*ANHL
      SALB=SIN(ANLB)
      CALB=COS(ANLB)
C
C Compute the width of a character in the fractional system and the
C width of the white space in the fractional system.
C
      WCFS=CHWM*WCHL*(XVPR-XVPL)
      WWFS=CHWM*WWHL*(XVPR-XVPL)
C
C Determine the values of IRNG and JRNG, which specify the size of the
C neighborhood used in testing for highs and lows.
C
      IF (IHLX.GT.0) THEN
        IRNG=IHLX
      ELSE
        IRNG=MAX(2,MIN(15,IZDM/8))
      END IF
C
      IF (IHLY.GT.0) THEN
        JRNG=IHLY
      ELSE
        JRNG=MAX(2,MIN(15,IZDN/8))
      END IF
C
C Make PLOTCHAR compute text-extent quantities.
C
      CALL PCGETI ('TE',ITMP)
      IF (ICFELL('CPHLLB',1).NE.0) RETURN
      CALL PCSETI ('TE',1)
      IF (ICFELL('CPHLLB',2).NE.0) RETURN
C
C Line loop follows.  The complete two-dimensional test for a minimum or
C maximum of the field is only performed for points which are minima or
C maxima along some line.  Finding these candidates is made efficient by
C using a count of consecutive increases or decreases of the function
C along the line.
C
C Tell IFTRAN to use the FORTRAN-66 implementation of BLOCK invocations.
C
C
        JPNT = 2
        GO TO 10003
10001   CONTINUE
        JPNT =JPNT +1
10003   CONTINUE
        IF (JPNT .GT.(IZDN-1)) GO TO 10002
C
        ICON=IRNG-1
        IPNT=-1
        GO TO 106
C
C Loop as long as the function is increasing along the line; we seek a
C possible maximum.
C
  101   CONTINUE
10004   CONTINUE
          IPNT=IPNT+1
          IF (IPNT.GE.IZDM) GO TO 107
          ZNOW=ZNXT
          ZNXT=ZDAT(IPNT+1,JPNT)
          IF (SVAL.NE.0..AND.ZNXT.EQ.SVAL) GO TO 105
          IF (.NOT.(ZNXT.LT.ZNOW)) GO TO 10005
            GO TO 10006
10005     CONTINUE
          IF (.NOT.(ZNXT.EQ.ZNOW)) GO TO 10007
            IFLG=IFLG+1
            ICON=0
          GO TO 10008
10007     CONTINUE
            ICON=ICON+1
10008     CONTINUE
        GO TO 10004
10006   CONTINUE
C
C Function decreases at next point.  Test for maximum on line.
C
        IF (.NOT.(ICON.LT.IRNG)) GO TO 10009
C
          IBEG=MAX(1,IPNT-IRNG)
          IEND=IPNT-ICON-1
C
          DO 10010 I=IBEG,IEND
            IF (ZNOW.EQ.ZDAT(I,JPNT)) IFLG=IFLG+1
            IF (ZNOW.LE.ZDAT(I,JPNT)) GO TO 102
10010     CONTINUE
C
10009   CONTINUE
C
        IBEG=IPNT+2
        IEND=MIN(IZDM,IPNT+IRNG)
C
        DO 10011 I=IBEG,IEND
          IF (ZNOW.EQ.ZDAT(I,JPNT)) IFLG=IFLG+1
          IF (ZNOW.LE.ZDAT(I,JPNT)) GO TO 102
10011   CONTINUE
C
C We have a maximum on the line.  Do a two-dimensional test for a
C maximum in the field.
C
        JBEG=MAX(1,JPNT-JRNG)
        JEND=MIN(IZDN,JPNT+JRNG)
        IBEG=MAX(1,IPNT-IRNG)
        IEND=MIN(IZDM,IPNT+IRNG)
C
        DO 10012 J=JBEG,JEND
          IF (.NOT.(J.NE.JPNT)) GO TO 10013
            DO 10014 I=IBEG,IEND
              IF (.NOT.(SVAL.EQ.0..OR.ZDAT(I,J).NE.SVAL)) GO TO 10015
                IF (ZDAT(I,J).EQ.ZNOW) IFLG=IFLG+1
                IF (ZDAT(I,J).GE.ZNOW) GO TO 102
              GO TO 10016
10015         CONTINUE
                IF (ABS(I-IPNT).LT.2.AND.ABS(J-JPNT).LT.2) GO TO 102
10016         CONTINUE
10014       CONTINUE
10013     CONTINUE
10012   CONTINUE
C
C We have a maximum in the field.  Process it.
C
        IF (.NOT.(TXHI(1:LTHI).NE.' ')) GO TO 10017
          IHOL=0
          XLBC=XAT1+RZDM*(REAL(IPNT)-1.)
          YLBC=YAT1+RZDN*(REAL(JPNT)-1.)
          L10019=    1
          GO TO 10019
10018     CONTINUE
10017   CONTINUE
C
C Start searching for a minimum.
C
  102   ICON=1
C
C Loop as long as the function is decreasing along the line.  We seek a
C possible minimum.
C
  103   CONTINUE
10020   CONTINUE
          IPNT=IPNT+1
          IF (IPNT.GE.IZDM) GO TO 107
          ZNOW=ZNXT
          ZNXT=ZDAT(IPNT+1,JPNT)
          IF (SVAL.NE.0..AND.ZNXT.EQ.SVAL) GO TO 105
          IF (.NOT.(ZNXT.GT.ZNOW)) GO TO 10021
            GO TO 10022
10021     CONTINUE
          IF (.NOT.(ZNXT.EQ.ZNOW)) GO TO 10023
            IFLG=IFLG+1
            ICON=0
          GO TO 10024
10023     CONTINUE
            ICON=ICON+1
10024     CONTINUE
        GO TO 10020
10022   CONTINUE
C
C Function increases at next point.  Test for a minimum on the line.
C
        IF (.NOT.(ICON.LT.IRNG)) GO TO 10025
C
          IBEG=MAX(1,IPNT-IRNG)
          IEND=IPNT-ICON-1
C
          DO 10026 I=IBEG,IEND
            IF (ZNOW.EQ.ZDAT(I,JPNT)) IFLG=IFLG+1
            IF (ZNOW.GE.ZDAT(I,JPNT)) GO TO 104
10026     CONTINUE
C
10025   CONTINUE
C
        IBEG=IPNT+2
        IEND=MIN(IZDM,IPNT+IRNG)
C
        DO 10027 I=IBEG,IEND
          IF (ZNOW.EQ.ZDAT(I,JPNT)) IFLG=IFLG+1
          IF (ZNOW.GE.ZDAT(I,JPNT)) GO TO 104
10027   CONTINUE
C
C We have a minimum on the line.  Do a two-dimensional test for a
C minimum of the field.
C
        JBEG=MAX(1,JPNT-JRNG)
        JEND=MIN(IZDN,JPNT+JRNG)
        IBEG=MAX(1,IPNT-IRNG)
        IEND=MIN(IZDM,IPNT+IRNG)
C
        DO 10028 J=JBEG,JEND
          IF (.NOT.(J.NE.JPNT)) GO TO 10029
            DO 10030 I=IBEG,IEND
              IF (.NOT.(SVAL.EQ.0..OR.ZDAT(I,J).NE.SVAL)) GO TO 10031
                IF (ZDAT(I,J).EQ.ZNOW) IFLG=IFLG+1
                IF (ZDAT(I,J).LE.ZNOW) GO TO 104
              GO TO 10032
10031         CONTINUE
                IF (ABS(I-IPNT).LT.2.AND.ABS(J-JPNT).LT.2) GO TO 104
10032         CONTINUE
10030       CONTINUE
10029     CONTINUE
10028   CONTINUE
C
C We have a minimum in the field.  Process it.
C
        IF (.NOT.(TXLO(1:LTLO).NE.' ')) GO TO 10033
          IHOL=1
          XLBC=XAT1+RZDM*(REAL(IPNT)-1.)
          YLBC=YAT1+RZDN*(REAL(JPNT)-1.)
          L10019=    2
          GO TO 10019
10034     CONTINUE
10033   CONTINUE
C
C Start searching for a maximum.
C
  104   ICON=1
        GO TO 101
C
C Skip special values on the line.
C
  105   ICON=0
  106   IPNT=IPNT+1
        IF (IPNT.GE.IZDM-1) GO TO 107
        IF (SVAL.NE.0..AND.ZDAT(IPNT+1,JPNT).EQ.SVAL) GO TO 105
        ICON=ICON+1
        IF (ICON.LE.IRNG) GO TO 106
C
        ICON=1
        ZNXT=ZDAT(IPNT+1,JPNT)
        IF (ZDAT(IPNT,JPNT).EQ.ZNXT) ICON=0
        IF (ZDAT(IPNT,JPNT).LE.ZNXT) GO TO 101
        GO TO 103
C
  107 CONTINUE
      GO TO 10001
10002 CONTINUE
C
C If there is evidence that the straightforward search for highs and
C lows may have missed some because of equal field values at neighboring
C locations within the search neighborhood and if the user has enabled
C an additional search for the highs and lows that were missed, do it.
C
      IF (.NOT.(IFLG.NE.0.AND.IHLE.NE.0)) GO TO 10035
C
C Grab a portion of the integer workspace of length IZDM*IZDN.
C
        CALL CPGIWS (IWRK,1,IZDM*IZDN,IWSE)
        IF (IWSE.NE.0.OR.ICFELL('CPHLLB',3).NE.0) GO TO 110
C
C Form an index array for the contents of ZDAT.  Each element of the
C index array is of the form (J-1)*IZDM+(I-1), where I and J are indices
C of an element of ZDAT.  The elements of the index array are sorted
C in order of increasing value of the associated elements of ZDAT.
C
        CALL CPHLSO (ZDAT,IZD1,IZDM,IZDN,IWRK(II01+1))
C
C Initialize a scan of the index array to look for elements of ZDAT
C that are equal to each other.  ZNXT is the larger element of ZDAT.
C NEQU keeps track of the number of equal values found in a row.
C
        ZNXT=ZVAL(IWRK(II01+1))
        NEQU=0
C
C Loop through the elements of the index array.
C
          INDX = 1
          GO TO 10038
10036     CONTINUE
          INDX =INDX +1
10038     CONTINUE
          IF (INDX .GT.(IZDM*IZDN)) GO TO 10037
C
C ZNOW is the element of ZDAT pointed at by element INDX of the index
C array.
C
          ZNOW=ZNXT
C
C ZNXT is the element of ZDAT pointed at by element INDX+1 of the index
C array.  If element INDX is the last element of the array, ZNXT is just
C set to a value different from ZNOW (the smallest element of ZDAT).
C
          IF (.NOT.(INDX.LT.IZDM*IZDN)) GO TO 10039
            ZNXT=ZVAL(IWRK(II01+INDX+1))
          GO TO 10040
10039     CONTINUE
            ZNXT=ZVAL(IWRK(II01+1))
10040     CONTINUE
C
C If ZNXT is equal to ZNOW, bump the value of NEQU, which keeps track
C of the number of consecutive elements of the index array that have
C pointed to the same value in the ZDAT array.
C
          IF (.NOT.(ZNXT.EQ.ZNOW)) GO TO 10041
C
            NEQU=NEQU+1
C
C Otherwise, ...
C
          GO TO 10042
10041     CONTINUE
C
C ... if a group of equal values has been seen but not yet processed and
C if it's not too big (where "too big" is defined pretty heuristically,
C the object being to prevent the code from burning up a bunch of time
C on what is probably a pointless search for a high/low label position
C that the user won't care about) and if it's not a group of special
C values, ...
C
            IF (.NOT.(NEQU.GT.0..AND.NEQU.LT.64.AND.(SVAL.EQ.0..OR.ZNOW.
     +NE.SVAL))) GO TO 10043
C
C ... process the group.  Processing consists of dividing the group into
C subgroups that are spatially connected (meaning that, given any two
C elements, A and B, of the subgroup, there's a sequence of elements of
C the subgroup that begins with A, ends with B, and is such that any two
C consecutive elements of the sequence point to elements of ZDAT that
C are within one grid unit of each other in both X and Y.)  NEQU is the
C number of equalities seen and is therefore one less than the number of
C equal values in the group.  INDX points to the element of the index
C array defining the last element of the group.  JNDX points to the
C element of the index array defining the first element of the group.
C KNDX points to the element of the index array defining the last
C element of the subgroup currently being worked on.
C
              JNDX=INDX-NEQU
              KNDX=JNDX
C
C Loop as long as elements of the group remain.
C
10044         CONTINUE
              IF (.NOT.(JNDX.LT.INDX)) GO TO 10045
C
C Look for another subgroup.
C
  108           CONTINUE
                DO 10046 LNDX=KNDX+1,INDX
                  LIM1=MOD(IWRK(II01+LNDX),IZDM)
                  LJM1=(IWRK(II01+LNDX))/IZDM
                  DO 10047 MNDX=JNDX,KNDX
                    MIM1=MOD(IWRK(II01+MNDX),IZDM)
                    MJM1=(IWRK(II01+MNDX))/IZDM
                    IF (.NOT.(ABS(LIM1-MIM1).LE.1.AND.ABS(LJM1-MJM1).LE.
     +1))           GO TO 10048
                      KNDX=KNDX+1
                      IF (.NOT.(KNDX.NE.LNDX)) GO TO 10049
                        ITMP=IWRK(II01+KNDX)
                        IWRK(II01+KNDX)=IWRK(II01+LNDX)
                        IWRK(II01+LNDX)=ITMP
10049                 CONTINUE
                      GO TO 108
10048               CONTINUE
10047             CONTINUE
10046           CONTINUE
C
C A subgroup has been found.  If it contains more than one element and
C not more than the number of elements specified by 'HLE' as the upper
C limit ...
C
                IF (.NOT.(JNDX.LT.KNDX.AND.(IHLE.EQ.1.OR.KNDX-JNDX.LT.IH
     +LE)))     GO TO 10050
C
C ... examine the elements of ZDAT pointed at by members of the subgroup
C to see whether the subgroup can be considered a high or a low.  IHLC
C is set positive to indicate that the subgroup is a high, negative to
C indicate that it is a low.  XLBC, YLBC, and NLBC are used to compute a
C mean position for the high or the low.
C
                  IHLC=0
                  XLBC=0.
                  YLBC=0.
                  NLBC=0
C
C Loop through the elements of the subgroup.
C
                  DO 10051 LNDX=JNDX,KNDX
                    IPNT=MOD(IWRK(LNDX),IZDM)+1
                    JPNT=(IWRK(LNDX))/IZDM+1
                    IBEG=MAX(1,IPNT-IRNG)
                    IEND=MIN(IZDM,IPNT+IRNG)
                    JBEG=MAX(1,JPNT-JRNG)
                    JEND=MIN(IZDN,JPNT+JRNG)
                    XLBC=XLBC+XAT1+RZDM*(REAL(IPNT)-1.)
                    YLBC=YLBC+YAT1+RZDN*(REAL(JPNT)-1.)
                    NLBC=NLBC+1
                    DO 10052 I=IBEG,IEND
                      DO 10053 J=JBEG,JEND
                        IF (.NOT.(I.NE.IPNT.OR.J.NE.JPNT)) GO TO 10054
                          IF (.NOT.(SVAL.EQ.0..OR.ZDAT(I,J).NE.SVAL)) GO
     + TO 10055
                            IF (.NOT.(ZNOW.GT.ZDAT(I,J))) GO TO 10056
                              IF (IHLC.LT.0) GO TO 109
                              IHLC=+1
                            GO TO 10057
10056                       CONTINUE
                            IF (.NOT.(ZNOW.LT.ZDAT(I,J))) GO TO 10058
                              IF (IHLC.GT.0) GO TO 109
                              IHLC=-1
10057                       CONTINUE
10058                       CONTINUE
                          GO TO 10059
10055                     CONTINUE
                            IF (ABS(I-IPNT).LT.2.AND.
     +                          ABS(J-JPNT).LT.2) GO TO 109
10059                     CONTINUE
10054                   CONTINUE
10053                 CONTINUE
10052               CONTINUE
10051             CONTINUE
C
C Finish computing the location of the "high" or "low" and, ...
C
                  XLBC=XLBC/REAL(NLBC)
                  IF (XLBC.LE.XAT1.OR.XLBC.GE.XATM) GO TO 109
                  YLBC=YLBC/REAL(NLBC)
                  IF (YLBC.LE.YAT1.OR.YLBC.GE.YATN) GO TO 109
C
C ... if all comparisons indicate that a high has been found, ...
C
                  IF (.NOT.(IHLC.GT.0)) GO TO 10060
C
C ... put a "high" label there; ...
C
                    IF (.NOT.(TXHI(1:LTHI).NE.' ')) GO TO 10061
                      IHOL=0
                      L10019=    3
                      GO TO 10019
10062                 CONTINUE
10061               CONTINUE
C
C ... but if all comparisons indicate that a low has been found, ...
C
                  GO TO 10063
10060             CONTINUE
                  IF (.NOT.(IHLC.LT.0)) GO TO 10064
C
C ... put a "low" label there.
C
                    IF (.NOT.(TXLO(1:LTLO).NE.' ')) GO TO 10065
                      IHOL=1
                      L10019=    4
                      GO TO 10019
10066                 CONTINUE
10065               CONTINUE
C
10063             CONTINUE
10064             CONTINUE
C
10050           CONTINUE
C
C We're done with that subgroup; initialize to look for the next one.
C
  109           JNDX=KNDX+1
                KNDX=JNDX
C
              GO TO 10044
10045         CONTINUE
C
10043       CONTINUE
C
C All elements of the group have been processed, so zero NEQU and keep
C looking through the index array.
C
            NEQU=0
C
10042     CONTINUE
C
        GO TO 10036
10037   CONTINUE
C
C All highs and lows that were missed by the normal algorithm have been
C found and labeled.
C
10035 CONTINUE
C
C Tell IFTRAN to use the FORTRAN-77 implementation of BLOCK invocations.
C
C
C Discard any integer workspace that may have been used above.
C
  110 LI01=0
C
C Return PLOTCHAR to its default state.
C
      CALL PCSETI ('TE',ITMP)
      IF (ICFELL('CPHLLB',4).NE.0) RETURN
C
C Done.
C
      RETURN
C
C The following internal procedure writes a high (if IHOL=0) or low (if
C IHOL=1) label, centered at the point whose subscript coordinates are
C IPNT and JPNT.
C
10019 CONTINUE
C
        IVIS=1
C
        IF (IMPF.NE.0) THEN
          XTMP=XLBC
          YTMP=YLBC
          CALL HLUCPMPXY (IMPF,XTMP,YTMP,XLBC,YLBC)
          IF (ICFELL('CPHLLB',5).NE.0) RETURN
          IF ((OORV.NE.0.).AND.(XLBC.EQ.OORV.OR.YLBC.EQ.OORV)) IVIS=0
        END IF
C
        IF (IVIS.NE.0) THEN
          XCLB=CUFX(XLBC)
          IF (ICFELL('CPHLLB',6).NE.0) RETURN
          YCLB=CUFY(YLBC)
          IF (ICFELL('CPHLLB',7).NE.0) RETURN
          ZDVL=ZDAT(IPNT,JPNT)
          IF (IHOL.EQ.0) THEN
            CALL CPSBST(TXHI(1:LTHI),CTMA,LCTM)
          ELSE
            CALL CPSBST(TXLO(1:LTLO),CTMA,LCTM)
          END IF
          CALL HLUCPCHHL (+1+4*IHOL)
          IF (ICFELL('CPHLLB',8).NE.0) RETURN
          IF (CTMA(1:LCTM).EQ.' ') GO TO 111
          CALL PLCHHQ (XLBC,YLBC,CTMA(1:LCTM),WCFS,360.,0.)
          IF (ICFELL('CPHLLB',9).NE.0) RETURN
          CALL HLUCPCHHL (-1-4*IHOL)
          IF (ICFELL('CPHLLB',10).NE.0) RETURN
          CALL PCGETR ('DL',DTOL)
          IF (ICFELL('CPHLLB',11).NE.0) RETURN
          CALL PCGETR ('DR',DTOR)
          IF (ICFELL('CPHLLB',12).NE.0) RETURN
          CALL PCGETR ('DB',DTOB)
          IF (ICFELL('CPHLLB',13).NE.0) RETURN
          CALL PCGETR ('DT',DTOT)
          IF (ICFELL('CPHLLB',14).NE.0) RETURN
          DTOL=DTOL+WWFS
          DTOR=DTOR+WWFS
          DTOB=DTOB+WWFS
          DTOT=DTOT+WWFS
          XTRA=.5*CHWM*WCHL*(XVPR-XVPL)
          DSTL=DTOL+XTRA
          DSTR=DTOR+XTRA
          DSTB=DTOB+XTRA
          DSTT=DTOT+XTRA
C
          IF (IOHL.NE.0) THEN
C
            IF (ANLB.EQ.0.) THEN
              XLLB=XCLB-DSTL
              XRLB=XCLB+DSTR
              YBLB=YCLB-DSTB
              YTLB=YCLB+DSTT
            ELSE
              XLBL=XCLB-DSTL*COS(ANLB)+DSTB*SIN(ANLB)
              XRBL=XCLB+DSTR*COS(ANLB)+DSTB*SIN(ANLB)
              XRTL=XCLB+DSTR*COS(ANLB)-DSTT*SIN(ANLB)
              XLTL=XCLB-DSTL*COS(ANLB)-DSTT*SIN(ANLB)
              YLBL=YCLB-DSTL*SIN(ANLB)-DSTB*COS(ANLB)
              YRBL=YCLB+DSTR*SIN(ANLB)-DSTB*COS(ANLB)
              YRTL=YCLB+DSTR*SIN(ANLB)+DSTT*COS(ANLB)
              YLTL=YCLB-DSTL*SIN(ANLB)+DSTT*COS(ANLB)
              XLLB=MIN(XLBL,XRBL,XRTL,XLTL)
              XRLB=MAX(XLBL,XRBL,XRTL,XLTL)
              YBLB=MIN(YLBL,YRBL,YRTL,YLTL)
              YTLB=MAX(YLBL,YRBL,YRTL,YLTL)
            END IF
C
            IF (IOHL/4.EQ.1) THEN
              IF (XLLB.LT.XVPL.OR.XRLB.GT.XVPR.OR.
     +            YBLB.LT.YVPB.OR.YTLB.GT.YVPT) GO TO 111
            ELSE IF (IOHL/4.GE.2) THEN
              DELX=0.
              IF (XLLB.LT.XVPL) DELX=XVPL-XLLB
              IF (XRLB+DELX.GT.XVPR) THEN
                IF (DELX.NE.0.) GO TO 111
                DELX=XVPR-XRLB
              END IF
              DELY=0.
              IF (YBLB.LT.YVPB) DELY=YVPB-YBLB
              IF (YTLB+DELY.GT.YVPT) THEN
                IF (DELY.NE.0.) GO TO 111
                DELY=YVPT-YTLB
              END IF
              XCLB=XCLB+DELX
              XLLB=XLLB+DELX
              XRLB=XRLB+DELX
              YCLB=YCLB+DELY
              YBLB=YBLB+DELY
              YTLB=YTLB+DELY
              XLBC=CFUX(XCLB)
              IF (ICFELL('CPHLLB',15).NE.0) RETURN
              YLBC=CFUY(YCLB)
              IF (ICFELL('CPHLLB',16).NE.0) RETURN
            END IF
C
          END IF
C
          IF (MOD(IOHL,4).NE.0) THEN
C
            ILB1=1
            ILB2=NLBS
            IF (MOD(IOHL,2).EQ.0) ILB1=INHL
            IF (MOD(IOHL/2,2).EQ.0) ILB2=INHL-1
C
              ILBL = ILB1
              GO TO 10069
10067         CONTINUE
              ILBL =ILBL +1
10069         CONTINUE
              IF (ILBL .GT.(ILB2)) GO TO 10068
C
              IF (ILBL.EQ.INIL) ETRA=.5*CHWM*WCIL*(XVPR-XVPL)
              IF (ILBL.EQ.INHL) ETRA=.5*CHWM*WCHL*(XVPR-XVPL)
              XCOL=RWRK(IR03+4*(ILBL-1)+1)
              YCOL=RWRK(IR03+4*(ILBL-1)+2)
              ANOL=RWRK(IR03+4*(ILBL-1)+3)
              SAOL=SIN(ANOL)
              CAOL=COS(ANOL)
              ICOL=INT(RWRK(IR03+4*(ILBL-1)+4))
              ODSL=RWRK(IR04-ICOL+3)+ETRA
              ODSR=RWRK(IR04-ICOL+4)+ETRA
              ODSB=RWRK(IR04-ICOL+5)+ETRA
              ODST=RWRK(IR04-ICOL+6)+ETRA
C
              IF (ANOL.EQ.0.) THEN
                XLOL=XCOL-ODSL
                XROL=XCOL+ODSR
                YBOL=YCOL-ODSB
                YTOL=YCOL+ODST
              ELSE
                XLBO=XCOL-ODSL*CAOL+ODSB*SAOL
                XRBO=XCOL+ODSR*CAOL+ODSB*SAOL
                XRTO=XCOL+ODSR*CAOL-ODST*SAOL
                XLTO=XCOL-ODSL*CAOL-ODST*SAOL
                YLBO=YCOL-ODSL*SAOL-ODSB*CAOL
                YRBO=YCOL+ODSR*SAOL-ODSB*CAOL
                YRTO=YCOL+ODSR*SAOL+ODST*CAOL
                YLTO=YCOL-ODSL*SAOL+ODST*CAOL
                XLOL=MIN(XLBO,XRBO,XRTO,XLTO)
                XROL=MAX(XLBO,XRBO,XRTO,XLTO)
                YBOL=MIN(YLBO,YRBO,YRTO,YLTO)
                YTOL=MAX(YLBO,YRBO,YRTO,YLTO)
              END IF
C
              IF (XRLB.GE.XLOL.AND.XLLB.LE.XROL.AND.
     +            YTLB.GE.YBOL.AND.YBLB.LE.YTOL) GO TO 111
C
            GO TO 10067
10068       CONTINUE
C
          END IF
C
          NLBS=NLBS+1
          IF (4*NLBS.GT.LR03) THEN
            CALL CPGRWS (RWRK,3,MAX(4*NLBS,LR03+100),IWSE)
            IF (IWSE.NE.0) THEN
              NLBS=NLBS-1
              GO TO 110
            ELSE IF (ICFELL('CPHLLB',17).NE.0) THEN
              NLBS=NLBS-1
              RETURN
            END IF
          END IF
          RWRK(IR03+4*(NLBS-1)+1)=XCLB
          RWRK(IR03+4*(NLBS-1)+2)=YCLB
          RWRK(IR03+4*(NLBS-1)+3)=ANLB
          RWRK(IR03+4*(NLBS-1)+4)=-NR04
          NR04=NR04+6
          IF (NR04.GT.LR04) THEN
            CALL CPGRWS (RWRK,4,MAX(NR04,LR04+100),IWSE)
            IF (IWSE.NE.0) THEN
              NLBS=NLBS-1
              GO TO 110
            ELSE IF (ICFELL('CPHLLB',18).NE.0) THEN
              NLBS=NLBS-1
              RETURN
            END IF
          END IF
          RWRK(IR04+NR04-5)=REAL(IHOL+1)
          RWRK(IR04+NR04-4)=ZDAT(IPNT,JPNT)
          RWRK(IR04+NR04-3)=DTOL
          RWRK(IR04+NR04-2)=DTOR
          RWRK(IR04+NR04-1)=DTOB
          RWRK(IR04+NR04  )=DTOT
C
        END IF
C
  111 CONTINUE
      GO TO (10018,10034,10062,10066) , L10019
C
      END
