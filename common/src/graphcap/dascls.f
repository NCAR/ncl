C
C	$Id: dascls.f,v 1.4 2008-07-27 12:23:43 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE DASCLS(WHICH, IOS, STATUS)
C
C  Level two DASH class processing.
C
C  INPUT
C       WHICH   --  the encoded path flags.
C
C  OUTPUT
C       IOS     --  I/O status flag.  This flag is valid only
C                   if STATUS indicates an error.
C       STATUS  --  The error status as defined in COMMON CAPERR.
C
      COMMON /PARTB1/ PART1, KEYSEP, KEYTER, FRMCOM
      COMMON /PARTB2/ PART2, PART3, PART4, PART5, CTSTR, CTLOC
C
C  THE NUMBER OF WORDS IN THE SEARCH PATH MUST BE BIG ENOUGH TO HOLD
C       THE NUMBER OF BITS PER PATH TIMES THE NUMBER OF LEVELS
C
      INTEGER WHSIZE
      PARAMETER (WHSIZE=20)
C
      INTEGER PARTSZ, OTHSZ, NTABLE
      PARAMETER(PARTSZ=3000, OTHSZ=150, NTABLE=50)
      CHARACTER*1 KEYSEP,KEYTER
      CHARACTER*1 FRMCOM(2)
      INTEGER PART2(OTHSZ), PART3(OTHSZ), PART4(NTABLE), PART5(NTABLE*2)
      CHARACTER*1 PART1(PARTSZ)
      INTEGER CTSTR, CTLOC
      COMMON /CAPERR/ ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1                FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR,
     2                UNDASC, DEVERR, DOCERR, TBLERR , STSZER, ENTERR,
     3                TABERR, TABSER, PRSFIL
      INTEGER ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1        FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR, UNDASC,
     2        DEVERR, DOCERR, STSZER, ENTERR, TABERR, TABSER, TBLERR,
     3        PRSFIL
      COMMON /CAPDEV/ DGISTR, DGISIZ, DGESTR, DGESIZ, DTISTR,
     1                DTISIZ, DCDLLX, DCDLLY, DCDURX, DCDURY,
     3                DCOAVL, CORFMT, CORFIN, BATCH , DHCSIZ,
     4                DHCSTR, CORXOF, CORYOF, DASBIT, CORXSC,
     5                CORYSC, VDWLLX, VDWLLY, VDWURX, VDWURY
      INTEGER         DGIMAX, DGEMAX, DTIMAX, DCFTMX, DHCMAX
      PARAMETER   (DGIMAX=300, DGEMAX=150, DTIMAX=100)
      PARAMETER   (DCFTMX=30 , DHCMAX=50)
      INTEGER         DGISTR(DGIMAX), DGISIZ, DGESTR(DGEMAX),
     1                DGESIZ, DTISTR(DTIMAX), DTISIZ, DCDLLX,
     2                DCDLLY, DCDURX, DCDURY, CORFMT(DCFTMX,4),
     3                CORFIN(8)     , DHCSIZ, DHCSTR(DHCMAX),
     4                CORXOF, CORYOF, DASBIT, VDWLLX, VDWLLY,
     5                VDWURX, VDWURY
      REAL            CORXSC, CORYSC, CORRIN(8)
      LOGICAL         DCOAVL, BATCH
C  Size of the COMMON
      INTEGER         LENDEV
      PARAMETER   (LENDEV=DGIMAX+1+DGEMAX+1+DTIMAX+1+4+1+4*DCFTMX+
     1                  8+2+DHCMAX+9)
      EQUIVALENCE (CORFIN,CORRIN)
C
      INTEGER WHICH(WHSIZE), IOS, STATUS
      INTEGER ROW2, DUMMY
C
C  Branch to the proper level 2 processing.
C
      ROW2 = WHICH(2)
C
C  DASH class processing--
C
C       ROW2                    KEYWORD
C       ----                    ---------------
C        1                      DASH_BIT_LENGTH
C
      IF (ROW2.EQ.1) THEN
                CALL GTINT(DASBIT, 1, DUMMY, IOS, STATUS)
      END IF
C
      RETURN
      END
