C
C	$Id: devcls.f,v 1.4 2008-07-27 12:23:43 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE DEVCLS(WHICH, IOS, STATUS)
C
C  Level two DEVICE class processing.
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
      COMMON /CAPLIN/ PLAVBL, LDSSTR, LDSSIZ, LDTSTR, LDTSIZ,
     1                LMSSTR, LMSSIZ, LMTSTR, LMTSIZ, LCSSTR,
     2                LCSSIZ, LCTSTR, LCTSIZ, LINFIN, LINFMT,
     3                LWSSTR, LWSSIZ, LWTSTR, LWTSIZ, LWTFIN,
     4                LWTFMT, LWTRNG, LWTSCF, LBSSTR, LBSSIZ,
     5                LBTSTR, LBTSIZ, LPSSTR, LPSSIZ, LPTSTR,
     6                LPTSIZ
      INTEGER         LDSMAX, LDTMAX, LMSMAX, LMTMAX, LCSMAX,
     1                LCTMAX, LVCFMX, LWSMAX, LWTMAX, LWTFMX,
     2                LBSMAX, LBTMAX, LPSMAX, LPTMAX
      PARAMETER   (LDSMAX=30, LDTMAX=15, LMSMAX=30, LMTMAX=15,
     1             LCSMAX=30, LCTMAX=15, LVCFMX=8,  LWSMAX=30,
     2             LWTMAX=15, LWTFMX=8 , LBSMAX=30, LBTMAX=15,
     3             LPSMAX=20, LPTMAX=20)
      INTEGER         LDSSTR(LDSMAX), LDSSIZ, LDTSTR(LDTMAX),
     1                LDTSIZ, LMSSTR(LMSMAX), LMSSIZ,
     2                LMTSTR(LMTMAX), LMTSIZ, LCSSTR(LCSMAX),
     3                LCSSIZ, LCTSTR(LCTMAX), LCTSIZ, LINFIN(8),
     4                LINFMT(LVCFMX,4)      , LWSSTR(LWSMAX),
     5                LWSSIZ, LWTSTR(LWTMAX), LWTSIZ, LWTFIN(8),
     6                LWTFMT(LWTFMX,4)      , LWTRNG(2)     ,
     7                LBSSTR(LBSMAX), LBSSIZ, LBTSTR(LBTMAX),
     8                LBTSIZ, LPSSTR(LPSMAX), LPSSIZ,
     9                LPTSTR(LPTMAX), LPTSIZ
      LOGICAL         PLAVBL
      REAL            LWTSCF,LINRIN(8),LWTRIN(8)
      INTEGER         LENLIN
      PARAMETER   (LENLIN=1+LDSMAX+1+LDTMAX+1+LMSMAX+1+
     1             LMTMAX+1+LCSMAX+1+LCTMAX+1+8+LVCFMX*4+
     2             LWSMAX+1+LWTMAX+1+8+LWTFMX*4+2+1+LBSMAX+
     3             1+LBTMAX+1+LPSMAX+1+LPTMAX+1)
      EQUIVALENCE (LINFIN,LINRIN), (LWTFIN,LWTRIN)
C
      INTEGER WHICH(WHSIZE), IOS, STATUS
      INTEGER ROW1, ROW2, ROW3
      INTEGER DUMMY, HOLDER(DCFTMX*4), II, JJ
C
C  Branch to the proper level 2 processing.
C
      ROW1 = WHICH(2)
      ROW2 = WHICH(3)
C
C  DEVICE class processing--
C
C       ROW1   ROW2    ROW3   Keyword
C       ----   ----    ----   ------------------------------------
C       1       1             DEVICE_GRAPHIC_INIT
C       2       1             DEVICE_TEXT_INIT
C       3       1             DEVICE_COORD_LOWER_LEFT_X
C       3       2             DEVICE_COORD_LOWER_LEFT_Y
C       3       3             DEVICE_COORD_UPPER_RIGHT_X
C       3       4             DEVICE_COORD_UPPER_RIGHT_Y
C       3       5             DEVICE_COORD_FORMAT
C       3       6             DEVICE_COORD_ENCODING
C       3       7             DEVICE_COORD_XOFFSET
C       3       8             DEVICE_COORD_YOFFSET
C       3       9             DEVICE_COORD_XSCALE
C       3      10             DEVICE_COORD_YSCALE
C       3      11             DEVICE_COORD_FLOATING_INFO
C       4       *       *     DEVICE_COLOR_ (Invoke SUBROUTINE DEVCOL)
C       5                     DEVICE_BATCH
C       6       1             DEVICE_CURSOR_HOME
C       7       *       *     DEVICE_MAP_ (Invoke SUBROUTINE DEVMAP)
C       8                     DEVICE_ERASE
C       9       1       1     DEVICE_VECTOR_COUNT_FORMAT
C       9       1       2     DEVICE_VECTOR_COUNT_ENCODING
C       9       1       3     DEVICE_VECTOR_COUNT_FLOATING_INFO
C      10       1             DEVICE_WINDOW_LOWER_LEFT_X
C      10       2             DEVICE_WINDOW_LOWER_LEFT_Y
C      10       3             DEVICE_WINDOW_UPPER_RIGHT_X
C      10       4             DEVICE_WINDOW_UPPER_RIGHT_Y
C
      IF (ROW1.EQ.1 .AND. ROW2.EQ.1) THEN
        CALL GTSTR(DGISTR, DGIMAX, DGISIZ, IOS, STATUS)
      ELSE IF (ROW1.EQ.2 .AND. ROW2.EQ.1) THEN
        CALL GTSTR(DTISTR, DTIMAX, DTISIZ, IOS, STATUS)
      ELSE IF (ROW1.EQ.3 .AND. ROW2.EQ.1) THEN
        CALL GTINT(DCDLLX, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.3 .AND. ROW2.EQ.2) THEN
        CALL GTINT(DCDLLY, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.3 .AND. ROW2.EQ.3) THEN
        CALL GTINT(DCDURX, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.3 .AND. ROW2.EQ.4) THEN
        CALL GTINT(DCDURY, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.3 .AND. ROW2.EQ.5) THEN
        CALL GTINT(HOLDER, DCFTMX*4, DUMMY, IOS, STATUS)
        IF (DUMMY .NE. 0) THEN
                CORFIN(2) = DUMMY/4
                DO 10 II = 1,CORFIN(2)
                DO 10 JJ = 1,4
                        CORFMT(II,JJ) = HOLDER((II-1)*4+JJ)
 10             CONTINUE
        END IF
      ELSE IF (ROW1.EQ.3 .AND. ROW2.EQ.6) THEN
        CALL GTINT(CORFIN(1), 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.3 .AND. ROW2.EQ.7) THEN
        CALL GTINT(CORXOF, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.3 .AND. ROW2.EQ.8) THEN
        CALL GTINT(CORYOF, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.3 .AND. ROW2.EQ.9) THEN
        CALL GTFLT(CORXSC, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.3 .AND. ROW2.EQ.10) THEN
        CALL GTFLT(CORYSC, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.3 .AND. ROW2.EQ.11) THEN
        CALL GTFLT(CORRIN(5), 4, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.4) THEN
        CALL DEVCOL(WHICH, IOS, STATUS)
      ELSE IF (ROW1.EQ.5) THEN
        CALL GTLOG(BATCH, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.6) THEN
        CALL GTSTR(DHCSTR, DHCMAX, DHCSIZ, IOS, STATUS)
      ELSE IF (ROW1.EQ.7) THEN
        CALL DEVMAP(WHICH, IOS, STATUS)
      ELSE IF (ROW1.EQ.8) THEN
        CALL GTSTR(DGESTR, DGEMAX, DGESIZ, IOS, STATUS)
      ELSE IF (ROW1.EQ.9 .AND.ROW2.EQ.1) THEN
        ROW3 = WHICH(4)
        IF (ROW3.EQ.1) THEN
                CALL GTINT(HOLDER, LVCFMX*4, DUMMY, IOS, STATUS)
                IF (DUMMY .NE. 0) THEN
                        LINFIN(2) = DUMMY/4
                        DO 20 II = 1,LINFIN(2)
                        DO 20 JJ = 1,4
                                LINFMT(II,JJ) = HOLDER((II-1)*4+JJ)
 20                     CONTINUE
                END IF
        ELSE IF (ROW3.EQ.2) THEN
                CALL GTINT(LINFIN(1), 1, DUMMY, IOS, STATUS)
        ELSE IF (ROW3.EQ.3) THEN
                CALL GTFLT(LINRIN(5), 4, DUMMY, IOS, STATUS)
        END IF
      ELSE IF (ROW1.EQ.10 .AND. ROW2.EQ.1) THEN
        CALL GTINT(VDWLLX, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.10 .AND. ROW2.EQ.2) THEN
        CALL GTINT(VDWLLY, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.10 .AND. ROW2.EQ.3) THEN
        CALL GTINT(VDWURX, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.10 .AND. ROW2.EQ.4) THEN
        CALL GTINT(VDWURY, 1, DUMMY, IOS, STATUS)
C
      END IF
      RETURN
      END
