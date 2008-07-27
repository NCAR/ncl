C
C	$Id: devmap.f,v 1.4 2008-07-27 12:23:43 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE DEVMAP(WHICH, IOS, STATUS)
C
C  Process the DEVICE_MAP keywords.
C
C
C  INPUT
C       WHICH   --  the encoded path flags.
C
C  OUTPUT
C       IOS     --  I/O status flag.  This flag is valid only
C                   if STATUS indicates an error.
C       STATUS  --  The error status as defined in COMMON CAPERR.
C
C
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
      COMMON /CAPCOL/ COLINT, COLIDX, IDXCUR, VDMINT, DMPAVL, COLFMT
     1               ,COLFIN, IDXMAX, MSTSTR, MSTSIZ, MTRSTR, DMPMDL
     2               ,MTRSIZ, DMPIDV, DMPFIN, DMPFMT
      INTEGER         MAPMAX, COLMAX, MSTMAX, MTRMAX, DMPMAX
      PARAMETER   (MAPMAX=256, COLMAX=15, MSTMAX=50, MTRMAX=20)
      PARAMETER   (DMPMAX=50)
      INTEGER         COLINT(MAPMAX*3)      , COLIDX(MAPMAX), IDXCUR,
     1                VDMINT, COLFMT(COLMAX,4)      , COLFIN(8)     ,
     2                IDXMAX, MSTSTR(MSTMAX), MSTSIZ, MTRSTR(MTRMAX),
     3                DMPMDL, MTRSIZ, DMPFIN(8)     , DMPFMT(DMPMAX,4)
      LOGICAL         DMPAVL, DMPIDV
      REAL            COLRIN(8),DMPRIN(8)
      INTEGER LENCOL
      PARAMETER   (LENCOL=MAPMAX*3+MAPMAX+1+1+1+COLMAX*4+8+1+MSTMAX+
     1                    1+MTRMAX+1+1+1+8+DMPMAX*4)
      EQUIVALENCE (COLFIN,COLRIN),(DMPFIN,DMPRIN)
      COMMON /CAPERR/ ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1                FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR,
     2                UNDASC, DEVERR, DOCERR, TBLERR , STSZER, ENTERR,
     3                TABERR, TABSER, PRSFIL
      INTEGER ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1        FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR, UNDASC,
     2        DEVERR, DOCERR, STSZER, ENTERR, TABERR, TABSER, TBLERR,
     3        PRSFIL
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
C
      INTEGER IOS, STATUS, WHICH(WHSIZE)
      INTEGER ROW3, ROW4, DUMMY, II, JJ, HOLDER(DMPMAX*4)
C
C  Get levels 3 and 4 of the parse path.
C
      ROW3 = WHICH(3)
      ROW4 = WHICH(4)
C
C  Branch to proper keyword processing.
C
C       ROW3    ROW4    Keyword
C       ----    ----    ---------------------------------------
C       1               DEVICE_MAP_AVAILABLE
C       2       1       DEVICE_MAP_INDEX_RANGE_MAX
C       2       2       DEVICE_MAP_INDEX_RANGE_DEFINED
C       3       1       DEVICE_MAP_INTENSITY_FORMAT
C       3       2       DEVICE_MAP_INTENSITY_ENCODING
C       3       3       DEVICE_MAP_INTENSITY_FLOATING_INFO
C       4               DEVICE_MAP_INDIVIDUAL
C       5       1       DEVICE_MAP_INSTRUCTION_START
C       5       2       DEVICE_MAP_INSTRUCTION_TERMINATOR
C       6               DEVCIE_MAP_INIT
C       7               DEVICE_MAP_MODEL
C
      IF (ROW3.EQ.1) THEN
        CALL GTLOG(DMPAVL, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW3.EQ.2) THEN
        IF (ROW4.EQ.1) THEN
                CALL GTINT(IDXMAX, 1, DUMMY, IOS, STATUS)
        ELSE IF (ROW4.EQ.2) THEN
                CALL GTINT(IDXCUR, 1, DUMMY, IOS, STATUS)
C
C  Set the index user map up through IDXCUR.
C
                DO 100 II = 1, IDXCUR
                        COLIDX(II) = II-1
 100            CONTINUE
C
C  If the map maximum index is not set, set it to the current size.
C
                IF (IDXMAX .EQ. 0) IDXMAX = IDXCUR
        END IF
      ELSE IF (ROW3.EQ.3) THEN
        IF (ROW4 .EQ. 1) THEN
                CALL GTINT(HOLDER, DMPMAX*4, DUMMY, IOS, STATUS)
                IF (DUMMY .NE. 0) THEN
                        DMPFIN(2) = DUMMY/4
                        DO 10 II = 1,DMPFIN(2)
                        DO 10 JJ = 1,4
                                DMPFMT(II,JJ) = HOLDER((II-1)*4+JJ)
 10                     CONTINUE
                END IF
        ELSE IF (ROW4.EQ.2) THEN
                CALL GTINT(DMPFIN(1), 1, DUMMY, IOS, STATUS)
        ELSE IF (ROW4.EQ.3) THEN
                CALL GTFLT(DMPRIN(5), 4, DUMMY, IOS, STATUS)
        END IF
      ELSE IF (ROW3 .EQ. 4) THEN
                CALL GTLOG(DMPIDV, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW3.EQ.5 .AND. ROW4.EQ.1) THEN
                CALL GTSTR(MSTSTR, MSTMAX, MSTSIZ, IOS, STATUS)
      ELSE IF (ROW3.EQ.5 .AND. ROW4.EQ.2) THEN
                CALL GTSTR(MTRSTR, MTRMAX, MTRSIZ, IOS, STATUS)
      ELSE IF (ROW3 .EQ. 6) THEN
                CALL GTINT(COLINT, MAPMAX*3, DUMMY, IOS, STATUS)
      ELSE IF (ROW3 .EQ. 7) THEN
                CALL GTINT(DMPMDL, 1, DUMMY, IOS, STATUS)
      END IF
C
      RETURN
      END
