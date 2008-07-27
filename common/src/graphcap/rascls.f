C
C	$Id: rascls.f,v 1.4 2008-07-27 12:23:44 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE RASCLS(WHICH, IOS, STATUS)
C
C
C  Process the RASTER keywords.
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
      COMMON /CAPSCN/ SCSSTR, SCSSIZ, SCTSTR, SCTSIZ, SCNLLX,
     1                SCNLLY, SCNURX, SCNURY, SCNXOF, SCNYOF,
     2                SCNXSC, SCNYSC, SCNFMT, SCNFIN, SCVFMT,
     3                SCVFIN, SCNSIM
      INTEGER         SCSMAX, SCTMAX, SFMMAX, SFNMAX, SCVFMX,
     1                SCVFIX
      PARAMETER  (SCSMAX=50, SCTMAX=50, SFMMAX=10, SFNMAX=8)
      PARAMETER  (SCVFMX=10, SCVFIX=8)
      INTEGER         SCSSTR(SCSMAX), SCSSIZ, SCTSTR(SCTMAX), SCTSIZ,
     2                SCNLLX, SCNLLY, SCNURX, SCNURY, SCNXOF, SCNYOF,
     3                SCNFMT(SFMMAX,4)      , SCNFIN(SFNMAX),
     4                SCVFMT(SCVFMX,4)      , SCVFIN(SCVFIX)
      REAL            SCNXSC, SCNYSC, SCNRIN(SFNMAX), SCVRIN(SCVFIX)
      LOGICAL         SCNSIM
      INTEGER         LENSCN
      PARAMETER  (LENSCN=SCSMAX+1+SCTMAX+1+1+1+1+1+1+1+1+1+
     1                   (SFMMAX*4)+SFNMAX+(SCVFMX*4)+SCVFIX+1)
      EQUIVALENCE (SCNFIN,SCNRIN), (SCVFIN,SCVRIN)
C
      INTEGER WHICH(WHSIZE), IOS, STATUS
      INTEGER ROW1, ROW2, ROW3
      INTEGER DUMMY, HOLDER(SFNMAX*4), II, JJ
C
C  Branch to the proper level 2 processing.
C
      ROW1 = WHICH(2)
      ROW2 = WHICH(3)
C
C  RASTER class processing--
C
C      ROW1    ROW2    ROW3     Keyword
C      ----    ----    ----     --------------------------------------
C       1       1               RASTER_COORD_LOWER_LEFT_X
C       1       2               RASTER_COORD_LOWER_LEFT_Y
C       1       3               RASTER_COORD_UPPER_RIGHT_X
C       1       4               RASTER_COORD_UPPER_RIGHT_Y
C       1       7               RASTER_COORD_XOFFSET
C       1       8               RASTER_COORD_YOFFSET
C       1       9               RASTER_COORD_XSCALE
C       1      10               RASTER_COORD_YSCALE
C       2       1               RASTER_DATA_FORMAT
C       2       2               RASTER_DATA_ENCODING
C       2       3               RASTER_DATA_FLOATING_INFO
C       3       1       1       RASTER_VECTOR_COUNT_FORMAT
C       3       1       2       RASTER_VECTOR_COUNT_ENCODING
C       3       1       3       RASTER_VECTOR_COUNT_FLOATING_INFO
C       4       1       1       RASTER_HORIZONTAL_INSTRUCTION_START
C       4       1       2       RASTER_HORIZONTAL_INSTRUCTION_TERMINATOR
C       5                       RASTER_SIMULATE
C
      IF (ROW1.EQ.1 .AND. ROW2.EQ.1) THEN
        CALL GTINT(SCNLLX, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.1 .AND. ROW2.EQ.2) THEN
        CALL GTINT(SCNLLY, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.1 .AND. ROW2.EQ.3) THEN
        CALL GTINT(SCNURX, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.1 .AND. ROW2.EQ.4) THEN
        CALL GTINT(SCNURY, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.1 .AND. ROW2.EQ.7) THEN
        CALL GTINT(SCNXOF, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.1 .AND. ROW2.EQ.8) THEN
        CALL GTINT(SCNYOF, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.1 .AND. ROW2.EQ.9) THEN
        CALL GTFLT(SCNXSC, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.1 .AND. ROW2.EQ.10) THEN
        CALL GTFLT(SCNYSC, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.2 .AND. ROW2.EQ.1) THEN
        CALL GTINT(HOLDER, SFMMAX*4, DUMMY, IOS, STATUS)
        IF (DUMMY .NE. 0) THEN
                SCNFIN(2) = DUMMY/4
                DO 10 II = 1,SCNFIN(2)
                DO 10 JJ = 1,4
                        SCNFMT(II,JJ) = HOLDER((II-1)*4+JJ)
 10             CONTINUE
        END IF
      ELSE IF (ROW1.EQ.2 .AND. ROW2.EQ.2) THEN
        CALL GTINT(SCNFIN(1), 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.2 .AND. ROW2.EQ.3) THEN
        CALL GTFLT(SCNRIN(5), 4, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.5) THEN
        CALL GTLOG(SCNSIM, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.4) THEN
        ROW3 = WHICH(4)
        IF(ROW2.EQ.1 .AND. ROW3.EQ.1) THEN
                CALL GTSTR(SCSSTR, SCSMAX, SCSSIZ, IOS, STATUS)
        ELSE IF (ROW2.EQ.1 .AND. ROW3.EQ.2) THEN
                CALL GTSTR(SCTSTR, SCTMAX, SCTSIZ, IOS, STATUS)
        END IF
      ELSE IF (ROW1.EQ.3 .AND.ROW2.EQ.1) THEN
        ROW3 = WHICH(4)
        IF (ROW3.EQ.1) THEN
                CALL GTINT(HOLDER, SCVFMX*4, DUMMY, IOS, STATUS)
                IF (DUMMY .NE. 0) THEN
                        SCVFIN(2) = DUMMY/4
                        DO 20 II = 1,SCVFIN(2)
                        DO 20 JJ = 1,4
                                SCVFMT(II,JJ) = HOLDER((II-1)*4+JJ)
 20                     CONTINUE
                END IF
        ELSE IF (ROW3.EQ.2) THEN
                CALL GTINT(SCVFIN(1), 1, DUMMY, IOS, STATUS)
        ELSE IF (ROW3.EQ.3) THEN
                CALL GTFLT(SCVRIN(5), 4, DUMMY, IOS, STATUS)
        END IF
C
      END IF
      RETURN
      END
