C
C	$Id: gwipm.f,v 1.7 2008-07-27 00:21:08 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWIPM
C
C  Process POLYMARKER instruction.
C
      include 'gksin.h'
      include 'gwiio.h'
      include 'gwiins.h'
      include 'gwiwsl.h'
      include 'gwiarq.h'
      include 'gwiast.h'
      include 'gwiadc.h'
      include 'gwiopc.h'
      include 'gwienu.h'
C
      INTEGER  KALL, IPRIM, NBYTES, NUMO, K, INDX1, INDX2
      DIMENSION NUMO(2)
C
      SAVE
      DATA KALL/0/ , IPRIM/2/
C
      KALL = KALL+1
      IF  (KALL .EQ. 1) THEN
C
C  If the picture is empty, send the clip indicator and rectangle;
C  set the WSL entry "DISPLAY SURFACE EMPTY" to "NOT EMPTY".
C
        IF (MDEMPT .EQ. GEMPTY) CALL GWICLP(1)
        MDEMPT = GNEMPT
C
C  Process pending attributes.
C
        IF (AGPEND(IPRIM)) THEN
C
C  Some changes are pending.
C
          IF (VALCHG(IVPLIX))  THEN
C
C  Send bundle index.
C
            NBYTES = 1+(MIXFW-1)/8
            CALL GWPTNI (CLMBIX, IDMBIX, NBYTES,  RERR)
            CALL GWPTPR (MRPMIX, MIXFW,  1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested.  Clear change flag.
C
            MSPMIX         = MRPMIX
            VALCHG(IVPMIX) = .FALSE.
          END IF
          IF (VALCHG(IVMTYP))  THEN
C
C  MARKER TYPE
C
            NBYTES = 1+(MIXFW-1)/8
            CALL GWPTNI (CLMTYP, IDMTYP,  NBYTES,  RERR)
            CALL GWPTPR (MRMTYP, MIXFW,  1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested.  Clear change flag.
C
            MSMTYP         = MRMTYP
            VALCHG(IVMTYP) = .FALSE.
          END IF
          IF (VALCHG(IVMSZS))  THEN
C
C  MARKER SIZE
C
            NBYTES = 1+(2*MCFPP-1)/8
            CALL GWPTNI (CLMSIZ, IDMSIZ, NBYTES,  RERR)
            CALL GFLCNV (ARMSZS,NUMO)
            CALL GWPTPR (NUMO, MCFPP,  2, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested.  Clear change flag.
C
            ASMSZS         = ARMSZS
            VALCHG(IVMSZS) = .FALSE.
          END IF
          IF (VALCHG(IVPMCI))  THEN
C
C  MARKER COLOR
C
            NBYTES = 1+(MCIXFW-1)/8
            CALL GWPTNI (CLMCLR, IDMCLR, NBYTES,  RERR)
            CALL GWPTPR (MRPMCI, MCIXFW,  1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested.  Clear change flag.
C
            MSPMCI         = MRPMCI
            VALCHG(IVPMCI) = .FALSE.
          ENDIF
          IF (ANYASF)  THEN
C
C  Some ASF has changed.
C
            CALL GWISAS (IPRIM, RERR)
            IF (RERR.NE.0)  RETURN
          END IF
C
C  Clear aggregate change variable.
C
           AGPEND(IPRIM) = .FALSE.
C
        ENDIF
C
C  Treat first call, put out opcode, and points.
C
C
C  Put out opcode (class and id) and total length.
C
        NBYTES = 1+(2*RL1*MVDCFW-1)/8
        CALL GWPTNI (CLPMRK, IDPMRK, NBYTES, RERR)
C
C  Put out first points array.
C
C  Truncate points to limits of NDC unit square, convert to VDC,
C  and store in WPXPY.
C
        DO 30 K=1,RL2
        INDX1 = 2*K-1
        INDX2 = INDX1+1
        WPXPY(INDX1) = MXOFF + INT(REAL(MXSCAL)*
     -                 (MAX(0.,MIN(1.0,RX(K)))))
        WPXPY(INDX2) = MYOFF + INT(REAL(MYSCAL)*
     -                 (MAX(0.,MIN(1.0,RY(K)))))
   30   CONTINUE
C
C  Send out points.
C
        CALL GWPTPR (WPXPY,  MVDCFW,     2*RL2, RERR)
        IF (RERR .NE. 0) RETURN
C
C  If there is to be no continuation, reset the parameter "KALL".
C
        IF (CONT .EQ. 0) THEN
          KALL = 0
          RETURN
        ENDIF
      ENDIF
C
C  Treat the continuation calls.
C
      IF (KALL .GT. 1) THEN
C
C  Truncate points to limits of NDC unit square, convert to VDC,
C  and store in WPXPY.
C
        DO 40 K=1,RL2
          INDX1 = 2*K-1
          INDX2 = INDX1+1
          WPXPY(INDX1) = MXOFF + INT(REAL(MXSCAL)*
     -                   (MAX(0.,MIN(1.0,RX(K)))))
          WPXPY(INDX2) = MYOFF + INT(REAL(MYSCAL)*
     -                   (MAX(0.,MIN(1.0,RY(K)))))
   40   CONTINUE
        IF (CONT .EQ. 0) THEN
          CALL GWPTPR (WPXPY,  MVDCFW,     2*RL2, RERR)
          IF (RERR .NE. 0) RETURN
          KALL = 0
        ELSE
          CALL GWPTPR (WPXPY,  MVDCFW,     2*RL2, RERR)
          IF (RERR .NE. 0) RETURN
        ENDIF
      ENDIF
      RETURN
C
      END
