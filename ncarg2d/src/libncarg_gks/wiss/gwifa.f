C
C	$Id: gwifa.f,v 1.7 2008-07-27 00:21:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWIFA
C
C  Process GKS fill area instruction.
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
      INTEGER  KALL, IPRIM, NBYTES, INDX1, INDX2, IX
      SAVE
C
      DATA  KALL/0/, IPRIM/4/
C
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
          IF (VALCHG(IVFAIX))  THEN
C
C  GKS fill area index, send CGM fill bundle index.
C
            NBYTES = 1+(MIXFW-1)/8
            CALL GWPTNI (CLFBIX, IDFBIX,  NBYTES,  RERR)
            CALL GWPTPR (MRFAIX, MIXFW,  1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            MSFAIX         = MRFAIX
            VALCHG(IVFAIX) = .FALSE.
          END IF
          IF (VALCHG(IVFAIS))  THEN
C
C  GKS fill area interior style, send CGM interior style.
C
            NBYTES = 1+(MEFW-1)/8
            CALL GWPTNI (CLINTS, IDINTS, NBYTES,  RERR)
            CALL GWPTPR (MRFAIS, MEFW,  1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            MSFAIS         = MRFAIS
            VALCHG(IVFAIS) = .FALSE.
          END IF
          IF (VALCHG(IVFASI))  THEN
C
C  GKS fill area style index, send just the hatch index since we
C  are currently not supporting the pattern index.
C
            NBYTES = 1+(MIXFW-1)/8
            CALL GWPTNI (CLHAIX, IDHAIX, NBYTES,  RERR)
            CALL GWPTPR (MRFASI, MIXFW, 1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            MSFASI = MRFASI
            VALCHG(IVFASI) = .FALSE.
          END IF
          IF (VALCHG(IVFACI))  THEN
C
C  GKS fill area color index, send CGM fill color.
C
            NBYTES = 1+(MCIXFW-1)/8
            CALL GWPTNI (CLFCLR, IDFCLR, NBYTES, RERR)
            CALL GWPTPR (MRFACI, MCIXFW,  1, RERR)
            IF (RERR .NE. 0) RETURN
C
C  Set sent value to requested, clear change flag.
C
            MSFACI         = MRFACI
            VALCHG(IVFACI) = .FALSE.
          END IF
          IF (ANYASF)  THEN
C
C  Some GKS ASF has changed, send CGM ASFs.
C
            CALL GWISAS (IPRIM, RERR)
            IF (RERR.NE.0)  RETURN
          END IF
C
C  Clear aggregate change variable.
C
          AGPEND(IPRIM) = .FALSE.
        END IF
C
C  Treat first call, put out opcode, and points.
C
C
C  Put out opcode (class and id) and total length.
C
        NBYTES = 1+(2*RL1*MVDCFW-1)/8
        CALL GWPTNI (CLPGON, IDPGON, NBYTES, RERR)
        IF (RERR .NE. 0) RETURN
C
C  Put out first points array.
C
C  Truncate points to limits of NDC unit square, convert to VDC,
C  and store in WPXPY.
C
        DO 30 IX=1,RL2
          INDX1 = 2*IX-1
          INDX2 = INDX1+1
          WPXPY(INDX1) = MXOFF+INT(REAL(MXSCAL)*
     -                   (MAX(0.,MIN(1.0,RX(IX)))))
          WPXPY(INDX2) = MYOFF+INT(REAL(MYSCAL)*
     -                   (MAX(0.,MIN(1.0,RY(IX)))))
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
        DO 40 IX=1,RL2
          INDX1 = 2*IX-1
          INDX2 = INDX1+1
          WPXPY(INDX1) = MXOFF+INT(REAL(MXSCAL)*
     -                   (MAX(0.,MIN(1.0,RX(IX)))))
          WPXPY(INDX2) = MYOFF+INT(REAL(MYSCAL)*
     -                   (MAX(0.,MIN(1.0,RY(IX)))))
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
