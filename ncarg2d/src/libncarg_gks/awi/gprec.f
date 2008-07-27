C
C	$Id: gprec.f,v 1.6 2008-07-27 00:20:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GPREC(IL,IA,RL,RA,SL,LSTR,STR,MLDR,ERRIND,LDR,DATREC)
C
C  PACK DATA RECORD
C
      INTEGER EPREC
      PARAMETER (EPREC=107)
      INTEGER IL,IA(*),RL,SL,LSTR(*),MLDR,LDR,ERRIND
      REAL RA(*)
      CHARACTER*(*) STR(*)
      CHARACTER*80  DATREC(LDR)
      DATA NCE/1/
C
C  Determine number of CHARACTER*80 records the packing is going to
C  require:  the integers are packed in I20 format, the reals in
C  E20.13 format, and the characters are packed in packets of
C  N per record, where N is 80 divided by the character string
C  of maximum length.
C
C---------------------------------------------------------------------
C
C  Where:
C    MAXSL = Maximum length of all character strings.
C    NCE   = Number of character entries in each data record.
C    NTL   = Number of data records needed to store the LSTR array.
C    NIL   = Number of data records needed to store INTEGER array.
C    NRL   = Number of data records needed to store the REAL array.
C    NSL   = Number of data records needed to store the CHARACTERs.
C
C  The packed data record structure looks like:
C
C  Data rec number           Stored items
C  ---------------           ------------
C  1                         IL,RL,SL,NTL,NIL,NRL,NSL,MAXSL
C                            (Stored as 8I10)
C  2 TO NTL+1                LSTR array stored as I10 integers
C  NTL+2 TO NTL+NIL+1        INTEGER array stored as I20
C  NTL+NIL+2 TO              REAL array stored as E20.13
C    NTL+NIL+NRL+1
C  NTL+NIL+NRL+2 TO          Characters stored in blocks of NCE
C    NTL+NIL+NRL+NSL+1
C
      NIL = 0
      IF (IL.GT.0) THEN
        NIL = IL/4
        IF (MOD(IL,4).NE.0) THEN
          NIL = NIL+1
        ENDIF
      ENDIF
      NRL = 0
      IF (RL.GT.0) THEN
        NRL = RL/4
        IF (MOD(RL,4).NE.0) THEN
          NRL = NRL+1
        ENDIF
      ENDIF
C
C  Find the maximum length of the character strings.
C
      MAXSL = 0
      IF (SL .GT. 0) THEN
        DO 200 I=1,SL
          MAXSL = MAX(MAXSL,LSTR(I))
  200   CONTINUE
      ENDIF
C
C  Determine the number of character entries to be packed in each
C  data record.
C
      IF (MAXSL.NE.0) THEN
        NCE = 80/MAXSL
        IF (NCE.LE.1) THEN
          NCE = 1
        ENDIF
      ENDIF
C
C  Determine the number of data records needed to store the LSTR
C  array (stored 8I10).
C
      NTL = 0
      IF (SL .GT. 0) THEN
        NPER = 8
        NTL = SL/NPER
        IF (MOD(SL,NPER).NE.0.OR.NTL.EQ.0) THEN
          NTL = NTL+1
        ENDIF
      ENDIF
C
C  Determine the number of records needed to store the characters.
C
      NSL = 0
      IF (SL .GT. 0) THEN
        IF (NCE .GT. 1) THEN
          NSL = SL/NCE
          IF (MOD(SL,NCE).NE.0 .OR. NSL.EQ.0) THEN
            NSL = NSL+1
          ENDIF
        ELSE
          DO 201 I=1,SL
            K = LSTR(I)/80
            IF (MOD(LSTR(I),80).NE.0 .OR. K.EQ.0) THEN
              NSL = NSL+K+1
            ELSE
              NSL = NSL+K
            ENDIF
  201     CONTINUE
        ENDIF
      ENDIF
      LDR = NTL+NIL+NRL+NSL+1
      IF (LDR .GT. MLDR) THEN
        ERRIND = 2001
        RETURN
      ENDIF
C
C  Initialize the data record.
C
      DO 202 I=1,LDR
        DATREC(I) = ' '
  202 CONTINUE
C
C  Pack the data record.
C
      WRITE(DATREC(1),501) IL,RL,SL,NTL,NIL,NRL,NSL,MAXSL
C
C  Write out LSTR array.
C
      NPER = 8
      IF (SL.GT.0) THEN
        INDX1 = SL/NPER
        INDX2 = MOD(SL,NPER)
        IF (INDX1.EQ.0) THEN
          WRITE(DATREC(2),501) (LSTR(LL),LL=1,SL)
        ELSE
          DO 203 I=1,INDX1
            IPNT1 = NPER*(I-1)+1
            IPNT2 = NPER*I
            WRITE(DATREC(I+1),501) (LSTR(LL),LL=IPNT1,IPNT2)
  203     CONTINUE
          IF (INDX2.NE.0) THEN
            NPNT1 = NPER*INDX1+1
            NPNT2 = NPER*INDX1+INDX2
            WRITE(DATREC(INDX1+2),501) (LSTR(LL),LL=NPNT1,NPNT2)
          ENDIF
        ENDIF
      ENDIF
C
C  Write out the integer array.
C
      NPER = 4
      IF (IL.GT.0) THEN
        INDX1 = IL/NPER
        INDX2 = MOD(IL,NPER)
        IF (INDX1.EQ.0) THEN
          WRITE(DATREC(NTL+2),502) (IA(LL),LL=1,IL)
        ELSE
          DO 204 I=1,INDX1
            IPNT1 = NPER*(I-1)+1
            IPNT2 = NPER*I
            WRITE(DATREC(NTL+I+1),502) (IA(LL),LL=IPNT1,IPNT2)
  204     CONTINUE
          IF (INDX2.NE.0) THEN
            NPNT1 = NPER*INDX1+1
            NPNT2 = NPER*INDX1+INDX2
            WRITE(DATREC(NTL+INDX1+2),502) (IA(LL),LL=NPNT1,NPNT2)
          ENDIF
        ENDIF
      ENDIF
C
C  Write out the real array.
C
      NPER = 4
      IF (RL.GT.0) THEN
        INDX1 = RL/NPER
        INDX2 = MOD(RL,NPER)
        IF (INDX1.EQ.0) THEN
          WRITE(DATREC(NTL+NIL+2),503) (RA(LL),LL=1,RL)
        ELSE
          DO 205 I=1,INDX1
            IPNT1 = NPER*(I-1)+1
            IPNT2 = NPER*I
            WRITE(DATREC(NTL+NIL+I+1),503) (RA(LL),LL=IPNT1,IPNT2)
  205     CONTINUE
          IF (INDX2.NE.0) THEN
            NPNT1 = NPER*INDX1+1
            NPNT2 = NPER*INDX1+INDX2
            WRITE(DATREC(NTL+NIL+INDX1+2),503) (RA(LL),LL=NPNT1,NPNT2)
          ENDIF
        ENDIF
      ENDIF
C
C  Write out the character arrays.
C
      NPER = NCE
      IF (SL .GT. 0) THEN
        IF (NPER .GT. 1) THEN
          INDX1 = SL/NPER
          INDX2 = MOD(SL,NPER)
          IF (INDX1 .EQ. 0) THEN
            DO 206 I=1,SL
              IPNT1 = (I-1)*MAXSL+1
              IPNT2 = IPNT1+LSTR(I)-1
              DATREC(NTL+NIL+NRL+2)(IPNT1:IPNT2) = STR(I)
  206       CONTINUE
          ELSE
            DO 207 I=1,INDX1
              DO 208 J=1,NCE
                JPNT1 = (J-1)*MAXSL+1
                JPNT2 = JPNT1+LSTR(NPER*(I-1)+J)-1
                DATREC(NTL+NIL+NRL+I+1)(JPNT1:JPNT2) = 
     +                 STR(NPER*(I-1)+J)       
  208         CONTINUE
  207       CONTINUE
            IF (INDX2 .NE. 0) THEN
              DO 209 J=1,INDX2
                JPNT1 = (J-1)*MAXSL+1
                JPNT2 = JPNT1+LSTR(NPER*INDX1+J)-1
                DATREC(NTL+NIL+NRL+INDX1+2)(JPNT1:JPNT2) = 
     +                 STR(NPER*INDX1+J)     
  209         CONTINUE
            ENDIF
          ENDIF
        ELSE
          JNDX = 0
          DO 210 I=1,SL
            INDX1 = LSTR(I)/80
            INDX2 = MOD(LSTR(I),80)
            IF (INDX1 .EQ. 0) THEN
              JNDX = JNDX+1
              DATREC(NTL+NIL+NRL+JNDX+1) = STR(I)(1:INDX2)
            ELSE
              DO 211 J=1,INDX1
                JNDX = JNDX+1
                DATREC(NTL+NIL+NRL+JNDX+1) = STR(I) (80*(J-1)+1:80*J)
  211         CONTINUE
              IF (INDX2 .NE. 0) THEN
                JNDX = JNDX+1
                DATREC(NTL+NIL+NRL+JNDX+1) = 
     +                 STR(I) (80*INDX1+1:80*INDX1+INDX2)
              ENDIF
            ENDIF
  210     CONTINUE
        ENDIF
      ENDIF
C
  501 FORMAT(8I10)
  502 FORMAT(4I20)
  503 FORMAT(4E20.13)
C
      RETURN
      END
