C
C	$Id: gurec.f,v 1.5 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GUREC(LDR,DATREC,IIL,IRL,ISL,ERRIND,
     +                 IL,IA,RL,RA,SL,LSTR,STR)
C
C  UNPACK DATA RECORD
C
      INTEGER EUREC
      PARAMETER (EUREC=108)
      DIMENSION IA(*),RA(*),LSTR(*)
      INTEGER RL,SL,ERRIND
      CHARACTER*80 DATREC(LDR)
      CHARACTER*(*) STR(*)
      DATA NCE/1/
C
C  This subroutine unpacks the data records that were packed using
C  the subroutine GPREC.
C
C  Read in  IL,RL,SL,NTL,NIL,NRL,NSL,MAXSL:
C
C  IL    -- Number of integers to be read
C  RL    -- Number of reals to be read
C  SL    -- Number of distinct character variables to be read
C  NTL   -- Number of data records used to store LSTR
C  NIL   -- Number of data records used to store the integers
C  NRL   -- Number of data records used to store the reals
C  NSL   -- Number of data records used to store the character 
C           variables
C  MAXSL -- Maximum character length of any of the character
C           variables
C
      READ (DATREC(1),501) IL,RL,SL,NTL,NIL,NRL,NSL,MAXSL
C
C  Do parameter checking
C
      ILDR = NTL+NIL+NRL+NSL+1
      IF (IIL.LT.IL .OR. IRL.LT.RL .OR. 
     +    ISL.LT.SL .OR. LDR.LT.ILDR) THEN
        ERRIND = 2001
        RETURN
      ENDIF
C
C  Read the LSTR array if there are any characters
C
      NPER = 8
      IF (SL .GT. 0) THEN
        INDX1 = SL/NPER
        INDX2 = MOD(SL,NPER)
        IF (INDX1 .EQ. 0) THEN
          READ (DATREC(2),501) (LSTR(LL),LL=1,SL)
        ELSE
          DO 200 I=1,INDX1
            IPNT1 = NPER*(I-1)+1
            IPNT2 = NPER*I
            READ (DATREC(I+1),501) (LSTR(LL),LL=IPNT1,IPNT2)
  200     CONTINUE
          IF (INDX2 .NE. 0) THEN
            NPNT1 = NPER*INDX1+1
            NPNT2 = NPER*INDX1+INDX2
            READ (DATREC(INDX1+2),501) (LSTR(LL),LL=NPNT1,NPNT2)
          ENDIF
        ENDIF
      ENDIF
C
C  Read the integer array
C
      NPER = 4
      IF (IL .GT. 0) THEN
        INDX1 = IL/NPER
        INDX2 = MOD(IL,NPER)
        IF (INDX1 .EQ. 0) THEN
          READ (DATREC(NTL+2),502) (IA(LL),LL=1,IL)
        ELSE
          DO 201 I=1,INDX1
            IPNT1 = NPER*(I-1)+1
            IPNT2 = NPER*I
            READ (DATREC(NTL+I+1),502) (IA(LL),LL=IPNT1,IPNT2)
  201     CONTINUE
          IF (INDX2 .NE. 0) THEN
            NPNT1 = NPER*INDX1+1
            NPNT2 = NPER*INDX1+INDX2
            READ (DATREC(NTL+INDX1+2),502) (IA(LL),LL=NPNT1,NPNT2)
          ENDIF
        ENDIF
      ENDIF
C
C  Read the real array.
C
      NPER = 4
      IF (RL .GT. 0) THEN
        INDX1 = RL/NPER
        INDX2 = MOD(RL,NPER)
        IF (INDX1 .EQ. 0) THEN
          READ (DATREC(NTL+NIL+2),503) (RA(LL),LL=1,RL)
        ELSE
          DO 202 I=1,INDX1
            IPNT1 = NPER*(I-1)+1
            IPNT2 = NPER*I
            READ (DATREC(NTL+NIL+I+1),503) (RA(LL),LL=IPNT1,IPNT2)
  202     CONTINUE
          IF (INDX2 .NE. 0) THEN
            NPNT1 = NPER*INDX1+1
            NPNT2 = NPER*INDX1+INDX2
            READ (DATREC(NTL+NIL+INDX1+2),503) (RA(LL),LL=NPNT1,NPNT2)
          ENDIF
        ENDIF
      ENDIF
C
C  Read the character arrays
C
      IF (MAXSL .NE. 0) THEN
        NCE = 80/MAXSL
        IF (NCE.LE.1) THEN
          NCE = 1
        ENDIF
      ENDIF
      NPER = NCE
      IF (SL .GT. 0) THEN
        IF (NPER .GT. 1) THEN
          INDX1 = SL/NPER
          INDX2 = MOD(SL,NPER)
          IF (INDX1 .EQ. 0) THEN
            DO 203 I=1,SL
              IPNT1 = (I-1)*MAXSL+1
              IPNT2 = IPNT1+LSTR(I)-1
              STR(I)(1:LSTR(I)) = DATREC(NTL+NIL+NRL+2)(IPNT1:IPNT2)
  203       CONTINUE
          ELSE
            DO 204 I=1,INDX1
              DO 205 J=1,NCE
                JPNT1 = (J-1)*MAXSL+1
                NDX = NPER*(I-1)+J
                JPNT2 = JPNT1+LSTR(NDX)-1
                STR(NDX) (1:LSTR(NDX)) = 
     ~                 DATREC(NTL+NIL+NRL+I+1)(JPNT1:JPNT2)
  205         CONTINUE
  204       CONTINUE
            IF (INDX2 .NE. 0) THEN
              DO 206 J=1,INDX2
                JPNT1 = (J-1)*MAXSL+1
                NDX = NPER*INDX1+J
                JPNT2 = JPNT1+LSTR(NDX)-1
                STR(NDX) (1:LSTR(NDX)) = 
     +                  DATREC(NTL+NIL+NRL+INDX1+2)(JPNT1:JPNT2)
  206         CONTINUE
            ENDIF
          ENDIF
        ELSE
          JNDX = 0
          DO 207 I=1,SL
            INDX1 = LSTR(I)/80
            INDX2 = MOD(LSTR(I),80)
            IF (INDX1 .EQ. 0) THEN
              JNDX = JNDX+1
              STR(I)(1:INDX2) = DATREC(NTL+NIL+NRL+JNDX+1)
            ELSE
              DO 208 J=1,INDX1
                JNDX = JNDX+1
                STR(I) (80*(J-1)+1:80*J) = DATREC(NTL+NIL+NRL+JNDX+1)
  208         CONTINUE
              IF (INDX2 .NE. 0) THEN
                JNDX = JNDX+1
                STR(I) (80*INDX1+1:80*INDX1+INDX2) =
     +                 DATREC(NTL+NIL+NRL+JNDX+1) (1:INDX2)
              ENDIF
            ENDIF
  207     CONTINUE
        ENDIF
      ENDIF
C
  501 FORMAT(8I10)
  502 FORMAT(4I20)
  503 FORMAT(4E20.13)
C
      RETURN
      END
