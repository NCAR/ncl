C
C	$Id: giitm.f,v 1.2 1993-01-09 01:58:49 fred Exp $
C
      SUBROUTINE GIITM (TYPE,LDR,DATREC)
C
C  INTERPRET ITEM
C
      INTEGER EIITM
      PARAMETER (EIITM=104)
C
      include 'gkscom.h'
C
      INTEGER TYPE,LDR
      CHARACTER*80 DATREC(LDR)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,EIITM,IER)
      IF (IER .NE. 0) RETURN
C
C  Set function code and put out WKID.
C
      FCODE = 104
      CONT  = 0
      CALL GZROI(0)
      IL1 = 1
      IL2 = 1
      ID(1) = TYPE
      IF (LDR .GE. 1) THEN
        IF (LDR .EQ. 1) THEN
          CONT = 0
          STRL1 = 80
          STRL2 = 80
          STR(1:80) = DATREC(1)
          CALL GZTOWK
          IF (RERR.NE.0) THEN
            ERS = 1
            CALL GERHND(RERR,EIITM,ERF)
            ERS = 0
            RETURN
          ENDIF
        ELSE
C
C  Send over the data record 80 characters at a time.
C
          CONT = 1
          STRL1 = 80*LDR
          STRL2 = 80
          LDRM1 = LDR-1
          DO 200 I=1,LDRM1
            IF (I .GT. 1) IL2 = 0
            STR(1:80) = DATREC(I)
            CALL GZTOWK
            IF (RERR.NE.0) THEN
              ERS = 1
              CALL GERHND(RERR,EIITM,ERF)
              ERS = 0
              RETURN
            ENDIF
  200     CONTINUE
          CONT = 0
          STR(1:80) = DATREC(LDR)
          CALL GZTOWK
          IF (RERR.NE.0) THEN
            ERS = 1
            CALL GERHND(RERR,EIITM,ERF)
            ERS = 0
            RETURN
          ENDIF
        ENDIF
      ELSE
        CONT = 0
        CALL GZTOWK
        IF (RERR.NE.0) THEN
          ERS = 1
          CALL GERHND(RERR,EIITM,ERF)
          ERS = 0
          RETURN
        ENDIF
      ENDIF
      RETURN
      END
