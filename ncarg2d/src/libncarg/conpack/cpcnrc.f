C
C $Id: cpcnrc.f,v 1.10 1995-04-26 22:44:36 kennison Exp $
C
      SUBROUTINE CPCNRC (ZDAT,KZDT,MZDT,NZDT,FLOW,FHGH,FINC,KSET,NHGH,
     +                   NDSH)
C
      DIMENSION ZDAT(KZDT,*)
C
C This routine simulates the old routine CONREC.
C
C Define some needed dimensions.
C
      PARAMETER (LRWK=5000,LIWK=2000,LAMA=12000,LOCV=10)
C
C Define required workspace arrays.
C
      DIMENSION RWRK(LRWK),IWRK(LIWK),IAMA(LAMA)
C
C Define a character variable to use for point-value labelling.
C
      CHARACTER*(LOCV) CROZ
C
C Declare the contour-line drawing routine.
C
      EXTERNAL CPDRPL
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CPCNRC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Turn off clipping, so that the informational label won't disappear.
C
      CALL GSCLIP (0)
C
C Copy the argument KSET to an internal variable NSET, limiting it to
C the range from -1 to +1.  Note: if the absolute value of KSET is
C greater than 1, saving and restoring of the SET call is not done.
C
      NSET=MOD(MOD(KSET+1,3)+3,3)-1
C
C If the absolute value of KSET is less than or equal to 1, save the
C current SET call.
C
      IF (ABS(KSET).LE.1) THEN
        CALL GETSET (SVPL,SVPR,SVPB,SVPT,SWDL,SWDR,SWDB,SWDT,LLFS)
        IF (ICFELL('CPCNRC',2).NE.0) RETURN
      END IF
C
C Arrange for the selection of contour levels as desired by the user.
C
      CALL CPSETR ('CMN - CONTOUR MINIMUM',1.)
      IF (ICFELL('CPCNRC',3).NE.0) RETURN
      CALL CPSETR ('CMX - CONTOUR MAXIMUM',0.)
      IF (ICFELL('CPCNRC',4).NE.0) RETURN
C
      IF (FINC.LT.0.) THEN
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',MAX(1,INT(-FINC)))
        IF (ICFELL('CPCNRC',5).NE.0) RETURN
        CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',0.)
        IF (ICFELL('CPCNRC',6).NE.0) RETURN
      ELSE IF (FINC.EQ.0.) THEN
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',16)
        IF (ICFELL('CPCNRC',7).NE.0) RETURN
        CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',0.)
        IF (ICFELL('CPCNRC',8).NE.0) RETURN
      ELSE
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',1)
        IF (ICFELL('CPCNRC',9).NE.0) RETURN
        CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',FINC)
        IF (ICFELL('CPCNRC',10).NE.0) RETURN
        IF (FLOW.LT.FHGH) THEN
          CALL CPSETR ('CMN - CONTOUR MINIMUM',FLOW)
          IF (ICFELL('CPCNRC',11).NE.0) RETURN
          CALL CPSETR ('CMX - CONTOUR MAXIMUM',FHGH)
          IF (ICFELL('CPCNRC',12).NE.0) RETURN
        END IF
      END IF
C
C Set up the desired mapping of output onto the plotter frame.
C
      IF (NSET.LT.0) THEN
        CALL CPSETI ('SET - DO-SET-CALL FLAG',1)
        IF (ICFELL('CPCNRC',13).NE.0) RETURN
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        IF (ICFELL('CPCNRC',14).NE.0) RETURN
        CALL CPSETR ('VPL - VIEWPORT LEFT EDGE',XVPL)
        IF (ICFELL('CPCNRC',15).NE.0) RETURN
        CALL CPSETR ('VPR - VIEWPORT RIGHT EDGE',XVPR)
        IF (ICFELL('CPCNRC',16).NE.0) RETURN
        CALL CPSETR ('VPB - VIEWPORT BOTTOM EDGE',YVPB)
        IF (ICFELL('CPCNRC',17).NE.0) RETURN
        CALL CPSETR ('VPT - VIEWPORT TOP EDGE',YVPT)
        IF (ICFELL('CPCNRC',18).NE.0) RETURN
        CALL CPSETI ('VPS - VIEWPORT SHAPE',0)
        IF (ICFELL('CPCNRC',19).NE.0) RETURN
      ELSE IF (NSET.EQ.0) THEN
        CALL CPSETI ('SET - DO-SET-CALL FLAG',1)
        IF (ICFELL('CPCNRC',20).NE.0) RETURN
        CALL CPSETR ('VPL - VIEWPORT LEFT EDGE',.05)
        IF (ICFELL('CPCNRC',21).NE.0) RETURN
        CALL CPSETR ('VPR - VIEWPORT RIGHT EDGE',.95)
        IF (ICFELL('CPCNRC',22).NE.0) RETURN
        CALL CPSETR ('VPB - VIEWPORT BOTTOM EDGE',.05)
        IF (ICFELL('CPCNRC',23).NE.0) RETURN
        CALL CPSETR ('VPT - VIEWPORT TOP EDGE',.95)
        IF (ICFELL('CPCNRC',24).NE.0) RETURN
        CALL CPSETI ('VPS - VIEWPORT SHAPE',4)
        IF (ICFELL('CPCNRC',25).NE.0) RETURN
      ELSE
        CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
        IF (ICFELL('CPCNRC',26).NE.0) RETURN
      END IF
C
C Decide what dash pattern to use.
C
      IDSH=ABS(NDSH)
      IF (IDSH.EQ.0.OR.IDSH.EQ.1.OR.IDSH.EQ.1023) THEN
        IDSH=IOR(ISHIFT(32767,1),1)
      ELSE
        IDSH=IOR(ISHIFT(IDSH,6),IAND(ISHIFT(IDSH,-4),63))
      END IF
C
C Decide whether to label highs and lows or not.
C
      IF (NHGH.EQ.0) THEN
        CALL CPSETC ('HLT - HIGH/LOW LABEL TEXT',
     +               'H:B:$ZDV$:E:''L:B:$ZDV$:E:')
        IF (ICFELL('CPCNRC',27).NE.0) RETURN
      ELSE
        CALL CPSETC ('HLT - HIGH/LOW LABEL TEXT',' ')
        IF (ICFELL('CPCNRC',28).NE.0) RETURN
      END IF
C
C Initialize CONPACK and give it all array dimensions.
C
      CALL CPRECT (ZDAT,KZDT,MZDT,NZDT,RWRK,LRWK,IWRK,LIWK)
      IF (ICFELL('CPCNRC',29).NE.0) RETURN
C
C If the field was constant, skip some of the following code.
C
      CALL CPGETI ('CFF - CONSTANT FIELD FLAG',ICFF)
      IF (ICFELL('CPCNRC',30).NE.0) RETURN
C
      IF (ICFF.EQ.0) THEN
C
C Pick contour levels.
C
        CALL CPPKCL (ZDAT,RWRK,IWRK)
        IF (ICFELL('CPCNRC',31).NE.0) RETURN
C
C Retrieve the contour levels selected, one at a time.  Discard levels
C which are outside the range, if any, specified by the user-supplied
C values of FLOW and FHGH, and move the parameters for all remaining
C levels to the beginning of the parameter arrays.  Set dash patterns
C for all levels.  The value of 'CIU' must be saved for possible
C restoration if it gets clobbered as a side effect of setting contour
C level 1.
C
        CALL CPGETR ('CIU - CONTOUR INTERVAL USED',CINU)
        IF (ICFELL('CPCNRC',32).NE.0) RETURN
C
        CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLO)
        IF (ICFELL('CPCNRC',33).NE.0) RETURN
        NCLN=0
C
        DO 10001 ICLO=1,NCLO
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLO)
          IF (ICFELL('CPCNRC',34).NE.0) RETURN
          CALL CPGETR ('CLV - CONTOUR LEVEL',CLEV)
          IF (ICFELL('CPCNRC',35).NE.0) RETURN
          IF (FLOW.GE.FHGH.OR.(CLEV.GE.FLOW-.001*CINU.AND.
     +                         CLEV.LE.FHGH+.001*CINU)) THEN
            NCLN=NCLN+1
            IF (NCLN.NE.ICLO) THEN
              CALL CPGETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
              IF (ICFELL('CPCNRC',36).NE.0) RETURN
              CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',NCLN)
              IF (ICFELL('CPCNRC',37).NE.0) RETURN
              CALL CPSETR ('CLV - CONTOUR LEVEL',CLEV)
              IF (ICFELL('CPCNRC',38).NE.0) RETURN
              CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
              IF (ICFELL('CPCNRC',39).NE.0) RETURN
              CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',NCLN+1)
              IF (ICFELL('CPCNRC',40).NE.0) RETURN
              CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',NCLN)
              IF (ICFELL('CPCNRC',41).NE.0) RETURN
              CALL CPSETI ('CLC - CONTOUR LINE COLOR INDEX',-1)
              IF (ICFELL('CPCNRC',42).NE.0) RETURN
              CALL CPSETC ('CLD - CONTOUR LINE DASH PATTERN',
     +                                             '$$$$$$$$$$$$$$$$')
              IF (ICFELL('CPCNRC',43).NE.0) RETURN
              CALL CPSETI ('CLL - CONTOUR LINE LINE WIDTH',-1)
              IF (ICFELL('CPCNRC',44).NE.0) RETURN
              CALL CPSETI ('LLC - LINE LABEL COLOR INDEX',-1)
              IF (ICFELL('CPCNRC',45).NE.0) RETURN
              CALL CPSETC ('LLT - LINE LABEL TEXT',' ')
              IF (ICFELL('CPCNRC',46).NE.0) RETURN
            END IF
          END IF
          IF (NDSH.GT.0.OR.(NDSH.LT.0..AND.CLEV.LT.0.)) THEN
            CALL CPSETI ('CLD - CONTOUR LINE DASH PATTERN',IDSH)
            IF (ICFELL('CPCNRC',47).NE.0) RETURN
          END IF
10001   CONTINUE
C
C If the number of contour levels decreased, reset parameters affected.
C
        IF (NCLN.LT.NCLO) THEN
          CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLN)
          IF (ICFELL('CPCNRC',48).NE.0) RETURN
          CALL CPSETR ('CIU - CONTOUR INTERVAL USED',CINU)
          IF (ICFELL('CPCNRC',49).NE.0) RETURN
        END IF
C
      END IF
C
C If requested, put out a simple background.
C
      IF (NSET.EQ.0) THEN
        CALL CPBACK (ZDAT,RWRK,IWRK)
        IF (ICFELL('CPCNRC',50).NE.0) RETURN
      END IF
C
C See how the user has chosen to position contour levels.
C
      CALL CPGETI ('LLP - LINE LABEL POSITIONING FLAG',LLPF)
      IF (ICFELL('CPCNRC',51).NE.0) RETURN
C
C Draw the contour lines, masking them if necessary.
C
      IF (LLPF.LE.1) THEN
        CALL CPCLDR (ZDAT,RWRK,IWRK)
        IF (ICFELL('CPCNRC',52).NE.0) RETURN
      ELSE
        CALL ARINAM (IAMA,LAMA)
        IF (ICFELL('CPCNRC',53).NE.0) RETURN
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
        IF (ICFELL('CPCNRC',54).NE.0) RETURN
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,CPDRPL)
        IF (ICFELL('CPCNRC',55).NE.0) RETURN
      END IF
C
C Plot labels.
C
      CALL CPLBDR (ZDAT,RWRK,IWRK)
      IF (ICFELL('CPCNRC',56).NE.0) RETURN
C
C If requested, label every point on the grid.
C
      IF (NHGH.GT.0) THEN
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        IF (ICFELL('CPCNRC',57).NE.0) RETURN
        CALL CPGETR ('CWM - CHARACTER WIDTH MULTIPLIER',CHWM)
        IF (ICFELL('CPCNRC',58).NE.0) RETURN
        CALL CPGETR ('HLA - HIGH/LOW LABEL ANGLE',ANGD)
        IF (ICFELL('CPCNRC',59).NE.0) RETURN
        CALL CPGETR ('HLS - HIGH/LOW LABEL SIZE',SIZE)
        IF (ICFELL('CPCNRC',60).NE.0) RETURN
        CALL CPGETI ('MAP - MAPPING FLAG',IMAP)
        IF (ICFELL('CPCNRC',61).NE.0) RETURN
        CALL CPGETR ('ORV - OUT-OF-RANGE VALUE',ORVA)
        IF (ICFELL('CPCNRC',62).NE.0) RETURN
        CALL CPGETR ('SPV - SPECIAL VALUE',SPVA)
        IF (ICFELL('CPCNRC',63).NE.0) RETURN
        CALL CPGETR ('XC1 - X COORDINATE AT I = 1',XCA1)
        IF (ICFELL('CPCNRC',64).NE.0) RETURN
        CALL CPGETR ('XCM - X COORDINATE AT I = M',XCAM)
        IF (ICFELL('CPCNRC',65).NE.0) RETURN
        CALL CPGETR ('YC1 - Y COORDINATE AT J = 1',YCA1)
        IF (ICFELL('CPCNRC',66).NE.0) RETURN
        CALL CPGETR ('YCN - Y COORDINATE AT J = N',YCAN)
        IF (ICFELL('CPCNRC',67).NE.0) RETURN
        SIZE=(XVPR-XVPL)*CHWM*SIZE
        IF (XCA1.EQ.XCAM) THEN
          XCA1=1.
          XCAM=REAL(MZDT)
        END IF
        IF (YCA1.EQ.YCAN) THEN
          YCA1=1.
          YCAN=REAL(NZDT)
        END IF
        DO 10002 J=1,NZDT
          YPOS=YCA1+REAL(J-1)*(YCAN-YCA1)/REAL(NZDT-1)
          DO 10003 I=1,MZDT
            XPOS=XCA1+REAL(I-1)*(XCAM-XCA1)/REAL(MZDT-1)
            IF (SPVA.EQ.0..OR.ZDAT(I,J).NE.SPVA) THEN
              CALL CPSETR ('ZDV - Z DATA VALUE',ZDAT(I,J))
              IF (ICFELL('CPCNRC',68).NE.0) RETURN
              CALL CPGETC ('ZDV - Z DATA VALUE',CROZ)
              IF (ICFELL('CPCNRC',69).NE.0) RETURN
              DO 10004 K=LOCV,2,-1
                IF (CROZ(K:K).NE.' ') THEN
                  LCRZ=K
                  GO TO 101
                END IF
10004         CONTINUE
              LCRZ=1
  101         IF (IMAP.EQ.0) THEN
                CALL PLCHHQ (XPOS,YPOS,CROZ(1:LCRZ),SIZE,ANGD,0.)
                IF (ICFELL('CPCNRC',70).NE.0) RETURN
              ELSE
                CALL HLUCPMPXY (IMAP,XPOS,YPOS,XMPD,YMPD)
                IF (ICFELL('CPCNRC',71).NE.0) RETURN
                IF (ORVA.EQ.0..OR.XMPD.NE.ORVA) THEN
                  CALL PLCHHQ (XMPD,YMPD,CROZ(1:LCRZ),SIZE,ANGD,0.)
                  IF (ICFELL('CPCNRC',72).NE.0) RETURN
                END IF
              END IF
            END IF
10003     CONTINUE
10002   CONTINUE
      END IF
C
C Done.  Restore the original SET call and return to the caller.
C
      IF (ABS(KSET).LE.1) THEN
        CALL SET (SVPL,SVPR,SVPB,SVPT,SWDL,SWDR,SWDB,SWDT,LLFS)
        IF (ICFELL('CPCNRC',73).NE.0) RETURN
      END IF
C
      RETURN
C
      END
