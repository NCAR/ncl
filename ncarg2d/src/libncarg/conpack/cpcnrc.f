C
C $Id: cpcnrc.f,v 1.4 1993-02-02 16:46:13 kennison Exp $
C
C
C-----------------------------------------------------------------------
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
      IF (ABS(KSET).LE.1)
     +CALL GETSET (SVPL,SVPR,SVPB,SVPT,SWDL,SWDR,SWDB,SWDT,LLFS)
C
C Arrange for the selection of contour levels as desired by the user.
C
      CALL CPSETR ('CMN - CONTOUR MINIMUM',1.)
      CALL CPSETR ('CMX - CONTOUR MAXIMUM',0.)
C
      IF (FINC.LT.0.) THEN
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',MAX(1,INT(-FINC)))
        CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',0.)
      ELSE IF (FINC.EQ.0.) THEN
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',16)
        CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',0.)
      ELSE
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',1)
        CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',FINC)
        IF (FLOW.LT.FHGH) THEN
          CALL CPSETR ('CMN - CONTOUR MINIMUM',FLOW)
          CALL CPSETR ('CMX - CONTOUR MAXIMUM',FHGH)
        END IF
      END IF
C
C Set up the desired mapping of output onto the plotter frame.
C
      IF (NSET.LT.0) THEN
        CALL CPSETI ('SET - DO-SET-CALL FLAG',1)
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        CALL CPSETR ('VPL - VIEWPORT LEFT EDGE',XVPL)
        CALL CPSETR ('VPR - VIEWPORT RIGHT EDGE',XVPR)
        CALL CPSETR ('VPB - VIEWPORT BOTTOM EDGE',YVPB)
        CALL CPSETR ('VPT - VIEWPORT TOP EDGE',YVPT)
        CALL CPSETI ('VPS - VIEWPORT SHAPE',0)
      ELSE IF (NSET.EQ.0) THEN
        CALL CPSETI ('SET - DO-SET-CALL FLAG',1)
        CALL CPSETR ('VPL - VIEWPORT LEFT EDGE',.05)
        CALL CPSETR ('VPR - VIEWPORT RIGHT EDGE',.95)
        CALL CPSETR ('VPB - VIEWPORT BOTTOM EDGE',.05)
        CALL CPSETR ('VPT - VIEWPORT TOP EDGE',.95)
        CALL CPSETI ('VPS - VIEWPORT SHAPE',4)
      ELSE
        CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
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
      ELSE
        CALL CPSETC ('HLT - HIGH/LOW LABEL TEXT',' ')
      END IF
C
C Initialize CONPACK and give it all array dimensions.
C
      CALL CPRECT (ZDAT,KZDT,MZDT,NZDT,RWRK,LRWK,IWRK,LIWK)
C
C If the field was constant, skip some of the following code.
C
      CALL CPGETI ('CFF - CONSTANT FIELD FLAG',ICFF)
C
      IF (ICFF.EQ.0) THEN
C
C Pick contour levels.
C
        CALL CPPKCL (ZDAT,RWRK,IWRK)
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
C
        CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLO)
        NCLN=0
C
        DO 10001 ICLO=1,NCLO
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLO)
          CALL CPGETR ('CLV - CONTOUR LEVEL',CLEV)
          IF (FLOW.GE.FHGH.OR.(CLEV.GE.FLOW-.001*CINU.AND.
     +                         CLEV.LE.FHGH+.001*CINU)) THEN
            NCLN=NCLN+1
            IF (NCLN.NE.ICLO) THEN
              CALL CPGETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
              CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',NCLN)
              CALL CPSETR ('CLV - CONTOUR LEVEL',CLEV)
              CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
              CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',NCLN+1)
              CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',NCLN)
              CALL CPSETI ('CLC - CONTOUR LINE COLOR INDEX',-1)
              CALL CPSETC ('CLD - CONTOUR LINE DASH PATTERN',
     +                                             '$$$$$$$$$$$$$$$$')
              CALL CPSETI ('CLL - CONTOUR LINE LINE WIDTH',-1)
              CALL CPSETI ('LLC - LINE LABEL COLOR INDEX',-1)
              CALL CPSETC ('LLT - LINE LABEL TEXT',' ')
            END IF
          END IF
          IF (NDSH.GT.0.OR.(NDSH.LT.0..AND.CLEV.LT.0.))
     +            CALL CPSETI ('CLD - CONTOUR LINE DASH PATTERN',IDSH)
10001   CONTINUE
C
C If the number of contour levels decreased, reset parameters affected.
C
        IF (NCLN.LT.NCLO) THEN
          CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLN)
          CALL CPSETR ('CIU - CONTOUR INTERVAL USED',CINU)
        END IF
C
      END IF
C
C If requested, put out a simple background.
C
      IF (NSET.EQ.0) CALL CPBACK (ZDAT,RWRK,IWRK)
C
C See how the user has chosen to position contour levels.
C
      CALL CPGETI ('LLP - LINE LABEL POSITIONING FLAG',LLPF)
C
C Draw the contour lines, masking them if necessary.
C
      IF (LLPF.LE.1) THEN
        CALL CPCLDR (ZDAT,RWRK,IWRK)
      ELSE
        CALL ARINAM (IAMA,LAMA)
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,CPDRPL)
      END IF
C
C Plot labels.
C
      CALL CPLBDR (ZDAT,RWRK,IWRK)
C
C If requested, label every point on the grid.
C
      IF (NHGH.GT.0) THEN
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        CALL CPGETR ('CWM - CHARACTER WIDTH MULTIPLIER',CHWM)
        CALL CPGETR ('HLA - HIGH/LOW LABEL ANGLE',ANGD)
        CALL CPGETR ('HLS - HIGH/LOW LABEL SIZE',SIZE)
        CALL CPGETI ('MAP - MAPPING FLAG',IMAP)
        CALL CPGETR ('ORV - OUT-OF-RANGE VALUE',ORVA)
        CALL CPGETR ('SPV - SPECIAL VALUE',SPVA)
        CALL CPGETR ('XC1 - X COORDINATE AT I = 1',XCA1)
        CALL CPGETR ('XCM - X COORDINATE AT I = M',XCAM)
        CALL CPGETR ('YC1 - Y COORDINATE AT J = 1',YCA1)
        CALL CPGETR ('YCN - Y COORDINATE AT J = N',YCAN)
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
              CALL CPGETC ('ZDV - Z DATA VALUE',CROZ)
              DO 10004 K=LOCV,2,-1
                IF (CROZ(K:K).NE.' ') THEN
                  LCRZ=K
                  GO TO 101
                END IF
10004         CONTINUE
              LCRZ=1
  101         IF (IMAP.EQ.0) THEN
                CALL PLCHHQ (XPOS,YPOS,CROZ(1:LCRZ),SIZE,ANGD,0.)
              ELSE
                CALL CPMPXY (IMAP,XPOS,YPOS,XMPD,YMPD)
                IF (ORVA.EQ.0..OR.XMPD.NE.ORVA)
     +               CALL PLCHHQ (XMPD,YMPD,CROZ(1:LCRZ),SIZE,ANGD,0.)
              END IF
            END IF
10003     CONTINUE
10002   CONTINUE
      END IF
C
C Done.  Restore the original SET call and return to the caller.
C
      IF (ABS(KSET).LE.1)
     +CALL SET (SVPL,SVPR,SVPB,SVPT,SWDL,SWDR,SWDB,SWDT,LLFS)
C
      RETURN
C
      END
