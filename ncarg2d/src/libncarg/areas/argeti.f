C
C The subroutine ARGETI.
C --- ---------- -------
C
      SUBROUTINE ARGETI (IPN,IVL)
C
      CHARACTER*(*) IPN
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C IPN is the name of the parameter whose value is to be retrieved.
C
C IVL is an integer variable in which the desired value is to be
C returned by ARGETI.
C
C Declare the AREAS common block.
C
C
C ARCOMN contains variables which are used by all the AREAS routines.
C
      COMMON /ARCOMN/ IAD,IAU,ILC,RLC,ILM,RLM,ILP,RLP,IBS,RBS,DBS,IDB,
     +                IDC,IDI
      SAVE   /ARCOMN/
C
C Declare the BLOCK DATA routine external, which should force it to
C load from a binary library.
C
      EXTERNAL ARBLDA
C
C Define a character temporary to hold an error message.
C
      CHARACTER*38 CTM
C
C Check for a parameter name that is too short.
C
      IF (.NOT.(LEN(IPN).LT.2)) GO TO 10001
        CTM(1:36)='ARGETI - PARAMETER NAME TOO SHORT - '
        CTM(37:36+LEN(IPN))=IPN
        CALL SETER (CTM(1:36+LEN(IPN)),1,2)
        STOP
10001 CONTINUE
C
C Get the appropriate parameter value.
C
      IF (.NOT.(IPN(1:2).EQ.'AT'.OR.IPN(1:2).EQ.'at')) GO TO 10002
        IF (IAU.EQ.0) CALL ARINIT
        IVL=IAU
        IF (IVL.GE.3) IVL=IBS
      GO TO 10003
10002 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'DB'.OR.IPN(1:2).EQ.'db')) GO TO 10004
        IVL=IDB
      GO TO 10003
10004 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'DC'.OR.IPN(1:2).EQ.'dc')) GO TO 10005
        IVL=IDC
      GO TO 10003
10005 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'DI'.OR.IPN(1:2).EQ.'di')) GO TO 10006
        IVL=IDI
      GO TO 10003
10006 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'LC'.OR.IPN(1:2).EQ.'lc')) GO TO 10007
        IVL=ILC
      GO TO 10003
10007 CONTINUE
        CTM(1:36)='ARGETI - PARAMETER NAME NOT KNOWN - '
        CTM(37:38)=IPN(1:2)
        CALL SETER (CTM(1:38),2,2)
        STOP
10003 CONTINUE
C
C Done.
C
      RETURN
C
      END
