C
C $Id: arseti.f,v 1.4 1993-06-03 22:44:45 kennison Exp $
C
      SUBROUTINE ARSETI (IPN,IVL)
C
      CHARACTER*(*) IPN
C
C This subroutine is called to set the integer value of a specified
C parameter.
C
C IPN is the name of the parameter whose value is to be retrieved.
C
C IVL is an integer variable containing the desired new value.
C
C Declare the AREAS common block.
C
C
C ARCOMN contains variables which are used by all the AREAS routines.
C
      COMMON /ARCOMN/ IAD,IAU,ILC,RLC,ILM,RLM,ILP,RLP,IBS,RBS,DBS,IDB,
     +                IDC,IDI,RLA,RWA,RDI,RSI
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
        CTM(1:36)='ARSETI - PARAMETER NAME TOO SHORT - '
        CTM(37:36+LEN(IPN))=IPN
        CALL SETER (CTM(1:36+LEN(IPN)),1,2)
        STOP
10001 CONTINUE
C
C Set the appropriate parameter value.
C
      IF (.NOT.(IPN(1:2).EQ.'AT'.OR.IPN(1:2).EQ.'at')) GO TO 10002
        IAD=MAX(0,IVL)
        IAU=0
      GO TO 10003
10002 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'DB'.OR.IPN(1:2).EQ.'db')) GO TO 10004
        IDB=IVL
      GO TO 10003
10004 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'DC'.OR.IPN(1:2).EQ.'dc')) GO TO 10005
        IDC=MAX(0,IVL)
      GO TO 10003
10005 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'LC'.OR.IPN(1:2).EQ.'lc')) GO TO 10006
        ILC=MAX(1000,IVL)
        IAU=0
      GO TO 10003
10006 CONTINUE
        CTM(1:36)='ARSETI - PARAMETER NAME NOT KNOWN - '
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
