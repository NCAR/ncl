C
C	$Id: sfsetc.f,v 1.3 1992-09-04 20:46:42 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SFSETC (CNP,CVP)
C
      CHARACTER*(*) CNP,CVP
C
C This subroutine is called to give a character value to a specified
C parameter.
C
C CNP is the name of the parameter whose value is to be set.
C
C CVP is a character variable containing the desired value.
C
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,LCH,LDP(8,8)
C
C Declare the block data routine external to force its loading.
C
      EXTERNAL SFBLDA
C
C Declare a local character variable in which to form an error message.
C
      CHARACTER*38 CTM
C
C Check for a parameter name that is too short.
C
      IF (.NOT.(LEN(CNP).LT.2)) GO TO 10001
      CTM(1:36)='SFSETC - PARAMETER NAME TOO SHORT - '
      CTM(37:36+LEN(CNP))=CNP
      CALL SETER (CTM(1:36+LEN(CNP)),1,2)
      STOP
10001 CONTINUE
C
C Set the appropriate parameter value.
C
      IF (.NOT.(CNP(1:2).EQ.'CH'.OR.CNP(1:2).EQ.'ch')) GO TO 10002
      LCH=ICHAR(CVP)
      GO TO 10003
10002 CONTINUE
      CTM(1:36)='SFSETC - PARAMETER NAME NOT KNOWN - '
      CTM(37:38)=CNP(1:2)
      CALL SETER (CTM(1:38),2,2)
      STOP
10003 CONTINUE
C
C Done.
C
      RETURN
C
      END
