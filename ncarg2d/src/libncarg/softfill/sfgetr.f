C
C	$Id: sfgetr.f,v 1.3 1992-09-04 20:46:40 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SFGETR (CNP,RVP)
C
      CHARACTER*(*) CNP
C
C This subroutine is called to retrieve the real value of a specified
C parameter.
C
C CNP is the name of the parameter whose value is to be retrieved.
C
C RVP is a real variable in which the desired value is to be returned
C by SFGETR.
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
      CHARACTER*48 CTM
C
C Check for a parameter name that is too short.
C
      IF (.NOT.(LEN(CNP).LT.2)) GO TO 10001
      CTM(1:46)='SFGETI OR SFGETR - PARAMETER NAME TOO SHORT - '
      CTM(47:46+LEN(CNP))=CNP
      CALL SETER (CTM(1:46+LEN(CNP)),1,2)
      STOP
10001 CONTINUE
C
C Get the appropriate parameter value.
C
      IF (.NOT.(CNP(1:2).EQ.'AN'.OR.CNP(1:2).EQ.'an')) GO TO 10002
      RVP=AID
      GO TO 10003
10002 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'CH'.OR.CNP(1:2).EQ.'ch')) GO TO 10004
      RVP=REAL(LCH)
      GO TO 10003
10004 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'DO'.OR.CNP(1:2).EQ.'do')) GO TO 10005
      RVP=REAL(LPA)
      GO TO 10003
10005 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'SP'.OR.CNP(1:2).EQ.'sp')) GO TO 10006
      RVP=DBL
      GO TO 10003
10006 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'TY'.OR.CNP(1:2).EQ.'ty')) GO TO 10007
      RVP=REAL(ITY)
      GO TO 10003
10007 CONTINUE
      CTM(1:46)='SFGETI OR SFGETR - PARAMETER NAME NOT KNOWN - '
      CTM(47:48)=CNP(1:2)
      CALL SETER (CTM(1:48),2,2)
      STOP
10003 CONTINUE
C
C Done.
C
      RETURN
C
      END
