C
C	$Id: vvsetc.f,v 1.1 1992-10-12 15:32:10 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVSETC (WHCH,CVAL)
C
      CHARACTER*(*) WHCH,CVAL
C
C This subroutine is called to give a specified character value to a
C specified parameter.
C
C WHCH is the name of the parameter whose value is to be set.
C
C CVAL is a character variable containing the new value of the
C parameter.
C
C Declare all the VELVCT common blocks
C
      PARAMETER (IPLVLS = 64)
C
      COMMON /VEC1/   ASH        ,EXT        ,ICTRFG     ,ILAB       ,
     +                IOFFD      ,IOFFM      ,ISX        ,ISY        ,
     +                RMN        ,RMX        ,SIDE       ,SIZE       ,
     +                XLT        ,YBT        ,ZMN        ,ZMX        ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
      COMMON /VEC2/   BIG        ,INCX       ,INCY
C
C Character variable declartions
C
      CHARACTER*140 CSTR
C
      COMMON /VVCHAR/ CSTR
C 
      SAVE /VEC1/, /VEC2/, /VVCHAR/
C
C Check for a parameter name that is too short.
C
      IF (LEN(WHCH).LT.3) THEN
        CSTR(1:36)='VVSETC - PARAMETER NAME TOO SHORT - '
        CSTR(37:36+LEN(WHCH))=WHCH
        CALL SETER (CSTR(1:36+LEN(WHCH)),1,2)
        STOP
      END IF
C
C Note that there are no character parameters in the VELVCT routine
C currently. This functionality is put in place for future use.
C
C Check for incorrect use of the index parameter.
C
C      IF (WHCH(1:3).EQ.'XXX'.OR.WHCH(1:3).EQ.'xxx') THEN
C        IF (IPAI.LT.1.OR.IPAI.GT.NCLV) THEN
C           CSTR(1:36)='VVSETC - SETTING XXX - PAI INCORRECT'
C           CSTR(18:20)=WHCH(1:3)
C           CALL SETER (CSTR(1:36),2,2)
C           STOP
C           GO TO 9900
C        END IF
C      END IF
C
C Set the proper parameter. (There are none yet.)
C
C      IF (WHCH(1:3).EQ.'XXX'.OR.WHCH(1:3).EQ.'xxx') THEN
C         CONTINUE
C      ELSE
C
      CSTR(1:36)='VVSETC - PARAMETER NAME NOT KNOWN - '
      CSTR(37:39)=WHCH(1:3)
      CALL SETER (CSTR(1:39),3,2)
      STOP
C
C      END IF
C
C      GOTO 9900
C
C 9800 CONTINUE
C
C      CSTR(1:46)='VVSETC - PARAMETER VALUE OUT OF RANGE - '
C      CSTR(40:42)=WHCH(1:3)
C      CALL SETER (CSTR(1:40),3,2)
C      STOP
C      
 9900 CONTINUE
C
C Done.
C
      RETURN
C
      END
