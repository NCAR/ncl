C
C	$Id: vvgetc.f,v 1.1 1992-10-12 15:31:06 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVGETC (WHCH,CVAL)
C
      CHARACTER*(*) WHCH,CVAL
C
C This subroutine is called to retrieve the character value of a
C specified parameter.
C
C WHCH is the name of the parameter whose value is to be retrieved.
C
C CVAL is a character variable in which the desired value is to be
C returned by VVGETC.
C
C Declare all of the VELVCT common blocks.
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
C Note that there are no character parameters in the VELVCT routine
C currently. This functionality is put in place for future use.
C
C Check for incorrect use of the index parameter.
C
C      IF (WHCH(1:3).EQ.'XXX'.OR.WHCH(1:3).EQ.'xxx') THEN
C        IF (IPAI.LT.1.OR.IPAI.GT.NCLV) THEN
C           CSTR(1:36)='VVGETC - GETTING XXX - PAI INCORRECT'
C           CSTR(18:20)=WHCH(1:3)
C           CALL SETER (CSTR(1:36),2,2)
C           STOP
C        END IF
C      END IF
C
C Get the proper parameter. (There are none yet.)
C
C      IF (WHCH(1:3).EQ.'XXX'.OR.WHCH(1:3).EQ.'xxx') THEN
C         CONTINUE
C      ELSE
C
      CSTR(1:36)='VVGETC - PARAMETER NAME NOT KNOWN - '
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
C      CSTR(1:46)='VVGETC - PARAMETER VALUE OUT OF RANGE - '
C      CSTR(40:42)=WHCH(1:3)
C      CALL SETER (CSTR(1:40),3,2)
C      STOP
C      
C 9900 CONTINUE
C
C Done.
C
C      RETURN
C
      END
