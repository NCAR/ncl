C***********************************************************************
C S O F T F I L L   -   I N T R O D U C T I O N
C***********************************************************************
C
C This file contains materials for a software area-fill package called
C SOFTFILL.  Double-spaced headers like the one above set off major
C portions of the file.  Included are implementation instructions, a
C write-up, user-level routines, and internal routines.
C
C***********************************************************************
C S O F T F I L L   -   I M P L E M E N T A T I O N
C***********************************************************************
C
C The master version of SOFTFILL is written in IFTRAN, an extended form
C of FORTRAN which provides many conveniences.  Running it through the
C IFTRAN preprocessor yields a standard FORTRAN 77 file, which is the
C version distributed as a part of NCAR Graphics.  If the file in which
C you are reading these words begins with
C
C    .OP LS=10001 LI=1 CB RT OC UC=0
C
C it is the IFTRAN version; otherwise, it is the FORTRAN 77 version.
C
C SOFTFILL requires various parts of the NCAR Graphics package to have
C been implemented (in particular, it uses the support routine SETER
C and various routines from SPPS).
C
C***********************************************************************
C S O F T F I L L   -   U S E R - L E V E L   R O U T I N E S
C***********************************************************************
C
      SUBROUTINE SFSGFA (XRA,YRA,NRA,DST,NST,IND,NND,ICI)
C
C Declare the dimensions of argument arrays.
C
      DIMENSION XRA(NRA),YRA(NRA),DST(NST),IND(NND)
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,LCH,LDP(8,8)
C
C Declare the block data routine external to force its loading.
C
      EXTERNAL SFBLDA
C
C Do fill of the type selected by the values of ICI and ITY.
C
      IF (.NOT.(ITY.EQ.0)) GO TO 10001
      IF (.NOT.(ICI.GE.0)) GO TO 10002
      CALL GQFACI (IER,ISC)
      IF (.NOT.(IER.NE.0)) GO TO 10003
      CALL SETER ('SFSGFA - ERROR EXIT FROM GQFACI',1,2)
      STOP
10003 CONTINUE
      CALL GSFACI (ICI)
10002 CONTINUE
      DO 10004 I=1,NRA
      XRA(I)=CUFX(XRA(I))
      YRA(I)=CUFY(YRA(I))
10004 CONTINUE
      CALL GETSET (XLVP,XRVP,YBVP,YTVP,XLWD,XRWD,YBWD,YTWD,LNLG)
      CALL SET    (XLVP,XRVP,YBVP,YTVP,XLVP,XRVP,YBVP,YTVP,   1)
      CALL GFA    (NRA,XRA,YRA)
      CALL SET    (XLVP,XRVP,YBVP,YTVP,XLWD,XRWD,YBWD,YTWD,LNLG)
      IF (ICI.GE.0) CALL GSFACI (ISC)
      GO TO 10005
10001 CONTINUE
      IF (.NOT.(ITY.GT.0)) GO TO 10006
      IF (.NOT.(ICI.GE.0)) GO TO 10007
      CALL GQPLCI (IER,ISC)
      IF (.NOT.(IER.NE.0)) GO TO 10008
      CALL SETER ('SFSGFA - ERROR EXIT FROM GQPLCI',2,2)
      STOP
10008 CONTINUE
      CALL GSPLCI (ICI)
10007 CONTINUE
      CALL SFWRLD (XRA,YRA,NRA,DST,NST,IND,NND)
      IF (.NOT.(ITY.GT.1)) GO TO 10009
      AID=AID+90.
      CALL SFNORM (XRA,YRA,NRA,DST,NST,IND,NND)
      AID=AID-90.
10009 CONTINUE
      IF (ICI.GE.0) CALL GSPLCI (ISC)
      GO TO 10005
10006 CONTINUE
      IF (.NOT.(ICI.GT.0)) GO TO 10010
      AIS=AID
      DBS=DBL
      IT1=-ITY
      DO 10011 I=1,IT1
      IT2=(ICI+IT1-I)/IT1
      IF (.NOT.(IT2.GT.0)) GO TO 10012
      AID=AIS+REAL((I-1)*(180/IT1))
      DBL=MAX(DBS/8.,DBS*2.**(6-IT2))
      IF (.NOT.(I.EQ.1)) GO TO 10013
      CALL SFWRLD (XRA,YRA,NRA,DST,NST,IND,NND)
      GO TO 10014
10013 CONTINUE
      CALL SFNORM (XRA,YRA,NRA,DST,NST,IND,NND)
10014 CONTINUE
10012 CONTINUE
10011 CONTINUE
      AID=AIS
      DBL=DBS
10010 CONTINUE
10005 CONTINUE
C
C Done.
C
      RETURN
C
      END
