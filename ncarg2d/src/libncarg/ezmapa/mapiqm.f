C
C $Id: mapiqm.f,v 1.2 1993-12-21 00:44:51 kennison Exp $
C
      SUBROUTINE MAPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
      DIMENSION IAM(*),XCS(*),YCS(*),IAI(*),IAG(*)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE /MAPCMC/
C
C Terminate the line, if any.
C
      IF (.NOT.(NCRA.GT.1)) GO TO 10000
      CALL ARDRLN (IAM,XCRA,YCRA,NCRA,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (.NOT.(ICFELL('MAPIQM',1).NE.0)) GO TO 10001
      IIER=-1
      RETURN
10001 CONTINUE
      NCRA=0
10000 CONTINUE
C
C Done.
C
      RETURN
C
      END
