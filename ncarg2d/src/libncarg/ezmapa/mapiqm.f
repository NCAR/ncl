C
C	$Id: mapiqm.f,v 1.1.1.1 1992-04-17 22:32:16 ncargd Exp $
C
C
C The subroutine MAPIQM.
C --- ---------- ------
C
      SUBROUTINE MAPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      DIMENSION IAM(*),XCS(*),YCS(*),IAI(*),IAG(*)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE /MAPCMC/
C
C Terminate the line, if any.
C
      IF (.NOT.(NCRA.GT.1)) GO TO 10000
      CALL ARDRLN (IAM,XCRA,YCRA,NCRA,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      NCRA=0
10000 CONTINUE
C
C Done.
C
      RETURN
C
      END
