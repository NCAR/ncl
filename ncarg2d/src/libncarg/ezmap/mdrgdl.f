C
C $Id: mdrgdl.f,v 1.9 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGDL (IRGL)
C
        INTEGER          IRGL
C
C This routine is intended to pick an appropriate data level at which to
C use the RANGS/GSHHS data and return it as the value of IRGL.
C
        DOUBLE PRECISION MDSCAL
C
C Pre-set IRGL in case we take an error exit.
C
        IRGL=0
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDRGDL - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        CALL MDGETI ('IN',INTF)
C
        IF (INTF.NE.0) THEN
          CALL MDPINT
          IF (ICFELL('MDRGDL',2).NE.0) RETURN
        END IF
C
C Retrieve the SPPS parameters specifying the mapping of fractional
C coordinates to the U/V plane.
C
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        IF (ICFELL('MDRGDL',3).NE.0) RETURN
C
C Find the angular separation of those points on the globe that project
C into two points near the center of the map and divide by the distance
C between the points on the map to get an estimate of the scale of the
C map in that vicinity.
C
        SCAL=MDSCAL(XVPL+.49*(XVPR-XVPL),.5*(YVPB+YVPT),
     +              XVPL+.51*(XVPR-XVPL),.5*(YVPB+YVPT))
C
C Return a value of the resolution flag based on a somewhat heuristic
C interpretation of the scale estimate.
C
        IF      (SCAL.LT.10.D0) THEN
          IRGL=0
        ELSE IF (SCAL.LT.20.D0) THEN
          IRGL=1
        ELSE IF (SCAL.LE.40.D0) THEN
          IRGL=2
        ELSE IF (SCAL.LE.80.D0) THEN
          IRGL=3
        ELSE
          IRGL=4
        END IF
C
C Done.
C
        RETURN
C
      END
