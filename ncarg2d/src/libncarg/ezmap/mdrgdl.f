C
C $Id: mdrgdl.f,v 1.3 2001-11-02 22:37:16 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
