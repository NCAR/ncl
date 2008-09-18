C
C $Id: mdrgsc.f,v 1.9 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGSC (JCOL,JCSF)
C
C This routine may be called to set the values of two arrays in EZMAP,
C one of which determines the colors to be used for outline maps drawn
C using the RANGS/GSHHS data, and the other of which determines the
C colors to be used for solid-filled maps drawn using that data.
C
        INTEGER JCOL(5),JCSF(5)
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPRGD/  ICOL(5),ICSF(5),IDPF,LCRA,NILN,NILT,OLAT,OLON
        INTEGER          ICOL,ICSF,IDPF,LCRA,NILN,NILT
        REAL             OLAT,OLON
        SAVE   /MAPRGD/
C
C Declare local variables.
C
        INTEGER          I
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDRGSC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer contents of color-index argument arrays to internal arrays.
C
        DO 101 I=1,5
          ICOL(I)=JCOL(I)
          ICSF(I)=JCSF(I)
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
