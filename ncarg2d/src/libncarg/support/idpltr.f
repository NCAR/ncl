C
C $Id: idpltr.f,v 1.4 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE IDPLTR (NDAT,XDAT,YDAT,IWRK)
C
        DIMENSION XDAT(NDAT),YDAT(NDAT),IWRK(*)
C
C This routine plots the triangulation.  The arguments NDAT, XDAT, YDAT,
C YDAT, and IWRK are all as used in a previous call to either IDSFFT or
C IDBVIP:
C
C   NDAT is the number of data points.
C
C   (XDAT(I),I=1,NDAT) are the X coordinates of the data points.
C
C   (YDAT(I),I=1,NDAT) are the Y coordinates of the data points.
C
C   IWRK is the integer work array.
C
C Note that the caller is responsible for doing the SET call required
C to map the triangles to the correct position on the plotter frame.
C
C Note also that some triangle edges are drawn twice.  No attempt is
C made to prevent this.
C
C Check for an uncleared prior error.
C
        IF (ICFELL('IDPLTR (BIVAR) - UNCLEARED PRIOR ERROR',1).NE.0)
     +                                                        RETURN
C
C Draw the triangles.
C
        DO 101 I=1,IWRK(5)
          IOT1=IWRK(15+3*(I-1)+1)
          IOT2=IWRK(15+3*(I-1)+2)
          IOT3=IWRK(15+3*(I-1)+3)
          CALL FRSTPT (XDAT(IOT1),YDAT(IOT1))
          IF (ICFELL('IDPLTR',2).NE.0) RETURN
          CALL VECTOR (XDAT(IOT2),YDAT(IOT2))
          IF (ICFELL('IDPLTR',3).NE.0) RETURN
          CALL VECTOR (XDAT(IOT3),YDAT(IOT3))
          IF (ICFELL('IDPLTR',4).NE.0) RETURN
          CALL VECTOR (XDAT(IOT1),YDAT(IOT1))
          IF (ICFELL('IDPLTR',5).NE.0) RETURN
          CALL PLOTIF (0.,0.,2)
          IF (ICFELL('IDPLTR',6).NE.0) RETURN
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
