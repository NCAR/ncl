C
C $Id: idlsqf.f,v 1.1 1995-11-03 23:45:23 kennison Exp $
C
      SUBROUTINE IDLSQF (X,Y,Z,N,A,B,C,XAVG,YAVG)
C
        DIMENSION X(N),Y(N),Z(N)
C
C IDLSQF fits a plane to the data defined by points (X(I),Y(I),Z(I)),
C for I from 1 to N.  It returns the coefficients A, B, and C in the
C equation "Z=AX+BY+C".  It also returns the average X and average Y.
C
        SA=0.
        SB=0.
        SC=0.
        SD=0.
        SE=0.
        SF=0.
        SG=0.
        SH=0.
C
        DO 101 I=1,N
          SA=SA+X(I)
          SB=SB+Y(I)
          SC=SC+Z(I)
          SD=SD+X(I)*X(I)
          SE=SE+Y(I)*Y(I)
          SF=SF+X(I)*Y(I)
          SG=SG+Y(I)*Z(I)
          SH=SH+X(I)*Z(I)
  101   CONTINUE
C
        RN=REAL(N)
C
        A=((RN*SH-SA*SC)*(RN*SE-SB*SB)-(RN*SF-SA*SB)*(RN*SG-SB*SC))/
     +    ((RN*SD-SA*SA)*(RN*SE-SB*SB)-(RN*SF-SA*SB)*(RN*SF-SA*SB))
        B=((RN*SD-SA*SA)*(RN*SG-SB*SC)-(RN*SF-SA*SB)*(RN*SH-SA*SC))/
     +    ((RN*SD-SA*SA)*(RN*SE-SB*SB)-(RN*SF-SA*SB)*(RN*SF-SA*SB))
        C=(SC-SA*A-SB*B)/RN
C
        XAVG=SA/RN
        YAVG=SB/RN
C
C Done.
C
        RETURN
C
      END
