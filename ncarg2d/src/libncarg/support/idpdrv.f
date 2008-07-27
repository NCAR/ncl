C
C $Id: idpdrv.f,v 1.5 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE IDPDRV (NDP,XD,YD,ZD,NT,IPT,PD,WK)
C
C This subroutine estimates partial derivatives of the first and
C second order at the data points.
C
C The input arguments are as follows:
C
C   NDP       number of data points.
C
C   XD,YD,ZD  arrays of dimension NDP containing the X,
C             Y, and Z coordinates of the data points.
C
C   NT        number of triangles.
C
C   IPT       integer array of dimension 3*NT containing the
C             point numbers of the vertices of the triangles.
C
C The output argument is as follows:
C
C   PD        array of dimension 5*NDP, where the estimated
C             ZX, ZY, ZXX, ZXY, and ZYY values at the Ith
C             data point are to be stored as the (5*I-4)th,
C             (5*I-3)rd, (5*I-2)nd, (5*I-1)st, and (5*I)th
C             elements, respectively, where I = 1, 2, ...,
C             NDP.
C
C The other argument is as follows:
C
C   WK        array of dimension NDP used internally as a
C             work area.
C
C Declaration statements.
C
      DIMENSION XD(NDP),YD(NDP),ZD(NDP),IPT(3*NT),PD(5*NDP),WK(NDP)
      DIMENSION IPTI(3),XV(3),YV(3),ZV(3),ZXV(3),ZYV(3),W1(3),W2(3)
C
      DATA EPSLN / 1.E-6 /
C
C Clear the PD array.
C
      JPDMX=5*NDP
C
      DO 101 JPD=1,JPDMX
        PD(JPD)=0.0
  101 CONTINUE
C
      DO 102 IDP=1,NDP
        WK(IDP)=0.0
  102 CONTINUE
C
C Estimate ZX and ZY.
C
      DO 105 IT=1,NT
        JPT0=3*(IT-1)
        DO 103 IV=1,3
          JPT=JPT0+IV
          IDP=IPT(JPT)
          IPTI(IV)=IDP
          XV(IV)=XD(IDP)
          YV(IV)=YD(IDP)
          ZV(IV)=ZD(IDP)
  103   CONTINUE
        DX1=XV(2)-XV(1)
        DY1=YV(2)-YV(1)
        DZ1=ZV(2)-ZV(1)
        DX2=XV(3)-XV(1)
        DY2=YV(3)-YV(1)
        DZ2=ZV(3)-ZV(1)
        VPX=DY1*DZ2-DZ1*DY2
        VPY=DZ1*DX2-DX1*DZ2
        VPZ=DX1*DY2-DY1*DX2
        VPZMN=ABS(DX1*DX2+DY1*DY2)*EPSLN
        IF (ABS(VPZ).LE.VPZMN) GO TO 105
        D12=SQRT((XV(2)-XV(1))**2+(YV(2)-YV(1))**2)
        D23=SQRT((XV(3)-XV(2))**2+(YV(3)-YV(2))**2)
        D31=SQRT((XV(1)-XV(3))**2+(YV(1)-YV(3))**2)
        W1(1)=1.0/(D31*D12)
        W1(2)=1.0/(D12*D23)
        W1(3)=1.0/(D23*D31)
        W2(1)=VPZ*W1(1)
        W2(2)=VPZ*W1(2)
        W2(3)=VPZ*W1(3)
        DO 104 IV=1,3
          IDP=IPTI(IV)
          JPD0=5*(IDP-1)
          WI=(W1(IV)**2)*W2(IV)
          PD(JPD0+1)=PD(JPD0+1)+VPX*WI
          PD(JPD0+2)=PD(JPD0+2)+VPY*WI
          WK(IDP)=WK(IDP)+VPZ*WI
  104   CONTINUE
  105 CONTINUE
      DO 106 IDP=1,NDP
        IF (WK(IDP).NE.0.) THEN
          JPD0=5*(IDP-1)
          PD(JPD0+1)=-PD(JPD0+1)/WK(IDP)
          PD(JPD0+2)=-PD(JPD0+2)/WK(IDP)
        END IF
  106 CONTINUE
C
C Estimate ZXX, ZXY, and ZYY.
C
      DO 109 IT=1,NT
        JPT0=3*(IT-1)
        DO 107 IV=1,3
          JPT=JPT0+IV
          IDP=IPT(JPT)
          IPTI(IV)=IDP
          XV(IV)=XD(IDP)
          YV(IV)=YD(IDP)
          JPD0=5*(IDP-1)
          ZXV(IV)=PD(JPD0+1)
          ZYV(IV)=PD(JPD0+2)
  107   CONTINUE
        DX1=XV(2)-XV(1)
        DY1=YV(2)-YV(1)
        DZX1=ZXV(2)-ZXV(1)
        DZY1=ZYV(2)-ZYV(1)
        DX2=XV(3)-XV(1)
        DY2=YV(3)-YV(1)
        DZX2=ZXV(3)-ZXV(1)
        DZY2=ZYV(3)-ZYV(1)
        VPXX=DY1*DZX2-DZX1*DY2
        VPXY=DZX1*DX2-DX1*DZX2
        VPYX=DY1*DZY2-DZY1*DY2
        VPYY=DZY1*DX2-DX1*DZY2
        VPZ=DX1*DY2-DY1*DX2
        VPZMN=ABS(DX1*DX2+DY1*DY2)*EPSLN
        IF (ABS(VPZ).LE.VPZMN) GO TO 109
        D12=SQRT((XV(2)-XV(1))**2+(YV(2)-YV(1))**2)
        D23=SQRT((XV(3)-XV(2))**2+(YV(3)-YV(2))**2)
        D31=SQRT((XV(1)-XV(3))**2+(YV(1)-YV(3))**2)
        W1(1)=1.0/(D31*D12)
        W1(2)=1.0/(D12*D23)
        W1(3)=1.0/(D23*D31)
        W2(1)=VPZ*W1(1)
        W2(2)=VPZ*W1(2)
        W2(3)=VPZ*W1(3)
        DO 108 IV=1,3
          IDP=IPTI(IV)
          JPD0=5*(IDP-1)
          WI=(W1(IV)**2)*W2(IV)
          PD(JPD0+3)=PD(JPD0+3)+VPXX*WI
          PD(JPD0+4)=PD(JPD0+4)+(VPXY+VPYX)*WI
          PD(JPD0+5)=PD(JPD0+5)+VPYY*WI
  108   CONTINUE
  109 CONTINUE
      DO 110 IDP=1,NDP
        JPD0=5*(IDP-1)
        IF (WK(IDP).NE.0.) THEN
          PD(JPD0+3)=-PD(JPD0+3)/WK(IDP)
          PD(JPD0+4)=-PD(JPD0+4)/(2.0*WK(IDP))
          PD(JPD0+5)=-PD(JPD0+5)/WK(IDP)
        END IF
  110 CONTINUE
C
      RETURN
C
      END
