C
C	$Id: contlk.f,v 1.2 2000-07-12 16:22:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE CONTLK (XD,YD,NDP,IPT)
C
C  DRAW THE TRIANGLES CREATED BY CONTNG
C
C
C
      COMMON /CONRA1/ CL(30)     ,NCL        ,OLDZ       ,PV(210)    ,
     1                FINC       ,HI         ,FLO
      COMMON /CONRA2/ REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                ISCALE     ,LOOK       ,PLDVLS     ,GRD        ,
     2                CINC       ,CHILO      ,CON        ,LABON      ,
     3                PMIMX      ,SCALE      ,FRADV      ,EXTRI      ,
     4                BPSIZ      ,LISTOP
      COMMON /CONRA3/ IREC
      COMMON /CONRA4/ NCP        ,NCPSZ
      COMMON /CONRA5/ NIT        ,ITIPV
      COMMON /CONRA6/ XST        ,YST        ,XED        ,YED        ,
     1                STPSZ      ,IGRAD      ,IG         ,XRG        ,
     2                YRG        ,BORD       ,PXST       ,PYST       ,
     3                PXED       ,PYED       ,ITICK
      COMMON /CONR18/ TITLE      ,ICNT   ,ITLSIZ
      COMMON /CONR19/ IHIGH      ,INMAJ      ,INLAB      ,INDAT      ,
     1              LEN      ,IFMT       ,LEND       ,
     2                IFMTD      ,ISIZEP     ,INMIN
      COMMON /CONRA9/ ICOORD(500),NP         ,MXXY       ,TR         ,
     1                BR         ,TL         ,BL         ,CONV       ,
     2                XN         ,YN         ,ITLL       ,IBLL       ,
     3                ITRL       ,IBRL       ,XC         ,YC         ,
     4                ITLOC(210) ,JX         ,JY         ,ILOC       ,
     5                ISHFCT     ,XO         ,YO         ,IOC        ,NC
      COMMON /CONR10/ NT         ,NL         ,NTNL       ,JWIPT      ,
     1                JWIWL      ,JWIWP      ,JWIPL      ,IPR        ,
     2                ITPV
      COMMON /CONR20/ NREP       ,NCRT       ,ISIZEL     ,
     1                MINGAP     ,ISIZEM         ,
     2                TENS
      COMMON /CONR12/ IXMAX      ,IYMAX      ,XMAX       ,YMAX
      LOGICAL         REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                LOOK       ,PLDVLS     ,GRD        ,LABON      ,
     2                PMIMX      ,FRADV      ,EXTRI      ,CINC       ,
     3                TITLE      ,LISTOP     ,CHILO      ,CON
      COMMON /CONR15/ ISTRNG
        CHARACTER*64 ISTRNG
        COMMON /CONR16/ FORM
        CHARACTER*10 FORM
        COMMON /CONR17/ NDASH, IDASH, EDASH
        CHARACTER*10 NDASH, IDASH, EDASH
C
C
      DIMENSION       XD(1)      ,YD(1)      ,IPT(1)
C
        SAVE
C
C  STATEMENT FUNCTIONS TO SCALE DATA FOR OVERLAYS
C
C     FX(XXX,YYY) = XXX
C     FY(XXX,YYY) = YYY
C
C   ADVANCE PICTURE IF DESIRED
C
      IF (FRADV) CALL FRAME
C
C  DRAW TRIANGLES
C
      DO  100 K=1,NT
         I = K*3
         I1 = IPT(I)
         I2 = IPT(I-1)
         I3 = IPT(I-2)
         XX = FX(XD(I1),YD(I1))
         CALL FL2INT (XX,FY(XD(I1),YD(I1)),MX1,MY1)
         CALL PLOTIT (MX1,MY1,0)
         XX = FX(XD(I2),YD(I2))
         CALL FL2INT (XX,FY(XD(I2),YD(I2)),MX,MY)
         CALL PLOTIT (MX,MY,1)
         XX = FX(XD(I3),YD(I3))
         CALL FL2INT (XX,FY(XD(I3),YD(I3)),MX,MY)
         CALL PLOTIT (MX,MY,1)
         CALL PLOTIT (MX1,MY1,1)
  100 CONTINUE
C
C     FLUSH PLOTIT BUFFER
C
      CALL PLOTIT(0,0,0)
      RETURN
      END
