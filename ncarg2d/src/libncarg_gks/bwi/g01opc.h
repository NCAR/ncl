C
C $Id: g01opc.h,v 1.2 2000-07-12 16:50:46 haley Exp $
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

C
C  Id code parameters for every element, and class codes for each class.
C
      COMMON /G01OPC/ IDNOOP, IDBEGM, IDENDM, IDBEGP, IDBGPB, IDENDP,
     +                IDMVER, IDMELT, IDDREP, IDCSEL, IDVEXT, IDVINT,
     +                IDCREC, IDCLIN, IDPLIN, IDPMRK, IDTEXT, IDPGON,
     +                IDCARY, IDGDP,  IDLBIX, IDLTYP, IDLWID, IDLCLR,
     +                IDMBIX, IDMTYP, IDMSIZ, IDMCLR, IDTBIX, IDTFON,
     +                IDTPRE, IDCHEX, IDCHSP, IDTCLR, IDCHHT, IDCHOR,
     +                IDTXPA, IDTXAL, IDFBIX, IDINTS, IDFCLR, IDHAIX,
     +                IDPTIX, IDFRPT, IDPTBL, IDPTSZ, IDCTBL, IDASFS,
     +                IDESC,  IDMESS, IDAPLD, IDBKGC, IDDSCR, IDFLST,
     +                CLDELM, CLMDES, CLPDES, CLCNTL, CLPRIM, CLPRAT,
     +                CLESCE, CLEXTE
C
C  Parameter data types.
C
      INTEGER         IDNOOP, IDBEGM, IDENDM, IDBEGP, IDBGPB, IDENDP,
     +                IDMVER, IDMELT, IDDREP, IDCSEL, IDVEXT, IDVINT,
     +                IDCREC, IDCLIN, IDPLIN, IDPMRK, IDTEXT, IDPGON,
     +                IDCARY, IDGDP,  IDLBIX, IDLTYP, IDLWID, IDLCLR,
     +                IDMBIX, IDMTYP, IDMSIZ, IDMCLR, IDTBIX, IDTFON,
     +                IDTPRE, IDCHEX, IDCHSP, IDTCLR, IDCHHT, IDCHOR,
     +                IDTXPA, IDTXAL, IDFBIX, IDINTS, IDFCLR, IDHAIX,
     +                IDPTIX, IDFRPT, IDPTBL, IDPTSZ, IDCTBL, IDASFS,
     +                IDESC,  IDMESS, IDAPLD, IDBKGC, IDDSCR, IDFLST,
     +                CLDELM, CLMDES, CLPDES, CLCNTL, CLPRIM, CLPRAT,
     +                CLESCE, CLEXTE
C
C Class code parameters for every element.
C
      INTEGER         CLNOOP, CLBEGM, CLENDM, CLBEGP, CLBGPB, CLENDP,
     +                CLMVER, CLMELT, CLDREP, CLCSEL, CLVEXT, CLVINT,
     +                CLCREC, CLCLIN, CLPLIN, CLPMRK, CLTEXT, CLPGON,
     +                CLCARY, CLGDP,  CLLBIX, CLLTYP, CLLWID, CLLCLR,
     +                CLMBIX, CLMTYP, CLMSIZ, CLMCLR, CLTBIX, CLTFON,
     +                CLTPRE, CLCHEX, CLCHSP, CLTCLR, CLCHHT, CLCHOR,
     +                CLTXPA, CLTXAL, CLFBIX, CLINTS, CLFCLR, CLHAIX,
     +                CLPTIX, CLFRPT, CLPTBL, CLPTSZ, CLCTBL, CLASFS,
     +                CLESC,  CLMESS, CLAPLD, CLBKGC, CLDSCR, CLFLST
C
C  Equivalence all individual class code parameters to the single
C  code for the class in which the element(s) belong.
C
      EQUIVALENCE (CLDELM, CLNOOP,CLBEGM,CLENDM,CLBEGP,CLBGPB,CLENDP)
      EQUIVALENCE (CLMDES, CLMVER,CLMELT,CLDREP,CLDSCR,CLFLST)
      EQUIVALENCE (CLPDES, CLCSEL,CLVEXT,CLBKGC)
      EQUIVALENCE (CLCNTL, CLVINT,CLCREC,CLCLIN)
      EQUIVALENCE (CLPRIM, CLPLIN,CLPMRK,CLTEXT,CLPGON,CLCARY,CLGDP)
      EQUIVALENCE (CLPRAT, CLLBIX,CLLTYP,CLLWID,CLLCLR,CLMBIX,CLMTYP)
      EQUIVALENCE (CLPRAT, CLMSIZ,CLMCLR,CLTBIX,CLTFON,CLTPRE,CLCHEX)
      EQUIVALENCE (CLPRAT, CLCHSP,CLTCLR,CLCHHT,CLCHOR,CLTXPA,CLTXAL)
      EQUIVALENCE (CLPRAT, CLFBIX,CLINTS,CLFCLR,CLHAIX,CLPTIX,CLFRPT)
      EQUIVALENCE (CLPRAT, CLPTBL,CLPTSZ,CLCTBL,CLASFS)
      EQUIVALENCE (CLESCE, CLESC), (CLEXTE, CLMESS,CLAPLD)
