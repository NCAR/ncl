C
C	$Id: gwibkd.f,v 1.10 2008-07-27 00:21:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWIBKD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA GWIBKDX
C
C  BLOCKDATA module for workstation driver type 3 (WISS).
C
      include 'gwiwsl.h'
      include 'gwiwdt.h'
      include 'gwiins.h'
      include 'gwiadf.h'
      include 'gwiadc.h'
      include 'gwiopc.h'
      include 'gwienu.h'
C
C  Workstation state list.
C
C  Workstation type and category.
C
      DATA LWTYPE,LWKCAT/3,3/
C
C  Internal constants.
C
C
C  Last function code, last continuation flag.
C
      DATA  MCODES/0/,  MCONTS/0/
C
C  VDC, color index, direct color component field widths.
C
      DATA  MVDCFW/16/,  MCIXFW/ 8/,  MDCCFW/ 8/
      DATA  MIXFW /16/,  MINTFW/16/,  MEFW  /16/
C
C  2**MDCCFW - 1
C
      DATA  MDCCRG/255/
C
C  Min AND Max VDC address limits in X and Y directions,
C  the VDC extent (the two concepts should be separated later).
C
      DATA  MINXVD/0/,  MAXXVD/32767/,  MINYVD/0/,  MAXYVD/32767/
C
C  NDC tO VDC mapping coefficients, X and Y directions.
C
      DATA  MXOFF/0/,  MXSCAL/32767/,  MYOFF/0/,  MYSCAL/32767/
C
C  Precision (in bits) of the two components of the representation
C  of real numbers in the segment.
C
      DATA  MCFPP/16/
C
C  Default attribute context.
C
C
C  Polyline index, linetype, line color index, linewidth scale.
C
      DATA  MDPLIX/1/,  MDLTYP/1/,  MDPLCI/1/,  ADLWSC/1.0/
C
C  Polymarker index, marker type, marker color index, marker scale.
C
      DATA  MDPMIX/1/,  MDMTYP/3/,  MDPMCI/1/,  ADMSZS/1.0/
C
C  Text index, text path, text alignment, text font.
C
      DATA  MDTXIX/1/,  MDTXP/0/,  MDTXAL/0,0/,  MDTXFO/1/
C
C  Text precision, text color index, character height.
C
      DATA  MDTXPR/0/,  MDTXCI/1/,  MDCHH/328/
C
C  Character orientation vectors.
C
      DATA  MDCHOV/0,328,328,0/
C
C  Character expansion factor, character spacing.
C
      DATA  ADCHXP/1.0/,  ADCHSP/0.0/
C
C  Fill area index, pattern size, pattern ref point.
C
      DATA  MDFAIX/1/,  MDPASZ/0,32767,32767,0/,  MDPARF/0,0/
C
C  Fill area interior style, style index, color index.
C
      DATA  MDFAIS/0/,  MDFASI/1/,  MDFACI/1/
C
C  ASF values.
C
      DATA  MDASF/13*0/
C
C  Pointers into attribute structure, VALCHG variables.
C
      DATA  IVPLIX/ 1/,  IVLTYP/ 2/,  IVLWSC/ 3/,  IVPLCI/ 4/
      DATA  IVPMIX/ 5/,  IVMTYP/ 6/,  IVMSZS/ 7/,  IVPMCI/ 8/
      DATA  IVTXIX/ 9/,  IVTXP /10/,  IVTXAL/11/,  IVCHH /12/
      DATA  IVCHOV/13/,  IVTXFO/14/,  IVTXPR/15/,  IVCHXP/16/
      DATA  IVCHSP/17/,  IVTXCI/18/,  IVFAIX/19/,  IVPASZ/20/
      DATA  IVPARF/21/,  IVFAIS/22/,  IVFASI/23/,  IVFACI/24/
      DATA  IVASF /25/
C
C  Pointers into attribute equivalencing arrays.
C
      DATA  IP2AEA/  1,  2, -3,  4,  5,  6, -7,  8,  9, 10, 11, 13, 14,        
     +              18, 19,-20,-21, 22, 23, 24, 28, 30, 31, 32, 33, 46/        
C
C  Pointers into ASF arrays.
C
      DATA  IALTYP/ 1/,  IALWSC/ 2/,  IAPLCI/ 3/,  IAMTYP/ 4/
      DATA  IAMSZS/ 5/,  IAPMCI/ 6/,  IATXFP/ 7/,  IACHXP/ 8/
      DATA  IACHSP/ 9/,  IATXCI/10/,  IAFAIS/11/,  IAFASI/12/
      DATA  IAFACI/13/
C
C  Number of CGM and GKS ASFs, GKS to CGM mapping array.
C
      DATA  NCGASF/18/,  NGKASF/13/
      DATA  MASMAP/1,2,3,4,5,6,7,7,8,9,10,11,13,12,12,0,0,0/
C
C  GKS enumeratives.
C
      DATA  GNO   /0/,     GYES/1/,   GCONDI/0/,   GALWAY/1/
      DATA  GINACT/0/,   GACTIV/1/,   GNEMPT/0/,   GEMPTY/1/
      DATA  GNPEND/0/,    GPEND/1/,    GCLIP/1/,   GNCLIP/0/
C
C  CGM opcodes, class and id codes.
C
      DATA  IDNOOP/0/,   IDBEGM/1/,   IDENDM/2/,   IDBEGP/3/
      DATA  IDBGPB/4/,   IDENDP/5/,   IDMVER/1/,   IDMELT/11/
      DATA  IDDREP/12/,  IDCSEL/2/,   IDVEXT/6/,   IDVINT/1/
      DATA  IDCREC/5/,   IDCLIN/6/,   IDPLIN/1/,   IDPMRK/3/
      DATA  IDTEXT/4/,   IDPGON/7/,   IDCARY/9/,   IDGDP/10/
      DATA  IDLBIX/1/,   IDLTYP/2/,   IDLWID/3/,   IDLCLR/4/
      DATA  IDMBIX/5/,   IDMTYP/6/,   IDMSIZ/7/,   IDMCLR/8/
      DATA  IDTBIX/9/,   IDTFON/10/,  IDTPRE/11/,  IDCHEX/12/
      DATA  IDCHSP/13/,  IDTCLR/14/,  IDCHHT/15/,  IDCHOR/16/
      DATA  IDTXPA/17/,  IDTXAL/18/,  IDFBIX/21/,  IDINTS/22/
      DATA  IDFCLR/23/,  IDHAIX/24/,  IDPTIX/25/,  IDFRPT/31/
      DATA  IDPTBL/32/,  IDPTSZ/33/,  IDCTBL/34/,  IDASFS/35/
      DATA  IDESC/1/,    IDMESS/1/,   IDAPLD/2/,   IDBKGC/7/
      DATA  IDDSCR/2/,   IDFLST/13/
      DATA  CLDELM/0/,   CLMDES/1/,   CLPDES/2/,   CLCNTL/3/
      DATA  CLPRIM/4/,   CLPRAT/5/,   CLESCE/6/,   CLEXTE/7/
C
      END
