C
C	$Id: g01bkd.f,v 1.1.1.1 1992-04-17 22:33:55 ncargd Exp $
C
        BLOCK DATA G01BKD
C
C       BLOCK DATA MODULE FOR WORKSTATION DRIVER TYPE 1.
C
C
C     G01WSL    -- workstation state list for workstation type 1,
C                  a vdm subset mo for level 0a GKS.
C
C                  Contains all of the items mandated GKS for the
C                  state list, plus supplementary items such as
C                  array dimensions, plus some state variables (such
C                  as clipping control parameters) which are not
C                  specified as part of the structure by GKS.
C
C
C     mwkid     - workstation identifier.
C     mconid    - connection identifier (lun for metafile output).
C     mwtype    - workstation type (should be 1 for this workstation).
C
C       (The previous 3 items are initialized by 'OPEN WORKSTATION')
C
C     mstate    - workstation state (0=ginact, 1=gactiv).
C     mopen     - marks workstation open/close (0=closed, 1=open)
C     mdefmo    - deferral mode (0=gasap,1=gbnil,2=gbnig,3=gasti),
C                 not settable at level 0, default 3 (gasti).
C     mregmo    - implicit regeneration mode (0=gssupd,1=gallow),
C                 not settable at level 0, default 1 (gallow).
C     mdempt    - display surface empty (0=gnempt,1=gempty),
C                 default 1 (gempty).
C     mnfram    - new frame action necessary at update (0=gno,1=gyes),
C                 should always be 0 (gno), because of 'mregmo', plus
C                 the fact that level 0 has only the functions SET
C                 COLOR REPRESENTATION and workstation transformation
C                 that have potential regeneration implications, and
C                 the treatment of these as IMM (the WDT for MO does
C                 not contain IRG/IMM information).
C     mtus      - workstation transformation update state (0=gnpend,
C                 1=gpend); default 0 (gnpend).  This item should
C                 always be 0 in this MO workstation, because of
C                 assumption discussed in 'mnfram'.  See also the above
C                 discussion of "Workstation Transformation".
C     rwindo    - requested workstation window in NDC; in order, the
C                 entries in 'rwindo' are rwxmin,rwxmax,rwymin,rwymax.
C                 Default 0,1,0,1.
C     cwindo    - current workstation window in NDC; in order, the
C                 entries in 'cwindo' are cwxmin,cwxmax,cwymin,cwymax.
C                 See above discussion on "Workstation Transformation".
C                 Default 0,1,0,1.
C     mrwkvp    - requested workstation viewport (mrvxmn,mrvxmx,mrvymn,
C                 mrvymx, converted from real DC to integer DC/VDC).
C                 Default 0,32767,0,32767.
C     mcwkvp    - current workstation viewport (mrvxmn,mrvxmx,mrvymn,
C                 mrvymx, converted from real DC to integer DC/VDC).
C                 See above discussion on "Workstation Window"
C                 Default 0,32767,0,32767.
C     molmax    - size of color table arrays (in'zd in block data).
C     mol       - number of indexes currently defined in color table,
C                 even for MO GKS mandates that this be at least 2 --
C                 indexes 0 and 1 are supposed to be defined in the WDT
C                 for every workstation (default 2).
C     mcovfl    - overflow flag for color index arrays, indicating
C                 whether number of simultaneously defined indexes,
C                 'mol', on MO has exceeded 'molmax' (0=no, 1=yes).
C                 Default 0.
C     mcsort    - sort flag for color index arrays, indicating whether
C                 'mcoli' and the color arrays are known to be in
C                 sort order or may not be (0=nosort, 1=sort).
C                 Default value is 1.
C     mcoli     - array molmax long to hold color indexes which have
C                 been defined; default (1)=0, (2)=1.
C     sred      - array molmax long to hold red components of defined
C                 color indexes; default (1)=0.0, (2)=0.8.
C     sgreen    - array molmax long to hold green components of defined
C                 color indexes; default (1)=0.0, (2)=0.8.
C     sblue     - array molmax long to hold blue components of defined
C                 color indexes;  default (1)=0.0, (2)=0.8.
C
C
C     mrcrec    - clipping rectangle (mxmin,mymin,mxmax,mymax;
C                 transformed by MO from real NDC and stored as
C                 corner pts, integer VDC (default: 0,0,32767,32767)
C     mrclip    - clipping indicator    (default: 0 [GNCLIP])
C
      COMMON  /G01WSL/  MWKID   ,MCONID ,MWTYPE ,MSTATE ,MOPEN  ,
     +                  MDEFMO  ,MREGMO ,MDEMPT ,MNFRAM ,MTUS   ,
     +                  RWINDO(4)       ,CWINDO(4)      ,
     +                  RWKVP (4)       ,CWKVP (4)      ,
     +                  MOLMAX  ,MOL    ,MCOVFL ,MCSORT ,MCOLI(256),
     +                  SRED(256)       ,SGREEN(256)    ,SBLUE(256),
     +                  MRCREC(4)       ,MRCLIP
        INTEGER         MWKID   ,MCONID ,MWTYPE ,MSTATE ,MOPEN
        INTEGER         MDEFMO  ,MREGMO ,MDEMPT ,MNFRAM ,MTUS
        INTEGER         MOLMAX  ,MOL    ,MCOVFL ,MCSORT ,MCOLI
        REAL            RWINDO          ,CWINDO
        REAL            RWKVP           ,CWKVP
        REAL            SRED            ,SGREEN         ,SBLUE
        INTEGER         MRCREC  ,MRCLIP
C
C     G01WDT    -- Workstation Description Table for workstation
C                  type 1, a VDM-subset MO for level 0a GKS.
C
C       lwtype  - workstation type, 1 for this workstation.
C       lwkcat  - workstation category, 4 (gmo)
C       mversn  - version number for metafile (METAFILE ELEM LIST)
C
      COMMON  /G01WDT/  LWTYPE, LWKCAT, MVERSN
        INTEGER         LWTYPE, LWKCAT, MVERSN
C
C     G01INS    -- Structure for preservation of miscellaneous
C                  internal state variables which may be needed
C                  and should be non-volatile.
C
C       mcodes  - set equal to function code of current invocation
C                 upon entry, after check for continuation error.
C       mconts  - continuation flag of last interface invocation.
C       mvdcfw  - field width for metafile coordinates, i.e., the
C                 number of bits per coordinate component in the
C                 metafile output stream; default 16 set in blockdata.
C       mcixfw  - field width for color indices, measured in bits;
C                 default 8 set in blockdata.
C       mdccfw  - field width for direct color components, measured
C                 in bits; default 8 set in blockdata.
C       mixfw   - field width for CGM parameters of type index,
C                 measured in bits; default 16 set in blockdata.
C       mintfw  - field width for CGM parameters of type integer,
C                 measured in bits; default 16 set in blockdata.
C       mefw    - field width for CGM parameters of type enumerated,
C                 measured in bits; fixed at 16, set in blockdata.
C       mdccrg  - normalized direct color component range, 2**mdccfw-1,
C                 0.0 to 1.0 real is mapped to 0 to mdccrg integer,
C                 and recorded thus in the metafile; default 255.
C                 Default; mxscal=32767.
C       minxvd  - minimum metafile x address; default 0.
C       maxxvd  - maximum metafile address; default 32767.
C       minyvd  - minimum metafile y address; default 0.
C       maxyvd  - maximuim metafile y address; default 32767.
C       mxoff   - x offset (additive constant) for transformation which
C                 converts real [0-1] NDC to integer VDC (dflt 0-32767).
C                 VDC(x) = mxoff + mxscal*NDC(x).  Default; mxoff=0.
C       mxscal  - x scaling factor for NDC to VDC transformation.
C                 Default; mxscal=32767.  Scale and offset would only
C                 be changeable if the scheme for Workstation Transform-
C                 tion handling by mapping a subset of NDC into VDC
C                 were realized.  In this case, offset and scaling
C                 would be computed based on the NDC window and the
C                 VDC viewport, for both x and y.
C       myoff   - y offset (additive constant) for transformation which
C                 converts real [0-1] NDC to integer VDC (dflt 0-32767).
C                 VDC(y) = myoff + myscal*NDC(y).  Default; myoff=0.
C       myscal  - y scaling factor for NDC to VDC transformation.
C                 Default; myscal=32767.
C       mcfpp   - the bit precision of each of the two components that
C                 make up a flaoting point number in the metacode.
C                 Default; mcfpp=16
C       mcfrm   - the current frame number for VDM
C       mcopcl  - the opcode class for the current VDM instruction
C       mcopid  - the opcode id for the current VDM instruction
C       mcnbyt  - the remainder byte count for the continue of a VDM
C                 instruction
C       mccbyt  - the current number of bytes being transfered to a
C                 VDM operand set
C       mslfmt  - indicator as to whether the current instruction being
C                 put out is a short format or long format instruction.
C
      COMMON  /G01INS/  MCODES  ,MCONTS ,
     +                  MVDCFW  ,MCIXFW ,MDCCFW ,MIXFW  ,MINTFW ,
     +                  MDCCRG  ,MXOFF  ,MXSCAL ,MYOFF  ,MYSCAL ,
     +                  MINXVD  ,MAXXVD ,MINYVD ,MAXYVD ,
     +                  MCFRM   ,MCOPCL ,MCOPID ,MCNBYT ,
     +                  MCCBYT  ,MCFPP  ,MSLFMT ,MEFW   ,MCTCHG ,
     +                  MBCCHG
        INTEGER         MCODES  ,MCONTS
        INTEGER         MVDCFW  ,MCIXFW ,MDCCFW ,MIXFW  ,MINTFW
        INTEGER         MDCCRG  ,MXOFF  ,MXSCAL ,MYOFF  ,MYSCAL
        INTEGER         MINXVD  ,MAXXVD ,MINYVD ,MAXYVD
        INTEGER         MCFRM   ,MCOPCL ,MCOPID ,MCNBYT
        INTEGER         MCCBYT  ,MCFPP  ,MSLFMT ,MEFW   ,MCTCHG
        INTEGER         MBCCHG
C
C     G01ADF  -- Contains the default values of the workstation
C                independent attributes.  These are the values which
C                the attributes implicitly assume after BEGIN PICTURE.
C                BEGIN PICTURE effectively causes these to be the
C                "sent" variables, even though the send is implicit.
C
C                The first part of the structure exactly parallels
C                the structure G01ARQ.  Following the first part
C                are defaults for clipping control parameters, ...
C
C
C       mdplix  - polyline index                (default: 1)
C       mdltyp  - line type                     (default: 1 [GLSOLI])
C       mdplci  - polyline color index          (default: 1)
C       adlwsc  - line width scale factor       (default: 1.0)
C
C       mdpmix  - polymarker index              (default: 1)
C       mdmtyp  - marker type                   (default: 3 [GAST])
C       mdpmci  - polymarker color index        (default: 1)
C       admszs  - marker size scale factor      (default: 1.0)
C
C       mdtxix  - text index                    (default: 1)
C       mdtxp   - text path                     (default: 0 [GRIGHT])
C       mdtxal  - text alignment        (default: 1,4 [GLEFT,GBASE])
C       mdtxfo  - text font                     (default: 1)
C       mdtxpr  - text precision                (default: 0 [GSTRP])
C       mdtxci  - text color index              (default: 1)
C       mdchh   - character height              (default: 328)
C       mdchov  - character orientation vectors
C                                   (default: 0,328 and 328,0)
C       adchxp  - character expansion factor    (default: 1.0)
C       adchsp  - character spacing             (default: 0.0)
C
C       mdfaix  - fill area index               (default: 1)
C       mdpasz  - pattern size                  (default: 0,32767,
C                                                         32767,0)
C       mdparf  - pattern reference point       (default: 0,0)
C       mdfais  - fill area interior style      (default: 0 [GHOLLO])
C       mdfasi  - fill area style index         (default: 1)
C       mdfaci  - fill area color index         (default: 1)
C
C       mdasf   - ASFs, in order:
C                  1 - linetype                    (default: 1 [GINDIV])
C                  2 - linewidth scale factor      (default: 1 [GINDIV])
C                  3 - polyline color index        (default: 1 [GINDIV])
C                  4 - marker type                 (default: 1 [GINDIV])
C                  5 - marker size scale factor    (default: 1 [GINDIV])
C                  6 - polymarker color index      (default: 1 [GINDIV])
C                  7 - text font and precision     (default: 1 [GINDIV])
C                  8 - character expansion factor  (default: 1 [GINDIV])
C                  9 - character spacing           (default: 1 [GINDIV])
C                 10 - text color index            (default: 1 [GINDIV])
C                 11 - fill area interior style    (default: 1 [GINDIV])
C                 12 - fill area style index       (default: 1 [GINDIV])
C                 13 - fill area color index       (default: 1 [GINDIV])
C
      COMMON  /G01ADF/  MDPLIX  ,MDLTYP ,ADLWSC ,MDPLCI ,
     +                  MDPMIX  ,MDMTYP ,ADMSZS ,MDPMCI ,
     +                  MDTXIX  ,MDTXP  ,MDTXAL(2)      ,MDCHH  ,
     +                  MDCHOV(4)       ,MDTXFO ,MDTXPR ,ADCHXP ,
     +                  ADCHSP  ,MDTXCI ,
     +                  MDFAIX  ,MDPASZ(4)       ,MDPARF(2)     ,
     +                  MDFAIS  ,MDFASI  ,MDFACI  ,
     +                  MDASF(13)
        INTEGER         MDPLIX  ,MDLTYP ,MDPLCI
        REAL            ADLWSC
        INTEGER         MDPMIX  ,MDMTYP ,MDPMCI
        REAL            ADMSZS
        INTEGER         MDTXIX  ,MDTXP  ,MDTXAL ,MDTXFO
        INTEGER         MDTXPR  ,MDTXCI ,MDCHH  ,MDCHOV
        REAL            ADCHXP  ,ADCHSP
        INTEGER         MDFAIX  ,MDPASZ ,MDPARF ,MDFAIS ,MDFASI
        INTEGER         MDFACI  ,MDASF
C
C     G01ADC  -- Control variables for attribute deferral scheme.
C
C
C       valchg  - array of logical variables indicating whether
C                 value changes are pending for each of the
C                 attributes.  There is a one-to-one relationship
C                 between the elements of valchng and and the
C                 aspect/pen variables in G01ARQ, etc.
C
C       asfchg  - array of logical variables indicating whether
C                 changes are pending for each of the ASFs.  The
C                 is a one-to-one relationship between the elements
C                 of this array and the ASF array passed by the
C                 workstation interface.
C
C       anyasf  - logical scalar that says whether any ASF has
C                 changed -- the logical OR of asfchg(*).
C
C           [The following set of integer variables are pointers into
C           'valchg', associating each valchg element with the proper
C           attribute.  Values of the pointers are first column of
C           numbers in parentheses.]
C
C       ivplix  - polyline pen                  ( 1)    (  1)
C       ivltyp  - linetype                      ( 2)    (  2)
C       ivlwsc  - linewidth scale factor        ( 3)    (- 3)
C       ivplci  - polyline color index          ( 4)    (  4)
C
C       ivpmix  - polymarker pen                ( 5)    (  5)
C       ivmtyp  - marker type                   ( 6)    (  6)
C       ivmszs  - marker size scale factor      ( 7)    (- 7)
C       ivpmci  - polymarker color index        ( 8)    (  8)
C
C       ivtxix  - text pen                      ( 9)    (  9)
C       ivtxp   - text path                     (10)    ( 10)
C       ivtxal  - text alignment                (11)    ( 11)
C       ivchh   - character height              (12)    ( 13)
C       ivchov  - character orientation vectors (13)    ( 14)
C       ivtxfo  - text font                     (14)    ( 18)
C       ivtxpr  - text precision                (15)    ( 19)
C       ivchxp  - character expansion factor    (16)    (-20)
C       ivchsp  - character spacing             (17)    (-21)
C       ivtxci  - text color index              (18)    ( 22)
C
C       ivfaix  - fill area pen                 (19)    ( 23)
C       ivpasz  - fill area pattern size        (20)    ( 24)
C       ivparf  - fill area pattern ref point   (21)    ( 28)
C       ivfais  - fill area interior style      (22)    ( 30)
C       ivfasi  - fill area style index         (23)    ( 31)
C       ivfaci  - fill area color index         (24)    ( 32)
C
C       ivasf   - ASFs                          (25)    ( 33)
C
C       ip2aea  - array of integer pointers that associated each
C                 of the above indexes with the start position of
C                 the attribute in the attribute equivalencing
C                 arrays.  Second column of values in parentheses
C                 above are values.  The absolute value of the item
C                 is the pointer, the sign means that the quantities
C                 stored in the attribute arrays are real-valued if
C                 negative, integer-valued if positive.  Note that
C                 there is one last entry in IP2AEA giving the location
C                 beyond the last attribute item stored in the
C                 equivalencing arrays.
C
C       il2aea - lengths of the items pointed to by the ip2aea.
C
C           [The integer variables in the following set are pointers
C           into asfchg, associating each asfchg element with the proper
C           attribute.  Values of the pointers are in parentheses.
C           Note these indexes correspond exactly with the ASF array
C           passed by the workstation interface.]
C
C       ialtyp  - linetype                      ( 1)
C       ialwsc  - linewidth scale factor        ( 2)
C       iaplci  - polyline color index          ( 3)
C
C       iamtyp  - marker type                   ( 4)
C       iamszs  - marker size scale factor      ( 5)
C       iapmci  - polymarker color index        ( 6)
C
C       iatxfp  - text font and precision       ( 7)
C       iachxp  - character expansion factor    ( 8)
C       iachsp  - character spacing             ( 9)
C       iatxci  - text color index              (10)
C
C       iafais  - fill area interior style      (11)
C       iafasi  - fill area style index         (12)
C       iafaci  - fill area color index         (13)
C
C
C       agpend  - logical array indicating for each geometric primitive
C                 whether any changes to the attribute context of any
C                 sort (pen, aspect, ASF) are pending (deferred).
C                 The order of the array elements is:
C                 polyline, polymarker, text, fill area (note that cell
C                 array has no attributes).
C
C       ncgasf  - number of ASFs defined in CGM standard (18 currently).
C       ngkasf  - number of ASFs defined in GKS standard (13 currently).
C       masmap(ncgasf) - mapping array from CGM ASFs to GKS ASFs.  For
C                 ix=1..ncgasf, masmap(ix) is the index (into ASFCHG,
C                 MRASF, MSASF) of the functionally corresponding GKS
C                 ASF.  Note that multiple CGM ASFs may correspond with
C                 a single GKS ASF (e.g., font and precision -->font/prec).
C                 Also a CGM ASF may correspond with no GKS ASFs, e.g.,
C                 the perimeter attributes.  In this case, the pointer
C                 is set to zero.
C
      COMMON  /G01ADC/  VALCHG(37)      ,ANYASF ,AGPEND(4)      ,
     +                  IVPLIX  ,IVLTYP ,IVLWSC ,IVPLCI ,IVPMIX ,
     +                  IVMTYP  ,IVMSZS ,IVPMCI ,IVTXIX ,IVTXP  ,
     +                  IVTXAL  ,IVCHH  ,IVCHOV ,IVTXFO ,IVTXPR ,
     +                  IVCHXP  ,IVCHSP ,IVTXCI ,IVFAIX ,IVPASZ ,
     +                  IVPARF  ,IVFAIS ,IVFASI ,IVFACI ,IVASF  ,
     +                  IP2AEA(26)      ,IL2AEA(26)     ,
     +                  IALTYP  ,IALWSC ,IAPLCI ,IAMTYP ,IAMSZS ,
     +                  IAPMCI  ,IATXFP ,IACHXP ,IACHSP ,IATXCI ,
     +                  IAFAIS  ,IAFASI ,IAFACI ,
     +                  NCGASF  ,NGKASF ,MASMAP(18)
        LOGICAL         VALCHG  ,ANYASF ,AGPEND
        INTEGER         IVPLIX  ,IVLTYP ,IVLWSC ,IVPLCI ,IVPMIX
        INTEGER         IVMTYP  ,IVMSZS ,IVPMCI ,IVTXIX ,IVTXP
        INTEGER         IVTXAL  ,IVCHH  ,IVCHOV ,IVTXFO ,IVTXPR
        INTEGER         IVCHXP  ,IVCHSP ,IVTXCI ,IVFAIX ,IVPASZ
        INTEGER         IVPARF  ,IVFAIS ,IVFASI ,IVFACI ,IVASF
        INTEGER         IP2AEA  ,IL2AEA
        INTEGER         IALTYP  ,IALWSC ,IAPLCI ,IAMTYP ,IAMSZS
        INTEGER         IAPMCI  ,IATXFP ,IACHXP ,IACHSP ,IATXCI
        INTEGER         IAFAIS  ,IAFASI ,IAFACI
        INTEGER         NCGASF  ,NGKASF ,MASMAP
C
        LOGICAL         ASFCHG(13)
        EQUIVALENCE     (VALCHG(25),ASFCHG(1))
C
C
C     G01IO   --      Output buffer(s), pointers, etc.
C
C       mpxysz  - size in words of mpxpy.
C       mpxpy   - holding buffer, for results of coordinate
C                 conversions.
C       mobfsz  - size in words of moutbf.
C       moutbf  - primary output buffer, integer array.
C       mbfpos  - next available storage location in moutbf,
C                 offset in bits from beginning of moutbf.
C       mfglun  - logical unit number for metafile generator output.
C       mxbits  - number of bits in moutbf
C       mdtype  - metacode data type id
C       mnfflg  - new-frame flag set to 1 (GYES) by a 'begin picture',
C                 cleared (GNO) by the metafile writing routines
C       mbmflg  - begin-metafile flag, set to 1 (GYES) on 'open
C                 wkstn', cleared (GNO) by metafile writing routines.
C       mbmflg  - end-metafile flag, set to 0 (GNO) on 'open
C                 wkstn', set to 1 (GYES) upon 'close workstation'.
C       mrecnm  - the current record to be written out
C       mioflg  - this is a flag value which is set to -9999; it
C                 is to be used by independent utilities which may
C                 want to write to the metafile output unit of this
C                 GKS package if it has been opened.
C
C    G01CHR   --     Character data
C       mfname  - Metafile output file name.
C


      COMMON  /G01IO/   MIOFLG  ,MRECNM ,MPXYSZ ,MPXPY(256)     ,
     +                  MOBFSZ  ,MOUTBF(720)    ,MBFPOS ,
     +                  MFGLUN  ,MXBITS         ,MDTYPE ,
     +                  MNFFLG  ,MBMFLG ,MEMFLG
        INTEGER         MIOFLG  ,MRECNM ,MPXYSZ ,MPXPY  ,MOBFSZ ,
     +                  MBFPOS  ,MFGLUN ,MOUTBF ,MXBITS ,MDTYPE ,
     +                  MNFFLG  ,MBMFLG ,MEMFLG
      COMMON  /G01CHA/  MFNAME  ,MPNAME
      CHARACTER*80      MFNAME  ,MPNAME
C
      COMMON /GKSENU/ GYES,  GNO,   GCONDI, GALWAY, GACTIV, GINACT
      COMMON /GKSENU/ GEMPTY,GNEMPT,GPEND,  GNPEND, GCLIP , GNCLIP
C
      INTEGER         GYES,  GNO,   GCONDI, GALWAY, GACTIV, GINACT
      INTEGER         GEMPTY,GNEMPT,GPEND,  GNPEND, GCLIP , GNCLIP
C     G01OPC --    Common containing all CGM opcodes that are
C                  used/needed by the metafile generator wkstn.
C
C                  Each CGM element has a class and an id that
C                  together make up the opcode.  The following table
C                  gives the element name, class parameter name,
C                  and id parameter name.
C
C                  The CGM elements are grouped in classes.  The
C                  class code is invariant within a class.  For each
C                  class, a parameter is defined that is equivalent
C                  to all the individual named class parameters
C                  within the class, which are defined as locals.
C
C
C        Element name                         Class     Id
C        ------------                         ------    ------
C
C        (Delimiter class)                    (cldelm)
C        noop                                 clnoop    idnoop
C        BEGIN METAFILE                       clbegm    idbegm
C        END METAFILE                         clendm    idendm
C        BEGIN PICTURE                        clbegp    idendp
C        BEGIN PICTURE BODY                   clbgpb    idbgpb
C        END PICTURE                          clendp    idendp
C
C        (Metafile Descriptor Class)          (clmdes)
C        METAFILE VERSION                     clmver    idmver
C        METAFILE ELEMENTS LIST               clmelt    idmelt
C        METAFILE DEFAULTS REPLACEMENT        cldrep    iddrep
C        METAFILE DESCRIPTION                 cldscr    iddscr
C        FONT LIST                            clflst    idflst
C
C        (Picture Delimiter Class)            (clpdes)
C        COLOUR SELECTION MODE                clcsel    idcsel
C        VDC EXTENT                           clvext    idvext
C        BACKGROUND COLOR                     clbkgc    idbkgc
C
C        (Control Class)                      (clcntl)
C        VDC INTEGER PRECISION                clvint    idvint
C        CLIP RECTANGLE                       clcrec    idcrec
C        CLIP INDICATOR                       clclin    idclin
C
C        (Graphical Primitives Class)         (clprim)
C        POLYLINE                             clplin    idplin
C        POLYMARKER                           clpmrk    idpmrk
C        TEXT                                 cltext    idtext
C        POLYGON                              clpgon    idpgon
C        CELL ARRAY                           clcary    idcary
C        GENERALIZED DRAWING PRIMITIVE        clgdp     idgdp
C
C        (Primitive Attributes Class)         (clprat)
C        LINE BUNDLE INDEX                    cllbix    idlbix
C        LINE TYPE                            clltyp    idltyp
C        LINE WIDTH                           cllwid    idlwid
C        LINE COLOUR                          cllclr    idlclr
C        MARKER BUNDLE INDEX                  clmbix    idmbix
C        MARKER TYPE                          clmtyp    idmtyp
C        MARKER SIZE                          clmsiz    idmsiz
C        MARKER COLOUR                        clmclr    idmclr
C        TEXT BUNDLE INDEX                    cltbix    idtbix
C        TEXT FONT INDEX                      cltfon    idtfon
C        TEXT PRECISION                       cltpre    idtpre
C        CHARACTER EXPANSION FACTOR           clchex    idchex
C        CHARACTER SPACING                    clchsp    idchsp
C        TEXT COLOUR                          cltclr    idtclr
C        CHARACTER HEIGHT                     clchht    idchht
C        CHARACTER ORIENTATION                clchor    idchor
C        TEXT PATH                            cltxpa    idtxpa
C        TEXT ALIGNMENT                       cltxal    idtxal
C        FILL BUNDLE INDEX                    clfbix    idfbix
C        INTERIOR STYLE                       clints    idints
C        FILL COLOUR                          clfclr    idfclr
C        HATCH INDEX                          clhaix    idhaix
C        PATTERN INDEX                        clptix    idptix
C        FILL REFERENCE POINT                 clfrpt    idfrpt
C        PATTERN TABLE                        clptbl    idptbl
C        PATTERN SIZE                         clptsz    idptsz
C        COLOUR TABLE                         clctbl    idctbl
C        ASPECT SOURCE FLAGS                  clasfs    idasfs
C
C        (Escape Elements Class)              (clesce)
C        ESCAPE                               clesc     idesc
C
C        (External Elements Class)            (clexte)
C        MESSAGE                              clmess    idmess
C        APPLICATION DATA                     clapld    idapld
C
C
C
C
C  Id code parameters for every element, and class codes for each class.
C
      COMMON /G01OPC/ IDNOOP, IDBEGM, IDENDM, IDBEGP, IDBGPB, IDENDP
      COMMON /G01OPC/ IDMVER, IDMELT, IDDREP, IDCSEL, IDVEXT, IDVINT
      COMMON /G01OPC/ IDCREC, IDCLIN, IDPLIN, IDPMRK, IDTEXT, IDPGON
      COMMON /G01OPC/ IDCARY, IDGDP,  IDLBIX, IDLTYP, IDLWID, IDLCLR
      COMMON /G01OPC/ IDMBIX, IDMTYP, IDMSIZ, IDMCLR, IDTBIX, IDTFON
      COMMON /G01OPC/ IDTPRE, IDCHEX, IDCHSP, IDTCLR, IDCHHT, IDCHOR
      COMMON /G01OPC/ IDTXPA, IDTXAL, IDFBIX, IDINTS, IDFCLR, IDHAIX
      COMMON /G01OPC/ IDPTIX, IDFRPT, IDPTBL, IDPTSZ, IDCTBL, IDASFS
      COMMON /G01OPC/ IDESC,  IDMESS, IDAPLD, IDBKGC, IDDSCR, IDFLST
      COMMON /G01OPC/ CLDELM, CLMDES, CLPDES, CLCNTL, CLPRIM, CLPRAT
      COMMON /G01OPC/ CLESCE, CLEXTE
C
C  Parameter data types.
C
      INTEGER         IDNOOP, IDBEGM, IDENDM, IDBEGP, IDBGPB, IDENDP
      INTEGER         IDMVER, IDMELT, IDDREP, IDCSEL, IDVEXT, IDVINT
      INTEGER         IDCREC, IDCLIN, IDPLIN, IDPMRK, IDTEXT, IDPGON
      INTEGER         IDCARY, IDGDP,  IDLBIX, IDLTYP, IDLWID, IDLCLR
      INTEGER         IDMBIX, IDMTYP, IDMSIZ, IDMCLR, IDTBIX, IDTFON
      INTEGER         IDTPRE, IDCHEX, IDCHSP, IDTCLR, IDCHHT, IDCHOR
      INTEGER         IDTXPA, IDTXAL, IDFBIX, IDINTS, IDFCLR, IDHAIX
      INTEGER         IDPTIX, IDFRPT, IDPTBL, IDPTSZ, IDCTBL, IDASFS
      INTEGER         IDESC,  IDMESS, IDAPLD, IDBKGC, IDDSCR, IDFLST
      INTEGER         CLDELM, CLMDES, CLPDES, CLCNTL, CLPRIM, CLPRAT
      INTEGER         CLESCE, CLEXTE
C
C Class code parameters for every element.
C
      INTEGER         CLNOOP, CLBEGM, CLENDM, CLBEGP, CLBGPB, CLENDP
      INTEGER         CLMVER, CLMELT, CLDREP, CLCSEL, CLVEXT, CLVINT
      INTEGER         CLCREC, CLCLIN, CLPLIN, CLPMRK, CLTEXT, CLPGON
      INTEGER         CLCARY, CLGDP,  CLLBIX, CLLTYP, CLLWID, CLLCLR
      INTEGER         CLMBIX, CLMTYP, CLMSIZ, CLMCLR, CLTBIX, CLTFON
      INTEGER         CLTPRE, CLCHEX, CLCHSP, CLTCLR, CLCHHT, CLCHOR
      INTEGER         CLTXPA, CLTXAL, CLFBIX, CLINTS, CLFCLR, CLHAIX
      INTEGER         CLPTIX, CLFRPT, CLPTBL, CLPTSZ, CLCTBL, CLASFS
      INTEGER         CLESC,  CLMESS, CLAPLD, CLBKGC, CLDSCR, CLFLST
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
C
C     WORKSTATION STATE LIST.
C
C       LENGTH OF COLOR TABLE ARRAYS.
        DATA  MOLMAX/256/
C
C     WORKSTATION DESCRIPTION TABLE
C
C       WORKSTATION TYPE AND CATEGORY
        DATA LWTYPE,LWKCAT/1,4/
C
C       METAFILE VERSION.
        DATA MVERSN/1/
C
C     INTERNAL CONSTANTS.
C
C
C       LAST FUNCTION CODE, LAST CONTINUATION FLAG.
        DATA  MCODES/0/,  MCONTS/0/
C
C       VDC, COLOR INDEX, DIRECT COLOR COMPONENT FIELD WIDTHS.
        DATA  MVDCFW/16/,  MCIXFW/8/,  MDCCFW/8/
        DATA  MIXFW/16/,  MINTFW/16/,  MEFW/16/
C
C       2**MDCCFW - 1
C       (SHOULD BE TIED PARAMETRICALLY TO MCIXFW).
        DATA  MDCCRG/255/
C
C       MIN AND MAX VDC ADDRESS LIMITS IN X AND Y DIRECTIONS,
C       THE VDC EXTENT (THE TWO CONCEPTS SHOULD BE SEPARATED LATER).
        DATA  MINXVD/0/,  MAXXVD/32767/,  MINYVD/0/,  MAXYVD/32767/
C
C       NDC TO VDC MAPPING COEFFICIENTS, X AND Y DIRECTIONS.
        DATA  MXOFF/0/,  MXSCAL/32767/,  MYOFF/0/,  MYSCAL/32767/
C
C       COLOR TABLE CHANGE, BACKGROUND COLOR CHANGE.
        DATA  MCTCHG/0/,  MBCCHG/0/
C
C       SIZE OF HOLDING BUFFER
        DATA  MPXYSZ/256/
C
C       INITIAL VALUE FOR CURRENT FRAME NUMBER
        DATA  MCFRM/1/
C
C       PRECISION (IN BITS) OF THE TWO COMPONENTS OF THE REPRESENTATION
C       OF REAL NUMBERS IN THE METAFILE
        DATA  MCFPP/16/
C
C     DEFAULT ATTRIBUTE CONTEXT.
C
C
C       POLYLINE INDEX, LINETYPE, LINE COLOR INDEX, LINEWIDTH SCALE.
        DATA  MDPLIX/1/,  MDLTYP/1/,  MDPLCI/1/,  ADLWSC/1.0/
C
C       POLYMARKER INDEX, MARKER TYPE, MARKER COLOR INDEX, MARKER SCALE.
        DATA  MDPMIX/1/,  MDMTYP/3/,  MDPMCI/1/,  ADMSZS/1.0/
C
C       TEXT INDEX, TEXT PATH, TEXT ALIGNMENT, TEXT FONT.
        DATA  MDTXIX/1/,  MDTXP/0/,  MDTXAL/0,0/,  MDTXFO/1/
C
C       TEXT PRECISION, TEXT COLOR INDEX, CHARACTER HEIGHT.
C       (MDCHH SHOULD EVENTUALLY BE TIED PARAMETRICALLY TO VDC EXTENT).
        DATA  MDTXPR/0/,  MDTXCI/1/,  MDCHH/328/
C
C       CHARACTER ORIENTATION VECTORS.
C       (MDCHOV SHOULD EVENTUALLY BE TIED PARAMETRICALLY TO VDC EXTENT).
        DATA  MDCHOV/0,328,328,0/
C
C       CHARACTER EXPANSION FACTOR, CHARACTER SPACING.
        DATA  ADCHXP/1.0/,  ADCHSP/0.0/
C
C       FILL AREA INDEX, PATTERN SIZE, PATTERN REF POINT.
C       (MDPASZ SHOULD EVENTUALLY BE TIED PARAMETRICALLY TO VDC EXTENT).
        DATA  MDFAIX/1/,  MDPASZ/0,32767,32767,0/,  MDPARF/0,0/
C
C       FILL AREA INTERIOR STYLE, STYLE INDEX, COLOR INDEX.
        DATA  MDFAIS/0/,  MDFASI/1/,  MDFACI/1/
C
C       ASF VALUES.
        DATA  MDASF/13*1/
C
C       POINTERS INTO ATTRIBUTE STRUCTURE, VALCHG VARIABLES.
C
        DATA  IVPLIX/ 1/,  IVLTYP/ 2/,  IVLWSC/ 3/,  IVPLCI/ 4/
        DATA  IVPMIX/ 5/,  IVMTYP/ 6/,  IVMSZS/ 7/,  IVPMCI/ 8/
        DATA  IVTXIX/ 9/,  IVTXP/10/,   IVTXAL/11/,  IVCHH/12/
        DATA  IVCHOV/13/,  IVTXFO/14/,  IVTXPR/15/,  IVCHXP/16/
        DATA  IVCHSP/17/,  IVTXCI/18/,  IVFAIX/19/,  IVPASZ/20/
        DATA  IVPARF/21/,  IVFAIS/22/,  IVFASI/23/,  IVFACI/24/
        DATA  IVASF/25/
C
C       POINTERS INTO ATTRIBUTE EQUIVALENCING ARRAYS.
C
        DATA  IP2AEA/  1,  2, -3,  4,  5,  6, -7,  8,  9, 10, 11, 13,
     +                14, 18, 19,-20,-21, 22, 23, 24, 28, 30, 31, 32,
     +                33, 46 /
C
C       POINTERS INTO ASF ARRAYS.
C
        DATA  IALTYP/ 1/,  IALWSC/ 2/,  IAPLCI/ 3/,  IAMTYP/ 4/
        DATA  IAMSZS/ 5/,  IAPMCI/ 6/,  IATXFP/ 7/,  IACHXP/ 8/
        DATA  IACHSP/ 9/,  IATXCI/10/,  IAFAIS/11/,  IAFASI/12/
        DATA  IAFACI/13/
C
C       NBR OF CGM AND GKS ASFS, GKS TO CGM MAPPING ARRAY.
C
        DATA  NCGASF/18/,  NGKASF/13/
        DATA  MASMAP/1,2,3,4,5,6,7,7,8,9,10,11,13,12,12,0,0,0/
C
C       GKS ENUMERATIVES.
C
        DATA  GNO/0/,      GYES/1/,     GCONDI/0/,   GALWAY/1/
        DATA  GINACT/0/,   GACTIV/1/,   GNEMPT/0/,   GEMPTY/1/
        DATA  GNPEND/0/,   GPEND/1/,    GCLIP/1/,    GNCLIP/0/
C
C       CGM OPCODES, CLASS AND ID CODES.
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
C       FLAG VALUE IN IO COMMON BLOCK
C
        DATA  MIOFLG/-9999/
C
C       METAFILE FILE NAME, PICTURE NAME
C
        DATA MFNAME/'GMETA'/
        DATA MPNAME/
     -     '
     -                    '/
        END
