C
C $Id: plchhq.f,v 1.27 2008-07-27 00:17:20 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PLCHHQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C This is the high-quality character-drawing routine.
C
C
C D E C L A R A T I O N S
C
C
      CHARACTER CHRS*(*)
C
C PARAMETER statements.  The value of MDGU declared here depends on the
C maximum number of elements that can ever be returned in the array RDGU
C by the routine PCCFFF.  One should use here whatever that value is,
C plus a fudge factor to compensate for the fact that, when mapping is
C turned on, PLCHHQ interpolates X/Y coordinates along long straight
C portions of character boundaries that were originally defined by just
C two points at the ends.  If the value of MDGU is changed, the common
C block PCSVEM must be modified in all routines in which it is declared;
C the PARAMETER statement only occurs in this one.
C
      PARAMETER (MDGU=7000,MCRA=MDGU/2,MPCS=32,NCSO=256)
C
C COMMON block declarations.  PCPRMS holds user-accessible internal
C parameters that do not affect the routine PLCHMQ.  PCSVEM holds other
C variables that are either used in more than one routine or that need
C to be saved from one call to the next.  PCPFLQ holds the values of
C internal parameters that affect routines besides PLCHHQ.
C
C Note that the sizes of IDDA and INDA may be reduced to match the
C values of IDDL and INDL computed below.
C
      COMMON /PCPRMS/ ADDS,CONS,DSTB,DSTL,DSTR,DSTT,HPIC(3),IBNU,
     +                IBXC(3),IBXF,ICEN,IORD,IOUC,IOUF,IPCC,IQUF,
     +                ISHC,ISHF,ITEF,JCOD,LSCI(16),NFCC,NODF,RBXL,
     +                RBXM,RBXX,RBXY,ROLW,RPLW,RSLW,SHDX,SHDY,SIZA,
     +                SSIC,SSPR,SUBS,VPIC(3),WPIC(3),XBEG,XCEN,XEND,
     +                XMUL(3),YBEG,YCEN,YEND,YMUL(3),ZINX,ZINY,ZINZ
      SAVE   /PCPRMS/
C
      COMMON /PCSVEM/ ICOD,IDDA(8625),IDDL,RDGU(MDGU),IDPC(256),IERU,
     +                INDA(789),INDL,INIT,IVCO,IVDU,NBPW,NPPW
      SAVE   /PCSVEM/
C
      COMMON /PCPFLQ/ IMAP,OORV,RHTW
      SAVE   /PCPFLQ/
C
C Define X and Y coordinate arrays to be used in calls to GPL and GFA.
C
      DIMENSION XCRA(MCRA),YCRA(MCRA)
C
C Declare some arrays in which to put pointers to pieces of a character
C that lies partly inside and partly outside the area visible under a
C given mapping.  We also have to save the X and Y coordinates at the
C beginning and end of each visible chunk of the character, so as to
C be able to interpolate points along the visible/invisible boundary
C between the visible chunks.
C
      DIMENSION IPCB(MPCS),IPCE(MPCS)
      DIMENSION XPCB(MPCS),YPCB(MPCS)
      DIMENSION XPCE(MPCS),YPCE(MPCS)
C
C Define arrays in which to put multipliers representing the combined
C effects of changing 'PH', 'PW', 'IH', 'IW', 'CH', 'CW', and the
C zoom factor.
C
      DIMENSION XMZM(3),YMZM(3)
C
C Define arrays implementing the subscript/superscript stack.
C
      DIMENSION NLEV(5),XCNO(5),YCNO(5),XRGO(5),YRGO(5),ICSO(5),ISZO(5),
     +          CSTO(5),SNTO(5)
C
C Define an array to hold the codes associated with 19 "special" ASCII
C characters which might occur in CHRS and for which we will attempt
C to provide a proper response.
C
      DIMENSION ISPC(19)
C
C Define a character array in which to define the characters to be
C associated with values of NDPC from 1 to 96.  DPC was the set of
C six-bit character codes used on the CDC 7600 (the machine on which
C PWRITX, the predecessor of PLCHHQ, was first implemented) and the
C digitization arrays of the PWRITX database are arranged in DPC order.
C For NDPC = 1 to 47, CDPC(NDPC) is the character that the DPC codes
C associated with that index value.  For NDPC = 48 to 73, CDPC(NDPC)
C is a lower-case letter.  For NDPC = 74 to 95, CDPC(NDPC) is an ASCII
C character which may be used in the input character string and for
C which we wish to select a meaningful character from the PWRITX
C database.  CDPC(96) is a blank; NDPC = 96 is used only as a signal
C that an octal function code has been found in the input character
C string, selecting a particular character from the PWRITX database.
C
      CHARACTER*1 CDPC(96)
C
C Define an array in which to define the ASCII decimal equivalents of
C the characters in CDPC.
C
      DIMENSION IASC(96)
C
C Declare an array in which to put some standard character heights,
C for use in calls to PCCFFF.
C
      DIMENSION SPIC(3)
C
C Define arrays in which to save the information required to draw the
C characters resulting from a single string.  These arrays allow us to
C easily do the separate passes needed for shadows and outlines and to
C do each of the passes either from left to right or from right to left
C in the character string.
C
      DIMENSION XCSV(NCSO),YCSV(NCSO),XMSV(NCSO),YMSV(NCSO),NFSV(NCSO),
     +          NDSV(NCSO),INSV(NCSO),NASV(NCSO),IPSV(NCSO)
C
      CHARACTER*(NCSO) CHSV
C
C Define arrays in which to save the definitions of the window and
C the viewport of the current normalization transformation.
C
      DIMENSION WNTC(4),VNTC(4)
C
C Declare an array in which to save text extent information for specific
C characters from specific fonts.
C
      DIMENSION STEI(6,95,37)
      SAVE STEI
C
C Define the characters to be associated with values of NDPC from 1
C to 96.
C
      DATA CDPC / 'A','B','C','D','E','F','G','H','I','J',
     +            'K','L','M','N','O','P','Q','R','S','T',
     +            'U','V','W','X','Y','Z','0','1','2','3',
     +            '4','5','6','7','8','9','+','-','*','/',
     +            '(',')','$','=',' ',',','.','a','b','c',
     +            'd','e','f','g','h','i','j','k','l','m',
     +            'n','o','p','q','r','s','t','u','v','w',
     +            'x','y','z','!','"','#','%','&',':',';',
     +            '<','>','?','@','[','\\',']','{','|','}',
     +            '~','''','^','_','`',' '               /
C
C Define the ASCII decimal equivalents of the characters in CDPC.
C
      DATA IASC /  65, 66, 67, 68, 69, 70, 71, 72, 73, 74,
     +             75, 76, 77, 78, 79, 80, 81, 82, 83, 84,
     +             85, 86, 87, 88, 89, 90, 48, 49, 50, 51,
     +             52, 53, 54, 55, 56, 57, 43, 45, 42, 47,
     +             40, 41, 36, 61, 32, 44, 46, 97, 98, 99,
     +            100,101,102,103,104,105,106,107,108,109,
     +            110,111,112,113,114,115,116,117,118,119,
     +            120,121,122, 33, 34, 35, 37, 38, 58, 59,
     +             60, 62, 63, 64, 91, 92, 93,123,124,125,
     +            126, 39, 94, 95, 96, 32                /
C
C Define the number of card images used to represent the contents of
C the arrays INDA and IDDA.  These are used to compute the number of
C binary words to be read into these arrays.
C
      DATA ICND,ICDD / 49,575 /
C
C Define indices for font, size, and case definitions.  These indices
C are used as displacements into the array INDA.  The sum of a font
C index (Roman or Greek), a size index (principal, indexical, or
C cartographic), a case index (upper or lower), and the DPC code for
C a particular character uniquely defines the index of an entry in
C INDA pointing to the proper digitization in the array IDDA.
C
      DATA IFRO,IFGR      / 0,384     /
      DATA ISZP,ISZI,ISZC / 0,128,256 /
      DATA ICSU,ICSL      / 0, 64     /
C
C Define the codes associated with 19 "special" ASCII characters which
C might possibly appear in CHRS.
C
      DATA ISPC / 93,484,485,111,421,91,92,99,100,94,321,105,337,106,
     +            745,336,746,431,483 /
C
C Define the standard character heights for principal, indexical, and
C cartographic sizes, for use in calls to PCCFFF.
C
      DATA SPIC / 21. , 13. , 9. /
C
C Initialize the array of text-extent information.
C
      DATA STEI / 21090*0. /
C
C
C I N I T I A L I Z A T I O N
C
C
C Do calls forcing BLOCKDATAs to be loaded from a binary library.
C
      CALL PCBLDA
      CALL PCBDFF
C
C Check for an uncleared prior error.
C
      IF (ICFELL('PLCHHQ - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If it hasn't already been done, do that part of the initialization
C which is done only once per loading of the routine.
C
      IF (INIT.EQ.0) THEN
C
        INIT=1
C
C NBPW is the number of bits per word and NPPW is the number of 15-bit
C parcels per word.  INDL and IDDL are the number of words to be read
C into the arrays INDA and IDDA, respectively.
C
        NBPW=I1MACH(5)
        NPPW=NBPW/15
        INDL=(ICND*16-1)/NPPW+1
        IDDL=(ICDD*16*15-1)/NBPW+1
C
C Define the error file logical unit number.
C
        IERU=I1MACH(4)
C
C If it hasn't already been done, initialize the numeric equivalent of
C the function code character.
C
        IF (NFCC.LT.0) NFCC=ICHAR(':')
C
C Initialize the table IDPC.  For a given character "c", IDPC(ICHAR(c))
C is the integer value of the equivalent of "c" in an augmented DPC
C character set.  DPC was the set of six-bit character codes used on
C the CDC 7600 (the machine on which the predecessor of PLCHHQ was
C first implemented) and the digitization arrays are arranged in DPC
C order.  First, all undefined entries are zeroed.
C
        DO 101 I=1,256
          IDPC(I)=0
  101   CONTINUE
C
C Then, the entries are defined, one at a time.
C
        DO 102 NDPC=1,95
          NCOL=ICHAR(CDPC(NDPC))
          IF (NCOL.GE.1.AND.NCOL.LE.256) THEN
            IF (IDPC(NCOL).EQ.0) IDPC(NCOL)=NDPC
          END IF
  102   CONTINUE
C
C Set the flag ICOD to force loading of the dataset implied by the
C user-set or default value of JCOD.
C
        ICOD=-1
C
      END IF
C
C Get the coordinates of the string position.  These are in the
C fractional system if mapping is turned off; otherwise, they are
C in an arbitrary X/Y system.
C
      IF (IMAP.LE.0) THEN
        XFRA=CUFX(XPOS)
        IF (ICFELL('PLCHHQ',2).NE.0) RETURN
        YFRA=CUFY(YPOS)
        IF (ICFELL('PLCHHQ',3).NE.0) RETURN
      ELSE
        XFRA=XPOS
        YFRA=YPOS
      END IF
C
C Determine the resolution of the plotter, as declared by default or
C by the user.
C
      CALL GETUSV ('XF',IRSX)
      IF (ICFELL('PLCHHQ',4).NE.0) RETURN
      RSLN=2.**IRSX-1.
C
C Determine a multiplier for the digitized size which will make the
C characters have the size requested by the user.
C
      IF (IMAP.LE.0) THEN
        IF (SIZE.LE.0.) THEN
          SIZM=ABS(SIZE)/1023.
        ELSE IF (SIZE.LT.1.) THEN
          SIZM=SIZE/WPIC(1)
        ELSE
          SIZM=(SIZE/RSLN)/WPIC(1)
        END IF
      ELSE
        SIZM=SIZE/WPIC(1)
      END IF
C
C If high-quality characters are being used, multiply by a fudge factor
C whose default value of 8/9 makes characters produced by PLCHHQ have
C the same height as those produced by PLCHMQ.  (By setting the value
C of this fudge factor to 1, a user can make things work more or less
C the way they used to.)
C
      IF (IQUF.EQ.0) SIZM=SIZA*SIZM
C
C If high-quality characters are to be used, load the high-quality
C character dataset.
C
      IF (IQUF.EQ.0) THEN
C
C If no dataset has been read, or if the user has requested the other
C one of the two, read the proper one.  The first time a given dataset
C is read, verify that it has been done correctly.
C
        IF (ICOD.NE.JCOD) THEN
C
          IBNS=IBNU
          CALL PCFOPN (IBNU,0)
          IF (ICFELL('PLCHHQ',5).NE.0) RETURN
C
          IF (JCOD.NE.0) THEN
            CALL PCFRED (IBNU,0,INDA,INDL)
            IF (ICFELL('PLCHHQ',6).NE.0) RETURN
            CALL PCFRED (IBNU,0,IDDA,IDDL)
            IF (ICFELL('PLCHHQ',7).NE.0) RETURN
          END IF
C
          CALL PCFRED (IBNU,0,INDA,INDL)
          IF (ICFELL('PLCHHQ',8).NE.0) RETURN
          CALL PCFRED (IBNU,0,IDDA,IDDL)
          IF (ICFELL('PLCHHQ',9).NE.0) RETURN
C
          CALL PCFCLS (IBNU,0)
          IF (ICFELL('PLCHHQ',10).NE.0) RETURN
          IBNU=IBNS
C
          IERR=0
          IF (JCOD.EQ.0.AND.IVCO.EQ.0) THEN
            CALL PCCCHK (IERR)
            IVCO=1
          END IF
          IF (JCOD.NE.0.AND.IVDU.EQ.0) THEN
            CALL PCDCHK (IERR)
            IVDU=1
          END IF
          IF (IERR.NE.0) THEN
            CALL SETER ('PLCHHQ - DATASET NOT LOADED CORRECTLY',IERR,1)
            RETURN
          END IF
C
          ICOD=JCOD
C
        END IF
C
C If lower-quality characters are to be used, save the PLCHMQ parameter
C specifying the ratio of character height to character width.
C
      ELSE
C
        RHWO=RHTW
C
      END IF
C
C Set the orientation angle, in radians, and determine the components
C of various vectors in the direction of the string and at right angles
C to it.
C
      ANGR=.017453292519943*ANGD
C
      SINO=SIN(ANGR)
      COSO=COS(ANGR)
      STSO=SIZM*SINO
      STCO=SIZM*COSO
      SINM=SIN(ANGR-1.57079632679489)
      COSM=COS(ANGR-1.57079632679489)
      SINP=SIN(ANGR+1.57079632679489)
      COSP=COS(ANGR+1.57079632679489)
C
C Find the number of characters in the user's input string.
C
      NCHR=LEN(CHRS)
C
C Initialize the number of the last generated character saved for the
C drawing passes.  (If there are fewer than NCSO characters, they can
C all be saved at once; otherwise, we have to make more than one pass
C through the input string.)
C
      ILCS=0
C
C Initialize the number of the last character generated during the
C first pass.  (When more than NCSO characters are defined by the
C input string, it is necessary to return to this point and repeat
C this pass, saving another batch of characters.)
C
  103 ILCG=0
C
C Zero the count of characters for which information has been saved.
C
      NCSV=0
C
C During this pass, we use an arbitrary starting position, run through
C the character string without drawing anything, and see what the final
C position is.  Then, during the drawing passes, we use this information
C to properly position the characters drawn.
C
      XBEG=XFRA
      YBEG=YFRA
C
C Set the current font number to the appropriate default.
C
      NFNT=NODF
C
C Make three copies of the starting position for various purposes.
C (XBOL,YBOL) is the point at the beginning of the current line.
C (XCEN,YCEN) is the point at the center of the last character.
C (XRGT,YRGT) is the point at the right end of the last character.
C
      XBOL=XBEG
      YBOL=YBEG
      XCEN=XBEG
      YCEN=YBEG
      XRGT=XBEG
      YRGT=YBEG
C
C Set the index of the last character examined in the input.
C
      ICHR=0
C
C Initialize the index offsets for font, size, and case to Roman,
C principal, and upper, respectively.
C
      IFNT=IFRO
      ISZE=ISZP
      ICSE=ICSU
C
C Initialize the index that says what size the current character is.
C
      IPIC=1
C
C Zero the character counts for "writing down" and for case change.
C
      NDWN=0
      NCSE=0
C
C Zero the variable defining the vertical space used by the previous
C character, which is used for vertical spacing when "writing down".
C
      VEPC=0.
C
C Zero variables which affect the spacing between characters.
C
      ADDP=0.
      SUBP=0.
C
C Set the current subscript/superscript level.
C
      NSSL=0
C
C Set the flag to indicate that we're not currently processing function
C codes.
C
      IPFC=0
C
C Initialize the multipliers representing the combined effects of
C changing 'PH', 'PW', 'IH', 'IW', 'CH', 'CW', and the zoom factor.
C
      XMZM(1)=ZINX*ZINZ*XMUL(1)
      XMZM(2)=ZINX*ZINZ*XMUL(2)
      XMZM(3)=ZINX*ZINZ*XMUL(3)
      YMZM(1)=ZINY*ZINZ*YMUL(1)
      YMZM(2)=ZINY*ZINZ*YMUL(2)
      YMZM(3)=ZINY*ZINZ*YMUL(3)
C
C If text extent quantities are to be computed, initialize the needed
C quantities.
C
      IF (ITEF.EQ.0.AND.ICEN.EQ.0.AND.IBXF.EQ.0) THEN
        IPSS=1
      ELSE
        IPSS=2
        DSTL=-1.E6
        DSTR=-1.E6
        DSTB=-1.E6
        DSTT=-1.E6
      END IF
C
C
C P R O C E S S   T H E   N E X T   C H A R A C T E R
C
C
C If there are no more characters in the string, jump to end-of-string
C processor.
C
  104 IF (ICHR.GE.NCHR) GO TO 108
C
C Get the next character from the string.  NCOL is the number of the
C character in the character collating sequence on the machine on
C which PLCHHQ is being run.
C
      ICHR=ICHR+1
      NCOL=ICHAR(CHRS(ICHR:ICHR))
C
C If the function-code signal character is encountered, flip the value
C of the function-code processing flag and go get the next character.
C Two function-code signal characters in a row are treated as a single
C occurrence of the character itself, selecting a character to be drawn.
C
      IF (NCOL.EQ.NFCC) THEN
        IF (IPFC.EQ.0.AND.ICHR.LT.NCHR) THEN
          NNXT=ICHAR(CHRS(ICHR+1:ICHR+1))
        ELSE
          NNXT=-1
        END IF
        IF (NNXT.EQ.NFCC) THEN
          ICHR=ICHR+1
        ELSE
          IPFC=1-IPFC
          GO TO 104
        END IF
      END IF
C
C NDPC is the number of the character in the DPC collating sequence.
C
      NDPC=IDPC(NCOL)
C
C If function codes are being processed, jump to the function code
C processor.
C
      IF (IPFC.NE.0) GO TO 128
C
C If the character is not one of the "standard" ones, take special
C action.  Lower-case alphabetic characters behave like upper-case
C alphabetic characters, but force the case to "lower".  "Special"
C characters yield a particular index into the PWRITX database or, in
C three cases, a blank.  As a last resort, a star is used.
C
      IF (NDPC.LT.1.OR.NDPC.GT.47) THEN
        IF (NDPC.GE.48.AND.NDPC.LE.73) THEN
          INDP=IFNT+ISZE+ICSL+NDPC-47
        ELSE IF (NDPC.GE.74.AND.NDPC.LE.92) THEN
          INDP=ISPC(NDPC-73)
        ELSE IF (NDPC.GE.93.AND.NDPC.LE.95) THEN
          INDP=45
        ELSE
          INDP=358
        END IF
        GO TO 105
      END IF
C
C Compute the index of the entry in INDA which points to the proper
C digitization in IDDA.
C
      INDP=IFNT+ISZE+ICSE+NDPC
C
C If high-quality characters are being used, get the digitization of
C the character in the array RDGU; if no digitization is found, try a
C star instead; if that can't be found, there's a more serious error.
C In any case, define the distances from the center of the character to
C its left and right ends.  Note: I have some doubts about the setting
C of CHFS to .1 in the following code (though it seems to have worked
C okay for a very long time).
C
  105 IF (IQUF.EQ.0) THEN
        IF (NFNT.EQ.0.OR.NDPC.LT.1.OR.NDPC.GT.95.OR.INDP.EQ.95) THEN
          CALL PCEXCD (INDP,IPSS,NDGU)
        ELSE
          IF (ICSE.NE.ICSL.OR.NDPC.LT.1.OR.NDPC.GT.26) THEN
            KDPC=NDPC
          ELSE
            KDPC=NDPC+47
          END IF
          NASC=IASC(KDPC)
          MFNT=MOD(NFNT,100)
          IF (ITEF.NE.0.AND.ANGD.EQ.360..AND.
     +        STEI(1,KDPC,MFNT).NE.0.) THEN
            RDGU(1)=SPIC(IPIC)*STEI(1,KDPC,MFNT)
            RDGU(2)=SPIC(IPIC)*STEI(2,KDPC,MFNT)
            RDGU(3)=SPIC(IPIC)*STEI(3,KDPC,MFNT)
            RDGU(4)=SPIC(IPIC)*STEI(4,KDPC,MFNT)
            RDGU(5)=SPIC(IPIC)*STEI(5,KDPC,MFNT)
            RDGU(6)=SPIC(IPIC)*STEI(6,KDPC,MFNT)
            NDGU=6
          ELSE
            IF (NFNT.GE.1.AND.NFNT.LE.20) THEN
              CALL PCCFFC (IPSS,IBNU,NFNT,NASC,SPIC(IPIC),
     +                                     RDGU,MDGU,NDGU)
              IF (ICFELL('PLCHHQ',11).NE.0) RETURN
            ELSE
              IF (IMAP.LE.0) THEN
                CHFS=SIZM*HPIC(IPIC)
              ELSE
                CHFS=.1
              END IF
              CALL PCCFFF (IPSS,IBNU,NFNT,NASC,SPIC(IPIC),CHFS,
     +                                          RDGU,MDGU,NDGU)
              IF (ICFELL('PLCHHQ',12).NE.0) RETURN
            END IF
            IF (NDGU.EQ.0) THEN
              CALL PCCFFC (IPSS,IBNU,1,NASC,SPIC(IPIC),RDGU,MDGU,NDGU)
              IF (ICFELL('PLCHHQ',13).NE.0) RETURN
              IF (NDGU.EQ.0) CALL PCEXCD (INDP,IPSS,NDGU)
            END IF
            IF (NDGU.GT.2.AND.STEI(1,KDPC,MFNT).EQ.0.) THEN
              STEI(1,KDPC,MFNT)=RDGU(1)/SPIC(IPIC)
              STEI(2,KDPC,MFNT)=RDGU(2)/SPIC(IPIC)
              STEI(3,KDPC,MFNT)=0.
              STEI(4,KDPC,MFNT)=0.
              STEI(5,KDPC,MFNT)=0.
              STEI(6,KDPC,MFNT)=0.
              DO 106 I=3,NDGU-1,2
                IF (RDGU(I).GT.-2047.) THEN
                  STEI(3,KDPC,MFNT)=MIN(STEI(3,KDPC,MFNT),
     +                                  RDGU(I  )/SPIC(IPIC))
                  STEI(4,KDPC,MFNT)=MIN(STEI(4,KDPC,MFNT),
     +                                  RDGU(I+1)/SPIC(IPIC))
                  STEI(5,KDPC,MFNT)=MAX(STEI(5,KDPC,MFNT),
     +                                  RDGU(I  )/SPIC(IPIC))
                  STEI(6,KDPC,MFNT)=MAX(STEI(6,KDPC,MFNT),
     +                                  RDGU(I+1)/SPIC(IPIC))
                END IF
  106         CONTINUE
            END IF
          END IF
        END IF
        IF (NDGU.EQ.0) THEN
          CALL PCEXCD (358,IPSS,NDGU)
          IF (NDGU.EQ.0) THEN
            CALL SETER ('PLCHHQ - INTERNAL LOGIC ERROR (NDGU = 0) - SEE
     +CONSULTANT',14,1)
            RETURN
          END IF
        END IF
        IF (CONS.EQ.0.) THEN
          DTLE=-XMZM(IPIC)*RDGU(1)
          DTRE= XMZM(IPIC)*RDGU(2)
        ELSE IF (CONS.LT.0.) THEN
          DTLE=-CONS
          DTRE=-CONS
        ELSE
          DTLE=CONS*WPIC(IPIC)
          DTRE=CONS*WPIC(IPIC)
        END IF
      ELSE
        DTLE=.5*WPIC(IPIC)
        DTRE=.5*WPIC(IPIC)
      END IF
C
C If this character is to be "written down" (that is to say, beneath
C the previous character), arrange for that.  Otherwise, just zero the
C "last character written down" flag.  Note that, while subscripting
C or superscripting is in progress, "writing down" is suspended.
C
      IF (NDWN.GT.0.AND.NSSL.EQ.0) THEN
        LCWD=1
        XCEN=XCEN+.5*SIZM*(VEPC+VPIC(IPIC))*COSM
        YCEN=YCEN+.5*SIZM*(VEPC+VPIC(IPIC))*SINM
        IF (ADDP.NE.0.) THEN
          IF (ADDS.LT.0.) THEN
            XCEN=XCEN+SIZM*ADDP*COSM
            YCEN=YCEN+SIZM*ADDP*SINM
          ELSE
            XCEN=XCEN+SIZM*.5*(ADDP+ADDS*WPIC(IPIC))*COSM
            YCEN=YCEN+SIZM*.5*(ADDP+ADDS*WPIC(IPIC))*SINM
          END IF
        END IF
        IF (SUBP.NE.0.) THEN
          IF (SUBS.LT.0.) THEN
            XCEN=XCEN-SIZM*SUBP*COSM
            YCEN=YCEN-SIZM*SUBP*SINM
          ELSE
            XCEN=XCEN-SIZM*.5*(SUBP+SUBS*WPIC(IPIC))*COSM
            YCEN=YCEN-SIZM*.5*(SUBP+SUBS*WPIC(IPIC))*SINM
          END IF
        END IF
        NDWN=NDWN-1
      ELSE
        LCWD=0
      END IF
C
C Find the coordinates of the center and the right end of the new
C character.
C
      IF (LCWD.EQ.0) THEN
        XCEN=XRGT+DTLE*STCO
        YCEN=YRGT+DTLE*STSO
        IF (ADDP.NE.0.) THEN
          IF (ADDS.LT.0.) THEN
            XCEN=XCEN+ADDP*STCO
            YCEN=YCEN+ADDP*STSO
          ELSE
            XCEN=XCEN+.5*(ADDP+ADDS*WPIC(IPIC))*STCO
            YCEN=YCEN+.5*(ADDP+ADDS*WPIC(IPIC))*STSO
          END IF
        END IF
        IF (SUBP.NE.0.) THEN
          IF (SUBS.LT.0.) THEN
            XCEN=XCEN-SUBP*STCO
            YCEN=YCEN-SUBP*STSO
          ELSE
            XCEN=XCEN-.5*(SUBP+SUBS*WPIC(IPIC))*STCO
            YCEN=YCEN-.5*(SUBP+SUBS*WPIC(IPIC))*STSO
          END IF
        END IF
      END IF
C
      XRGT=XCEN+DTRE*STCO
      YRGT=YCEN+DTRE*STSO
C
C If information about the current character is to be saved for the
C drawing passes, do it.
C
      ILCG=ILCG+1
      IF (ILCG.GE.ILCS+1.AND.ILCG.LE.ILCS+NCSO) THEN
        NCSV=NCSV+1
        XCSV(NCSV)=XCEN
        YCSV(NCSV)=YCEN
        IPSV(NCSV)=IPIC
        XMSV(NCSV)=XMZM(IPIC)
        YMSV(NCSV)=YMZM(IPIC)
        NFSV(NCSV)=NFNT
        NDSV(NCSV)=NDPC
        INSV(NCSV)=INDP
        NASV(NCSV)=NASC
        IF (INDP.EQ.95) THEN
          CHSV(NCSV:NCSV)='x'
        ELSE IF (INDP.EQ.358) THEN
          CHSV(NCSV:NCSV)=' '
        ELSE IF (ICSE.EQ.ICSL.AND.NDPC.GE.1.AND.NDPC.LE.26) THEN
          CHSV(NCSV:NCSV)=CDPC(NDPC+47)
        ELSE
          CHSV(NCSV:NCSV)=CDPC(NDPC)
        END IF
      END IF
C
C If appropriate, update the quantities from which the magnitudes of
C the text-extent vectors will be computed.
C
      IF (ITEF.NE.0.OR.ICEN.NE.0.OR.IBXF.NE.0) THEN
C
        UCEN=+(XCEN-XFRA)*COSO+(YCEN-YFRA)*SINO
        VCEN=-(XCEN-XFRA)*SINO+(YCEN-YFRA)*COSO
C
        IF (IQUF.EQ.0) THEN
C
          DO 107 I=3,NDGU-1,2
            IF (RDGU(I).GT.-2047.) THEN
              DSTL=MAX(DSTL,-UCEN-SIZM*XMZM(IPIC)*RDGU(I  ))
              DSTR=MAX(DSTR,+UCEN+SIZM*XMZM(IPIC)*RDGU(I  ))
              DSTB=MAX(DSTB,-VCEN-SIZM*YMZM(IPIC)*RDGU(I+1))
              DSTT=MAX(DSTT,+VCEN+SIZM*YMZM(IPIC)*RDGU(I+1))
            END IF
  107     CONTINUE
C
        ELSE
C
          IF (RHTW.GE.0.) RHTW=1.75*YMZM(IPIC)/XMZM(IPIC)
C
          DSTL=MAX(DSTL,-UCEN-SIZM*WPIC(IPIC)/3.,
     +                  -UCEN+SIZM*WPIC(IPIC)/3.)
          DSTR=MAX(DSTR,+UCEN-SIZM*WPIC(IPIC)/3.,
     +                  +UCEN+SIZM*WPIC(IPIC)/3.)
          DSTB=MAX(DSTB,-VCEN-SIZM*ABS(RHTW)*WPIC(IPIC)/3.,
     +                  -VCEN+SIZM*ABS(RHTW)*WPIC(IPIC)/3.)
          DSTT=MAX(DSTT,+VCEN-SIZM*ABS(RHTW)*WPIC(IPIC)/3.,
     +                  +VCEN+SIZM*ABS(RHTW)*WPIC(IPIC)/3.)
C
        END IF
C
      END IF
C
C Save the vertical space occupied by the current character as that of
C the previous character.
C
      VEPC=VPIC(IPIC)
C
C Set variables affecting the amount of space between characters.
C
      IF (ADDS.LT.0.) THEN
        ADDP=-ADDS
      ELSE IF (ADDS.GT.0.) THEN
        ADDP=ADDS*WPIC(IPIC)
      END IF
C
      IF (SUBS.LT.0.) THEN
        SUBP=-SUBS
      ELSE IF (SUBS.GT.0.) THEN
        SUBP=SUBS*WPIC(IPIC)
      END IF
C
C If case was set for a specified number of characters, see if all of
C them have been done and, if so, reset the case index.
C
      IF (NCSE.GT.0) THEN
        NCSE=NCSE-1
        IF (NCSE.EQ.0) ICSE=ICSP
      END IF
C
C If we are subscripting or superscripting for a specified number of
C characters, see if all of them have been done and, if so, reset all
C of the required variables.
C
      IF (NSSL.NE.0) THEN
        IF (NLEV(NSSL).GT.0) THEN
          NLEV(NSSL)=NLEV(NSSL)-1
          IF (NLEV(NSSL).EQ.0) THEN
            XCEN=XCNO(NSSL)
            YCEN=YCNO(NSSL)
            XRGT=XRGO(NSSL)
            YRGT=YRGO(NSSL)
            ICSE=ICSO(NSSL)
            ISZE=ISZO(NSSL)
            IPIC=ISZE/128+1
            NSSL=NSSL-1
          END IF
        END IF
      END IF
C
C Go get the next character from the input string.
C
      GO TO 104
C
C
C E N D   O F   S T R I N G   E N C O U N T E R E D
C
C
C Figure out where the end of the character string fell.  This depends
C on whether the last character was "written down" or "written across".
C In the former case, the bottom of the character box is used; in the
C latter case, the center of the right edge of the character is used.
C
  108 IF (LCWD.NE.0) THEN
        XEND=XCEN+.5*VEPC*STSO
        YEND=YCEN-.5*VEPC*STCO
      ELSE
        XEND=XRGT
        YEND=YRGT
      END IF
C
C Compute the adjustment required to achieve the desired centering.
C
      IF (ICEN.EQ.0) THEN
        XADJ=-.5*(CNTR+1.)*(XEND-XBEG)
        YADJ=-.5*(CNTR+1.)*(YEND-YBEG)
      ELSE
        XADJ=-.5*(DSTR-DSTL)*COSO+.5*(DSTT-DSTB)*SINO
        YADJ=-.5*(DSTR-DSTL)*SINO-.5*(DSTT-DSTB)*COSO
      END IF
C
C Adjust the values of XBEG, YBEG, XEND, and YEND as required by a
C user call to retrieve them.
C
      XBEG=XBEG+XADJ
      YBEG=YBEG+YADJ
C
      XEND=XEND+XADJ
      YEND=YEND+YADJ
C
C If text extents were computed, adjust them to be measured from
C the point relative to which the string is being centered.
C
      IF (ITEF.NE.0.OR.ICEN.NE.0.OR.IBXF.NE.0) THEN
        DSTL=DSTL-XADJ*COSO-YADJ*SINO
        DSTR=DSTR+XADJ*COSO+YADJ*SINO
        DSTB=DSTB+XADJ*SINO-YADJ*COSO
        DSTT=DSTT-XADJ*SINO+YADJ*COSO
      END IF
C
C If the object of the call was just to compute text extents, quit.
C
      IF (ITEF.NE.0.AND.ANGD.EQ.360.) GO TO 127
C
C
C D R A W   T H E   C H A R A C T E R S
C
C
C Save information about the current normalization transformation.
C
      CALL GQCNTN (IERR,INTC)
C
      IF (IERR.NE.0) THEN
        CALL SETER ('PLCHHQ - ERROR EXIT FROM GQCNTN',16,1)
        RETURN
      END IF
C
      CALL GQNT (INTC,IERR,WNTC,VNTC)
C
      IF (IERR.NE.0) THEN
        CALL SETER ('PLCHHQ - ERROR EXIT FROM GQNT',17,1)
        RETURN
      END IF
C
C Then, save the values of the initial polyline, fill area, and text
C color indices, the initial line width, and the initial fill area
C interior style.
C
      CALL GQPLCI (IERR,IPLC)
C
      IF (IERR.NE.0) THEN
        CALL SETER ('PLCHHQ - ERROR EXIT FROM GQPLCI',18,1)
        RETURN
      END IF
C
      CALL GQFACI (IERR,IFAC)
C
      IF (IERR.NE.0) THEN
        CALL SETER ('PLCHHQ - ERROR EXIT FROM GQFACI',19,1)
        RETURN
      END IF
C
      CALL GQTXCI (IERR,ITXC)
C
      IF (IERR.NE.0) THEN
        CALL SETER ('PLCHHQ - ERROR EXIT FROM GQTXCI',20,1)
        RETURN
      END IF
C
      CALL GQLWSC (IERR,RILW)
C
      IF (IERR.NE.0) THEN
        CALL SETER ('PLCHHQ - ERROR EXIT FROM GQLWSC',21,1)
        RETURN
      END IF
C
      CALL GQFAIS (IERR,IIIS)
C
      IF (IERR.NE.0) THEN
        CALL SETER ('PLCHHQ - ERROR EXIT FROM GQFAIS',22,1)
        RETURN
      END IF
C
C Set up variables in which to keep track of the current color index,
C the current line width, and the current fill area interior style.
C
      ICCI=-1
      RCLW=RILW
      ICIS=IIIS
C
C What we want to do now is to draw a box (if there is to be one) around
C the characters and then the characters themselves.  For I = 1, we fill
C the box shadow; for I = 2, we fill the box; for I = 3, we draw the box
C outline.  For I = 4 through 3+3*NCSV, we draw either a character
C shadow, the principal part of the character, or a character outline.
C The internal parameter IORD determines the order in which all the
C pieces of the characters are done.
C
      DO 126 I=1,3+3*NCSV
C
C Set things up to draw a part of the box ...
C
        IF (I.LE.3) THEN
C
          IF (ILCS.NE.0) GO TO 126
C
          IF      (I.EQ.1) THEN
            IDRF=MOD(IBXF/4,2)
            IDRC=IBXC(3)
            RDLW=RBXL
          ELSE IF (I.EQ.2) THEN
            IDRF=MOD(IBXF/2,2)
            IDRC=IBXC(2)
            RDLW=RBXL
          ELSE
            IDRF=MOD(IBXF  ,2)
            IDRC=IBXC(1)
            RDLW=RBXL
          END IF
C
          IF (IDRF.EQ.0) GO TO 126
C
          XCEN=XFRA
          YCEN=YFRA
          IPIC=1
          XMZM(IPIC)=1.
          YMZM(IPIC)=1.
          NDGU=14
          WBXM=RBXM*SIZM*HPIC(1)
          RDGU( 1)=-(DSTL+WBXM)/SIZM
          RDGU( 2)=+(DSTR+WBXM)/SIZM
          RDGU( 3)=-(DSTL+WBXM)/SIZM
          RDGU( 4)=-(DSTB+WBXM)/SIZM
          RDGU( 5)=+(DSTR+WBXM)/SIZM
          RDGU( 6)=-(DSTB+WBXM)/SIZM
          RDGU( 7)=+(DSTR+WBXM)/SIZM
          RDGU( 8)=+(DSTT+WBXM)/SIZM
          RDGU( 9)=-(DSTL+WBXM)/SIZM
          RDGU(10)=+(DSTT+WBXM)/SIZM
          RDGU(11)=-(DSTL+WBXM)/SIZM
          RDGU(12)=-(DSTB+WBXM)/SIZM
          RDGU(13)=-2048.
          RDGU(14)=0.
C
          IF      (I.EQ.1) THEN
            RDGU(13)=-2047.
            XCEN=XCEN+RBXX*HPIC(1)*STCO-RBXY*HPIC(1)*STSO
            YCEN=YCEN+RBXX*HPIC(1)*STSO+RBXY*HPIC(1)*STCO
          ELSE IF (I.EQ.2) THEN
            RDGU(13)=-2047.
          END IF
C
C ... or a part of a character.
C
        ELSE
C
          IF (ABS(IORD).LE.1) THEN
            IDRW=1+(I-4)/NCSV
            ICSV=1+MOD(I-4,NCSV)
          ELSE
            ICSV=1+(I-4)/3
            IDRW=1+MOD(I-4,3)
          END IF
C
          IF (IORD.LT.0) ICSV=NCSV+1-ICSV
C
          IF (IDRW.EQ.1) THEN
            IDRF=ISHF
            IDRC=ISHC
            RDLW=RSLW
          ELSE IF (IDRW.EQ.2) THEN
            IDRF=1
            IDRC=IPCC
            RDLW=RPLW
          ELSE IF (IDRW.EQ.3) THEN
            IDRF=IOUF
            IDRC=IOUC
            RDLW=ROLW
          END IF
C
          IF (IDRF.EQ.0) GO TO 126
C
          XCEN=XCSV(ICSV)+XADJ
          YCEN=YCSV(ICSV)+YADJ
          IPIC=IPSV(ICSV)
          XMZM(IPIC)=XMSV(ICSV)
          YMZM(IPIC)=YMSV(ICSV)
          NFNT=NFSV(ICSV)
          NDPC=NDSV(ICSV)
          INDP=INSV(ICSV)
          NASC=NASV(ICSV)
C
          IF      (IDRW.EQ.1) THEN
            XCEN=XCEN+SHDX*HPIC(1)*STCO-SHDY*HPIC(1)*STSO
            YCEN=YCEN+SHDX*HPIC(1)*STSO+SHDY*HPIC(1)*STCO
          ELSE IF (IDRW.EQ.3) THEN
            IF (NFNT.GE.21.AND.NFNT.LE.99) NFNT=NFNT+100
          END IF
C
C Get the digitization of the desired character.  Note: I have some
C doubts about the setting of CHFS to .1 in the following code (though
C it seems to have worked okay for a very long time).
C
          IF (IQUF.EQ.0) THEN
            IF (NFNT.EQ.0.OR.NDPC.LT. 1.OR.
     +                       NDPC.GT.95.OR.INDP.EQ.95) THEN
              CALL PCEXCD (INDP,2,NDGU)
            ELSE
              IF (NFNT.GE.1.AND.NFNT.LE.20) THEN
                CALL PCCFFC (2,IBNU,NFNT,NASC,SPIC(IPIC),RDGU,MDGU,NDGU)
                IF (ICFELL('PLCHHQ',23).NE.0) RETURN
              ELSE
                IF (IMAP.LE.0) THEN
                  CHFS=SIZM*HPIC(IPIC)
                ELSE
                  CHFS=.1
                END IF
                CALL PCCFFF (2,IBNU,NFNT,NASC,SPIC(IPIC),CHFS,
     +                                         RDGU,MDGU,NDGU)
                IF (ICFELL('PLCHHQ',24).NE.0) RETURN
                IF (NASC.EQ.32) RDGU(3)=-2048.
              END IF
              IF (NDGU.EQ.0) THEN
                CALL PCCFFC (2,IBNU,1,NASC,SPIC(IPIC),RDGU,MDGU,NDGU)
                IF (ICFELL('PLCHHQ',25).NE.0) RETURN
                IF (NDGU.EQ.0) CALL PCEXCD (INDP,2,NDGU)
              END IF
            END IF
C
            IF (NDGU.EQ.0) THEN
              CALL PCEXCD (358,2,NDGU)
              IF (NDGU.EQ.0) THEN
                CALL SETER ('PLCHHQ - INTERNAL LOGIC ERROR (NDGU = 0) -
     +SEE CONSULTANT',26,1)
                RETURN
              END IF
            END IF
C
          END IF
C
        END IF
C
C Digitization defined.  Set up the requested drawing color ...
C
        IF (IDRC.GE.0) THEN
          IF (IDRC.NE.ICCI) THEN
            ICCI=IDRC
            IF (ICFELL(' ',0).NE.0) RETURN
            CALL PLOTIF (0.,0.,2)
            IF (ICFELL('PLCHHQ',27).NE.0) RETURN
            CALL GSPLCI (IDRC)
            CALL GSFACI (IDRC)
            CALL GSTXCI (IDRC)
          END IF
        ELSE IF (ICCI.GE.0) THEN
          ICCI=-1
          IF (ICFELL(' ',0).NE.0) RETURN
          CALL PLOTIF (0.,0.,2)
          IF (ICFELL('PLCHHQ',28).NE.0) RETURN
          CALL GSPLCI (IPLC)
          CALL GSFACI (IFAC)
          CALL GSTXCI (ITXC)
        END IF
C
C ... and line width.
C
        IF (RDLW.LE.0.) RDLW=RILW
C
        IF (RDLW.NE.RCLW) THEN
          RCLW=RDLW
          IF (ICFELL(' ',0).NE.0) RETURN
          CALL PLOTIF (0.,0.,2)
          IF (ICFELL('PLCHHQ',29).NE.0) RETURN
          CALL GSLWSC (RDLW)
        END IF
C
C If the box or a high-quality character is being drawn, ...
C
        IF (I.LE.3.OR.IQUF.EQ.0) THEN
C
C ... if mapping is turned on, pre-process the digitized character to
C interpolate points along long straight portions of it and make sure
C that any fill areas defined are properly closed.  This avoids
C problems that otherwise occur.
C
          IF (IMAP.GT.0) THEN
C
            NCRA=0
C
            KTMP=1
C
            DO 110 K=3,NDGU-1,2
              IF (K.EQ.3.OR.RDGU(K-2).LE.-2047.
     +                  .OR.RDGU(K  ).LE.-2047.) THEN
                IF (RDGU(K).LE.-2047.) THEN
                  IF (RDGU(K).EQ.-2047.) THEN
                    IF (XCRA(NCRA).NE.RDGU(KTMP+2).OR.
     +                  YCRA(NCRA).NE.RDGU(KTMP+3)) THEN
                      IF (NCRA+1.LE.MCRA) THEN
                        NCRA=NCRA+1
                        XCRA(NCRA)=RDGU(KTMP+2)
                        YCRA(NCRA)=RDGU(KTMP+3)
                      ELSE
                        CALL SETER ('PLCHHQ - INTERNAL LOGIC ERROR (XCRA
     +/YCRA TOO SMALL) - SEE CONSULTANT',30,1)
                        RETURN
                      END IF
                    END IF
                  END IF
                  KTMP=K
                END IF
                IF (NCRA+1.LE.MCRA) THEN
                  NCRA=NCRA+1
                  XCRA(NCRA)=RDGU(K  )
                  YCRA(NCRA)=RDGU(K+1)
                ELSE
                  CALL SETER ('PLCHHQ - INTERNAL LOGIC ERROR (XCRA/YCRA
     +TOO SMALL) - SEE CONSULTANT',31,1)
                  RETURN
                END IF
              ELSE
                NINT=MAX(1,INT(ABS(RDGU(K  )-RDGU(K-2))/2.1),
     +                     INT(ABS(RDGU(K+1)-RDGU(K-1))/2.1))
                IF (NCRA+NINT.LE.MCRA) THEN
                  DO 109 IINT=1,NINT
                    P=REAL(NINT-IINT)/REAL(NINT)
                    NCRA=NCRA+1
                    XCRA(NCRA)=P*RDGU(K-2)+(1.-P)*RDGU(K  )
                    YCRA(NCRA)=P*RDGU(K-1)+(1.-P)*RDGU(K+1)
  109             CONTINUE
                ELSE
                  CALL SETER ('PLCHHQ - INTERNAL LOGIC ERROR (XCRA/YCRA
     +TOO SMALL) - SEE CONSULTANT',32,1)
                  RETURN
                END IF
              END IF
  110       CONTINUE
C
            NDGU=2+2*NCRA
C
            DO 111 K=3,NDGU-1,2
              RDGU(K  )=XCRA(K/2)
              RDGU(K+1)=YCRA(K/2)
  111       CONTINUE
C
          END IF
C
C Initialize NCRA, which is a count of the number of X/Y coordinate
C pairs in the arrays XCRA and YCRA, and IORF, which is a count of
C the number of those that were found to be "out of range" (invisible
C under the current mapping).
C
          NCRA=0
          IORF=0
C
C Examine the X/Y coordinate pairs of the digitization [an X from
C RDGU(K) and a Y from RDGU(K+1)], one by one.  If the X coordinate
C is greater than -2047, the X/Y pair is just added to what's in
C XCRA and YCRA.  If the X coordinate equals -2048, it says that
C what has been saved up in XCRA and YCRA is a polyline to be drawn.
C If the X coordinate equals -2047, it says that what has been saved
C up is a polygon to be filled.
C
          DO 125 K=3,NDGU-1,2
C
C If the X/Y pair is to be saved in XCRA and YCRA, increment NCRA ...
C
            IF (RDGU(K).GT.-2047.) THEN
C
              NCRA=NCRA+1
C
C ... and then check the mapping flag.  If it's turned off, ...
C
              IF (IMAP.LE.0) THEN
C
C ... just save the X and Y coordinates, transformed to the fractional
C coordinate system; ...
C
                XCRA(NCRA)=XCEN+XMZM(IPIC)*RDGU(K  )*STCO
     +                         -YMZM(IPIC)*RDGU(K+1)*STSO
                YCRA(NCRA)=YCEN+XMZM(IPIC)*RDGU(K  )*STSO
     +                         +YMZM(IPIC)*RDGU(K+1)*STCO
C
C ... otherwise, ...
C
              ELSE
C
C ... map the X/Y coordinates by calling PCMPXY ...
C
                XINP=XCEN+(XMZM(IPIC)*(RDGU(K  )*STCO)
     +                    -YMZM(IPIC)*(RDGU(K+1)*STSO))
                YINP=YCEN+(XMZM(IPIC)*(RDGU(K  )*STSO)
     +                    +YMZM(IPIC)*(RDGU(K+1)*STCO))
C
                CALL PCMPXY (IMAP,XINP,YINP,XTMP,YTMP)
                IF (ICFELL('PLCHHQ',33).NE.0) RETURN
C
C ... and check for an out-of-range point (one that's invisible under
C the current mapping).  If the out-of-range flag is turned off or
C if the X coordinate coming back is not equal to it ...
C
                IF (OORV.EQ.0..OR.XTMP.NE.OORV) THEN
C
C ... transform the point into the fractional system and save it; ...
C
                  XCRA(NCRA)=CUFX(XTMP)
                  IF (ICFELL('PLCHHQ',34).NE.0) RETURN
                  YCRA(NCRA)=CUFY(YTMP)
                  IF (ICFELL('PLCHHQ',35).NE.0) RETURN
C
C ... otherwise, set the X coordinate to the out-of-range flag and
C save a pointer into the digitization array as the value of the Y
C coordinate.
C
                ELSE
C
                  IORF=IORF+1
                  XCRA(NCRA)=OORV
                  YCRA(NCRA)=REAL(K)
C
                END IF
C
              END IF
C
C If RDGU(K) is either -2048 or -2047, and there are saved coordinates
C in XCRA and YCRA, ...
C
            ELSE IF (NCRA.GT.0) THEN
C
C ... if none of them are out-of-range, the entire collection of points
C can be treated as a one-piece polyline or polygon to be filled, ...
C
              IF (IORF.EQ.0) THEN
C
                NPCS=1
                IPCB(1)=1
                IPCE(1)=NCRA
C
C ... and if all of them are out-of-range, ...
C
              ELSE IF (IORF.EQ.NCRA) THEN
C
C ... there are no pieces of polyline or polygon to be dealt with; ...
C
                NPCS=0
C
C ... otherwise, ...
C
              ELSE
C
C ... we have to do something about the fact that the edge of the
C character lies partly in areas invisible under the mapping ...
C
C First, we have to find out exactly where each line segment with
C an invisible end-point appears or disappears and add those points
C to the ends of the visible pieces.  When we are done, NPCS says
C how many pieces there are; for each value of "i" between 1 and
C NPCS, IPCB(i) is the index, in the arrays XCRA and YCRA, of the
C first point of the piece and IPCE(i) is the index of the last point
C of the piece.  XPCB(i) and YPCB(i) are the X and Y coordinates that
C mapped into the first point of the piece and XPCE(i) and YPCE(i) are
C the X and Y coordinates that mapped into the last point of the piece.
C XPCB(1), YPCB(1), XPCE(NPCS), and YPCE(NPCS) are sometimes given
C dummy values; this is only done in cases where we know that those
C values will not be needed for anything.
C
C Initialization of the loop through the X and Y coordinates in XCRA
C and YCRA differs depending on whether the first point is out-of-range
C or not.  ISTA is a "state" flag that says whether the last point we
C looked at was out-of-range (ISTA=0) or not (ISTA=1).
C
                IF (XCRA(1).EQ.OORV) THEN
                  NPCS=0
                  ISTA=0
                ELSE
                  NPCS=1
                  IPCB(1)=1
                  XPCB(1)=OORV
                  YPCB(1)=OORV
                  ISTA=1
                END IF
C
C Start the loop through the X and Y coordinates with element 2.
C
                ICRA=2
C
C The loop through the X and Y coordinates is implemented using IF's,
C rather than a DO, because we may very occasionally need to add an
C element in the middle of the arrays that we are looking at, which
C changes the value of NCRA.  Control returns here to examine element
C number ICRA of XCRA and YCRA.
C
  112           IF (ICRA.GT.NCRA) GO TO 118
C
C If the last point looked at was out-of-range and this one is not,
C interpolate, using a binary halving process, to find a point at the
C very edge of visibility, and fill the out-of-range slot with the
C X and Y coordinates of that point.
C
                  IF (ISTA.EQ.0.AND.XCRA(ICRA).NE.OORV) THEN
C
                    KTMP=INT(YCRA(ICRA-1))
C
                    XINV=XCEN+(XMZM(IPIC)*(RDGU(KTMP  )*STCO)
     +                        -YMZM(IPIC)*(RDGU(KTMP+1)*STSO))
                    YINV=YCEN+(XMZM(IPIC)*(RDGU(KTMP  )*STSO)
     +                        +YMZM(IPIC)*(RDGU(KTMP+1)*STCO))
C
                    XVIS=XCEN+(XMZM(IPIC)*(RDGU(KTMP+2)*STCO)
     +                        -YMZM(IPIC)*(RDGU(KTMP+3)*STSO))
                    YVIS=YCEN+(XMZM(IPIC)*(RDGU(KTMP+2)*STSO)
     +                        +YMZM(IPIC)*(RDGU(KTMP+3)*STCO))
C
                    DO 113 IHLF=1,64
C
                      XHLF=.5*(XINV+XVIS)
                      YHLF=.5*(YINV+YVIS)
C
                      CALL PCMPXY (IMAP,XHLF,YHLF,XTMP,YTMP)
                      IF (ICFELL('PLCHHQ',36).NE.0) RETURN
C
                      IF (XTMP.EQ.OORV) THEN
                        IF (XHLF.EQ.XINV.AND.YHLF.EQ.YINV) GO TO 114
                        XINV=XHLF
                        YINV=YHLF
                      ELSE
                        IF (XHLF.EQ.XVIS.AND.YHLF.EQ.YVIS) GO TO 114
                        XVIS=XHLF
                        YVIS=YHLF
                      END IF
C
  113               CONTINUE
C
  114               IF (NPCS.GE.MPCS) THEN
                      CALL SETER ('PLCHHQ - INTERNAL LOGIC ERROR (NPCS T
     +OO BIG) - SEE CONSULTANT',37,1)
                      RETURN
                    END IF
C
                    NPCS=NPCS+1
                    IPCB(NPCS)=ICRA-1
                    XPCB(NPCS)=XVIS
                    YPCB(NPCS)=YVIS
                    CALL PCMPXY (IMAP,XVIS,YVIS,XTMP,YTMP)
                    IF (ICFELL('PLCHHQ',38).NE.0) RETURN
                    XCRA(ICRA-1)=CUFX(XTMP)
                    IF (ICFELL('PLCHHQ',39).NE.0) RETURN
                    YCRA(ICRA-1)=CUFY(YTMP)
                    IF (ICFELL('PLCHHQ',40).NE.0) RETURN
                    ISTA=1
C
C Similarly, if the last point looked at was not out-of-range and this
C one is, interpolate, using a binary halving process, to find a point
C at the very edge of visibility, and fill the out-of-range slot with
C the X and Y coordinates of that point.  In this case, though, if
C there is a next point, and it is not out-of-range, we have to insert
C a copy of the current point to serve as the starting point of the
C next piece of the polyline.
C
                  ELSE IF (ISTA.NE.0.AND.XCRA(ICRA).EQ.OORV) THEN
C
                    IF (ICRA.NE.NCRA) THEN
C
                      IF (XCRA(ICRA+1).NE.OORV) THEN
C
                        IF (NCRA.GE.MCRA) THEN
                          CALL SETER ('PLCHHQ - INTERNAL LOGIC ERROR (NC
     +RA TOO BIG) - SEE CONSULTANT',41,1)
                          RETURN
                        END IF
C
                        DO 115 L=NCRA,ICRA,-1
                          XCRA(L+1)=XCRA(L)
                          YCRA(L+1)=YCRA(L)
  115                   CONTINUE
C
                        NCRA=NCRA+1
C
                      END IF
C
                    END IF
C
                    KTMP=INT(YCRA(ICRA))
C
                    XINV=XCEN+(XMZM(IPIC)*(RDGU(KTMP  )*STCO)
     +                        -YMZM(IPIC)*(RDGU(KTMP+1)*STSO))
                    YINV=YCEN+(XMZM(IPIC)*(RDGU(KTMP  )*STSO)
     +                        +YMZM(IPIC)*(RDGU(KTMP+1)*STCO))
C
                    XVIS=XCEN+(XMZM(IPIC)*(RDGU(KTMP-2)*STCO)
     +                        -YMZM(IPIC)*(RDGU(KTMP-1)*STSO))
                    YVIS=YCEN+(XMZM(IPIC)*(RDGU(KTMP-2)*STSO)
     +                        +YMZM(IPIC)*(RDGU(KTMP-1)*STCO))
C
                    DO 116 IHLF=1,64
C
                      XHLF=.5*(XINV+XVIS)
                      YHLF=.5*(YINV+YVIS)
C
                      CALL PCMPXY (IMAP,XHLF,YHLF,XTMP,YTMP)
                      IF (ICFELL('PLCHHQ',42).NE.0) RETURN
C
                      IF (XTMP.EQ.OORV) THEN
                        IF (XHLF.EQ.XINV.AND.YHLF.EQ.YINV) GO TO 117
                        XINV=XHLF
                        YINV=YHLF
                      ELSE
                        IF (XHLF.EQ.XVIS.AND.YHLF.EQ.YVIS) GO TO 117
                        XVIS=XHLF
                        YVIS=YHLF
                      END IF
C
  116               CONTINUE
  117               IPCE(NPCS)=ICRA
                    XPCE(NPCS)=XVIS
                    YPCE(NPCS)=YVIS
                    CALL PCMPXY (IMAP,XVIS,YVIS,XTMP,YTMP)
                    IF (ICFELL('PLCHHQ',43).NE.0) RETURN
                    XCRA(ICRA)=CUFX(XTMP)
                    IF (ICFELL('PLCHHQ',44).NE.0) RETURN
                    YCRA(ICRA)=CUFY(YTMP)
                    IF (ICFELL('PLCHHQ',45).NE.0) RETURN
                    ISTA=0
C
                  END IF
C
C Increment to the next elements of XCRA and YCRA ...
C
                  ICRA=ICRA+1
C
C ... and return to the beginning of the loop.
C
                  GO TO 112
C
C Finish storing information about the last piece, if any.
C
  118           IF (ISTA.NE.0) THEN
                  IPCE(NPCS)=NCRA
                  XPCE(NPCS)=OORV
                  YPCE(NPCS)=OORV
                END IF
C
C If the polyline defined is a polygon to be filled ...
C
                IF (RDGU(K).EQ.-2047.) THEN
C
C ... interpolate points from the end of each visible piece to the
C beginning of the next.  A heuristic algorithm is used to ensure
C that these points will lie along the line separating areas which
C are visible under the current mapping from those which are not.
C
C Loop through the visible pieces of the polygon.
C
                  DO 123 L=1,NPCS
C
C Compute MBEG and MEND, which point to the beginning and end of a
C portion of the arrays XCRA and YCRA which mapped to an out-of-range
C (invisible) area.
C
                    LPL1=MOD(L,NPCS)+1
C
                    MBEG=IPCE(L)+1
                    IF (MBEG.GT.NCRA) MBEG=MBEG-NCRA
C
                    MEND=IPCB(LPL1)-1
                    IF (MEND.LT.MBEG-1) MEND=MEND+NCRA
C
C If there are one or more invisible points, replace them.
C
                    IF (MBEG.LE.MEND) THEN
C
C (XCP1,YCP1) and (XCP2,YCP2) are endpoints X1 and X2 of a line segment
C on the visible/invisible boundary between which we wish to interpolate
C other points.
C
                      XCP1=XPCE(L)
                      YCP1=YPCE(L)
                      XCP2=XPCB(LPL1)
                      YCP2=YPCB(LPL1)
C
C If either X1 or X2 is an out-of-range value, something has happened
C that should not have been possible, so take an error exit.
C
                      IF (XCP1.EQ.OORV.OR.XCP2.EQ.OORV) THEN
                        CALL SETER ('PLCHHQ - INTERNAL LOGIC ERROR (XCP1
     + OR XCP2 = OORV) - SEE CONSULTANT',46,1)
                        RETURN
                      END IF
C
C XDIF and YDIF are components of the vector from X1 to X2.
C
                      XDIF=XCP2-XCP1
                      YDIF=YCP2-YCP1
C
C Loop through the elements of XCRA and YCRA to be replaced.
C
                      DO 122 M=MBEG,MEND
C
C When the points being replaced include a section at the end of the
C arrays, followed by a section at the beginning, M may range past
C NCRA; MSTR is the appropriate index between 1 and NCRA.
C
                        MSTR=MOD(M-1,NCRA)+1
C
C (XTM0,YTM0) is a point X0 along the straight line from X1 to X2.
C
                        P=REAL(M-MBEG+1)/REAL(MEND-MBEG+2)
C
                        XTM0=(1.-P)*XCP1+P*XCP2
                        YTM0=(1.-P)*YCP1+P*YCP2
C
C XSTP and YSTP are the components of a "step vector" that is at right
C angles to the line from X1 to X2 and 1/32 of its length.
C
                        XSTP=+YDIF/32.
                        YSTP=-XDIF/32.
C
C Loop on step size, looking at the ends of the two step vectors
C emanating from X0, one pointing in one direction and the other
C pointing in the opposite direction.  What we are hoping for is
C that one such endpoint will be visible and the other invisible.
C
                        DO 121 ISTP=1,6
C
                          XTM1=XTM0+XSTP
                          YTM1=YTM0+YSTP
                          XTM2=XTM0-XSTP
                          YTM2=YTM0-YSTP
C
                          CALL PCMPXY (IMAP,XTM1,YTM1,XMP1,YMP1)
                          IF (ICFELL('PLCHHQ',47).NE.0) RETURN
                          CALL PCMPXY (IMAP,XTM2,YTM2,XMP2,YMP2)
                          IF (ICFELL('PLCHHQ',48).NE.0) RETURN
C
                          IF      (XMP1.EQ.OORV.AND.XMP2.NE.OORV) THEN
                            XINV=XTM1
                            YINV=YTM1
                            XVIS=XTM2
                            YVIS=YTM2
                          ELSE IF (XMP1.NE.OORV.AND.XMP2.EQ.OORV) THEN
                            XVIS=XTM1
                            YVIS=YTM1
                            XINV=XTM2
                            YINV=YTM2
                          ELSE
                            XSTP=XSTP+XSTP
                            YSTP=YSTP+YSTP
                            GO TO 121
                          END IF
C
C At this point, we have found two such points.  Use a binary halving
C process to find the point where the line between them intersects the
C visible/invisible boundary and use that point to generate the values
C to be put in the arrays XCRA and YCRA.
C
                          DO 119 IHLF=1,64
                            XHLF=.5*(XINV+XVIS)
                            YHLF=.5*(YINV+YVIS)
                            CALL PCMPXY (IMAP,XHLF,YHLF,XTMP,YTMP)
                            IF (ICFELL('PLCHHQ',49).NE.0) RETURN
                            IF (XTMP.EQ.OORV) THEN
                              IF (XHLF.EQ.XINV.AND.
     +                            YHLF.EQ.YINV) GO TO 120
                              XINV=XHLF
                              YINV=YHLF
                            ELSE
                              IF (XHLF.EQ.XVIS.AND.
     +                            YHLF.EQ.YVIS) GO TO 120
                              XVIS=XHLF
                              YVIS=YHLF
                            END IF
  119                     CONTINUE
C
C Replace the original point with one on the visible/invisible edge.
C
  120                     CALL PCMPXY (IMAP,XVIS,YVIS,XTMP,YTMP)
                          IF (ICFELL('PLCHHQ',50).NE.0) RETURN
C
                          XCRA(MSTR)=CUFX(XTMP)
                          IF (ICFELL('PLCHHQ',51).NE.0) RETURN
                          YCRA(MSTR)=CUFY(YTMP)
                          IF (ICFELL('PLCHHQ',52).NE.0) RETURN
C
                          GO TO 122
C
C End of loop on step size.
C
  121                   CONTINUE
C
C If control drops through the end of the loop, supply a point that
C we at least know is visible.
C
                        XCRA(MSTR)=XCRA(IPCE(L))
                        YCRA(MSTR)=YCRA(IPCE(L))
C
C End of loop replacing elements of XCRA and YCRA.
C
  122                 CONTINUE
C
C End of invisible-point replacement.
C
                    END IF
C
C End of loop through visible pieces of the polygon.
C
  123             CONTINUE
C
                END IF
C
C End of partly-invisible case.
C
              END IF
C
C Now, if there are visible pieces of the polyline/polygon, ...
C
              IF (NPCS.NE.0) THEN
C
C ... redefine the current normalization transformation, if possible,
C so that we can use fractional coordinates and yet have the same
C viewport as before ...
C
                IF (INTC.NE.0) THEN
                  CALL GSWN (1,VNTC(1),VNTC(2),VNTC(3),VNTC(4))
                END IF
C
C ... change color (during pass 2) if so directed by information from
C the fontcap, ...
C
                ICCS=-2
C
                IF (IDRW.EQ.2.AND.RDGU(K+1).GT.0.) THEN
C
                  ITMP=INT(RDGU(K+1))
C
                  IF (ITMP.GE.1.AND.ITMP.LE.16) THEN
C
                    IDRC=LSCI(ITMP)
C
                    IF (IDRC.GE.0) THEN
C
                      IF (IDRC.NE.ICCI) THEN
                        ICCS=ICCI
                        ICCI=IDRC
                        IF (ICFELL(' ',0).NE.0) RETURN
                        CALL PLOTIF (0.,0.,2)
                        IF (ICFELL('PLCHHQ',53).NE.0) RETURN
                        CALL GSPLCI (ICCI)
                        CALL GSFACI (ICCI)
                        CALL GSTXCI (ICCI)
                      END IF
C
                    END IF
C
                  END IF
C
                END IF
C
C ... draw the pieces of the polyline, ...
C
                IF (RDGU(K).EQ.-2048.) THEN
C
                  DO 124 L=1,NPCS
                    IF (IPCE(L)-IPCB(L)+1.GT.1)
     +              CALL GPL (IPCE(L)-IPCB(L)+1,XCRA(IPCB(L)),
     +                                          YCRA(IPCB(L)))
  124             CONTINUE
C
C ... or fill the polygon, ...
C
                ELSE
C
                  IF (NCRA.GT.2) THEN
C ... Jira1667: use new SOLID_TEXT_FILL (4), rather than SOLID_FILL (1)
                    IF (ICIS.NE.4) THEN
                      CALL GSFAIS (4)
                      ICIS=4
                    END IF
                    CALL GFA (NCRA,XCRA,YCRA)
                  END IF
C
                END IF
C
C ... restore the current normalization transformation (if it was
C changed above) ...
C
                IF (INTC.NE.0) THEN
                  CALL GSWN (1,WNTC(1),WNTC(2),WNTC(3),WNTC(4))
                END IF
C
C ... and, if the color was just overridden above, set it back to what
C it was.
C
                IF (ICCS.GE.-1) THEN
C
                  ICCI=ICCS
C
                  IF (ICFELL(' ',0).NE.0) RETURN
                  CALL PLOTIF (0.,0.,2)
                  IF (ICFELL('PLCHHQ',54).NE.0) RETURN
C
                  IF (ICCI.GE.0) THEN
                    CALL GSPLCI (ICCI)
                    CALL GSFACI (ICCI)
                    CALL GSTXCI (ICCI)
                  ELSE
                    CALL GSPLCI (IPLC)
                    CALL GSFACI (IFAC)
                    CALL GSTXCI (ITXC)
                  END IF
C
                END IF
C
              END IF
C
C Then, zero NCRA and IORF to say that we are done with that part of
C the digitization ...
C
              NCRA=0
              IORF=0
C
C End of processing of visible pieces.
C
            END IF
C
C End of loop through the digitization array.
C
  125     CONTINUE
C
C Otherwise, the quality flag indicates that PLCHMQ or PLCHLQ should be
C called to draw the character, so arrange for that.
C
        ELSE
C
          IF (IQUF.EQ.1.AND.RHTW.GE.0.) RHTW=1.75*YMZM(IPIC)/XMZM(IPIC)
C
          IF (IMAP.LE.0) THEN
            XTMP=CFUX(XCEN)
            IF (ICFELL('PLCHHQ',55).NE.0) RETURN
            YTMP=CFUY(YCEN)
            IF (ICFELL('PLCHHQ',56).NE.0) RETURN
          ELSE
            XTMP=XCEN
            YTMP=YCEN
          END IF
C
          CALL PCMQLQ (XTMP,YTMP,CHSV(ICSV:ICSV),
     +                   SIZM*WPIC(IPIC),ANGD,0.)
          IF (ICFELL('PLCHHQ',57).NE.0) RETURN
C
        END IF
C
  126 CONTINUE
C
C Restore, as necessary, all the current color indices, the current line
C width scale factor, and the fill area interior style.
C
      IF (ICCI.GE.0) THEN
        IF (ICFELL(' ',0).NE.0) RETURN
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('PLCHHQ',58).NE.0) RETURN
        CALL GSPLCI (IPLC)
        CALL GSFACI (IFAC)
        CALL GSTXCI (ITXC)
      END IF
C
      IF (RCLW.NE.RILW) THEN
        IF (ICFELL(' ',0).NE.0) RETURN
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('PLCHHQ',59).NE.0) RETURN
        CALL GSLWSC (RILW)
      END IF
C
      IF (ICIS.NE.IIIS) CALL GSFAIS (IIIS)
C
C If not all the characters have been drawn yet, go back for more.
C
      IF (ILCG-ILCS.GT.NCSO) THEN
        ILCS=ILCS+NCSO
        GO TO 103
      END IF
C
C
C D O N E   -   R E T U R N   T O   C A L L E R
C
C
C Restore the PLCHMQ parameter specifying the ratio of character height
C to width.
C
  127 IF (IQUF.NE.0) RHTW=RHWO
C
C Done.
C
      RETURN
C
C
C F U N C T I O N   C O D E   P R O C E S S I N G
C
C
C If the current character is invalid, issue an error message and go
C back for the next one.
C
  128 IF (NDPC.LE.0.OR.(NDPC.GE.35.AND.NDPC.NE.45.AND.NDPC.NE.46)) THEN
        WRITE (IERU,1001) ICHR,CHRS(ICHR:ICHR)
        GO TO 104
      END IF
C
C If the character is an octal digit, retrieve an octal number in INDP
C and jump back to process the character it points to.  NDPC is set to
C the value signalling that a particular character from the PWRITX
C database has been selected.
C
      IF (NDPC.GE.27.AND.NDPC.LE.34) THEN
        CALL PCGTPI (CHRS,NCHR,ICHR,8,INDP)
        ICHR=ICHR-1
        NDPC=96
        GO TO 105
      END IF
C
C Blanks and commas are just separators; ignore them.
C
      IF (NDPC.EQ.45.OR.NDPC.EQ.46) GO TO 104
C
C Jump to the appropriate section to process an alphabetic character.
C
C             A,  B,  C,  D,  E,  F,  G,  H,  I,  J,  K,  L,  M,
C             N,  O,  P,  Q,  R,  S,  T,  U,  V,  W,  X,  Y,  Z
C
      GO TO (143,137,144,142,140,147,131,145,133,129,134,136,129,
     +       141,129,132,104,130,138,129,135,146,129,148,149,150) , NDPC
C
C Log an error if it's not one of the legal ones.
C
  129 WRITE (IERU,1001) ICHR,CHRS(ICHR:ICHR)
      GO TO 104
C
C FONT DEFINITION
C ---------------
C
C Define index offset into INDA.
C
C R is for Roman font.
C
  130 IFNT=IFRO
      GO TO 104
C
C G is for Greek font.
C
  131 IFNT=IFGR
      GO TO 104
C
C SIZE DEFINITION
C ---------------
C
C Define index offset into INDA and set current character height and
C width.
C
C P is for Principal size.
C
  132 ISZE=ISZP
      IPIC=1
      GO TO 104
C
C I is for Indexical size.
C
  133 ISZE=ISZI
      IPIC=2
      GO TO 104
C
C K is for Cartographic size.
C
  134 ISZE=ISZC
      IPIC=3
      GO TO 104
C
C CASE DEFINITION
C ---------------
C
C Set old and current index offsets into INDA and look for character
C count.
C
C  U is for Upper case.
C
  135 ICSP=ICSL
      ICSE=ICSU
      CALL PCGTDI (CHRS,NCHR,ICHR,NCSE)
      GO TO 104
C
C L is for Lower case.
C
  136 ICSP=ICSU
      ICSE=ICSL
      CALL PCGTDI (CHRS,NCHR,ICHR,NCSE)
      GO TO 104
C
C LEVEL DEFINITION
C ----------------
C
C B is for suBscript.  Set the sine and cosine of the offset angle and
C jump to join the superscript code.
C
  137 SINT=SINM
      COST=COSM
      GO TO 139
C
C S is for Superscript.  Set the sine and cosine of the offset angle.
C
  138 SINT=SINP
      COST=COSP
C
C Increment the subscript/superscript level, but to no more than five.
C
  139 NSSL=MIN(NSSL+1,5)
C
C Retrieve the character count, if any, following the B or S.
C
      CALL PCGTDI (CHRS,NCHR,ICHR,NLEV(NSSL))
C
C Compute the size of the offset to the superscript or subscript and
C use that to modify the sine and cosine factors.
C
      OFFS=SSPR*SIZM
      IF (ISZE.NE.ISZP) OFFS=SSIC*SIZM
      COST=OFFS*COST
      SINT=OFFS*SINT
C
C Save all the variables which will be needed to undo the offset on a
C stack.
C
      XCNO(NSSL)=XCEN
      YCNO(NSSL)=YCEN
      XRGO(NSSL)=XRGT
      YRGO(NSSL)=YRGT
      ICSO(NSSL)=ICSE
      ISZO(NSSL)=ISZE
      SNTO(NSSL)=SINT
      CSTO(NSSL)=COST
C
C Reset the coordinates of the right end of the last character.
C
      XRGT=XRGT+COST
      YRGT=YRGT+SINT
C
C Reset the character case and size to values appropriate for subscripts
C and superscripts.
C
      IF (ISZE.EQ.ISZP) THEN
        ISZE=ISZI
        IPIC=2
      ELSE
        ISZE=ISZC
        IPIC=3
        ICSE=ICSU
      END IF
C
        GO TO 104
C
C E is for End of subscripting or superscripting, which is ignored if
C we're not in that mode.
C
  140 IF (NSSL.EQ.0) GO TO 104
C
C Return everything to what it was after drawing the base character.
C
      XCEN=XCNO(NSSL)
      YCEN=YCNO(NSSL)
      XRGT=XRGO(NSSL)
      YRGT=YRGO(NSSL)
      ICSE=ICSO(NSSL)
      ISZE=ISZO(NSSL)
      IPIC=ISZE/128+1
      NSSL=NSSL-1
C
      GO TO 104
C
C N is for Normal, which is ignored if we're not subscripting or
C superscripting.
C
  141 IF (NSSL.EQ.0) GO TO 104
C
C Return everything to what it was after drawing the base character
C except the position of the right edge, in which we just reverse the
C original offset.
C
      XCEN=XCNO(NSSL)
      YCEN=YCNO(NSSL)
      XRGT=XRGT-CSTO(NSSL)
      YRGT=YRGT-SNTO(NSSL)
      ICSE=ICSO(NSSL)
      ISZE=ISZO(NSSL)
      IPIC=ISZE/128+1
      NSSL=NSSL-1
C
      GO TO 104
C
C DIRECTION DEFINITION
C --------------------
C
C D is for Down.  Set the character count to the following decimal
C integer, if there is one, or to a number exceeding the number of
C characters left in the string, otherwise.  Set the flag indicating
C that the last character was "written down".
C
  142 CALL PCGTDI (CHRS,NCHR,ICHR,NDWN)
      IF (NDWN.LE.0) NDWN=NCHR
      LCWD=1
      GO TO 104
C
C A is for Across.  Clear the "down" flags.
C
  143 NDWN=0
      LCWD=0
      GO TO 104
C
C COORDINATE DEFINITION
C ---------------------
C
C C is for Carriage return.  Reset the beginning-of-line position and
C the positions of the center and right edge of the last character.
C Suppress spacing manipulations at the beginning of the new line.
C
  144 XBOL=XBOL+SIZM*VPIC(IPIC)*COSM
      YBOL=YBOL+SIZM*VPIC(IPIC)*SINM
      XCEN=XBOL
      YCEN=YBOL
      XRGT=XBOL
      YRGT=YBOL
      ADDP=0.
      SUBP=0.
      GO TO 104
C
C H is for Horizontal.
C
C Retrieve the integer which follows the H and use it to modify all of
C the current character-position variables.
C
  145 CALL PCGTDI (CHRS,NCHR,ICHR,NUPA)
      DELX=REAL(NUPA)
      IF (ICHR.LT.NCHR) THEN
        IF (CHRS(ICHR+1:ICHR+1).EQ.'Q') THEN
          ICHR=ICHR+1
          IF (NUPA.EQ.0) NUPA=1
          DELX=REAL(NUPA)*WPIC(IPIC)
        END IF
      END IF
      XCEN=XCEN+DELX*STCO
      YCEN=YCEN+DELX*STSO
      XRGT=XRGT+DELX*STCO
      YRGT=YRGT+DELX*STSO
      GO TO 104
C
C V is for Vertical.
C
C Retrieve the integer which follows the V and use it to modify all of
C the current character-position variables.
C
  146 CALL PCGTDI (CHRS,NCHR,ICHR,NUPA)
      DELY=REAL(NUPA)
      IF (ICHR.LT.NCHR) THEN
        IF (CHRS(ICHR+1:ICHR+1).EQ.'Q') THEN
          ICHR=ICHR+1
          IF (NUPA.EQ.0) NUPA=1
          DELY=REAL(NUPA)*VPIC(IPIC)
        END IF
      END IF
      XCEN=XCEN-DELY*STSO
      YCEN=YCEN+DELY*STCO
      XRGT=XRGT-DELY*STSO
      YRGT=YRGT+DELY*STCO
      GO TO 104
C
C FONT CHANGE REQUEST
C -------------------
C
C F is for Font.  The intent is to request use of one of the Hershey
C fonts which are defined by fontcaps, stroking out the characters,
C rather than leaving it to be done by the translator.
C
C Retrieve the integer that follows the F; it determines which font to
C use.  If there is no such integer, go back to the font that was in
C use when PLCHHQ was called.
C
  147 ICHS=ICHR
      CALL PCGTDI (CHRS,NCHR,ICHR,NFNT)
      IF (ICHR.EQ.ICHS) THEN
        NFNT=NODF
        GO TO 104
      END IF
C
C If the font number was given, force it to be positive and check the
C value given.  In place of illegal values, use a 1.  When a new font
C is made available, the block IF must be updated and a copy of it must
C be put in the routine PCSETR.
C
      IF (NFNT.LT.0) NFNT=-NFNT
C
      IF ((NFNT.GE. 23.AND.NFNT.LE. 24).OR.
     +    (NFNT.GE. 27.AND.NFNT.LE. 28).OR.
     +    (NFNT.GE. 31.AND.NFNT.LE. 32).OR.
     +    (NFNT.GE. 38.AND.NFNT.LE.120).OR.
     +    (NFNT.GE.123.AND.NFNT.LE.124).OR.
     +    (NFNT.GE.127.AND.NFNT.LE.128).OR.
     +    (NFNT.GE.131.AND.NFNT.LE.132).OR.NFNT.GE.138) NFNT=1
C
      GO TO 104
C
C ZOOM REQUEST
C ------------
C
C X is for X zoom, Y is for Y zoom, and Z is for Zoom.  The intent is
C to temporarily change the width, the height, or both dimensions, of
C the characters to a given percentage of normal.
C
C The integer which follows the X, Y, or Z determines the percent
C of normal to be used.  If a "Q" follows the number, it requests a
C vertical adjustment of the current center and right positions to
C make the characters look as if they were all written on the same
C base line.  This has no meaning for "X" and is just ignored.
C
C Process an "X".
C
  148 CALL PCGTDI (CHRS,NCHR,ICHR,ITMP)
      ZOOM=REAL(ITMP/100.)
      IF (ZOOM.EQ.0.) ZOOM=1.
      XMZM(1)=ZOOM*XMUL(1)
      XMZM(2)=ZOOM*XMUL(2)
      XMZM(3)=ZOOM*XMUL(3)
      IF (ICHR.LT.NCHR) THEN
        IF (CHRS(ICHR+1:ICHR+1).EQ.'Q') ICHR=ICHR+1
      END IF
      GO TO 104
C
C Process a "Y".
C
  149 CALL PCGTDI (CHRS,NCHR,ICHR,ITMP)
      ZOLD=YMZM(1)/YMUL(1)
      ZOOM=REAL(ITMP/100.)
      IF (ZOOM.EQ.0.) ZOOM=1.
      YMZM(1)=ZOOM*YMUL(1)
      YMZM(2)=ZOOM*YMUL(2)
      YMZM(3)=ZOOM*YMUL(3)
      IF (ICHR.LT.NCHR) THEN
        IF (IQUF.EQ.0.AND.CHRS(ICHR+1:ICHR+1).EQ.'Q') THEN
          ICHR=ICHR+1
          DELY=.5*HPIC(IPIC)*(ZOOM-ZOLD)
          XCEN=XCEN-DELY*STSO
          YCEN=YCEN+DELY*STCO
          XRGT=XRGT-DELY*STSO
          YRGT=YRGT+DELY*STCO
        END IF
      END IF
      GO TO 104
C
C Process a "Z".
C
  150 CALL PCGTDI (CHRS,NCHR,ICHR,ITMP)
      ZOLD=YMZM(1)/YMUL(1)
      ZOOM=REAL(ITMP/100.)
      IF (ZOOM.EQ.0.) ZOOM=1.
      XMZM(1)=ZOOM*XMUL(1)
      XMZM(2)=ZOOM*XMUL(2)
      XMZM(3)=ZOOM*XMUL(3)
      YMZM(1)=ZOOM*YMUL(1)
      YMZM(2)=ZOOM*YMUL(2)
      YMZM(3)=ZOOM*YMUL(3)
      IF (ICHR.LT.NCHR) THEN
        IF (IQUF.EQ.0.AND.CHRS(ICHR+1:ICHR+1).EQ.'Q') THEN
          ICHR=ICHR+1
          DELY=.5*HPIC(IPIC)*(ZOOM-ZOLD)
          XCEN=XCEN-DELY*STSO
          YCEN=YCEN+DELY*STCO
          XRGT=XRGT-DELY*STSO
          YRGT=YRGT+DELY*STCO
        END IF
      END IF
      GO TO 104
C
C Formats.
C
 1001 FORMAT(' PLCHHQ - CHARACTER NUMBER',I3,' (',A1,') IS NOT A',
     +       ' LEGAL FUNCTION CODE')
C
      END
