C
C $Id: ctblda.f,v 1.4 2004-03-26 21:00:08 kennison Exp $
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
      BLOCK DATA CTBLDA
C
C Declare all of the CONPACKT common blocks.
C
C
C CTCOM1 contains integer and real variables.
C
      COMMON /CTCOM1/ ANCF,ANHL,ANIL,ANLL,CDMX,CHWM,CINS,CINT(10)
      COMMON /CTCOM1/ CINU,CLDB(256),CLDL(256),CLDR(256)
      COMMON /CTCOM1/ CLDT(256),CLEV(256),CLWA(258),CXCF
      COMMON /CTCOM1/ CXIL,CYCF,CYIL,DBLF,DBLM,DBLN,DBLV,DFLD,DMAX
      COMMON /CTCOM1/ DMIN,DOPT,DVAL,EPSI,FNCM,GRAV,GRSD,GSDM,HCHL
      COMMON /CTCOM1/ HCHS,HLSR,IAIA(258),IAIB(256),IBCF,IBHL
      COMMON /CTCOM1/ IBIL,IBLL,ICAF,ICCF,ICCL(258),ICFF,ICHI
      COMMON /CTCOM1/ ICHL,ICIL,ICLL(256),ICLO,ICLP(256),ICLS
      COMMON /CTCOM1/ ICLU(258),ICLV,ICLW,IDUF,IGCL,IGLB,IGRM
      COMMON /CTCOM1/ IGRN,IGVS,IHCF,IHLE,IIWS(2),IIWU,ILBC
      COMMON /CTCOM1/ IMPF,INCX(8),INCY(8),INHL,INIL,INIT,INLL
      COMMON /CTCOM1/ IOCF,IOHL,IOLL,IPAI,IPCF,IPIC,IPIE,IPIL,IPLL
      COMMON /CTCOM1/ IRWS(4),IRWU,ISET,ITBM,IWSO,JODP,JOMA
      COMMON /CTCOM1/ JOTZ,LCTM,LEA1,LEA2,LEA3,LEE1,LEE2,LEE3,LINS
      COMMON /CTCOM1/ LINT(10),LINU,LIWB,LIWK,LIWM,LIWS(2),LNLG
      COMMON /CTCOM1/ LOEN,LOPN,LOTN,LRWC,LRWG,LRWK,LRWM,LRWS(4)
      COMMON /CTCOM1/ LSDD,LSDL,LSDM,LTCF,LTHI,LTIL,LTLO,MIRO
      COMMON /CTCOM1/ NCLB(256),NCLV,NDGL,NEDG,NEXL,NEXT,NEXU
      COMMON /CTCOM1/ NLBS,NLSD,NLZF,NOMF,NOVS,NPNT,NR04,NSDL
      COMMON /CTCOM1/ NSDR,NTRI,OORV,PITH,SCFS,SCFU,SEGL,T2DS
      COMMON /CTCOM1/ UCMN,UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL
      COMMON /CTCOM1/ UWDR,UWDT,WCCF,WCHL,WCIL,WCLL,WLCF,WLHL,WLIL
      COMMON /CTCOM1/ WLLL,WOCH,WODA,WTCD,WTGR,WTNC,WTOD,WWCF,WWHL
      COMMON /CTCOM1/ WWIL,WWLL,XLBC,XMAX,XMIN,XVPL,XVPR,XWDL,XWDR
      COMMON /CTCOM1/ YLBC,YMAX,YMIN,YVPB,YVPT,YWDB,YWDT,ZMAX,ZMIN
C
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      EQUIVALENCE (IRWS(3),IR03),(LRWS(3),LR03)
      EQUIVALENCE (IRWS(4),IR04),(LRWS(4),LR04)
      SAVE   /CTCOM1/
C
C CTCOM2 holds character parameters.
C
      COMMON /CTCOM2/ CHEX,CLBL(256),CLDP(258),CTMA,CTMB,FRMT
      COMMON /CTCOM2/ TXCF,TXHI,TXIL,TXLO
      CHARACTER*13 CHEX
      CHARACTER*64 CLBL
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*64 TXCF
      CHARACTER*32 TXHI
      CHARACTER*128 TXIL
      CHARACTER*32 TXLO
      SAVE   /CTCOM2/
C
C Below are descriptions of all the COMMON variables and default values
C for those which require defaults.
C
C
C ANCF is the parameter 'CFA', which is the angle, in degrees, at which
C the constant-field label is to be written.
C
      DATA ANCF / 0. /
C
C ANHL is the parameter 'HLA', which is the angle, in degrees, at which
C high and low labels are to be written.
C
      DATA ANHL / 0. /
C
C ANIL is the parameter 'ILA', which is the angle, in degrees, at which
C the informational label is to be written.
C
      DATA ANIL / 0. /
C
C ANLL is the parameter 'LLA', which is the angle, in degrees, at which
C line labels are to be written when ABS('LLP') is 2 or greater and
C 'LLO' is 0.
C
      DATA ANLL / 0. /
C
C CDMX is the parameter 'PC3', used in positioning labels according to
C the penalty scheme.  It specifies the maximum cumulative change in
C direction, in degrees, to be allowed along any portion of the contour
C line covered by a circle centered on a label and having a radius equal
C to half the width of the label.
C
      DATA CDMX / 60. /
C
C CHEX is used to hold the character string which stands between the
C mantissa and the exponent of a numeric value.
C
C CHWM is the parameter 'CWM', the character-width multiplier.
C
      DATA CHWM / 1. /
C
C CINS is the parameter 'CIS', the contour interval specified by the
C user (for use when 'CLS' is positive or zero).
C
      DATA CINS / 0. /
C
C CINT is the parameter 'CIT', the contour interval table.
C
      DATA CINT / 1.,2.,2.5,4.,5.,5*0. /
C
C CINU is the parameter 'CIU', the contour interval actually used.
C
      DATA CINU / 0. /
C
C CLBL is the parameter array 'LLT', each element of which is a string
C of characters to be used as a label for an associated contour level.
C
      DATA CLBL / 256*' ' /
C
C CLDP is the parameter array 'CLD', which holds the dash patterns
C associated with the contour levels.  The last two elements of this
C array give the dash patterns for the edge of the grid and the edge
C of the visible area, respectively.
C
      DATA CLDP / 258*'$$$$$$$$$$$$$$$$' /
C
C CLDB, CLDL, CLDR, and CLDT are arrays, each element of which is the
C magnitude of one of the text-extent vectors for the label in CLBL.
C
C CLEV is the parameter array 'CLV', each element of which is a contour
C level.
C
      DATA CLEV / 256*0. /
C
C CLWA is the parameter array 'CLL', each element of which specifies
C the line width to be used for an associated contour line.  The last
C two elements of this array give the line widths for the edge of the
C grid and the edge of the visible area, respectively.
C
      DATA CLWA / 258*0. /
C
C CTMA and CTMB are character-variable temporaries, used for various
C purposes throughout the code.
C
      DATA CTMA,CTMB / ' ',' ' /
C
C CXCF and CYCF are the parameters 'CFX' and 'CFY', which are the x and
C y coordinates of a basepoint relative to which the constant-field
C label is to be positioned.  These coordinates are given in a
C fractional coordinate system superimposed on the grid.
C
      DATA CXCF,CYCF / .50,.50 /
C
C CXIL and CYIL are the parameters 'ILX' and 'ILY', which are the x and
C y coordinates of a basepoint relative to which the informational label
C is to be positioned.  These coordinates are given in a fractional
C coordinate system superimposed on the grid.
C
      DATA CXIL,CYIL / .98,-.02 /
C
C DBLM is the parameter 'PC6', used in positioning labels according to
C the penalty scheme.  It specifies the minimum distance (as a fraction
C of the width of the viewport) to be allowed between any two labels on
C the same contour line.
C
      DATA DBLM / .30 /
C
C DBLF, DBLN and DBLV are the parameters 'RC1', 'RC2', and 'RC3', used
C in positioning labels at regular intervals along a contour line.
C
      DATA DBLF,DBLN,DBLV / .25,.25,.05 /
C
C DFLD is the parameter 'PC5', used in positioning labels according to
C the penalty scheme.  It is the "folding distance" (as a fraction of
C the viewport's width) in the formula for the penalty term which
C attempts to force labels to be at an optimal distance from each other.
C
      DATA DFLD / .15 /
C
C DMAX and DMIN are the parameters 'DMX' and 'DMN', the maximum and
C minimum values in the user's array of data.
C
      DATA DMAX,DMIN / 0.,0. /
C
C DOPT is the parameter 'PC4', used in positioning labels according to
C the penalty scheme.  It is the "optimal distance" (as a fraction of
C the viewport's width) in the formula for the penalty term which
C attempts to force labels to be at an optimal distance from each other.
C
      DATA DOPT / .05 /
C
C DVAL is the parameter 'DVA', which holds a data value, either the
C value at a high or low or the value of a constant field.
C
      DATA DVAL / 0. /
C
C EPSI is a machine "epsilon", whose real value is computed as required.
C
C FNCM is the parameter 'PC2', used in positioning labels according to
C the penalty scheme.  It is the maximum (estimated) number of contour
C bands allowed to cross a label.
C
      DATA FNCM / 5. /
C
C FRMT is a format to be used by the routine CTNUMB.  It is constructed
C as needed by the routine CTINRC.
C
C GRAV is the average gradient in the array of gradients computed for
C use in positioning labels according to the penalty scheme.
C
C GRSD is the standard deviation of an array of gradients.
C
C GSDM is the parameter 'PC1', used in positioning labels according to
C the penalty scheme.  "GSDM" stands for "Gradient Standard Deviation
C Multiplier".  GRAV+GSDM*GRSD is the largest gradient allowed (where
C GRAV is the average gradient and GRSD is the stardard deviation of
C the gradients).
C
      DATA GSDM / 1. /
C
C HCHL and HCHS are the parameters 'HCL' and 'HCS', respectively.  The
C former specifies the length of a hachure and the latter specifies the
C spacing of hachures along a contour line, as fractions of the width of
C the viewport.
C
      DATA HCHL,HCHS / .004,.010 /
C
C HLSR is the parameter 'HLR', which specifies the radius of a sphere
C around a possible high (low) within which field values must be less
C than (greater than) the value at the possible high (low) in order
C for it to be marked.  A positive value specifies a radius equal to
C HLSR*MAX(XMAX-XMIN,YMAX-YMIN,ZMAX-ZMIN).  A negative value specifies
C the value ABS(HLSR).
C
      DATA HLSR / .075 /
C
C IAIA is the parameter array 'AIA', each element of which is an area
C identifier for the area above the associated contour level.  The
C default values suppress area fill (except for the last two elements,
C which are used for the edge of the grid and the edge of the visible
C area, respectively - in these cases, the value is for the area on the
C other side of the edge from the area which has contour lines in it).
C
      DATA IAIA / 256*0,0,-1  /
C
C IAIB is the parameter array 'AIB', each element of which is an area
C identifier for the area below the associated contour level.  The
C default values suppress area fill.
C
      DATA IAIB / 256*0 /
C
C IBCF is the parameter 'CFB', which is zero if no box is to be drawn
C around the constant-field label.  Adding 1 to the value causes the box
C to be drawn and adding 2 to it causes the box to be filled.
C
      DATA IBCF / 0 /
C
C IBHL is the parameter 'HLB', which is zero if no box is to be drawn
C around the high/low labels.  Adding 1 to the value causes the box to
C be drawn and adding 2 to it causes the box to be filled.
C
      DATA IBHL / 0 /
C
C IBIL is the parameter 'ILB', which is zero if no box is to be drawn
C around the informational label.  Adding 1 to the value causes the box
C to be drawn and adding 2 to it causes the box to be filled.
C
      DATA IBIL / 0 /
C
C IBLL is the parameter 'LLB', which is zero if no boxes are to be drawn
C around line labels.  Adding 1 to the value causes the box to be drawn
C and adding 2 to it causes the box to be filled.
C
      DATA IBLL / 0 /
C
C ICAF is the parameter 'CAF', which determines the way in which CTCICA
C modifies an element of the cell array.
C
      DATA ICAF / 0 /
C
C ICCF is the parameter 'CFC', which determines the color of the
C constant-field label.
C
      DATA ICCF / -1 /
C
C ICCL is the parameter array 'CLC', each element of which specifies
C the color index for the lines at an associated contour level (except
C for the last two elements, which are used for the edge of the grid
C and the edge of the visible area, respectively).
C
      DATA ICCL / 256*0,2*-1 /
C
C ICFF is the parameter 'CFF' (output only) which is non-zero if the
C field being contoured is constant.
C
      DATA ICFF / 0 /
C
C ICHI is the parameter 'HIC', which determines the color of the high
C labels.
C
      DATA ICHI / -1 /
C
C ICHL is the parameter 'HLC', which determines the color of the high
C and low labels (except as overridden by the values of ICHI and ICLO).
C
      DATA ICHL / -1 /
C
C ICIL is the parameter 'ILC', which determines the color of the
C informational label.
C
      DATA ICIL / -1 /
C
C ICLL is the parameter array 'LLC', each element of which specifies
C the color index for the line labels at an associated contour level.
C
      DATA ICLL / 256*-1 /
C
C ICLO is the parameter 'LOC', which determines the color of the low
C labels.
C
      DATA ICLO / -1 /
C
C ICLP is an array used to order the contour levels.
C
C ICLS is the parameter 'CLS', the contour-level selection flag.  A
C negative value "-n" indicates that "n" contour levels should be used,
C splitting the range from the minimum field value to the maximum field
C value into n+1 equal intervals.  A positive value "+n" indicates that
C CONPACKT is to choose the contour levels, in which case, if CINS is
C greater than zero, it is used as the contour interval, but if CINS is
C less than or equal to zero, a contour interval is chosen by CONPACKT
C in such a way as to give approximately "n" contour lines.  The value
C "0" suppresses the selection of contour levels by CONPACK; the user is
C expected to set them.
C
      DATA ICLS / 16 /
C
C ICLU is the parameter array 'CLU', each element of which says what is
C to be done with the associated contour level (except for the last
C two elements, which are used for the edge of the grid and the edge of
C the visible area, respectively).
C
      DATA ICLU / 258*0 /
C
C ICLV, where used, is the index of the contour level with which
C something is being done.
C
C ICLW is set by CTPKLP and used in CTPLPS; it is the index of the
C pointer, in ICLP, of the current contour level.  ICLP(ICLW-1) is then
C the index of the next smaller contour level and ICLP(ICLW+1) is the
C index of the next larger contour level.
C
C IDUF is the parameter 'DPU', the dash pattern use flag.  The value
C zero says to draw contour lines using no dash patterns, by calling
C CURVE.  A non-zero value says to use dash patterns: a negative value
C says to call DPCURV and a positive value says to call CURVED.  When
C 'DPU' is non-zero, its absolute value is the number of repetitions of
C the dash pattern to use between each occurrence of a label.
C
      DATA IDUF / 3 /
C
C IGCL and IGLB are the parameters 'GIC' and 'GIL', group identifiers
C for contour lines and label boxes, respectively.
C
      DATA IGCL,IGLB / 3,3 /
C
C IGRM and IGRN are the first and second dimensions of a gradient array
C to be computed for use in positioning labels according to the penalty
C scheme (selected by setting ABS('LLP') to 3).  When this scheme is
C used, IGRM*IGRN words are required, in the real workspace array, for
C the gradients.  IGRM and IGRN are computed using the given value of
C LRWG and the known desired aspect ratio of the gradient array.
C
C IGVS is the parameter 'GIS', the group identifier to be used for
C vertical stripping, if that is done.
C
      DATA IGVS / 4 /
C
C IHCF is the parameter 'HCF', which says whether or not hachuring is
C turned on and what type of hachuring is to be done.
C
      DATA IHCF / 0 /
C
C IHLE is the parameter 'HLE', a flag the user can set non-zero to
C enable CONPACKT to search for highs and lows involving more than
C one adjacent equal value in the field.
C
      DATA IHLE / 0 /
C
C IIWS is an array of base indices in the integer work array.  LIWS is
C an associated array of lengths.  For each I for which LIWS(I) is not
C zero, IIWS(I)+1 is the index of the first word, and IIWS(I)+LIWS(I)
C the index of the last word, of a portion of the integer work array
C reserved for some particular purpose.
C
      DATA IIWS,LIWS / 2*0 , 2*0 /
C
C IIWU is the parameter 'IWU', which may be used to find out how much
C space was used in the integer workspace.
C
      DATA IIWU / 0 /
C
C ILBC is the color-index specifier for area fill of label boxes.
C
      DATA ILBC / 0 /
C
C IMPF is the parameter 'MAP', the mapping flag.
C
      DATA IMPF / 0 /
C
C INCX and INCY define the x and y components of the eight possible
C directions the contour-line-following vector can assume.
C
      DATA INCX / -1 , -1 ,  0 ,  1 ,  1 ,  1 ,  0 , -1 /
      DATA INCY /  0 ,  1 ,  1 ,  1 ,  0 , -1 , -1 , -1 /
C
C INHL is used to save the index of the first high/low label in the
C list of labels.
C
C INIL is used to save the index of the informational label in the list
C of labels.
C
C INIT is a flag indicating whether some necessary constants have been
C computed yet or not.
C
      DATA INIT / 0 /
C
C INLL is used to save the index of the first contour-line label in the
C list of labels.
C
C IOCF is a flag that is set by the contour-tracing routine CTTRCL to
C indicate whether an open contour is being traced (IOCF=0) or a closed
C contour is being traced (IOCF=1).  This flag is used by the hachuring
C routines to detect a problem caused by the user's not having set the
C parameter 'RWC' big enough.  IOCF is also set by the routine CTTRVE,
C which traces the visible/invisible edge, in a slightly more complex
C way, to provide the routine that puts that edge in the area map with
C the information that it needs.
C
C IOHL is the parameter 'HLO', which specifies what is to be done with
C high/low labels which overlap the informational label or the edge of
C the viewport.
C
      DATA IOHL / 3 /
C
C IOLL is the parameter 'LLO', which specifies how line labels are to
C be oriented.
C
      DATA IOLL / 0 /
C
C IPAI is the parameter 'PAI', which is the index for parameter arrays.
C
      DATA IPAI / 0 /
C
C IPCF is the parameter 'CFP', specifying how the constant-field label
C is to be positioned.
C
      DATA IPCF / 0 /
C
C IPIC is the parameter 'PIC', which indicates the number of points to
C interpolate between each pair of points defining a contour line.
C
      DATA IPIC / 0 /
C
C IPIE is the parameter 'PIE', which indicates the number of points to
C interpolate between each pair of points defining an "edge" line.
C
      DATA IPIE / 0 /
C
C IPIL is the parameter 'ILP', specifying how the informational label
C is to be positioned.
C
      DATA IPIL / 4 /
C
C IPLL is the parameter 'LLP', which says how line labels are to be
C positioned.
C
      DATA IPLL / 1 /
C
C IRWS is an array of base indices in the real work array.  LRWS is an
C associated array of lengths.  For each I for which LRWS(I) is not
C zero, IRWS(I)+1 is the index of the first word, and IRWS(I)+LRWS(I)
C the index of the last word, of a portion of the real work array
C reserved for some particular purpose.
C
      DATA IRWS,LRWS / 4*0 , 4*0 /
C
C IRWU is the parameter 'RWU', which may be used to find out how much
C space was used in the real workspace.
C
      DATA IRWU / 0 /
C
C ISET is the parameter 'SET', which says whether or not CONPACKT is to
C call SET.
C
      DATA ISET / 1 /
C
C ITBM contains the parameters 'TBX' and 'TBA', which are used to mask
C triangle blocking flags.  It has the form 4096*ITBX+ITBA; both ITBX
C and ITBA are 12-bit masks.  If ITBF is the triangle blocking flag for
C some triangle of the triangular mesh, then, in general, the triangle
C will be blocked if and only if the value of AND(XOR(ITBF,ITBX),ITBA)
C is non-zero.  The default values are such as to block only triangles
C having the low-order bit of the blocking flag set.
C
      DATA ITBM / 1 /  !  ITBX = 0, ITBA = 1
C
C IWSO is the parameter 'WSO', which says what to do when workspace
C overflow occurs.
C
      DATA IWSO / 1 /
C
C JODP, JOMA, and JOTZ are used to hold 0/1 flags extracted from the
C parameter 'NOF'.  Each is non-zero if and only if some extraneous
C portion of a numeric label may be omitted.
C
C LCTM is the length of the character string in CTMA.
C
      DATA LCTM / 1 /
C
C LEA1, LEA2, and LEA3 are the actual lengths of the three portions of
C the character string CHEX.
C
C LEE1, LEE2, and LEE3 are the effective lengths of the three portions
C of the character string CHEX.
C
C LINS is the parameter 'LIS', which is given the value "n" to specify
C that every nth contour level determined by a user-set value of CINS
C should be labelled; the value zero specifies that no levels are to be
C labelled.  (The contents of the array LINT determine the interval at
C which labels chosen by CONPACKT itself are labelled.)
C
      DATA LINS / 5 /
C
C LINT is the parameter 'LIT', the label interval table.
C
      DATA LINT / 5,5,4,5,5,5*0 /
C
C LINU is the parameter 'LIU', which is the label interval actually
C used.
C
      DATA LINU / 0 /
C
C LIWB is the length of the integer workspace to be made available to
C the routine CTTDBF, which is called to set the blocking flags for
C triangles being mapped by TDPACK.
C
      DATA LIWB / 2500 /
C
C LIWK is the length of the user's integer workspace array, as declared
C in the last call to CTMESH.
C
C LIWM is the parameter 'IWM', which specifies the length of the integer
C workspaces to be used in calls to ARDRLN (for the argument arrays IAI
C and IAG).
C
      DATA LIWM / 10 /
C
C LIWS is described with IIWS, above.
C
C LNLG is the linear/log flag for the SET call defining the mapping
C from the current viewport to the window and vice-versa.
C
C LOEN, LOPN, and LOTN are the lengths of an edge node, a point node,
C and a triangle node, respectively, as set by the routine CTMESH.
C
C LRWC is the parameter 'RWC', the number of words to be used in the
C real workspace array to hold X coordinates of points defining a piece
C of a contour line.  If line smoothing is turned off, 2*LRWC words
C will be required, LRWC for X coordinates and LRWC for Y coordinates.
C If line smoothing is turned on, 7*LRWC words will be required, 2*LRWC
C for X and Y coordinates and 5*LRWC for various scratch arrays.
C
      DATA LRWC / 1000 /
C
C LRWG is the parameter 'RWG', the number of words to be used in the
C real workspace array for gradients required by the penalty scheme for
C label positioning (which is only used when ABS('LLP') is set to 3).
C
      DATA LRWG / 1000 /
C
C LRWK is the length of the user's real workspace array, as declared in
C the last call to CTMESH.
C
C LRWM is the parameter 'RWM', which specifies the length of the real
C workspaces to be used in calls to ARDRLN (for the argument arrays XCS
C and YCS).
C
      DATA LRWM / 100 /
C
C LRWS is described with IRWS, above.
C
C LSDD is set by CTMESH to indicate the position of the leftmost
C significant digit in ABS(DMAX-DMIN).  This information is needed
C in CTPKLB.
C
C LSDL is used for the leftmost-significant-digit argument of CTNUMB,
C which is based on, but not identical with, the leftmost-significant-
C digit parameter 'NLS'.
C
C LSDM is set by CTMESH to indicate the position of the leftmost
C significant digit in MAX(ABS(DMIN),ABS(DMAX)).  This information
C is needed in CTPKLB.
C
C LTCF is the length of the constant-field label, before substitution.
C
      DATA LTCF / 31 /
C
C LTHI is the length of the label for a high, before substitution.
C
      DATA LTHI / 12 /
C
C LTIL is the length of the informational label, before substitution.
C
      DATA LTIL / 36 /
C
C LTLO is the length of the label for a low, before substitution.
C
      DATA LTLO / 12 /
C
C MIRO is a flag used to signal that the coordinate transformations in
C effect will cause mirror imaging.
C
      DATA MIRO / 0 /
C
C NCLB is an array, each element of which gives the length of the
C label in the associated element of the array CLBL.
C
C NCLV is the parameter 'NCL', which specifies the number of contour
C levels in the array CLEV.
C
      DATA NCLV / 0 /
C
C NDGL is used for the number-of-significant-digits argument of CTNUMB,
C which is based on, but not identical with, the number-of-significant-
C digits parameter 'NSD'.
C
C NEDG is the number of edges in the triangular mesh, as set by CTMESH.
C
C NEXL is the parameter 'NEL', which specifies the desired length of
C exponents in numeric labels.  A value which is zero or negative
C indicates that exponents should be written in the shortest possible
C form.  A positive value "n" indicates that a sign should be used (+
C or -) and that the length should be padded, if necessary, to n digits
C with leading zeroes.
C
      DATA NEXL / 0 /
C
C NEXT is the parameter 'NET', which is the numeric exponent type,
C specifying what characters are to be used between the mantissa of a
C numeric label and the exponent.  The value 0 implies the use of an
C E, as in FORTRAN "E format", the value 1 implies the use of function
C codes, as expected by the utility routine PLOTCHAR, to generate
C "x10n", where n is a superscript exponent, and the value 2 implies
C the use of "x10**".
C
      DATA NEXT / 1 /
C
C NEXU is the parameter 'NEU', the numeric exponent use flag.  A value
C less than or equal to zero forces the use of the exponential form in
C all numeric labels.  A positive value n indicates that the form
C without an exponent should be used as long as it requires no more
C than n characters; otherwise the form requiring the fewest characters
C should be used.
C
      DATA NEXU / 5 /
C
C NLBS specifies the current number of entries in the list of labels.
C
      DATA NLBS / 0 /
C
C NLSD is the parameter 'NLS', the leftmost-significant-digit flag.
C The value zero indicates that the leftmost non-zero digit of a
C number represented by a numeric label is to be considered its first
C significant digit.  A non-zero value indicates that the digit in the
C same digit position as the leftmost non-zero digit of the largest
C number (in absolute value) in the field is to be considered the
C leftmost significant digit.  This tends to make the numeric labels
C more consistent with one another.  Consider the following example,
C using three significant digits:
C
C    'NLS'=0:  .500  1.00  1.50  ...  9.50  10.5  ...
C    'NLS'=1:  .5    1.0   1.5   ...  9.5   10.5  ...
C
      DATA NLSD / 1 /
C
C NLZF is the parameter 'NLZ', which may be set non-zero to force a
C zero preceding the decimal point in no-exponent representations of
C numbers.
C
      DATA NLZF / 0 /
C
C NOMF is the parameter 'NOF', which specifies the numeric omission
C flags, which say what parts of a numeric label may be omitted.  The
C value 0 says that no part may be omitted.  Adding a 4 indicates that
C a leading "1" or "1." which is unnecessary (as in "1x10**13") may be
C omitted, adding a 2 indicates that a trailing decimal point (as in
C "13.") may be omitted, and adding a 1 indicates that trailing zeroes
C (as in "46.200") may be omitted.
C
      DATA NOMF / 6 /
C
C NOVS is the parameter 'NVS', which specifies the number of vertical
C strips to be created by edges added to the area map with group
C identifier 'GIS'.
C
      DATA NOVS / 1 /
C
C NR04 is the current number of words of real work space devoted to the
C list of labels which are not line labels (the informational label and
C high/low labels).  ??? THIS IS WEIRD ???
C
C NSDL is the parameter 'NSD', which specifies the maximum number of
C significant digits to be used in numeric labels representing contour
C field values.  A negative value "-n" indicates that n significant
C digits should be used.  A positive value "n" indicates that m+n digits
C should be used, where m is the number of digits that are the same for
C all values in the field.  (For example, if the minimum value is 1163.6
C and the maximum value is 1165.9, then the value of m is 3.)
C
      DATA NSDL / 4 /
C
C NSDR is the number of significant digits in a real number, which is
C computed as required by CONPACKT itself.
C
C NTRI is the number of triangles in the triangular mesh, as set by
C CTMESH.
C
C OORV is the parameter 'ORV', an out-of-range value to be returned by
C CTMPXY for both coordinates of a point which is invisible.
C
      DATA OORV / 0. /
C
C PITH is the parameter 'PIT', the "point interpolation threshold".  In
C routines that map polylines using CTMPXY, this value is used to check
C whether two points have mapped so far apart that some interpolated
C points should be inserted.  A value less than or equal to zero (like
C the default) says that no such checks are to be performed.  A value
C greater than zero represents a fraction of the height or width of the
C window in the user coordinate system.
C
      DATA PITH / 0. /
C
C SCFS is the parameter 'SFS', the scale factor selector.
C
      DATA SCFS / 1. /
C
C SCFU is the parameter 'SFU', the scale factor in use.
C
      DATA SCFU / 1. /
C
C SEGL is the parameter 'SSL', the desired distance between points used
C to draw the curves generated by contour-line-smoothing, expressed as
C a fraction of the width of the window in the coordinate system in
C which the smoothing is being done.
C
      DATA SEGL / .01 /
C
C T2DS is the parameter 'T2D', which is the contour-line smoothing flag.
C A value of zero specifies that no such smoothing is to be done.  A
C value less than zero specifies that smoothing is to be done prior to
C mapping, a value greater than zero that smoothing is to be done after
C mapping, in both cases using splines under tension.  The absolute
C value of T2DS is the desired tension.
C
      DATA T2DS / 0. /
C
C TXCF is the parameter 'CFT', the text of the constant-field label.
C
      DATA TXCF / 'CONSTANT FIELD - VALUE IS $DVA$' /
C
C TXHI is accessed by the parameter names 'HLT' and 'HIT'; it defines
C the text of the label for a high.
C
      DATA TXHI / 'H:B:$DVA$:E:' /
C
C TXIL is the parameter 'ILT', the text of the informational label.
C
      DATA TXIL / 'CONTOUR FROM $CMN$ TO $CMX$ BY $CIU$' /
C
C TXLO is accessed by the parameter names 'HLT' and 'LOT'; it defines
C the text of the label for a low.
C
      DATA TXLO / 'L:B:$DVA$:E:' /
C
C UCMN and UCMX are the parameters 'CMN' and 'CMX', which may be set to
C force use of the contour levels 'CMN', 'CMN'+'CIS', 'CMN'+2*'CIS', ...
C
      DATA UCMN,UCMX / 1.,0. /
C
C UVPL, UVPR, UVPB, and UVPT are the parameters 'VPL', 'VPR', 'VPB',
C and 'VPT', specifying the edges of an area in which the viewport is
C to lie.  Each is expressed as a fraction of the distance from left to
C right, or from bottom to top, in the plotter frame.
C
      DATA UVPL,UVPR,UVPB,UVPT / .05,.95,.05,.95 /
C
C UVPS is the parameter 'VPS', specifying the desired shape of the
C viewport.
C
      DATA UVPS / .25 /
C
C UWDL, UWDR, UWDB, and UWDT are the parameters 'WDL', 'WDR', 'WDB',
C and 'WDT', specifying the user-coordinate-system values at the left,
C right, bottom, and top edges of the window.  These are used when
C CONPACKT is asked to do the call to SET; they become arguments 5
C through 8 in the call.
C
      DATA UWDL,UWDR,UWDB,UWDT / 0.,0.,0.,0. /
C
C WCCF is the parameter 'CFS', which specifies the width of a character
C in the constant-field label, as a fraction of the viewport width.
C
      DATA WCCF / .012 /
C
C WCHL is the parameter 'HLS', which specifies the width of a character
C in the high/low labels, as a fraction of the viewport width.
C
      DATA WCHL / .012 /
C
C WCIL is the parameter 'ILS', which specifies the width of a character
C in the informational label, as a fraction of the viewport width.
C
      DATA WCIL / .012 /
C
C WCLL is the parameter 'LLS', which specifies the width of a character
C in a contour-line label positioned using the regular scheme or the
C penalty scheme, as a fraction of the viewport width.
C
      DATA WCLL / .010 /
C
C WLCF, WLHL, WLIL, and WLLL are line-width specifiers for the boxes
C around constant-field, high/low, informational, and line labels,
C respectively.
C
      DATA WLCF,WLHL,WLIL,WLLL / 0.,0.,0.,0. /
C
C WOCH and WODA are the parameters 'DPS' and 'DPV'.  WOCH specifies the
C width of a character (other than a dollar sign or an apostrophe) in a
C dash pattern.  WODA specifies the length of the solid line represented
C by a dollar sign or the gap represented by an apostrophe in a dash
C pattern.  Both are given as fractions of the viewport width.
C
      DATA WOCH,WODA / .010,.005 /
C
C WTCD is the parameter 'PW3', used in positioning labels according to
C the penalty scheme.  It is the weight for the "change-in-direction"
C term in the penalty formula.
C
      DATA WTCD / 1. /
C
C WTGR is the parameter 'PW1', used in positioning labels according to
C the penalty scheme.  It is the weight for the "gradient" term in the
C penalty formula.
C
      DATA WTGR / 2. /
C
C WTNC is the parameter 'PW2', used in positioning labels according to
C the penalty scheme.  It is the weight for the "number-of-contours"
C term in the penalty formula.
C
      DATA WTNC / 0. /
C
C WTOD is the parameter 'PW4', used in positioning labels according to
C the penalty scheme.  It is the weight for the optimal-distance term
C in the penalty formula.
C
      DATA WTOD / 1. /
C
C WWCF is the parameter 'CFW', which specifies the width of the white
C space around the constant-field label, as a fraction of the viewport
C width.
C
      DATA WWCF / .005 /
C
C WWHL is the parameter 'HLW', which specifies the width of the white
C space around a high/low label, as a fraction of the viewport width.
C
      DATA WWHL / .005 /
C
C WWIL is the parameter 'ILW', which specifies the width of the white
C space around the informational label, as a fraction of the viewport
C width.
C
      DATA WWIL / .005 /
C
C WWLL is the parameter 'LLW', which specifies the width of the white
C space around a contour-line label positioned using the regular scheme
C or the penalty scheme, as a fraction of the viewport width.
C
      DATA WWLL / .005 /
C
C XLBC is the parameter 'LBX', which may be retrieved in any of the
C change routines and specifies the X position of the label's center,
C in the current user coordinate system.
C
      DATA XLBC / 0. /
C
C XMAX and XMIN are the parameters 'XMX' and 'XMN', the maximum and
C minimum values among the user's X coordinate data.
C
      DATA XMAX,XMIN / 0.,0. /
C
C XVPL and XVPR specify the positions of the current viewport's left
C and right edges.  Both values are between 0. and 1.
C
C XWDL and XWDR are the values at the left and right edges of the
C current window in the user coordinate system.
C
C YLBC is the parameter 'LBY', which may be retrieved in any of the
C change routines and specifies the Y position of the label's center,
C in the current user coordinate system.
C
      DATA YLBC / 0. /
C
C YMAX and YMIN are the parameters 'YMX' and 'YMN', the maximum and
C minimum values among the user's Y coordinate data.
C
      DATA YMAX,YMIN / 0.,0. /
C
C YVPB and YVPT specify the positions of the current viewport's bottom
C and top edges.  Both values are between 0. and 1.
C
C YWDB and YWDT are the values at the bottom and top edges of the
C current window in the user coordinate system.
C
C ZMAX and ZMIN are the parameters 'ZMX' and 'ZMN', the maximum and
C minimum values among the user's Z coordinate data.
C
      DATA ZMAX,ZMIN / 0.,0. /
C
      END
