/*
 *  $Id: ncargC.h,v 1.25 2008-07-23 17:31:26 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#ifndef _ncargC_h
#define _ncargC_h

#include <stdio.h>
#include <ncarg/gks.h>
#include <ncarg/c.h>

#ifdef  __cplusplus
#define NCARG_PROTO_BEGIN       extern "C" {
#define NCARG_PROTO_END         }
#else
#define NCARG_PROTO_BEGIN
#define NCARG_PROTO_END 
#endif

/* This macro protects C function names from C++ name-mangling. */
NCARG_PROTO_BEGIN

/*  NCAR Graphics Utility C-bindings  */
    
extern void c_ardrln(
#ifdef  NeedFuncProto
    int *iam,
    float *xcd,
    float *ycd,
    int ncd,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*colrln_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai)
#endif
);

extern void c_aredam(
#ifdef  NeedFuncProto
    int *iam,
    float *xca,
    float *yca,
    int lca,
    int igi,
    int idl,
    int idr
#endif
);

extern void c_argeti(
#ifdef  NeedFuncProto
    char *ipn,
    int *ivl
#endif
);

extern void c_argetr(
#ifdef  NeedFuncProto
    char *ipn,
    float *rvl
#endif
);

extern void c_argtai(
#ifdef  NeedFuncProto
    int *iam,
    float xcd,
    float ycd,
    int *iai,
    int *iag,
    int mai,
    int *nai,
    int icf
#endif
);

extern void c_arinam(
#ifdef  NeedFuncProto
    int *iam,
    int lam
#endif
);

extern void c_armvam(
#ifdef  NeedFuncProto
    int *iam,
    int *ian,
    int lan
#endif
);

extern void c_arpram(
#ifdef  NeedFuncProto
    int *iam,
    int if1,
    int if2,
    int if3
#endif
);

extern void c_arscam(
#ifdef  NeedFuncProto
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*cpcolr_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai)
#endif
);

extern void c_arseti(
#ifdef  NeedFuncProto
    char *ipn,
    int ivl
#endif
);

extern void c_arsetr(
#ifdef  NeedFuncProto
    char *ipn,
    float rvl
#endif
);

extern void c_ardbpa(
#ifdef  NeedFuncProto
    int *iam,
    int igi,
    char *lab
#endif
);

extern void c_agback(
#ifdef  NeedFuncProto
    void
#endif
);

extern char *c_agbnch(
#ifdef  NeedFuncProto
    int idsh
#endif
);

extern void c_agcurv(
#ifdef  NeedFuncProto
    float *xvec,
    int iiex,
    float *yvec,
    int iiey,
    int nexy,
    int kdsh
#endif
);

extern char *c_agdshn(
#ifdef  NeedFuncProto
    int idsh
#endif
);

extern void c_aggetc(
#ifdef  NeedFuncProto
    char *tpid,
    char *cusr,
    int len
#endif
);

extern void c_aggetf(
#ifdef  NeedFuncProto
    char *tpid,
    float *fusr
#endif
);

extern void c_aggeti(
#ifdef  NeedFuncProto
    char *tpid,
    int *iusr
#endif
);

extern void c_aggetp(
#ifdef  NeedFuncProto
    char *tpid,
    float *fura,
    int lura
#endif
);

extern void c_aggetr(
#ifdef  NeedFuncProto
    char *tpid,
    float *fusr
#endif
);

extern void c_agrstr(
#ifdef  NeedFuncProto
    int ifno
#endif
);

extern void c_agsave(
#ifdef  NeedFuncProto
    int ifno
#endif
);

extern void c_agsetc(
#ifdef  NeedFuncProto
    char *tpid,
    char *cusr
#endif
);

extern void c_agsetf(
#ifdef  NeedFuncProto
    char *tpid,
    float fusr
#endif
);

extern void c_agseti(
#ifdef  NeedFuncProto
    char *tpid,
    int iusr
#endif
);

extern void c_agsetp(
#ifdef  NeedFuncProto
    char *tpid,
    float *fura,
    int lura
#endif
);

extern void c_agsetr(
#ifdef  NeedFuncProto
    char *tpid,
    float fusr
#endif
);

extern void c_agstup(
#ifdef  NeedFuncProto
    float *xdra,
    int nvix,
    int iivx,
    int nevx,
    int iiex,
    float *ydra,
    int nviy,
    int iivy,
    int nevy,
    int iiey
#endif
);

extern void c_anotat(
#ifdef  NeedFuncProto
    char *labx,
    char *laby,
    int lbac,
    int lset,
    int ndsh,
    char *dshl[]
#endif
);

extern void c_displa(
#ifdef  NeedFuncProto
    int lfra,
    int lrow,
    int ltyp
#endif
);

extern void c_ezmxy(
#ifdef  NeedFuncProto
    float *xdra,
    float *ydra,
    int idxy,
    int many,
    int npts,
    char *labg
#endif
);

extern void c_ezmy(
#ifdef  NeedFuncProto
    float *ydra,
    int idxy,
    int many,
    int npts,
    char *labg
#endif
);

extern void c_ezxy(
#ifdef  NeedFuncProto
    float *xdra,
    float *ydra,
    int npts,
    char *labg
#endif
);

extern void c_idbvip(
#ifdef  NeedFuncProto
    int md,
    int ndp,
    float *xd,
    float *yd,
    float *zd,
    int nip,
    float *xi,
    float *yi,
    float *zi,
    int *iwk,
    float *wk
#endif
);

extern void c_idsfft(
#ifdef  NeedFuncProto
    int md,
    int ndp,
    float *xd,
    float *yd,
    float *zd,
    int nxi,
    int nyi,
    int nzi,
    float *xi,
    float *yi,
    float *zi,
    int *iwk,
    float *wk
#endif
);

extern void c_ezy(
#ifdef  NeedFuncProto
    float *ydra,
    int npts,
    char *labg
#endif
);

extern void c_hlsrgb(
#ifdef  NeedFuncProto
    float h,
    float l,
    float s,
    float *r,
    float *g,
    float *b
#endif
);

extern void c_hsvrgb(
#ifdef  NeedFuncProto
    float h,
    float s,
    float v,
    float *r,
    float *g,
    float *b
#endif
);

extern void c_rgbhls(
#ifdef  NeedFuncProto
    float r,
    float g,
    float b,
    float *h,
    float *l,
    float *s
#endif
);

extern void c_rgbhsv(
#ifdef  NeedFuncProto
    float r,
    float g,
    float b,
    float *h,
    float *s,
    float *v
#endif
);

extern void c_rgbyiq(
#ifdef  NeedFuncProto
    float r,
    float g,
    float b,
    float *y,
    float *i,
    float *q
#endif
);

extern void c_yiqrgb(
#ifdef  NeedFuncProto
    float y,
    float i,
    float q,
    float *r,
    float *g,
    float *b
#endif
);

extern void c_cpback(
#ifdef  NeedFuncProto
    float *zdat,
    float *rwrk,
    int *iwrk
#endif
);

extern void c_cpclam(
#ifdef  NeedFuncProto
    float *zdat,
    float *rwrk,
    int *iwrk,
    int *iama
#endif
);

extern void c_cpcldm(
#ifdef  NeedFuncProto
    float *zdat,
    float *rwrk,
    int *iwrk,
    int *iama,
    int (*rtpl_)(
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi)
#endif
);

extern void c_cpcldr(
#ifdef  NeedFuncProto
    float *zdat,
    float *rwrk,
    int *iwrk
#endif
);

extern void c_cpcica(
#ifdef  NeedFuncProto
    float *zdat,
    float *rwrk,
    int *iwrk,
    int *icra,
    int ica1,
    int icam,
    int ican,
    float xcpf,
    float ycpf,
    float xcqf,
    float ycqf
#endif
);

extern void c_cpcltr(
#ifdef  NeedFuncProto
    float *zdat,
    float *rwrk,
    int *iwrk,
    float clvl,
    int *ijmp,
    int *irw1,
    int *irw2,
    int *nrwk
#endif
);

extern void c_cpcnrc(
#ifdef  NeedFuncProto
    float *zdat,
    int kzdt,
    int mzdt,
    int nzdt,
    float flow,
    float fhgh,
    float finc,
    int kset,
    int nhgh,
    int ndsh
#endif
);

extern void c_cpezct(
#ifdef  NeedFuncProto
    float *zdat,
    int mzdt,
    int nzdt
#endif
);

extern void c_cpgetc(
#ifdef  NeedFuncProto
    char *whch,
    char *cval,
    int len
#endif
);

extern void c_cpgeti(
#ifdef  NeedFuncProto
    char *whch,
    int *ival
#endif
);

extern void c_cpgetr(
#ifdef  NeedFuncProto
    char *whch,
    float *rval
#endif
);

extern void c_cplbam(
#ifdef  NeedFuncProto
    float *zdat,
    float *rwrk,
    int *iwrk,
    int *iama
#endif
);

extern void c_cplbdr(
#ifdef  NeedFuncProto
    float *zdat,
    float *rwrk,
    int *iwrk
#endif
);

extern void c_cppkcl(
#ifdef  NeedFuncProto
    float *zdat,
    float *rwrk,
    int *iwrk
#endif
);

extern void c_cppklb(
#ifdef  NeedFuncProto
    float *zdat,
    float *rwrk,
    int *iwrk
#endif
);

extern void c_cprect(
#ifdef  NeedFuncProto
    float *zdat,
    int kzdt,
    int mzdt,
    int nzdt,
    float *rwrk,
    int krwk,
    int *iwrk,
    int kiwk
#endif
);

extern void c_cprset(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_cpscae(
#ifdef  NeedFuncProto
    int *icra,
    int ica1,
    int icam,
    int ican,
    float xcpf,
    float ycpf,
    float xcqf,
    float ycqf,
    int ind1,
    int ind2,
    int icaf,
    int iaid
#endif
);

extern void c_cpsetc(
#ifdef  NeedFuncProto
    char *whch,
    char *cval
#endif
);

extern void c_cpseti(
#ifdef  NeedFuncProto
    char *whch,
    int ival
#endif
);

extern void c_cpsetr(
#ifdef  NeedFuncProto
    char *whch,
    float rval
#endif
);

extern void c_cpsprs(
#ifdef  NeedFuncProto
    float *zsps,
    int ksps,
    int msps,
    int nsps,
    float *rwrk,
    int krwk,
    int *iwrk,
    int kiwk,
    float *zdat,
    int kzdt
#endif
);

extern void c_cpsps1(
#ifdef  NeedFuncProto
    float *zsps,
    int ksps,
    int msps,
    int nsps,
    float *rwrk,
    int krwk,
    int *iwrk,
    int kiwk,
    float *zdat,
    int kzdt
#endif
);

extern void c_cpsps2(
#ifdef  NeedFuncProto
    float *xsps,
    float *ysps,
    float *zsps,
    int ksps,
    int msps,
    int nsps,
    float *rwrk,
    int krwk,
    int *iwrk,
    int kiwk,
    float *zdat,
    int kzdt
#endif
);

extern void c_cpmviw(
#ifdef  NeedFuncProto
    int *iwko,
    int *iwrk,
    int lwkn
#endif
);

extern void c_cpmvrw(
#ifdef  NeedFuncProto
    float *rwko,
    float *rwrk,
    int lwkn
#endif
);

extern void c_ctback(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk
#endif
);

extern void c_ctcica(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    int *icra,
    int ica1,
    int icam,
    int ican,
    float xcpf,
    float ycpf,
    float xcqf,
    float ycqf
#endif
);

extern void c_ctclam(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    int *iama
#endif
);

extern void c_ctcldm(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    int *iama,
    int (*rtpl_)(
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi)
#endif
);

extern void c_ctcldr(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk
#endif
);

extern void c_ctcltr(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    float clvl,
    int *ijmp,
    int *irw1,
    int *irw2,
    int *nrwk
#endif
);

extern void c_ctgetc(
#ifdef  NeedFuncProto
    char *whch,
    char *cval,
    int len
#endif
);

extern void c_ctgeti(
#ifdef  NeedFuncProto
    char *whch,
    int *ival
#endif
);

extern void c_ctgetr(
#ifdef  NeedFuncProto
    char *whch,
    float *rval
#endif
);

extern void c_ctlbam(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    int *iama
#endif
);

extern void c_ctlbdr(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk
#endif
);

extern void c_ctmesh(
#ifdef  NeedFuncProto
    float *rpnt,
    int npnt,
    int lopn,
    int *iedg,
    int nedg,
    int loen,
    int *itri,
    int ntri,
    int lotn,
    float *rwrk,
    int lrwk,
    int *iwrk,
    int liwk
#endif
);

extern void c_ctmviw(
#ifdef  NeedFuncProto
    int *iwko,
    int *iwrk,
    int lwkn
#endif
);

extern void c_ctmvrw(
#ifdef  NeedFuncProto
    float *rwko,
    float *rwrk,
    int lwkn
#endif
);

extern void c_ctpkcl(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk
#endif
);

extern void c_ctpklb(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk
#endif
);

extern void c_ctrset(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_ctsetc(
#ifdef  NeedFuncProto
    char *whch,
    char *cval
#endif
);

extern void c_ctseti(
#ifdef  NeedFuncProto
    char *whch,
    int ival
#endif
);

extern void c_ctsetr(
#ifdef  NeedFuncProto
    char *whch,
    float rval
#endif
);

extern void c_cttdbf(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    int iflg,
    float atol
#endif
);

extern void c_cttdbm(
#ifdef  NeedFuncProto
    int ihbx,
    int iebx,
    int iwbx,
    int iubx,
    int ihba,
    int ieba,
    int iwba,
    int iuba
#endif
);

extern void c_cttdca(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    int *icra,
    int ica1,
    int icam,
    int ican,
    float xcpf,
    float ycpf,
    float xcqf,
    float ycqf
#endif
);

extern void c_cttddm(
#ifdef  NeedFuncProto
    float *rpnt,
    int *iedg,
    int *itri,
    float *rwrk,
    int *iwrk,
    int idia
#endif
);

extern void c_cttmrg(
#ifdef  NeedFuncProto
    int idim,
    int jdim,
    float *rlat,
    float *rlon,
    float *rdat,
    int *iscr,
    float sval,
    int (*rtmi_) (
        int *idim,
        int *jdim,
        int *iini,
        int *jini,
        int *iino,
        int *jino),
    float *rpnt,
    int mpnt,
    int *npnt,
    int lopn,
    int *iedg,
    int medg,
    int *nedg,
    int loen,
    int *itri,
    int mtri,
    int *ntri,
    int lotn
#endif
);

extern void c_cttmtl(
#ifdef  NeedFuncProto
    int ntto,
    float *tbuf,
    int mbuf,
    int *nbuf,
    int *ippp,
    int mppp,
    int *nppp,
    int *ippe,
    int mppe,
    int *nppe,
    float *rpnt,
    int mpnt,
    int *npnt,
    int lopn,
    int *iedg,
    int medg,
    int *nedg,
    int loen,
    int *itri,
    int mtri,
    int *ntri,
    int lotn
#endif
);

extern void c_curved(
#ifdef  NeedFuncProto
    float *x,
    float *y,
    int n
#endif
);

extern void c_dashdb(
#ifdef  NeedFuncProto
    int *ipat
#endif
);

extern void c_dashdc(
#ifdef  NeedFuncProto
    char *ipat,
    int jcrt,
    int jsize
#endif
);

extern void c_frstd(
#ifdef  NeedFuncProto
    float x,
    float y
#endif
);

extern void c_lastd(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_lined(
#ifdef  NeedFuncProto
    float xa,
    float ya,
    float xb,
    float yb
#endif
);

extern void c_vectd(
#ifdef  NeedFuncProto
    float x,
    float y
#endif
);

extern void c_dpcurv(
#ifdef  NeedFuncProto
    float *xcpu,
    float *ycpu,
    int npts
#endif
);

extern void c_dpdraw(
#ifdef  NeedFuncProto
    float xcpf,
    float ycpf,
    int ifvl
#endif
);

extern void c_dpfrst(
#ifdef NeedFuncProto
    float xcpu,
    float ycpu
#endif
);

extern void c_dpgetc(
#ifdef NeedFuncProto
    char *pnam,
    char *cval,
    int len
#endif
);

extern void c_dpgeti(
#ifdef NeedFuncProto
    char *pnam,
    int *ival
#endif
);

extern void c_dpgetr(
#ifdef NeedFuncProto
    char *pnam,
    float *rval
#endif
);

extern void c_dplast(
#ifdef NeedFuncProto
    void
#endif
);

extern void c_dpline(
#ifdef NeedFuncProto
    float xcp1,
    float ycp1,
    float xcp2,
    float ycp2
#endif
);

extern void c_dpsetc(
#ifdef NeedFuncProto
    char *pnam,
    char *cval
#endif
);

extern void c_dpseti(
#ifdef NeedFuncProto
    char *pnam,
    int ival
#endif
);

extern void c_dpsetr(
#ifdef NeedFuncProto
    char *pnam,
    float rval
#endif
);

extern void c_dpsmth(
#ifdef NeedFuncProto
    float xcpf,
    float ycpf,
    int ifvl
#endif
);

extern void c_dpvect(
#ifdef NeedFuncProto
    float xcpu,
    float ycpu
#endif
);

extern int c_mapaci(
#ifdef NeedFuncProto
    int iai
#endif
);

extern void c_mapbla(
#ifdef NeedFuncProto
    int *iamp
#endif
);

extern void c_mapblm(
#ifdef NeedFuncProto
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*lpr_)(
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi)
#endif
);

extern void c_mapdrw(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mapfst(
#ifdef NeedFuncProto
    float xlat,
    float xlon
#endif
);

extern void c_mapgci(
#ifdef NeedFuncProto
    float alat,
    float alon,
    float blat,
    float blon,
    int nopi,
    float *rlti,
    float *rlni
#endif
);

extern void c_mapgrd(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mapgrm(
#ifdef NeedFuncProto
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*lpr_)(
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi)
#endif
);

extern void c_mapgtc(
#ifdef NeedFuncProto
    char *whch,
    char *cval,
    int len
#endif
);

extern void c_mapgtd(
#ifdef NeedFuncProto
    char *whch,
    double *dval
#endif
);

extern void c_mapgti(
#ifdef NeedFuncProto
    char *whch,
    int *ival
#endif
);

extern void c_mapgtl(
#ifdef NeedFuncProto
    char *whch,
    int *lval
#endif
);

extern void c_mapgtr(
#ifdef NeedFuncProto
    char *whch,
    float *rval
#endif
);

extern void c_mapint(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mapiq(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mapiqa(
#ifdef NeedFuncProto
    int *iamp,
    int igrp,
    int idlt,
    int idrt
#endif
);

extern void c_mapiqd(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mapiqm(
#ifdef NeedFuncProto
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*lpr_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai)
#endif
);

extern void c_mapit(
#ifdef NeedFuncProto
    float xlat,
    float xlon,
    int ifst
#endif
);

extern void c_mapita(
#ifdef NeedFuncProto
    float xlat,
    float xlon,
    int ifst,
    int *iamp,
    int igrp,
    int idlt,
    int idrt
#endif
);

extern void c_mapitd(
#ifdef NeedFuncProto
    float xlat,
    float xlon,
    int ifst
#endif
);

extern void c_mapitm(
#ifdef NeedFuncProto
    float xlat,
    float xlon,
    int ifst,
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*lpr_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai)
#endif
);

extern void c_maplbl(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_maplmb(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_maplmm(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_maplot(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mappos(
#ifdef NeedFuncProto
    float arg1,
    float arg2,
    float arg3,
    float arg4
#endif
);

extern void c_maproj(
#ifdef NeedFuncProto
    char *str,
    float arg2,
    float arg3,
    float arg4
#endif
);

extern void c_maprs(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_maprst(
#ifdef NeedFuncProto
    int ifno
#endif
);

extern void c_mapsav(
#ifdef NeedFuncProto
    int ifno
#endif
);

extern void c_mapset(
#ifdef NeedFuncProto
    char *str,
    float *arg2,
    float *arg3,
    float *arg4,
    float *arg5
#endif
);

extern void c_mapstc(
#ifdef NeedFuncProto
    char *whch,
    char *cval
#endif
);

extern void c_mapstd(
#ifdef NeedFuncProto
    char *whch,
    double dval
#endif
);

extern void c_mapsti(
#ifdef NeedFuncProto
    char *whch,
    int ival
#endif
);

extern void c_mapstl(
#ifdef NeedFuncProto
    char *whch,
    int lval
#endif
);

extern void c_mapstr(
#ifdef NeedFuncProto
    char *whch,
    float rval
#endif
);

extern void c_maptra(
#ifdef NeedFuncProto
    float rlat,
    float rlon,
    float *uval,
    float *vval
#endif
);

extern void c_maptri(
#ifdef NeedFuncProto
    float uval,
    float vval,
    float *rlat,
    float *rlon
#endif
);

extern void c_maptrn(
#ifdef NeedFuncProto
    float rlat,
    float rlon,
    float *u,
    float *v
#endif
);

extern void c_mapvec(
#ifdef NeedFuncProto
    float xlat,
    float xlon
#endif
);

extern void c_maqini(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_maqtra(
#ifdef NeedFuncProto
    float rlat,
    float rlon,
    float *uval,
    float *vval
#endif
);

extern void c_maqtri(
#ifdef NeedFuncProto
    float uval,
    float vval,
    float *rlat,
    float *rlon
#endif
);

extern void c_maqtrn(
#ifdef NeedFuncProto
    float rlat,
    float rlon,
    float *u,
    float *v
#endif
);

extern char *c_mdfnme(
#ifdef NeedFuncProto
    int iain,
    int ilvl
#endif
);

extern void c_mdgcog(
#ifdef NeedFuncProto
    double clat,
    double clon,
    double crad,
    double *alat,
    double *alon,
    int npts
#endif
);

extern void c_mdgetc(
#ifdef NeedFuncProto
    char *whch,
    char *cval,
    int len
#endif
);

extern void c_mdgetd(
#ifdef NeedFuncProto
    char *whch,
    double *dval
#endif
);

extern void c_mdgeti(
#ifdef NeedFuncProto
    char *whch,
    int *ival
#endif
);

extern void c_mdgetl(
#ifdef NeedFuncProto
    char *whch,
    int *lval
#endif
);

extern void c_mdgetr(
#ifdef NeedFuncProto
    char *whch,
    float *rval
#endif
);

extern void c_mdglty(
#ifdef NeedFuncProto
    int *ilty
#endif
);

extern int c_mdiaty(
#ifdef NeedFuncProto
    int iain
#endif
);

extern int c_mdifnb(
#ifdef NeedFuncProto
    char *chrs
#endif
);

extern int c_mdilnb(
#ifdef NeedFuncProto
    char *chrs
#endif
);

extern int c_mdiola(
#ifdef NeedFuncProto
    int iaid,
    int ilvl
#endif
);

extern int c_mdiosa(
#ifdef NeedFuncProto
    int iaid,
    int ilvl
#endif
);

extern int c_mdipai(
#ifdef NeedFuncProto
    int iain,
    int iaip
#endif
);

extern int c_mdipan(
#ifdef NeedFuncProto
    int  iain,
    char *anme
#endif
);

extern int c_mdipar(
#ifdef NeedFuncProto
    int iain
#endif
);

extern int c_mdisci(
#ifdef NeedFuncProto
    int iain
#endif
);

extern void c_mdlach(
#ifdef NeedFuncProto
    double rlat,
    char *chrs,
    int clen,
    int *nchr
#endif
);

extern void c_mdlbln(
#ifdef NeedFuncProto
    float xcop,
    float ycop,
    float xcoq,
    float ycoq,
    float offx,
    float offy,
    float size,
    float angl,
    float cent
#endif
);

extern void c_mdlblt(
#ifdef NeedFuncProto
    float xcop,
    float ycop,
    float xcoq,
    float ycoq,
    float offx,
    float offy,
    float size,
    float angl,
    float cent
#endif
);

extern void c_mdlnam(
#ifdef NeedFuncProto
    char *flnm,
    int   ilvl,
    int  *iama
#endif
);

extern void c_mdlndm(
#ifdef NeedFuncProto
    char  *flnm,
    int    ilvl,
    int   *iama,
    float *xcra,
    float *ycra,
    int    mcra,
    int   *iaai,
    int   *iagi,
    int    mnog,
    int (*ulpr_)(
        float *xcra,
        float *ycra,
        int   *ncra,
        int   *iaai,
        int   *iagi,
        int   *ngpsi)
#endif
);

extern void c_mdlndr(
#ifdef NeedFuncProto
    char *flnm,
    int   ilvl
#endif
);

extern void c_mdlnri(
#ifdef NeedFuncProto
    char *flnm
#endif
);

extern void c_mdloch(
#ifdef NeedFuncProto
    double rlon,
    char *chrs,
    int clen,
    int *nchr
#endif
);

extern char *c_mdname(
#ifdef NeedFuncProto
    int iain
#endif
);

extern int c_mdpaci(
#ifdef NeedFuncProto
    int iai
#endif
);

extern void c_mdpbla(
#ifdef NeedFuncProto
    int *iamp
#endif
);

extern void c_mdpblm(
#ifdef NeedFuncProto
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*lpr_)(
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi)
#endif
);

extern void c_mdpdrw(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mdpfst(
#ifdef NeedFuncProto
    double xlat,
    double xlon
#endif
);

extern void c_mdpgci(
#ifdef NeedFuncProto
    double alat,
    double alon,
    double blat,
    double blon,
    int nopi,
    double *rlti,
    double *rlni
#endif
);

extern void c_mdpgrd(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mdpgrm(
#ifdef NeedFuncProto
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*lpr_)(
        float *xcra,
        float *ycra,
        int *ncra,
        int *iaia,
        int *igia,
        int *nagi)
#endif
);

extern void c_mdpint(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mdpiq(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mdpiqa(
#ifdef NeedFuncProto
    int *iamp,
    int igrp,
    int idlt,
    int idrt
#endif
);

extern void c_mdpiqd(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mdpiqm(
#ifdef NeedFuncProto
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*lpr_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai)
#endif
);

extern void c_mdpit(
#ifdef NeedFuncProto
    double xlat,
    double xlon,
    int ifst
#endif
);

extern void c_mdpita(
#ifdef NeedFuncProto
    double xlat,
    double xlon,
    int ifst,
    int *iamp,
    int igrp,
    int idlt,
    int idrt
#endif
);

extern void c_mdpitd(
#ifdef NeedFuncProto
    double xlat,
    double xlon,
    int ifst
#endif
);

extern void c_mdpitm(
#ifdef NeedFuncProto
    double xlat,
    double xlon,
    int ifst,
    int *iam,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*lpr_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai)
#endif
);

extern void c_mdplbl(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mdplmb(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mdplmm(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mdplot(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mdppos(
#ifdef NeedFuncProto
    double arg1,
    double arg2,
    double arg3,
    double arg4
#endif
);

extern void c_mdproj(
#ifdef NeedFuncProto
    char *str,
    double arg2,
    double arg3,
    double arg4
#endif
);

extern void c_mdprs(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mdprst(
#ifdef NeedFuncProto
    int ifno
#endif
);

extern void c_mdpsav(
#ifdef NeedFuncProto
    int ifno
#endif
);

extern void c_mdpset(
#ifdef NeedFuncProto
    char *str,
    double *arg2,
    double *arg3,
    double *arg4,
    double *arg5
#endif
);

extern void c_mdptra(
#ifdef NeedFuncProto
    double rlat,
    double rlon,
    double *uval,
    double *vval
#endif
);

extern void c_mdptri(
#ifdef NeedFuncProto
    double uval,
    double vval,
    double *rlat,
    double *rlon
#endif
);

extern void c_mdptrn(
#ifdef NeedFuncProto
    double rlat,
    double rlon,
    double *u,
    double *v
#endif
);

extern void c_mdpvec(
#ifdef NeedFuncProto
    double xlat,
    double xlon
#endif
);

extern void c_mdqini(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mdqtra(
#ifdef NeedFuncProto
    double rlat,
    double rlon,
    double *uval,
    double *vval
#endif
);

extern void c_mdqtri(
#ifdef NeedFuncProto
    double uval,
    double vval,
    double *rlat,
    double *rlon
#endif
);

extern void c_mdqtrn(
#ifdef NeedFuncProto
    double rlat,
    double rlon,
    double *u,
    double *v
#endif
);

extern void c_mdrgdl(
#ifdef NeedFuncProto
    int *irgl
#endif
);

extern void c_mdrggc(
#ifdef NeedFuncProto
    int lcol[5],
    int lcsf[5]
#endif
);

extern void c_mdrgol(
#ifdef NeedFuncProto
    int irgl,
    float *rwrk,
    int lrwk
#endif
);

extern void c_mdrgsc(
#ifdef NeedFuncProto
    int lcol[5],
    int lcsf[5]
#endif
);

extern void c_mdrgsf(
#ifdef NeedFuncProto
    int irgl,
    float *rwrk,
    int lrwk,
    int *iama,
    int lama
#endif
);

extern void c_mdritd(
#ifdef NeedFuncProto
    int iaxs,
    double angl,
    double *ucrd,
    double *vcrd,
    double *wcrd
#endif
);

extern void c_mdrset(
#ifdef  NeedFuncProto
    void
#endif
);

extern double c_mdscal(
#ifdef NeedFuncProto
    float xcop,
    float ycop,
    float xcoq,
    float ycoq
#endif
);

extern void c_mdsetc(
#ifdef NeedFuncProto
    char *whch,
    char *cval
#endif
);

extern void c_mdsetd(
#ifdef NeedFuncProto
    char *whch,
    double dval
#endif
);

extern void c_mdseti(
#ifdef NeedFuncProto
    char *whch,
    int ival
#endif
);

extern void c_mdsetl(
#ifdef NeedFuncProto
    char *whch,
    int lval
#endif
);

extern void c_mdsetr(
#ifdef NeedFuncProto
    char *whch,
    float rval
#endif
);

extern void c_mdutfd(
#ifdef NeedFuncProto
    double rlat,
    double rlon,
    double *u,
    double *v
#endif
);

extern void c_mdutfs(
#ifdef NeedFuncProto
    float rlat,
    float rlon,
    float *u,
    float *v
#endif
);

extern void c_mdutid(
#ifdef NeedFuncProto
    double uval,
    double vval,
    double *rlat,
    double *rlon
#endif
);

extern void c_mdutin(
#ifdef NeedFuncProto
    int iprj,
    int izon,
    int isph,
    double *para,
    double umin,
    double umax,
    double vmin,
    double vmax
#endif
);

extern void c_mdutis(
#ifdef NeedFuncProto
    float uval,
    float vval,
    float *rlat,
    float *rlon
#endif
);

extern char *c_mpfnme(
#ifdef NeedFuncProto
    int iain,
    int ilvl
#endif
);

extern void c_mpgetc(
#ifdef NeedFuncProto
    char *whch,
    char *cval,
    int len
#endif
);

extern void c_mpgetd(
#ifdef NeedFuncProto
    char *whch,
    double *rval
#endif
);

extern void c_mpgeti(
#ifdef NeedFuncProto
    char *whch,
    int *ival
#endif
);

extern void c_mpgetl(
#ifdef NeedFuncProto
    char *whch,
    int *lval
#endif
);

extern void c_mpgetr(
#ifdef NeedFuncProto
    char *whch,
    float *rval
#endif
);

extern void c_mpglty(
#ifdef NeedFuncProto
    int *ilty
#endif
);

extern int c_mpiaty(
#ifdef NeedFuncProto
    int iain
#endif
);

extern int c_mpifnb(
#ifdef NeedFuncProto
    char *chrs
#endif
);

extern int c_mpilnb(
#ifdef NeedFuncProto
    char *chrs
#endif
);

extern int c_mpiola(
#ifdef NeedFuncProto
    int iaid,
    int ilvl
#endif
);

extern int c_mpiosa(
#ifdef NeedFuncProto
    int iaid,
    int ilvl
#endif
);

extern int c_mpipai(
#ifdef NeedFuncProto
    int iain,
    int iaip
#endif
);

extern int c_mpipan(
#ifdef NeedFuncProto
    int  iain,
    char *anme
#endif
);

extern int c_mpipar(
#ifdef NeedFuncProto
    int iain
#endif
);

extern int c_mpisci(
#ifdef NeedFuncProto
    int iain
#endif
);

extern void c_mplnam(
#ifdef NeedFuncProto
    char *flnm,
    int   ilvl,
    int  *iama
#endif
);

extern void c_mplndm(
#ifdef NeedFuncProto
    char  *flnm,
    int    ilvl,
    int   *iama,
    float *xcra,
    float *ycra,
    int    mcra,
    int   *iaai,
    int   *iagi,
    int    mnog,
    int (*ulpr_)(
        float *xcra,
        float *ycra,
        int   *ncra,
        int   *iaai,
        int   *iagi,
        int   *ngpsi)
#endif
);

extern void c_mplndr(
#ifdef NeedFuncProto
    char *flnm,
    int   ilvl
#endif
);

extern void c_mplnri(
#ifdef NeedFuncProto
    char *flnm
#endif
);

extern char *c_mpname(
#ifdef NeedFuncProto
    int iain
#endif
);

extern void c_mprset(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_mpsetc(
#ifdef NeedFuncProto
    char *whch,
    char *cval
#endif
);

extern void c_mpsetd(
#ifdef NeedFuncProto
    char *whch,
    double dval
#endif
);

extern void c_mpseti(
#ifdef NeedFuncProto
    char *whch,
    int ival
#endif
);

extern void c_mpsetl(
#ifdef NeedFuncProto
    char *whch,
    int lval
#endif
);

extern void c_mpsetr(
#ifdef NeedFuncProto
    char *whch,
    float rval
#endif
);

extern void c_mputfd(
#ifdef NeedFuncProto
    double rlat,
    double rlon,
    double *u,
    double *v
#endif
);

extern void c_mputfs(
#ifdef NeedFuncProto
    float rlat,
    float rlon,
    float *u,
    float *v
#endif
);

extern void c_mputid(
#ifdef NeedFuncProto
    double uval,
    double vval,
    double *rlat,
    double *rlon
#endif
);

extern void c_mputin(
#ifdef NeedFuncProto
    int iprj,
    int izon,
    int isph,
    double *para,
    double umin,
    double umax,
    double vmin,
    double vmax
#endif
);

extern void c_mputis(
#ifdef NeedFuncProto
    float uval,
    float vval,
    float *rlat,
    float *rlon
#endif
);

extern void c_supcon(
#ifdef NeedFuncProto
    float rlat,
    float rlon,
    float *uval,
    float *vval
#endif
);

extern void c_supmap(
#ifdef NeedFuncProto
    int jprj,
    float plat,
    float plon,
    float rota,
    float *plm1,
    float *plm2,
    float *plm3,
    float *plm4,
    int jlts,
    int jgrd,
    int iout,
    int idot,
    int *ierr
#endif
);

extern void c_gflas1(
#ifdef  NeedFuncProto
    int iname
#endif
);

extern void c_gflas2(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_gflas3(
#ifdef  NeedFuncProto
    int iname
#endif
);

extern void c_gflas4(
#ifdef  NeedFuncProto
    int id,
    char *fname
#endif
);

extern void c_gacolr(
#ifdef  NeedFuncProto
    int kaxs,
    int klbl,
    int kmjt,
    int kmnt
#endif
);

extern void c_gagetc(
#ifdef  NeedFuncProto
    char *pnam,
    char *cval,
    int len
#endif
);

extern void c_gageti(
#ifdef  NeedFuncProto
    char *pnam,
    int *ival
#endif
);

extern void c_gagetr(
#ifdef  NeedFuncProto
    char *pnam,
    float *rval
#endif
);

extern void c_gasetc(
#ifdef  NeedFuncProto
    char *pnam,
    char *cval
#endif
);

extern void c_gaseti(
#ifdef  NeedFuncProto
    char *pnam,
    int ival
#endif
);

extern void c_gasetr(
#ifdef  NeedFuncProto
    char *pnam,
    float rval
#endif
);

extern void c_grid(
#ifdef  NeedFuncProto
    int mjrx,
    int mnrx,
    int mjry,
    int mnry
#endif
);

extern void c_gridal(
#ifdef  NeedFuncProto
    int mjrx,
    int mnrx,
    int mjry,
    int mnry,
    int ixlb,
    int iylb,
    int igph,
    float xint,
    float yint
#endif
);

extern void c_gridl(
#ifdef  NeedFuncProto
    int mjrx,
    int mnrx,
    int mjry,
    int mnry
#endif
);

extern void c_halfax(
#ifdef  NeedFuncProto
    int mjrx,
    int mnrx,
    int mjry,
    int mnry,
    float xint,
    float yint,
    int ixlb,
    int iylb
#endif
);

extern void c_labmod(
#ifdef  NeedFuncProto
    char *fmtx,
    char *fmty,
    int numx,
    int numy,
    int iszx,
    int iszy,
    int ixdc,
    int iydc,
    int ixor
#endif
);

extern void c_perim(
#ifdef  NeedFuncProto
    int mjrx,
    int mnrx,
    int mjry,
    int mnry
#endif
);

extern void c_periml(
#ifdef  NeedFuncProto
    int mjrx,
    int mnrx,
    int mjry,
    int mnry
#endif
);

extern void c_tick4(
#ifdef  NeedFuncProto
    int lmjx,
    int lmnx,
    int lmjy,
    int lmny
#endif
);

extern void c_ticks(
#ifdef  NeedFuncProto
    int lmjr,
    int lmnr
#endif
);

extern void c_histgr(
#ifdef  NeedFuncProto
    float *dat1,
    int ndim,
    int npts,
    int iflag,
    float *class_values,
    int nclass,
    float *wrk,
    int nwrk
#endif
);

extern void c_hstopc(
#ifdef  NeedFuncProto
    char *iopt,
    char *string,
    int number,
    int ilch
#endif
);

extern void c_hstopi(
#ifdef  NeedFuncProto
    char *string,
    int param1,
    int param2,
    int *icol,
    int lcol
#endif
);

extern void c_hstopl(
#ifdef  NeedFuncProto
    char *iopt
#endif
);

extern void c_hstopr(
#ifdef  NeedFuncProto
    char *iopt,
    float *array,
    int isize
#endif
);

extern void c_ezisos(
#ifdef  NeedFuncProto
    float *t,
    int mu,
    int mv,
    int mw,
    float eye[3],
    float *slab,
    float tiso
#endif
);

extern void c_isgeti(
#ifdef  NeedFuncProto
    char *ipn,
    int *ivl
#endif
);

extern void c_isgetr(
#ifdef  NeedFuncProto
    char *ipn,
    float *rvl
#endif
);

extern void c_isosrf(
#ifdef  NeedFuncProto
    float *t,
    int lu,
    int mu,
    int lv,
    int mv,
    int mw,
    float eye[3],
    int muvwp2,
    float *slab,
    float tiso,
    int iflag
#endif
);

extern void c_ispltf(
#ifdef  NeedFuncProto
    float rxn,
    float ryn,
    int ient
#endif
);

extern void c_isseti(
#ifdef  NeedFuncProto
    char *ipn,
    int ivl
#endif
);

extern void c_issetr(
#ifdef  NeedFuncProto
    char *ipn,
    float rvl
#endif
);

extern void c_istr32(
#ifdef  NeedFuncProto
    float ut,
    float vt,
    float wt,
    float xt,
    float yt,
    float zt,
    int ient
#endif
);

extern void c_pwrzi(
#ifdef  NeedFuncProto
    float x,
    float y,
    float z,
    char *id,
    int n,
    int isize,
    int lin3,
    int itop,
    int icnt
#endif
);

extern void c_trn32i(
#ifdef  NeedFuncProto
    float ut,
    float vt,
    float wt,
    float xt,
    float yt,
    float zt,
    int ient
#endif
);

extern void c_lbgeti(
#ifdef  NeedFuncProto
    char *whch,
    int *ival
#endif
);

extern void c_lbgetr(
#ifdef  NeedFuncProto
    char *whch,
    float *rval
#endif
);

extern void c_lblbar(
#ifdef  NeedFuncProto
    int ihov,
    float xleb,
    float xreb,
    float ybeb,
    float yteb,
    int nbox,
    float wsfb,
    float hsfb,
    int *lfin,
    int iftp,
    char *llbs[],
    int nlbs,
    int lbab
#endif
);

extern void c_lbseti(
#ifdef  NeedFuncProto
    char *whch,
    int ival
#endif
);

extern void c_lbsetr(
#ifdef  NeedFuncProto
    char *whch,
    float rval
#endif
);

extern void c_ngdots(
#ifdef  NeedFuncProto
    float *x,
    float *y,
    int num,
    float size,
    int icolor
#endif
);

extern void c_nglogo(
#ifdef NeedFuncProto
    int iwk,
    float x,
    float y,
    float size,
    int itype,
    int icol1,
    int icol2
#endif
);

extern void c_ngezlogo();

extern void c_nggcog(
#ifdef  NeedFuncProto
    float clat,
    float clon,
    float crad,
    float *alat,
    float *alon,
    int npts
#endif
);

extern void c_nggsog(
#ifdef  NeedFuncProto
    float slat,
    float slon,
    float srad,
    float *alat,
    float *alon
#endif
);

extern void c_ngritd(
#ifdef  NeedFuncProto
    int iaxs,
    float angl,
    float *ucrd,
    float *vcrd,
    float *wcrd
#endif
);


extern void c_ngsetr(
#ifdef  NeedFuncProto
    char *pnam,
    float rval
#endif
);

extern void c_ngsetc(
#ifdef  NeedFuncProto
    char *cnp,
    char *cvp
#endif
);

extern void c_nggetc(
#ifdef  NeedFuncProto
    char *cnp,
    char *cvp,
    int len
#endif
);

extern void c_nggetr(
#ifdef  NeedFuncProto
    char *pnam,
    float *rval
#endif
);

extern void c_ngseti(
#ifdef  NeedFuncProto
    char *pnam,
    int ival
#endif
);

extern void c_nggeti(
#ifdef  NeedFuncProto
    char *pnam,
    int *ival
#endif
);

extern int c_ngckop(
#ifdef  NeedFuncProto
    int wkid
#endif
);

extern void c_ngmftc(
#ifdef NeedFuncProto
    int wkid
#endif
);

extern void c_ngreop(
#ifdef NeedFuncProto
    int wkid,
    int conid,
    int itype,
    char *fname,
    int iopt,
    int *iat,
    float *rat,
    int ncolrs,
    int nstart,
    Gcolr_rep *ctab
#endif
);

extern void c_ngsrat(
#ifdef NeedFuncProto
    int iopt,
    int *iat,
    float *rat
#endif
);

extern void c_ngwsym(
#ifdef  NeedFuncProto
    char *ftype,
    int num,
    float x,
    float y,
    float size,
    int icolor,
    int ialt
#endif
);

extern void c_ngpict(
#ifdef  NeedFuncProto
    int wkid,
    int action
#endif
);

extern int c_ngpswk(
#ifdef  NeedFuncProto
    char *pstype,
    char *orient,
    char *color
#endif
);

extern void c_pcdlsc(
#ifdef  NeedFuncProto
    int ifci
#endif
);

extern void c_pcgetc(
#ifdef  NeedFuncProto
    char *whch,
    char *cval,
    int len
#endif
);

extern void c_pcgeti(
#ifdef  NeedFuncProto
    char *whch,
    int *ival
#endif
);

extern void c_pcgetr(
#ifdef  NeedFuncProto
    char *whch,
    float *rval
#endif
);

extern void c_pcsetc(
#ifdef  NeedFuncProto
    char *whch,
    char *cval
#endif
);

extern void c_pcseti(
#ifdef  NeedFuncProto
    char *whch,
    int ival
#endif
);

extern void c_pcsetr(
#ifdef  NeedFuncProto
    char *whch,
    float rval
#endif
);

extern void c_pchiqu(
#ifdef  NeedFuncProto
    float xpos,
    float ypos,
    char *chrs,
    float size,
    float angd,
    float cntr
#endif
);

extern void c_plchhq(
#ifdef  NeedFuncProto
    float xpos,
    float ypos,
    char *chrs,
    float size,
    float angd,
    float cntr
#endif
);

extern void c_pcloqu(
#ifdef  NeedFuncProto
    float xpos,
    float ypos,
    char *chrs,
    float size,
    float angd,
    float cntr
#endif
);

extern void c_plchlq(
#ifdef  NeedFuncProto
    float xpos,
    float ypos,
    char *chrs,
    float size,
    float angd,
    float cntr
#endif
);

extern void c_pcmequ(
#ifdef  NeedFuncProto
    float xpos,
    float ypos,
    char *chrs,
    float size,
    float angd,
    float cntr
#endif
);

extern void c_plchmq(
#ifdef  NeedFuncProto
    float xpos,
    float ypos,
    char *chrs,
    float size,
    float angd,
    float cntr
#endif
);

extern void c_pcmpxy(
#ifdef  NeedFuncProto
    int imap,
    float xinp,
    float yinp,
    float *xotp,
    float *yotp
#endif
);

extern char *c_pcpnwi(
#ifdef  NeedFuncProto
    char *whch,
    int ipai
#endif
);

extern void c_ppditr(
#ifdef  NeedFuncProto
        float *xccp,
        float *yccp,
        int nccp,
        float *xcsp,
        float *ycsp,
        int ncsp,
        float *rwrk,
        int *iwrk,
        int nwrk,
        int (*urpt_)(
            float *xcbl,
            float *xcbr,
            float *ycob,
            float *dxle,
            float *dxre,
            float *ycot),
        int *ierr
#endif
);

extern void c_ppintr(
#ifdef  NeedFuncProto
        float *xccp,
        float *yccp,
        int nccp,
        float *xcsp,
        float *ycsp,
        int ncsp,
        float *rwrk,
        int *iwrk,
        int nwrk,
        int (*urpt_)(
            float *xcbl,
            float *xcbr,
            float *ycob,
            float *dxle,
            float *dxre,
            float *ycot),
        int *ierr
#endif
);

extern void c_ppplcl(
#ifdef  NeedFuncProto
        float xmin,
        float xmax,
        float ymin,
        float ymax,
        float *xcpl,
        float *ycpl,
        int ncpl,
        float *rwrk,
        int lwrk,
        int (*urpf_)(
            float *xcra,
            float *ycra,
            int *ncra),
        int *ierr
#endif
);

extern void c_ppuntr(
#ifdef  NeedFuncProto
        float *xccp,
        float *yccp,
        int nccp,
        float *xcsp,
        float *ycsp,
        int ncsp,
        float *rwrk,
        int *iwrk,
        int nwrk,
        int (*urpt_)(
            float *xcbl,
            float *xcbr,
            float *ycob,
            float *dxle,
            float *dxre,
            float *ycot),
        int *ierr
#endif
);

extern void c_ppdipo(
#ifdef  NeedFuncProto
        float *xccp,
        float *yccp,
        int nccp,
        float *xcsp,
        float *ycsp,
        int ncsp,
        float *rwrk,
        int *iwrk,
        int nwrk,
        int (*urpp_)(
            float *xcra,
            float *ycra,
            int *ncra),
        int *ierr
#endif
);

extern void c_ppinpo(
#ifdef  NeedFuncProto
        float *xccp,
        float *yccp,
        int nccp,
        float *xcsp,
        float *ycsp,
        int ncsp,
        float *rwrk,
        int *iwrk,
        int nwrk,
        int (*urpp_)(
            float *xcra,
            float *ycra,
            int *ncra),
        int *ierr
#endif
);

extern void c_ppppap(
#ifdef  NeedFuncProto
        float *xcop,
        float *ycop,
        int ncop,
        int nbts
#endif
);

extern void c_ppunpo(
#ifdef  NeedFuncProto
        float *xccp,
        float *yccp,
        int nccp,
        float *xcsp,
        float *ycsp,
        int ncsp,
        float *rwrk,
        int *iwrk,
        int nwrk,
        int (*urpp_)(
            float *xcra,
            float *ycra,
            int *ncra),
        int *ierr
#endif
);

extern void c_sfgetc(
#ifdef  NeedFuncProto
    char *cnp,
    char *cvp,
    int len
#endif
);

extern void c_sfgeti(
#ifdef  NeedFuncProto
    char *cnp,
    int *ivp
#endif
);

extern void c_sfgetp(
#ifdef  NeedFuncProto
    int idp[8][8]
#endif
);

extern void c_sfgetr(
#ifdef  NeedFuncProto
    char *cnp,
    float *rvp
#endif
);

extern void c_sfnorm(
#ifdef  NeedFuncProto
    float *xra,
    float *yra,
    int nra,
    float *dst,
    int nst,
    int *ind,
    int nnd
#endif
);

extern void c_sfsetc(
#ifdef  NeedFuncProto
    char *cnp,
    char *cvp
#endif
);

extern void c_sfseti(
#ifdef  NeedFuncProto
    char *cnp,
    int ivp
#endif
);

extern void c_sfsetp(
#ifdef  NeedFuncProto
    int idp[8][8]
#endif
);

extern void c_sfsetr(
#ifdef  NeedFuncProto
    char *cnp,
    float rvp
#endif
);

extern void c_sfsgfa(
#ifdef  NeedFuncProto
    float *xra,
    float *yra,
    int nra,
    float *dst,
    int nst,
    int *ind,
    int nnd,
    int ici
#endif
);

extern void c_sfwrld(
#ifdef  NeedFuncProto
    float *xra,
    float *yra,
    int nra,
    float *dst,
    int nst,
    int *ind,
    int nnd
#endif
);

extern float c_cfux(
#ifdef  NeedFuncProto
    float rx
#endif
);

extern float c_cfuy(
#ifdef  NeedFuncProto
    float ry
#endif
);

extern void c_clsgks(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_wmbarb(
#ifdef NeedFuncProto
    float x,
    float y,
    float u,
    float v
#endif
);

extern void c_wmvect(
#ifdef NeedFuncProto
    float x,
    float y,
    float u,
    float v
#endif
);

extern void c_wmvectmap(
#ifdef NeedFuncProto
    float x,
    float y,
    float u,
    float v
#endif
);

extern void c_wmvlbl(
#ifdef NeedFuncProto
    float x,
    float y
#endif
);

extern void c_wmw2nx(
#ifdef NeedFuncProto
    int n,
    float *p,
    float *q
#endif
);

extern void c_wmw2ny(
#ifdef NeedFuncProto
    int n,
    float *p,
    float *q
#endif
);

extern void c_wmdflt(
#ifdef NeedFuncProto
    void
#endif
);

extern void c_wmdrft(
#ifdef NeedFuncProto
    int n,
    float *x,
    float *y
#endif
);

extern void c_wmdrrg(
#ifdef NeedFuncProto
    int n,
    float *x,
    float *y,
    char *itype,
    int nc,
    float *xc,
    float *yc
#endif
);

extern void c_wmgetc(
#ifdef NeedFuncProto
    char *cnp,
    char *cvp,
    int len
#endif
);

extern float c_cmfx(
#ifdef  NeedFuncProto
    int ix
#endif
);

extern float c_cmfy(
#ifdef  NeedFuncProto
    int iy
#endif
);

extern float c_cmux(
#ifdef  NeedFuncProto
    int ix
#endif
);

extern float c_cmuy(
#ifdef  NeedFuncProto
    int iy
#endif
);

extern float c_cpfx(
#ifdef  NeedFuncProto
    int ix
#endif
);

extern float c_cpfy(
#ifdef  NeedFuncProto
    int iy
#endif
);

extern float c_cpux(
#ifdef  NeedFuncProto
    int ix
#endif
);

extern float c_cpuy(
#ifdef  NeedFuncProto
    int iy
#endif
);

extern float c_cufx(
#ifdef  NeedFuncProto
    float rx
#endif
);

extern float c_cufy(
#ifdef  NeedFuncProto
    float ry
#endif
);

extern void c_curve(
#ifdef  NeedFuncProto
    float *px,
    float *py,
    int np
#endif
);

extern void c_frame(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_fl2int(
#ifdef  NeedFuncProto
    float px,
    float py,
    int *ix,
    int *iy
#endif
);

extern void c_frstpt(
#ifdef  NeedFuncProto
    float px,
    float py
#endif
);

extern void c_getset(
#ifdef  NeedFuncProto
    float *vl,
    float *vr,
    float *vb,
    float *vt,
    float *wl,
    float *wr,
    float *wb,
    float *wt,
    int *lf
#endif
);

extern void c_getsi(
#ifdef  NeedFuncProto
    int *ix,
    int *iy
#endif
);

extern void c_getusv(
#ifdef  NeedFuncProto
    char *vn,
    int *iv
#endif
);

extern int c_kfmx(
#ifdef  NeedFuncProto
    float rx
#endif
);

extern int c_kfmy(
#ifdef  NeedFuncProto
    float ry
#endif
);

extern int c_kfpx(
#ifdef  NeedFuncProto
    float rx
#endif
);

extern int c_kfpy(
#ifdef  NeedFuncProto
    float ry
#endif
);

extern int c_kupx(
#ifdef  NeedFuncProto
    float rx
#endif
);

extern int c_kupy(
#ifdef  NeedFuncProto
    float ry
#endif
);

extern int c_kumx(
#ifdef  NeedFuncProto
    float rx
#endif
);

extern int c_kumy(
#ifdef  NeedFuncProto
    float ry
#endif
);

extern int c_kmpx(
#ifdef  NeedFuncProto
    int ix
#endif
);

extern int c_kmpy(
#ifdef  NeedFuncProto
    int iy
#endif
);

extern int c_kpmx(
#ifdef  NeedFuncProto
    int ix
#endif
);

extern int c_kpmy(
#ifdef  NeedFuncProto
    int iy
#endif
);

extern float NGCALLF(cfux,CFUX)(
#ifdef  NeedFuncProto
    float *rx
#endif
);

extern float NGCALLF(cfuy,CFUY)(
#ifdef  NeedFuncProto
    float *ry
#endif
);

extern float NGCALLF(cmfx,CMFX)(
#ifdef  NeedFuncProto
    int *ix
#endif
);

extern float NGCALLF(cmfy,CMFY)(
#ifdef  NeedFuncProto
    int *iy
#endif
);

extern float NGCALLF(cmux,CMUX)(
#ifdef  NeedFuncProto
    int *ix
#endif
);

extern float NGCALLF(cmuy,CMUY)(
#ifdef  NeedFuncProto
    int *iy
#endif
);

extern float NGCALLF(cpfx,CPFX)(
#ifdef  NeedFuncProto
    int *ix
#endif
);

extern float NGCALLF(cpfy,CPFY)(
#ifdef  NeedFuncProto
    int *iy
#endif
);

extern float NGCALLF(cpux,CPUX)(
#ifdef  NeedFuncProto
    int *ix
#endif
);

extern float NGCALLF(cpuy,CPUY)(
#ifdef  NeedFuncProto
    int *iy
#endif
);

extern float NGCALLF(cufx,CUFX)(
#ifdef  NeedFuncProto
    float *rx
#endif
);

extern float NGCALLF(cufy,CUFY)(
#ifdef  NeedFuncProto
    float *ry
#endif
);

extern int NGCALLF(kfmx,KFMX)(
#ifdef  NeedFuncProto
    float *rx
#endif
);

extern int NGCALLF(kfmy,KFMY)(
#ifdef  NeedFuncProto
    float *ry
#endif
);

extern int NGCALLF(kfpx,KFPX)(
#ifdef  NeedFuncProto
    float *rx
#endif
);

extern int NGCALLF(kfpy,KFPY)(
#ifdef  NeedFuncProto
    float *ry
#endif
);

extern int NGCALLF(kmpx,KMPX)(
#ifdef  NeedFuncProto
    int *ix
#endif
);

extern int NGCALLF(kmpy,KMPY)(
#ifdef  NeedFuncProto
    int *iy
#endif
);

extern int NGCALLF(kpmx,KPMX)(
#ifdef  NeedFuncProto
    int *ix
#endif
);

extern int NGCALLF(kpmy,KPMY)(
#ifdef  NeedFuncProto
    int *iy
#endif
);

extern int NGCALLF(kumx,KUMX)(
#ifdef  NeedFuncProto
    float *rx
#endif
);

extern int NGCALLF(kumy,KUMY)(
#ifdef  NeedFuncProto
    float *ry
#endif
);

extern int NGCALLF(kupx,KUPX)(
#ifdef  NeedFuncProto
    float *rx
#endif
);

extern int NGCALLF(kupy,KUPY)(
#ifdef  NeedFuncProto
    float *ry
#endif
);

extern void c_line(
#ifdef  NeedFuncProto
    float x1,
    float y1,
    float x2,
    float y2
#endif
);

extern void c_opngks(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_plotif(
#ifdef  NeedFuncProto
    float fx,
    float fy,
    int ip
#endif
);

extern void c_plotit(
#ifdef  NeedFuncProto
    int ix,
    int iy,
    int ip
#endif
);

extern void c_point(
#ifdef  NeedFuncProto
    float px,
    float py
#endif
);

extern void c_points(
#ifdef  NeedFuncProto
    float *px,
    float *py,
    int np,
    int ic,
    int il
#endif
);        

extern void c_pwrit(
#ifdef  NeedFuncProto
    float px,
    float py,
    char *ch,
    int nc,
    int is,
    int io,
    int ic
#endif
);

extern void c_set(
#ifdef  NeedFuncProto
    float vl,
    float vr,
    float vb,
    float vt,
    float wl,
    float wr,
    float wb,
    float wt,
    int lf
#endif
);

extern void c_seti(
#ifdef  NeedFuncProto
    int ix,
    int iy
#endif
);

extern void c_setusv(
#ifdef  NeedFuncProto
    char *vn,
    int iv
#endif
);

extern void c_sflush(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_vector(
#ifdef  NeedFuncProto
    float px,
    float py
#endif
);

extern void c_wtstr(
#ifdef  NeedFuncProto
    float px,
    float py,
    char *ch,
    int is,
    int io,
    int ic
#endif
);

extern void c_ezsrfc(
#ifdef  NeedFuncProto
    float *z,
    int m,
    int n,
    float angh,
    float angv,
    float *work
#endif
);

extern void c_pwrzs(
#ifdef  NeedFuncProto
    float x,
    float y,
    float z,
    char *id,
    int n,
    int isize,
    int lin3,
    int itop,
    int icnt
#endif
);

extern void c_srface(
#ifdef  NeedFuncProto
    float *x,
    float *y,
    float *z,
    int *m,
    int mx,
    int nx,
    int ny,
    float s[6],
    float stereo
#endif
);

extern void c_setr(
#ifdef  NeedFuncProto
    float xmin,
    float xmax,
    float ymin,
    float ymax,
    float zmin,
    float zmax,
    float r0
#endif
);

extern void c_trn32s(
#ifdef  NeedFuncProto
    float x,
    float y,
    float z,
    float xt,
    float yt,
    float zt,
    int iflag
#endif
);

extern void c_ftitle(
#ifdef  NeedFuncProto
    int movie
#endif
);

extern void c_slgeti(
#ifdef  NeedFuncProto
    char *pa,
    int *ival
#endif
);

extern void c_slgetr(
#ifdef  NeedFuncProto
    char *pa,
    float *rval
#endif                
);                

extern void c_slogap(
#ifdef NeedFuncProto
    float time,
    int mtst
#endif
);

extern void c_slrset();

extern void c_slseti(
#ifdef  NeedFuncProto
    char *pa,
    int ival
#endif
);                

extern void c_slsetr(
#ifdef  NeedFuncProto
    char *pa,
    float rval
#endif
);

extern void c_stitle(
#ifdef  NeedFuncProto
    char *cards[],
    int ncards,
    int nyst,
    int nyfin,
    float tst,
    float tmv,
    float tfin,
    int mv
#endif
);

extern void c_stgeti(
#ifdef  NeedFuncProto
    char *whch,
    int *ival
#endif
);

extern void c_stgetr(
#ifdef  NeedFuncProto
    char *whch,
    float *rval
#endif
);

extern void c_stinit(
#ifdef  NeedFuncProto
    float *u,
    int lu,
    float *v,
    int lv,
    float *p,
    int lp,
    int m,
    int n,
    float *wrk,
    int lw
#endif
);

extern void c_stream(
#ifdef  NeedFuncProto
    float *u,
    float *v,
    float *p,
    int *iam,
    int (*stumsl_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai),
    float *wrk
#endif
);

extern void c_stseti(
#ifdef  NeedFuncProto
    char *whch,
    int ival
#endif
);

extern void c_stsetr(
#ifdef  NeedFuncProto
    char *whch,
    float rval
#endif
);

extern void c_ezstrm(
#ifdef  NeedFuncProto
    float *u,
    float *v,
    float *work,
    int imax,
    int jmax
#endif
);

extern void c_strset(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_strmln(
#ifdef  NeedFuncProto
    float *u,
    float *v,
    float *work,
    int imax,
    int iptsx,
    int jptsy,
    int nset,
    int *ier
#endif
);

extern void c_encd(
#ifdef  NeedFuncProto
    float valu,
    float ash,
    char *iout,
    int *nc,
    int ioffd
#endif
);

extern void c_entsr(
#ifdef  NeedFuncProto
    int *irold,
    int irnew
#endif
);

extern void c_eprin(
#ifdef  NeedFuncProto
    void
#endif
);
    
extern void c_errof(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_msbsf1(
#ifdef  NeedFuncProto
    int m,
    int n,
    float xmin,
    float xmax,
    float ymin,
    float ymax,
    float *z,
    int iz,
    float *zp,
    float *temp,
    float sigma
#endif
);

extern void c_msbsf2(
#ifdef  NeedFuncProto
    float dxmin,
    float dxmax,
    int md,
    float dymin,
    float dymax,
    int nd,
    float *dz,
    int idz,
    int m,
    int n,
    float xmin,
    float xmax,
    float ymin,
    float ymax,
    float *z,
    int iz,
    float *zp,
    float *work,
    float sigma
#endif
);

extern void c_msceez(
#ifdef  NeedFuncProto
    float del1,
    float del2,
    float sigma,
    float *c1,
    float *c2,
    float *c3,
    int n
#endif
);

extern void c_mskrv1(
#ifdef  NeedFuncProto
    int n,
    float *x,
    float *y,
    float slp1,
    float slpn,
    float *xp,
    float *yp,
    float *temp,
    float *s,
    float sigma,
    int islpsw
#endif
);

extern void c_mskrv2(
#ifdef  NeedFuncProto
    float t,
    float *xs,
    float *ys,
    int n,
    float *x,
    float *y,
    float *xp,
    float *yp,
    float *s,
    float sigma,
    int ics,
    float *slp
#endif
);

extern int c_msntvl(
#ifdef  NeedFuncProto
    int n,
    float t,
    float *x
#endif
);

extern void c_msshch(
#ifdef  NeedFuncProto
    float *sinhm,
    float *coshm,
    float x,
    int isw
#endif
);

extern void c_mssrf1(
#ifdef  NeedFuncProto
    int m,
    int n,
    float *x,
    float *y,
    float *z,
    int iz,
    float *zx1,
    float *zxm,
    float *zy1,
    float *zyn,
    float zxy11,
    float zxym1,
    float zxy1n,
    float zxymn,
    int islpsw,
    float *zp,
    float *temp,
    float sigma,
    int *ierr
#endif
);

extern float c_mssrf2(
#ifdef  NeedFuncProto
    float xx,
    float yy,
    int m,
    int n,
    float *x,
    float *y,
    float *z,
    int iz,
    float *zp,
    float sigma
#endif
);

extern void c_mstrms(
#ifdef  NeedFuncProto
    float *diag,
    float *sdiag,
    float sigma,
    float del
#endif
);

extern int c_nerro(
#ifdef  NeedFuncProto
    int *nerr
#endif
);

extern char *c_semess(
#ifdef  NeedFuncProto
    int itrim
#endif
);

extern int c_icfell(
#ifdef  NeedFuncProto
    char *messg,
    int nerrf
#endif
);

extern void c_q8qst4(
#ifdef  NeedFuncProto
    char *name,
    char *lbrary,
    char *entry,
    char *vrsion
#endif
);

extern void c_reset();

extern void c_retsr(
#ifdef  NeedFuncProto
    int irold
#endif
);

extern void c_seter(
#ifdef  NeedFuncProto
    char *messg,
    int nerr,
    int iopt
#endif
);

extern int c_icloem(
#ifdef  NeedFuncProto
    char *messg
#endif
);

extern void c_curve3(
#ifdef  NeedFuncProto
    float *u,
    float *v,
    float *w,
    int n
#endif
);

extern void c_fence3(
#ifdef  NeedFuncProto
    float *u,
    float *v,
    float *w,
    int n,
    int ior,
    float bot
#endif
);

extern void c_frst3(
#ifdef  NeedFuncProto
    float u,
    float v,
    float w
#endif
);

extern void c_line3(
#ifdef  NeedFuncProto
    float ua,
    float va,
    float wa,
    float ub,
    float vb,
    float wb
#endif
);

extern void c_perim3(
#ifdef  NeedFuncProto
    int magr1,
    int mini1,
    int magr2,
    int mini2,
    int iwhich,
    float var
#endif
);

extern void c_point3(
#ifdef  NeedFuncProto
    float u,
    float v,
    float w
#endif
);

extern void c_pwrzt(
#ifdef  NeedFuncProto
    float x,
    float y,
    float z,
    char *id,
    int n,
    int isize,
    int lin3,
    int itop,
    int icnt
#endif
);

extern void c_set3(
#ifdef  NeedFuncProto
    float xa,
    float xb,
    float ya,
    float yb,
    float ulo,
    float uhi,
    float vlo,
    float vhi,
    float wlo,
    float whi,
    float eye[3]
#endif
);

extern void c_tdclrs(
#ifdef NeedFuncProto
    int   iwid,
    int   ibow,
    float shde,
    float shdr,
    int   iofc,
    int   iolc,
    int   ilmt
#endif
);

extern void c_tdctri(
#ifdef NeedFuncProto
    float *rtri,
    int    mtri,
    int    *ntri,
    int    iaxs,
    float  rcut
#endif
);

extern void c_tdcudp(
#ifdef NeedFuncProto
    float *ucrv,
    float *vcrv,
    float *wcrv,
    int    ncrv,
    int    iarh,
    float  arhl,
    float  arhw
#endif
);

extern void c_tdcurv(
#ifdef NeedFuncProto
    float *ucrv,
    float *vcrv,
    float *wcrv,
    int    ncrv,
    int    iarh,
    float  arhl,
    float  arhw
#endif
);

extern void c_tddtri(
#ifdef NeedFuncProto
    float *rtri,
    int    mtri,
    int   *ntri,
    int   *itwk
#endif
);

extern void c_tdez2d(
#ifdef NeedFuncProto
    int nx,
    int ny,
    float *x,
    float *y,
    float *z,
    float rmult,
    float theta,
    float phi,
    int ist
#endif
);

extern void c_tdez3d(
#ifdef NeedFuncProto
    int nx,
    int ny,
    int nz,
    float *x,
    float *y,
    float *z,
    float *u,
    float value,
    float rmult,
    float theta,
    float phi,
    int ist
#endif
);

extern void c_tdgeti(
#ifdef NeedFuncProto
    char *pnam,
    int *ival
#endif
);

extern void c_tdgetr(
#ifdef NeedFuncProto
    char *pnam,
    float *rval
#endif
);

extern void c_tdgrds(
#ifdef NeedFuncProto
    float umin,
    float vmin,
    float wmin,
    float umax,
    float vmax,
    float wmax,
    float ustp,
    float vstp,
    float wstp,
    int   igrt,
    int   ihid
#endif
);

extern void c_tdgrid(
#ifdef NeedFuncProto
    float xbeg,
    float xstp,
    int   noxs,
    float ybeg,
    float ystp,
    int   noys,
    int   igrd
#endif
);

extern void c_tdgtrs(
#ifdef NeedFuncProto
    int    irst,
    int   *ifc1,
    int   *ifc2,
    int   *ifc3,
    int   *ifc4,
    int   *ilc1,
    int   *ilc2,
    int   *iltd,
    float *ustp,
    float *vstp,
    float *wstp
#endif
);

extern void c_tdinit(
#ifdef NeedFuncProto
    float umid,
    float vmid,
    float wmid,
    float uori,
    float vori,
    float wori,
    float uthi,
    float vthi,
    float wthi,
    float otep
#endif
);

extern void c_tditri(
#ifdef NeedFuncProto
    float *u,
    int    nu,
    float *v,
    int    nv,
    float *w,
    int    nw,
    float *f,
    int    lf1d,
    int    lf2d,
    float  fiso,
    float *rtri,
    int    mtri,
    int   *ntri,
    int    irst
#endif
);

extern void c_tdlbla(
#ifdef NeedFuncProto
    int   iaxs,
    char* ilbl,
    char* nlbl,
    float xat0,
    float xat1,
    float yat0,
    float yat1,
    float angd
#endif
);

extern void c_tdlbls(
#ifdef NeedFuncProto
    float umin,
    float vmin,
    float wmin,
    float umax,
    float vmax,
    float wmax,
    char* unlb,
    char* vnlb,
    char* wnlb,
    char* uilb,
    char* vilb,
    char* wilb,
    int   ipck
#endif
);

extern void c_tdline(
#ifdef NeedFuncProto
    float ucp1,
    float vcp1,
    float wcp1,
    float ucp2,
    float vcp2,
    float wcp2
#endif
);

extern void c_tdlndp(
#ifdef NeedFuncProto
    float ucp1,
    float vcp1,
    float wcp1,
    float ucp2,
    float vcp2,
    float wcp2
#endif
);

extern void c_tdlnpa(
#ifdef NeedFuncProto
    float xcp1,
    float ycp1,
    float xcp2,
    float ycp2
#endif
);

extern void c_tdlpdp(
#ifdef NeedFuncProto
    float xcp1,
    float ycp1,
    float xcp2,
    float ycp2
#endif
);

extern void c_tdmtri(
#ifdef NeedFuncProto
    int    imrk,
    float  umrk,
    float  vmrk,
    float  wmrk,
    float  smrk,
    float *rtri,
    int    mtri,
    int   *ntri,
    int    irst,
    float  umin,
    float  vmin,
    float  wmin,
    float  umax,
    float  vmax,
    float  wmax
#endif
);

extern void c_tdotri(
#ifdef NeedFuncProto
    float *rtri,
    int    mtri,
    int   *ntri,
    float *rtwk,
    int   *itwk,
    int    iord
#endif
);

extern void c_tdpara(
#ifdef NeedFuncProto
    float ua00,
    float va00,
    float wa00,
    float uv10,
    float vv10,
    float wv10,
    float uv01,
    float vv01,
    float wv01
#endif
);

extern void c_tdplch(
#ifdef NeedFuncProto
    float xpos,
    float ypos,
    char *chrs,
    float size,
    float angd,
    float cntr
#endif
);

extern void c_tdprpa(
#ifdef NeedFuncProto
    float  xipa,
    float  yipa,
    float *xi2d,
    float *yi2d
#endif
);

extern void c_tdprpi(
#ifdef NeedFuncProto
    float  xi2d,
    float  yi2d,
    float *xipa,
    float *yipa
#endif
);

extern void c_tdprpt(
#ifdef NeedFuncProto
    float  ui3d,
    float  vi3d,
    float  wi3d,
    float *xi2d,
    float *yi2d
#endif
);

extern void c_tdseti(
#ifdef NeedFuncProto
    char *pnam,
    int ival
#endif
);

extern void c_tdsetr(
#ifdef NeedFuncProto
    char *pnam,
    float rval
#endif
);

extern void c_tdsort(
#ifdef NeedFuncProto
    float *rwrk,
    int    nwrk,
    int    iord,
    int   *iwrk
#endif
);

extern void c_tdstri(
#ifdef NeedFuncProto
    float *u,
    int    nu,
    float *v,
    int    nv,
    float *w,
    int    lw1d,
    float *rtri,
    int    mtri,
    int   *ntri,
    int    irst
#endif
);

extern void c_tdstrs(
#ifdef NeedFuncProto
    int   irst,
    int   ifc1,
    int   ifc2,
    int   ifc3,
    int   ifc4,
    int   ilc1,
    int   ilc2,
    int   iltd,
    float ustp,
    float vstp,
    float wstp
#endif
);

extern void c_tick4(
#ifdef NeedFuncProto
    int lmjx,
    int lmnx,
    int lmjy,
    int lmny
#endif
);

extern void c_tick43(
#ifdef  NeedFuncProto
    int magu,
    int minu,
    int magv,
    int minv,
    int magw,
    int minw
#endif
);

extern void c_vect3(
#ifdef  NeedFuncProto
    float u,
    float v,
    float w
#endif
);

extern void c_ezvec(
#ifdef  NeedFuncProto
    float *u,
    float *v,
    int m,
    int n
#endif
);

extern void c_vvinit(
#ifdef  NeedFuncProto
    float *u,
    int lu,
    float *v,
    int lv,
    float *p,
    int lp,
    int m,
    int n,
    float *wrk,
    int lw
#endif
);

extern void c_velvec(
#ifdef  NeedFuncProto
    float *u,
    int lu,
    float *v,
    int lv,
    int m,
    int n,
    float flo,
    float hi,
    int nset,
    int ispv,
    float *spv
#endif
);

extern void c_vvectr(
#ifdef  NeedFuncProto
    float *u,
    float *v,
    float *p,
    int *iam,
    int (*vvudmv_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai),
    float *wrk
#endif
);

extern void c_vvgetc(
#ifdef  NeedFuncProto
    char *whch,
    char *cval,
    int len
#endif
);

extern void c_vvgeti(
#ifdef  NeedFuncProto
    char *whch,
    int *ival
#endif
);

extern void c_vvgetr(
#ifdef  NeedFuncProto
    char *whch,
    float *rval
#endif
);

extern void c_vvrset(
#ifdef  NeedFuncProto
    void
#endif
);

extern void c_vvsetc(
#ifdef  NeedFuncProto
    char *whch,
    char *cval
#endif
);

extern void c_vvseti(
#ifdef  NeedFuncProto
    char *whch,
    int ival
#endif
);

extern void c_vvsetr(
#ifdef  NeedFuncProto
    char *whch,
    float rval
#endif
);

extern void c_velvct(
#ifdef  NeedFuncProto
    float *u,
    int lu,
    float *v,
    int lv,
    int m,
    int n,
    float flo,
    float hi,
    int nset,
    int length,
    int ispv,
    float *spv
#endif
);
    
extern void c_wmgeti(
#ifdef NeedFuncProto
    char *cnp,
    int *ivp
#endif
);

extern void c_wmgetr(
#ifdef NeedFuncProto
    char *cnp,
    float *rvp
#endif
);

extern int c_wmgtln(
#ifdef NeedFuncProto
    char *lab,
    int lablen,    
    int ilr
#endif
);

extern void c_wmlabc(
#ifdef NeedFuncProto
    float x,
    float y,
    char *city,
    char *temps
#endif
);

extern void c_wmlabs(
#ifdef NeedFuncProto
    float x,
    float y,
    char *symtyp
#endif
);

extern void c_wmlabw(
#ifdef NeedFuncProto
    float x,
    float y,
    char *label
#endif
);


extern void c_wmlabt(
#ifdef NeedFuncProto
    float x,
    float y,
    char *label,
    int iflg
#endif
);

extern void c_wmlgnd(
#ifdef NeedFuncProto
    float x,
    float y,
    int ntype,
    int irows,
    int icols
#endif
);

extern void c_wmrgwt(
#ifdef NeedFuncProto
    int n,
    float *x,
    float *y,
    int ifnt,
    int nasc
#endif
);

extern void c_wmsetc(
#ifdef NeedFuncProto
    char *cnp,
    char *cvp
#endif
);

extern void c_wmseti(
#ifdef NeedFuncProto
    char *cnp,
    int ivp
#endif
);

extern void c_wmsetr(
#ifdef NeedFuncProto
    char *cnp,
    float rvp
#endif
);

extern void c_wmstnm(
#ifdef NeedFuncProto
    float x,
    float y,
    char *imdat
#endif
);

/*  Common routines  */
    
extern void Pad_char_array(
#ifdef  NeedFuncProto
    char *str1[],
    char *str2,
    int n,
    int maxlen
#endif
);

extern int chk_ret_str(
#ifdef  NeedFuncProto
    char *str,
    int len,
    char *error_msg
#endif
);

#if defined(cray)
extern _fcd conv_cray_str(
#ifdef  NeedFuncProto
    char *str
#endif
);
#endif

NCARG_PROTO_END

#endif  /* _ncargC_h */ 
