#include <stdio.h>
#include <stdlib.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/ncl/defs.h>
#include "Symbol.h"
#include "NclDataDefs.h"
#include "Machine.h"
#include "NclFile.h"
#include "NclVar.h"
#include "NclCoordVar.h"
#include "VarSupport.h"
#include "DataSupport.h"
#include "NclMdInc.h"
#include "NclHLUObj.h"
#include "parser.h"
#include "OpsList.h"
#include "ApiRecords.h"
#include "TypeSupport.h"
#include "NclBuiltInSupport.h"
#include "FileSupport.h"
#include "NclAtt.h"
#include "NclList.h"
#include "ListSupport.h"
#include "NclFileInterfaces.h"
#include <ncarg/ncl/NclBuiltIns.h>

/*
 * The following are the required NCAR Graphics include files.
 * They should be located in ${NCARG_ROOT}/include
 */
#include <ncarg/ngmath.h>

/*
 * Declare wrapper function
 */
extern NhlErrorTypes vinth2p_W(void);
extern NhlErrorTypes vinth2p_ecmwf_W(void);

extern NhlErrorTypes g2gsh_W(void);
extern NhlErrorTypes f2gsh_W(void);
extern NhlErrorTypes g2fsh_W(void);
extern NhlErrorTypes f2fsh_W(void);
extern NhlErrorTypes fo2fsh_W(void);
extern NhlErrorTypes f2fosh_W(void);
extern NhlErrorTypes fo2fshv_W(void);
extern NhlErrorTypes f2foshv_W(void);
extern NhlErrorTypes g2gshv_W(void);
extern NhlErrorTypes f2gshv_W(void);
extern NhlErrorTypes g2fshv_W(void);
extern NhlErrorTypes f2fshv_W(void);
extern NhlErrorTypes eofcov_W(void);
extern NhlErrorTypes eofcor_W(void);
extern NhlErrorTypes eofcov_pcmsg_W(void);
extern NhlErrorTypes eofcor_pcmsg_W(void);
extern NhlErrorTypes eofcov_ts_W(void);
extern NhlErrorTypes eofcor_ts_W(void);
extern NhlErrorTypes eofcov_ts_pcmsg_W(void);
extern NhlErrorTypes eofcor_ts_pcmsg_W(void);
extern NhlErrorTypes eof_varimax_W(void);
extern NhlErrorTypes center_finite_diff_W(void);
extern NhlErrorTypes uv2vr_cfd_W(void);
extern NhlErrorTypes uv2dv_cfd_W(void);
extern NhlErrorTypes svdcov_W(void);
extern NhlErrorTypes svdstd_W(void);
extern NhlErrorTypes svdcov_sv_W(void);
extern NhlErrorTypes svdstd_sv_W(void);
extern NhlErrorTypes svdpar_W(void);
extern NhlErrorTypes sindex_yrmo_W(void);
extern NhlErrorTypes snindex_yrmo_W(void);
extern NhlErrorTypes x_skewt_W(void);
extern NhlErrorTypes y_skewt_W(void);
extern NhlErrorTypes tmr_skewt_W(void);
extern NhlErrorTypes tda_skewt_W(void);
extern NhlErrorTypes satlft_skewt_W(void);
extern NhlErrorTypes ptlcl_skewt_W(void);
extern NhlErrorTypes showal_skewt_W(void);
extern NhlErrorTypes pw_skewt_W(void);
extern NhlErrorTypes cape_thermo_W(void);
extern NhlErrorTypes gaus_lobat_W(void);
extern NhlErrorTypes gaus_lobat_wgt_W(void);
extern NhlErrorTypes linrood_latwgt_W(void);
extern NhlErrorTypes linrood_wgt_W(void);

extern NhlErrorTypes dv2uvf_W(void);
extern NhlErrorTypes dv2uvg_W(void);
extern NhlErrorTypes gradsf_W(void);
extern NhlErrorTypes gradsg_W(void);
extern NhlErrorTypes igradsf_W(void);
extern NhlErrorTypes igradsg_W(void);
extern NhlErrorTypes igradsF_W(void);
extern NhlErrorTypes igradsG_W(void);
extern NhlErrorTypes ilapsf_W(void);
extern NhlErrorTypes ilapsg_W(void);
extern NhlErrorTypes ilapsF_W(void);
extern NhlErrorTypes ilapsG_W(void);
extern NhlErrorTypes ilapvf_W(void);
extern NhlErrorTypes ilapvg_W(void);
extern NhlErrorTypes lapsf_W(void);
extern NhlErrorTypes lapsg_W(void);
extern NhlErrorTypes lapsF_W(void);
extern NhlErrorTypes lapsG_W(void);
extern NhlErrorTypes lapvf_W(void);
extern NhlErrorTypes lapvg_W(void);
extern NhlErrorTypes lderuvf_W(void);
extern NhlErrorTypes lderuvg_W(void);
extern NhlErrorTypes uv2sfvpf_W(void);
extern NhlErrorTypes uv2sfvpg_W(void);
extern NhlErrorTypes uv2vrdvf_W(void);
extern NhlErrorTypes uv2vrdvg_W(void);
extern NhlErrorTypes uv2dvf_W(void);
extern NhlErrorTypes uv2dvg_W(void);
extern NhlErrorTypes uv2dvF_W(void);
extern NhlErrorTypes uv2dvG_W(void);
extern NhlErrorTypes uv2vrf_W(void);
extern NhlErrorTypes uv2vrg_W(void);
extern NhlErrorTypes uv2vrF_W(void);
extern NhlErrorTypes uv2vrG_W(void);
extern NhlErrorTypes vr2uvf_W(void);
extern NhlErrorTypes vr2uvg_W(void);
extern NhlErrorTypes vrdv2uvf_W(void);
extern NhlErrorTypes vrdv2uvg_W(void);

extern NhlErrorTypes vhaec_W(void);
extern NhlErrorTypes vhaeC_W(void);
extern NhlErrorTypes vhagc_W(void);
extern NhlErrorTypes vhagC_W(void);
extern NhlErrorTypes vhsec_W(void);
extern NhlErrorTypes vhseC_W(void);
extern NhlErrorTypes vhsgc_W(void);
extern NhlErrorTypes vhsgC_W(void);
extern NhlErrorTypes shaec_W(void);
extern NhlErrorTypes shagc_W(void);
extern NhlErrorTypes shsec_W(void);
extern NhlErrorTypes shsgc_W(void);
extern NhlErrorTypes shaeC_W(void);
extern NhlErrorTypes shagC_W(void);
extern NhlErrorTypes shseC_W(void);
extern NhlErrorTypes shsgC_W(void);
extern NhlErrorTypes rhomb_trunc_W(void);
extern NhlErrorTypes rhomb_trunC_W(void);
extern NhlErrorTypes tri_trunc_W(void);
extern NhlErrorTypes tri_trunC_W(void);
extern NhlErrorTypes pop_remap_W(void);
extern NhlErrorTypes smth9_W(void);

extern NhlErrorTypes nggcog_W(void);

extern NhlErrorTypes natgrids_W(void);
extern NhlErrorTypes natgridd_W(void);
extern NhlErrorTypes nnsetp_W (void);
extern NhlErrorTypes nngetp_W (void);
extern NhlErrorTypes nngetaspects_W(void);
extern NhlErrorTypes nngetslopes_W(void);
extern NhlErrorTypes nnpntinits_W(void);
extern NhlErrorTypes nnpnts_W(void);
extern NhlErrorTypes nnpntend_W(void);

extern NhlErrorTypes dsgrid2s_W(void);
extern NhlErrorTypes dsgrid2d_W(void);
extern NhlErrorTypes dsgrid3s_W(void);
extern NhlErrorTypes dsgrid3d_W(void);
extern NhlErrorTypes dspnt2s_W(void);
extern NhlErrorTypes dspnt2d_W(void);
extern NhlErrorTypes dspnt3s_W(void);
extern NhlErrorTypes dspnt3d_W(void);
extern NhlErrorTypes dssetp_W(void);
extern NhlErrorTypes dsgetp_W(void);

extern NhlErrorTypes nngetaspectd_W(void);
extern NhlErrorTypes nngetsloped_W(void);
extern NhlErrorTypes nnpntinitd_W(void);
extern NhlErrorTypes nnpntd_W(void);
extern NhlErrorTypes nnpntendd_W(void);

extern NhlErrorTypes shgrid_W(void);
extern NhlErrorTypes shgetnp_W(void);
extern NhlErrorTypes shgetp_W(void);
extern NhlErrorTypes shsetp_W(void);

extern NhlErrorTypes csstri_W(void);
extern NhlErrorTypes cssgrid_W(void);
extern NhlErrorTypes css2c_W(void);
extern NhlErrorTypes csc2s_W(void);
extern NhlErrorTypes csvoro_W(void);
extern NhlErrorTypes cssetp_W(void);
extern NhlErrorTypes csgetp_W(void);

extern NhlErrorTypes ftsetp_W(void);
extern NhlErrorTypes ftgetp_W(void);
extern NhlErrorTypes ftcurv_W(void);
extern NhlErrorTypes ftcurvd_W(void);
extern NhlErrorTypes ftcurvi_W(void);
extern NhlErrorTypes ftcurvp_W(void);
extern NhlErrorTypes ftcurvpi_W(void);
extern NhlErrorTypes ftcurvs_W(void);
extern NhlErrorTypes ftcurvps_W(void);
extern NhlErrorTypes ftkurv_W(void);
extern NhlErrorTypes ftkurvp_W(void);
extern NhlErrorTypes ftkurvd_W(void);
extern NhlErrorTypes ftkurvpd_W(void);
extern NhlErrorTypes ftsurf_W(void);

extern NhlErrorTypes csa1s_W(void);
extern NhlErrorTypes csa1xs_W(void);
extern NhlErrorTypes csa2s_W(void);
extern NhlErrorTypes csa2xs_W(void);
extern NhlErrorTypes csa2ls_W(void);
extern NhlErrorTypes csa2lxs_W(void);
extern NhlErrorTypes csa3s_W(void);
extern NhlErrorTypes csa3xs_W(void);
extern NhlErrorTypes csa3ls_W(void);
extern NhlErrorTypes csa3lxs_W(void);

extern NhlErrorTypes drwsrfc_W(void);
extern NhlErrorTypes drwvctc_W(void);
extern NhlErrorTypes drwconc_W(void);
extern NhlErrorTypes tdinit_W(void);
extern NhlErrorTypes tdpara_W(void);
extern NhlErrorTypes tdez2d_W(void);
extern NhlErrorTypes tdez3d_W(void);
extern NhlErrorTypes tdsetp_W(void);
extern NhlErrorTypes tdgetp_W(void);
extern NhlErrorTypes wmsetp_W(void);
extern NhlErrorTypes wmgetp_W(void);
extern NhlErrorTypes wmbarb_W(void);
extern NhlErrorTypes wmdrft_W(void);
extern NhlErrorTypes wmlabs_W(void);

extern NhlErrorTypes regcoef_W(void);
extern NhlErrorTypes regCoef_W(void);
extern NhlErrorTypes regline_W(void);
extern NhlErrorTypes stat2_W(void);
extern NhlErrorTypes stat_trim_W(void);
extern NhlErrorTypes stat_medrng_W(void);
extern NhlErrorTypes stat4_W(void);
extern NhlErrorTypes dim_stat4_W(void);
extern NhlErrorTypes dim_median_W(void);
extern NhlErrorTypes dim_rmvmean_W(void);
extern NhlErrorTypes dim_rmvmed_W(void);
extern NhlErrorTypes dim_standardize_W(void);
extern NhlErrorTypes dim_pqsort_W(void);
extern NhlErrorTypes esacr_W(void);
extern NhlErrorTypes esacv_W(void);
extern NhlErrorTypes esccr_W(void);
extern NhlErrorTypes esccv_W(void);
extern NhlErrorTypes escorc_W(void);
extern NhlErrorTypes ezfftf_W(void);
extern NhlErrorTypes ezfftb_W(void);
extern NhlErrorTypes fourier_info_W(void);

extern NhlErrorTypes rdsstoi_W(void);
extern NhlErrorTypes vibeta_W(void);
extern NhlErrorTypes prcwater_dp_W(void);
extern NhlErrorTypes int2p_W(void);
extern NhlErrorTypes hydro_W(void);
extern NhlErrorTypes linmsg_W(void);
extern NhlErrorTypes linint1_W(void);
extern NhlErrorTypes linint2_W(void);
extern NhlErrorTypes linint2_points_W(void);
extern NhlErrorTypes pres_hybrid_W(void);
extern NhlErrorTypes pres_hybrid_ccm_W(void);
extern NhlErrorTypes dpres_hybrid_W(void);
extern NhlErrorTypes dpres_hybrid_ccm_W(void);
extern NhlErrorTypes pslhyp_W(void);
extern NhlErrorTypes pslec_W(void);
extern NhlErrorTypes pslhor_W(void);
extern NhlErrorTypes gc_latlon_W(void);

extern NhlErrorTypes monthday_W(void);
extern NhlErrorTypes day_of_year_W(void);
extern NhlErrorTypes days_in_month_W(void);
extern NhlErrorTypes day_of_week_W(void);
extern NhlErrorTypes isleapyear_W(void);
extern NhlErrorTypes greg2jul_W(void);
extern NhlErrorTypes jul2greg_W(void);

extern NhlErrorTypes angmom_atm_W(void);
extern NhlErrorTypes relhum_W(void);
extern NhlErrorTypes runave_W(void);
extern NhlErrorTypes wgt_runave_W(void);
extern NhlErrorTypes wgt_areaave_W(void);
extern NhlErrorTypes wgt_volave_W(void);
extern NhlErrorTypes wgt_arearmse_W(void);
extern NhlErrorTypes wgt_volrmse_W(void);
extern NhlErrorTypes wgt_volrmse_ccm_W(void);
extern NhlErrorTypes wgt_volave_ccm_W(void);
extern NhlErrorTypes dtrend_W(void);
extern NhlErrorTypes dtrend_msg_W(void);
extern NhlErrorTypes local_min_W(void);
extern NhlErrorTypes local_max_W(void);
extern NhlErrorTypes fluxEddy_W(void);
extern NhlErrorTypes cz2ccm_W(void);
extern NhlErrorTypes specx_anal_W(void);
extern NhlErrorTypes specxy_anal_W(void);
extern NhlErrorTypes chiinv_W(void);
extern NhlErrorTypes betainc_W(void);
extern NhlErrorTypes ttest_W(void);
extern NhlErrorTypes ftest_W(void);
extern NhlErrorTypes rtest_W(void);
extern NhlErrorTypes equiv_sample_size_W(void);
extern NhlErrorTypes NhlGetNamedColorIndex_W(void);
extern NhlErrorTypes output_gif_W(void);
extern NhlErrorTypes nice_mnmxintvl_W(void);
extern NhlErrorTypes dim_gbits_W(void);
extern NhlErrorTypes getbitsone_W(void);

void NclAddUserFuncs(void)
{
    void *args;
    int dimsizes[NCL_MAX_DIMENSIONS];
    int nargs;
/*
 * Register "vinth2p".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(9);
        SetArgTemplate(args,0,"numeric",NclANY,NclANY);nargs++;
        SetArgTemplate(args,1,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,2,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,3,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,4,"numeric",NclANY,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,5,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,6,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,7,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,8,"logical",1,dimsizes);nargs++;
        NclRegisterFunc(vinth2p_W,args,"vinth2p",nargs);

/*
 * Register "vinth2p_ecmwf".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(12);
        SetArgTemplate(args,0,"numeric",NclANY,NclANY);nargs++;
        SetArgTemplate(args,1,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,2,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,3,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,4,"numeric",NclANY,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,5,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,6,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,7,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,8,"logical",1,dimsizes);nargs++;

        SetArgTemplate(args,9,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,10,"numeric",NclANY,NclANY);nargs++;
        dimsizes[0] = 2;
        SetArgTemplate(args,11,"numeric",2,NclANY);nargs++;
        NclRegisterFunc(vinth2p_ecmwf_W,args,"vinth2p_ecmwf",nargs);

/*
 * Register "g2gsh".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 2;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(g2gsh_W,args,"g2gsh",nargs);
/*
 * Register "f2gsh".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 2;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(f2gsh_W,args,"f2gsh",nargs);
/*
 * Register "g2fsh".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 2;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(g2fsh_W,args,"g2fsh",nargs);
/*
 * Register "f2fsh".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;

    NclRegisterFunc(f2fsh_W,args,"f2fsh",nargs);
/*
 * Register "fo2fsh".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(fo2fsh_W,args,"fo2fsh",nargs);
/*
 * Register "f2fosh".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(f2fosh_W,args,"f2fosh",nargs);
/*
 * Register "fo2fshv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(fo2fshv_W,args,"fo2fshv",nargs);
/*
 * Register "f2foshv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(f2foshv_W,args,"f2foshv",nargs);
/*
 * Register "g2gshv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterProc(g2gshv_W,args,"g2gshv",nargs);
/*
 * Register "f2gshv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterProc(f2gshv_W,args,"f2gshv",nargs);
/*
 * Register "g2fshv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(g2fshv_W,args,"g2fshv",nargs);
/*
 * Register "f2fshv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(f2fshv_W,args,"f2fshv",nargs);
/*
 * Register "eofcov".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(eofcov_W,args,"eofcov",nargs);
/*
 * Register "eofcov_pcmsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(eofcov_pcmsg_W,args,"eofcov_pcmsg",nargs);
/*
 * Register "eofcor".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(eofcor_W,args,"eofcor",nargs);
/*
 * Register "eofcor_pcmsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(eofcor_pcmsg_W,args,"eofcor_pcmsg",nargs);
/*
 * Register "eofcov_ts".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(eofcov_ts_W,args,"eofcov_ts",nargs);
/*
 * Register "eofcov_ts_pcmsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(eofcov_ts_pcmsg_W,args,"eofcov_ts_pcmsg",nargs);
/*
 * Register "eofcor_ts".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(eofcor_ts_W,args,"eofcor_ts",nargs);
/*
 * Register "eofcor_ts_pcmsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(eofcor_ts_pcmsg_W,args,"eofcor_ts_pcmsg",nargs);
/*
 * Register "eof_varimax".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    NclRegisterFunc(eof_varimax_W,args,"eof_varimax",nargs);
/*
 * Register "center_finite_diff".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(center_finite_diff_W,args,"center_finite_diff",nargs);
/*
 * Register "uv2vr_cfd".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(uv2vr_cfd_W,args,"uv2vr_cfd",nargs);
/*
 * Register "uv2dv_cfd".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(uv2dv_cfd_W,args,"uv2dv_cfd",nargs);
/*
 * Register "svdcov".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(7);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    NclRegisterFunc(svdcov_W,args,"svdcov",nargs);
/*
 * Register "svdstd".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(7);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    NclRegisterFunc(svdstd_W,args,"svdstd",nargs);
/*
 * Register "svdcov_sv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    NclRegisterFunc(svdcov_sv_W,args,"svdcov_sv",nargs);
/*
 * Register "svdstd_sv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    NclRegisterFunc(svdstd_sv_W,args,"svdstd_sv",nargs);
/*
 * Register "sindex_yrmo".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(sindex_yrmo_W,args,"sindex_yrmo",nargs);
/*
 * Register "svdpar".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;

    NclRegisterProc(svdpar_W,args,"svdpar",nargs);
/*
 * Register "snindex_yrmo".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;

    NclRegisterFunc(snindex_yrmo_W,args,"snindex_yrmo",nargs);
/*
 * Register "x_skewt".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(x_skewt_W,args,"x_skewt",nargs);
/*
 * Register "y_skewt".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(y_skewt_W,args,"y_skewt",nargs);
/*
 * Register "tmr_skewt".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(tmr_skewt_W,args,"tmr_skewt",nargs);
/*
 * Register "tda_skewt".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(tda_skewt_W,args,"tda_skewt",nargs);
/*
 * Register "satlft_skewt".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(satlft_skewt_W,args,"satlft_skewt",nargs);
/*
 * Register "ptlcl_skewt".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterProc(ptlcl_skewt_W,args,"ptlcl_skewt",nargs);
/*
 * Register "showal_skewt".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(showal_skewt_W,args,"showal_skewt",nargs);
/*
 * Register "pw_skewt".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(pw_skewt_W,args,"pw_skewt",nargs);
/*
 * Register "cape_thermo".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(cape_thermo_W,args,"cape_thermo",nargs);
/*
 * Register gaus_lobat".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(gaus_lobat_W,args,"gaus_lobat",nargs);
/*
 * Register gaus_lobat_wgt".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(gaus_lobat_wgt_W,args,"gaus_lobat_wgt",nargs);
/*
 * Register "linrood_latwgt".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(linrood_latwgt_W,args,"linrood_latwgt",nargs);
/*
 * Register "linrood_wgt".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(linrood_wgt_W,args,"linrood_wgt",nargs);
/*
 * Register "dv2uvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(dv2uvf_W,args,"dv2uvf",nargs);

/*
 * Register "dv2uvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    NclRegisterProc(dv2uvg_W,args,"dv2uvg",nargs);
/*
 * Register "gradsf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(gradsf_W,args,"gradsf",nargs);

/*
 * Register "gradsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(gradsg_W,args,"gradsg",nargs);

/*
 * Register "igradsf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(igradsf_W,args,"igradsf",nargs);

/*
 * Register "igradsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(igradsg_W,args,"igradsg",nargs);
/*
 * Register "igradsF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(igradsF_W,args,"igradsF",nargs);

/*
 * Register "igradsG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(igradsG_W,args,"igradsG",nargs);

/*
 * Register "ilapsf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(ilapsf_W,args,"ilapsf",nargs);

/*
 * Register "ilapsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(ilapsg_W,args,"ilapsg",nargs);

/*
 * Register "ilapsF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(ilapsF_W,args,"ilapsF",nargs);

/*
 * Register "ilapsG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(ilapsG_W,args,"ilapsG",nargs);

/*
 * Register "ilapvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(ilapvf_W,args,"ilapvf",nargs);

/*
 * Register "ilapvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(ilapvg_W,args,"ilapvg",nargs);

/*
 * Register "lapsf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(lapsf_W,args,"lapsf",nargs);

/*
 * Register "lapsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(lapsg_W,args,"lapsg",nargs);

/*
 * Register "lapsF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(lapsF_W,args,"lapsF",nargs);

/*
 * Register "lapsG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(lapsG_W,args,"lapsG",nargs);

/*
 * Register "lapvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(lapvf_W,args,"lapvf",nargs);

/*
 * Register "lapvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(lapvg_W,args,"lapvg",nargs);

/*
 * Register "uv2sfvpf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(uv2sfvpf_W,args,"uv2sfvpf",nargs);

/*
 * Register "uv2sfvpg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(uv2sfvpg_W,args,"uv2sfvpg",nargs);

/*
 * Register "uv2vrdvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(uv2vrdvg_W,args,"uv2vrdvg",nargs);

/*
 * Register "uv2vrdvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(uv2vrdvf_W,args,"uv2vrdvf",nargs);

/*
 * Register "lderuvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(lderuvf_W,args,"lderuvf",nargs);

/*
 * Register "lderuvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(lderuvg_W,args,"lderuvg",nargs);

/*
 * Register "uv2dvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

        NclRegisterProc(uv2dvf_W,args,"uv2dvf",nargs);

/*
 * Register "uv2dvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(uv2dvg_W,args,"uv2dvg",nargs);

/*
 * Register "uv2dvF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(uv2dvF_W,args,"uv2dvF",nargs);

/*
 * Register "uv2dvG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(uv2dvG_W,args,"uv2dvG",nargs);

/*
 * Register "uv2vrf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(uv2vrf_W,args,"uv2vrf",nargs);

/*
 * Register "uv2vrg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(uv2vrg_W,args,"uv2vrg",nargs);

/*
 * Register "uv2vrF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(uv2vrF_W,args,"uv2vrF",nargs);

/*
 * Register "uv2vrG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(uv2vrG_W,args,"uv2vrG",nargs);

/*
 * Register "vr2uvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(vr2uvf_W,args,"vr2uvf",nargs);

/*
 * Register "vr2uvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(vr2uvg_W,args,"vr2uvg",nargs);

/*
 * Register "vrdv2uvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(vrdv2uvf_W,args,"vrdv2uvf",nargs);

/*
 * Register "vrdv2uvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(vrdv2uvg_W,args,"vrdv2uvg",nargs);

/*
 * Register "vhaec".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(vhaec_W,args,"vhaec",nargs);

/*
 * Register "vhaeC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(vhaeC_W,args,"vhaeC",nargs);

/*
 * Register "vhagc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(vhagc_W,args,"vhagc",nargs);

/*
 * Register "vhagC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(vhagC_W,args,"vhagC",nargs);

/*
 * Register "vhsec".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(vhsec_W,args,"vhsec",nargs);

/*
 * Register "vhseC".
 *
 * Create private argument array.
 */
    nargs = 0;
    dimsizes[0] = 1;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(vhseC_W,args,"vhseC",nargs);

/*
 * Register "vhsgc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(vhsgc_W,args,"vhsgc",nargs);

/*
 * Register "vhsgC".
 *
 * Create private argument array.
 */
    nargs = 0;
    dimsizes[0] = 1;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(vhsgC_W,args,"vhsgC",nargs);

/*
 * Register "shaec".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(shaec_W,args,"shaec",nargs);

/*
 * Register "shagc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(shagc_W,args,"shagc",nargs);

/*
 * Register "shsec".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(shsec_W,args,"shsec",nargs);

/*
 * Register "shsgc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterProc(shsgc_W,args,"shsgc",nargs);

/*
 * Register "shaeC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(shaeC_W,args,"shaeC",nargs);

/*
 * Register "shagC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(shagC_W,args,"shagC",nargs);

/*
 * Register "shseC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(shseC_W,args,"shseC",nargs);

/*
 * Register "shsgC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(shsgC_W,args,"shsgC",nargs);

/*
 * Register "rhomb_trunC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(rhomb_trunC_W,args,"rhomb_trunC",nargs);

/*
 * Register "rhomb_trunc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterProc(rhomb_trunc_W,args,"rhomb_trunc",nargs);

/*
 * Register "tri_trunC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(tri_trunC_W,args,"tri_trunC",nargs);

/*
 * Register "tri_trunc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1; 
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterProc(tri_trunc_W,args,"tri_trunc",nargs);

/*
 * Register "pop_remap"
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterProc(pop_remap_W,args,"pop_remap",nargs);

/*
 * Register "smth9"
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;

    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(smth9_W,args,"smth9",nargs);

/*
 *  Register nggcog.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterProc(nggcog_W, args, "nggcog", nargs);

/*
 * Register "natgrids".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(5);
/*
 * Configure five parameters identically as single dimension float
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(natgrids_W,args,"natgrids",nargs);
/*
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(5);
/*
 * Configure five parameters identically as single dimension double
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",NclANY,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(natgridd_W,args,"natgridd",nargs);
/*
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(7);
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
/*
 * Configure three parameters identically as single dimension float
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;

        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterProc(drwsrfc_W,args,"drwsrfc",nargs);
/*
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(3);
    dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterProc(drwvctc_W,args,"drwvctc",nargs);
/*
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(2);
    dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterProc(drwconc_W,args,"drwconc",nargs);
/*
 * Register tdinit.
 */
        nargs = 0;
        args = NewArgs(4);

		dimsizes[0] = 3;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

		dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterProc(tdinit_W,args,"tdinit",nargs);
/*
 * Register tdpara.
 */
        nargs = 0;
        args = NewArgs(3);

		dimsizes[0] = 3;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterProc(tdpara_W,args,"tdpara",nargs);
/*
 * Register tdez2d.
 */
        nargs = 0;
        args = NewArgs(8);
    dimsizes[0] = 1;

        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;

        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;

        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterProc(tdez2d_W,args,"tdez2d",nargs);
/*
 * Register tdez3d.
 */
        nargs = 0;
        args = NewArgs(10);
    dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;

        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",3,NclANY);nargs++;

        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterProc(tdez3d_W,args,"tdez3d",nargs);

/*
 *  Register tdsetp.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    SetArgTemplate(args, nargs, NclANY, 1, dimsizes);
    nargs++;
    NclRegisterProc(tdsetp_W, args, "tdsetp", nargs);

/*
 *  Register wmsetp.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    SetArgTemplate(args, nargs, NclANY, 1, dimsizes);
    nargs++;
    NclRegisterProc(wmsetp_W, args, "wmsetp", nargs);

/*
 *  Register wmgetp.
 */
    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    NclRegisterFunc(wmgetp_W, args, "wmgetp", nargs);

/*
 *  Register tdgetp.
 */
    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    NclRegisterFunc(tdgetp_W, args, "tdgetp", nargs);

/*
 * Register wmbarb
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;

    SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
    NclRegisterProc(wmbarb_W,args,"wmbarb",nargs);

/*
 * Register wmdrft
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    
    SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
    NclRegisterProc(wmdrft_W,args,"wmdrft",nargs);

/*
 * Register wmlabs
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    
    SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    NclRegisterProc(wmlabs_W,args,"wmlabs",nargs);

/*
 *  Register nnsetp.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    SetArgTemplate(args, nargs, NclANY, 1, dimsizes);
    nargs++;
    NclRegisterProc(nnsetp_W, args, "nnsetp", nargs);

/*
 *  Register nngetp.
 */
    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    NclRegisterFunc(nngetp_W, args, "nngetp", nargs);

/*
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(2);
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterFunc(nngetaspects_W,args,"nngetaspects",nargs);
/*
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(2);
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterFunc(nngetaspectd_W,args,"nngetaspectd",nargs);
/*
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(2);
        SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterFunc(nngetslopes_W,args,"nngetslopes",nargs);
/*
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(2);
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterFunc(nngetsloped_W,args,"nngetsloped",nargs);
/*
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(3);
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterProc(nnpntinits_W,args,"nnpntinits",nargs);
/*
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(3);
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterProc(nnpntinitd_W,args,"nnpntinitd",nargs);
/*
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(2);
    dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterFunc(nnpnts_W,args,"nnpnts",nargs);
/*
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(2);
    dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterFunc(nnpntd_W,args,"nnpntd",nargs);
/*
 * Create private argument array
 */
        args = NewArgs(0);
        nargs = 0;
    dimsizes[0] = 1;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterProc(nnpntend_W,args,"nnpntend",nargs);
/*
 * Create private argument array
 */
        args = NewArgs(0);
        nargs = 0;
    dimsizes[0] = 1;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterProc(nnpntendd_W,args,"nnpntendd",nargs);

/*
 *  Register Shsgrid functions.
 */

/*
 *  Register shgetnp.
 */
    nargs = 0;
    args = NewArgs(7);
    
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes); nargs++;
    NclRegisterFunc(shgetnp_W, args, "shgetnp", nargs);
/*
 *  Register Shgrid.
 */
    nargs = 0;
    args = NewArgs(7);
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterFunc(shgrid_W, args, "shgrid", nargs);

/*
 *  Register shsetp.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args, nargs, "string", 1, NclANY);
    nargs++;
    SetArgTemplate(args, nargs, NclANY, 1, NclANY);
    nargs++;
    NclRegisterProc(shsetp_W, args, "shsetp", nargs);
 
/*
 *  Register shgetp.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args, nargs, "string", 1, NclANY);
    nargs++;
    NclRegisterFunc(shgetp_W, args, "shgetp", nargs);

/*
 *  Register Cssgrid functions.
 */

/*
 *  Register csvoro.
 */
    nargs = 0;
    args = NewArgs(10);
    
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "integer", 1, NclANY); nargs++;
    NclRegisterProc(csvoro_W, args, "csvoro", nargs);

/*
 *  Register csc2s.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args, nargs, "numeric", NclANY, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", NclANY, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", NclANY, NclANY); nargs++;
    NclRegisterFunc(csc2s_W, args, "csc2s", nargs);

/*
 *  Register css2c.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args, nargs, "numeric", NclANY, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", NclANY, NclANY); nargs++;
    NclRegisterFunc(css2c_W, args, "css2c", nargs);

/*
 *  Register cssetp.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    SetArgTemplate(args, nargs, NclANY, 1, dimsizes);
    nargs++;
    NclRegisterProc(cssetp_W, args, "cssetp", nargs);

/*
 *  Register csgetp.
 */
    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    NclRegisterFunc(csgetp_W, args, "csgetp", nargs);

/*
 *  Register csstri.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    NclRegisterFunc(csstri_W, args, "csstri", nargs);

/*
 *  Register cssgrid.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", NclANY, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    NclRegisterFunc(cssgrid_W, args, "cssgrid", nargs);

/*
 *  Register Fitgrid functions.
 */

/*
 *  Register ftsetp.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, NclANY, 1, dimsizes); nargs++;
    NclRegisterProc(ftsetp_W, args, "ftsetp", nargs);

/*
 *  Register ftgetp.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args, nargs, "string", 1, dimsizes); nargs++;
    NclRegisterFunc(ftgetp_W, args, "ftgetp", nargs);

/*
 *  Register ftcurv.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", NclANY, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterFunc(ftcurv_W, args, "ftcurv", nargs);

/*
 *  Register ftcurvd.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", NclANY, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterFunc(ftcurvd_W, args, "ftcurvd", nargs);

/*
 *  Register ftcurvi.
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", NclANY, NclANY); nargs++;
    NclRegisterFunc(ftcurvi_W, args, "ftcurvi", nargs);

/*
 *  Register ftcurvp.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", NclANY, NclANY); nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterFunc(ftcurvp_W, args, "ftcurvp", nargs);

/*
 *  Register ftcurvpi.
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", NclANY, NclANY); nargs++;
    NclRegisterFunc(ftcurvpi_W, args, "ftcurvpi", nargs);

/*
 *  Register ftcurvs.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", NclANY, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterFunc(ftcurvs_W, args, "ftcurvs", nargs);

/*
 *  Register ftcurvps.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", NclANY, NclANY); nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterFunc(ftcurvps_W, args, "ftcurvps", nargs);

/*
 *  Register ftkurv.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterProc(ftkurv_W, args, "ftkurv", nargs);

/*
 *  Register ftkurvp.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterProc(ftkurvp_W, args, "ftkurvp", nargs);

/*
 *  Register ftkurvd.
 */
    nargs = 0;
    args = NewArgs(9);
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterProc(ftkurvd_W, args, "ftkurvd", nargs);

/*
 *  Register ftkurvpd.
 */
    nargs = 0;
    args = NewArgs(9);
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterProc(ftkurvpd_W, args, "ftkurvpd", nargs);

/*
 *  Register ftsurf.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", NclANY, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterFunc(ftsurf_W, args, "ftsurf", nargs);

/*
 *  Register csa1s.
 */
  nargs = 0;
  args = NewArgs(4);
  SetArgTemplate(args,nargs,"float",NclANY,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",NclANY,NclANY); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  NclRegisterFunc(csa1s_W,args,"csa1s",nargs);

/*
 *  Register csa1xs.
 */
  nargs = 0;
  args = NewArgs(7);
  SetArgTemplate(args,nargs,"float",NclANY,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",NclANY,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"float",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  NclRegisterFunc(csa1xs_W,args,"csa1xs",nargs);

/*
 *  Register csa2s.
 */
  nargs = 0;
  args = NewArgs(6);
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",NclANY,NclANY); nargs++;
  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  NclRegisterFunc(csa2s_W,args,"csa2s",nargs);

/*
 *  Register csa2xs.
 */
  nargs = 0;
  args = NewArgs(9);
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",NclANY,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;

  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"float",1,dimsizes); nargs++;
  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  NclRegisterFunc(csa2xs_W,args,"csa2xs",nargs);

/*
 *  Register csa2ls.
 */
  nargs = 0;
  args = NewArgs(6);
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",NclANY,NclANY); nargs++;

  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  NclRegisterFunc(csa2ls_W,args,"csa2ls",nargs);

/*
 *  Register csa2lxs.
 */
  nargs = 0;
  args = NewArgs(9);
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",NclANY,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;

  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"float",1,dimsizes); nargs++;
  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  NclRegisterFunc(csa2lxs_W,args,"csa2lxs",nargs);

/*
 *  Register csa3s.
 */
  nargs = 0;
  args = NewArgs(8);
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",NclANY,NclANY); nargs++;

  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  NclRegisterFunc(csa3s_W,args,"csa3s",nargs);

/*
 *  Register csa3xs.
 */
  nargs = 0;
  args = NewArgs(11);
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",NclANY,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;

  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"float",1,dimsizes); nargs++;
  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  NclRegisterFunc(csa3xs_W,args,"csa3xs",nargs);

/*
 *  Register csa3ls.
 */
  nargs = 0;
  args = NewArgs(8);
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",NclANY,NclANY); nargs++;

  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  NclRegisterFunc(csa3ls_W,args,"csa3ls",nargs);

/*
 *  Register csa3lxs.
 */
  nargs = 0;
  args = NewArgs(11);
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",NclANY,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;

  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"float",1,dimsizes); nargs++;
  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  NclRegisterFunc(csa3lxs_W,args,"csa3lxs",nargs);

/*
 * Register "dsgrid2s".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(5);
/*
 * Configure five parameters identically as single dimension float
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(dsgrid2s_W,args,"dsgrid2s",nargs);
/*
 * Register "dsgrid2d".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(5);
/*
 * Configure five parameters identically as single dimension double
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",NclANY,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(dsgrid2d_W,args,"dsgrid2d",nargs);
/*
 * Register "dsgrid3s".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(7);
/*
 * Configure five parameters identically as single dimension float
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(dsgrid3s_W,args,"dsgrid3s",nargs);
/*
 * Register "dsgrid3d".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(7);
/*
 * Configure five parameters identically as single dimension float
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",NclANY,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(dsgrid3d_W,args,"dsgrid3d",nargs);
/*
 * Register "dspnt2s".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(6);
/*
 * Configure six parameters identically as single dimension float
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterProc(dspnt2s_W,args,"dspnt2s",nargs);
/*
 * Register "dspnt2d".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(6);
/*
 * Configure six parameters identically as single dimension double
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterProc(dspnt2d_W,args,"dspnt2d",nargs);
/*
 * Register "dspnt3s".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(8);
/*
 * Configure eight parameters identically as single dimension float
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterProc(dspnt3s_W,args,"dspnt3s",nargs);
/*
 * Register "dspnt3d".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(8);
/*
 * Configure eight parameters identically as single dimension float
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterProc(dspnt3d_W,args,"dspnt3d",nargs);


/*
 *  Register dssetp.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args, nargs, "string", 1, NclANY);
    nargs++;
    SetArgTemplate(args, nargs, NclANY, 1, NclANY);
    nargs++;
    NclRegisterProc(dssetp_W, args, "dssetp", nargs);
 
/*
 *  Register dsgetp.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args, nargs, "string", 1, NclANY);
    nargs++;
    NclRegisterFunc(dsgetp_W, args, "dsgetp", nargs);

/*
 * Register "regcoef".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;

    NclRegisterFunc(regcoef_W,args,"regcoef",nargs);
/*
 * Register "regCoef".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(regCoef_W,args,"regCoef",nargs);
/*
 * Register "regline".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(regline_W,args,"regline",nargs);
/*
 * Register "stat2".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;

    NclRegisterProc(stat2_W,args,"stat2",nargs);
/*
 * Register "stat_trim".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;

    NclRegisterProc(stat_trim_W,args,"stat_trim",nargs);
/*
 * Register "stat4".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;

    NclRegisterProc(stat4_W,args,"stat4",nargs);
/*
 * Register "dim_stat4".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(dim_stat4_W,args,"dim_stat4",nargs);
/*
 * Register "stat_medrng".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;

    NclRegisterProc(stat_medrng_W,args,"stat_medrng",nargs);
/*
 * Register "dim_median".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(dim_median_W,args,"dim_median",nargs);
/*
 * Register "dim_rmvmean".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(dim_rmvmean_W,args,"dim_rmvmean",nargs);
/*
 * Register "dim_rmvmed".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(dim_rmvmed_W,args,"dim_rmvmed",nargs);
/*
 * Register "dim_standardize".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(dim_standardize_W,args,"dim_standardize",nargs);
/*
 * Register "dim_pqsort".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(dim_pqsort_W,args,"dim_pqsort",nargs);
/*
 * Register "esacr".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;

    NclRegisterFunc(esacr_W,args,"esacr",nargs);
/*
 * Register "esacv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;

    NclRegisterFunc(esacv_W,args,"esacv",nargs);
/*
 * Register "esccr".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;

    NclRegisterFunc(esccr_W,args,"esccr",nargs);
/*
 * Register "esccv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;

    NclRegisterFunc(esccv_W,args,"esccv",nargs);
/*
 * Register "escorc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(escorc_W,args,"escorc",nargs);
/*
 * Register "ezfftf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(ezfftf_W,args,"ezfftf",nargs);
/*
 * Register "ezfftb".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(ezfftb_W,args,"ezfftb",nargs);
/*
 * Register "fourier_info".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(fourier_info_W,args,"fourier_info",nargs);
/*
 * Register "rdsstoi".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;

    NclRegisterFunc(rdsstoi_W,args,"rdsstoi",nargs);
/*
 * Register "vibeta".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(vibeta_W,args,"vibeta",nargs);
/*
 * Register "prcwater_dp".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(prcwater_dp_W,args,"prcwater_dp",nargs);
/*
 * Register "int2p".
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(int2p_W,args,"int2p",nargs);
/*
 * Register "hydro".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    NclRegisterFunc(hydro_W,args,"hydro",nargs);

/*
 * Register "linmsg".
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    NclRegisterFunc(linmsg_W,args,"linmsg",nargs);

/*
 * Register "linint1".
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(linint1_W,args,"linint1",nargs);

/*
 * Register "linint2".
 */
    nargs = 0;
    args = NewArgs(7);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(linint2_W,args,"linint2",nargs);

/*
 * Register "linint2_points".
 */
    nargs = 0;
    args = NewArgs(7);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(linint2_points_W,args,"linint2_points",nargs);

/*
 * Register "pres_hybrid".
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(pres_hybrid_W,args,"pres_hybrid",nargs);

/*
 * Register "pres_hybrid_ccm".
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(pres_hybrid_ccm_W,args,"pres_hybrid_ccm",nargs);

/*
 * Register "dpres_hybrid".
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(dpres_hybrid_W,args,"dpres_hybrid",nargs);

/*
 * Register "dpres_hybrid_ccm".
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(dpres_hybrid_ccm_W,args,"dpres_hybrid_ccm",nargs);

/*
 * Register "pslhyp".
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(pslhyp_W,args,"pslhyp",nargs);

/*
 * Register "pslec".
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    NclRegisterFunc(pslec_W,args,"pslec",nargs);

/*
 * Register "pslhor".
 */
    nargs = 0;
    args = NewArgs(6);

    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(pslhor_W,args,"pslhor",nargs);

/*
 * Register "gc_latlon".
 */
    nargs = 0;
    args = NewArgs(6);

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(gc_latlon_W,args,"gc_latlon",nargs);
/*
 * Register "monthday".
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    NclRegisterFunc(monthday_W,args,"monthday",nargs);

/*
 * Register "isleapyear".
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    NclRegisterFunc(isleapyear_W,args,"isleapyear",nargs);
/*
 * Register "days_in_month".
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    NclRegisterFunc(days_in_month_W,args,"days_in_month",nargs);

/*
 * Register "day_of_week".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    NclRegisterFunc(day_of_week_W,args,"day_of_week",nargs);

/*
 * Register "day_of_year".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    NclRegisterFunc(day_of_year_W,args,"day_of_year",nargs);

/*
 * Register "greg2jul".
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    NclRegisterFunc(greg2jul_W,args,"greg2jul",nargs);

/*
 * Register "jul2greg".
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    NclRegisterFunc(jul2greg_W,args,"jul2greg",nargs);

/*
 * Register "angmom_atm".
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    NclRegisterFunc(angmom_atm_W,args,"angmom_atm",nargs);

/*
 * Register "relhum".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    NclRegisterFunc(relhum_W,args,"relhum",nargs);

/*
 * Register "runave".
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(runave_W,args,"runave",nargs);

/*
 * Register "wgt_runave".
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_runave_W,args,"wgt_runave",nargs);

/*
 * Register "wgt_areaave".
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_areaave_W,args,"wgt_areaave",nargs);

/*
 * Register "wgt_volave".
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_volave_W,args,"wgt_volave",nargs);

/*
 * Register "wgt_volave_ccm".
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_volave_ccm_W,args,"wgt_volave_ccm",nargs);

/*
 * Register "wgt_arearmse".
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_arearmse_W,args,"wgt_arearmse",nargs);

/*
 * Register "wgt_volrmse".
 */
    nargs = 0;
    args = NewArgs(6);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_volrmse_W,args,"wgt_volrmse",nargs);

/*
 * Register "wgt_volrmse_ccm".
 */
    nargs = 0;
    args = NewArgs(7);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_volrmse_ccm_W,args,"wgt_volrmse_ccm",nargs);

/*
 * Register "dtrend".
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    NclRegisterFunc(dtrend_W,args,"dtrend",nargs);

/*
 * Register "dtrend_msg".
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    NclRegisterFunc(dtrend_msg_W,args,"dtrend_msg",nargs);

/*
 * Register "local_min".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    NclRegisterFunc(local_min_W,args,"local_min",nargs);

/*
 * Register "local_max".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    NclRegisterFunc(local_max_W,args,"local_max",nargs);

/*
 * Register "fluxEddy".
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    NclRegisterFunc(fluxEddy_W,args,"fluxEddy",nargs);

/*
 * Register "cz2ccm".
 */
    nargs = 0;
    args = NewArgs(8);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    NclRegisterFunc(cz2ccm_W,args,"cz2ccm",nargs);

/*
 * Register "specx_anal".
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    NclRegisterFunc(specx_anal_W,args,"specx_anal",nargs);

/*
 * Register "specxy_anal".
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    NclRegisterFunc(specxy_anal_W,args,"specxy_anal",nargs);

/*
 * Register "chiinv".
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    NclRegisterFunc(chiinv_W,args,"chiinv",nargs);

/*
 * Register "betainc".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    NclRegisterFunc(betainc_W,args,"betainc",nargs);

/*
 * Register "ttest".
 */
    nargs = 0;
    args = NewArgs(8);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(ttest_W,args,"ttest",nargs);
/*
 * Register "ftest".
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(ftest_W,args,"ftest",nargs);

/*
 * Register "rtest".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(rtest_W,args,"rtest",nargs);

/*
 * Register "equiv_sample_size".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",NclANY,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(equiv_sample_size_W,args,"equiv_sample_size",nargs);

/*
 *  Register NhlGetNamedColorIndex.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args, nargs, "graphic", NclANY, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "string", NclANY, NclANY);   nargs++;
    NclRegisterFunc(NhlGetNamedColorIndex_W, args, "NhlGetNamedColorIndex", nargs);


/*
 *  Register output_gif.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args, nargs, "graphic", NclANY, NclANY);  nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);   nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);   nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);   nargs++;
    NclRegisterProc(output_gif_W, args, "output_gif", nargs);

/*
 *  Register nice_mnmxintvl.
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "logical", 1, dimsizes);  nargs++;
    NclRegisterFunc(nice_mnmxintvl_W, args, "nice_mnmxintvl", nargs);

/*
 *  Register dim_gbits.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args, nargs, "numeric", NclANY, NclANY);  nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    NclRegisterFunc(dim_gbits_W, args, "dim_gbits", nargs);

/*
 *  Register getbitsone.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args, nargs, "numeric", NclANY, NclANY);  nargs++;
    NclRegisterFunc(getbitsone_W, args, "getbitsone", nargs);

    return;
}

/*
 * NOTE: the following function stubs must be included with the
 * source for function NclAddUserFuncs. Future releases of this
 * documentation will describe their uses. For now though NCL
 * WILL NOT compile WITHOUT them.
 */
 void NclAddUserFileFormats(void)
{
  return;
}
 void NclAddUserHLUObjs(void)
{
  return;
}

/*
 * Coerce a missing value to double.  Also, set a default missing
 * value and set a float missing value for the return (in case the 
 * return type is a float).
 */
void coerce_missing(
NclBasicDataTypes type_x,
int               has_missing_x,
NclScalar         *missing_x,
NclScalar         *missing_dx,
NclScalar         *missing_rx)
{
/*
 * Check for missing value and coerce if neccesary.
 */
  if(has_missing_x) {
/*
 * Coerce missing value to double.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               missing_dx,
               missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));

    if(type_x != NCL_double && missing_rx != NULL) {
      _Nclcoerce((NclTypeClass)nclTypefloatClass,
                 missing_rx,
                 missing_x,
                 1,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
    if(missing_dx != NULL) {
/*
 * Get the default missing value, just in case.
 */ 
      if(type_x != NCL_double) {
        missing_dx->doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
        if(missing_rx != NULL) {
          missing_rx->floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
        }
      }
      else {
        missing_dx->doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
      }
    }
  }
}

/*
 * Coerce data to double, or just return a pointer to it if
 * it is already double.
 */
double *coerce_input_double(
void              *x,
NclBasicDataTypes type_x,
int               size_x,
int               has_missing_x,
NclScalar         *missing_x,
NclScalar         *missing_dx)
{
  double *dx;
/*
 * Coerce x to double if necessary.
 */
  if(type_x != NCL_double) {
    dx = (double*)calloc(size_x,sizeof(double));
    if( dx == NULL ) return(NULL);
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 (void*)dx,
                 x,
                 size_x,
                 missing_dx,
                 missing_x,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 (void*)dx,
                 x,
                 size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * x is already double.
 */
    dx = (double*)x;
  }
  return(dx);
}

/*
 * Coerce a subset of the data to double.
 */
void coerce_subset_input_double(
void              *x,
double            *tmp_x,
int               index_x,
NclBasicDataTypes type_x,
int               size_x,
int               has_missing_x,
NclScalar         *missing_x,
NclScalar         *missing_dx
)
{
  NclTypeClass typeclass_x;
  
/*
 * typeclass_x is what allows us to get the size of the type of x.
 */
  typeclass_x = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_x)));
/*
 * Coerce x to double.
 */
  if(has_missing_x) {
/*
 * Coerce subset to double, with missing values.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               tmp_x,
               (void*)((char*)x+index_x*(typeclass_x->type_class.size)),
               size_x,
               missing_dx,
               missing_x,
               typeclass_x);
  }
  else {
/*
 * Coerce subset to double, with no missing values.
 */
    _Nclcoerce((NclTypeClass)nclTypedoubleClass,
               tmp_x,
               (void*)((char*)x+index_x*(typeclass_x->type_class.size)),
               size_x,
               NULL,
               NULL,
               typeclass_x);
  }
}

/*
 * Checks if a variable is a scalar or not.
 * Returns 1 if it is, and a 0 if it isn't.
 */
int is_scalar(
int    ndims_x,
int    *dsizes_x
)
{
  int is_scalar;
  if(ndims_x == 1 && dsizes_x[0] == 1) {
    is_scalar = 1;
  }
  else {
    is_scalar = 0;
  }
  return(is_scalar);
}


/*
 * Copy a scalar to an array of scalars.
 */
double *copy_scalar_to_array(
double       *x,
int          ndims_x,
int          *dsizes_x,
int          size_x)
{
  int i;
  double *dx;
/*
 * Check if x is a scalar. If so, then allocate an array to hold
 * this scalar value in every element.
 */
  if(is_scalar(ndims_x,dsizes_x)) {
    dx = (double*)calloc(size_x,sizeof(double));
    if( dx == NULL ) return(NULL);
    for(i = 0; i < size_x; i++) dx[i] = *x;
  }
  else {
/*
 * x is not a scalar, so just point dx at x.
 */
    dx = x;
  }
  return(dx);
}


/*
 * Coerce double data back to float. This is mainly used for routines
 * that need to return type float. 
 */
float *coerce_output_float(
double *dx,
void   *x,
int    size_x,
int    has_allocated
)
{
  int i;
  float *rx;

  if(!has_allocated) {
/*
 * If the NCL wrapper is for a function that needs to return a float,
 * then allocate space for a float here.
 */
    rx = (float*)calloc(size_x,sizeof(float));
  }
  else {
/*
 * If returning float values to a pre-allocated slot, we need to copy the
 * coerced float values back to the original location.  Do this by creating
 * a pointer of type float that points to the original location, and then
 * loop through the values and do the coercion.
 */
    rx = (float*)x;
  }

  if( rx != NULL ) {
    for( i = 0; i < size_x; i++ ) rx[i]  = (float)dx[i];
  }
  NclFree(dx);   /* Free up double precision values */
  return(rx);
}

/*
 * This routine doesn't really do any coercion.  It just checks if 
 * the type is float, and if so, it allocates enough space to hold
 * double values. Otherwise, it returns a pointer to the double values.
 */
double *coerce_output_double(
void              *x,
NclBasicDataTypes type_x,
int               size_x)
{
  double *dx;
/*
 * Check type of x
 */
  if(type_x == NCL_float) {
/*
 * Allocate space for double precision x. There's no need to do a
 * coercion because x is an output-only variable (i.e, there are no
 * values coming in).  x can only be float or double, so only allocate
 * space for a d.p. array if x is float.
 */
    dx = (double*)calloc(size_x,sizeof(double));
  }
  else {
/*
 * Input is already double.
 */
    dx = (double*)x;
  }
  return(dx);
}

/*
 * Checks if the input contains any missing values, and returns a
 * 1 if it does, and a 0 if it doesn't.
 */
int contains_missing(
double *x,
int    size_x,
int    has_missing_x,
double missing
)
{
  int l, found_missing = 0;
/*
 * Check for missing values.
 */
  if(has_missing_x) {
    l = 0;
    while( l < size_x && !found_missing ) {
      if(x[l++] == missing) found_missing = 1;
    }
  }
  return(found_missing);
}

/*
 * Checks if the input contains any missing values, and returns a
 * 1 if it does, and a 0 if it doesn't.
 */
int contains_missing_float(
float *x,
int    size_x,
int    has_missing_x,
float missing
)
{
  int l, found_missing = 0;
/*
 * Check for missing values.
 */
  if(has_missing_x) {
    l = 0;
    while( l < size_x && !found_missing ) {
      if(x[l++] == missing) found_missing = 1;
    }
  }
  return(found_missing);
}

/*
 * Sets a subset of the output data to missing.
 */
void set_subset_output_missing(
void              *x,
int               index_x,
NclBasicDataTypes type_x,
int               size_x,
double            missing_x)
{
  int i;
  for(i = 0; i < size_x; i++) {
    if(type_x != NCL_double) {
      ((float*)x)[index_x+i] = (float)missing_x;
    }
    else {
      ((double*)x)[index_x+i] = missing_x;
    }
  }
}


void compute_nlatnlon(
int *dsizes,
int ndims,
int *nlat,
int *nlon,
int *nlatnlon,
int *nt,
int *total
)
{
  int i;

  *nlat = dsizes[ndims-2];
  *nlon = dsizes[ndims-1];
  *nlatnlon = *nlat * *nlon;
  *nt = 1;
  for(i = 0; i < ndims-2; i++) *nt *= dsizes[i];
  *total = *nlatnlon * *nt;
}

void compute_nlatanlona(
int *dsizes_in,
int *dsizes_out,
int ndims_in,
int ndims_out,
int *nlata,
int *nlona,
int *nlatanlona,
int *nlatb,
int *nlonb,
int *nlatbnlonb,
int *size_leftmost,
int *size_in,
int *size_out
)
{
  int i;

  *nlata = dsizes_in[ndims_in-2];
  *nlona = dsizes_in[ndims_in-1];
  *nlatb = dsizes_out[ndims_out-2];
  *nlonb = dsizes_out[ndims_out-1];
  *nlatanlona = *nlata * *nlona;
  *nlatbnlonb = *nlatb * *nlonb;
  *size_leftmost = 1;
  for(i = 0; i < ndims_in-2; i++) *size_leftmost *= dsizes_in[i];
  *size_in  = *size_leftmost * *nlatanlona; 
  *size_out = *size_leftmost * *nlatbnlonb;
}

