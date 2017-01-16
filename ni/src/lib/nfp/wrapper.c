#include <stdio.h>
#include <stdlib.h>
#include "wrapper.h"

#define min(x,y)   ((x) < (y) ? (x) : (y))
#define max(x,y)   ((x) > (y) ? (x) : (y))

/*
 * Declare wrapper function
 */
extern NhlErrorTypes vinth2p_W(void);
extern NhlErrorTypes vinth2p_nodes_W(void);
extern NhlErrorTypes vinth2p_ecmwf_W(void);
extern NhlErrorTypes vinth2p_ecmwf_nodes_W(void);
extern NhlErrorTypes vintp2p_ecmwf_W(void);

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
extern NhlErrorTypes eofunc_W(void);
extern NhlErrorTypes eofunc_n_W(void);
extern NhlErrorTypes eofunc_ts_W(void);
extern NhlErrorTypes eofunc_ts_n_W(void);
extern NhlErrorTypes eofcov_W(void);
extern NhlErrorTypes eofcor_W(void);
extern NhlErrorTypes eofcov_tr_W(void);
extern NhlErrorTypes eofcor_tr_W(void);
extern NhlErrorTypes eofcov_pcmsg_W(void);
extern NhlErrorTypes eofcor_pcmsg_W(void);
extern NhlErrorTypes eofcov_ts_W(void);
extern NhlErrorTypes eofcor_ts_W(void);
extern NhlErrorTypes eofcov_ts_pcmsg_W(void);
extern NhlErrorTypes eofcor_ts_pcmsg_W(void);
extern NhlErrorTypes eof2data_W(void);
extern NhlErrorTypes eof2data_n_W(void);
extern NhlErrorTypes eof_varimax_W(void);
extern NhlErrorTypes eofunc_varimax_W(void);
extern NhlErrorTypes eofunc_varimax_jl_W(void);
extern NhlErrorTypes center_finite_diff_W(void);
extern NhlErrorTypes center_finite_diff_n_W(void);
extern NhlErrorTypes uv2vr_cfd_W(void);
extern NhlErrorTypes uv2dv_cfd_W(void);
extern NhlErrorTypes svdcov_W(void);
extern NhlErrorTypes svdstd_W(void);
extern NhlErrorTypes svdcov_sv_W(void);
extern NhlErrorTypes svdstd_sv_W(void);
extern NhlErrorTypes dgeevx_lapack_W(void);
extern NhlErrorTypes svd_lapack_W(void);
extern NhlErrorTypes svdpar_W(void);
extern NhlErrorTypes sindex_yrmo_W(void);
extern NhlErrorTypes bw_bandpass_filter_W(void);
extern NhlErrorTypes weibull_W(void);
extern NhlErrorTypes trend_manken_W(void);
extern NhlErrorTypes thornthwaite_W(void);
extern NhlErrorTypes thornthwaite_r_W(void);
extern NhlErrorTypes speidx_W(void);
extern NhlErrorTypes kmeans_as136_W(void);
extern NhlErrorTypes snindex_yrmo_W(void);
#ifdef BuildEEMD
extern NhlErrorTypes ceemdan_W(void);
extern NhlErrorTypes eemd_W(void);
extern NhlErrorTypes emd_num_imfs_W(void);
#endif
extern NhlErrorTypes x_skewt_W(void);
extern NhlErrorTypes y_skewt_W(void);
extern NhlErrorTypes tmr_skewt_W(void);
extern NhlErrorTypes tda_skewt_W(void);
extern NhlErrorTypes satlft_skewt_W(void);
extern NhlErrorTypes ptlcl_skewt_W(void);
extern NhlErrorTypes showal_skewt_W(void);
extern NhlErrorTypes pw_skewt_W(void);
extern NhlErrorTypes rip_cape_2d_W(void);
extern NhlErrorTypes rip_cape_3d_W(void);
extern NhlErrorTypes wrf_cape_2d_W(void);
extern NhlErrorTypes wrf_cape_3d_W(void);
extern NhlErrorTypes wrf_tk_W(void);
extern NhlErrorTypes wrf_td_W(void);
extern NhlErrorTypes wrf_rh_W(void);
extern NhlErrorTypes wrf_slp_W(void);
extern NhlErrorTypes wrf_interp_1d_W(void);
extern NhlErrorTypes wrf_interp_2d_xy_W(void);
extern NhlErrorTypes wrf_interp_3d_z_W(void);
extern NhlErrorTypes wrf_smooth_2d_W(void);
extern NhlErrorTypes wrf_latlon_to_ij_W(void);
extern NhlErrorTypes wrf_uvmet_W(void);
extern NhlErrorTypes wrf_dbz_W(void);
extern NhlErrorTypes wrf_eth_W(void);
extern NhlErrorTypes wrf_pvo_W(void);
extern NhlErrorTypes wrf_avo_W(void);
extern NhlErrorTypes wrf_helicity_W(void);
extern NhlErrorTypes wrf_updraft_helicity_W(void);
extern NhlErrorTypes wrf_wetbulb_W(void);
extern NhlErrorTypes wrf_omega_W(void);
extern NhlErrorTypes wrf_virtual_temp_W(void);
extern NhlErrorTypes wrf_ll_to_ij_W(void);
extern NhlErrorTypes wrf_ij_to_ll_W(void);
extern NhlErrorTypes wrf_wps_read_nml_W(void);
extern NhlErrorTypes wrf_wps_open_int_W(void);
extern NhlErrorTypes wrf_wps_rdhead_int_W(void);
extern NhlErrorTypes wrf_wps_rddata_int_W(void);
extern NhlErrorTypes wrf_wps_close_int_W(void);
extern NhlErrorTypes wrf_wps_read_int_W(void);
extern NhlErrorTypes wrf_wps_write_int_W(void);
extern NhlErrorTypes wrf_vintrp_W(void);
extern NhlErrorTypes wrf_ctt_W(void);
extern NhlErrorTypes wrf_cloud_frac_W(void);
extern NhlErrorTypes wrf_monotonic_W(void);

extern NhlErrorTypes cape_thermo_W(void);
extern NhlErrorTypes gaus_lobat_W(void);
extern NhlErrorTypes gaus_lobat_wgt_W(void);
extern NhlErrorTypes linrood_latwgt_W(void);
extern NhlErrorTypes linrood_wgt_W(void);
extern NhlErrorTypes wgt_vert_avg_beta_W(void);
extern NhlErrorTypes mjo_cross_segment_W(void);
extern NhlErrorTypes mjo_cross_coh2pha_W(void);
extern NhlErrorTypes bin_sum_W(void);
extern NhlErrorTypes bin_avg_W(void);
extern NhlErrorTypes trop_wmo_W(void);
extern NhlErrorTypes moc_globe_atl_W(void);
extern NhlErrorTypes wetbulb_W(void);
extern NhlErrorTypes extval_mlegev_W(void);

extern NhlErrorTypes rgbhsv_W(void);
extern NhlErrorTypes hsvrgb_W(void);
extern NhlErrorTypes rgbhls_W(void);
extern NhlErrorTypes hlsrgb_W(void);
extern NhlErrorTypes rgbyiq_W(void);
extern NhlErrorTypes yiqrgb_W(void);

extern NhlErrorTypes gc_tarea_W(void);
extern NhlErrorTypes gc_qarea_W(void);
extern NhlErrorTypes gc_pnt2gc_W(void);
extern NhlErrorTypes gc_dangle_W(void);
extern NhlErrorTypes gc_aangle_W(void);
extern NhlErrorTypes gc_clkwise_W(void);
extern NhlErrorTypes gc_inout_W(void);
extern NhlErrorTypes gc_inout_mask_func_W(void);
extern NhlErrorTypes gc_inout_mask_proc_W(void);
extern NhlErrorTypes gc_onarc_W(void);
extern NhlErrorTypes area_poly_sphere_W(void);
extern NhlErrorTypes dv2uvf_W(void);
extern NhlErrorTypes dv2uvg_W(void);
extern NhlErrorTypes dv2uvF_W(void);
extern NhlErrorTypes dv2uvG_W(void);
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
extern NhlErrorTypes uv2sfvpF_W(void);
extern NhlErrorTypes uv2sfvpG_W(void);
extern NhlErrorTypes uv2vrdvf_W(void);
extern NhlErrorTypes uv2vrdvg_W(void);
extern NhlErrorTypes uv2vrdvF_W(void);
extern NhlErrorTypes uv2vrdvG_W(void);
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
extern NhlErrorTypes vr2uvF_W(void);
extern NhlErrorTypes vr2uvG_W(void);
extern NhlErrorTypes vrdv2uvf_W(void);
extern NhlErrorTypes vrdv2uvF_W(void);
extern NhlErrorTypes vrdv2uvg_W(void);
extern NhlErrorTypes vrdv2uvG_W(void);
extern NhlErrorTypes sfvp2uvf_W(void);
extern NhlErrorTypes sfvp2uvg_W(void);

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
extern NhlErrorTypes shsgc_R42_W(void);
extern NhlErrorTypes shaeC_W(void);
extern NhlErrorTypes shagC_W(void);
extern NhlErrorTypes shseC_W(void);
extern NhlErrorTypes shsgC_W(void);
extern NhlErrorTypes set_sphere_radius_W(void);
extern NhlErrorTypes get_sphere_radius_W(void);
extern NhlErrorTypes rhomb_trunc_W(void);
extern NhlErrorTypes rhomb_trunC_W(void);
extern NhlErrorTypes tri_trunc_W(void);
extern NhlErrorTypes tri_trunC_W(void);
extern NhlErrorTypes exp_tapershC_W(void);
extern NhlErrorTypes exp_tapersh_W(void);
extern NhlErrorTypes exp_tapersh_wgts_W(void);
extern NhlErrorTypes pop_remap_W(void);
extern NhlErrorTypes depth_to_pres_W(void);
extern NhlErrorTypes potmp_insitu_ocn_W(void);
extern NhlErrorTypes wgt_area_smooth_W(void);
extern NhlErrorTypes mixed_layer_depth_W(void);
extern NhlErrorTypes smth9_W(void);
extern NhlErrorTypes simpeq_W(void);
extern NhlErrorTypes simpne_W(void);
extern NhlErrorTypes poisson_grid_fill_W(void);
extern NhlErrorTypes wk_smooth121_W(void);
extern NhlErrorTypes spcorr_W(void);
extern NhlErrorTypes spcorr_n_W(void);
extern NhlErrorTypes pdfxy_bin_W(void);
extern NhlErrorTypes pdfx_bin_W(void);
extern NhlErrorTypes kolsm2_n_W(void);
extern NhlErrorTypes determinant_W(void);

extern NhlErrorTypes nggcog_W(void);
extern NhlErrorTypes ngritd_W(void);

extern NhlErrorTypes natgrids_W(void);
extern NhlErrorTypes natgridd_W(void);
extern NhlErrorTypes natgrid_W(void);
extern NhlErrorTypes nnsetp_W (void);
extern NhlErrorTypes nngetp_W (void);
extern NhlErrorTypes nngetaspects_W(void);
extern NhlErrorTypes nngetaspectd_W(void);
extern NhlErrorTypes nngetslopes_W(void);
extern NhlErrorTypes nngetsloped_W(void);
extern NhlErrorTypes nngetwts_W(void);
extern NhlErrorTypes nngetwtsd_W(void);
extern NhlErrorTypes nnpntinits_W(void);
extern NhlErrorTypes nnpntinitd_W(void);
extern NhlErrorTypes nnpntinit_W(void);
extern NhlErrorTypes nnpnts_W(void);
extern NhlErrorTypes nnpntd_W(void);
extern NhlErrorTypes nnpnt_W(void);
extern NhlErrorTypes nnpntend_W(void);
extern NhlErrorTypes nnpntendd_W(void);

extern NhlErrorTypes dsgrid2s_W(void);
extern NhlErrorTypes dsgrid2d_W(void);
extern NhlErrorTypes dsgrid2_W(void);
extern NhlErrorTypes dsgrid3s_W(void);
extern NhlErrorTypes dsgrid3d_W(void);
extern NhlErrorTypes dsgrid3_W(void);
extern NhlErrorTypes dspnt2s_W(void);
extern NhlErrorTypes dspnt2d_W(void);
extern NhlErrorTypes dspnt2_W(void);
extern NhlErrorTypes dspnt3s_W(void);
extern NhlErrorTypes dspnt3d_W(void);
extern NhlErrorTypes dspnt3_W(void);
extern NhlErrorTypes dssetp_W(void);
extern NhlErrorTypes dsgetp_W(void);


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

extern NhlErrorTypes csa1d_W(void);
extern NhlErrorTypes csa1xd_W(void);
extern NhlErrorTypes csa2d_W(void);
extern NhlErrorTypes csa2xd_W(void);
extern NhlErrorTypes csa2ld_W(void);
extern NhlErrorTypes csa2lxd_W(void);
extern NhlErrorTypes csa3d_W(void);
extern NhlErrorTypes csa3xd_W(void);
extern NhlErrorTypes csa3ld_W(void);
extern NhlErrorTypes csa3lxd_W(void);

extern NhlErrorTypes csa1x_W(void);
extern NhlErrorTypes csa1_W(void);
extern NhlErrorTypes csa2_W(void);
extern NhlErrorTypes csa2x_W(void);
extern NhlErrorTypes csa2l_W(void);
extern NhlErrorTypes csa2lx_W(void);
extern NhlErrorTypes csa3x_W(void);
extern NhlErrorTypes csa3_W(void);
extern NhlErrorTypes csa3lx_W(void);
extern NhlErrorTypes csa3l_W(void);

extern NhlErrorTypes drwsrfc_W(void);
extern NhlErrorTypes drwvctc_W(void);
extern NhlErrorTypes drwconc_W(void);

extern NhlErrorTypes pcsetp_W(void);
extern NhlErrorTypes tdinit_W(void);
extern NhlErrorTypes tdpara_W(void);
extern NhlErrorTypes tdclrs_W(void);
extern NhlErrorTypes tdgetp_W(void);
extern NhlErrorTypes tdgtrs_W(void);
extern NhlErrorTypes tdsetp_W(void);
extern NhlErrorTypes tdstrs_W(void);
extern NhlErrorTypes tdprpt_W(void);
extern NhlErrorTypes tdprpa_W(void);
extern NhlErrorTypes tdprpi_W(void);
extern NhlErrorTypes tdline_W(void);
extern NhlErrorTypes tdlnpa_W(void);
extern NhlErrorTypes tdgrds_W(void);
extern NhlErrorTypes tdgrid_W(void);
extern NhlErrorTypes tdlbls_W(void);
extern NhlErrorTypes tdlbla_W(void);
extern NhlErrorTypes tdplch_W(void);
extern NhlErrorTypes tdcurv_W(void);
extern NhlErrorTypes tddtri_W(void);
extern NhlErrorTypes tdstri_W(void);
extern NhlErrorTypes tditri_W(void);
extern NhlErrorTypes tdmtri_W(void);
extern NhlErrorTypes tdttri_W(void);
extern NhlErrorTypes tdctri_W(void);
extern NhlErrorTypes tdotri_W(void);
extern NhlErrorTypes tdsort_W(void);
extern NhlErrorTypes tdez1d_W(void);
extern NhlErrorTypes tdez2d_W(void);
extern NhlErrorTypes tdez3d_W(void);

extern NhlErrorTypes wmsetp_W(void);
extern NhlErrorTypes wmgetp_W(void);
extern NhlErrorTypes ngsetp_W(void);
extern NhlErrorTypes nggetp_W(void);
extern NhlErrorTypes wmbarb_W(void);
extern NhlErrorTypes wmbarbmap_W(void);
extern NhlErrorTypes wmvect_W(void);
extern NhlErrorTypes wmvectmap_W(void);
extern NhlErrorTypes wmvlbl_W(void);
extern NhlErrorTypes wmdrft_W(void);
extern NhlErrorTypes wmlabs_W(void);
extern NhlErrorTypes wmstnm_W(void);
extern NhlErrorTypes nglogo_W(void);
extern NhlErrorTypes ngezlogo_W(void);

extern NhlErrorTypes cancor_W(void);
extern NhlErrorTypes regcoef_W(void);
extern NhlErrorTypes regCoef_W(void);
extern NhlErrorTypes regCoef_n_W(void);
extern NhlErrorTypes regCoef_shields_W(void);
extern NhlErrorTypes regline_W(void);
extern NhlErrorTypes reg_multlin_W(void);
extern NhlErrorTypes stat2_W(void);
extern NhlErrorTypes stat_trim_W(void);
extern NhlErrorTypes stat_medrng_W(void);
extern NhlErrorTypes stat4_W(void);
extern NhlErrorTypes dim_stat4_W(void);
extern NhlErrorTypes dim_stat4_n_W(void);
extern NhlErrorTypes dim_median_W(void);
extern NhlErrorTypes dim_median_n_W(void);
extern NhlErrorTypes dim_rmvmean_W(void);
extern NhlErrorTypes dim_rmvmean_n_W(void);
extern NhlErrorTypes dim_rmvmed_W(void);
extern NhlErrorTypes dim_rmvmed_n_W(void);
extern NhlErrorTypes dim_standardize_W(void);
extern NhlErrorTypes dim_standardize_n_W(void);
extern NhlErrorTypes dim_rmsd_W(void);
extern NhlErrorTypes dim_rmsd_n_W(void);
extern NhlErrorTypes dim_pqsort_W(void);
extern NhlErrorTypes dim_pqsort_n_W(void);
extern NhlErrorTypes dim_num_W(void);
extern NhlErrorTypes dim_num_n_W(void);
extern NhlErrorTypes dim_numrun_n_W(void);
extern NhlErrorTypes dim_avg_wgt_W(void);
extern NhlErrorTypes dim_avg_wgt_n_W(void);
extern NhlErrorTypes dim_sum_wgt_W(void);
extern NhlErrorTypes dim_sum_wgt_n_W(void);
extern NhlErrorTypes dim_acumrun_n_W(void);
extern NhlErrorTypes esacr_W(void);
extern NhlErrorTypes esacv_W(void);
extern NhlErrorTypes esccr_W(void);
extern NhlErrorTypes esccr_shields_W(void);
extern NhlErrorTypes esccv_W(void);
extern NhlErrorTypes escorc_W(void);
extern NhlErrorTypes escovc_W(void);
extern NhlErrorTypes escorc_n_W(void);
extern NhlErrorTypes ezfftf_W(void);
extern NhlErrorTypes ezfftb_W(void);
extern NhlErrorTypes ezfftf_n_W(void);
extern NhlErrorTypes ezfftb_n_W(void);
extern NhlErrorTypes cfftf_W(void);
extern NhlErrorTypes cfftb_W(void);
extern NhlErrorTypes cfftf_frq_reorder_W(void);
extern NhlErrorTypes fft2df_W(void);
extern NhlErrorTypes fft2db_W(void);
extern NhlErrorTypes lspoly_old_W(void);
extern NhlErrorTypes lspoly_n_old_W(void);
extern NhlErrorTypes lspoly_W(void);
extern NhlErrorTypes lspoly_n_W(void);
extern NhlErrorTypes fourier_info_W(void);
extern NhlErrorTypes stdatmus_z2tdp_W(void);
extern NhlErrorTypes stdatmus_p2tdz_W(void);
extern NhlErrorTypes covcorm_W(void);
extern NhlErrorTypes covcorm_xy_W(void);

extern NhlErrorTypes rdsstoi_W(void);
extern NhlErrorTypes vibeta_W(void);
extern NhlErrorTypes prcwater_dp_W(void);
extern NhlErrorTypes int2p_W(void);
extern NhlErrorTypes int2p_n_W(void);
extern NhlErrorTypes zonal_mpsi_W(void);
extern NhlErrorTypes taper_W(void);
extern NhlErrorTypes taper_n_W(void);
extern NhlErrorTypes hydro_W(void);
extern NhlErrorTypes mixhum_ptrh_W(void);
extern NhlErrorTypes mixhum_ptd_W(void);
extern NhlErrorTypes dewtemp_trh_W(void);
extern NhlErrorTypes lclvl_W(void);
extern NhlErrorTypes linmsg_W(void);
extern NhlErrorTypes linmsg_n_W(void);
extern NhlErrorTypes linint1_W(void);
extern NhlErrorTypes linint1_n_W(void);
extern NhlErrorTypes linint2_W(void);
extern NhlErrorTypes linint2_points_W(void);
extern NhlErrorTypes area_hi2lores_W(void);
extern NhlErrorTypes area_conserve_remap_W(void);
extern NhlErrorTypes rcm2rgrid_W(void);
extern NhlErrorTypes rgrid2rcm_W(void);
extern NhlErrorTypes rcm2points_W(void);
extern NhlErrorTypes pres_hybrid_W(void);
extern NhlErrorTypes pres_hybrid_ccm_W(void);
extern NhlErrorTypes pres_hybrid_jra55_W(void);
extern NhlErrorTypes dpres_hybrid_W(void);
extern NhlErrorTypes dpres_hybrid_ccm_W(void);
extern NhlErrorTypes dpres_plevel_W(void);
extern NhlErrorTypes pres2hybrid_W(void);
extern NhlErrorTypes hyi2hyo_W(void);
extern NhlErrorTypes pres_sigma_W(void);
extern NhlErrorTypes sigma2hybrid_W(void);
extern NhlErrorTypes pslhyp_W(void);
extern NhlErrorTypes pslec_W(void);
extern NhlErrorTypes pslhor_W(void);
extern NhlErrorTypes dz_height_W(void);
extern NhlErrorTypes gc_latlon_W(void);
extern NhlErrorTypes testspan_W(void);

extern NhlErrorTypes monthday_W(void);
extern NhlErrorTypes day_of_year_W(void);
extern NhlErrorTypes days_in_month_W(void);
extern NhlErrorTypes day_of_week_W(void);
extern NhlErrorTypes isleapyear_W(void);
extern NhlErrorTypes greg2jul_W(void);
extern NhlErrorTypes jul2greg_W(void);

extern NhlErrorTypes utm2latlon_W(void);
extern NhlErrorTypes latlon2utm_W(void);

extern NhlErrorTypes cd_calendar_W(void);
extern NhlErrorTypes cd_inv_calendar_W(void);

#ifdef BuildUdunits
extern NhlErrorTypes ut_calendar_W(void);
extern NhlErrorTypes ut_inv_calendar_W(void);
extern NhlErrorTypes ut_calendar_fix_W(void);
extern NhlErrorTypes ut_inv_calendar_fix_W(void);
#endif

/*
 *  vis5d+ functions
 */
# ifdef BuildV5D
extern NhlErrorTypes    v5d_create_W(void);
extern NhlErrorTypes    v5d_write_W(void);
extern NhlErrorTypes    v5d_write_var_W(void);
extern NhlErrorTypes    v5d_close_W(void);
extern NhlErrorTypes    v5d_missing_W(void);
# endif /* BuildV5D */

extern NhlErrorTypes angmom_atm_W(void);
extern NhlErrorTypes relhum_W(void);
extern NhlErrorTypes relhum_water_W(void);
extern NhlErrorTypes relhum_ice_W(void);
extern NhlErrorTypes runave_W(void);
extern NhlErrorTypes runave_n_W(void);
extern NhlErrorTypes wgt_runave_W(void);
extern NhlErrorTypes wgt_runave_n_W(void);
extern NhlErrorTypes wgt_areaave_W(void);
extern NhlErrorTypes wgt_areaave2_W(void);
extern NhlErrorTypes wgt_areasum2_W(void);
extern NhlErrorTypes wgt_arearmse_W(void);
extern NhlErrorTypes wgt_arearmse2_W(void);
extern NhlErrorTypes wgt_volave_W(void);
extern NhlErrorTypes wgt_volrmse_W(void);
extern NhlErrorTypes wgt_volrmse_ccm_W(void);
extern NhlErrorTypes wgt_volave_ccm_W(void);
extern NhlErrorTypes filwgts_lancos_W(void);
extern NhlErrorTypes filwgts_lanczos_W(void);
extern NhlErrorTypes filwgts_normal_W(void);
extern NhlErrorTypes dtrend_W(void);
extern NhlErrorTypes dtrend_n_W(void);
extern NhlErrorTypes dtrend_quadratic_W(void);
extern NhlErrorTypes dtrend_msg_W(void);
extern NhlErrorTypes dtrend_msg_n_W(void);
extern NhlErrorTypes dtrend_quadratic_msg_n_W(void);
extern NhlErrorTypes local_min_W(void);
extern NhlErrorTypes local_max_W(void);
extern NhlErrorTypes fluxEddy_W(void);
extern NhlErrorTypes cz2ccm_W(void);
extern NhlErrorTypes specx_anal_W(void);
extern NhlErrorTypes specxy_anal_W(void);
extern NhlErrorTypes chiinv_W(void);
extern NhlErrorTypes betainc_W(void);
extern NhlErrorTypes gammainc_W(void);
extern NhlErrorTypes student_t_W(void);
extern NhlErrorTypes ttest_W(void);
extern NhlErrorTypes ftest_W(void);
extern NhlErrorTypes rtest_W(void);
extern NhlErrorTypes equiv_sample_size_W(void);
extern NhlErrorTypes z2geouv_W(void);
extern NhlErrorTypes NhlGetNamedColorIndex_W(void);
extern NhlErrorTypes rgba_to_color_index_W(void);
extern NhlErrorTypes color_index_to_rgba_W(void);
extern NhlErrorTypes output_gif_W(void);
/*
extern NhlErrorTypes attcreate_W(void);
*/
extern NhlErrorTypes nice_mnmxintvl_W(void);
extern NhlErrorTypes dim_gbits_W(void);
extern NhlErrorTypes getbitsone_W(void);
extern NhlErrorTypes conform_W(void);
extern NhlErrorTypes conform_dims_W(void);
extern NhlErrorTypes reshape_W(void);
extern NhlErrorTypes reshape_ind_W(void);
extern NhlErrorTypes fftshift_W(void);
extern NhlErrorTypes paleo_outline_W(void);
extern NhlErrorTypes inverse_matrix_W(void);
extern NhlErrorTypes solve_linsys_W(void);
extern NhlErrorTypes omega_ccm_W(void);
extern NhlErrorTypes wavelet_W(void);
extern NhlErrorTypes wavelet_default_W(void);
extern NhlErrorTypes grid2triple_W(void);
extern NhlErrorTypes triple2grid_W(void);
extern NhlErrorTypes triple2grid2d_W(void);
extern NhlErrorTypes obj_anal_ic_W(void);

extern NhlErrorTypes random_setallseed_W(void);
extern NhlErrorTypes random_chi_W(void);
extern NhlErrorTypes random_gamma_W(void);
extern NhlErrorTypes random_normal_W(void);
extern NhlErrorTypes random_uniform_W(void);
extern NhlErrorTypes round_W(void);
extern NhlErrorTypes generate_2d_array_W(void);
extern NhlErrorTypes isnan_ieee_W(void);
extern NhlErrorTypes replace_ieeenan_W(void);
extern NhlErrorTypes cdfbin_p_W(void);
extern NhlErrorTypes cdfbin_s_W(void);
extern NhlErrorTypes cdfbin_xn_W(void);
extern NhlErrorTypes cdfbin_pr_W(void);
extern NhlErrorTypes cdfgam_p_W(void);
extern NhlErrorTypes cdfgam_x_W(void);
extern NhlErrorTypes cdfnor_p_W(void);
extern NhlErrorTypes cdfnor_x_W(void);
extern NhlErrorTypes cdfchi_p_W(void);
extern NhlErrorTypes cdft_t_W(void);
extern NhlErrorTypes cdft_p_W(void);
extern NhlErrorTypes gamma_W(void);
extern NhlErrorTypes ind_resolve_W(void);
extern NhlErrorTypes unique_string_W(void);
extern NhlErrorTypes tempnam_W(void);
extern NhlErrorTypes get_ncl_version_W(void);
extern NhlErrorTypes echo_on_W(void);
extern NhlErrorTypes echo_off_W(void);
extern NhlErrorTypes erf_W(void);
extern NhlErrorTypes erfc_W(void);

extern NhlErrorTypes write_matrix_W(void);
extern NhlErrorTypes ctwrap_W(void);

extern NhlErrorTypes kron_product_W(void);

extern NhlErrorTypes sparse_matrix_mult_W(void);
extern NhlErrorTypes sparse_matrix_mult_trimesh_W(void);

extern NhlErrorTypes dim_gamfit_n_W(void);
extern NhlErrorTypes dim_spi_n_W(void);

/* added by mabouali */
/* for the Google Earth Project */
extern NhlErrorTypes directVincenty_W(void);
extern NhlErrorTypes rgba2png_W(void);
extern NhlErrorTypes add_NCL_KML_Arrow_W(void);
extern NhlErrorTypes add_NCL_KML_2DGrid_W(void);
extern NhlErrorTypes add_NCL_KML_UnstructGrid_W(void);
extern NhlErrorTypes TransformCoordinate_W(void);

/* 
 * ESMF regridding functions.
 */

#ifdef BuildESMF
extern NhlErrorTypes SMMul_W(void);
extern NhlErrorTypes Unstruct2KML_W(void);
extern NhlErrorTypes SCRIP2KML_W(void);
#endif

#ifdef BuildGRIDSPEC
extern NhlErrorTypes nccffregridW(void);
extern NhlErrorTypes nccfmakeconformalcubichgridW(void);
extern NhlErrorTypes nccfmakecouplermosaicW(void);
extern NhlErrorTypes nccfmakegnomoniccubichgridW(void);
extern NhlErrorTypes nccfmakehgridfromfileW(void);
extern NhlErrorTypes nccfmakemosaicW(void);
extern NhlErrorTypes nccfmakeregularlatlonhgridW(void);
extern NhlErrorTypes nccfmakesimplecartesianhgridW(void);
extern NhlErrorTypes nccfmakespectralhgridW(void);
extern NhlErrorTypes nccfmaketopogW(void);
extern NhlErrorTypes nccfmaketripolarhgridW(void);
extern NhlErrorTypes nccfmakevgridW(void);
#endif

void NclAddUserFuncs(void)
{
    void *args;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    int nargs;
/*
 * Register "vinth2p".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(9);
        SetArgTemplate(args,0,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,1,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,2,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,3,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,4,"numeric",0,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,5,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,6,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,7,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,8,"logical",1,dimsizes);nargs++;
        NclRegisterFunc(vinth2p_W,args,"vinth2p",nargs);

/*
 * Register "vinth2p_nodes".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(9);
        SetArgTemplate(args,0,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,1,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,2,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,3,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,4,"numeric",0,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,5,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,6,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,7,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,8,"logical",1,dimsizes);nargs++;
        NclRegisterFunc(vinth2p_nodes_W,args,"vinth2p_nodes",nargs);

/*
 * Register "vinth2p_ecmwf".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(12);
        SetArgTemplate(args,0,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,1,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,2,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,3,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,4,"numeric",0,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,5,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,6,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,7,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,8,"logical",1,dimsizes);nargs++;

        SetArgTemplate(args,9,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,10,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,11,"numeric",0,NclANY);nargs++;
        NclRegisterFunc(vinth2p_ecmwf_W,args,"vinth2p_ecmwf",nargs);

/*
 * Register "vinth2p_ecmwf_nodes".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(12);
        SetArgTemplate(args,0,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,1,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,2,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,3,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,4,"numeric",0,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,5,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,6,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,7,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,8,"logical",1,dimsizes);nargs++;

        SetArgTemplate(args,9,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,10,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,11,"numeric",0,NclANY);nargs++;
        NclRegisterFunc(vinth2p_ecmwf_nodes_W,args,"vinth2p_ecmwf_nodes",nargs);

/*
 * Register "vintp2p_ecmwf".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(10);
        SetArgTemplate(args,0,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,1,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,2,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,3,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,4,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,5,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,6,"logical",1,dimsizes);nargs++;
        SetArgTemplate(args,7,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,8,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,9,"numeric",0,NclANY);nargs++;

        NclRegisterFunc(vintp2p_ecmwf_W,args,"vintp2p_ecmwf",nargs);

/*
 * Register "g2gsh".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 2;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 2;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 2;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(g2fsh_W,args,"g2fsh",nargs);
/*
 * Register "f2fsh".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(f2fsh_W,args,"f2fsh",nargs);
/*
 * Register "fo2fsh".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(fo2fsh_W,args,"fo2fsh",nargs);
/*
 * Register "f2fosh".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(f2fosh_W,args,"f2fosh",nargs);
/*
 * Register "fo2fshv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(fo2fshv_W,args,"fo2fshv",nargs);
/*
 * Register "f2foshv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(f2foshv_W,args,"f2foshv",nargs);
/*
 * Register "g2gshv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(g2fshv_W,args,"g2fshv",nargs);
/*
 * Register "f2fshv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(f2fshv_W,args,"f2fshv",nargs);
/*
 * Register "eofunc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(eofunc_W,args,"eofunc",nargs);

/*
 * Register "eofunc_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(eofunc_n_W,args,"eofunc_n",nargs);

/*
 * Register "eofunc_ts".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(eofunc_ts_W,args,"eofunc_ts",nargs);

/*
 * Register "eofunc_ts".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(eofunc_ts_n_W,args,"eofunc_ts_n",nargs);
/*
 * Register "eofcov".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(eofcov_W,args,"eofcov",nargs);
/*
 * Register "eofcov_tr".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(eofcov_tr_W,args,"eofcov_tr",nargs);
/*
 * Register "eofcor_tr".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(eofcor_tr_W,args,"eofcor_tr",nargs);
/*
 * Register "eofcov_pcmsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(eofcov_ts_W,args,"eofcov_ts",nargs);
/*
 * Register "eofcov_ts_pcmsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(eofcor_ts_W,args,"eofcor_ts",nargs);
/*
 * Register "eofcor_ts_pcmsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(eof_varimax_W,args,"eof_varimax",nargs);

/*
 * Register "eofunc_varimax".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,0,1,dimsizes);nargs++;
    NclRegisterFunc(eofunc_varimax_W,args,"eofunc_varimax",nargs);

/*
 * Register "eofunc_varimax_jl".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,0,1,dimsizes);nargs++;
    NclRegisterFunc(eofunc_varimax_jl_W,args,"eofunc_varimax_jl",nargs);

/*
 * Register "eof2data".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    NclRegisterFunc(eof2data_W,args,"eof2data",nargs);

/*
 * Register "eof2data_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(eof2data_n_W,args,"eof2data_n",nargs);
/*
 * Register "center_finite_diff".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(center_finite_diff_W,args,"center_finite_diff",nargs);
/*
 * Register "center_finite_diff_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(center_finite_diff_n_W,args,"center_finite_diff_n",nargs);
/*
 * Register "uv2vr_cfd".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
 * Register "svdcov".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    NclRegisterFunc(svd_lapack_W,args,"svd_lapack",nargs);
/*
 * Register "dgeevx_lapack".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(6);

        SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

        NclRegisterFunc(dgeevx_lapack_W,args,"dgeevx_lapack",nargs);
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

#ifdef BuildEEMD

/*
 * Register "ceemdan"
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    NclRegisterFunc(ceemdan_W,args,"ceemdan",nargs);

/*
 * Register "eemd"
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    NclRegisterFunc(eemd_W,args,"eemd",nargs);

/*
 * Register "emd_num_imfs".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    NclRegisterFunc(emd_num_imfs_W,args,"emd_num_imfs",nargs);
#endif

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
 * Register "rip_cape_2d".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(7);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(rip_cape_2d_W,args,"rip_cape_2d",nargs);
/*
 * Register "rip_cape_3d".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(7);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(rip_cape_3d_W,args,"rip_cape_3d",nargs);

/*
 * Register "wrf_cape_2d".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(7);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(wrf_cape_2d_W,args,"wrf_cape_2d",nargs);
/*
 * Register "wrf_cape_3d".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(7);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(wrf_cape_3d_W,args,"wrf_cape_3d",nargs);
/*
 * Register "wrf_tk".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(wrf_tk_W,args,"wrf_tk",nargs);
/*
 * Register "wrf_td".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(wrf_td_W,args,"wrf_td",nargs);
/*
 * Register "wrf_rh".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(wrf_rh_W,args,"wrf_rh",nargs);
/*
 * Register "wrf_slp".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(wrf_slp_W,args,"wrf_slp",nargs);
/*
 * Register "wrf_interp_1d".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(wrf_interp_1d_W,args,"wrf_interp_1d",nargs);
/*
 * Register "wrf_interp_2d_xy".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(wrf_interp_2d_xy_W,args,"wrf_interp_2d_xy",nargs);
/*
 * Register "wrf_interp_3d_z".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(wrf_interp_3d_z_W,args,"wrf_interp_3d_z",nargs);
/*
 * Register "wrf_smooth_2d".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(2);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        NclRegisterProc(wrf_smooth_2d_W,args,"wrf_smooth_2d",nargs);
/*
 * Register "wrf_latlon_to_ij".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(4);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

        NclRegisterFunc(wrf_latlon_to_ij_W,args,"wrf_latlon_to_ij",nargs);
/*
 * Register "wrf_uvmet".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(6);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_uvmet_W,args,"wrf_uvmet",nargs);

/*
 * Register "wrf_dbz".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(8);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_dbz_W,args,"wrf_dbz",nargs);

/*
 * Register "wrf_eth".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(3);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

        NclRegisterFunc(wrf_eth_W,args,"wrf_eth",nargs);

/*
 * Register "wrf_pvo".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(11);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_pvo_W,args,"wrf_pvo",nargs);

/*
 * Register "wrf_avo".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(9);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_avo_W,args,"wrf_avo",nargs);

/*
 * Register "wrf_helicity".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(5);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_helicity_W,args,"wrf_helicity",nargs);

/*
 * Register "wrf_updraft_helicity".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(8);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_updraft_helicity_W,args,"wrf_updraft_helicity",nargs);

/*
 * Register "wrf_wetbulb".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(3);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

        NclRegisterFunc(wrf_wetbulb_W,args,"wrf_wetbulb",nargs);
/*
 * Register "extval_mlegev".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(3);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

        NclRegisterFunc(extval_mlegev_W,args,"extval_mlegev",nargs);

/*
 * Register "wrf_omega".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(4);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

        NclRegisterFunc(wrf_omega_W,args,"wrf_omega",nargs);
/*
 * Register "wrf_virtual_temp".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(2);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

        NclRegisterFunc(wrf_virtual_temp_W,args,"wrf_virtual_temp",nargs);
/*
 * Register "wrf_ll_to_ij".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(3);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_ll_to_ij_W,args,"wrf_ll_to_ij",nargs);

/*
 * Register "wrf_ij_to_ll".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(3);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_ij_to_ll_W,args,"wrf_ij_to_ll",nargs);

/*
 * Register "wrf_wps_open_int".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(1);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_wps_open_int_W,args,"wrf_wps_open_int",nargs);

/*
 * Register "wrf_wps_rdhead_int"
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(7);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;

        NclRegisterProc(wrf_wps_rdhead_int_W,args,"wrf_wps_rdhead_int",nargs);

/*
 * Register "wrf_wps_rddata_int"
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(3);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_wps_rddata_int_W,args,"wrf_wps_rddata_int",nargs);

/*
 * Register "wrf_wps_close_int".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(1);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        NclRegisterProc(wrf_wps_close_int_W,args,"wrf_wps_close_int",nargs);

/*
 * Register "wrf_wps_read_int".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(1);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_wps_read_int_W,args,"wrf_wps_read_int",nargs);

/*
 * Register "wrf_wps_write_int".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(6);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

        NclRegisterProc(wrf_wps_write_int_W,args,"wrf_wps_write_int",nargs);
/*
 * Register "wrf_wps_read_nml".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(1);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_wps_read_nml_W,args,"wrf_wps_read_nml",nargs);

/*
 * Register "wrf_ctt".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(8);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_ctt_W,args,"wrf_ctt",nargs);

/*
 * Register "wrf_cloud_fraction"
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(2);
        SetArgTemplate(args,0,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,1,"numeric",0,NclANY);nargs++;
        NclRegisterFunc(wrf_cloud_frac_W,args,"wrf_cloud_fraction",nargs);

/*
 * Register "wrf_vintrp".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(14);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_vintrp_W,args,"wrf_vintrp",nargs);

/*
 * Register "wrf_monotonic".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(6);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        NclRegisterFunc(wrf_monotonic_W,args,"wrf_monotonic",nargs);

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
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

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
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(linrood_latwgt_W,args,"linrood_latwgt",nargs);
/*
 * Register "linrood_wgt".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(linrood_wgt_W,args,"linrood_wgt",nargs);
/*
 * Register "wgt_vert_avg_beta".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(5);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(wgt_vert_avg_beta_W,args,"wgt_vert_avg_beta",nargs);

/*
 * Register "mjo_cross_segment".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(3);

        SetArgTemplate(args,nargs,"numeric",3,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",3,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        NclRegisterFunc(mjo_cross_segment_W,args,"mjo_cross_segment",nargs);

/*
 * Register "mjo_cross_coh2pha".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(2);

        SetArgTemplate(args,nargs,"numeric",3,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        NclRegisterProc(mjo_cross_coh2pha_W,args,"mjo_cross_coh2pha",nargs);

/*
 * Register "bin_sum".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(7);

        SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

        NclRegisterProc(bin_sum_W,args,"bin_sum",nargs);

/*
 * Register "bin_avg".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(6);

        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

        NclRegisterFunc(bin_avg_W,args,"bin_avg",nargs);

/*
 * Register "trop_wmo".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(4);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

        NclRegisterFunc(trop_wmo_W,args,"trop_wmo",nargs);

/*
 * Register "moc_globe_atl".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(6);
    
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",3,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",3,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",3,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",3,NclANY);nargs++;

    NclRegisterFunc(moc_globe_atl_W,args,"moc_globe_atl",nargs);

/*
 * Register "wetbulb".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(wetbulb_W,args,"wetbulb",nargs);

/*
 * Register "gc_onarc".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(gc_onarc_W,args,"gc_onarc",nargs);

/*
 * Register "area_poly_sphere".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    NclRegisterFunc(area_poly_sphere_W,args,"area_poly_sphere",nargs);


/*
 * Register "bw_bandpass_filter".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(5);

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(bw_bandpass_filter_W,args,"bw_bandpass_filter",nargs);

/*
 * Register "weibull".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(weibull_W,args,"weibull",nargs);
/*
 * Register "trend_manken".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(trend_manken_W,args,"trend_manken",nargs);
/*
 * Register "thornthwaite".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(4);

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(thornthwaite_W,args,"thornthwaite",nargs);

/*
 * Register "thornthwaite_r". This is a C routine provided via
 * the "spei" code for 'R'. This routine is NOT being advertised.
 * It is mainly to test against the Fortran thornthwaite we already
 * have.
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(4);

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(thornthwaite_W,args,"thornthwaite_r",nargs);

/*
 * Register "spei_deprecated"
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(6);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(speidx_W,args,"spei_deprecated",nargs);

/*
 * Register "kmeans_as136".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(kmeans_as136_W,args,"kmeans_as136",nargs);

/*
 * Register "gc_pnt2gc".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(gc_pnt2gc_W,args,"gc_pnt2gc",nargs);

/*
 * Register "gc_aangle".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(gc_aangle_W,args,"gc_aangle",nargs);

/*
 * Register "gc_clkwise".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(gc_clkwise_W,args,"gc_clkwise",nargs);

/*
 * Register "gc_inout".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(gc_inout_W,args,"gc_inout",nargs);

/*
 * Register "gc_inout_mask_func".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(gc_inout_mask_func_W,args,"gc_inout_mask_func",nargs);

/*
 * Register "gc_inout_mask_proc".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterProc(gc_inout_mask_proc_W,args,"gc_inout_mask_proc",nargs);

/*
 * Register "gc_qarea".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(gc_qarea_W,args,"gc_qarea",nargs);

/*
 * Register "rgbhsv".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(rgbhsv_W,args,"rgbhsv",nargs);

/*
 * Register "hsvrgb".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(hsvrgb_W,args,"hsvrgb",nargs);

/*
 * Register "rgbhls".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(rgbhls_W,args,"rgbhls",nargs);

/*
 * Register "hlsrgb".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(hlsrgb_W,args,"hlsrgb",nargs);

/*
 * Register "rgbyiq".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(rgbyiq_W,args,"rgbyiq",nargs);

/*
 * Register "yiqrgb".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(yiqrgb_W,args,"yiqrgb",nargs);

/*
 * Register "gc_tarea".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(gc_tarea_W,args,"gc_tarea",nargs);

/*
 * Register "gc_dangle".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(gc_dangle_W,args,"gc_dangle",nargs);

/*
 * Register "dv2uvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(dv2uvf_W,args,"dv2uvf",nargs);

/*
 * Register "dv2uvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterProc(dv2uvg_W,args,"dv2uvg",nargs);
/*
 * Register "dv2uvF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(dv2uvF_W,args,"dv2uvF",nargs);

/*
 * Register "dv2uvG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(dv2uvG_W,args,"dv2uvG",nargs);

/*
 * Register "gradsf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(gradsf_W,args,"gradsf",nargs);

/*
 * Register "gradsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(gradsg_W,args,"gradsg",nargs);

/*
 * Register "igradsf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(igradsf_W,args,"igradsf",nargs);

/*
 * Register "igradsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(igradsg_W,args,"igradsg",nargs);
/*
 * Register "igradsF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(igradsF_W,args,"igradsF",nargs);

/*
 * Register "igradsG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(igradsG_W,args,"igradsG",nargs);

/*
 * Register "ilapsf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(ilapsf_W,args,"ilapsf",nargs);

/*
 * Register "ilapsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(ilapsg_W,args,"ilapsg",nargs);

/*
 * Register "ilapsF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(ilapsF_W,args,"ilapsF",nargs);

/*
 * Register "ilapsG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(ilapsG_W,args,"ilapsG",nargs);

/*
 * Register "ilapvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(ilapvf_W,args,"ilapvf",nargs);

/*
 * Register "ilapvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(ilapvg_W,args,"ilapvg",nargs);

/*
 * Register "lapsf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(lapsf_W,args,"lapsf",nargs);

/*
 * Register "lapsg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(lapsg_W,args,"lapsg",nargs);

/*
 * Register "lapsF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(lapsF_W,args,"lapsF",nargs);

/*
 * Register "lapsG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(lapsG_W,args,"lapsG",nargs);

/*
 * Register "lapvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(lapvf_W,args,"lapvf",nargs);

/*
 * Register "lapvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(lapvg_W,args,"lapvg",nargs);

/*
 * Register "uv2sfvpf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(uv2sfvpf_W,args,"uv2sfvpf",nargs);

/*
 * Register "uv2sfvpF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(uv2sfvpF_W,args,"uv2sfvpF",nargs);

/*
 * Register "uv2sfvpG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(uv2sfvpG_W,args,"uv2sfvpG",nargs);

/*
 * Register "uv2sfvpg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(uv2sfvpg_W,args,"uv2sfvpg",nargs);

/*
 * Register "uv2vrdvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(uv2vrdvg_W,args,"uv2vrdvg",nargs);
/*
 * Register "uv2vrdvG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(uv2vrdvG_W,args,"uv2vrdvG",nargs);

/*
 * Register "uv2vrdvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(uv2vrdvf_W,args,"uv2vrdvf",nargs);
/*
 * Register "uv2vrdvF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(uv2vrdvF_W,args,"uv2vrdvF",nargs);

/*
 * Register "lderuvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(lderuvf_W,args,"lderuvf",nargs);

/*
 * Register "lderuvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(lderuvg_W,args,"lderuvg",nargs);

/*
 * Register "uv2dvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

        NclRegisterProc(uv2dvf_W,args,"uv2dvf",nargs);

/*
 * Register "uv2dvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(uv2dvg_W,args,"uv2dvg",nargs);

/*
 * Register "uv2dvF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(uv2dvF_W,args,"uv2dvF",nargs);

/*
 * Register "uv2dvG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(uv2dvG_W,args,"uv2dvG",nargs);

/*
 * Register "uv2vrf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(uv2vrf_W,args,"uv2vrf",nargs);

/*
 * Register "uv2vrg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(uv2vrg_W,args,"uv2vrg",nargs);

/*
 * Register "uv2vrF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(uv2vrF_W,args,"uv2vrF",nargs);

/*
 * Register "uv2vrG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(uv2vrG_W,args,"uv2vrG",nargs);

/*
 * Register "vr2uvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(vr2uvf_W,args,"vr2uvf",nargs);

/*
 * Register "vr2uvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(vr2uvg_W,args,"vr2uvg",nargs);

/*
 * Register "vr2uvF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(vr2uvF_W,args,"vr2uvF",nargs);

/*
 * Register "vr2uvG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(vr2uvG_W,args,"vr2uvG",nargs);

/*
 * Register "vrdv2uvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(vrdv2uvf_W,args,"vrdv2uvf",nargs);

/*
 * Register "vrdv2uvF".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(vrdv2uvF_W,args,"vrdv2uvF",nargs);

/*
 * Register "vrdv2uvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(vrdv2uvg_W,args,"vrdv2uvg",nargs);

/*
 * Register "vrdv2uvG".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(vrdv2uvG_W,args,"vrdv2uvG",nargs);

/*
 * Register "sfvp2uvf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(sfvp2uvf_W,args,"sfvp2uvf",nargs);

/*
 * Register "sfvp2uvg".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(sfvp2uvg_W,args,"sfvp2uvg",nargs);

/*
 * Register "vhaec".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(vhaec_W,args,"vhaec",nargs);

/*
 * Register "vhaeC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(vhaeC_W,args,"vhaeC",nargs);

/*
 * Register "vhagc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(vhagc_W,args,"vhagc",nargs);

/*
 * Register "vhagC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(vhagC_W,args,"vhagC",nargs);

/*
 * Register "vhsec".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(vhsec_W,args,"vhsec",nargs);

/*
 * Register "vhseC".
 *
 * Create private argument array.
 */
    nargs = 0;
    dimsizes[0] = 1;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(vhseC_W,args,"vhseC",nargs);

/*
 * Register "vhsgc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(vhsgc_W,args,"vhsgc",nargs);

/*
 * Register "vhsgC".
 *
 * Create private argument array.
 */
    nargs = 0;
    dimsizes[0] = 1;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(vhsgC_W,args,"vhsgC",nargs);

/*
 * Register "shaec".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(shaec_W,args,"shaec",nargs);

/*
 * Register "shagc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(shagc_W,args,"shagc",nargs);

/*
 * Register "shsec".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(shsec_W,args,"shsec",nargs);

/*
 * Register "shsgc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(shsgc_W,args,"shsgc",nargs);
/*
 * Register "shsgc_R42".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(shsgc_R42_W,args,"shsgc_R42",nargs);


/*
 * Register "shaeC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(shaeC_W,args,"shaeC",nargs);

/*
 * Register "shagC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(shagC_W,args,"shagC",nargs);

/*
 * Register "shseC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(shseC_W,args,"shseC",nargs);

/*
 * Register "shsgC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(shsgC_W,args,"shsgC",nargs);

/*
 *  Register set_sphere_radius.
 */
    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes); nargs++;
    NclRegisterProc(set_sphere_radius_W, args, "set_sphere_radius", nargs);

/*
 *  Register get_sphere_radius.
 */
    nargs = 0;
    NclRegisterFunc(get_sphere_radius_W, args, "get_sphere_radius", nargs);

/*
 * Register "rhomb_trunC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1; 
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterProc(tri_trunc_W,args,"tri_trunc",nargs);

/*
 * Register "exp_tapershC".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(exp_tapershC_W,args,"exp_tapershC",nargs);

/*
 * Register "exp_tapersh".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterProc(exp_tapersh_W,args,"exp_tapersh",nargs);

/*
 * Register "exp_tapersh_wgts".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(exp_tapersh_wgts_W,args,"exp_tapersh_wgts",nargs);

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
 * Register "depth_to_pres"
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(depth_to_pres_W,args,"depth_to_pres",nargs);

/*
 * Register "potmp_insitu_ocn"
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(potmp_insitu_ocn_W,args,"potmp_insitu_ocn",nargs);

/*
 * Register "mixed_layer_depth".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(5);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

        NclRegisterFunc(mixed_layer_depth_W,args,"mixed_layer_depth",nargs);

/*
 * Register "wgt_area_smooth"
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(wgt_area_smooth_W,args,"wgt_area_smooth",nargs);

/*
 * Register "smth9"
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(smth9_W,args,"smth9",nargs);

/*
 * Register "simpne"
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(simpne_W,args,"simpne",nargs);

/*
 * Register "poisson_grid_fill".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(7);

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterProc(poisson_grid_fill_W,args,"poisson_grid_fill",nargs);

/*
 * Register "wk_smooth121".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(1);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterProc(wk_smooth121_W,args,"wk_smooth121",nargs);

/*
 * Register "spcorr".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(spcorr_W,args,"spcorr",nargs);

/*
 * Register "spcorr".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(spcorr_n_W,args,"spcorr_n",nargs);

/*
 * Register "pdfxy_bin".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(5);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

        NclRegisterFunc(pdfxy_bin_W,args,"pdfxy_bin",nargs);

/*
 * Register "pdfx_bin".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(3);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

        NclRegisterFunc(pdfx_bin_W,args,"pdfx_bin",nargs);

/*
 * Register "kolsm2_n".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(3);

        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

        NclRegisterFunc(kolsm2_n_W,args,"kolsm2_n",nargs);

/*
 * Register "determinant".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(1);

        SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
        NclRegisterFunc(determinant_W,args,"determinant",nargs);

/*
 * Register "simpeq"
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(simpeq_W,args,"simpeq",nargs);

/*
 *  Register nggcog.
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "float", 1, NclANY); nargs++;
    NclRegisterProc(nggcog_W, args, "nggcog", nargs);

/*
 *  Register ngritd.
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "float", 1, dimsizes); nargs++;
    NclRegisterProc(ngritd_W, args, "ngritd", nargs);

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
        SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(natgrids_W,args,"natgrids",nargs);
/*
 * Register "natgridd".
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
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(natgridd_W,args,"natgridd",nargs);
/*
 * Register "natgrid".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(5);
/*
 * Configure five parameters identically as single dimension numeric
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(natgrid_W,args,"natgrid",nargs);
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
 *  Register pcsetp.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    SetArgTemplate(args, nargs, 0, 1, dimsizes);
    nargs++;
    NclRegisterProc(pcsetp_W, args, "pcsetp", nargs);

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
 * Register tdclrs.
 */
        nargs = 0;
        args = NewArgs(7);

        dimsizes[0] = 1;

        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        NclRegisterProc(tdclrs_W,args,"tdclrs",nargs);
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
 * Register tdgtrs.
 */
        nargs = 0;
        args = NewArgs(11);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterProc(tdgtrs_W,args,"tdgtrs",nargs);
/*
 *  Register tdsetp.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    SetArgTemplate(args, nargs, 0, 1, dimsizes);
    nargs++;
    NclRegisterProc(tdsetp_W, args, "tdsetp", nargs);

/*
 * Register tdstrs.
 */
        nargs = 0;
        args = NewArgs(11);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterProc(tdstrs_W,args,"tdstrs",nargs);
/*
 * Register tdprpt.
 */
        nargs = 0;
        args = NewArgs(1);

        dimsizes[0] = 3;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterFunc(tdprpt_W,args,"tdprpt",nargs);
/*
 * Register tdprpa.
 */
        nargs = 0;
        args = NewArgs(1);

        dimsizes[0] = 2;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterFunc(tdprpa_W,args,"tdprpa",nargs);
/*
 * Register tdprpi.
 */
        nargs = 0;
        args = NewArgs(1);

        dimsizes[0] = 2;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterFunc(tdprpi_W,args,"tdprpi",nargs);
/*
 * Register tdline.
 */
        nargs = 0;
        args = NewArgs(3);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
        dimsizes[0] = 3;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        NclRegisterProc(tdline_W,args,"tdline",nargs);
/*
 * Register tdlnpa.
 */
        nargs = 0;
        args = NewArgs(3);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
        dimsizes[0] = 2;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        NclRegisterProc(tdlnpa_W,args,"tdlnpa",nargs);
/*
 * Register tdgrds.
 */
        nargs = 0;
        args = NewArgs(6);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
        dimsizes[0] = 3;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        NclRegisterProc(tdgrds_W,args,"tdgrds",nargs);
/*
 * Register tdgrid.
 */
        nargs = 0;
        args = NewArgs(8);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        NclRegisterProc(tdgrid_W,args,"tdgrid",nargs);
/*
 * Register tdlbls.
 */
        nargs = 0;
        args = NewArgs(6);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
        dimsizes[0] = 3;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        NclRegisterProc(tdlbls_W,args,"tdlbls",nargs);
/*
 * Register tdlbla.
 */
        nargs = 0;
        args = NewArgs(7);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        dimsizes[0] = 2;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterProc(tdlbla_W,args,"tdlbla",nargs);
/*
 * Register tdplch.
 */
        nargs = 0;
        args = NewArgs(7);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterProc(tdplch_W,args,"tdplch",nargs);
/*
 * Register tdplch.
 */
        nargs = 0;
        args = NewArgs(7);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterProc(tdcurv_W,args,"tdcurv",nargs);
/*
 * Register tddtri.
 */
        nargs = 0;
        args = NewArgs(4);
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

        NclRegisterProc(tddtri_W,args,"tddtri",nargs);
/*
 * Register tdstri.
 */
        nargs = 0;
        args = NewArgs(6);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;

        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        NclRegisterProc(tdstri_W,args,"tdstri",nargs);
/*
 * Register tditri.
 */
        nargs = 0;
        args = NewArgs(8);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",3,NclANY);nargs++;

        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        NclRegisterProc(tditri_W,args,"tditri",nargs);

/*
 * Register tdmtri.
 */
        nargs = 0;
        args = NewArgs(8);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        dimsizes[0] = 3;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        dimsizes[0] = 3;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterProc(tdmtri_W,args,"tdmtri",nargs);
/*
 * Register tdttri.
 */
        nargs = 0;
        args = NewArgs(11);

        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        dimsizes[0] = 3;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterProc(tdttri_W,args,"tdttri",nargs);
/*
 * Register tdctri.
 */
        nargs = 0;
        args = NewArgs(4);

        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        NclRegisterProc(tdctri_W,args,"tdctri",nargs);
/*
 * Register tdotri.
 */
        nargs = 0;
        args = NewArgs(4);

        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        NclRegisterFunc(tdotri_W,args,"tdotri",nargs);
/*
 * Register tdsort.
 */
        nargs = 0;
        args = NewArgs(2);

        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

        NclRegisterFunc(tdsort_W,args,"tdsort",nargs);
/*
 *  Register wmsetp.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    SetArgTemplate(args, nargs, 0, 1, dimsizes);
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
 *  Register ngsetp.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    SetArgTemplate(args, nargs, 0, 1, dimsizes);
    nargs++;
    NclRegisterProc(ngsetp_W, args, "ngsetp", nargs);
/*
 *  Register nggetp.
 */
    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    NclRegisterFunc(nggetp_W, args, "nggetp", nargs);
/*
 * Register tdez1d.
 */
        nargs = 0;
        args = NewArgs(11);
        dimsizes[0] = 1;

        SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;

        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;

        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;

        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterProc(tdez1d_W,args,"tdez1d",nargs);
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

        NclRegisterProc(tdez3d_W,args,"tdez3d",nargs);
/*
 * Register wmbarb
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;

    SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    NclRegisterProc(wmbarb_W,args,"wmbarb",nargs);

/*
 * Register wmbarbmap
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;

    SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    NclRegisterProc(wmbarbmap_W,args,"wmbarbmap",nargs);

/*
 * Register wmvect
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;

    SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    NclRegisterProc(wmvect_W,args,"wmvect",nargs);

/*
 * Register wmvectmap
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;

    SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    NclRegisterProc(wmvectmap_W,args,"wmvectmap",nargs);

/*
 * Register wmvlbl
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;

    SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
    NclRegisterProc(wmvlbl_W,args,"wmvlbl",nargs);

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
 * Register wmstnm
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    
    SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    NclRegisterProc(wmstnm_W,args,"wmstnm",nargs);

/*
 *  Registering nglogo_W
 */
    nargs   =   0;
    args = NewArgs(7);
    dimsizes[0] = 1;

    SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterProc(nglogo_W,args,"nglogo",nargs);

/*
 *  Registering ngezlogo_W
 */
    nargs   =   0;
    args = NewArgs(1);
    dimsizes[0] = 1;

    SetArgTemplate(args,nargs,"graphic",1,dimsizes);nargs++;

    NclRegisterProc(ngezlogo_W,args,"ngezlogo",nargs);

/*
 *  Register nnsetp.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    SetArgTemplate(args, nargs, 0, 1, dimsizes);
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
 * Register nngetaspects.
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
 * Register nngetaspectd.
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
 * Register nngetslopes.
 */
        nargs = 0;
        args = NewArgs(2);
        SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterFunc(nngetslopes_W,args,"nngetslopes",nargs);
/*
 * Register nngetwts.
 */
        nargs = 0;
        args = NewArgs(6);
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        dimsizes[0] = 3;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"float",1,dimsizes);nargs++;
/*
 * Register wrapper procedure pointer and argument templates
 */
        NclRegisterProc(nngetwts_W,args,"nngetwts",nargs);
/*
 * Register nngetwtsd.
 */
        nargs = 0;
        args = NewArgs(6);
        dimsizes[0] = 1;
        SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        dimsizes[0] = 3;
        SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
/*
 * Register wrapper procedure pointer and argument templates
 */
        NclRegisterProc(nngetwtsd_W,args,"nngetwtsd",nargs);
/*
 * Register nngetsloped.
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
 * Register nnpntinits.
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
 * Register nnpntinitd.
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
 * Register nnpntinit.
 */
        nargs = 0;
        args = NewArgs(3);
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterProc(nnpntinit_W,args,"nnpntinit",nargs);
/*
 * Register nnpnts.
 */
        nargs = 0;
        args = NewArgs(2);
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;

/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterFunc(nnpnts_W,args,"nnpnts",nargs);
/*
 * Register nnpntd.
 */
        nargs = 0;
        args = NewArgs(2);
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterFunc(nnpntd_W,args,"nnpntd",nargs);
/*
 * Register nnpnt.
 */
        nargs = 0;
        args = NewArgs(2);
        SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
        SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
/*
 * Register wrapper function pointer and argument templates
 */
        NclRegisterFunc(nnpnt_W,args,"nnpnt",nargs);
/*
 * Register nnpntend.
 */
        args = NewArgs(0);
        nargs = 0;

        NclRegisterProc(nnpntend_W,args,"nnpntend",nargs);
/*
 * Register nnppntendd.
 */
        args = NewArgs(0);
        nargs = 0;
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
    SetArgTemplate(args, nargs, 0, 1, NclANY);
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
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    NclRegisterFunc(csc2s_W, args, "csc2s", nargs);

/*
 *  Register css2c.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    NclRegisterFunc(css2c_W, args, "css2c", nargs);

/*
 *  Register cssetp.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);
    nargs++;
    SetArgTemplate(args, nargs, 0, 1, dimsizes);
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
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
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
    SetArgTemplate(args, nargs, 0, 1, dimsizes); nargs++;
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
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    NclRegisterFunc(ftcurv_W, args, "ftcurv", nargs);

/*
 *  Register ftcurvd.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    NclRegisterFunc(ftcurvd_W, args, "ftcurvd", nargs);

/*
 *  Register ftcurvi.
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    NclRegisterFunc(ftcurvi_W, args, "ftcurvi", nargs);

/*
 *  Register ftcurvp.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    NclRegisterFunc(ftcurvp_W, args, "ftcurvp", nargs);

/*
 *  Register ftcurvpi.
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    NclRegisterFunc(ftcurvpi_W, args, "ftcurvpi", nargs);

/*
 *  Register ftcurvs.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    NclRegisterFunc(ftcurvs_W, args, "ftcurvs", nargs);

/*
 *  Register ftcurvps.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    NclRegisterFunc(ftcurvps_W, args, "ftcurvps", nargs);

/*
 *  Register ftkurv.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    NclRegisterProc(ftkurv_W, args, "ftkurv", nargs);

/*
 *  Register ftkurvp.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    NclRegisterProc(ftkurvp_W, args, "ftkurvp", nargs);

/*
 *  Register ftkurvd.
 */
    nargs = 0;
    args = NewArgs(9);
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    NclRegisterProc(ftkurvd_W, args, "ftkurvd", nargs);

/*
 *  Register ftkurvpd.
 */
    nargs = 0;
    args = NewArgs(9);
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    NclRegisterProc(ftkurvpd_W, args, "ftkurvpd", nargs);

/*
 *  Register ftsurf.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY); nargs++;
    NclRegisterFunc(ftsurf_W, args, "ftsurf", nargs);

/*
 *  Register csa1s.
 */
  nargs = 0;
  args = NewArgs(4);
  SetArgTemplate(args,nargs,"float",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",0,NclANY); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"float",1,NclANY); nargs++;
  NclRegisterFunc(csa1s_W,args,"csa1s",nargs);

/*
 *  Register csa1xs.
 */
  nargs = 0;
  args = NewArgs(7);
  SetArgTemplate(args,nargs,"float",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"float",0,NclANY); nargs++;
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
  SetArgTemplate(args,nargs,"float",0,NclANY); nargs++;
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
  SetArgTemplate(args,nargs,"float",0,NclANY); nargs++;
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
  SetArgTemplate(args,nargs,"float",0,NclANY); nargs++;

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
  SetArgTemplate(args,nargs,"float",0,NclANY); nargs++;
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
  SetArgTemplate(args,nargs,"float",0,NclANY); nargs++;

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
  SetArgTemplate(args,nargs,"float",0,NclANY); nargs++;
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
  SetArgTemplate(args,nargs,"float",0,NclANY); nargs++;

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
  SetArgTemplate(args,nargs,"float",0,NclANY); nargs++;
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
 *  Register csa1d.
 */
  nargs = 0;
  args = NewArgs(4);
  SetArgTemplate(args,nargs,"double",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",0,NclANY); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  NclRegisterFunc(csa1d_W,args,"csa1d",nargs);

/*
 *  Register csa1xd.
 */
  nargs = 0;
  args = NewArgs(7);
  SetArgTemplate(args,nargs,"double",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"double",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  NclRegisterFunc(csa1xd_W,args,"csa1xd",nargs);

/*
 *  Register csa2d.
 */
  nargs = 0;
  args = NewArgs(6);
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",0,NclANY); nargs++;
  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  NclRegisterFunc(csa2d_W,args,"csa2d",nargs);

/*
 *  Register csa2xd.
 */
  nargs = 0;
  args = NewArgs(9);
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;

  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"double",1,dimsizes); nargs++;
  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  NclRegisterFunc(csa2xd_W,args,"csa2xd",nargs);

/*
 *  Register csa2ld.
 */
  nargs = 0;
  args = NewArgs(6);
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",0,NclANY); nargs++;

  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  NclRegisterFunc(csa2ld_W,args,"csa2ld",nargs);

/*
 *  Register csa2lxd.
 */
  nargs = 0;
  args = NewArgs(9);
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;

  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"double",1,dimsizes); nargs++;
  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  NclRegisterFunc(csa2lxd_W,args,"csa2lxd",nargs);

/*
 *  Register csa3d.
 */
  nargs = 0;
  args = NewArgs(8);
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",0,NclANY); nargs++;

  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  NclRegisterFunc(csa3d_W,args,"csa3d",nargs);

/*
 *  Register csa3xd.
 */
  nargs = 0;
  args = NewArgs(11);
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;

  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"double",1,dimsizes); nargs++;
  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  NclRegisterFunc(csa3xd_W,args,"csa3xd",nargs);

/*
 *  Register csa3ld.
 */
  nargs = 0;
  args = NewArgs(8);
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",0,NclANY); nargs++;

  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  NclRegisterFunc(csa3ld_W,args,"csa3ld",nargs);

/*
 *  Register csa3lxd.
 */
  nargs = 0;
  args = NewArgs(11);
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;

  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"double",1,dimsizes); nargs++;
  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"double",1,NclANY); nargs++;
  NclRegisterFunc(csa3lxd_W,args,"csa3lxd",nargs);

/*
 *  Register csa1x.
 */
  nargs = 0;
  args = NewArgs(7);
  SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  NclRegisterFunc(csa1x_W,args,"csa1x",nargs);

/*
 *  Register csa1.
 */
  nargs = 0;
  args = NewArgs(4);
  SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  NclRegisterFunc(csa1_W,args,"csa1",nargs);

/*
 *  Register csa2.
 */
  nargs = 0;
  args = NewArgs(6);
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;
  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  NclRegisterFunc(csa2_W,args,"csa2",nargs);

/*
 *  Register csa2x.
 */
  nargs = 0;
  args = NewArgs(9);

  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;

  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"numeric",1,dimsizes); nargs++;
  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  NclRegisterFunc(csa2x_W,args,"csa2x",nargs);

/*
 *  Register csa2l.
 */
  nargs = 0;
  args = NewArgs(6);
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;

  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  NclRegisterFunc(csa2l_W,args,"csa2l",nargs);

/*
 *  Register csa2lx.
 */
  nargs = 0;
  args = NewArgs(9);
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;

  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"numeric",1,dimsizes); nargs++;
  dimsizes[0] = 2;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  NclRegisterFunc(csa2lx_W,args,"csa2lx",nargs);

/*
 *  Register csa3x.
 */
  nargs = 0;
  args = NewArgs(11);
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;

  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"numeric",1,dimsizes); nargs++;
  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  NclRegisterFunc(csa3x_W,args,"csa3x",nargs);

/*
 *  Register csa3.
 */
  nargs = 0;
  args = NewArgs(8);
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;

  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  NclRegisterFunc(csa3_W,args,"csa3",nargs);

/*
 *  Register csa3lx.
 */
  nargs = 0;
  args = NewArgs(11);
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;

  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;
  dimsizes[0] = 1;
  SetArgTemplate(args,nargs,"numeric",1,dimsizes); nargs++;
  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  NclRegisterFunc(csa3lx_W,args,"csa3lx",nargs);

/*
 *  Register csa3l.
 */
  nargs = 0;
  args = NewArgs(8);
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;

  dimsizes[0] = 3;
  SetArgTemplate(args,nargs,"integer",1,dimsizes); nargs++;

  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  SetArgTemplate(args,nargs,"numeric",1,NclANY); nargs++;
  NclRegisterFunc(csa3l_W,args,"csa3l",nargs);

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
        SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
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
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(dsgrid2d_W,args,"dsgrid2d",nargs);
/*
 * Register "dsgrid2".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(5);
/*
 * Configure five parameters identically as single dimension double
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(dsgrid2_W,args,"dsgrid2",nargs);
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
        SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
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
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(dsgrid3d_W,args,"dsgrid3d",nargs);
/*
 * Register "dsgrid3".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(7);
/*
 * Configure five parameters identically as single dimension float
 * arrays of any size.
 */
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(dsgrid3_W,args,"dsgrid3",nargs);
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
        SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
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
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterProc(dspnt2d_W,args,"dspnt2d",nargs);
/*
 * Register "dspnt2".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(5);

        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(dspnt2_W,args,"dspnt2",nargs);
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
        SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"float",0,NclANY);nargs++;
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
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterProc(dspnt3d_W,args,"dspnt3d",nargs);

/*
 * Register "dspnt3".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(7);

        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
/*
 * Register wrapper function pointer and argument templates.
 */
        NclRegisterFunc(dspnt3_W,args,"dspnt3",nargs);


/*
 *  Register dssetp.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args, nargs, "string", 1, NclANY);
    nargs++;
    SetArgTemplate(args, nargs, 0, 1, NclANY);
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
 * Register "cancor".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(cancor_W,args,"cancor",nargs);
/*
 * Register "regcoef".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;

    NclRegisterFunc(regcoef_W,args,"regcoef",nargs);
/*
 * Register "regCoef".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(regCoef_W,args,"regCoef",nargs);
/*
 * Register "regCoef_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(regCoef_n_W,args,"regCoef_n",nargs);
/*
 * Register "regCoef_shields".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(regCoef_shields_W,args,"regCoef_shields",nargs);
/*
 * Register "regline".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",(int) 1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",(int) 1,NclANY);nargs++;

    NclRegisterFunc(regline_W,args,"regline",nargs);
/*
 * Register "reg_multlin".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(reg_multlin_W,args,"reg_multlin",nargs);
/*
 * Register "stat2".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;

    NclRegisterProc(stat2_W,args,"stat2",nargs);
/*
 * Register "stat_trim".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;

    NclRegisterProc(stat_trim_W,args,"stat_trim",nargs);
/*
 * Register "stat4".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;

    NclRegisterProc(stat4_W,args,"stat4",nargs);
/*
 * Register "dim_stat4".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(dim_stat4_W,args,"dim_stat4",nargs);
/*
 * Register "dim_stat4_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(dim_stat4_n_W,args,"dim_stat4_n",nargs);
/*
 * Register "stat_medrng".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;

    NclRegisterProc(stat_medrng_W,args,"stat_medrng",nargs);
/*
 * Register "dim_median".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(dim_median_W,args,"dim_median",nargs);
/*
 * Register "dim_median_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(dim_median_n_W,args,"dim_median_n",nargs);
/*
 * Register "dim_rmvmean".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(dim_rmvmean_W,args,"dim_rmvmean",nargs);
/*
 * Register "dim_rmvmean_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(dim_rmvmean_n_W,args,"dim_rmvmean_n",nargs);
/*
 * Register "dim_rmvmed".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(dim_rmvmed_W,args,"dim_rmvmed",nargs);
/*
 * Register "dim_rmvmed_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(dim_rmvmed_n_W,args,"dim_rmvmed_n",nargs);
/*
 * Register "dim_standardize".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(dim_standardize_W,args,"dim_standardize",nargs);

/*
 * Register "dim_standardize_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(dim_standardize_n_W,args,"dim_standardize_n",nargs);
/*
 * Register "dim_rmsd".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(dim_rmsd_W,args,"dim_rmsd",nargs);
/*
 * Register "dim_rmsd_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(dim_rmsd_n_W,args,"dim_rmsd_n",nargs);
/*
 * Register "dim_pqsort".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(dim_pqsort_W,args,"dim_pqsort",nargs);
/*
 * Register "dim_pqsort_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(dim_pqsort_n_W,args,"dim_pqsort_n",nargs);
/*
 * Register "dim_num".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",0,NclANY);nargs++;

    NclRegisterFunc(dim_num_W,args,"dim_num",nargs);
/*
 * Register "dim_num_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"logical",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(dim_num_n_W,args,"dim_num_n",nargs);

/*
 * Register "dim_numrun_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(dim_numrun_n_W,args,"dim_numrun_n",nargs);
/*
 * Register "dim_avg_wgt".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(dim_avg_wgt_W,args,"dim_avg_wgt",nargs);
/*
 * Register "dim_avg_wgt_n".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(dim_avg_wgt_n_W,args,"dim_avg_wgt_n",nargs);
/*
 * Register "dim_sum_wgt".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(dim_sum_wgt_W,args,"dim_sum_wgt",nargs);
/*
 * Register "dim_sum_wgt_n".
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(dim_sum_wgt_n_W,args,"dim_sum_wgt_n",nargs);

/*
 * Register "dim_gamfit_n".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(dim_gamfit_n_W,args,"dim_gamfit_n",nargs);

/*
 * Register "dim_sum_wgt_n".
 */
    nargs = 0;
    args = NewArgs(4);

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(dim_acumrun_n_W,args,"dim_acumrun_n",nargs);

/*
 * Register "dim_spi_n".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(dim_spi_n_W,args,"dim_spi_n",nargs);

/*
 * Register "esacr".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;

    NclRegisterFunc(esacr_W,args,"esacr",nargs);
/*
 * Register "esacv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;

    NclRegisterFunc(esacv_W,args,"esacv",nargs);
/*
 * Register "esccr".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;

    NclRegisterFunc(esccr_W,args,"esccr",nargs);
/*
 * Register "esccr_shields".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;

    NclRegisterFunc(esccr_shields_W,args,"esccr_shields",nargs);
/*
 * Register "esccv".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;

    NclRegisterFunc(esccv_W,args,"esccv",nargs);
/*
 * Register "escorc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(escorc_W,args,"escorc",nargs);
/*
 * Register "escorc_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(escorc_n_W,args,"escorc_n",nargs);
/*
 * Register "escovc".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(escovc_W,args,"escovc",nargs);
/*
 * Register "ezfftf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(ezfftf_W,args,"ezfftf",nargs);
/*
 * Register "ezfftb".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(ezfftb_W,args,"ezfftb",nargs);

/*
 * Register "ezfftf_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(ezfftf_n_W,args,"ezfftf_n",nargs);

/*
 * Register "ezfftb_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(ezfftb_n_W,args,"ezfftb_n",nargs);

/*
 * Register "cfftf".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(cfftf_W,args,"cfftf",nargs);

/*
 * Register "cfftb".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(cfftb_W,args,"cfftb",nargs);

/*
 * Register "cfftf_frq_reorder".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(cfftf_frq_reorder_W,args,"cfftf_frq_reorder",nargs);

/*
 * Register "fft2df".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(1);

    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;

    NclRegisterFunc(fft2df_W,args,"fft2df",nargs);

/*
 * Register "fft2db".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(1);

    SetArgTemplate(args,nargs,"numeric",3,NclANY);nargs++;

    NclRegisterFunc(fft2db_W,args,"fft2db",nargs);

/*
 * Register "lspoly_old".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(lspoly_old_W,args,"lspoly_old",nargs);

/*
 * Register "lspoly".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(lspoly_W,args,"lspoly",nargs);

/*
 * Register "lspoly_n".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(lspoly_n_W,args,"lspoly_n",nargs);

/*
 * Register "lspoly_n_old".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(lspoly_n_old_W,args,"lspoly_n_old",nargs);
/*
 * Register "fourier_info".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(fourier_info_W,args,"fourier_info",nargs);
/*
 * Register "stdatmus_z2tdp".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(stdatmus_z2tdp_W,args,"stdatmus_z2tdp",nargs);
/*
 * Register "stdatmus_p2tdz".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(stdatmus_p2tdz_W,args,"stdatmus_p2tdz",nargs);
/*
 * Register "rdsstoi".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    dimsizes[0] = 9;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(rdsstoi_W,args,"rdsstoi",nargs);

/*
 * Register "covcorm".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 2;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(covcorm_W,args,"covcorm",nargs);

/*
 * Register "covcorm_xy".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    dimsizes[0] = 3;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(covcorm_xy_W,args,"covcorm_xy",nargs);

/*
 * Register "vibeta".
 *
 * Create private argument array.
 */
    nargs = 0;
    args = NewArgs(6);

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(prcwater_dp_W,args,"prcwater_dp",nargs);
/*
 * Register "int2p".
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(int2p_W,args,"int2p",nargs);
/*
 * Register "int2p_n".
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(int2p_n_W,args,"int2p_n",nargs);
/*
 * Register "zonal_mpsi".
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(zonal_mpsi_W,args,"zonal_mpsi",nargs);
/*
 * Register "taper".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(taper_W,args,"taper",nargs);
/*
 * Register "taper_n".
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(taper_n_W,args,"taper_n",nargs);
/*
 * Register "hydro".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(hydro_W,args,"hydro",nargs);

/*
 * Register "mixhum_ptrh".
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(mixhum_ptrh_W,args,"mixhum_ptrh",nargs);

/*
 * Register "mixhum_ptd".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(mixhum_ptd_W,args,"mixhum_ptd",nargs);

/*
 * Register "dewtemp_trh".
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(dewtemp_trh_W,args,"dewtemp_trh",nargs);

/*
 * Register "lclvl".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(lclvl_W,args,"lclvl",nargs);

/*
 * Register "linmsg".
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    NclRegisterFunc(linmsg_W,args,"linmsg",nargs);

/*
 * Register "linmsg_n".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(linmsg_n_W,args,"linmsg_n",nargs);

/*
 * Register "linint1".
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(linint1_W,args,"linint1",nargs);

/*
 * Register "linint1_n".
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(linint1_n_W,args,"linint1_n",nargs);

/*
 * Register "linint2".
 */
    nargs = 0;
    args = NewArgs(7);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(linint2_points_W,args,"linint2_points",nargs);

/*
 * Register "area_hi2lores".
 */
    nargs = 0;
    args = NewArgs(8);

    dimsizes[0] = 1;

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(area_hi2lores_W,args,"area_hi2lores",nargs);

/*
 * Register "area_conserve_remap".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(6);

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(area_conserve_remap_W,args,"area_conserve_remap",nargs);

/*
 * Register "rcm2rgrid".
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    NclRegisterFunc(rcm2rgrid_W,args,"rcm2rgrid",nargs);

/*
 * Register "rgrid2rcm".
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    NclRegisterFunc(rgrid2rcm_W,args,"rgrid2rcm",nargs);

/*
 * Register "rcm2points".
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    NclRegisterFunc(rcm2points_W,args,"rcm2points",nargs);

/*
 * Register "pres_hybrid".
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(pres_hybrid_ccm_W,args,"pres_hybrid_ccm",nargs);

/*
 * Register "pres_hybrid_jra55".
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(pres_hybrid_jra55_W,args,"pres_hybrid_jra55",nargs);

/*
 * Register "dpres_hybrid".
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(dpres_hybrid_ccm_W,args,"dpres_hybrid_ccm",nargs);

/*
 * Register "dpres_plevel".
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;

    NclRegisterFunc(dpres_plevel_W,args,"dpres_plevel",nargs);

/*
 * Register "pres2hybrid".
 */
    nargs = 0;
    args = NewArgs(7);

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(pres2hybrid_W,args,"pres2hybrid",nargs);


/*
 * Register "hyi2hyo".
 */
    nargs = 0;
    args = NewArgs(8);
    dimsizes[0] = 1;

    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(hyi2hyo_W,args,"hyi2hyo",nargs);
/*
 * Register "pres_sigma".
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(pres_sigma_W,args,"pres_sigma",nargs);

/*
 * Register "sigma2hybrid".
 */
    nargs = 0;
    args = NewArgs(7);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(sigma2hybrid_W,args,"sigma2hybrid",nargs);

/*
 * Register "pslhyp".
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(pslhyp_W,args,"pslhyp",nargs);

/*
 * Register "pslec".
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(pslec_W,args,"pslec",nargs);

/*
 * Register "pslhor".
 */
    nargs = 0;
    args = NewArgs(6);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;

    NclRegisterFunc(pslhor_W,args,"pslhor",nargs);

/*
 * Register "dz_height".
 */
    nargs = 0;
    args = NewArgs(4);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;

    NclRegisterFunc(dz_height_W,args,"dz_height",nargs);

/*
 * Register "gc_latlon".
 */
    nargs = 0;
    args = NewArgs(6);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(gc_latlon_W,args,"gc_latlon",nargs);
/*
 * Register "testspan".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(3);

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;

    NclRegisterFunc(testspan_W,args,"testspan",nargs);
/*
 * Register "monthday".
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    NclRegisterFunc(monthday_W,args,"monthday",nargs);

/*
 * Register "isleapyear".
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    NclRegisterFunc(isleapyear_W,args,"isleapyear",nargs);
/*
 * Register "days_in_month".
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    NclRegisterFunc(days_in_month_W,args,"days_in_month",nargs);

/*
 * Register "day_of_week".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    NclRegisterFunc(day_of_week_W,args,"day_of_week",nargs);

/*
 * Register "day_of_year".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    NclRegisterFunc(day_of_year_W,args,"day_of_year",nargs);

/*
 * Register "greg2jul".
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    NclRegisterFunc(greg2jul_W,args,"greg2jul",nargs);

/*
 * Register "jul2greg".
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(jul2greg_W,args,"jul2greg",nargs);

/*
 * Register "cd_calendar".
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(cd_calendar_W,args,"cd_calendar",nargs);

/*
 * Register "cd_inv_calendar".
 */
    nargs = 0;
    args = NewArgs(8);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(cd_inv_calendar_W,args,"cd_inv_calendar",nargs);

#ifdef BuildUdunits
/*
 * Register "ut_calendar"
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(ut_calendar_W,args,"ut_calendar",nargs);

/*
 * Register "ut_calendar_fix"
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(ut_calendar_fix_W,args,"ut_calendar_fix",nargs);

/*
 * Register "ut_inv_calendar".
 */
    nargs = 0;
    args = NewArgs(8);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(ut_inv_calendar_W,args,"ut_inv_calendar",nargs);

/*
 * Register "ut_inv_calendar_fix".
 */
    nargs = 0;
    args = NewArgs(8);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(ut_inv_calendar_fix_W,args,"ut_inv_calendar_fix",nargs);
#endif

/*
 * Register "utm2latlon" and "latlon2utm".
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(utm2latlon_W,args,"utm2latlon",nargs);

    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(latlon2utm_W,args,"latlon2utm",nargs);

/*
 * Register "angmom_atm".
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    NclRegisterFunc(angmom_atm_W,args,"angmom_atm",nargs);

/*
 * Register "relhum".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(relhum_W,args,"relhum",nargs);

/*
 * Register "relhum_water".
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    
    NclRegisterFunc(relhum_water_W,args,"relhum_water",nargs);

/*
 * Register "relhum_ice".
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    
    NclRegisterFunc(relhum_ice_W,args,"relhum_ice",nargs);

/*
 * Register "runave".
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(runave_W,args,"runave",nargs);

/*
 * Register "runave_n".
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(runave_n_W,args,"runave_n",nargs);

/*
 * Register "wgt_runave".
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_runave_W,args,"wgt_runave",nargs);

/*
 * Register "wgt_runave_n".
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_runave_n_W,args,"wgt_runave_n",nargs);

/*
 * Register "wgt_areaave".
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_areaave_W,args,"wgt_areaave",nargs);

/*
 * Register "wgt_areaave2".
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_areaave2_W,args,"wgt_areaave2",nargs);

/*
 * Register "wgt_areasum2".
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_areasum2_W,args,"wgt_areasum2",nargs);

/*
 * Register "wgt_arearmse".
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_arearmse_W,args,"wgt_arearmse",nargs);

/*
 * Register "wgt_arearmse2".
 */
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_arearmse2_W,args,"wgt_arearmse2",nargs);

/*
 * Register "wgt_volave".
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_volave_ccm_W,args,"wgt_volave_ccm",nargs);

/*
 * Register "wgt_volrmse".
 */
    nargs = 0;
    args = NewArgs(6);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(wgt_volrmse_ccm_W,args,"wgt_volrmse_ccm",nargs);

/*
 * Register "filwgts_lancos" and "filwgts_lanczos". These routines
 * are identical. One is the misspelled, original version (filwgts_lancos).
 */
    nargs = 0;
    args = NewArgs(5);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    NclRegisterFunc(filwgts_lanczos_W,args,"filwgts_lanczos",nargs);
    NclRegisterFunc(filwgts_lanczos_W,args,"filwgts_lancos",nargs);

/*
 * Register "filwgts_normal".
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(filwgts_normal_W,args,"filwgts_normal",nargs);

/*
 * Register "dtrend".
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    NclRegisterFunc(dtrend_W,args,"dtrend",nargs);

/*
 * Register "dtrend_n".
 */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(dtrend_n_W,args,"dtrend_n",nargs);

/*
 * Register "dtrend_quadratic".
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(dtrend_quadratic_W,args,"dtrend_quadratic",nargs);

/*
 * Register "dtrend_msg".
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    NclRegisterFunc(dtrend_msg_W,args,"dtrend_msg",nargs);

/*
 * Register "dtrend_msg_n".
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(dtrend_msg_n_W,args,"dtrend_msg_n",nargs);

/*
 * Register "dtrend_msg_n".
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    NclRegisterFunc(dtrend_quadratic_msg_n_W,args,"dtrend_quadratic_msg_n",nargs);

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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(fluxEddy_W,args,"fluxEddy",nargs);

/*
 * Register "cz2ccm".
 */
    nargs = 0;
    args = NewArgs(8);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
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
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(chiinv_W,args,"chiinv",nargs);
    NclRegisterFunc(chiinv_W,args,"cdfchi_x",nargs);

/*
 * Register "betainc".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(betainc_W,args,"betainc",nargs);

/*
 * Register "gammainc".
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    NclRegisterFunc(gammainc_W,args,"gammainc",nargs);

/*
 * Register "student_t".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(student_t_W,args,"student_t",nargs);
/*
 * Register "ttest".
 */
    nargs = 0;
    args = NewArgs(8);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"logical",1,dimsizes);nargs++;

    NclRegisterFunc(ttest_W,args,"ttest",nargs);
/*
 * Register "ftest".
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(ftest_W,args,"ftest",nargs);

/*
 * Register "rtest".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(rtest_W,args,"rtest",nargs);

/*
 * Register "equiv_sample_size".
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"numeric",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterFunc(equiv_sample_size_W,args,"equiv_sample_size",nargs);

/*
 * Register "z2geouv".
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;

    NclRegisterFunc(z2geouv_W,args,"z2geouv",nargs);

/*
 *  Register NhlGetNamedColorIndex.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args, nargs, "graphic", 0, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "string", 0, NclANY);   nargs++;
    NclRegisterFunc(NhlGetNamedColorIndex_W, args, "NhlGetNamedColorIndex", nargs);

/*
 *  Register rgba_to_color_index.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args, nargs, "float", 0, NclANY);  nargs++;
    NclRegisterFunc(rgba_to_color_index_W, args, "rgba_to_color_index", nargs);

/*
 *  Register color_index_to_rgba.
 */

    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args, nargs, "integer", 1, NclANY);  nargs++;
    NclRegisterFunc(color_index_to_rgba_W, args, "color_index_to_rgba", nargs);

/*
 *  Register output_gif.
 */
    nargs = 0;
    args = NewArgs(4);
    SetArgTemplate(args, nargs, "graphic", 0, NclANY);  nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);   nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);   nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);   nargs++;
    NclRegisterProc(output_gif_W, args, "output_gif", nargs);

/*
 *  Register attcreate.
 */
/*
    nargs = 0;
    args = NewArgs(4);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);   nargs++;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);   nargs++;
    SetArgTemplate(args, nargs, "graphic", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "logical", 1, dimsizes);  nargs++;
    NclRegisterFunc(attcreate_W, args, "attcreate", nargs);
*/
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
    /* 'snumeric' allows ushort, ubyte, etc. */
    SetArgTemplate(args, nargs, "snumeric", 0, NclANY);  nargs++;

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
    /* 'snumeric' allows ushort, ubyte, etc. */
    SetArgTemplate(args, nargs, "snumeric", 0, NclANY);  nargs++;
    NclRegisterFunc(getbitsone_W, args, "getbitsone", nargs);

/*
 *  Register conform.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args, nargs, 0, 0, NclANY);  nargs++;
    SetArgTemplate(args, nargs, 0, 0, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, NclANY);  nargs++;
    NclRegisterFunc(conform_W, args, "conform", nargs);
/*
 *  Register conform_dims.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, 0, 0, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, NclANY);  nargs++;
    NclRegisterFunc(conform_dims_W, args, "conform_dims", nargs);

/*
 *  Register reshape
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args, nargs, 0, 0, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    NclRegisterFunc(reshape_W, args, "reshape", nargs);

/*
 *  Register fftshift
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args, nargs, "numeric", 0, NclANY);  nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    NclRegisterFunc(fftshift_W, args, "fftshift", nargs);

/*
 *  Register reshape_ind
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args, nargs, 0, 0, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    NclRegisterFunc(reshape_ind_W, args, "reshape_ind", nargs);

/*
 *  Register omega_ccm.
 */
    nargs = 0;
    args = NewArgs(11);
    SetArgTemplate(args, nargs, "numeric", 0, NclANY);    nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY);    nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY);    nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY);    nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY);    nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY);    nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY);    nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY);    nargs++;

    SetArgTemplate(args, nargs, "numeric", 1, NclANY);    nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);    nargs++;

    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;

    NclRegisterFunc(omega_ccm_W, args, "omega_ccm", nargs);

/*
 *  Register wavelet.
 */
    nargs = 0;
    args = NewArgs(12);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);    nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);    nargs++;

    NclRegisterFunc(wavelet_W, args, "wavelet", nargs);

/*
 *  Register wavelet_default.
 */
    nargs = 0;
    args = NewArgs(2);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);    nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;

    NclRegisterFunc(wavelet_default_W, args, "wavelet_default", nargs);

/*
 *  Register grid2triple.
 */
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 2, NclANY);  nargs++;

    NclRegisterFunc(grid2triple_W, args, "grid2triple", nargs);

/*
 *  Register triple2grid.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "logical", 1, dimsizes);  nargs++;

    NclRegisterFunc(triple2grid_W, args, "triple2grid", nargs);

/*
 *  Register triple2grid2d.
 */
    nargs = 0;
    args = NewArgs(6);
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 2, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 2, NclANY);  nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "logical", 1, dimsizes);  nargs++;

    NclRegisterFunc(triple2grid2d_W, args, "triple2grid2d", nargs);

/*
 * Register "obj_anal_ic".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(7);

    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "logical", 1, dimsizes);  nargs++;

    NclRegisterFunc(obj_anal_ic_W,args,"obj_anal_ic",nargs);
/*
 *  Register paleo_outline.
 */
    nargs = 0;
    args = NewArgs(5);
    SetArgTemplate(args, nargs, "numeric", 2, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "float", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "string", 1, NclANY);  nargs++;
    NclRegisterProc(paleo_outline_W, args, "paleo_outline", nargs);

/*
 *  Register inverse_matrix.
 */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args, nargs, "numeric", 2, NclANY);  nargs++;
    NclRegisterFunc(inverse_matrix_W, args, "inverse_matrix", nargs);

/*
 *  Register solve_linsys.
 */
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args, nargs, "numeric", 2, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 0, NclANY);  nargs++;
    NclRegisterFunc(solve_linsys_W, args, "solve_linsys", nargs);

/*
 *  Register random_setallseed.
 */
    nargs = 0;
    args = NewArgs(2);

    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;

    NclRegisterProc(random_setallseed_W, args, "random_setallseed", nargs);

/*
 *  Register random_chi.
 */
    nargs = 0;
    args = NewArgs(2);

    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;

    NclRegisterFunc(random_chi_W, args, "random_chi", nargs);

/*
 *  Register random_gamma.
 */
    nargs = 0;
    args = NewArgs(3);

    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;

    NclRegisterFunc(random_gamma_W, args, "random_gamma", nargs);

/*
 *  Register random_normal.
 */
    nargs = 0;
    args = NewArgs(3);

    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;

    NclRegisterFunc(random_normal_W, args, "random_normal", nargs);

/*
 *  Register random_uniform.
 */
    nargs = 0;
    args = NewArgs(3);

    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;

    NclRegisterFunc(random_uniform_W, args, "random_uniform", nargs);

/*
 *  Register round.
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args, nargs, "numeric", 0, NclANY);  nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;

    NclRegisterFunc(round_W, args, "round", nargs);

/*
 *  Register generate_2d_array.
 */
    nargs = 0;
    args = NewArgs(6);

    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    dimsizes[0] = 2;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;

    NclRegisterFunc(generate_2d_array_W, args, "generate_2d_array", nargs);

/*
 *  Register isnan_ieee.
 */
    nargs = 0;
    args = NewArgs(1);

    SetArgTemplate(args, nargs, "numeric", 0, NclANY);  nargs++;
    NclRegisterFunc(isnan_ieee_W, args, "isnan_ieee", nargs);

/*
 *  Register replace_ieeenan.
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args, nargs, "numeric", 0, NclANY);  nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    NclRegisterProc(replace_ieeenan_W, args, "replace_ieeenan", nargs);

/*
 *  Registering cdfbin_p.
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(cdfbin_p_W,args,"cdfbin_p",nargs);

/*
 *  Registering cdfbin_x.
*/
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(cdfbin_s_W,args,"cdfbin_s",nargs);

/*
 *  Registering cdfbin_xn.
*/
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(cdfbin_xn_W,args,"cdfbin_xn",nargs);

/*
 *  Registering cdfbin_pr.
*/
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(cdfbin_pr_W,args,"cdfbin_pr",nargs);

/*
 *  Registering cdfgam_p.
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(cdfgam_p_W,args,"cdfgam_p",nargs);

/*
 *  Registering cdfgam_x.
*/
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(cdfgam_x_W,args,"cdfgam_x",nargs);

/*
 *  Registering cdfnor_p.
*/
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(cdfnor_p_W,args,"cdfnor_p",nargs);

/*
 *  Registering cdfnor_x.
 */
    nargs = 0;
    args = NewArgs(3);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(cdfnor_x_W,args,"cdfnor_x",nargs);

/*
 *  Registering cdfchi_p.
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(cdfchi_p_W,args,"cdfchi_p",nargs);

/*
 *  Registering cdft_t_W.
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(cdft_t_W,args,"cdft_t",nargs);

/*
 *  Registering cdft_p_W.
 */
    nargs = 0;
    args = NewArgs(2);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(cdft_p_W,args,"cdft_p",nargs);

/*
 *  Registering gamma
 */
    nargs = 0;
    args = NewArgs(1);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(gamma_W,args,"gamma",nargs);

/*
 *  Register ind_resolve.
 */
    nargs = 0;
    args = NewArgs(2);

    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;

    NclRegisterFunc(ind_resolve_W, args, "ind_resolve", nargs);

/*
 *  Register unique_string.
 */
    nargs = 0;
    args = NewArgs(1);

    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);  nargs++;
    NclRegisterFunc(unique_string_W, args, "unique_string", nargs);

/*
 *  Register tempnam.
 */
    nargs = 0;
    args = NewArgs(2);

    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);  nargs++;

    NclRegisterFunc(tempnam_W, args, "tempnam", nargs);

/*
 *  Register get_ncl_version.
 */
    nargs = 0;
    args = NewArgs(0);
    NclRegisterFunc(get_ncl_version_W, args, "get_ncl_version", nargs);

/*
 *  Register echo_on and echo_off
 */
    nargs = 0;
    args = NewArgs(0);
    NclRegisterProc(echo_on_W, args, "echo_on", nargs);

    nargs = 0;
    args = NewArgs(0);
    NclRegisterProc(echo_off_W, args, "echo_off", nargs);

/*
 * Register "erf".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(1);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;

    NclRegisterFunc(erf_W,args,"erf",nargs);

/*
 * Register "erfc".
 *
 * Create private argument array
 */
    nargs = 0;
    args = NewArgs(1);

    SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
    
    NclRegisterFunc(erfc_W,args,"erfc",nargs);

/*
 * Register "kron_product".
 *
 * Create private argument array
 */
        nargs = 0;
        args = NewArgs(2);

        SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",2,NclANY);nargs++;

        NclRegisterFunc(kron_product_W,args,"kron_product",nargs);

/*
 * Register "sparse_matrix_mult".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(5);
        SetArgTemplate(args,0,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,1,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,2,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,3,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
        NclRegisterFunc(sparse_matrix_mult_W,args,"sparse_matrix_mult",nargs);

/*
 * Register "sparse_matrix_mult_trimesh".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(6);
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",1,NclANY);nargs++;
        SetArgTemplate(args,nargs,"numeric",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"integer",2,NclANY);nargs++;
        SetArgTemplate(args, nargs, "numeric", 1, NclANY);  nargs++;
        NclRegisterFunc(sparse_matrix_mult_trimesh_W,args,"sparse_matrix_mult_trimesh",nargs);

/*
 *  Register ctwrap.
 */
    nargs = 0;
    args = NewArgs(5);

    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "graphic", 1, dimsizes);  nargs++;
    SetArgTemplate(args, nargs, "float", 2, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "float", 2, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "float", 2, NclANY);  nargs++;
    SetArgTemplate(args, nargs, "logical", 1, dimsizes);  nargs++;

    NclRegisterProc(ctwrap_W, args, "ctwrap", nargs);

/*
 * Register "write_matrix"
 *
 * There are five functions associated with write_matrix() -- see the
 * source file $NCARG_ROOT/ni/src/lib/nfp/writeMatrix.f
 *
 * Supported numeric data types:
 *      byte data (type: NCL_byte)
 *      short integer data (type: NCL_short)
 *      integer data (type: NCL_short, NCL_int, NCL_long)
 *      floating point data (type: NCL_float)
 *      double precision data (type: NCL_double)
 */
    nargs = 0;
    args = NewArgs(3);

    /* Incoming data may only be 2D, dimensioned to any size */
    SetArgTemplate(args, nargs, "numeric", 2, NclANY); nargs++;

    /* Data format is a string scalar */
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "string", 1, dimsizes);  nargs++;

    /* Determine if further arguments are provided. */
    SetArgTemplate(args, nargs, "logical", 1, NULL);  nargs++;

    NclRegisterProc(write_matrix_W, args, "write_matrix", nargs);

#ifdef BuildGDAL    
/*
 * Register "directVincenty".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(8);
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"double",0,NclANY);nargs++;
        dimsizes[0]=1;
        SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
        NclRegisterProc(directVincenty_W,args,"directVincenty",nargs);

/*
 * Register "rgba2png".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(8);
        dimsizes[0]=1;
        SetArgTemplate(args,nargs,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"uint",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"uint",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"uint",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"uint",0,NclANY);nargs++;
        SetArgTemplate(args,nargs,"uint",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"uint",1,dimsizes);nargs++;
        SetArgTemplate(args,nargs,"uint",1,dimsizes);nargs++;
        NclRegisterProc(rgba2png_W,args,"rgba2png",nargs);     

/*
 * Register "add_NCL_KML_Arrow".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(18);
        dimsizes[0]=1;
        SetArgTemplate(args,0,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,1,"uint",1,dimsizes);nargs++;
        SetArgTemplate(args,2,"uint",1,dimsizes);nargs++;
        SetArgTemplate(args,3,"float",2,NclANY);nargs++;
        SetArgTemplate(args,4,"float",2,NclANY);nargs++;
        SetArgTemplate(args,5,"float",2,NclANY);nargs++;
        SetArgTemplate(args,6,"float",2,NclANY);nargs++;
        SetArgTemplate(args,7,"float",2,NclANY);nargs++;
        SetArgTemplate(args,8,"uint",2,NclANY);nargs++;
        SetArgTemplate(args,9,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,10,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,11,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,12,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,13,"float",1,dimsizes);nargs++;
        SetArgTemplate(args,14,"uint",1,dimsizes);nargs++;
        SetArgTemplate(args,15,"logical",1,dimsizes);nargs++;
        SetArgTemplate(args,16,"logical",1,dimsizes);nargs++;
        SetArgTemplate(args,17,"logical",1,dimsizes);nargs++;

        NclRegisterProc(add_NCL_KML_Arrow_W,args,"add_NCL_KML_Arrow",nargs);  


/*
 * Register "add_NCL_KML_2DGrid_W".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(9);
        dimsizes[0]=1;
        SetArgTemplate(args,0,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,1,"float",2,NclANY);nargs++;
        SetArgTemplate(args,2,"float",2,NclANY);nargs++;
        SetArgTemplate(args,3,"float",2,NclANY);nargs++;
        SetArgTemplate(args,4,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,5,"uint",1,dimsizes);nargs++;
        SetArgTemplate(args,6,"logical",1,dimsizes);nargs++;
        SetArgTemplate(args,7,"logical",1,dimsizes);nargs++;
        SetArgTemplate(args,8,"logical",1,dimsizes);nargs++;

        NclRegisterProc(add_NCL_KML_2DGrid_W,args,"add_NCL_KML_2DGrid",nargs);


/*
 * Register "add_NCL_KML_UnstructGrid_W".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(10);
        dimsizes[0]=1;
        SetArgTemplate(args,0,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,1,"uint",2,NclANY);nargs++;
        SetArgTemplate(args,2,"float",1,NclANY);nargs++;
        SetArgTemplate(args,3,"float",1,NclANY);nargs++;
        SetArgTemplate(args,4,"float",1,NclANY);nargs++;
        SetArgTemplate(args,5,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,6,"uint",1,dimsizes);nargs++;
        SetArgTemplate(args,7,"logical",1,dimsizes);nargs++;
        SetArgTemplate(args,8,"logical",1,dimsizes);nargs++;
        SetArgTemplate(args,9,"logical",1,dimsizes);nargs++;

        NclRegisterProc(add_NCL_KML_UnstructGrid_W,args,"add_NCL_KML_UnstructGrid",nargs);


/*
 * Register "TransformCoordinate_W".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(5);
        dimsizes[0]=1;
        SetArgTemplate(args,0,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,0,"string",1,dimsizes);nargs++;
        SetArgTemplate(args,2,"double",0,NclANY);nargs++;
        SetArgTemplate(args,3,"double",0,NclANY);nargs++;
        SetArgTemplate(args,4,"double",0,NclANY);nargs++;

        NclRegisterProc(TransformCoordinate_W,args,"transform_coordinate",nargs);

#endif
        
/* 
 * ESMF regridding functions.
 */
#ifdef BuildESMF
/*
 * Register "SMMul".
 *
 * Create private argument array.
 */
 
        nargs = 0;
        args = NewArgs(5);
        SetArgTemplate(args,0,"integer",1,NclANY);nargs++;
        SetArgTemplate(args,1,"integer",1,NclANY);nargs++;
        SetArgTemplate(args,2,"double",1,NclANY);nargs++;
        SetArgTemplate(args,3,"double",2,NclANY);nargs++;
        SetArgTemplate(args,4,"double",2,NclANY);nargs++;
        NclRegisterProc(SMMul_W,args,"SMMulFast",nargs);
/*
 * Register "Unstruct2KML".
 *
 * Create private argument array.
 */
       nargs = 0;
       args = NewArgs(6);
       SetArgTemplate(args,0,"string",1,NclANY);nargs++;
       SetArgTemplate(args,1,"string",1,NclANY);nargs++;
       SetArgTemplate(args,2,"double",1,NclANY);nargs++;
       SetArgTemplate(args,3,"double",1,NclANY);nargs++;
       SetArgTemplate(args,4,"integer",2,NclANY);nargs++;
       SetArgTemplate(args,5,"integer",1,NclANY);nargs++;
       NclRegisterProc(Unstruct2KML_W,args,"Unstruct2KML",nargs);        
/*
 * Register "Unstruct2KML".
 *
 * Create private argument array.
 */
       nargs = 0;
       args = NewArgs(3);
       SetArgTemplate(args,0,"string",1,NclANY);nargs++;
       SetArgTemplate(args,1,"string",1,NclANY);nargs++;
       SetArgTemplate(args,1,"string",1,NclANY);nargs++;
       NclRegisterProc(SCRIP2KML_W,args,"SCRIP2KML",nargs);
#endif
        
# ifdef BuildV5D
/*
 *  Register vis5d+ functions
 *
 *  This is a limited set of Vis5d+ functionality -- only what is needed to
 *  write a vis5d+ formatted file.
 */

    /* v5d_create_W */
    nargs = 0;
    args = NewArgs(14);
    dimsizes[0] = 1;

    /* filename */
    SetArgTemplate(args, nargs, "string", 1, dimsizes);  nargs++;
    /* NumTimes */
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    /* NumVars */
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    /* Nr -- number of rows */
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    /* Nc -- number of columns */
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    /* Nl -- number of levels */
    SetArgTemplate(args, nargs, "integer", 1, NclANY);  nargs++;
    /* VarName */
    SetArgTemplate(args, nargs, "string", 1, NclANY);  nargs++;
    /* TimeStamp */
    SetArgTemplate(args, nargs, "integer", 1, NclANY);  nargs++;
    /* DateStamp */
    SetArgTemplate(args, nargs, "integer", 1, NclANY);  nargs++;
    /* CompressMode */
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    /* Projection */
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    /* ProjArgs */
    SetArgTemplate(args, nargs, "float", 1, NclANY);  nargs++;
    /* Vertical */
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    /* VertArgs */
    SetArgTemplate(args, nargs, "float", 1, NclANY);  nargs++;

    NclRegisterProc(v5d_create_W, args, "v5d_create", nargs);

    /* v5d_write */
    nargs = 0;
    args = NewArgs(3);
    dimsizes[0] = 1;

    /* Time Step */
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    /* Var Num */
    SetArgTemplate(args, nargs, "integer", 1, dimsizes);  nargs++;
    /* Var Data */
    SetArgTemplate(args, nargs, "float", 3, NclANY);  nargs++;
    NclRegisterProc(v5d_write_W, args, "v5d_write", nargs);

    /* v5d_write_var */
    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args, nargs, "float", 4, NclANY);  nargs++;
    NclRegisterProc(v5d_write_var_W, args, "v5d_write_var", nargs);

    /* v5d_close */
    nargs = 0;
    args = NewArgs(0);
    NclRegisterProc(v5d_close_W, args, "v5d_close", nargs);

    /* v5d_missing */
    nargs = 0;
    args = NewArgs(0);
    NclRegisterFunc(v5d_missing_W, args, "v5d_missing", nargs);

# endif /* BuildV5D */


#ifdef BuildGRIDSPEC

/**** BEGIN GRIDSPEC WRAPPERS****/

    /*
    * nccffregridW
    */

    nargs = 0;

    /* Create Private Argument Array*/
    args = NewArgs(34);
 
    /*SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;*/
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"uint",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"uint",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;

    NclRegisterProc(nccffregridW,args,"fregrid",nargs);

    /*
    * nccf_make_conformal_cubic_hgrid_W
    */

    nargs = 0;

    /* Create Private Argument Array*/
    args= NewArgs(6);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
   /* SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;*/

    NclRegisterProc(nccfmakeconformalcubichgridW, args,"make_conformal_cubic_hgrid",nargs);

    /*
    * nccf_make_coupler_mosaic_W
    */

    nargs = 0;

    /* Create Private Argument Array*/
    args = NewArgs(9);
    dimsizes[0] = 1;
    /*SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;*/
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    
    NclRegisterProc(nccfmakecouplermosaicW,args,"make_coupler_mosaic",nargs);

    /*
    * nccf_make_gnomonic_cubic_hgrid_W
    */

    nargs = 0;

    /* Create Private Argument Array*/
    args= NewArgs(5);
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
   /* SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;*/

    NclRegisterProc(nccfmakegnomoniccubichgridW, args,"make_gnomonic_cubic_hgrid",nargs);
    
    /*
    * nccf_make_hgrid_from_file_W
    */

    nargs = 0;

    /* Create Private Argument Array*/
    args = NewArgs(7);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    /*SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;*/
    
    NclRegisterProc(nccfmakehgridfromfileW,args,"make_hgrid_from_file",nargs);

    /*
    * nccf_make_mosaic_W
    */

    nargs = 0;

    /* Create Private Argument Array*/
    args = NewArgs(11);
    dimsizes[0] = 1;
   /* SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;*/
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    
    NclRegisterProc(nccfmakemosaicW,args,"make_mosaic",nargs);

    /*
    * nccf_make_regular_latlon_hgrid_W
    */

    nargs = 0;

    /* Create Private Argument Array*/
    args = NewArgs(10);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
   /* SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;*/
    
    NclRegisterProc(nccfmakeregularlatlonhgridW,args,"make_regular_latlon_hgrid",nargs);

    /*
    * nccf_make_simple_cartesian_hgrid_W
    */

    nargs = 0;

    /* Create Private Argument Array*/
    args = NewArgs(11);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
   /* SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;*/
    
    NclRegisterProc(nccfmakesimplecartesianhgridW,args,"make_simple_cartesian_hgrid",nargs);

    /*
    * nccf_make_spectral_hgrid_W
    */

    nargs = 0;

    /* Create Private Argument Array*/
    args = NewArgs(7);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
   /* SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;*/
    
    NclRegisterProc(nccfmakespectralhgridW,args,"make_spectral_hgrid",nargs);

    /*
    * nccf_make_topog
    */

    nargs = 0;

    /* Create Private Argument Array*/
    args = NewArgs(26);
    dimsizes[0] = 1; 
   /* SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;*/
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;

    NclRegisterProc(nccfmaketopogW,args,"make_topog",nargs);

    /*
    * nccf_make_tripolar_hgrid_W
    */

    nargs = 0;

    /* Create Private Argument Array*/
    args = NewArgs(11);
    dimsizes[0] = 1;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"double",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
   /* SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;*/
    
    NclRegisterProc(nccfmaketripolarhgridW,args,"make_tripolar_hgrid",nargs);

    /*
    * nccf_make_vgrid_W
    */

    nargs = 0;

    /* Create Private Argument Array*/
    args = NewArgs(5);
    dimsizes[0] = 1;
   /* SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;*/
    SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;
    SetArgTemplate(args,nargs,"double",1,NclANY);nargs++;
   /* SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;*/
   /* SetArgTemplate(args,nargs,"integer",1,dimsizes);nargs++;*/
    SetArgTemplate(args,nargs,"integer",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    SetArgTemplate(args,nargs,"string",1,NclANY);nargs++;
    
    NclRegisterProc(nccfmakevgridW,args,"make_vgrid",nargs);

/**** END GRIDSPEC WRAPPERS ****/

#endif

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
               (void*)missing_dx,
               (void*)missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));

    if(type_x != NCL_double && missing_rx != NULL) {
      _Nclcoerce((NclTypeClass)nclTypefloatClass,
                 (void*)missing_rx,
                 (void*)missing_x,
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
 * Coerce a missing value to long. If no missing value, then set a
 * default missing value for the long type.
 */
void coerce_missing_long(
NclBasicDataTypes type_x,
long              has_missing_x,
NclScalar         *missing_x,
NclScalar         *missing_lx)
{
/*
 * Check for missing value and coerce if neccesary.
 */
  if(has_missing_x) {
/*
 * Coerce missing value to long.
 */
    _Nclcoerce((NclTypeClass)nclTypelongClass,
               (void*)missing_lx,
               (void*)missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
    if(missing_lx != NULL) {
/*
 * Get the default missing value, just in case.
 */ 
      missing_lx->longval = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
    }
  }
}

/*
 * Coerce a missing value to int. If no missing value, then set a
 * default missing value for the int type.
 */
void coerce_missing_int(
NclBasicDataTypes type_x,
int               has_missing_x,
NclScalar         *missing_x,
NclScalar         *missing_ix)
{
/*
 * Check for missing value and coerce if neccesary.
 */
  if(has_missing_x) {
/*
 * Coerce missing value to int.
 */
    _Nclcoerce((NclTypeClass)nclTypeintClass,
               (void*)missing_ix,
               (void*)missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
    if(missing_ix != NULL) {
/*
 * Get the default missing value, just in case.
 */ 
      missing_ix->intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
    }
  }
}

/*
 * Coerce a missing value to short. If no missing value, then set a
 * default missing value for the short type.
 */
void coerce_missing_short(
NclBasicDataTypes type_x,
short             has_missing_x,
NclScalar         *missing_x,
NclScalar         *missing_sx)
{
/*
 * Check for missing value and coerce if neccesary.
 */
  if(has_missing_x) {
/*
 * Coerce missing value to short.
 */
    _Nclcoerce((NclTypeClass)nclTypeshortClass,
               (void*)missing_sx,
               (void*)missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
  }
  else {
    if(missing_sx != NULL) {
/*
 * Get the default missing value, just in case.
 */ 
      missing_sx->shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
    }
  }
}

/*
 * Coerce a missing value to double.  Also, set a default missing
 * value and set an int/long/float missing value for the return.
 */
void coerce_missing_more_types(
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
               (void*)missing_dx,
               (void*)missing_x,
               1,
               NULL,
               NULL,
               _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));

    if(missing_rx != NULL) {
      if(type_x == NCL_double) { 
        _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                   (void*)missing_rx,
                   (void*)missing_x,
                   1,
                   NULL,
                   NULL,
                   _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
      }
      else if(type_x == NCL_float) { 
        _Nclcoerce((NclTypeClass)nclTypefloatClass,
                   (void*)missing_rx,
                   (void*)missing_x,
                   1,
                   NULL,
                   NULL,
                   _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
      }
      else if(type_x == NCL_long) { 
        _Nclcoerce((NclTypeClass)nclTypelongClass,
                   (void*)missing_rx,
                   (void*)missing_x,
                   1,
                   NULL,
                   NULL,
                   _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
      }
      else {
        _Nclcoerce((NclTypeClass)nclTypeintClass,
                   (void*)missing_rx,
                   (void*)missing_x,
                   1,
                   NULL,
                   NULL,
                   _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
      }
    }
  }
  else {
    if(missing_dx != NULL) {
/*
 * Get the default missing value, just in case.
 */ 
      if(type_x == NCL_double) {
        missing_dx->doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
      }
      else if(type_x == NCL_float) {
        missing_dx->doubleval = (double)((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
        if(missing_rx != NULL) {
          missing_rx->floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
        }
      }
      else if(type_x == NCL_long) {
        missing_dx->doubleval = (double)((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
        if(missing_rx != NULL) {
          missing_rx->longval = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
        }
      }
      else {
        missing_dx->doubleval = (double)((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
        if(missing_rx != NULL) {
          missing_rx->intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
        }
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
ng_size_t         size_x,
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
                 missing_x,
                 missing_dx,
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
 * Coerce a contiguous subset of the data to double.
 */
void coerce_subset_input_double(
void              *x,
double            *tmp_x,
ng_size_t         index_x,
NclBasicDataTypes type_x,
ng_size_t         size_x,
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
               missing_x,
               missing_dx,
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
 * Coerce a non-contiguous subset of the data to double.
 */
void coerce_subset_input_double_step(
void              *x,
double            *tmp_x,
ng_size_t         index_x,
ng_size_t         step_x,
NclBasicDataTypes type_x,
ng_size_t         size_x,
int               has_missing_x,
NclScalar         *missing_x,
NclScalar         *missing_dx
)
{
  ng_size_t i, ii;
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
    for(i = 0; i < size_x; i++ ) {
      ii = step_x*i;
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 &tmp_x[i],
                 (void*)((char*)x+(index_x+ii)*(typeclass_x->type_class.size)),
                 1,
                 missing_x,
                 missing_dx,
                 typeclass_x);
    }
  }
  else {
/*
 * Coerce subset to double, with no missing values.
 */
    for(i = 0; i < size_x; i++ ) {
      ii = step_x*i;
      _Nclcoerce((NclTypeClass)nclTypedoubleClass,
                 &tmp_x[i],
                 (void*)((char*)x+(index_x+ii)*(typeclass_x->type_class.size)),
                 1,
                 NULL,
                 NULL,
                 typeclass_x);
    }
  }
}

/*
 * Checks if a variable is a scalar or not.
 * Returns 1 if it is, and a 0 if it isn't.
 */
logical is_scalar(
int        ndims_x,
ng_size_t *dsizes_x
)
{
  if(ndims_x == 1 && dsizes_x[0] == 1) return(True);
  return(False);
}


/*
 * Checks if a variable is a scalar or an array 
 * of all degenerate dimensions, i.e. 1 x 1 x 2.
 * Returns 1 if it is, and a 0 if it isn't.
 */
logical is_scalar_array(
int        ndims_x,
ng_size_t *dsizes_x
)
{
  int i = 0;
  while(i < ndims_x) {
    if(dsizes_x[i++] != 1) return(False);
  }
  return(True);
}


/*
 * Copy a scalar to an array of scalars.
 */
double *copy_scalar_to_array(
double       *x,
int          ndims_x,
ng_size_t    *dsizes_x,
ng_size_t    size_x)
{
  ng_size_t i;
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
ng_size_t size_x,
int    has_allocated
)
{
  ng_size_t i;
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
 * Copy double data back to a long array, using a void array. 
 */
void coerce_output_long_only(
void   *x,
double *dx,
ng_size_t size_x,
ng_size_t index_x
)
{
  ng_size_t i;
  for( i = 0; i < size_x; i++ ) ((long*)x)[index_x+i]  = (long)dx[i];
}

/*
 * Copy double data back to a int array, using a void array. 
 */
void coerce_output_int_only(
void   *x,
double *dx,
ng_size_t size_x,
ng_size_t index_x
)
{
  ng_size_t i;

  for( i = 0; i < size_x; i++ ) ((int*)x)[index_x+i]  = (int)dx[i];
}


/*
 * This routine doesn't really do any coercion.  It just checks if 
 * the type is float, and if so, it allocates enough space to hold
 * double values. Otherwise, it returns a pointer to the double values.
 */
double *coerce_output_double(
void              *x,
NclBasicDataTypes type_x,
ng_size_t         size_x)
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
ng_size_t size_x,
int    has_missing_x,
double missing
)
{
  ng_size_t l;
  int found_missing = 0;
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
ng_size_t size_x,
int    has_missing_x,
float missing
)
{
  int found_missing = 0;
  ng_size_t l;
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
ng_size_t         index_x,
NclBasicDataTypes type_x,
ng_size_t         size_x,
double            missing_x)
{
  ng_size_t i;
  for(i = 0; i < size_x; i++) {
    if(type_x != NCL_double) {
      ((float*)x)[index_x+i] = (float)missing_x;
    }
    else {
      ((double*)x)[index_x+i] = missing_x;
    }
  }
}


/*
 * Sets a subset of non-contiguous output data to missing.
 */
void set_subset_output_missing_step(
void              *x,
ng_size_t         index_x,
ng_size_t         step_x,
NclBasicDataTypes type_x,
ng_size_t         size_x,
double            missing_x)
{
  ng_size_t i;
  for(i = 0; i < size_x; i++) {
    if(type_x != NCL_double) {
      ((float*)x)[index_x + i*step_x] = (float)missing_x;
    }
    else {
      ((double*)x)[index_x + i*step_x] = missing_x;
    }
  }
}


void compute_nlatnlon(
ng_size_t *dsizes,
int ndims,
ng_size_t *nlat,
ng_size_t *nlon,
ng_size_t *nlatnlon,
ng_size_t *nt,
ng_size_t *total
)
{
  ng_size_t i;

  *nlat = dsizes[ndims-2];
  *nlon = dsizes[ndims-1];
  *nlatnlon = *nlat * *nlon;
  *nt = 1;
  for(i = 0; i < ndims-2; i++) *nt *= dsizes[i];
  *total = *nlatnlon * *nt;
}

void compute_nlatanlona(
ng_size_t *dsizes_in,
ng_size_t *dsizes_out,
int ndims_in,
int ndims_out,
ng_size_t *nlata,
ng_size_t *nlona,
ng_size_t *nlatanlona,
ng_size_t *nlatb,
ng_size_t *nlonb,
ng_size_t *nlatbnlonb,
ng_size_t *size_leftmost,
ng_size_t *size_in,
ng_size_t *size_out
)
{
  ng_size_t i;

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

/*
 * Prints min/max of input data.
 */
void print_minmax(
void *x,
ng_size_t size_x,
NclBasicDataTypes type_x
)
{
  double xmin, xmax;
  ng_size_t i;
  if(type_x != NCL_double) {
        xmin = xmax = (double)((float*)x)[0];
  }
  else {
        xmin = xmax = ((double*)x)[0];
  }
  
  if(type_x != NCL_double) {
        for( i = 1; i < size_x; i++ ) {
          xmin = min(xmin,(double)((float*)x)[i]);
          xmax = max(xmax,(double)((float*)x)[i]);
        }
  }
  else {
        for( i = 1; i < size_x; i++ ) {
          xmin = min(xmin,((double*)x)[i]);
          xmax = max(xmax,((double*)x)[i]);
        }
  }
  printf("xmin = %g xmax = %g\n", xmin, xmax );
}

/*
 * Copy double data back to a float array, using a void array. 
 */
void coerce_output_float_only(
void   *x,
double *dx,
ng_size_t size_x,
ng_size_t index_x
)
{
  ng_size_t i;

  for( i = 0; i < size_x; i++ ) ((float*)x)[index_x+i]  = (float)dx[i];
}

/*
 * Copy double data back to double or float array, using a void array. 
 */
void coerce_output_float_or_double(
void   *x,
double *dx,
NclBasicDataTypes type_x,
ng_size_t size_x,
ng_size_t index_x
)
{
  ng_size_t i;

  if(type_x == NCL_double) {
    for( i = 0; i < size_x; i++ ) ((double*)x)[index_x+i]  = dx[i];
  }
  else {
    for( i = 0; i < size_x; i++ ) ((float*)x)[index_x+i]  = (float)dx[i];
  }
}

/*
 * Copy double data back to double or float array, using a void array. 
 * This is identical to coerce_output_float_or_double, except it only
 * copies values where the corresponding indexes value is != 0.
 */
void coerce_output_float_or_double_ind(
void   *x,
double *dx,
NclBasicDataTypes type_x,
ng_size_t size_x,
ng_size_t index_x,
ng_size_t *indexes
)
{
  ng_size_t i;

  if(type_x == NCL_double) {
    for( i = 0; i < size_x; i++ ) {
      if(indexes[i] != 0) {
        ((double*)x)[index_x+i]  = dx[i];
      }
    }
  }
  else {
    for( i = 0; i < size_x; i++ ) {
      if(indexes[i] != 0) {
        ((float*)x)[index_x+i]  = (float)dx[i];
      }
    }
  }
}


/*
 * Copy double data back to double or float non-contiguous array,
 * using a void array. 
 */
void coerce_output_float_or_double_step(
void   *x,
double *dx,
NclBasicDataTypes type_x,
ng_size_t size_x,
ng_size_t index_x,
ng_size_t step_x
)
{
  ng_size_t i;

  if(type_x == NCL_double) {
    for( i = 0; i < size_x; i++ ) ((double*)x)[index_x+(step_x*i)] = dx[i];
  }
  else {
    for( i = 0; i < size_x; i++ ) ((float*)x)[index_x+(step_x*i)]  = (float)dx[i];
  }
}


/*
 * Copy double data back to double, float, long, or int non-contiguous
 * array, using a void array.  Really, there's no need for both
 * coerce_output_step and coerce_output_float_or_double_step to
 * both exist. coerce_output_float_or_double_step has been around
 * awhile, and we never need a routine that returned something other
 * than floats or doubles until NCL V6.4.0 (dim_acumrun_n)
 *
 */
void coerce_output_step(
void   *x,
double *dx,
NclBasicDataTypes type_x,
ng_size_t size_x,
ng_size_t index_x,
ng_size_t step_x
)
{
  ng_size_t i;

  if(type_x == NCL_double) {
    for( i = 0; i < size_x; i++ ) ((double*)x)[index_x+(step_x*i)] = dx[i];
  }
  else if(type_x == NCL_float) {
    for( i = 0; i < size_x; i++ ) ((float*)x)[index_x+(step_x*i)]  = (float)dx[i];
  }
  else if(type_x == NCL_long) {
    for( i = 0; i < size_x; i++ ) ((long*)x)[index_x+(step_x*i)]  = (long)dx[i];
  }
  else {
    for( i = 0; i < size_x; i++ ) ((int*)x)[index_x+(step_x*i)]  = (int)dx[i];
  }
}



/*
 * Coerce data to float, or just return a pointer to it if
 * it is already float.
 */
float *coerce_input_float(
void              *x,
NclBasicDataTypes type_x,
ng_size_t         size_x,
int               has_missing_x,
NclScalar         *missing_x,
NclScalar         *missing_fx)
{
  float *fx;
/*
 * Coerce x to float if necessary.
 */
  if(type_x != NCL_float) {
    fx = (float*)calloc(size_x,sizeof(float));
    if( fx == NULL ) return(NULL);
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypefloatClass,
                 (void*)fx,
                 x,
                 size_x,
                 missing_x,
                 missing_fx,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypefloatClass,
                 (void*)fx,
                 x,
                 size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * x is already float.
 */
    fx = (float*)x;
  }
  return(fx);
}

/*
 * Coerce data to int, or just return a pointer to it if
 * it is already int.
 */
int *coerce_input_int(
void              *x,
NclBasicDataTypes type_x,
ng_size_t         size_x,
int               has_missing_x,
NclScalar         *missing_x,
NclScalar         *missing_ix)
{
  int *ix;
/*
 * Coerce x to integer if necessary.
 */
  if(type_x != NCL_int) {
    ix = (int*)calloc(size_x,sizeof(int));
    if( ix == NULL ) return(NULL);
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypeintClass,
                 (void*)ix,
                 x,
                 size_x,
                 missing_x,
                 missing_ix,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypeintClass,
                 (void*)ix,
                 x,
                 size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * x is already int.
 */
    ix = (int*)x;
  }
  return(ix);
}

/*
 * Coerce data to unsigned int, or just return a pointer to it if
 * it is already uint.
 */
unsigned int *coerce_input_uint(
void              *x,
NclBasicDataTypes type_x,
ng_size_t         size_x,
int               has_missing_x,
NclScalar         *missing_x,
NclScalar         *missing_uix)
{
  uint *uix;
/*
 * Coerce x to unsigned int if necessary.
 */
  if(type_x != NCL_uint) {
    uix = (unsigned int*)calloc(size_x,sizeof(unsigned int));
    if( uix == NULL ) return(NULL);
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypeuintClass,
                 (void*)uix,
                 x,
                 size_x,
                 missing_x,
                 missing_uix,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypeuintClass,
                 (void*)uix,
                 x,
                 size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * x is already uint.
 */
    uix = (unsigned int*)x;
  }
  return(uix);
}

/*
 * Coerce data to unsigned long or just return a pointer to it if it is already ulong. 
 */
unsigned long *coerce_input_ulong(
void              *x,
NclBasicDataTypes type_x,
ng_size_t         size_x,
int               has_missing_x,
NclScalar         *missing_x,
NclScalar         *missing_ulx)
{
  unsigned long *ulx;
/*
 * Coerce x to unsigned long if necessary.
 */
  if(type_x != NCL_ulong) {
    ulx = (unsigned long *)calloc(size_x,sizeof(unsigned long));
    if( ulx == NULL ) return(NULL);
    if(has_missing_x) {
      _Nclcoerce((NclTypeClass)nclTypeulongClass,
                 (void*)ulx,
                 x,
                 size_x,
                 missing_x,
                 missing_ulx,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
    else {
      _Nclcoerce((NclTypeClass)nclTypeulongClass,
                 (void*)ulx,
                 x,
                 size_x,
                 NULL,
                 NULL,
                 _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(type_x)));
    }
  }
  else {
/*
 * x is already ulong.
 */
    ulx = (unsigned long*)x;
  }
  return(ulx);
}

/*
 * Coerce a contiguous subset of the data to float.
 */
void coerce_subset_input_float(
void              *x,
float             *tmp_x,
ng_size_t         index_x,
NclBasicDataTypes type_x,
ng_size_t         size_x,
int               has_missing_x,
NclScalar         *missing_x,
NclScalar         *missing_fx
)
{
  NclTypeClass typeclass_x;
  
/*
 * typeclass_x is what allows us to get the size of the type of x.
 */
  typeclass_x = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_x)));
/*
 * Coerce x to float.
 */
  if(has_missing_x) {
/*
 * Coerce subset to float, with missing values.
 */
    _Nclcoerce((NclTypeClass)nclTypefloatClass,
               tmp_x,
               (void*)((char*)x+index_x*(typeclass_x->type_class.size)),
               size_x,
               missing_x,
               missing_fx,
               typeclass_x);
  }
  else {
/*
 * Coerce subset to float, with no missing values.
 */
    _Nclcoerce((NclTypeClass)nclTypefloatClass,
               tmp_x,
               (void*)((char*)x+index_x*(typeclass_x->type_class.size)),
               size_x,
               NULL,
               NULL,
               typeclass_x);
  }
}

/*
 * Force incoming numeric values to be integer.
 */
void force_subset_input_int(
void              *x,
int               *tmp_x,
ng_size_t         index_x,
NclBasicDataTypes type_x,
ng_size_t         size_x
)
{
  ng_size_t i;
  NclTypeClass typeclass_x, typeclass_xi;
  typeclass_x  = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_x)));
  typeclass_xi = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(NCL_int)));
  for( i = 0; i < size_x; i++ ) _NclScalarForcedCoerce((char*)x + (index_x+i)*typeclass_x->type_class.size,type_x,
                                                       (char*)tmp_x + i*typeclass_xi->type_class.size,NCL_int);
}

/*
 * Force incoming numeric values to be long.
 */
void force_subset_input_long(
void              *x,
long              *tmp_x,
ng_size_t         index_x,
NclBasicDataTypes type_x,
ng_size_t         size_x
)
{
  ng_size_t i;
  NclTypeClass typeclass_x, typeclass_xl;
  typeclass_x  = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_x)));
  typeclass_xl = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(NCL_long)));
  for( i = 0; i < size_x; i++ ) _NclScalarForcedCoerce((char*)x + (index_x+i)*typeclass_x->type_class.size,type_x,
                                                       (char*)tmp_x + i*typeclass_xl->type_class.size,NCL_long);
}

/*
 * Force incoming numeric values to be long.
 */
void force_subset_input_short(
void              *x,
short             *tmp_x,
ng_size_t         index_x,
NclBasicDataTypes type_x,
ng_size_t         size_x
)
{
  ng_size_t i;
  NclTypeClass typeclass_x, typeclass_xs;
  typeclass_x  = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(type_x)));
  typeclass_xs = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(NCL_short)));
  for( i = 0; i < size_x; i++ ) _NclScalarForcedCoerce((char*)x + (index_x+i)*typeclass_x->type_class.size,type_x,
                                                       (char*)tmp_x + i*typeclass_xs->type_class.size,NCL_short);
}

/*
 * Retrieve the dimension name info of a particular
 * input argument to an NCL function or procedure.
 */
NclDimRec *get_dim_info(arg_num,num_args)
int arg_num, num_args;
{
  NclStackEntry tmp_var;
  NclDimRec *dim_info;
  int i;

  tmp_var = _NclGetArg(arg_num,num_args,DONT_CARE);

  if(tmp_var.kind == NclStk_VAR) {
          dim_info = malloc(sizeof(NclDimRec) * tmp_var.u.data_var->var.n_dims);
          for (i = 0; i < tmp_var.u.data_var->var.n_dims; i++) {
                  dim_info[i].dim_quark = tmp_var.u.data_var->var.dim_info[i].dim_quark;
                  dim_info[i].dim_num = tmp_var.u.data_var->var.dim_info[i].dim_num;
                  dim_info[i].dim_size = tmp_var.u.data_var->var.dim_info[i].dim_size;
          }
          return(dim_info);
  }
  else {
          return(NULL);
  }
}

/*
 * This function takes a void* variable that holds dimension sizes
 * as bytes, shorts, integers, or longs, and returns the dimensions 
 * as an ng_size_t array.
 */
ng_size_t *get_dimensions(void *tmp_dimensions,int n_dimensions,
                          NclBasicDataTypes type_dimensions, const char *name)
{
  int i;
  ng_size_t *dimensions;

  dimensions = (ng_size_t *)NclMalloc(sizeof(ng_size_t) * n_dimensions);
  switch (type_dimensions) {
  case NCL_byte:
    for (i = 0; i < n_dimensions; i++) {
      ((ng_size_t *)dimensions)[i] = ((byte*)tmp_dimensions)[i];
    }
    break;

  case NCL_short:
    for (i = 0; i < n_dimensions; i++) {
      ((ng_size_t *)dimensions)[i] = ((short*)tmp_dimensions)[i];
    }
    break;

  case NCL_int:
    for (i = 0; i < n_dimensions; i++) {
      ((ng_size_t *)dimensions)[i] = ((int*)tmp_dimensions)[i];
    }
    break;
    
  case NCL_long:
    for (i = 0; i < n_dimensions; i++) {
      ((ng_size_t *)dimensions)[i] = ((long*)tmp_dimensions)[i];
    }
    break;

  default:
    NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: The input dimension sizes must be byte, short, integer or long",name);
    NclFree(dimensions);
    return(NULL);
  }
  return(dimensions);
}

int *get_dims_for_n_funcs(int arg_num,  int num_args, NclStackEntry tmpdata,
                           const char *name, int *ndims)
{
  NclBasicDataTypes type_dims;
  void *dims_ptr;
  int i, *dims; 
  ng_size_t num_dims[1];
  NrmQuark *dim_names;
  NclVar tmpvar;

  switch(tmpdata.kind) {
  case NclStk_VAR:
    tmpvar = tmpdata.u.data_var;
    break;
  case NclStk_VAL:
    tmpvar = NULL;
    break;
  default:
    NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Internal error", name);
    return(NULL);
  }

  dims_ptr = (void *)NclGetArgValue(arg_num,num_args,NULL,num_dims,NULL,
                                    NULL,&type_dims,DONT_CARE);
  if(type_dims != NCL_int && type_dims != NCL_string) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: The input dimensions must be integers representing dimension numbers, or strings representing dimension names",name);
    return(NULL);
  }
  dims   = (int*)calloc(num_dims[0],sizeof(int));
  if(dims == NULL) {
    NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Can't allocate memory for dimension indexes");
    return(NULL);
  }

  if(type_dims == NCL_int) {
    for(i = 0; i < num_dims[0]; i++) {
      dims[i] = ((int*)dims_ptr)[i];
    }
  }
  else {
    if(tmpvar != NULL) {
      dim_names = (NrmQuark *)NclGetArgValue(1,2,NULL,NULL,NULL,NULL,NULL,DONT_CARE);
    }
    else {
      NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: Can't determine dimension names from input array",name);
      return(NULL);
    }
    for(i = 0; i < num_dims[0]; i++) {
/*
 * Don't do any error checking here. Let the calling routine do it.
 */
      dims[i] = _NclIsDim(tmpvar,NrmQuarkToString(dim_names[i]));
    }
  }
  *ndims = num_dims[0];
  return(dims);
}

/* For the qsort procedure */
int cmpdouble (const void * a, const void * b)
{
  if (*(double*)a > *(double*)b) return 1;
  else if (*(double*)a < *(double*)b) return -1;
  else return 0;  
}

