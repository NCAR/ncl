/*
 *  Tests usage of FRAME with CGM output mixed with output 
 *  to X workstations.
 */
#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

#define min(x,y) ((x) < (y) ? (x) : (y))

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{
    int i;
    int ws_type, n, nfpp, font, prec, nchh, nchx, nptxi;
    float minchh, maxchh,minchx,maxchx;
    Gcolr_rep rgb[2];
    Gtext_align text_align;
    Gint_list generic;
    Gint ierr, length_list;
    Gtext_facs text_facs;
    Gline_facs line_facs;
    Gmarker_facs marker_facs;
    Gpoint text_pos;
/*
 * Initialize GKS structures.
 */
    text_pos.x = text_pos.y = .5;
    text_align.hor = GHOR_CTR;
    text_align.vert = GVERT_HALF;
    rgb[0].rgb.red = 1.;
    rgb[0].rgb.blue = rgb[0].rgb.green = 0.;
    rgb[1].rgb.red = rgb[1].rgb.blue = 0.;
    rgb[1].rgb.green = 1.;
/*
 * Open GKS.
 */
	gopen_gks("stdout",10000);
/*
 *  open cgm workstation.
 */
    gopen_ws(WKID,NULL,WSTYPE);
    gset_colr_rep(WKID,6,&rgb[0]);
    gset_colr_rep(WKID,7,&rgb[1]);
	gset_char_ht(.04);
    gset_text_align(&text_align);
/*
 *  activate the cgm workstation and set the text color to red.
 */
    gactivate_ws(WKID);
/*
 *  create picture 1 in the metafile.
 */
	gset_text_colr_ind(6);
	gtext(&text_pos,"picture 1");
	c_frame();
/*
 *  picture 2 in the metafile--
 */
	gtext(&text_pos,"picture 2");
	c_frame();
/*
 *  picture 3 in the metafile and to x window.
 */
	gopen_ws(5,NULL,8);
	gactivate_ws(5);
    gset_colr_rep(5,6,&rgb[0]);
    gset_colr_rep(5,7,&rgb[1]);

	gtext(&text_pos,"picture 3");
	c_frame();
/*
 *  open another x workstation (window 2).
 */
	gopen_ws(7,NULL,8);
	gactivate_ws(7);
    gset_colr_rep(7,6,&rgb[0]);
    gset_colr_rep(7,7,&rgb[1]);

	gtext(&text_pos,"picture 4");
	c_frame();
/*
 * Initialize generic structure
 */
    generic.ints = (Gint *)malloc(5*sizeof(Gint));
/*
 * Inquire set of open workstations
 */
    printf( "\nginq_set_open_wss (starting with index 1):\n" );
    ginq_set_open_wss(2,1,&ierr,&generic,&length_list);
    printf( "open_ws.num_ints = %d, length_list = %d\n", generic.num_ints, length_list );
    for( i = 0; i < generic.num_ints; i++ ) {
        printf( "ws[%d] = %d\n", i+1, generic.ints[i] );
    }
/*
 * Inquire set of active workstations
 */
    printf( "\nginq_set_active_wss (starting with index 1):\n" );
    ginq_set_active_wss(2,1,&ierr,&generic,&length_list);
    printf( "active_ws.num_ints = %d, length_list = %d\n", generic.num_ints, length_list );
    for( i = 0; i < generic.num_ints; i++ ) {
        printf( "ws[%d] = %d\n", i+1, generic.ints[i] );
    }
/*
 * Inquire list of available workstation types
 */
    printf( "\nginq_list_avail_ws_types (starting with index 1):\n" );
    ginq_list_avail_ws_types(2,1,&ierr,&generic,&length_list);
    printf( "ws_type.num_ints = %d, length_list = %d\n", generic.num_ints, length_list );
    for( i = 0; i < generic.num_ints; i++ ) {
        printf( "ws_type[%d] = %d\n", i+1, generic.ints[i] );
    }
/*
 * Inquire list of normalization transformation numbers
 */
    printf( "\nginq_list_norm_tran_nums (starting with index 1):\n" );
    ginq_list_norm_tran_nums(2,1,&ierr,&generic,&length_list);
    printf( "norm_tran_num.num_ints = %d, length_list = %d\n", generic.num_ints, length_list );
    for( i = 0; i < generic.num_ints; i++ ) {
        printf( "nm[%d] = %d\n", i+1, generic.ints[i] );
    }
/*
 * Inquire list of colour indices
 */
    printf( "\nginq_list_colr_inds (starting with index 1):\n" );
    ginq_list_colr_inds(1,2,1,&ierr,&generic,&length_list);
    printf( "color_inds.num_ints = %d, length_list = %d\n", generic.num_ints, length_list );
    for( i = 0; i < generic.num_ints; i++ ) {
        printf( "color[%d] = %d\n", i+1, generic.ints[i] );
    }
/*
 * Inquire line facilities
 */
    printf( "\nInquiring about line facilities:\n" );
    line_facs.types.ints = (Gint *)malloc(5*sizeof(Gint));
/*
 * Actual call
 */
    ginq_line_facs(1,3,0,&ierr,&line_facs,&length_list);
/*
 * Print information
 */
    printf( "length_list = %d\n", length_list );
    printf( "nom_width = %g\n", line_facs.nom_width );
    printf( "min_width = %g\n", line_facs.min_width );
    printf( "max_width = %g\n", line_facs.max_width );
    for( i = 0; i < line_facs.types.num_ints; i++ ) {
        printf( "types[%d] = %d\n", i, line_facs.types.ints[i] );
    }
/*
 * Inquire marker facilities
 */
    printf( "\nInquiring about marker facilities:\n" );
    marker_facs.types.ints = (Gint *)malloc(5*sizeof(Gint));
/*
 * Actual call
 */
    ginq_marker_facs(1,3,0,&ierr,&marker_facs,&length_list);
/*
 * Print information
 */
    printf( "length_list = %d\n", length_list );
    printf( "nomms = %g\n", marker_facs.nom_size );
    printf( "min_size = %g\n", marker_facs.min_size );
    printf( "max_size = %g\n", marker_facs.max_size );
    for( i = 0; i < marker_facs.types.num_ints; i++ ) {
        printf( "types[%d] = %d\n", i, marker_facs.types.ints[i] );
    }
/*
 * Inquire text facilities
 */
    printf( "\nInquiring about text facilities:\n" );
    text_facs.font_precs = (Gtext_font_prec *)malloc(3*sizeof(Gtext_font_prec));
    ws_type = 1;
    n = 1;
/*
 * Actual call
 */
    ginq_text_facs(1,3,0,&ierr,&text_facs,&length_list);
    gqtxf_(&ws_type,&n,&ierr,&nfpp,&font,&prec,&nchh,&minchh,&maxchh,&nchx,&minchx,&maxchx,&nptxi);
/*
 * Print information
 */
    printf( "length_list = %d\n", length_list );
    printf( "Number of font precisions = %d\n", text_facs.num_font_precs );
    for( i = 0; i < text_facs.num_font_precs; i++ ) {
        printf( "font[%d] = %d, prec[%d] = %d\n", i, text_facs.font_precs[i].font, i, text_facs.font_precs[i].prec );
    }
    printf( "Number of character heights = %d\n", text_facs.num_char_hts );
    printf( "Minimum character height = %g\n", text_facs.min_char_ht );
    printf( "Maximum character height = %g\n", text_facs.max_char_ht );
    printf( "Number of character expansions = %d\n", text_facs.num_char_expans );
    printf( "Minimum character expansion = %g\n", text_facs.min_char_expan );
    printf( "Maximum character expansion = %g\n", text_facs.max_char_expan );
    printf( "Number of pred inds = %d\n", text_facs.num_pred_inds );

    printf( "\nFortran routine: gqtxf:\n" );
    printf( "Number of font precisions = %d\n", nfpp );
    printf( "Font = %d, Prec = %d\n", font, prec );
    printf( "Number of character heights = %d\n", nchh );
    printf( "Minimum character height = %g\n", minchh );
    printf( "Maximum character height = %g\n", maxchh );
    printf( "Number of character expansions = %d\n", nchx );
    printf( "Minimum character expansion = %g\n", minchx );
    printf( "Maximum character expansion = %g\n", maxchx );
    printf( "Number of pred inds = %d\n", nptxi );
/*
 *  deactivate metafile and draw picture 5.
 */
	gdeactivate_ws(WKID);
	gtext(&text_pos,"picture 5");
	c_frame();
/*
 *  re-activate the metafile.
 */
        gactivate_ws(WKID);
        gtext(&text_pos,"picture 6");
        c_frame();
/*
 *  deactivate first window and put out picture 7.
 */
        gdeactivate_ws(5);
        gclose_ws(5);
        gtext(&text_pos,"picture 7");
        c_frame();

        gtext(&text_pos,"picture 8");
        c_frame();

        gdeactivate_ws(7);
        gdeactivate_ws(WKID);
        gclose_ws(7);
        gclose_ws(WKID);
/*
 * Close GKS.
 */
        gclose_gks();
}
