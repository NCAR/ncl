/*
 *  tests usage of ngpict with cgm output mixed with output 
 *  to x workstations.
 */

#include <stdio.h>

#include <ncarg/ncargC.h>
#include <ncarg/gks.h>

main()
{
    Gcolr_rep rgb[2];
    Gtext_align text_align;
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
    gopen_gks("stderr",10000);
/*
 *  open cgm workstation.
 */
    gopen_ws(1,NULL,1);
    gset_colr_rep(1,6,&rgb[0]);
    gset_colr_rep(1,7,&rgb[1]);
    gset_char_ht(.04);
    gset_text_align(&text_align);
/*
 *  activate the cgm workstation and set the text color to red.
 */
    gactivate_ws(1);
/*
 *  create picture 1 in the metafile.
 */
    gset_text_colr_ind(6);
    gtext(&text_pos,"picture 1");
    c_ngpict(1,1);
/*
 *  picture 2 in the metafile--
 */
    gtext(&text_pos,"picture 2");
    c_ngpict(1,1);
/*
 *  picture 3 in the metafile and to x window.
 */
    gopen_ws(5,NULL,8);
    gactivate_ws(5);
    gset_colr_rep(5,6,&rgb[0]);
    gset_colr_rep(5,7,&rgb[1]);

    gtext(&text_pos,"picture 3");
    c_ngpict(1,1);
    c_ngpict(5,4);
/*
 *  open another x workstation (window 2).
 */
    gopen_ws(7,NULL,8);
    gactivate_ws(7);
    gset_colr_rep(7,6,&rgb[0]);
    gset_colr_rep(7,7,&rgb[1]);

    gtext(&text_pos,"picture 4");
    c_ngpict(1,1);
    c_ngpict(7,0);
    c_ngpict(5,4);
    c_ngpict(7,1);
/*
 *  deactivate metafile and draw picture 5.
 */
    gdeactivate_ws(1);
    gtext(&text_pos,"picture 5");
    c_ngpict(7,0);
    c_ngpict(5,4);
    c_ngpict(7,1);
/*
 *  re-activate the metafile.
 */
    gactivate_ws(1);
    gtext(&text_pos,"picture 6");
    c_ngpict(1,1);
    c_ngpict(7,0);
    c_ngpict(5,4);
    c_ngpict(7,1);
/*
 *  deactivate first window and put out picture 7.
 */
    gdeactivate_ws(5);
    gclose_ws(5);
    gtext(&text_pos,"picture 7");
    c_ngpict(1,1);
    c_ngpict(7,4);

    gtext(&text_pos,"picture 8");
    c_ngpict(1,1);
    c_ngpict(7,4);

    gdeactivate_ws(7);
    gdeactivate_ws(1);
    gclose_ws(7);
    gclose_ws(1);

    gclose_gks();
}
