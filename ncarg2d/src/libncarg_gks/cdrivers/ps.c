/*
 *      $Id: ps.c,v 1.41.2.1 2010-03-17 20:53:30 brownrig Exp $
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

/*
 *
 *      File:           ps.c
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Thu Aug 19 10:30:32 MDT 1993
 *
 *      Description:    This file contains the definition of the PostScript
 *                      device driver.
 */
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include <ncarg/gksP.h>
#include <ncarg/c.h>
#include <ncarg/gks.h>
#include "gksc.h"
#include "gks.h"
#include "common.h"
#include "ps_device.h"
#include "psddi.h"
#include "ps.h"
#include "argb.h"

char    *PSFontNames[] = {
                            "/Times-Roman", "/Times-Bold", "/Times-Italic",
                            "/Times-BoldItalic", "/Helvetica",
                            "/Helvetica-Bold", "/Helvetica-Oblique",
                            "/Helvetica-BoldOblique", "/Courier",
                            "/Courier-Bold", "/Courier-Oblique",
                            "/Courier-BoldOblique", "/Symbol"
                };

extern int      orig_wks_id;
extern void     gerr_hand (Gint, Gint, const char *);
int             c_model;

void writePSColor(FILE*, int);

void PSprint_points(PSddp *psa, PSPoint *points, unsigned num, 
                        terminator_type terminator)
{
        int     i,tmpx,tmpy;

        for (i = 0 ; i < num ; ) {
                tmpx = psa->dspace.llx 
                        + (int) (points[i].x * (float) psa->dspace.xspan);
                tmpy = psa->dspace.lly 
                        + (int) (points[i].y * (float) psa->dspace.yspan);
                (void) fprintf(psa->file_pointer, "%d %d ", tmpx, tmpy);
                switch (terminator) {
                        case DO_NOTHING:
                                break;
                        case MOVETO:
                                fprintf(psa->file_pointer, "M ");
                                break;
                        case RMOVETO:
                                fprintf(psa->file_pointer, "Rm ");
                                break;
                        case LINETO:
                                fprintf(psa->file_pointer, "L ");
                                break;
                        case NEW_LINE:
                                fprintf(psa->file_pointer, "\n");
                                break;
                        default:
                                break;
                }
                i++;
                if ( i % POINTS_PER_LINE == 0)
                        fprintf(psa->file_pointer, "\n");
        }
        if (terminator == POLY_POINT) fprintf(psa->file_pointer, "%d P\n",num);
        if (terminator == POLY_FILL) fprintf(psa->file_pointer,"%d N F\n",num);
        if (terminator == POLY_MARKER) fprintf(psa->file_pointer, "%d ",num);
}

/*
 * Set the current dash pattern depending on the line type.
 */
void PSset_dashpattern (PSddp *psa)
{
        int     rscale, dash_size, dot_size, gap_size;
                
        rscale = (int)((1./PS_SCALE)*
                        (MAX(1.,.5*(psa->attributes.linewidth))));
        dash_size = 10 * rscale;
        dot_size = 3 * rscale;
        gap_size = 5 * rscale;
        switch (psa->attributes.linetype) {
        case SOLID_LINE:
                (void) fprintf(psa->file_pointer,"[] 0 D\n");
                break;
        case DASHED_LINE:
                (void) fprintf(psa->file_pointer,"[%d] 0 D\n",
                        dash_size);
                break;
        case DOTTED_LINE:
                (void) fprintf(psa->file_pointer,"[%d] 0 D\n",
                        dot_size);
                break;
        case DASH_DOT_LINE:
                (void) fprintf(psa->file_pointer,"[%d %d %d %d] "
                        "0 D\n", dash_size, gap_size, dot_size,
                        gap_size);
                break;
        case DASH_DOT_DOT_LINE:
                (void) fprintf(psa->file_pointer,"[%d %d %d %d %d %d] "
                        "0 D\n", dash_size, gap_size, dot_size,
                        gap_size, dot_size, gap_size);
                break;
        default:
                (void) fprintf(psa->file_pointer,"[] 0 D\n");
                break;
        }
}

/*
 *  Calculate the current clipping rectangle in PostScript coordinates.
 *
 */
PSClipRect *GetPSClipping (PSddp *psa, CoordSpace s1, CoordSpace s2)
{
        static  PSClipRect      rect;
        CoordSpace      rtmp;

        if ((s2.llx < s1.urx) && (s1.llx < s2.urx) &&
            (s2.lly < s1.ury) && (s1.lly < s2.ury)) {
                rtmp.llx = MAX(s1.llx, s2.llx);
                rtmp.lly = MAX(s1.lly, s2.lly);
                rtmp.urx = MIN(s1.urx, s2.urx);
                rtmp.ury = MIN(s1.ury, s2.ury);
                
                /*
                 *  Convert to workstation viewport coordinates.
                 */
                rtmp.llx = rtmp.llx * (psa->transform).x_scale +
                                (psa->transform).x_trans;
                rtmp.lly = rtmp.lly * (psa->transform).y_scale +
                                (psa->transform).y_trans;
                rtmp.urx = rtmp.urx * (psa->transform).x_scale +
                                (psa->transform).x_trans;
                rtmp.ury = rtmp.ury * (psa->transform).y_scale +
                                (psa->transform).y_trans;
                
                /*
                 *  Calculate the PostScript coordinates.
                 */
                rect.llx = (psa->dspace.llx) +
                   (int)(((float)(psa->dspace.xspan)) * rtmp.llx);
                rect.urx = (psa->dspace.llx) +
                   (int)(((float)(psa->dspace.xspan)) * rtmp.urx);
                rect.lly = (psa->dspace.lly) +
                   (int)(((float)(psa->dspace.yspan)) * rtmp.lly);
                rect.ury = (psa->dspace.lly) +
                   (int)(((float)(psa->dspace.yspan)) * rtmp.ury);
                rect.null = FALSE;

        }
        else
        {
                rect.llx = rect.urx = rect.lly = rect.ury = 0;
                rect.null = TRUE;
        }
        return (&rect);
}

/*
 *  Put out the appropriate clipping rectangle.
 */
void OutputClipping (PSddp *psa, int type)
{
        if (type == DEFAULT_CLIPPING_RECT) {
                (void) fprintf(psa->file_pointer,"%d %d %d %d Oc\n",
                                psa->dspace.llx, psa->dspace.lly,
                                psa->dspace.urx, psa->dspace.ury);
        }
        else {
                if (psa->ps_clip.null == FALSE) {
                        (void) fprintf(psa->file_pointer,"%d %d %d %d Oc\n",
                                psa->ps_clip.llx, psa->ps_clip.lly,
                                psa->ps_clip.urx, psa->ps_clip.ury);
                }
        }
}

/*
 *  Puts a preview bitmap out saying that there is no true preview
 *  bitmap available.
 */
void InsertPreview(FILE *fp)
{
        const   char    *PreviewMap[] = {
                   "7ffffffffffffffffffffffffffffffffffffffffffffffffe",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100403c05ff03fe2e08727f807c13ff8f0ff03c1c0200807",
                   "e0100403c05ff83ff2e0872ff807c13ff8f1ff83e1c0200807",
                   "e0100407e05c7838f2e0873e3c07e103c0f3e783e1c0200807",
                   "e0100407e05c383872e0873c1c07e103c0f387c3f1c0200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e010040ff05c383872e08738080ff103c0f705e3f9c0200807",
                   "e010040f705ff83ff2e08738080e7103c0f705e3ddc0200807",
                   "e010040f705ff03fe2e08738080e7103c0f705e3ddc0200807",
                   "e010041d385fd03f82e08738081c3903c0f705e3cdc0200807",
                   "e010041ff85c103802e0873c1c1ff903c0f385c3cfc0200807",
                   "e010041ff85c103802e0873c1c1ff903c0f3c7c3c7c0200807",
                   "e010043d3c5c103802e0873f7c3c3d03c0f3ef83c3c0200807",
                   "e01004391c5c103802ff872ff83c1d03c0f1ff03c3c0200807",
                   "e01004391c5c103802ff8727f83c1d03c0f0ff03c3c0200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e3f805e10e4e1e0e07f89ffc08ff81c3f81ffc3a009e23cbc7",
                   "e7fc05e10f4e1f0e0ffc9ffc08ffc1c7fc1ffc3a009e21cb87",
                   "ef1e07f10f4e1f0e1f3c81e008e5e1c75c1e3c3a00bf21ef87",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "ee1e07f90fce1fce1e0e81e008e4f1c7401e1c3a00ffa0ff07",
                   "fc1007f90fce1fce3a0f81e008e471c7e01e1c3a00ffa07e07",
                   "fc1007390eee1eee3a0f81e008e471c7fc1ffc3a00f3a07c07",
                   "fc1007390eee1eee3a0f81e008e471c1fe1ffc3a00f3a03c07",
                   "fc100e1d0e6e1e6e3a0f81e008e471c05e1fe43a00e1e03c07",
                   "ee1e0ffd0e7e1e7e1e0e81e008e4f1ce4e1e043a00ffe03807",
                   "ee1e0ffd0e7e1e3e1e1e81e008e4e1ce4e1e043a00ffe03807",
                   "efbe1e1f0e5e1e1e1f7c81e008e7e1c7de1e043a01e1e03807",
                   "e7fc1c0f0e5e1e1e0ff881e008ffc1c3fc1e043fe1c0e03807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e0107c717fc01e08ffc083ff0bff017843ff0e0e0e9ff00807",
                   "e0107c71f1e03f08e3e087ef0b8781fc439f0e0e0ebc780807",
                   "e0107c71e0e03f08e2e087238b8781fc43970e0e0eb8380807",
                   "e0107e71e0e07f88e2e08f200b8781fe43970e0e0eb8380807",
                   "e0107e71c0407f88e3c08e200b8701fe43970ffe0ef0200807",
                   "e0107771c0407388ffc08e3f8bff01ce43ff0ffe0ef0200807",
                   "e0107771c0407388ff808e3f8bfe01ce43fe0ffe0ef0200807",
                   "e0107771c040f1c8e3c08e3f8b8f038743f80e0e0ef0200807",
                   "e01077f1e0e0ffc8e3c08f238b8703ff43900e0e0eb8380807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e01074f1fbe1f1e8e3c087ef8b870787c3900e0e0ebef80807",
                   "e01074f17fc1d0e8e3c083ff8b870703c3900e0e0e9ff00807",
                   "e01074f13fc1d0e8e3e081fd8b878703c3900e0e0e8fe00807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e01e0e0ffc5ff83ff2008e271c7fe1c7cf107ff2e0e0ffc807",
                   "e01e0e0ffc5ff83ff200872e1c7fe1c7ce107ff2e0e0ffc807",
                   "e01e0e0f005c3c380200872e1c7401e7ce100702e0e0e00807",
                   "e01e0e0f005c1c380200872e1c7401e7ce100702e0e0e00807",
                   "e01e0e0f005c1c380200832c1c7401e7ce100702e0e0e00807",
                   "e01ffe0ff85c383fe20083bc1c7fc1eecc100702ffe0ff8807",
                   "e01ffe0ff85ff83fe20083bc1c7fc1eedc100702ffe0ff8807",
                   "e01ffe0ff85ff03fe20083bc1c7fc16edc100702ffe0ff8807",
                   "e01e0e0f005c7838020081f81c74016cdc100702e0e0e00807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e01e0e0f005c3838020080f01c74017cf8100702e0e0e00807",
                   "e01e0e0f005c3838027080f01c74013c78100702e0e0e00807",
                   "e01e0e0ffc5c383ff27080f01c7fe13c78100702e0e0ffc807",
                   "e01e0e0ffc5c3c3ff27080f01c7fe11870100702e0e0ffc807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401ffc0ff08fe07ff2fe81fc17fe071ff0ffe80200807",
                   "e0100401ffc1ff89ff07ff3ff83fe17fe071ff8ffe80200807",
                   "e0100401e3c3f789c700f03c787cf170f071c782e080200807",
                   "e0100401e1c393c98380f038387471707071c782e080200807",
                   "e0100401e1c391c9c200f03c087471707071c782e080200807",
                   "e0100401e1c711e9fa00f03f88e40170e071c782e080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e0100401ffc711e87f80f027f8e4017fc071ff02e080200807",
                   "e0100401fe4711e80780f02078e40171e071fc02e080200807",
                   "e0100401e04391cb8380f03838747170e071c402e080200807",
                   "e0100401e043d3cb8380f03838747170e071c402e080200807",
                   "e0100401e043ff89e780f03e787df170e071c402e080200807",
                   "e0100401e041ff08ff00f02ff83fe170e071c402e080200807",
                   "e0100401e040ff087e00f027e81fc170f071c402e080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e010040100ff11c9c3ff8ff81c1c7ff047ff07fa0080200807",
                   "e010040101ff91c9c3ff8ffc1c1c7ff047ff0ffe0080200807",
                   "e010040103e791c9c2388e3c1c1c070040701f3e0080200807",
                   "e010040103c3d1c9c2388e3c1c1c070040701c1e0080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e01004010741f1c9c2388e3c1c1c070040703c0f0080200807",
                   "e01004010741f1c9c2388ffc1c1c070040703c0f0080200807",
                   "e01004010741f1c9c2388ff81c1c070040703c0f0080200807",
                   "e01004010741f1c9c2388fe01c1c070040703c0f0080200807",
                   "e010040103c1d1c9c2388e201c1c070040701c0e0080200807",
                   "e010040103c3d1cbc2388e201c3c070040701e1e0080200807",
                   "e010040103ef91ff82388e201f7c070040701f7e0080200807",
                   "e010040101ff10ff82388e200ffc070040700ffa0080200807",
                   "e010040100ff107f02388e200ff40700407007fa0080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e01004010040100f038ffc3ff81c7fc7c79004020080200807",
                   "e01004010040100b870ffc3ff81c7fc3c71004020080200807",
                   "e01004010040100b870e803c3c1c7103cf1004020080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e010040100401009860e803c1c1c7101fe1004020080200807",
                   "e010040100401009ce0ff83c381c7fc0fc1004020080200807",
                   "e010040100401009ce0ff83ff81c7fc0f81004020080200807",
                   "e010040100401009ce0ff83ff81c7fc0781004020080200807",
                   "e010040100401008fe0e803c781c7100781004020080200807",
                   "e010040100401008fe0e803c381c7100701004020080200807",
                   "e0100401004010087a0e803c381c7100701004020080200807",
                   "e0100401004010087a0e803c381c7100701004020080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e0100401004010087a0ffc3c3c1c7100701004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e010041ff078381ff23fe07fe8ff83fe47fc3c3a3880200807",
                   "e010041d7078383c7a78f07008e7838e47103c3a3880200807",
                   "e0100419387838383a70f07008c5c30747103c3a3880200807",
                   "e010041d007838383a70f07008e4038047103c3a3880200807",
                   "e010041f8078387802e0807fc8fc03f047fc3c3a3880200807",
                   "e010041ff078387802e0807fc8ff83fe47fc3c3a3880200807",
                   "e0100407f878387802e0807fc83fc1ff47fc3c3a3880200807",
                   "e01004017878387802e080700807c10f47103c3a3880200807",
                   "e0100439387838383a70f07009c5c70747103c3a3880200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e010041f787ef03efa7df07008f7c3cf47103ef23880200807",
                   "e010040ff05ff01ff23fe07fe87f81fe47101ff23fe0200807",
                   "e0100407e04ff00fe21fc07fe83f01fc47100fe23fe0200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e01004010040101c7a3c9ff00ff43ff07ff004020080200807",
                   "e01004010040101c7e7c9ff81ffc3ff07ff004020080200807",
                   "e01004010040101c7e7c9c783e7c3978471004020080200807",
                   "e01004010040101c7e7c9c38383c3938471004020080200807",
                   "e01004010040101c7e7c9c38381c3938471004020080200807",
                   "e01004010040101c7efc9c38781e3970471004020080200807",
                   "e01004010040101c7efc9ff8781e3ff0471004020080200807",
                   "e01004010040101c76dc9ff0781e3fe0471004020080200807",
                   "e01004010040101c76dc9fe0781e39f0471004020080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "e01004010040101c77dc9c203c3c3970471004020080200807",
                   "e01004010040101c77dc9c203efc3970471004020080200807",
                   "e01004010040101c739c9c201ff43970471004020080200807",
                   "e01004010040101c739c9c200ff43978471004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "e0100401004010080200802008040100401004020080200807",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "ffffffffffffffffffffffffffffffffffffffffffffffffff",
                   "7ffffffffffffffffffffffffffffffffffffffffffffffffe"
        };
        int     i;

        for (i = 0; i < 200; i++) {
                (void) fprintf (fp, "%% %s\n", PreviewMap[i]);
        }
}

void PSpreamble (PSddp *psa, preamble_type type)
{
        int     i;
        float   scl = psa->scaling;
        FILE    *fp;

        fp = psa->file_pointer;

        if (type == FOR_FILE) {
                if ((psa->type == EPSF) || (psa->type == EPSI)) {
                        (void) fprintf(fp, "%%!PS-Adobe-2.0 EPSF-2.0\n");
                }
                else {
                        (void) fprintf(fp, "%%!PS-Adobe-2.0\n");
                }
                if (psa->type == RPS) {
                        (void) fprintf(fp, "%%%%Pages: (atend)\n");
                        (void) fprintf(fp, "%%%%PageOrder: Ascend\n");
                }
                (void) fprintf(fp, "%%%%Creator: NCAR GKS\n");
                (void) fprintf(fp, "%%%%CreationDate: %s %s\n",
                                        __DATE__,__TIME__);
                if (psa->type != RPS) {
                  (void) fprintf(psa->file_pointer, 
                              "%%%%BoundingBox: (atend)\n");
                }
                (void) fprintf(fp, "%%%%DocumentFonts: (atend)\n");
                (void) fprintf(fp, "%%%%EndComments\n");

                /*  For an EPSI file, put out a preview bitmap here that 
                 *  contains a message saying that there is no preview 
                 *  bitmap.  A true preview bitmap will be available when 
                 *  a direct raster driver is written.
                 */
                if (psa->type == EPSI) {
                        (void) fprintf(fp, "%%%%BeginPreview: 200 200 1 200\n");
                        InsertPreview(fp);
                        (void) fprintf(fp, "%%%%EndPreview\n");
                }

                /*
                 *  Macro definitions:
                 *  A  --  scale
                 *  B  --  circle marker
                 *  C  --  setlinecap
                 *  D  --  setdash
                 *  E  --  eofill
                 *  F  --  fill area
                 *  G  --  currentrgbcolor
                 *  H  --  currentlinewidth
                 *  I  --  currentdash
                 *  J  --  setlinejoin
                 *  K  --  stroke
                 *  L  --  lineto
                 *  M  --  stroke newpath moveto
                 *  N  --  closepath
                 *  O  --  select color index
                 *  P  --  polyline
                 *  Q  --  plus marker
                 *  R  --  setrgbcolor
                 *  S  --  stroke showpage
                 *  T  --  define color table value
                 *  U  --  put
                 *  V  --  asterisk marker
                 *  W  --  setlinewidth
                 *  X  --  cross marker
                 *  Y  --  show text vertically
                 *  Z  --  rotate
                 *  As --  ashow
                 *  Bg --  put out background
                 *  Cc --  Colored cell array (with simulation for B&W)
                 *  Ch --  clippath
                 *  Ci --  colorimage
                 *  Cm --  currentmatrix
                 *  Co --  concat
                 *  Cp --  clip
                 *  Ct --  currentpoint
                 *  Cu --  currentfile
                 *  Ex --  exch
                 *  Fs --  select a PS font at a specified size
                 *  Gs --  gsave
                 *  Gr --  grestore
                 *  Im --  image
                 *  In --  initgraphics
                 *  Ld --  prologue for landscape mode
                 *  Ls --  locally-defined save of graphics state on the 
                 *         operand stack
                 *  Lr --  restore used with Ls
                 *  M3 --  find the mimimum of three numbers
                 *  Ml --  setmiterlimit
                 *  Mx --  matrix
                 *  Ng --  neg
                 *  Np --  newpath
                 *  Oc --  output new clipping path (usage: llx lly urx ury Oc))
                 *  Oh --  horizontal outline text (usage: char_space string Oh)
                 *  Ov --  vertical outline text (usage: char_space string Ov)
                 *  Pt --  prologue for portrait mode
                 *  Rh --  readhexstring
                 *  Rm --  rmoveto
                 *  Sc --  invoke setcmykcolor instead of setrgbcolor
                 *  Sm --  setmatrix
                 *  Tr --  translate
                 */
                (void) fprintf(fp, "%%%%BeginProlog\n\n");
                
                /* Initialize variables only for EPS */
                if ((psa->type == EPSF) || (psa->type == EPSI)) {
                        (void) fprintf(fp, "/n 0 def\n");
                        (void) fprintf(fp, "/s 0 def\n");
                        (void) fprintf(fp, "/ms 0 def\n");
                        (void) fprintf(fp, "/ts 0 def\n");
                        (void) fprintf(fp, "/xt 0 def\n");
                        (void) fprintf(fp, "/yt 0 def\n");
                        (void) fprintf(fp, "/rd 0 def\n");
                        (void) fprintf(fp, "/gr 0 def\n");
                        (void) fprintf(fp, "/bl 0 def\n");
                        (void) fprintf(fp, "/strn 0 def\n");
                        (void) fprintf(fp, "/str1 0 def\n");
                        (void) fprintf(fp, "/tstr 0 def\n");
                        (void) fprintf(fp, "/llx 0 def\n");
                        (void) fprintf(fp, "/lly 0 def\n");
                        (void) fprintf(fp, "/urx 0 def\n");
                        (void) fprintf(fp, "/ury 0 def\n");
                        (void) fprintf(fp, "/chr 0 def\n");
                        (void) fprintf(fp, "/code 0 def\n");
                        (void) fprintf(fp, "/CellWidth 0 def\n");
                        (void) fprintf(fp, "/ColorStr 0 def\n");
                        (void) fprintf(fp, "/YIndx 0 def\n");
                        (void) fprintf(fp, "/GrayVal 0 def\n");
                        (void) fprintf(fp, "/ispc 0 def\n\n");
                }

                (void) fprintf(fp, "/M3\n");
                (void) fprintf(fp, "{\n");
                (void) fprintf(fp, "  2 copy lt {pop} {exch pop} ifelse\n");
                (void) fprintf(fp, "  2 copy lt {pop} {exch pop} ifelse\n");
                (void) fprintf(fp, "} bind def\n");

                (void) fprintf(fp, "/Sc\n");
                (void) fprintf(fp, "{\n");
                (void) fprintf(fp, "  1. exch sub /ylo exch def\n");
                (void) fprintf(fp, "  1. exch sub /mag exch def\n");
                (void) fprintf(fp, "  1. exch sub /cyn exch def\n");
                (void) fprintf(fp, "  ylo mag cyn M3 /b_cmy exch def\n");
                (void) fprintf(fp, "  cyn b_cmy sub /c_cmy exch def\n");
                (void) fprintf(fp, "  mag b_cmy sub /m_cmy exch def\n");
                (void) fprintf(fp, "  ylo b_cmy sub /y_cmy exch def\n");
                (void) fprintf(fp, "  c_cmy m_cmy y_cmy b_cmy setcmykcolor\n");
                (void) fprintf(fp, "} bind def\n");

                (void) fprintf(fp, "/P\n");
                (void) fprintf(fp, "{/n exch def\n");
                (void) fprintf(fp, "  newpath\n");
                (void) fprintf(fp, "  moveto\n");
                (void) fprintf(fp, "  2 1 n {pop lineto} for\n");
                (void) fprintf(fp, "  stroke\n");
                (void) fprintf(fp, " } bind def\n\n");
        
                (void) fprintf(fp, "/F\n");
                (void) fprintf(fp, "{/n exch def\n");
                (void) fprintf(fp, "  newpath\n");
                (void) fprintf(fp, "  moveto\n");
                (void) fprintf(fp, "  2 1 n {pop lineto} for\n");
                (void) fprintf(fp, "  closepath\n");
                (void) fprintf(fp, "  eofill\n");
                (void) fprintf(fp, "} bind def\n\n");

                (void) fprintf(fp, "/B\n");
                (void) fprintf(fp, "{ /s exch def\n");
                (void) fprintf(fp, "  /n exch def\n");
                (void) fprintf(fp, "  1 1 n {newpath pop "
                                "s 0 360 arc stroke} for\n");
                (void) fprintf(fp, "} bind def\n\n");

                (void) fprintf(fp, "/Q\n");
                (void) fprintf(fp, "{ /s exch def\n");
                (void) fprintf(fp, "  /n exch def\n");
                (void) fprintf(fp, "  /ms 0 s sub def\n");
                (void) fprintf(fp, "  /ts s s add def\n");
                (void) fprintf(fp, "  1 1 n {gsave newpath pop translate\n"
                                   "  0 0 moveto ms 0 rmoveto ts 0 rlineto\n"
                                   "  0 0 moveto 90 rotate ms"
                                   "  0 rmoveto ts 0 rlineto\n"
                                   "  stroke grestore} for\n");
                (void) fprintf(fp, "} bind def\n\n");

                (void) fprintf(fp, "/Fs\n");
                (void) fprintf(fp, "{ /s exch def\n");
                (void) fprintf(fp, "  findfont\n");
                (void) fprintf(fp, "  s scalefont\n");
                (void) fprintf(fp, "  setfont\n");
                (void) fprintf(fp, "} bind def\n\n");

                (void) fprintf(fp, "/V\n");
                (void) fprintf(fp, "{ /s exch def\n");
                (void) fprintf(fp, "  /n exch def\n");
                (void) fprintf(fp, "  /ms 0 s sub def\n");
                (void) fprintf(fp, "  /ts s s add def\n");
                (void) fprintf(fp, "  1 1 n {gsave newpath pop translate\n");
                (void) fprintf(fp, "  0 0 moveto 30 rotate ms 0 rmoveto"
                                   "  ts 0 rlineto\n");
                (void) fprintf(fp, "  0 0 moveto 60 rotate ms 0 rmoveto"
                                   "  ts 0 rlineto\n");
                (void) fprintf(fp, "  0 0 moveto 60 rotate ms 0 rmoveto"
                                   "  ts 0 rlineto\n");
                (void) fprintf(fp, "  stroke grestore} for\n");
                (void) fprintf(fp, "} bind def\n\n");

                (void) fprintf(fp, "/X\n");
                (void) fprintf(fp, "{ /s exch def\n");
                (void) fprintf(fp, "  /n exch def\n");
                (void) fprintf(fp, "  /s s 1.25 mul def\n");
                (void) fprintf(fp, "  /ms 0 s sub def\n");
                (void) fprintf(fp, "  /ts s s add def\n");
                (void) fprintf(fp, "  1 1 n {gsave newpath pop translate\n");
                (void) fprintf(fp, "  0 0 moveto 45 rotate ms 0 rmoveto"
                                   "  ts 0 rlineto\n");
                (void) fprintf(fp, "  0 0 moveto 90 rotate ms 0 rmoveto"
                                   "  ts 0 rlineto\n");
                (void) fprintf(fp, "  stroke grestore} for\n");
                (void) fprintf(fp, "} bind def\n\n");

                (void) fprintf(fp, "/Ls\n");
                (void) fprintf(fp, "{matrix currentmatrix\n");
                if (c_model != 0) {
                  (void) fprintf(fp, " currentrgbcolor\n");
                }
                else {
                  (void) fprintf(fp, " currentcmykcolor\n");
                }
                (void) fprintf(fp, " currentlinewidth\n");
                (void) fprintf(fp, " currentlinecap\n");
                (void) fprintf(fp, " currentlinejoin\n");
                (void) fprintf(fp, " currentdash\n");
                (void) fprintf(fp, "} bind def\n\n");

                (void) fprintf(fp, "/Lr\n");
                (void) fprintf(fp, "{setdash\n");
                (void) fprintf(fp, " setlinejoin\n");
                (void) fprintf(fp, " setlinecap\n");
                (void) fprintf(fp, " setlinewidth\n");
                if (c_model != 0) {
                  (void) fprintf(fp, " setrgbcolor\n");
                }
                else {
                  (void) fprintf(fp, " setcmykcolor\n");
                }
                (void) fprintf(fp, " setmatrix\n");
                (void) fprintf(fp, "} bind def\n\n");

                (void) fprintf(fp, "/M {stroke newpath moveto} bind def\n");
                (void) fprintf(fp, "/L {lineto} bind def\n");
                (void) fprintf(fp, "/E {eofill} def\n");
                (void) fprintf(fp, "/J {setlinejoin} def\n");
                (void) fprintf(fp, "/C {setlinecap} def\n");
                (void) fprintf(fp, "/W {setlinewidth} def\n");
                (void) fprintf(fp, "/S {stroke showpage} def\n");
                (void) fprintf(fp, "/A {scale} def\n");
                (void) fprintf(fp, "/U {put} def\n");
                (void) fprintf(fp, "/D {setdash} def\n");
                (void) fprintf(fp, "/I {currentdash} def\n");
                (void) fprintf(fp, "/H {currentlinewidth} def\n");
                (void) fprintf(fp, "/G {currentrgbcolor} def\n");
                if (c_model != 0) {
                  (void) fprintf(fp, "/R {setrgbcolor} def\n");
                }
                else {
                  (void) fprintf(fp, "/R {Sc} def\n");
                }
                (void) fprintf(fp, "/N {closepath} def\n");
                (void) fprintf(fp, "/K {stroke} def\n");
                (void) fprintf(fp, "/Z {rotate} def\n");
                (void) fprintf(fp, "/As {ashow} def\n");
                (void) fprintf(fp, "/Ch {clippath} def\n");
                (void) fprintf(fp, "/Ci {colorimage} def\n");
                (void) fprintf(fp, "/Co {concat} def\n");
                (void) fprintf(fp, "/Cm {currentmatrix} def\n");
                (void) fprintf(fp, "/Cp {clip} def\n");
                (void) fprintf(fp, "/Ct {currentpoint} def\n");
                (void) fprintf(fp, "/Cu {currentfile} def\n");
                (void) fprintf(fp, "/Ex {exch} def\n");
                (void) fprintf(fp, "/Im {image} def\n");
                (void) fprintf(fp, "/In {initgraphics} def\n");
                (void) fprintf(fp, "/Ml {setmiterlimit} def\n");
                (void) fprintf(fp, "/Mx {matrix} def\n");
                (void) fprintf(fp, "/Np {newpath} def\n");
                (void) fprintf(fp, "/Ng {neg} def\n");
                (void) fprintf(fp, "/Rh {readhexstring} def\n");
                (void) fprintf(fp, "/Rm {rmoveto} def\n");
                (void) fprintf(fp, "/Sm {setmatrix} def\n");
                (void) fprintf(fp, "/Tr {translate} def\n");
                (void) fprintf(fp, "/Gs {gsave} def\n");
                (void) fprintf(fp, "/Gr {grestore} def\n\n");

                (void) fprintf(fp, "/Ov\n");
                (void) fprintf(fp, "{/strn exch def\n");
                (void) fprintf(fp, " /ispc exch def\n");
                (void) fprintf(fp, " /str1 1 string def\n");
                (void) fprintf(fp, "  strn\n");
                (void) fprintf(fp, "  {/code exch def\n");
                (void) fprintf(fp, "   /chr str1 0 code put str1 def\n");
                (void) fprintf(fp, "   0 ispc neg rmoveto\n");
                (void) fprintf(fp, "   gsave\n");
                (void) fprintf(fp, "   chr stringwidth pop 2 div neg\n");
                (void) fprintf(fp, "   0 rmoveto\n");
                (void) fprintf(fp, "   chr false charpath stroke\n");
                (void) fprintf(fp, "   grestore\n");
                (void) fprintf(fp, "  } forall\n");
                (void) fprintf(fp, "} bind def\n\n");

                (void) fprintf(fp, "/Oh\n");
                (void) fprintf(fp, "{/strn exch def\n");
                (void) fprintf(fp, " /ispc exch def\n");
                (void) fprintf(fp, " /tstr 1 string def\n");
                (void) fprintf(fp, " strn\n");
                (void) fprintf(fp, "   {\n");
                (void) fprintf(fp, "    /code exch def\n");
                (void) fprintf(fp, "    /chr tstr 0 code put tstr def\n");
                (void) fprintf(fp, "    chr false charpath\n");
                (void) fprintf(fp, "    currentpoint\n");
                (void) fprintf(fp, "    /yt exch def /xt exch def\n");
                (void) fprintf(fp, "    stroke\n");
                (void) fprintf(fp, "    xt ispc add yt moveto\n");
                (void) fprintf(fp, "   } forall\n");
                (void) fprintf(fp, " } bind def\n\n");

                (void) fprintf(fp, "/T 256 array def\n");
                (void) fprintf(fp, "/O\n");
                (void) fprintf(fp, "{ T\n");
                (void) fprintf(fp, "  exch\n");
                (void) fprintf(fp, "  get\n");
                (void) fprintf(fp, "  aload\n");
                (void) fprintf(fp, "  pop\n");
                if (c_model != 0) {
                  (void) fprintf(fp, "  setrgbcolor\n");
                }
                else {
                  (void) fprintf(fp, "  Sc\n");
                }
                (void) fprintf(fp, "} bind def\n\n");

                (void) fprintf(fp, "/Y\n");
                (void) fprintf(fp, "{/strn exch def\n");
                (void) fprintf(fp, " /ispc exch def\n");
                (void) fprintf(fp, " /str1 1 string def\n");
                (void) fprintf(fp, "  strn\n");
                (void) fprintf(fp, "  {/code exch def\n");
                (void) fprintf(fp, "   /chr str1 0 code put str1 def\n");
                (void) fprintf(fp, "    0 ispc neg rmoveto\n");
                (void) fprintf(fp, "    gsave\n");
                (void) fprintf(fp, "      chr stringwidth pop "
                                           "2 div neg 0 rmoveto\n");
                (void) fprintf(fp, "      chr show\n");
                (void) fprintf(fp, "    grestore\n");
                (void) fprintf(fp, "  } forall\n");
                (void) fprintf(fp, "} def\n\n");

                (void) fprintf(fp, "/Bg\n");
                (void) fprintf(fp, "{/ury Ex def /urx Ex def ");
                (void) fprintf(fp, "/lly Ex def /llx Ex def\n");
                (void) fprintf(fp, " Np Ls Gr Gs 0 O Ch E Lr\n");
                (void) fprintf(fp, " llx lly M urx lly L urx ury L\n");
                (void) fprintf(fp, " llx ury L llx lly L\n");
                (void) fprintf(fp, " Cp Np\n");
                (void) fprintf(fp, "} def\n\n");

                (void) fprintf(fp, "/Oc\n");
                (void) fprintf(fp, "{/ury exch def\n");
                (void) fprintf(fp, " /urx exch def\n");
                (void) fprintf(fp, " /lly exch def\n");
                (void) fprintf(fp, " /llx exch def\n");
                (void) fprintf(fp, " Np\n");
                (void) fprintf(fp, " llx lly M urx lly L urx ury L ");
                (void) fprintf(fp, " llx ury L llx lly L\n");
                (void) fprintf(fp, " Cp Np\n");
                (void) fprintf(fp, "} def\n\n");

                (void) fprintf(fp,"/Cc\n");
                (void) fprintf(fp,"{/CellWidth exch def\n");
                (void) fprintf(fp, " /strn CellWidth 1 add string def\n");
                (void) fprintf(fp, 
                        " /strn1 CellWidth 1 add 3 mul string def\n");
                (void) fprintf(fp, " systemdict /colorimage known\n");
                (void) fprintf(psa->file_pointer,
                        " { { Cu strn1 Rh pop } false 3 Ci}\n");
                (void) fprintf(fp," { { Cu strn1 Rh pop\n");
                (void) fprintf(fp,"     /ColorStr exch def\n");
                (void) fprintf(fp,"     0 1 CellWidth {\n");
                (void) fprintf(fp,"       /YIndx exch def\n");
                (void) fprintf(fp, "       /GrayVal YIndx 3 mul def\n");
                (void) fprintf(fp,"       strn YIndx\n");
                (void) fprintf(fp,
                        "         ColorStr GrayVal       get 0.30 mul\n");
                (void) fprintf(fp,
                        "         ColorStr GrayVal 1 add get 0.59 mul\n");
                (void) fprintf(fp,
                        "         ColorStr GrayVal 2 add get 0.11 mul\n");
                (void) fprintf(fp,"         add add cvi\n");
                (void) fprintf(fp,"       put\n");
                (void) fprintf(fp,"     } for\n");
                (void) fprintf(fp,"     strn\n");
                (void) fprintf(fp,"   } Im\n");
                (void) fprintf(fp," } ifelse\n");
                (void) fprintf(fp,"} def\n\n");

                (void) fprintf(fp,"/Ld\n");
                (void) fprintf(fp,"{/ury exch def /urx exch def ");
                (void) fprintf(fp," /lly exch def /llx exch def\n");
                (void) fprintf(fp," -90 rotate\n");
                (void) fprintf(fp," lly urx add neg llx lly sub translate\n");
                (void) fprintf(fp,"} bind def\n");

                (void) fprintf(fp,"/Pt\n");
                (void) fprintf(fp,"{/ury exch def /urx exch def ");
                (void) fprintf(fp," /lly exch def /llx exch def\n");
                (void) fprintf(fp," 90 rotate\n");
                (void) fprintf(fp," lly urx add lly llx sub translate\n");
                (void) fprintf(fp,"} bind def\n");

                (void) fprintf(fp, "%%%%EndProlog\n\n");

/*
 *  Put out page size information only for regulare PS files.
 */
                if (psa->type == RPS) {

                  (void)fprintf(fp, "%%%%BeginSetup\n");
                  (void)fprintf(fp, "<</PageSize [%05d %05d]>> setpagedevice\n",
                                       psa->paper_width, psa->paper_height);
                  (void) fprintf(fp, "%%%%EndSetup\n\n");
                }

                DefaultColorTable(psa);


        }
        else if (type == FOR_PICTURE) {
                if (psa->type == RPS) {
                        (void) fprintf(fp, "%%%%Page: %d %d\n", 
                                psa->page_number, psa->page_number);
                }
                psa->pict_empty = FALSE;
                (void) fprintf(fp, "%.5f %.5f A ", scl, scl);
                (void) fprintf(fp, "%d J ",psa->line_join);
                (void) fprintf(fp, "%d C ",psa->line_cap);
                if (psa->line_join == MITER) {
                        (void) fprintf(fp, "%.1f Ml ",psa->miter_limit);
                }
                psa->attributes.linewidth_set = 
                                (int) ((psa->nominal_width_scale) *
                                (psa->attributes.linewidth)/(psa->scaling));
                (void) fprintf(fp, "%d W ", psa->attributes.linewidth_set);
                PSset_dashpattern (psa);

                /*  Issue user-defined colors */
                for (i = 0; i < MAX_COLORS; i++) {
                        if (psa->color_map[i+3*MAX_COLORS] != 0) {
                                (void) fprintf(psa->file_pointer,
                                        "T %3d [%5.3f %5.3f %5.3f] U\n",i, 
                                        psa->color_map[3*i  ],
                                        psa->color_map[3*i+1],
                                        psa->color_map[3*i+2]);
                        }
                }

                if (psa->orientation == LANDSCAPE) {
                        (void) fprintf(fp,"%d %d %d %d Ld\n",
                                psa->dspace.llx, psa->dspace.lly,
                                psa->dspace.urx, psa->dspace.ury);
                }

                /*  Put out the default clipping rectangle and save it */
                OutputClipping (psa, DEFAULT_CLIPPING_RECT);
                (void) fprintf(fp, "Gs\n");

                if (psa->attributes.clip_ind == CLIPPING_ON) {
                        OutputClipping (psa, PS_CLIPPING_RECT);
                }

                if (psa->background && (psa->suppress_flag != 1) &&
                                       (psa->suppress_flag != 2)) {
                        PSbackground(psa);
                }
                if (psa->color == MONO) {   /* Use foreground color for mono */
                        (void) fprintf(fp, "1 O\n");
                }
                else {
                    /*****RLB
                        (void) fprintf(fp,"%d O\n", 
                                psa->attributes.ps_colr_ind);
                     *****/
                    writePSColor(fp, psa->attributes.ps_colr_ind);
                }
        }
}

/*
 * Color the background.
 */
void PSbackground (PSddp *psa)
{
        /*
         *  The logic of Bg is complicated by the fact that one cannot
         *  increase the size of the clip rectangle without calling
         *  initclip, and this command is not allowed in EPSFs.  We
         *  have to restore to a graphics state that has the full 
         *  clipping rectangle.
         */

        if ((psa->type == RPS) && (psa->full_background != FALSE)) {
                (void) fprintf(psa->file_pointer,
                        "Gs In Ch 0 O E K Np Gr\n");
        }
        else if (psa->attributes.clip_ind == CLIPPING_ON) {
                (void) fprintf(psa->file_pointer,"%d %d %d %d Bg\n",
                        psa->ps_clip.llx, psa->ps_clip.lly,
                        psa->ps_clip.urx, psa->ps_clip.ury);
        }
        else { 
                (void) fprintf(psa->file_pointer,"%d %d %d %d Bg\n",
                        psa->dspace.llx, psa->dspace.lly,
                        psa->dspace.urx, psa->dspace.ury);
        }
}

/*
 *  Put out the default color table.
 */
void DefaultColorTable(PSddp *psa)
{
        int i;
        char tmpstr[9], *strng;
        const char *color_values[MAX_COLORS] = {
           "1. 1. 1." , "0. 0. 0." , "1. 0. 0." , "0. 1. 0." , "0. 0. 1." , 
           "0. 1. 1." , "1. 0. .8" , "1. 1. 0." , "1. .5 0." , ".6 .8 0." , 
           "0. 1. .6" , "0. .5 1." , ".5 0. .8" , "1. 0. .6" , ".3 .3 .3" , 
           ".7 .7 .7" , "1. 1. .3" , ".7 1. .5" , ".5 1. .6" , ".2 1. .7" ,
           ".3 .8 .8" , ".5 .7 .8" , ".7 .5 .8" , "1. .3 .9" , ".7 .9 .5" , 
           ".4 .9 .5" , ".2 .9 .7" , ".2 .7 .9" , ".2 .5 1." , ".5 .3 1." , 
           ".7 .2 1." , ".9 .1 1." , ".9 1. .2" , ".7 1. .3" , ".5 1. .3" , 
           ".2 1. .5" , ".2 .8 .6" , ".2 .7 .7" , ".2 .5 .8" , ".2 .4 .9" ,
           ".4 .3 .9" , ".7 .2 .9" , ".8 .2 .8" , ".9 .3 .7" , ".8 .4 .6" , 
           ".8 .6 .5" , ".9 .7 .4" , ".9 .7 .3" , "1. .9 .9" , ".8 1. .1" , 
           ".6 1. .2" , ".5 1. .2" , ".2 .9 .5" , ".2 .8 .5" , ".2 .7 .7" , 
           ".2 .5 .7" , ".2 .4 .9" , ".4 .3 .9" , ".5 .2 .9" , ".8 .2 .7" ,
           "1. .2 .7" , "1. .3 .6" , "1. .4 .5" , "1. .5 .4" , "1. .8 .1" , 
           ".7 1. .0" , ".6 1. .1" , ".4 1. .2" , ".1 1. .3" , ".1 .8 .4" , 
           ".2 .7 .5" , ".1 .6 .7" , ".1 .5 .7" , ".1 .4 .8" , ".2 .3 .9" , 
           ".2 .2 1." , ".3 .1 1." , ".5 .1 .9" , ".7 .0 .8" , ".9 .0 .7" ,
           ".9 .7 .1" , ".7 .9 .1" , ".5 .9 .1" , ".3 .9 .2" , ".1 .9 .3" , 
           ".2 .7 .4" , ".1 .7 .5" , ".1 .5 .6" , ".1 .4 .7" , ".2 .3 .7" , 
           ".2 .2 .8" , ".3 .2 .8" , ".5 .1 .7" , ".7 .1 .7" , ".9 .2 .5" , 
           ".8 .2 .5" , ".8 .7 .0" , ".6 .8 .1" , ".4 .9 .1" , ".3 .9 .1" ,
           ".1 .9 .2" , ".2 .8 .2" , ".2 .7 .3" , ".2 .6 .5" , ".1 .5 .5" , 
           ".1 .4 .6" , ".2 .3 .7" , ".3 .2 .7" , ".4 .2 .6" , ".7 .2 .5" , 
           ".8 .2 .4" , "1. .2 .3" , ".7 .7 .0" , ".5 .7 .1" , ".4 .8 .1" , 
           ".2 .9 .1" , ".2 .9 .1" , ".2 .8 .2" , ".1 .7 .2" , ".1 .7 .3" ,
           ".1 .6 .4" , ".1 .4 .5" , ".1 .4 .5" , ".2 .3 .5" , ".4 .3 .4" , 
           ".5 .3 .4" , ".7 .3 .3" , ".8 .3 .2" , ".7 .6 .0" , ".5 .7 .1" , 
           ".4 .7 .1" , ".2 .7 .2" , ".1 .7 .2" , ".1 .6 .3" , ".1 .5 .4" , 
           ".1 .4 .4" , ".1 .3 .5" , ".1 .2 .7" , ".1 .1 .7" , ".4 .1 .5" ,
           ".5 .1 .5" , ".6 .1 .4" , ".7 .1 .4" , ".9 .1 .3" , ".5 .5 .1" , 
           ".4 .6 .1" , ".3 .6 .1" , ".2 .6 .2" , ".1 .5 .2" , ".1 .4 .3" , 
           ".1 .4 .4" , ".2 .3 .4" , ".2 .2 .4" , ".2 .2 .5" , ".2 .1 .5" , 
           ".3 .1 .5" , ".4 .1 .4" , ".5 .1 .4" , ".7 .1 .3" , ".8 .1 .2" ,
           ".5 .4 .1" , ".4 .5 .1" , ".3 .6 .0" , ".2 .6 .1" , ".1 .7 .1" , 
           ".1 .6 .1" , ".1 .5 .2" , ".1 .4 .2" , ".1 .4 .2" , ".1 .3 .3" , 
           ".1 .2 .4" , ".1 .2 .5" , ".1 .1 .5" , ".1 .0 .6" , ".3 .0 .5" , 
           ".5 .0 .4" , ".7 .3 .0" , ".7 .4 .0" , ".5 .4 .1" , ".3 .4 .2" ,
           ".2 .3 .2" , ".2 .2 .3" , ".2 .2 .4" , ".3 .1 .4" , ".5 .1 .3" , 
           ".6 .1 .2" , ".7 .1 .2" , ".8 .0 .2" , "1. .0 .2" , "1. .1 .1" , 
           ".9 .2 .1" , ".8 .2 .1" , ".6 .2 .0" , ".4 .4 .0" , ".2 .4 .1" , 
           ".2 .3 .2" , ".2 .2 .2" , ".2 .2 .3" , ".2 .1 .3" , ".3 .1 .3" ,
           ".5 .1 .2" , ".7 .1 .2" , ".8 .0 .2" , ".9 .0 .1" , "1. .0 .1" , 
           "1. .1 .1" , ".8 .2 .0" , ".7 .2 .0" , ".5 .2 .0" , ".4 .3 .0" , 
           ".1 .4 .1" , ".1 .4 .1" , ".1 .3 .2" , ".1 .2 .2" , ".1 .2 .3" , 
           ".2 .1 .4" , ".2 .0 .3" , ".4 .0 .2" , ".6 .0 .2" , ".7 .0 .1" ,
           ".8 .0 .1" , ".7 .1 .1" , ".7 .1 .1" , ".6 .2 .0" , ".4 .2 .0" , 
           ".2 .3 .0" , ".1 .3 .0" , ".1 .2 .1" , ".1 .2 .1" , ".1 .1 .2" , 
           ".1 .1 .3" , ".1 .0 .3" , ".1 .0 .2" , ".2 .0 .2" , ".3 .0 .2" , 
           ".3 .0 .1" , ".4 .0 .1" , ".4 .1 .1" , ".4 .1 .0" , ".2 .1 .0" ,
           ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , 
           ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , 
           ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , ".0 .0 .0" , 
           ".0 .0 .0"
        };

        for (i = 0; i < MAX_COLORS; i++) {
                strcpy(tmpstr, color_values[i]);
                fprintf(psa->file_pointer, "[%s] ",color_values[i]);
                if ((i > 0) && ((i+1) % 7) == 0) {
                        fprintf(psa->file_pointer, "\n");
                }

                /* Store the default values in a local color table */
                strng = strtok(tmpstr, " ");
                psa->color_map[3*i] = atof(strng);
                strng = strtok((char *) NULL, " ");
                psa->color_map[3*i+1] = atof(strng);
                strng = strtok((char *) NULL, " ");
                psa->color_map[3*i+2] = atof(strng);
                /* Last part of color_map is flags to indicate if user-set */
                psa->color_map[i + (3*MAX_COLORS)] = 0;
        }
        fprintf(psa->file_pointer, " T astore pop\n");
}

/*
 *  Initialize local data.
 */
static void PSinit(PSddp *psa, int *coords)
{
        int     i, cllx, clly, curx, cury;
        float   rscale;

        psa->hatch_spacing = PS_HATCH_SPACING;
        psa->stack_size = MAX_STACK;
        psa->path_size = MAX_PATH;
        psa->line_join = ROUND;
        psa->line_cap = ROUNDED;
        psa->nominal_width_scale = .5;
        psa->full_background = FALSE;
        psa->suppress_flag = SUPPRESS_FLAG;
        psa->miter_limit = MITER_LIMIT_DEFAULT;
        psa->sfill_spacing = PS_FILL_SPACING;

        /*
         *  Flag to suppress putting out background color rectangle.
         */
        psa->suppress_flag = *(coords+6);

        /*
         *  Color model flag is in *(coords+5); set the global
         *  variable c_model to its value.
         */
        c_model = *(coords+5);

        /*
         *  Coordinate initializations (the scale factor is in *(coords+4)).
         */
        rscale = (float) *(coords+4);
        if (rscale > 0.) {
                psa->scaling = 1./rscale;       
        }
        else {
                psa->scaling = PS_SCALE;
                rscale = 1./PS_SCALE;
        }
        cllx = *coords;
        clly = *(coords+1);
        curx = *(coords+2);
        cury = *(coords+3);
        if ((cllx != -9999) && (clly != -9999) && 
                (curx != -9999) && (cury != -9999)) {
                psa->dspace.llx = (int) (((float) cllx) * rscale);
                psa->dspace.urx = (int) (((float) curx) * rscale);
                psa->dspace.lly = (int) (((float) clly) * rscale);
                psa->dspace.ury = (int) (((float) cury) * rscale);
        }
        else {
                psa->dspace.llx = (int) (((float) LLX_DEFAULT) * rscale);
                psa->dspace.urx = (int) (((float) URX_DEFAULT) * rscale);
                psa->dspace.lly = (int) (((float) LLY_DEFAULT) * rscale);
                psa->dspace.ury = (int) (((float) URY_DEFAULT) * rscale);
        }
        psa->dspace.xspan = ((psa->dspace.urx) - (psa->dspace.llx));
        psa->dspace.yspan = ((psa->dspace.ury) - (psa->dspace.lly));

        psa->ps_clip.llx = psa->dspace.llx;
        psa->ps_clip.lly = psa->dspace.lly;
        psa->ps_clip.urx = psa->dspace.urx;
        psa->ps_clip.ury = psa->dspace.ury;
        psa->ps_clip.null = FALSE;

        psa->background = FALSE;
        psa->pict_empty = TRUE;
        psa->page_number = 1;
        
        psa->paper_width = *(coords+10);
        psa->paper_height = *(coords+11);

        for (i = 0; i < NUM_PS_FONTS; i++) {
                psa->fonts_used[i] = 0;
        };

        psa->attributes.linetype         = LINETYPE_DEFAULT;
        psa->attributes.linetype_set     = LINETYPE_DEFAULT;
        psa->attributes.linewidth        = LINEWIDTH_DEFAULT;
        psa->attributes.linewidth_set    = 
                (int) ((psa->nominal_width_scale) *
                (psa->attributes.linewidth)/(psa->scaling));
        psa->attributes.linewidth_set    = LINEWIDTH_DEFAULT;
        psa->attributes.line_colr_ind    = LINE_COLR_DEFAULT;
        psa->attributes.marker_type      = MARKER_TYPE_DEFAULT;
        psa->attributes.marker_size      = MARKER_SIZE_DEFAULT;
        psa->attributes.marker_colr_ind  = MARKER_COLR_IND_DEFAULT;
        psa->attributes.text_font        = TEXT_FONT_DEFAULT;
        psa->attributes.text_prec        = TEXT_PREC_DEFAULT;
        psa->attributes.char_expan       = CHAR_EXPAN_DEFAULT;
        psa->attributes.char_space       = CHAR_SPACE_DEFAULT;
        psa->attributes.text_colr_ind    = TEXT_COLR_IND_DEFAULT;
        psa->attributes.char_ht          = CHAR_HT_DEFAULT;
        psa->attributes.char_up_vec_x    = CHAR_UP_VEC_X_DEFAULT;
        psa->attributes.char_up_vec_y    = CHAR_UP_VEC_Y_DEFAULT;
        psa->attributes.char_base_vec_x  = CHAR_BASE_VEC_X_DEFAULT;
        psa->attributes.char_base_vec_y  = CHAR_BASE_VEC_y_DEFAULT;
        psa->attributes.text_path        = TEXT_PATH_DEFAULT;
        psa->attributes.text_align_horiz = TEXT_ALIGN_HORIZ_DEFAULT;
        psa->attributes.text_align_vert  = TEXT_ALIGN_VERT_DEFAULT;
        psa->attributes.fill_int_style   = FILL_INT_STYLE_DEFAULT;
        psa->attributes.fill_style_ind   = FILL_STYLE_IND_DEFAULT;
        psa->attributes.fill_colr_ind    = FILL_COLR_IND_DEFAULT;
        psa->attributes.ps_colr_ind      = PS_COLR_IND_DEFAULT;
        psa->attributes.clip_ind         = CLIP_IND_DEFAULT;

}

/*
 *  Given an NCAR font number and a character, return a structure giving the
 *  equivalent PostScript font number (as per the association given below), 
 *  character number, and normalized character width.
 *
 *  The function returns a zero if the character is available and returns
 *  a -1 if it is not.  If the character is not availavble, the description
 *  for a filled Times-Roman asterisk is returned in psfc.
 */
static int MapFonts (PSddp *psa, int cnum, PSCharInfo *psfc)

{

        /*
         *  Metrics for the standard thirteen PostScript fonts:
         *
         *    Times-Roman
         *    Times-Bold
         *    Times-Italic
         *    Times-BoldItalic
         *    Helvetica 
         *    Helvetica-Bold
         *    Helvetica-Oblique
         *    Helvetica-BoldOblique
         *    Courier
         *    Courier-Bold
         *    Courier-Oblique
         *    Courier-BoldOblique
         *    Symbol
         *
         *  In the PSFontMetrics array, for each of the thirteen fonts, 
         *  the first 96 elements are character widths for character 
         *  numbers 32-127; the second 96 elements are widths for 
         *  character numbers 160-255; the final element is the nominal 
         *  font height (the height of an "X" for all but the Symbol font).  
         *  All of the numbers are based on PostScript fonts normalized 
         *  by a scale of 1000.
         */
        const int PSFontMetrics[NUM_PS_FONTS][193] =
          {
            /* Times-Roman  */
            {
              250, 333, 408, 500, 500, 833, 778, 333, 333, 333, 500, 564, 250,
              333, 250, 278, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
              278, 278, 564, 564, 564, 444, 921, 722, 667, 667, 722, 611, 556,
              722, 722, 333, 389, 722, 611, 889, 722, 722, 556, 722, 667, 556,
              611, 722, 722, 944, 722, 722, 611, 333, 278, 333, 469, 500, 333,
              444, 500, 444, 500, 444, 333, 500, 500, 278, 278, 500, 278, 778,
              500, 500, 500, 500, 333, 389, 278, 500, 500, 722, 500, 500, 444,
              480, 200, 480, 541, 250, 250, 333, 500, 500, 167, 500, 500, 500,
              500, 180, 444, 500, 333, 333, 556, 556, 250, 500, 500, 500, 250,
              250, 453, 350, 333, 444, 444, 500,1000,1000, 250, 444, 250, 333,
              333, 333, 333, 333, 333, 333, 333, 250, 333, 333, 250, 333, 333,
              333,1000, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
              250, 250, 250, 250, 250, 889, 250, 276, 250, 250, 250, 250, 611,
              722, 889, 310, 250, 250, 250, 250, 250, 667, 250, 250, 250, 278,
              250, 250, 278, 500, 722, 500, 250, 250, 250, 250, 662
            },

            /* Times-Bold  */
            {
              250, 333, 555, 500, 500,1000, 833, 333, 333, 333, 500, 570, 250,
              333, 250, 278, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
              333, 333, 570, 570, 570, 500, 930, 722, 667, 722, 722, 667, 611,
              778, 778, 389, 500, 778, 667, 944, 722, 778, 611, 778, 722, 556,
              667, 722, 722,1000, 722, 722, 667, 333, 278, 333, 581, 500, 333,
              500, 556, 444, 556, 444, 333, 500, 556, 278, 333, 556, 278, 833,
              556, 500, 556, 556, 444, 389, 333, 556, 500, 722, 500, 500, 444,
              394, 220, 394, 520, 250, 250, 333, 500, 500, 167, 500, 500, 500,
              500, 278, 500, 500, 333, 333, 556, 556, 250, 500, 500, 500, 250,
              250, 540, 350, 333, 500, 500, 500,1000,1000, 250, 500, 250, 333,
              333, 333, 333, 333, 333, 333, 333, 250, 333, 333, 250, 333, 333,
              333,1000, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
              250, 250, 250, 250, 250,1000, 250, 300, 250, 250, 250, 250, 667,
              778,1000, 330, 250, 250, 250, 250, 250, 722, 250, 250, 250, 278,
              250, 250, 278, 500, 722, 556, 250, 250, 250, 250, 676
            },

            /* Times-Italic  */
            {
              250, 333, 420, 500, 500, 833, 778, 333, 333, 333, 500, 675, 250,
              333, 250, 278, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
              333, 333, 675, 675, 675, 500, 920, 611, 611, 667, 722, 611, 611,
              722, 722, 333, 444, 667, 556, 833, 667, 722, 611, 722, 611, 500,
              556, 722, 611, 833, 611, 556, 556, 389, 278, 389, 422, 500, 333,
              500, 500, 444, 500, 444, 278, 500, 500, 278, 278, 444, 278, 722,
              500, 500, 500, 500, 389, 389, 278, 500, 444, 667, 444, 444, 389,
              400, 275, 400, 541, 250, 250, 389, 500, 500, 167, 500, 500, 500,
              500, 214, 556, 500, 333, 333, 500, 500, 250, 500, 500, 500, 250,
              250, 523, 350, 333, 556, 556, 500, 889,1000, 250, 500, 250, 333,
              333, 333, 333, 333, 333, 333, 333, 250, 333, 333, 250, 333, 333,
              333, 889, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
              250, 250, 250, 250, 250, 889, 250, 276, 250, 250, 250, 250, 556,
              722, 944, 310, 250, 250, 250, 250, 250, 667, 250, 250, 250, 278,
              250, 250, 278, 500, 667, 500, 250, 250, 250, 250, 653
            },

            /* Times-BoldItalic  */
            {
              250, 389, 555, 500, 500, 833, 778, 333, 333, 333, 500, 570, 250,
              333, 250, 278, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
              333, 333, 570, 570, 570, 500, 832, 667, 667, 667, 722, 667, 667,
              722, 778, 389, 500, 667, 611, 889, 722, 722, 611, 722, 667, 556,
              611, 722, 667, 889, 667, 611, 611, 333, 278, 333, 570, 500, 333,
              500, 500, 444, 500, 444, 333, 500, 556, 278, 278, 500, 278, 778,
              556, 500, 500, 500, 389, 389, 278, 556, 444, 667, 500, 444, 389,
              348, 220, 348, 570, 250, 250, 389, 500, 500, 167, 500, 500, 500,
              500, 278, 500, 500, 333, 333, 556, 556, 250, 500, 500, 500, 250,
              250, 500, 350, 333, 500, 500, 500,1000,1000, 250, 500, 250, 333,
              333, 333, 333, 333, 333, 333, 333, 250, 333, 333, 250, 333, 333,
              333,1000, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
              250, 250, 250, 250, 250, 944, 250, 266, 250, 250, 250, 250, 611,
              722, 944, 300, 250, 250, 250, 250, 250, 722, 250, 250, 250, 278,
              250, 250, 278, 500, 722, 500, 250, 250, 250, 250, 669
            },

            /* Helvetica */
            {
              278, 278, 355, 556, 556, 889, 667, 222, 333, 333, 389, 584, 278,
              333, 278, 278, 556, 556, 556, 556, 556, 556, 556, 556, 556, 556,
              278, 278, 584, 584, 584, 556,1015, 667, 667, 722, 722, 667, 611,
              778, 722, 278, 500, 667, 556, 833, 722, 778, 667, 778, 722, 667,
              611, 722, 667, 944, 667, 667, 611, 278, 278, 278, 469, 556, 222,
              556, 556, 500, 556, 556, 278, 556, 556, 222, 222, 500, 222, 833,
              556, 556, 556, 556, 333, 500, 278, 556, 500, 722, 500, 500, 500,
              334, 260, 334, 584, 278, 278, 333, 556, 556, 167, 556, 556, 556,
              556, 191, 333, 556, 333, 333, 500, 500, 278, 556, 556, 556, 278,
              278, 537, 350, 222, 333, 333, 556,1000,1000, 278, 611, 278, 333,
              333, 333, 333, 333, 333, 333, 333, 278, 333, 333, 278, 333, 333,
              333,1000, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278,
              278, 278, 278, 278, 278,1000, 278, 370, 278, 278, 278, 278, 556,
              778,1000, 365, 278, 278, 278, 278, 278, 889, 278, 278, 278, 278,
              278, 278, 222, 611, 944, 611, 278, 278, 278, 278, 718
            },

            /* Helvetica-Bold  */
            {
              278, 333, 474, 556, 556, 889, 722, 278, 333, 333, 389, 584, 278,
              333, 278, 278, 556, 556, 556, 556, 556, 556, 556, 556, 556, 556,
              333, 333, 584, 584, 584, 611, 975, 722, 722, 722, 722, 667, 611,
              778, 722, 278, 556, 722, 611, 833, 722, 778, 667, 778, 722, 667,
              611, 722, 667, 944, 667, 667, 611, 333, 278, 333, 584, 556, 278,
              556, 611, 556, 611, 556, 333, 611, 611, 278, 278, 556, 278, 889,
              611, 611, 611, 611, 389, 556, 333, 611, 556, 778, 556, 556, 500,
              389, 280, 389, 584, 278, 278, 333, 556, 556, 167, 556, 556, 556,
              556, 238, 500, 556, 333, 333, 611, 611, 278, 556, 556, 556, 278,
              278, 556, 350, 278, 500, 500, 556,1000,1000, 278, 611, 278, 333,
              333, 333, 333, 333, 333, 333, 333, 278, 333, 333, 278, 333, 333,
              333,1000, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278,
              278, 278, 278, 278, 278,1000, 278, 370, 278, 278, 278, 278, 611,
              778,1000, 365, 278, 278, 278, 278, 278, 889, 278, 278, 278, 278,
              278, 278, 278, 611, 944, 611, 278, 278, 278, 278, 718
            },

            /* Helvetica-Oblique */
            {
              278, 278, 355, 556, 556, 889, 667, 222, 333, 333, 389, 584, 278,
              333, 278, 278, 556, 556, 556, 556, 556, 556, 556, 556, 556, 556,
              278, 278, 584, 584, 584, 556,1015, 667, 667, 722, 722, 667, 611,
              778, 722, 278, 500, 667, 556, 833, 722, 778, 667, 778, 722, 667,
              611, 722, 667, 944, 667, 667, 611, 278, 278, 278, 469, 556, 222,
              556, 556, 500, 556, 556, 278, 556, 556, 222, 222, 500, 222, 833,
              556, 556, 556, 556, 333, 500, 278, 556, 500, 722, 500, 500, 500,
              334, 260, 334, 584, 278, 278, 333, 556, 556, 167, 556, 556, 556,
              556, 191, 333, 556, 333, 333, 500, 500, 278, 556, 556, 556, 278,
              278, 537, 350, 222, 333, 333, 556,1000,1000, 278, 611, 278, 333,
              333, 333, 333, 333, 333, 333, 333, 278, 333, 333, 278, 333, 333,
              333,1000, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278,
              278, 278, 278, 278, 278,1000, 278, 370, 278, 278, 278, 278, 556,
              778,1000, 365, 278, 278, 278, 278, 278, 889, 278, 278, 278, 278,
              278, 278, 222, 611, 944, 611, 278, 278, 278, 278, 718
            },

            /* Helvetica-BoldOblique */
            {
              278, 333, 474, 556, 556, 889, 722, 278, 333, 333, 389, 584, 278,
              333, 278, 278, 556, 556, 556, 556, 556, 556, 556, 556, 556, 556,
              333, 333, 584, 584, 584, 611, 975, 722, 722, 722, 722, 667, 611,
              778, 722, 278, 556, 722, 611, 833, 722, 778, 667, 778, 722, 667,
              611, 722, 667, 944, 667, 667, 611, 333, 278, 333, 584, 556, 278,
              556, 611, 556, 611, 556, 333, 611, 611, 278, 278, 556, 278, 889,
              611, 611, 611, 611, 389, 556, 333, 611, 556, 778, 556, 556, 500,
              389, 280, 389, 584, 278, 278, 333, 556, 556, 167, 556, 556, 556,
              556, 238, 500, 556, 333, 333, 611, 611, 278, 556, 556, 556, 278,
              278, 556, 350, 278, 500, 500, 556,1000,1000, 278, 611, 278, 333,
              333, 333, 333, 333, 333, 333, 333, 278, 333, 333, 278, 333, 333,
              333,1000, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278, 278,
              278, 278, 278, 278, 278,1000, 278, 370, 278, 278, 278, 278, 611,
              778,1000, 365, 278, 278, 278, 278, 278, 889, 278, 278, 278, 278,
              278, 278, 278, 611, 944, 611, 278, 278, 278, 278, 718
            },

            /* Courier */
            {
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 563
            },

            /* Courier-Bold */
            {
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 583
            },

            /* Courier-Oblique */
            {
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 563
            },

            /* Courier-BoldOblique */
            {
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
              600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 583
            },

            /* Symbol */
            {
              250, 333, 713, 500, 549, 833, 778, 439, 333, 333, 500, 549, 250,
              549, 250, 278, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500,
              278, 278, 549, 549, 549, 444, 549, 722, 667, 722, 612, 611, 763,
              603, 722, 333, 631, 722, 686, 889, 722, 722, 768, 741, 556, 592,
              611, 690, 439, 768, 645, 795, 611, 333, 863, 333, 658, 500, 500,
              631, 549, 549, 494, 439, 521, 411, 603, 329, 603, 549, 549, 576,
              521, 549, 549, 521, 549, 603, 439, 576, 713, 686, 493, 686, 494,
              480, 200, 480, 549, 250, 250, 620, 247, 549, 167, 713, 500, 753,
              753, 753, 753,1042, 987, 603, 987, 603, 400, 549, 411, 549, 549,
              713, 494, 460, 549, 549, 549, 549,1000, 603,1000, 658, 823, 686,
              795, 987, 768, 768, 823, 768, 768, 713, 713, 713, 713, 713, 713,
              713, 768, 713, 790, 790, 890, 823, 549, 250, 713, 603, 603,1042,
              987, 603, 987, 603, 494, 329, 790, 790, 786, 713, 384, 384, 384,
              384, 384, 384, 494, 494, 494, 494, 250, 329, 274, 686, 686, 686,
              384, 384, 384, 384, 384, 384, 494, 494, 494, 250, 675
            }
          };

        /*
         *  Arrays for converting Hershey math symbols to PS fonts and 
         *  characters.
         */
        const int hms2psf[96] = 
             {
                 4,  4,  4,  4, 12,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
                 4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
                 4, 12, 12, 12, 12, 12,  4, 12, 12, 12, 12, 12, 12,  0,  0, 12,
                12, 12, -1, 12, -1, 12, 12, 12, 12, 12, -1,  0,  0,  0,  0, 12,
                 4, 12, 12, 12,  0,  0,  0, -1, -1, 12, 12, 12, 12, 12, 12, 12,
                12, 12, 12, 12, 12, 12, 12, -1, 12, 12,  0,  0,  0,  0, 12,  0
             };
        const int hms2psc[96] = 
             {
                 32,  33,  34,  35,  83,  37,  38,  39,  40,  41,  42,  43,
                 44,  45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,
                 56,  57,  58,  59,  60,  61,  62,  63,  64,  94, 208,  92,
                183, 189,  92,  42, 183, 176, 164, 163, 179, 178, 179, 225,
                241, 189,  -1, 177,  -1, 180, 183, 184, 185, 186,  -1,  91,
                 92,  93,  94,  36,  96, 181, 126, 162, 198,  39,  96,  -1,
                 -1, 214, 204, 200, 201, 199, 206, 174, 173, 172, 175, 182,
                209, 214, 242,  -1, 165,  80, 167, 123,  32, 125, 126,  32
             };

        /*
         *  Array for converting NCAR math symbol characters to PS 
         *  Symbol font characters.
         */
        const int nms2psc[96] = 
             {
                 32,  64,  92, 163,  94, 165, 166,  34,  36,  39, 126, 171, 
                172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 
                184, 185, 186, 187, 188,  -1, 190, 191, 192, 193, 194, 195, 
                196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 
                208, 209,  32,  32,  32, 213, 214, 215, 216, 217, 218, 219,
                220, 221, 222, 223, 224, 225,  32,  32,  32, 229,  32,  32,
                 32,  32,  32,  32,  32,  32,  32,  32,  32, 241, 242,  -1,
                 -1,  -1,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32
             };
        /*
         *  Array for converting NCAR text symbol characters to PS 
         *  text symbols.
         */
        const int nts2pts[96] = 
             {
                 32,  32, 162,  32,  32,  32,  32, 167,  32, 169, 170, 171, 
                172, 173,  32,  32,  32,  32, 178, 179,  32,  32, 182,  32, 
                184,  32, 186, 187, 188,  32,  32,  32,  32, 193, 194, 195, 
                196, 197, 198, 199, 200,  32, 202, 203,  32, 205, 206, 207, 
                 32,  32, 210, 211, 212,  32,  32,  32,  32,  32,  32,  32, 
                 32,  32,  32,  32,  32,  32,  -1,  -1,  -1,  32,  32,  32, 
                 32,  32,  32,  32,  -1,  -1,  -1,  -1,  32,  32,  32,  32, 
                 32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32, 
             };
        int index=0, fnum, return_value=0;

        psfc->char_num = cnum;
        psfc->outline = FALSE;
        fnum =  psa->attributes.text_font;

        switch (fnum) {
        case    DEFAULT: 
                psfc->font = PS_HELVETICA;
                psa->fonts_used[PS_HELVETICA] = 1;
                break;
        case    H_CARTOGRAPHIC_ROMAN:
                psfc->font = PS_HELVETICA;
                psa->fonts_used[PS_HELVETICA] = 1;
                break;
        case    H_CARTOGRAPHIC_GREEK:
                if ((cnum >= 32) && (cnum <= 127)) {
                        if ((cnum >= 65 && cnum <= 90) || 
                                        (cnum >=97 && cnum <= 122)) {
                                psfc->font = PS_SYMBOL;
                                psa->fonts_used[PS_SYMBOL] = 1;
                        }
                        else {
                                psfc->font = PS_HELVETICA;
                                psa->fonts_used[PS_HELVETICA] = 1;
                        }
                }
                break;
        case    H_SIMPLEX_ROMAN:
                psfc->font = PS_HELVETICA;
                psa->fonts_used[PS_HELVETICA] = 1;
                break;
        case    H_SIMPLEX_GREEK:
                if ((cnum >= 32) && (cnum <= 127)) {
                        if ((cnum >= 65 && cnum <= 90) || 
                                        (cnum >=97 && cnum <= 122)) {
                                psfc->font = PS_SYMBOL;
                                psa->fonts_used[PS_SYMBOL] = 1;
                        }
                        else {
                                psfc->font = PS_HELVETICA;
                                psa->fonts_used[PS_HELVETICA] = 1;
                        }
                }
                break;
        case    H_SIMPLEX_SCRIPT: 
                return_value = -1;
        case    H_COMPLEX_ROMAN:
                psfc->font = PS_TIMES_ROMAN;
                psa->fonts_used[PS_TIMES_ROMAN] = 1;
                break;
        case    H_COMPLEX_GREEK:
                if ((cnum >= 32) && (cnum <= 127)) {
                        if ((cnum >= 65 && cnum <= 90) || 
                                        (cnum >=97 && cnum <= 122)) {
                                psfc->font = PS_SYMBOL;
                                psa->fonts_used[PS_SYMBOL] = 1;
                        }
                        else {
                                psfc->font = PS_TIMES_ROMAN;
                                psa->fonts_used[PS_TIMES_ROMAN] = 1;
                        }
                }
                break;
        case    H_COMPLEX_SCRIPT:
                return_value = -1;
                break;
        case    H_COMPLEX_ITALIC:
                psfc->font = PS_TIMES_ITALIC;
                psa->fonts_used[PS_TIMES_ITALIC] = 1;
                break;
        case    H_COMPLEX_CYRILLIC:
                return_value = -1;
                break;
        case    H_DUPLEX_ROMAN:
                psfc->font = PS_HELVETICA_BOLD;
                psa->fonts_used[PS_HELVETICA_BOLD] = 1;
                break;
        case    H_TRIPLEX_ROMAN: 
                psfc->font = PS_TIMES_BOLD;
                psa->fonts_used[PS_TIMES_BOLD] = 1;
                break;
        case    H_TRIPLEX_ITALIC:
                psfc->font = PS_TIMES_BOLDITALIC;
                psa->fonts_used[PS_TIMES_BOLDITALIC] = 1;
                break;
        case    H_GOTHIC_GERMAN:
                return_value = -1;
                break;
        case    H_GOTHIC_ENGLISH:
                return_value = -1;
                break;
        case    H_GOTHIC_ITALIAN:
                return_value = -1;
                break;
        case    H_MATH_SYMBOLS:
                if ((cnum >= 32) && (cnum <= 127)) {
                        psfc->font = hms2psf[cnum-32];
                        psfc->char_num = hms2psc[cnum-32];
                        psa->fonts_used[psfc->font] = 1;
                }
                break;
        case    H_SYMBOL_SET1:
                return_value = -1;
                break;
        case    H_SYMBOL_SET2:
                return_value = -1;
                break;
        case    NCAR_HELVETICA: 
                psfc->font = PS_HELVETICA;
                psa->fonts_used[PS_HELVETICA] = 1;
                break;
        case    NCAR_HELVETICA_BOLD:
                psfc->font = PS_HELVETICA_BOLD;
                psa->fonts_used[PS_HELVETICA_BOLD] = 1;
                break;
        case    NCAR_HELVETICA_OBLIQUE:
                psfc->font = PS_HELVETICA_OBLIQUE;
                psa->fonts_used[PS_HELVETICA_OBLIQUE] = 1;
                break;
        case    NCAR_HELVETICA_BOLDOBLIQUE:
                psfc->font = PS_HELVETICA_BOLDOBLIQUE;
                psa->fonts_used[PS_HELVETICA_BOLDOBLIQUE] = 1;
                break;
        case    NCAR_TIMES_ROMAN:
                psfc->font = PS_TIMES_ROMAN;
                psa->fonts_used[PS_TIMES_ROMAN] = 1;
                break;
        case    NCAR_TIMES_BOLD: 
                psfc->font = PS_TIMES_BOLD;
                psa->fonts_used[PS_TIMES_BOLD] = 1;
                break;
        case    NCAR_TIMES_ITALIC:
                psfc->font = PS_TIMES_ITALIC;
                psa->fonts_used[PS_TIMES_ITALIC] = 1;
                break;
        case    NCAR_TIMES_BOLDITALIC:
                psfc->font = PS_TIMES_BOLDITALIC;
                psa->fonts_used[PS_TIMES_BOLDITALIC] = 1;
                break;
        case    NCAR_COURIER:
                psfc->font = PS_COURIER;
                psa->fonts_used[PS_COURIER] = 1;
                break;
        case    NCAR_COURIER_BOLD:
                psfc->font = PS_COURIER_BOLD;
                psa->fonts_used[PS_COURIER_BOLD] = 1;
                break;
        case    NCAR_COURIER_OBLIQUE:
                psfc->font = PS_COURIER_OBLIQUE;
                psa->fonts_used[PS_COURIER_OBLIQUE] = 1;
                break;
        case    NCAR_COURIER_BOLDOBLIQUE:
                psfc->font = PS_COURIER_BOLDOBLIQUE;
                psa->fonts_used[PS_COURIER_BOLDOBLIQUE] = 1;
                break;
        case    NCAR_GREEK: 
                if ((cnum >= 32) && (cnum <= 127)) {
                        if ((cnum >= 65 && cnum <= 90) || 
                                        (cnum >=97 && cnum <= 122)) {
                                psfc->font = PS_SYMBOL;
                                psa->fonts_used[PS_SYMBOL] = 1;
                        }
                        else {
                                psfc->font = PS_TIMES_ROMAN;
                                psa->fonts_used[PS_TIMES_ROMAN] = 1;
                        }
                        if (cnum == 118) {
                                psfc->char_num = 161;
                        }
                }
                break;
        case    NCAR_MATH_SYMBOLS:
                psfc->font = PS_SYMBOL;
                psa->fonts_used[PS_SYMBOL] = 1;
                if ((cnum >= 32) && (cnum <= 127)) {
                        psfc->char_num = nms2psc[cnum-32];
                }
                break;
        case    NCAR_TEXT_SYMBOLS:
                psfc->font = PS_TIMES_ROMAN;    
                psa->fonts_used[PS_TIMES_ROMAN] = 1;
                if ((cnum >= 32) && (cnum <= 127)) {
                        if (cnum == 54) {
                                psfc->font = PS_HELVETICA;
                                psa->fonts_used[PS_HELVETICA] = 1;
                        }
                        if (cnum >= 82 && cnum <= 84) {
                                psfc->font = PS_SYMBOL;
                                psa->fonts_used[PS_HELVETICA] = 1;
                        }
                        psfc->char_num = nts2pts[cnum-32];
                }
                break;
        case    NCAR_WEATHER1:
                return_value = -1;
                break;
        case    NCAR_WEATHER2:
                return_value = -1;
                break;
        case    NCAR_HELVETICA_O: 
                psfc->font = PS_HELVETICA;
                psa->fonts_used[PS_HELVETICA] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_HELVETICA_BOLD_O:
                psfc->font = PS_HELVETICA_BOLD;
                psa->fonts_used[PS_HELVETICA_BOLD] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_HELVETICA_OBLIQUE_O:
                psfc->font = PS_HELVETICA_OBLIQUE;
                psa->fonts_used[PS_HELVETICA_OBLIQUE] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_HELVETICA_BOLDOBLIQUE_O:
                psfc->font = PS_HELVETICA_BOLDOBLIQUE;
                psa->fonts_used[PS_HELVETICA_BOLDOBLIQUE] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_TIMES_ROMAN_O:
                psfc->font = PS_TIMES_ROMAN;
                psa->fonts_used[PS_TIMES_ROMAN] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_TIMES_BOLD_O: 
                psfc->font = PS_TIMES_BOLD;
                psa->fonts_used[PS_TIMES_BOLD] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_TIMES_ITALIC_O:
                psfc->font = PS_TIMES_ITALIC;
                psa->fonts_used[PS_TIMES_ITALIC] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_TIMES_BOLDITALIC_O:
                psfc->font = PS_TIMES_BOLDITALIC;
                psa->fonts_used[PS_TIMES_BOLDITALIC] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_COURIER_O:
                psfc->font = PS_COURIER;
                psa->fonts_used[PS_COURIER] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_COURIER_BOLD_O:
                psfc->font = PS_COURIER_BOLD;
                psa->fonts_used[PS_COURIER_BOLD] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_COURIER_OBLIQUE_O:
                psfc->font = PS_COURIER_OBLIQUE;
                psa->fonts_used[PS_COURIER_OBLIQUE] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_COURIER_BOLDOBLIQUE_O:
                psfc->font = PS_COURIER_BOLDOBLIQUE;
                psa->fonts_used[PS_COURIER_BOLDOBLIQUE] = 1;
                psfc->outline = TRUE;
                break;
        case    NCAR_GREEK_O: 
                psfc->outline = TRUE;
                if ((cnum >= 32) && (cnum <= 127)) {
                        if ((cnum >= 65 && cnum <= 90) || 
                                        (cnum >=97 && cnum <= 122)) {
                                psfc->font = PS_SYMBOL;
                                psa->fonts_used[PS_SYMBOL] = 1;
                        }
                        else {
                                psfc->font = PS_TIMES_ROMAN;
                                psa->fonts_used[PS_TIMES_ROMAN] = 1;
                        }
                        if (cnum == 118) {
                                psfc->char_num = 161;
                        }
                }
                break;
        case    NCAR_MATH_SYMBOLS_O:
                psfc->font = PS_SYMBOL;
                psa->fonts_used[PS_SYMBOL] = 1;
                psfc->outline = TRUE;
                if ((cnum >= 32) && (cnum <= 127)) {
                        psfc->char_num = nms2psc[cnum-32];
                }
                break;
        case    NCAR_TEXT_SYMBOLS_O:
                psfc->font = PS_TIMES_ROMAN;    
                psa->fonts_used[PS_TIMES_ROMAN] = 1;
                psfc->outline = TRUE;
                if ((cnum >= 32) && (cnum <= 127)) {
                        if (cnum == 54) {
                                psfc->font = PS_HELVETICA;
                                psa->fonts_used[PS_HELVETICA] = 1;
                        }
                        if (cnum >= 82 && cnum <= 84) {
                                psfc->font = PS_SYMBOL;
                                psa->fonts_used[PS_SYMBOL] = 1;
                        }
                        psfc->char_num = nts2pts[cnum-32];
                }
                break;
        case    NCAR_WEATHER1_O:
                return_value = -1;
                break;
        case    NCAR_WEATHER2_O:
                return_value = -1;
                break;
        default:
                return_value = -1;
                break;
        }

        if ((psfc->char_num >= 32) && (psfc->char_num <= 127)) {
                index = (psfc->char_num) - 32;
        }
        else if ((psfc->char_num >= 160) && (psfc->char_num <= 255)) {
                index = (psfc->char_num) - 160 + 96;
        }
        else if (psfc->char_num == -1) {
                return_value = -1;
        }
        else {
                psfc->font_height = 
                        PSFontMetrics[psfc->font][FONT_HEIGHT_ARRAY_INDEX];
                psfc->char_width = 0;
                return(0);
        }

        /*
         *  Return a filled Times-Roman asterisk if character not available.
         */
        if (return_value == -1) {
                psfc->font = PS_TIMES_ROMAN;
                psa->fonts_used[PS_TIMES_ROMAN] = 1;
                psfc->char_num = 42;
                psfc->char_width = PSFontMetrics[psfc->font][10];
                psfc->font_height = 
                        PSFontMetrics[psfc->font][FONT_HEIGHT_ARRAY_INDEX];
                psfc->outline = FALSE;
                return(return_value);
        }

        psfc->char_width = PSFontMetrics[psfc->font][index];
        psfc->font_height = PSFontMetrics[psfc->font][FONT_HEIGHT_ARRAY_INDEX];
        if (fnum == H_CARTOGRAPHIC_ROMAN || fnum == H_CARTOGRAPHIC_GREEK) {
                psfc->font_height *= 1.5;
        }

        return(0);

}

/*
 *  Function for reversing the characters in a string.
 */
void reverse_chars(char *str)
{
        int     strl = strlen(str), i, index;
        char    ctmp;

        for (i = 0; i < strl/2; i++) {
                index = strl-i-1;
                strncpy(&ctmp, str+i, 1);
                strncpy(str+i, str+index, 1);
                strncpy(str+index, &ctmp, 1);
        }
}

/*
 *  Set up the file name for the output file depending on the workstation
 *  ID, and if it is PS, EPS, or EPSI, or whether it has been specifically
 *  provided.
 */
char *GetFileName(int wkid, ps_file_type file_type, char *file_name)
{
        static char tname[257];
        static char *tch;

        /*
         *  A setting of the environment variable NCARG_GKS_PSOUTPUT
         *  takes precedence over everything.
         */
        tch = getenv("NCARG_GKS_PSOUTPUT");
        if ( (tch != (char *) NULL) && (strlen(tch) > 0)) {
                return (tch);
        }

        if ( (strncmp(file_name, "DEFAULT", 7) == 0) ||
             (strlen(file_name) == 0) ) {
                switch (file_type) {
                case RPS:
                        (void) sprintf(tname,"gmeta%d.ps",wkid);
                        return (tname);
                case EPSF:
                        (void) sprintf(tname,"gmeta%d.eps",wkid);
                        return (tname);
                case EPSI:
                        (void) sprintf(tname,"gmeta%d.epsi",wkid);
                        return (tname);
                default:
                        (void) sprintf(tname,"gmeta%d.ps",wkid);
                        return (tname);
                }
        }
        else {      /* User has defined a name */
                tch = strtok(file_name, " ");
                return (tch);   
        }
}

/*
 *  Put out a character string for PostScript display.  The special
 *  characters "(", ")", "\" are put out as PostScript octal
 *  constants as are any characters having an ASCII Decimal Equivalent
 *  larger than 127.
 */
static void PSoutput_string(PSddp *psa, char *str)
{
        char    ctmp;
        int     i, itmp;

        for (i = 0; i < (size_t) strlen(str); i++) {
                strncpy(&ctmp, str+i, 1);
                itmp = (int) (ctmp & 255);
                if ((itmp == 40) || (itmp == 41) ||
                    (itmp == 92) || (itmp > 127)) {
                        fprintf(psa->file_pointer, "\\%03o", itmp);
                }
                else {
                        fprintf(psa->file_pointer, "%c", ctmp);
                }
        }
}

/*
 *  Check to see if trying to put out more than one page for an EPS file.
 */
static int check_EPS (GKSC *gksc)
{
        PSddp   *psa;
        static int      first_call = TRUE;

        psa = (PSddp *) gksc->ddp;

        if (((psa->type == EPSF) || (psa->type == EPSI)) &&
                (psa->page_number > 1)) {
                        if (first_call) {
                          ESprintf(ERR_EPS_PAGES,
                                "PS: Encapsulated PostScript files"
                                " cannot have more than one picture.\n");
                          (void) ps_CloseWorkstation(gksc);
                          first_call = FALSE;
                        }
                        return(ERR_EPS_PAGES);
        }
        return(0);
}


/*
 *  Put out polyline.  If the second argument is non-zero, draw
 *  a line from the last to the first point if required.
 */
static void OutputPolyline (GKSC *gksc, int closed)
{
        PSPoint *pptr = (PSPoint *) gksc->p.list, tpoint;
        PSddp   *psa = (PSddp *) gksc->ddp;
        int     npoints = gksc->p.num, mpoints, i;
        int     StackSize;

        /*
         *  Put the line out in chunks of stack_size length.
         */
        StackSize = psa->stack_size;
        for (i = 0; i < npoints; i += StackSize) {
                if (i == 0) {
                        (npoints > StackSize) ? (mpoints = StackSize) :
                                (mpoints = npoints);
                        PSprint_points((PSddp *) gksc->ddp, pptr, mpoints, 
                                POLY_POINT);
                }
                else {
                        ((npoints - i) > StackSize) ? (mpoints = StackSize+1) :
                                (mpoints = npoints - i + 1);
                        PSprint_points((PSddp *) gksc->ddp, pptr+i-1, mpoints,
                                POLY_POINT);
                }
        }
        /*
         *  If a closed polygon is requested, draw a line from the last 
         *  point to the first if they are not the same.
         */
        if (closed) {
                if ((pptr[0].x != pptr[npoints-1].x) || 
                    (pptr[0].y != pptr[npoints-1].y)) {
                        tpoint.x = pptr[npoints-1].x;
                        tpoint.y = pptr[npoints-1].y;
                        PSprint_points(psa, &tpoint, 1, MOVETO);
                        tpoint.x = pptr[0].x;
                        tpoint.y = pptr[0].y;
                        PSprint_points(psa, &tpoint, 1, LINETO);
                }
        }
}

/*
 *  Put out the polymarkers
 */
static void OutputPolymarker (GKSC *gksc, int markersize, int markertype)
{
        PSPoint *pptr = (PSPoint *) gksc->p.list;
        PSddp   *psa = (PSddp *) gksc->ddp;
        int     npoints = gksc->p.num, mpoints, i, tsize;
        int     StackSize;

        /*
         *  Put the markers out in chunks of stack_size length.
         */
        StackSize = psa->stack_size;
        for (i = 0; i < npoints; i += StackSize) {
                ((npoints-i) > StackSize) ? (mpoints = StackSize) :
                        (mpoints = npoints-i);
                PSprint_points((PSddp *) gksc->ddp, pptr+i, mpoints, 
                        POLY_MARKER);
                tsize = MAX(1, (int) (.9 * (float) markersize));
                switch (markertype) {
                case    DOT_MARKER:      /* Dots cannot be scaled */
                        tsize = MAX(1, (int) (.28/(psa->scaling)));
                        (void) fprintf(psa->file_pointer, "%d B\n", tsize);
                        break;
                case    PLUS_MARKER:
                        (void) fprintf(psa->file_pointer, "%d Q\n", tsize);
                        break;
                case    STAR_MARKER:
                        (void) fprintf(psa->file_pointer, "%d V\n", tsize);
                        break;
                case    CIRCLE_MARKER:
                        (void) fprintf(psa->file_pointer, "%d B\n", tsize);
                        break;
                case    X_MARKER:
                        (void) fprintf(psa->file_pointer, "%d X\n", tsize);
                        break;
                default:
                        (void) fprintf(psa->file_pointer, "%d V\n", tsize);
                        break;
                }
        }
}

/*ARGSUSED*/
int ps_OpenWorkstation(GKSC *gksc)
{
        char    *sptr = (char *) gksc->s.list;
        PSddp   *psa;
        char    *ctmp;
        FILE    *fp;
        int     *pint, wks_type;
        extern  int     orig_wks_id;
        _NGCesc *cesc;

        psa = (PSddp *) malloc (sizeof (PSddp));
        if (psa == (PSddp *) NULL) {
                ESprintf(ERR_PS_MEMORY, "PS: malloc(%d)", sizeof(PSddp));
                return(ERR_PS_MEMORY);
        }

        gksc->ddp = (GKSC_Ptr) psa;

        /*
         * Handle Initial C Escape Elements.
         *      (none currently defined for ps - so all of them cause gerhnd.)
         */
        while((cesc = _NGGetCEscInit())){
                gerr_hand(182,11,NULL);
        }

        pint = (int *)(gksc->i.list);
        wks_type = *(pint+1);        
        psa->wks_id = orig_wks_id;

        if ( (wks_type == CPP) || (wks_type == CEP) || (wks_type == CIP) ||
             (wks_type == CPL) || (wks_type == CEL) || (wks_type == CIL)) {
                psa->color = COLOR;
        }
        else {
                psa->color = MONO;
        }

        if ( (wks_type == CPP) || (wks_type == MPP) ||
             (wks_type == CPL) || (wks_type == MPL)) {
                psa->type = RPS;
        }
        else if ((wks_type == CEP) || (wks_type == MEP) ||
                 (wks_type == CEL) || (wks_type == MEL)) {
                psa->type = EPSF;
        }
        else {
                psa->type = EPSI;
        }

        if ( (wks_type == CPP) || (wks_type == CEP) || (wks_type == CIP) ||
             (wks_type == MPP) || (wks_type == MEP) || (wks_type == MIP)) {
                psa->orientation = PORTRAIT;
        }
        else {
                psa->orientation = LANDSCAPE;
        }

        psa->output_file = GetFileName(psa->wks_id, psa->type, sptr);
        if (strncmp(psa->output_file, "stdout", 6) == 0) {
                fp = stdout;
        }
        else {
                fp = fopen(psa->output_file,"w");
        }
        if (fp == (FILE *) NULL) {
                ctmp = (char *) calloc(strlen(psa->output_file)+3, 1);
                strcat(ctmp,"\"");
                strcat(ctmp+1,psa->output_file);
                strcat(ctmp+1+strlen(psa->output_file),"\"");
                ESprintf(ERR_OPN_PS, "PS: fopen(%s, \"w\")", ctmp);
                free(ctmp);
                return(ERR_OPN_PS);
        }
        psa->file_pointer = fp;
        
        PSinit(psa, pint+2);       /* Initialize local data. */

        /* 
         *  Initialize all transformations as well as the device coordinate
         *  space (store these in the device dependent data).
         */
        TransformSetWindow(&(psa->tsystem), 0.0, 0.0, 1.0, 1.0);
        TransformSetViewport(&(psa->tsystem),0.0, 0.0, 1.0, 1.0);
        TransformSetNDScreenSpace(&(psa->tsystem),0.0, 0.0, 1.0, 1.0);
        TransformSetScreenSpace(&(psa->tsystem),0.0, 0.0, 1.0, 1.0);

        psa->transform = TransformGetTransform(&psa->tsystem);

        PSpreamble(psa, FOR_FILE);

        return(0);
}

/*ARGSUSED*/
int ps_ActivateWorkstation(gksc)
        GKSC    *gksc;
{
        return(0);
}

/*ARGSUSED*/
int ps_DeactivateWorkstation(gksc)
        GKSC    *gksc;
{
        PSddp   *psa;

        psa = (PSddp *) gksc->ddp;

        (void) fflush(psa->file_pointer);
        return(0);
}

/*ARGSUSED*/
int ps_CloseWorkstation(gksc)
        GKSC    *gksc;
{
        PSddp   *psa;
        int     pages, i, j;

        psa = (PSddp *) gksc->ddp;

        (void) fprintf(psa->file_pointer, "%%%%Trailer\n");

        (void) fprintf(psa->file_pointer, "%%%%DocumentFonts: ");
        for (i = 0; i < NUM_PS_FONTS; i++) {
                if (psa->fonts_used[i] == 1) {
                        for (j = 1; j < (size_t) strlen(PSFontNames[i]); j++) {
                                (void) fprintf(psa->file_pointer, "%c",
                                        *(PSFontNames[i]+j));
                        }
                        (void) fprintf(psa->file_pointer, " ");
                }
        }
        (void) fprintf(psa->file_pointer, "\n");

        pages = psa->page_number;
        pages--;
        if (psa->type == RPS) {
                (void) fprintf(psa->file_pointer, "%%%%Pages: %d\n", pages);
        }

        if (psa->type != RPS) {
           (void) fprintf(psa->file_pointer, "%%%%BoundingBox: ");
           (void) fprintf(psa->file_pointer, "%d ", (psa->bspace.llx)-1);
           (void) fprintf(psa->file_pointer, "%d ", (psa->bspace.lly)-1);
           (void) fprintf(psa->file_pointer, "%d ", (psa->bspace.urx)+1);
           (void) fprintf(psa->file_pointer, "%d ", (psa->bspace.ury)+1);
           (void) fprintf(psa->file_pointer,"\n");
        }

        (void) fprintf(psa->file_pointer, "%%%%EOF\n");

        (void) fflush(psa->file_pointer);
        psa->pict_empty = TRUE;
        fclose(psa->file_pointer);

        free(psa);
        return(0);
}

/*ARGSUSED*/
int ps_ClearWorkstation(gksc)
        GKSC    *gksc;
{
        PSddp   *psa;
        int     ier = 0;
        psa = (PSddp *) gksc->ddp;

        if (psa->pict_empty == FALSE) {
                (void) fprintf(psa->file_pointer, "Gr\n");
        }
        else {
                if ((ier = check_EPS(gksc)) != 0) return(ier);
                PSpreamble(psa, FOR_PICTURE);
                (void) fprintf(psa->file_pointer, "Gr\n");
        }
                        
        (void) fprintf(psa->file_pointer, "S\n");
        (void) fflush(psa->file_pointer);
        psa->pict_empty = TRUE;
        psa->page_number++;
        return(0);
}


/*ARGSUSED*/
int ps_Polyline(gksc)
        GKSC    *gksc;
{
        PSddp   *psa;
        int     ps_linewidth, requested_color, current_color, requested_type;
        int     ier = 0;

        psa = (PSddp *) gksc->ddp;

        if ((ier = check_EPS(gksc)) != 0) return(ier);

        if (psa->pict_empty) {
                PSpreamble(psa, FOR_PICTURE);
                psa->pict_empty = FALSE;
        }

        if (psa->color == COLOR) {         
           requested_color = psa->attributes.line_colr_ind;
           current_color = psa->attributes.ps_colr_ind;
           if (requested_color != current_color) {
                   /***RLB (void) fprintf(psa->file_pointer, "%d O\n", requested_color);*/
                   writePSColor(psa->file_pointer, requested_color);
                   psa->attributes.ps_colr_ind = requested_color;
           }
        }
        if (psa->color == MONO) {         
           requested_color = psa->attributes.line_colr_ind;
           current_color = psa->attributes.ps_colr_ind;
           if (requested_color != current_color) {
                   if (requested_color == 0) {
                        (void) fprintf(psa->file_pointer,"0 O\n");
                   }
                   else {
                        (void) fprintf(psa->file_pointer,"1 O\n");
                   } 
                   psa->attributes.ps_colr_ind = requested_color;
           }
        }

        ps_linewidth = (int) ((psa->nominal_width_scale) * 
                                (psa->attributes.linewidth)/(psa->scaling));
        ps_linewidth = MAX(1, ps_linewidth);
        if (ps_linewidth != psa->attributes.linewidth_set) {
                (void) fprintf(psa->file_pointer, "%d W\n", ps_linewidth);
                psa->attributes.linewidth_set = ps_linewidth;
        }

        requested_type = psa->attributes.linetype;
        if (requested_type != psa->attributes.linetype_set) {
                PSset_dashpattern(psa);
                psa->attributes.linetype_set = requested_type;
        }

        if ((psa->ps_clip.null) && 
                (psa->attributes.clip_ind == CLIPPING_ON)) return(0);
                
        OutputPolyline (gksc,0);
        return(0);
}

/*ARGSUSED*/
int ps_Polymarker(gksc)
        GKSC    *gksc;
{

        PSddp   *psa;
        int     markersize, markertype, requested_color, current_color;
        int     ier = 0;

        psa = (PSddp *) gksc->ddp;

        if ((ier = check_EPS(gksc)) != 0) return(ier);

        if (psa->pict_empty) {
                PSpreamble(psa, FOR_PICTURE);
                psa->pict_empty = FALSE;
        }

        if (psa->color == COLOR) { 
           requested_color = psa->attributes.marker_colr_ind;
           current_color = psa->attributes.ps_colr_ind;
           if (requested_color != current_color) {
                   /****RLB (void) fprintf(psa->file_pointer, "%d O\n", requested_color);*/
                   writePSColor(psa->file_pointer, requested_color);
                   psa->attributes.ps_colr_ind = requested_color;
           }
        }
        if (psa->color == MONO) {         
           requested_color = psa->attributes.marker_colr_ind;
           current_color = psa->attributes.ps_colr_ind;
           if (requested_color != current_color) {
                   if (requested_color == 0) {
                        (void) fprintf(psa->file_pointer,"0 O\n");
                   }
                   else {
                        (void) fprintf(psa->file_pointer,"1 O\n");
                   } 
                   psa->attributes.ps_colr_ind = requested_color;
           }
        }

        /*
         * Nominal length of marker element is six units in the default
         * PostScript user coordinate space (half width is three units).
         */
        markersize = 3 * (int) ((psa->transform.y_scale) *
                                (psa->attributes.marker_size)/(psa->scaling));
        markertype = psa->attributes.marker_type;

        /*
         *  Save current linewith and type and use the defaults.
         */
        (void) fprintf(psa->file_pointer, "H I %d W [] 0 D\n", 
                (int) ((psa->nominal_width_scale) * 
                (LINEWIDTH_DEFAULT/(psa->scaling))));
        
        if ((psa->ps_clip.null) && 
                (psa->attributes.clip_ind == CLIPPING_ON)) return(0);

        OutputPolymarker (gksc, markersize, markertype);
 
        /*
         *  Restore original linewith and type.
         */
        (void) fprintf(psa->file_pointer, "D W\n");

        return(0);
}

/*ARGSUSED*/
int ps_Text(gksc)
        GKSC    *gksc;
{
        PSPoint *pptr = (PSPoint *) gksc->p.list;
        char    *sptr = (char *) gksc->s.list;
        char ctmp, *tptr;
        PSddp   *psa;
        PSCharInfo fc;
        PSTextent  textent;
        int     requested_color, current_color;
        int     PSFontScale, PSCharHeight, PSCharSpace;
        int     i, j, found, x_position, y_position, y_inc, return_value=0;
        int     string_height=0, max_char_width, char_spacing=0;
        int     num_chars, old_font=0, current_font, strpos, fcount;

        float   xoffset=0.0, yoffset=0.0, vert_offset=0.0;
        float   nominal_vert_adjust, tdiff, bdiff, char_expn;
        float   tm_a, tm_b, tm_c, tm_d, tmp1, aspect_ratio;
        float   fc2wsvp, string_height_wsvp, char_spacing_wsvp;
        float   max_char_width_wsvp;
        float   up_x, up_y, base_x, base_y, mag_up, mag_base, base2up_ratio;

        psa = (PSddp *) gksc->ddp;

        if ((return_value = check_EPS(gksc)) != 0) return(return_value);

        num_chars = strlen(sptr);
        if (num_chars == 0) {
                return(0);
        }

        if (psa->pict_empty) {
                PSpreamble(psa, FOR_PICTURE);
                psa->pict_empty = FALSE;
        }
                
        if (psa->color == COLOR) { 
           requested_color = psa->attributes.text_colr_ind;
           current_color = psa->attributes.ps_colr_ind;
           if (requested_color != current_color) {
                   /****RLB (void) fprintf(psa->file_pointer, "%d O\n", requested_color);*/
                   writePSColor(psa->file_pointer, requested_color);
                   psa->attributes.ps_colr_ind = requested_color;
           }
        }
        if (psa->color == MONO) {         
           requested_color = psa->attributes.text_colr_ind;
           current_color = psa->attributes.ps_colr_ind;
           if (requested_color != current_color) {
                   if (requested_color == 0) {
                        (void) fprintf(psa->file_pointer,"0 O\n");
                   }
                   else {
                        (void) fprintf(psa->file_pointer,"1 O\n");
                   } 
                   psa->attributes.ps_colr_ind = requested_color;
           }
        }

        up_x = psa->attributes.char_up_vec_x;
        up_y = psa->attributes.char_up_vec_y;
        base_x = psa->attributes.char_base_vec_x;
        base_y = psa->attributes.char_base_vec_y;
        mag_up = MAG(up_x, up_y);
        mag_base = MAG(base_x, base_y);
        base2up_ratio = mag_base/mag_up;
        PSCharHeight = (int)((psa->attributes.char_ht) * 
                                (float) (psa->dspace.yspan));

        /*
         *  Find the text extent boundaries in font coordinate units.
         *  This computation is complicated by the fact that the character 
         *  spacing may cause the left extent of the string to move to the 
         *  left instead of right, and similarly for the top extent.
         */
        aspect_ratio = 
                ((float)(psa->dspace.yspan))/((float)(psa->dspace.xspan));
        for (i = 0, textent.left = 50000, textent.right = -50000, 
                        textent.top = 50000, textent.bottom = -50000,
                        x_position = 0, y_position = 0, max_char_width = 0; 
                        i < num_chars; 
                        i++) {
                strncpy(&ctmp, sptr+i, 1);      

                /*
                 *  Find the equivalent PostScript font and character to the
                 *  current input character.
                 */
                found = MapFonts(psa, (int) (ctmp & 255), &fc);
                if (found < 0) {
                        return_value = ERR_PS_CHAR;
                }

                /*
                 *  Use height of first character for subsequent computation
                 *  of adjustment for vertical alignment.
                 */
                if (i == 0) {     
                        string_height = fc.font_height;
                        char_spacing = (int)((psa->attributes.char_space) *
                                        base2up_ratio * (float) string_height);
                }

                textent.left = MIN(textent.left, x_position);
                textent.right = MAX(textent.right, x_position + fc.char_width);
                x_position += fc.char_width;

                /*
                 *  The vertical text extent is 1000 (or 0 if the character 
                 *  is null) since that is what the font coordinates are 
                 *  scaled by.
                 */
                y_inc = ((fc.char_width == 0) ? 0 : 1000);
                textent.bottom = MAX(textent.bottom, y_position + y_inc);
                textent.top = MIN(textent.top, y_position);
                y_position += y_inc;
                max_char_width = MAX(max_char_width, fc.char_width);

                if (i == (num_chars - 1)) {
                        break;
                }
                x_position += char_spacing;
                y_position += char_spacing;
        }

        textent.left = (int) ((float) textent.left * aspect_ratio);
        textent.right = (int) ((float) textent.right * aspect_ratio);

        /*
         *  Convert character height to proper scalefont argument.  Scale
         *  the PS font by a factor SC so that  (font_height/1000.)*SC equals
         *  the PS character height.
         */
        PSFontScale = (int)(1000. * (float) PSCharHeight) / 
                                                ((float)(fc.font_height));

        /*
         *  Scale factor for converting font coordinate units to 
         *  workstation viewport units.
         */
        fc2wsvp = (0.001 * (float) PSFontScale) / (psa->dspace.yspan);

        textent.left_wsvp = (float) textent.left * fc2wsvp;
        textent.right_wsvp = (float) textent.right * fc2wsvp;
        textent.top_wsvp = (float) textent.top * fc2wsvp;
        textent.bottom_wsvp = (float) textent.bottom * fc2wsvp;
        string_height_wsvp = (float) string_height * fc2wsvp;
        char_spacing_wsvp = (float) char_spacing * fc2wsvp;
        max_char_width_wsvp = (float) max_char_width * fc2wsvp;
        tdiff = .43 * ((1000./string_height) - 1.);
        bdiff = .57 * ((1000./string_height) - 1.);

        /*
         *  Save the current graphics state.
         */
        (void) fprintf(psa->file_pointer,"Gs ");

        /*
         *  Translate the axes to the current point, scale as per
         *  the character expansion factor, rotate in accordance
         *  with the base vector, and deform as per the angle
         *  between the base and up vector.
         */
        PSprint_points(psa, pptr, 1, DO_NOTHING);
        (void) fprintf(psa->file_pointer, "Tr ");
        (void) fprintf(psa->file_pointer, "0 0 M ");

        /*
         *  Normalize up and base vectors, calculate the transformation
         *  matrix.
         */
        up_x = up_x/mag_up;
        up_y = up_y/mag_up;
        base_x = base_x/mag_base;
        base_y = base_y/mag_base;
        char_expn = psa->attributes.char_expan;
        tm_a = char_expn * base2up_ratio * base_x;
        tm_b = char_expn * base2up_ratio * base_y;
        tmp1 = up_x * base_x + up_y * base_y;
        tm_c = tm_a * tmp1 + base_y * (up_x * base_y - up_y * base_x);
        tm_d = tm_b * tmp1 + base_x * (up_y * base_x - up_x * base_y);
        if ((tm_a != 1.) || (tm_b != 0.) || (tm_c != 0.) || (tm_d != 1.)) {
                fprintf(psa->file_pointer,"[%.3f %.3f %.3f %.3f 0 0 ] Co ",
                        tm_a, tm_b, tm_c, tm_d); 
        }
        if (aspect_ratio != 1.) {
                fprintf(psa->file_pointer,"[%.3f 0. 0. 1. 0. 0.] Co ",
                        1./aspect_ratio);
        }

        /*
         *  Adjust the input coordinate in accordance with the horizontal
         *  and vertical alignments.
         */
        if (((psa->attributes.text_path) == RIGHT_TEXT_PATH) ||
            ((psa->attributes.text_path) == LEFT_TEXT_PATH)) {
                switch (psa->attributes.text_align_horiz) {
                case NORMAL_ALIGNMENT_HORIZ:
                    if ((psa->attributes.text_path) == RIGHT_TEXT_PATH) {
                        xoffset = textent.left_wsvp;
                    }
                    else if ((psa->attributes.text_path) == LEFT_TEXT_PATH) {
                    xoffset = textent.right_wsvp;
                    }
                    break;
                case LEFT_ALIGNMENT_HORIZ:
                    xoffset = textent.left_wsvp;
                    break;
                case CENTER_ALIGNMENT_HORIZ:
                    xoffset = 0.5*(textent.left_wsvp + textent.right_wsvp);
                    break;
                case RIGHT_ALIGNMENT_HORIZ:
                    xoffset = textent.right_wsvp;
                    break;
                }
                pptr[0].x = -xoffset;
        }
        else if (((psa->attributes.text_path) == UP_TEXT_PATH) ||
            ((psa->attributes.text_path) == DOWN_TEXT_PATH)) {
                switch (psa->attributes.text_align_horiz) {
                case NORMAL_ALIGNMENT_HORIZ:
                    xoffset = 0.;
                    break;
                case LEFT_ALIGNMENT_HORIZ:
                    xoffset = -.5 * max_char_width_wsvp;
                    break;
                case CENTER_ALIGNMENT_HORIZ:
                    xoffset = 0.;
                    break;
                case RIGHT_ALIGNMENT_HORIZ:
                    xoffset =  .5 * max_char_width_wsvp;
                    break;
                }
                pptr[0].x = -xoffset;
        }

        if (((psa->attributes.text_path) == RIGHT_TEXT_PATH) ||
            ((psa->attributes.text_path) == LEFT_TEXT_PATH)) {
                switch (psa->attributes.text_align_vert) {
                case NORMAL_ALIGNMENT_VERT:
                    yoffset = 0.;
                    break;
                case TOP_ALIGNMENT_VERT:
                    yoffset = (1. + tdiff) * string_height_wsvp;
                    break;
                case CAP_ALIGNMENT_VERT:
                    yoffset = string_height_wsvp;
                    break;
                case HALF_ALIGNMENT_VERT:
                    yoffset = 0.5 * string_height_wsvp;
                    break;
                case BASE_ALIGNMENT_VERT:
                    yoffset = 0.;
                    break;
                case BOTTOM_ALIGNMENT_VERT:
                    yoffset = -bdiff * string_height_wsvp;
                    break;
                }
                pptr[0].y = -yoffset;
        }
        else if (((psa->attributes.text_path) == UP_TEXT_PATH) ||
            ((psa->attributes.text_path) == DOWN_TEXT_PATH)) {
                nominal_vert_adjust = bdiff * string_height_wsvp + 
                                        char_spacing_wsvp;
                switch (psa->attributes.text_align_vert) {
                case NORMAL_ALIGNMENT_VERT:
                    if ((psa->attributes.text_path) == UP_TEXT_PATH) {
                        vert_offset = -textent.bottom_wsvp - char_spacing_wsvp;
                    }
                    else if ((psa->attributes.text_path) == DOWN_TEXT_PATH) {
                        vert_offset = -nominal_vert_adjust - textent.top_wsvp;
                    }
                    break;
                case TOP_ALIGNMENT_VERT:
                    vert_offset = -nominal_vert_adjust - textent.top_wsvp;
                    break;
                case CAP_ALIGNMENT_VERT:
                    vert_offset = -nominal_vert_adjust - 
                                tdiff * string_height_wsvp - textent.top_wsvp;
                    break;
                case HALF_ALIGNMENT_VERT:
                    vert_offset = -nominal_vert_adjust - 
                                0.5 * (textent.bottom_wsvp + textent.top_wsvp);
                    break;
                case BASE_ALIGNMENT_VERT:
                    vert_offset = -textent.bottom_wsvp - char_spacing_wsvp;
                    break;
                case BOTTOM_ALIGNMENT_VERT:
                    vert_offset = -nominal_vert_adjust - textent.bottom_wsvp;
                    break;
                }
                pptr[0].y = -vert_offset;
        }
        /*
         *  Reverse the characters in the input string if the
         *  text path is left or up.
         */
        if (((psa->attributes.text_path) == UP_TEXT_PATH) ||
            ((psa->attributes.text_path) == LEFT_TEXT_PATH)) {
                reverse_chars(sptr);
        }

        (void) fprintf(psa->file_pointer,"%d %d Rm\n", 
                (int) (((float) (psa->dspace.xspan)) * pptr[0].x), 
                (int) (((float) (psa->dspace.yspan)) * pptr[0].y)); 

        PSCharSpace = (int) ((float) PSCharHeight * base2up_ratio *
                        (psa->attributes.char_space));
        
        /*
         *  Put out the string accounting for the fact that a string
         *  in a single GKS font may result in strings from several
         *  PostScript fonts.
         */
        if ((psa->ps_clip.null) && 
                (psa->attributes.clip_ind == CLIPPING_ON)) return(0);

        for (i = 0, strpos = 0, fcount = 0; i < num_chars; i++) {
                strncpy(&ctmp, sptr+i, 1);
                found = MapFonts(psa, (int) (ctmp & 255), &fc);
                if (i == 0) {
                        old_font = fc.font;
                }
                fcount++;
                current_font = fc.font;
                if ((current_font != old_font) || (i == (num_chars-1))) {
                        if (i == (num_chars-1)) {
                                fcount++;
                                old_font = current_font;
                        }
                        tptr = (char *) calloc(sizeof(char), fcount);
                        for (j = 0; j < fcount-1; j++) {
                                strncpy(&ctmp, sptr+strpos+j, 1);
                                found = MapFonts(psa, (int) (ctmp & 255), &fc);
                                *(tptr+j) = (char) fc.char_num;
                        }

                        if (((psa->attributes.text_path) == RIGHT_TEXT_PATH) ||
                           ((psa->attributes.text_path) == LEFT_TEXT_PATH)) {
                                if (fc.outline == FALSE) {
                                   (void) fprintf(psa->file_pointer,
                                           "%s %d Fs %d 0 (",
                                           PSFontNames[old_font], PSFontScale,
                                           PSCharSpace);
                                   PSoutput_string(psa, tptr);
                                   (void) fprintf(psa->file_pointer,") As ");
                                }
                                else {
                                   (void) fprintf(psa->file_pointer,
                                           "%s %d Fs %d (",
                                           PSFontNames[old_font], PSFontScale,
                                           PSCharSpace);
                                   PSoutput_string(psa, tptr);
                                   (void) fprintf(psa->file_pointer,") Oh ");
                                }
                        }
                        else if (((psa->attributes.text_path)==UP_TEXT_PATH) ||
                            ((psa->attributes.text_path) == DOWN_TEXT_PATH)) {
                                if (fc.outline == FALSE) {
                                   (void) fprintf(psa->file_pointer,
                                           "%s %d Fs %d (",
                                           PSFontNames[old_font], PSFontScale,
                                           PSFontScale + PSCharSpace);
                                   PSoutput_string(psa, tptr);
                                   (void) fprintf(psa->file_pointer,") Y ");
                                }
                                else {
                                   (void) fprintf(psa->file_pointer,
                                           "%s %d Fs %d (",
                                           PSFontNames[old_font], PSFontScale,
                                           PSFontScale + PSCharSpace);
                                   PSoutput_string(psa, tptr);
                                   (void) fprintf(psa->file_pointer,") Ov ");
                                }
                        }

                        free(tptr);
                        old_font = current_font;
                        strpos = strpos + fcount - 1;
                        fcount = 1;
                }
        }

        /*
         *  Restore the graphics state to what it was before.
         */
        (void) fprintf(psa->file_pointer,"Gr\n");

        return(return_value);
}

/*ARGSUSED*/
int ps_FillArea(gksc)
        GKSC    *gksc;
{
        PSPoint *pptr = (PSPoint *) gksc->p.list;
        PSddp   *psa = (PSddp *) gksc->ddp;
        int     requested_color, current_color;
        int     npoints = gksc->p.num, i, linewidth, ier;
        int     StackSize, PathSize;

        StackSize = psa->stack_size;
        PathSize = psa->path_size;
        if ((ier = check_EPS(gksc)) != 0) return(ier);

        if (psa->pict_empty) {
                PSpreamble(psa, FOR_PICTURE);
                psa->pict_empty = FALSE;
        }
                
        if (psa->color == COLOR) { 
           requested_color = psa->attributes.fill_colr_ind;
           current_color = psa->attributes.ps_colr_ind;
           if (requested_color != current_color) {
                   /****RLB (void) fprintf(psa->file_pointer, "%d O\n", requested_color);*/
                   writePSColor(psa->file_pointer, requested_color);
                   psa->attributes.ps_colr_ind = requested_color;
           }
        }
        if (psa->color == MONO) {         
           requested_color = psa->attributes.fill_colr_ind;
           current_color = psa->attributes.ps_colr_ind;
           if (requested_color != current_color) {
                   if (requested_color == 0) {
                        (void) fprintf(psa->file_pointer,"0 O\n");
                   }
                   else {
                        (void) fprintf(psa->file_pointer,"1 O\n");
                   } 
                   psa->attributes.ps_colr_ind = requested_color;
           }
        }

        /*
         *  Set up the line attributes to be used.
         */
        if ((psa->attributes.fill_int_style != SOLID_FILL && psa->attributes.fill_int_style != SOLID_TEXT_FILL) ||
                        (npoints >= PathSize)) {
                fprintf(psa->file_pointer, "I\n");
                fprintf(psa->file_pointer, "H\n");
                fprintf(psa->file_pointer,"[] 0 D\n");
                linewidth = 
                        (int) (1.1*(psa->dspace.yspan)*(psa->sfill_spacing));
                fprintf(psa->file_pointer, "%d W\n", linewidth);
        }

        if ((psa->ps_clip.null) && 
                (psa->attributes.clip_ind == CLIPPING_ON)) return(0);

        switch(psa->attributes.fill_int_style) {
        case HOLLOW_FILL:   /* Put out polyline */
            OutputPolyline (gksc, 1);
            fprintf(psa->file_pointer, "K\n");
            break;
        case SOLID_FILL:
        case SOLID_TEXT_FILL:
            if (npoints < StackSize) {           /* Short form PS fill */
              PSprint_points((PSddp *) gksc->ddp, pptr, npoints, POLY_FILL);
            }
            else if (npoints < PathSize) {       /* Long form PS fill */
              PSprint_points((PSddp *) gksc->ddp, pptr, 1, MOVETO);
              for (i = 1; i < npoints; i++) {
                PSprint_points((PSddp *) gksc->ddp, pptr+i, 1, LINETO);
                if ( i % (POINTS_PER_LINE - 2) == 0)
                        fprintf(psa->file_pointer, "\n");
              }
              fprintf(psa->file_pointer, "N E\n");
            }
            else {                               /* Software fill  */
              ps_SoftFill (gksc, 0., psa->sfill_spacing);
              fprintf(psa->file_pointer, "K\n");
            }
            break;
        case PATTERN_FILL:  /* currently not implemented, issue polyline */
            OutputPolyline (gksc, 1);
            fprintf(psa->file_pointer, "K\n");
            break;
        case HATCH_FILL:
            switch (psa->attributes.fill_style_ind) {
            case HORIZONTAL_HATCH:
                ps_SoftFill (gksc, 0., psa->hatch_spacing);  
                break;
            case VERTICAL_HATCH:
                ps_SoftFill (gksc, 90., psa->hatch_spacing);  
                break;
            case POSITIVE_HATCH:
                ps_SoftFill (gksc, 45., psa->hatch_spacing);  
                break;
            case NEGATIVE_HATCH:
                ps_SoftFill (gksc, 135., psa->hatch_spacing);  
                break;
            case HORIZ_VERT_HATCH:
                ps_SoftFill (gksc, 0., psa->hatch_spacing);  
                fprintf(psa->file_pointer, "K\n");
                ps_SoftFill (gksc, 90., psa->hatch_spacing);  
                break;
            case POS_NEG_HATCH:
                ps_SoftFill (gksc, 45., psa->hatch_spacing);  
                fprintf(psa->file_pointer, "K\n");
                ps_SoftFill (gksc, 135., psa->hatch_spacing);  
                break;
            default:
                OutputPolyline (gksc, 1);
                break;
            }
            fprintf(psa->file_pointer, "K\n");
            break;
        default:
            OutputPolyline (gksc, 1);
            fprintf(psa->file_pointer, "K\n");
            break;
        }

        /*
         *  Restore line attributes.
         */
        if ((psa->attributes.fill_int_style != SOLID_FILL && psa->attributes.fill_int_style != SOLID_TEXT_FILL) ||
                        (npoints >= PathSize)) {
                fprintf(psa->file_pointer, "W ");
                fprintf(psa->file_pointer, "D\n");
        }

        return(0);
}


/*ARGSUSED*/
int ps_Cellarray(gksc)
        GKSC    *gksc;
{
        PSPoint *pptr = (PSPoint *) gksc->p.list;
        int             *iptr = (int *) gksc->i.list;
        PSddp   *psa = (PSddp *) gksc->ddp;

        int     *xptr = (int *) gksc->x.list;  /* color index array */

        int     nx = iptr[0];   /* number of cols       */
        int     ny = iptr[1];   /* number of rows       */

        int     index, intensity;
        int     i, j, color_index, ier = 0;
        float   x_scale, y_scale, ftmp;

        PSPoint *Pptr = &pptr[0];
        PSPoint *Qptr = &pptr[1];
        PSPoint *Rptr = &pptr[2];

        if ((ier = check_EPS(gksc)) != 0) return(ier);

        if (psa->pict_empty) {
                PSpreamble(psa, FOR_PICTURE);
                psa->pict_empty = FALSE;
        }
                
        if ((psa->ps_clip.null) && 
                (psa->attributes.clip_ind == CLIPPING_ON)) return(0);

        x_scale = (float) nx / ((Rptr->x - Pptr->x) * (psa->dspace.xspan));
        y_scale = (float) ny / ((Qptr->y - Rptr->y) * (psa->dspace.yspan));

        
        (void) fprintf(psa->file_pointer,"Gs %d %d Tr\n",
           (psa->dspace.llx) + (int)(((float)(psa->dspace.xspan))*Pptr[0].x),
           (psa->dspace.lly) + (int)(((float)(psa->dspace.yspan))*Pptr[0].y));

        (void) fprintf(psa->file_pointer, "%d %d 8 [%f 0 0 %f 0 0] ",
                        nx, ny, x_scale, y_scale);

        if (psa->color == COLOR) {       
           (void) fprintf(psa->file_pointer,"%d Cc\n", nx-1);
        }
        else {
           (void) fprintf(psa->file_pointer, "\n/strn %d string def\n", nx);
           (void) fprintf(psa->file_pointer, "{Cu strn Rh pop} Im\n");
        }

        for (i = 0, index = 0; i < ny; i++) {
                for (j = 0; j < nx; j++, index++) {
                        if ( ((index % 40) == 0) && (index > 0)) {
                                (void) fprintf(psa->file_pointer,"\n");
                        }
                        color_index = xptr[index];
                        float tred, tgreen, tblue;
                        index2rgb(psa->color_map, color_index, &tred, &tgreen, &tblue);
                        if (psa->color == COLOR) {
                            fprintf(psa->file_pointer, "%02X%02X%02X",
                                    (int)(255. * tred),
                                    (int)(255. * tgreen), 
                                    (int)(255. * tblue));
                        }
                        else {
                            ftmp = 0.30*tred + 0.59*tgreen + 0.11*tblue;
                           intensity = (int) (255. * ftmp);
                           (void) fprintf(psa->file_pointer, "%02X", intensity);
                        }
                }
        }
        (void) fprintf(psa->file_pointer, "\nGr\n");
        
        return(0);
}

/*ARGSUSED*/
int ps_SetLinetype(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int             *iptr = (int *) gksc->i.list;

        psa->attributes.linetype = iptr[0];
        return(0);
}

/*ARGSUSED*/
int ps_SetLineWidthScaleFactor(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        float           *fptr = (float *) gksc->f.list;

        psa->attributes.linewidth = fptr[0];
        return(0);
}


/*ARGSUSED*/
int ps_SetPolylineColorIndex(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int             *xptr = (int *) gksc->x.list;

        psa->attributes.line_colr_ind = xptr[0];
        return(0);
}

/*ARGSUSED*/
int ps_SetMarkerType(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int             *iptr = (int *) gksc->i.list;

        psa->attributes.marker_type = iptr[0];
        return(0);
}


/*ARGSUSED*/
int ps_SetMarkerSizeScaleFactor(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        float           *fptr = (float *) gksc->f.list;

        psa->attributes.marker_size = fptr[0];
        return(0);
}

/*ARGSUSED*/
int ps_SetPolymarkerColorIndex(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int             *xptr = (int *) gksc->x.list;

        psa->attributes.marker_colr_ind = xptr[0];
        return(0);
}


/*ARGSUSED*/
int ps_SetTextFontAndPrecision(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int             *iptr = (int *) gksc->i.list;

        psa->attributes.text_font = iptr[0];
        psa->attributes.text_prec = iptr[1];

        return(0);
}

/*ARGSUSED*/
int ps_SetCharacterExpansionFactor(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        float           *fptr = (float *) gksc->f.list;

        psa->attributes.char_expan = fptr[0];
        return(0);
}


/*ARGSUSED*/
int ps_SetCharacterSpacing(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        float           *fptr = (float *) gksc->f.list;

        psa->attributes.char_space = fptr[0];
        return(0);
}

/*ARGSUSED*/
int ps_SetTextColorIndex(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int             *xptr = (int *) gksc->x.list;

        psa->attributes.text_colr_ind = xptr[0];
        return(0);
}


/*ARGSUSED*/
int ps_SetCharacterHeightAndUpVector(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        float           *fptr = (float *) gksc->f.list;

        double          up_x = (double) fptr[0];
        double          up_y = (double) fptr[2];
        double          base_x = (double) fptr[1];
        double          base_y = (double) fptr[3];

        /*
         *  Transform to workstation viewport space.
         */
        up_x *= (psa->transform).x_scale;
        up_y *= (psa->transform).y_scale;
        base_x *= (psa->transform).x_scale;
        base_y *= (psa->transform).y_scale;
        psa->attributes.char_up_vec_x = (float) up_x;
        psa->attributes.char_up_vec_y = (float) up_y;
        psa->attributes.char_base_vec_x = (float) base_x;
        psa->attributes.char_base_vec_y = (float) base_y;

        psa->attributes.char_ht = MAG(up_x,up_y);

        return(0);
}

/*ARGSUSED*/
int ps_SetTextPath(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int             *iptr = (int *) gksc->i.list;

        psa->attributes.text_path = iptr[0];
        return(0);
}


/*ARGSUSED*/
int ps_SetTextAlignment(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int             *iptr = (int *) gksc->i.list;

        psa->attributes.text_align_horiz = iptr[0];
        psa->attributes.text_align_vert = iptr[1];
        return(0);
}

/*ARGSUSED*/
int ps_SetFillAreaInteriorStyle(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int             *iptr = (int *) gksc->i.list;

        psa->attributes.fill_int_style = iptr[0];
        return(0);
}


/*ARGSUSED*/
int ps_SetFillAreaStyleIndex(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int             *iptr = (int *) gksc->i.list;
        psa->attributes.fill_style_ind = iptr[0];
        return(0);
}

/*ARGSUSED*/
int ps_SetFillAreaColorIndex(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int             *xptr = (int *) gksc->x.list;

        psa->attributes.fill_colr_ind = xptr[0];
        return(0);
}


/*ARGSUSED*/
int ps_SetColorRepresentation(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int     *xptr = (int *) gksc->x.list;
        PSColor *rgbptr = (PSColor *) gksc->rgb.list;

        unsigned        index   = (unsigned) xptr[0];

        if (index & ARGB_MASK)  /* argb color? */
            return 1;

        float           r =  rgbptr[0].r;
        float           g =  rgbptr[0].g;
        float           b =  rgbptr[0].b;

        psa->color_map[3*index  ] = r;
        psa->color_map[3*index+1] = g;
        psa->color_map[3*index+2] = b;
        psa->color_map[index+3*MAX_COLORS] = 1;

        if ((index == 0)  && (psa->suppress_flag != 1) &&
                             (psa->suppress_flag != 2)) {
          /*
           *  Do not flag the background setting if the defined
           *  color is white.
           */
           if ((psa->color_map[0] != 1.) || (psa->color_map[1] != 1.) ||
               (psa->color_map[2] != 1.)) {
                  psa->background = TRUE;
           }
        }

        if (psa->pict_empty == FALSE) {
          (void) fprintf(psa->file_pointer,"T %3d [%5.3f %5.3f %5.3f] U\n",
                          index, r, g, b);

          /*
           *  If the index being defined is the same as the current
           *  color, reset the current color to the newly defined value.
           */
          if (psa->attributes.ps_colr_ind == (int) index ) {
                  (void) fprintf(psa->file_pointer,"%d O\n",index);
          }

          if ((index == 0) && (psa->suppress_flag != 1) &&
                              (psa->suppress_flag != 2)) {
           /*
            *  Do not put out background if the defined color is white.
            */
            if ((psa->color_map[0] != 1.) || (psa->color_map[1] != 1.) ||
                (psa->color_map[2] != 1.)) {
              PSbackground(psa);
              psa->background = TRUE;
            }
          }
        }

        return(0);
}

/*ARGSUSED*/
int ps_SetClipIndicator(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int     *iptr = (int *) gksc->i.list, np = 2;
        int     orig_clip_ind, rec_chg = 0, default_rect = 0;

        float   tclipx[2], tclipy[2];

        PSClipRect *Crect;

        /*
         *  Check to see if the PostScript clipping rectangle before 
         *  this call is the default rectangle.
         */
        if ((psa->ps_clip.llx == psa->dspace.llx) && 
            (psa->ps_clip.lly == psa->dspace.lly) &&
            (psa->ps_clip.urx == psa->dspace.urx) &&
            (psa->ps_clip.ury == psa->dspace.ury)) {
                default_rect = 1;
        }

        /*
         *  Save the current GKS viewport rectangle and current clip
         *  indicator.  The GKS clipping rectangle is stored in ndc.
         */
        ps_ConvPoints(gksc->ddp, tclipx, tclipy, &gksc->p, &np, COOKED_TO_RAW);
        psa->gks_clip.llx = tclipx[0];
        psa->gks_clip.lly = tclipy[0];
        psa->gks_clip.urx = tclipx[1];
        psa->gks_clip.ury = tclipy[1];
        orig_clip_ind = psa->attributes.clip_ind;
        psa->attributes.clip_ind = iptr[0];

        /*
         *  Calculate the new PostScript clip rectangle and check to
         *  to see if it has changed and store it if it has.
         */
        Crect = GetPSClipping (psa, psa->gks_clip, psa->tsystem.window);
        if ((Crect->llx != psa->ps_clip.llx) || 
            (Crect->lly != psa->ps_clip.lly) ||
            (Crect->urx != psa->ps_clip.urx) ||
            (Crect->ury != psa->ps_clip.ury)) {
                psa->ps_clip.llx = Crect->llx;
                psa->ps_clip.lly = Crect->lly;
                psa->ps_clip.urx = Crect->urx;
                psa->ps_clip.ury = Crect->ury;
                psa->ps_clip.null = Crect->null;
                rec_chg = 1;
        }

        if (psa->pict_empty == FALSE) {
          if (iptr[0] == CLIPPING_ON) {
                if (orig_clip_ind == CLIPPING_OFF) {
                        if (default_rect == 1) {
                                return(0);
                        }
                        else {
                        /*
                         *  Before putting out the current clipping rectangle,
                         *  save all relevant current attributes except the
                         *  clipping rectangle; restore the clipping rectangle
                         *  to its original default; resave the default 
                         *  rectangle; restore the current attributes.  This
                         *  convoluted logic is made necessary bacause there
                         *  is no way to increase the size of the clipping
                         *  rectangle outside of using "initclip", and that
                         *  command is not allowed in EPS files.
                         */
                                (void) fprintf(psa->file_pointer, 
                                                "Ls Gr Gs Lr\n");
                                OutputClipping (psa, PS_CLIPPING_RECT);
                        }
                }
                else {
                        if (rec_chg == 1) { 
                                (void) fprintf(psa->file_pointer, 
                                                "Ls Gr Gs Lr\n");
                                OutputClipping (psa, PS_CLIPPING_RECT);
                        }
                        else {
                                return(0);
                        }
                }
          }
          else {
                if (orig_clip_ind == CLIPPING_OFF) {
                        return(0);
                }
                else {
                        if (default_rect == 1) { 
                                return(0);
                        }
                        else {
                                (void) fprintf(psa->file_pointer, 
                                                "Ls Gr Gs Lr\n");
                                OutputClipping (psa, DEFAULT_CLIPPING_RECT);
                        }
                }
          }
        }
        return(0);
}


/*ARGSUSED*/
int ps_GetColorRepresentation(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int     *xptr = (int *) gksc->x.list;
        PSColor *rgbptr = (PSColor *) gksc->rgb.list;

        int     index   = xptr[0];

        index2rgb(psa->color_map, index, &rgbptr[0].r, &rgbptr[0].g, &rgbptr[0].b);

        return(0);
}

void ps_NcarLogo(GKSC *gksc,float x,float y,float size)
{
  PSddp   *psa;
  int     llx,lly,urx,ury,xspan,yspan;
  float   scaling,xp,yp,scale_factor;
  char    *translate,*scale;

  FILE    *fp;

  psa = (PSddp *) gksc->ddp;

  if (psa->pict_empty == TRUE) {
     PSpreamble(psa, FOR_PICTURE);
  }
                        
  psa->pict_empty = FALSE;

  fp = psa->file_pointer;
  scaling = psa->scaling;
  translate = (char *) calloc(33,sizeof(char));
  scale = (char *) calloc(29,sizeof(char));

  llx = psa->dspace.llx;
  lly = psa->dspace.lly;
  urx = psa->dspace.urx;
  ury = psa->dspace.ury;
  xspan = psa->dspace.xspan;
  yspan = psa->dspace.yspan;

  scale_factor = (scaling * size * yspan)/67.;
  sprintf(scale   ,"%10.2f ", scale_factor);
  sprintf(scale+11,"%10.2f ", scale_factor);
  sprintf(scale+22,"scale\n");

  xp = scaling*((float) llx + x*(float) xspan);
  yp = scaling*((float) lly + y*(float) yspan);
  sprintf(translate   ,"%10.2f ", xp);
  sprintf(translate+11,"%10.2f ", yp);
  sprintf(translate+22,"translate\n");

  /* Macros for the NCAR Logo */
  (void) fprintf(fp, "/f    {fill}         bind def\n");
  (void) fprintf(fp, "/ldf  {load}         bind def\n");
  (void) fprintf(fp, "/mo   {moveto}       bind def\n");
  (void) fprintf(fp, "/ln   {lineto}       bind def\n");
  (void) fprintf(fp, "/clp  {clip newpath} bind def\n");
  (void) fprintf(fp, "/cv   {curveto}      bind def\n");
  (void) fprintf(fp, "/cmyk {setcmykcolor} bind def\n");
  (void) fprintf(fp, "/cp   {closepath}    bind def\n");
  (void) fprintf(fp, "\n");

  (void) fprintf(fp, "gsave\n");
  (void) fprintf(fp, "[0 0 0 0 0 0] defaultmatrix setmatrix\n");

  if (psa->orientation == LANDSCAPE) {
    (void) fprintf(fp,"%f %f %f %f Ld\n",
                   scaling*(psa->dspace.llx), scaling*(psa->dspace.lly),
                   scaling*(psa->dspace.urx), scaling*(psa->dspace.ury));
  }

  (void) fprintf(fp, "%s", translate);
  (void) fprintf(fp, "%s", scale);
  (void) fprintf(fp, "-40 33 translate\n");
  (void) fprintf(fp, "1 -1 scale\n");
  free(scale);
  free(translate);

  (void) fprintf(fp, "\n");
  (void) fprintf(fp, "1 setflat\n");
  (void) fprintf(fp, "0 0 mo\n");
  (void) fprintf(fp, "0 66.7773 ln\n");
  (void) fprintf(fp, "88.833 66.7773 ln\n");
  (void) fprintf(fp, "88.833 0 ln\n");
  (void) fprintf(fp, "clp\n");
  (void) fprintf(fp, "gsave\n");
  (void) fprintf(fp, "0 0 mo\n");
  (void) fprintf(fp, "56.3926 18.7793 ln\n");
  (void) fprintf(fp, "56.3926 44.1348 ln\n");
  (void) fprintf(fp, "0 44.1348 ln\n");
  (void) fprintf(fp, "0 0 ln\n");
  (void) fprintf(fp, "clp\n");
  (void) fprintf(fp, "gsave\n");
  (void) fprintf(fp, "91.2529 90.4863 mo\n");
  (void) fprintf(fp, "91.2529 154.908 34.9492 207.136 -34.4951 207.136 cv\n");
  (void) fprintf(fp, "-103.943 207.136 -160.242 154.908 -160.242 90.4863 cv\n");
  (void) fprintf(fp, "-160.242 26.0635 -103.943 -26.1572 -34.4951 -26.1572 cv\n");
  (void) fprintf(fp, "34.9492 -26.1572 91.2529 26.0635 91.2529 90.4863 cv\n");
  (void) fprintf(fp, "clp\n");
  (void) fprintf(fp, "-161.243 -27.1562 mo\n");
  (void) fprintf(fp, "92.2529 -27.1562 ln\n");
  (void) fprintf(fp, "92.2529 208.137 ln\n");
  (void) fprintf(fp, "-161.243 208.137 ln\n");
  (void) fprintf(fp, "-161.243 -27.1562 ln\n");
  (void) fprintf(fp, "1 0.51 0 0.3 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "76.0459 90.4883 mo\n");
  (void) fprintf(fp, "76.0459 147.121 26.5576 193.027 -34.4971 193.027 cv\n");
  (void) fprintf(fp, "-95.5439 193.027 -145.036 147.121 -145.036 90.4883 cv\n");
  (void) fprintf(fp, "-145.036 33.8594 -95.5439 -12.0469 -34.4971 -12.0469 cv\n");
  (void) fprintf(fp, "26.5576 -12.0469 76.0459 33.8594 76.0459 90.4883 cv\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "76.0186 90.4883 mo\n");
  (void) fprintf(fp, "76.0186 147.105 26.542 193 -34.4971 193 cv\n");
  (void) fprintf(fp, "-95.5322 193 -145.009 147.105 -145.009 90.4883 cv\n");
  (void) fprintf(fp, "-145.009 33.8711 -95.5322 -12.0234 -34.4971 -12.0234 cv\n");
  (void) fprintf(fp, "26.542 -12.0234 76.0186 33.8711 76.0186 90.4883 cv\n");
  (void) fprintf(fp, "0.993882 0.50688 0 0.298165 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.9912 90.4883 mo\n");
  (void) fprintf(fp, "75.9912 147.09 26.5264 192.977 -34.4971 192.977 cv\n");
  (void) fprintf(fp, "-95.5166 192.977 -144.981 147.09 -144.981 90.4883 cv\n");
  (void) fprintf(fp, "-144.981 33.8867 -95.5166 -11.9961 -34.4971 -11.9961 cv\n");
  (void) fprintf(fp, "26.5264 -11.9961 75.9912 33.8867 75.9912 90.4883 cv\n");
  (void) fprintf(fp, "0.987778 0.503767 0 0.296333 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.9639 90.4883 mo\n");
  (void) fprintf(fp, "75.9639 147.078 26.5107 192.949 -34.4971 192.949 cv\n");
  (void) fprintf(fp, "-95.501 192.949 -144.954 147.078 -144.954 90.4883 cv\n");
  (void) fprintf(fp, "-144.954 33.9023 -95.501 -11.9727 -34.4971 -11.9727 cv\n");
  (void) fprintf(fp, "26.5107 -11.9727 75.9639 33.9023 75.9639 90.4883 cv\n");
  (void) fprintf(fp, "0.981687 0.500661 0 0.294506 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.9365 90.4883 mo\n");
  (void) fprintf(fp, "75.9365 147.062 26.4951 192.926 -34.4971 192.926 cv\n");
  (void) fprintf(fp, "-95.4854 192.926 -144.927 147.062 -144.927 90.4883 cv\n");
  (void) fprintf(fp, "-144.927 33.918 -95.4854 -11.9453 -34.4971 -11.9453 cv\n");
  (void) fprintf(fp, "26.4951 -11.9453 75.9365 33.918 75.9365 90.4883 cv\n");
  (void) fprintf(fp, "0.97561 0.497561 0 0.292683 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.9092 90.4883 mo\n");
  (void) fprintf(fp, "75.9092 147.051 26.4795 192.898 -34.4971 192.898 cv\n");
  (void) fprintf(fp, "-95.4697 192.898 -144.899 147.051 -144.899 90.4883 cv\n");
  (void) fprintf(fp, "-144.899 33.9297 -95.4697 -11.9219 -34.4971 -11.9219 cv\n");
  (void) fprintf(fp, "26.4795 -11.9219 75.9092 33.9297 75.9092 90.4883 cv\n");
  (void) fprintf(fp, "0.969546 0.494469 0 0.290864 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.8818 90.4883 mo\n");
  (void) fprintf(fp, "75.8818 147.035 26.4639 192.875 -34.4971 192.875 cv\n");
  (void) fprintf(fp, "-95.4541 192.875 -144.872 147.035 -144.872 90.4883 cv\n");
  (void) fprintf(fp, "-144.872 33.9414 -95.4541 -11.8945 -34.4971 -11.8945 cv\n");
  (void) fprintf(fp, "26.4639 -11.8945 75.8818 33.9414 75.8818 90.4883 cv\n");
  (void) fprintf(fp, "0.963496 0.491383 0 0.289049 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.8545 90.4883 mo\n");
  (void) fprintf(fp, "75.8545 147.02 26.4482 192.848 -34.4971 192.848 cv\n");
  (void) fprintf(fp, "-95.4385 192.848 -144.845 147.02 -144.845 90.4883 cv\n");
  (void) fprintf(fp, "-144.845 33.957 -95.4385 -11.8711 -34.4971 -11.8711 cv\n");
  (void) fprintf(fp, "26.4482 -11.8711 75.8545 33.957 75.8545 90.4883 cv\n");
  (void) fprintf(fp, "0.957459 0.488304 0 0.287238 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.8271 90.4883 mo\n");
  (void) fprintf(fp, "75.8271 147.008 26.4326 192.824 -34.4971 192.824 cv\n");
  (void) fprintf(fp, "-95.4268 192.824 -144.817 147.008 -144.817 90.4883 cv\n");
  (void) fprintf(fp, "-144.817 33.9727 -95.4268 -11.8438 -34.4971 -11.8438 cv\n");
  (void) fprintf(fp, "26.4326 -11.8438 75.8271 33.9727 75.8271 90.4883 cv\n");
  (void) fprintf(fp, "0.951436 0.485232 0 0.285431 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.7998 90.4883 mo\n");
  (void) fprintf(fp, "75.7998 146.992 26.417 192.797 -34.4971 192.797 cv\n");
  (void) fprintf(fp, "-95.4111 192.797 -144.79 146.992 -144.79 90.4883 cv\n");
  (void) fprintf(fp, "-144.79 33.9844 -95.4111 -11.8203 -34.4971 -11.8203 cv\n");
  (void) fprintf(fp, "26.417 -11.8203 75.7998 33.9844 75.7998 90.4883 cv\n");
  (void) fprintf(fp, "0.945426 0.482167 0 0.283628 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.7725 90.4883 mo\n");
  (void) fprintf(fp, "75.7725 146.98 26.4053 192.773 -34.4971 192.773 cv\n");
  (void) fprintf(fp, "-95.3955 192.773 -144.763 146.98 -144.763 90.4883 cv\n");
  (void) fprintf(fp, "-144.763 34 -95.3955 -11.793 -34.4971 -11.793 cv\n");
  (void) fprintf(fp, "26.4053 -11.793 75.7725 34 75.7725 90.4883 cv\n");
  (void) fprintf(fp, "0.93943 0.479109 0 0.281829 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.7451 90.4883 mo\n");
  (void) fprintf(fp, "75.7451 146.965 26.3896 192.746 -34.4971 192.746 cv\n");
  (void) fprintf(fp, "-95.3799 192.746 -144.735 146.965 -144.735 90.4883 cv\n");
  (void) fprintf(fp, "-144.735 34.0117 -95.3799 -11.7695 -34.4971 -11.7695 cv\n");
  (void) fprintf(fp, "26.3896 -11.7695 75.7451 34.0117 75.7451 90.4883 cv\n");
  (void) fprintf(fp, "0.933448 0.476058 0 0.280034 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.7178 90.4883 mo\n");
  (void) fprintf(fp, "75.7178 146.949 26.374 192.723 -34.4971 192.723 cv\n");
  (void) fprintf(fp, "-95.3643 192.723 -144.708 146.949 -144.708 90.4883 cv\n");
  (void) fprintf(fp, "-144.708 34.0273 -95.3643 -11.7422 -34.4971 -11.7422 cv\n");
  (void) fprintf(fp, "26.374 -11.7422 75.7178 34.0273 75.7178 90.4883 cv\n");
  (void) fprintf(fp, "0.927479 0.473014 0 0.278244 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.6904 90.4883 mo\n");
  (void) fprintf(fp, "75.6904 146.938 26.3584 192.695 -34.4971 192.695 cv\n");
  (void) fprintf(fp, "-95.3486 192.695 -144.681 146.938 -144.681 90.4883 cv\n");
  (void) fprintf(fp, "-144.681 34.043 -95.3486 -11.7188 -34.4971 -11.7188 cv\n");
  (void) fprintf(fp, "26.3584 -11.7188 75.6904 34.043 75.6904 90.4883 cv\n");
  (void) fprintf(fp, "0.921524 0.469977 0 0.276457 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.6631 90.4883 mo\n");
  (void) fprintf(fp, "75.6631 146.922 26.3428 192.672 -34.4971 192.672 cv\n");
  (void) fprintf(fp, "-95.333 192.672 -144.653 146.922 -144.653 90.4883 cv\n");
  (void) fprintf(fp, "-144.653 34.0547 -95.333 -11.6914 -34.4971 -11.6914 cv\n");
  (void) fprintf(fp, "26.3428 -11.6914 75.6631 34.0547 75.6631 90.4883 cv\n");
  (void) fprintf(fp, "0.915583 0.466947 0 0.274675 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.6357 90.4883 mo\n");
  (void) fprintf(fp, "75.6357 146.91 26.3271 192.645 -34.4971 192.645 cv\n");
  (void) fprintf(fp, "-95.3174 192.645 -144.626 146.91 -144.626 90.4883 cv\n");
  (void) fprintf(fp, "-144.626 34.0703 -95.3174 -11.668 -34.4971 -11.668 cv\n");
  (void) fprintf(fp, "26.3271 -11.668 75.6357 34.0703 75.6357 90.4883 cv\n");
  (void) fprintf(fp, "0.909655 0.463924 0 0.272897 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.6084 90.4883 mo\n");
  (void) fprintf(fp, "75.6084 146.895 26.3154 192.621 -34.4971 192.621 cv\n");
  (void) fprintf(fp, "-95.3018 192.621 -144.599 146.895 -144.599 90.4883 cv\n");
  (void) fprintf(fp, "-144.599 34.082 -95.3018 -11.6406 -34.4971 -11.6406 cv\n");
  (void) fprintf(fp, "26.3154 -11.6406 75.6084 34.082 75.6084 90.4883 cv\n");
  (void) fprintf(fp, "0.903741 0.460908 0 0.271122 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.5811 90.4883 mo\n");
  (void) fprintf(fp, "75.5811 146.879 26.2998 192.594 -34.4971 192.594 cv\n");
  (void) fprintf(fp, "-95.2861 192.594 -144.571 146.879 -144.571 90.4883 cv\n");
  (void) fprintf(fp, "-144.571 34.0977 -95.2861 -11.6172 -34.4971 -11.6172 cv\n");
  (void) fprintf(fp, "26.2998 -11.6172 75.5811 34.0977 75.5811 90.4883 cv\n");
  (void) fprintf(fp, "0.897841 0.457899 0 0.269352 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.5537 90.4883 mo\n");
  (void) fprintf(fp, "75.5537 146.867 26.2842 192.566 -34.4971 192.566 cv\n");
  (void) fprintf(fp, "-95.2744 192.566 -144.544 146.867 -144.544 90.4883 cv\n");
  (void) fprintf(fp, "-144.544 34.1133 -95.2744 -11.5898 -34.4971 -11.5898 cv\n");
  (void) fprintf(fp, "26.2842 -11.5898 75.5537 34.1133 75.5537 90.4883 cv\n");
  (void) fprintf(fp, "0.891955 0.454897 0 0.267587 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.5264 90.4883 mo\n");
  (void) fprintf(fp, "75.5264 146.852 26.2686 192.543 -34.4971 192.543 cv\n");
  (void) fprintf(fp, "-95.2588 192.543 -144.517 146.852 -144.517 90.4883 cv\n");
  (void) fprintf(fp, "-144.517 34.125 -95.2588 -11.5664 -34.4971 -11.5664 cv\n");
  (void) fprintf(fp, "26.2686 -11.5664 75.5264 34.125 75.5264 90.4883 cv\n");
  (void) fprintf(fp, "0.886083 0.451902 0 0.265825 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.499 90.4883 mo\n");
  (void) fprintf(fp, "75.499 146.84 26.2529 192.52 -34.4971 192.52 cv\n");
  (void) fprintf(fp, "-95.2432 192.52 -144.489 146.84 -144.489 90.4883 cv\n");
  (void) fprintf(fp, "-144.489 34.1406 -95.2432 -11.5391 -34.4971 -11.5391 cv\n");
  (void) fprintf(fp, "26.2529 -11.5391 75.499 34.1406 75.499 90.4883 cv\n");
  (void) fprintf(fp, "0.880224 0.448914 0 0.264067 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.4717 90.4883 mo\n");
  (void) fprintf(fp, "75.4717 146.824 26.2373 192.492 -34.4971 192.492 cv\n");
  (void) fprintf(fp, "-95.2275 192.492 -144.462 146.824 -144.462 90.4883 cv\n");
  (void) fprintf(fp, "-144.462 34.1523 -95.2275 -11.5117 -34.4971 -11.5117 cv\n");
  (void) fprintf(fp, "26.2373 -11.5117 75.4717 34.1523 75.4717 90.4883 cv\n");
  (void) fprintf(fp, "0.87438 0.445934 0 0.262314 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.4443 90.4883 mo\n");
  (void) fprintf(fp, "75.4443 146.809 26.2217 192.465 -34.4971 192.465 cv\n");
  (void) fprintf(fp, "-95.2119 192.465 -144.435 146.809 -144.435 90.4883 cv\n");
  (void) fprintf(fp, "-144.435 34.168 -95.2119 -11.4883 -34.4971 -11.4883 cv\n");
  (void) fprintf(fp, "26.2217 -11.4883 75.4443 34.168 75.4443 90.4883 cv\n");
  (void) fprintf(fp, "0.868549 0.44296 0 0.260565 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.417 90.4883 mo\n");
  (void) fprintf(fp, "75.417 146.797 26.2061 192.441 -34.4971 192.441 cv\n");
  (void) fprintf(fp, "-95.1963 192.441 -144.407 146.797 -144.407 90.4883 cv\n");
  (void) fprintf(fp, "-144.407 34.1836 -95.1963 -11.4648 -34.4971 -11.4648 cv\n");
  (void) fprintf(fp, "26.2061 -11.4648 75.417 34.1836 75.417 90.4883 cv\n");
  (void) fprintf(fp, "0.862732 0.439993 0 0.25882 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.3896 90.4883 mo\n");
  (void) fprintf(fp, "75.3896 146.781 26.1904 192.418 -34.4971 192.418 cv\n");
  (void) fprintf(fp, "-95.1807 192.418 -144.38 146.781 -144.38 90.4883 cv\n");
  (void) fprintf(fp, "-144.38 34.1953 -95.1807 -11.4375 -34.4971 -11.4375 cv\n");
  (void) fprintf(fp, "26.1904 -11.4375 75.3896 34.1953 75.3896 90.4883 cv\n");
  (void) fprintf(fp, "0.856929 0.437034 0 0.257079 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.3623 90.4883 mo\n");
  (void) fprintf(fp, "75.3623 146.77 26.1748 192.391 -34.4971 192.391 cv\n");
  (void) fprintf(fp, "-95.1689 192.391 -144.353 146.77 -144.353 90.4883 cv\n");
  (void) fprintf(fp, "-144.353 34.2109 -95.1689 -11.4102 -34.4971 -11.4102 cv\n");
  (void) fprintf(fp, "26.1748 -11.4102 75.3623 34.2109 75.3623 90.4883 cv\n");
  (void) fprintf(fp, "0.851141 0.434082 0 0.255342 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.335 90.4883 mo\n");
  (void) fprintf(fp, "75.335 146.754 26.1631 192.363 -34.4971 192.363 cv\n");
  (void) fprintf(fp, "-95.1533 192.363 -144.325 146.754 -144.325 90.4883 cv\n");
  (void) fprintf(fp, "-144.325 34.2227 -95.1533 -11.3867 -34.4971 -11.3867 cv\n");
  (void) fprintf(fp, "26.1631 -11.3867 75.335 34.2227 75.335 90.4883 cv\n");
  (void) fprintf(fp, "0.845366 0.431137 0 0.25361 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.3076 90.4883 mo\n");
  (void) fprintf(fp, "75.3076 146.738 26.1475 192.34 -34.4971 192.34 cv\n");
  (void) fprintf(fp, "-95.1377 192.34 -144.298 146.738 -144.298 90.4883 cv\n");
  (void) fprintf(fp, "-144.298 34.2383 -95.1377 -11.3633 -34.4971 -11.3633 cv\n");
  (void) fprintf(fp, "26.1475 -11.3633 75.3076 34.2383 75.3076 90.4883 cv\n");
  (void) fprintf(fp, "0.839605 0.428199 0 0.251882 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.2803 90.4883 mo\n");
  (void) fprintf(fp, "75.2803 146.727 26.1318 192.316 -34.4971 192.316 cv\n");
  (void) fprintf(fp, "-95.1221 192.316 -144.271 146.727 -144.271 90.4883 cv\n");
  (void) fprintf(fp, "-144.271 34.2539 -95.1221 -11.3359 -34.4971 -11.3359 cv\n");
  (void) fprintf(fp, "26.1318 -11.3359 75.2803 34.2539 75.2803 90.4883 cv\n");
  (void) fprintf(fp, "0.833859 0.425268 0 0.250158 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.2529 90.4883 mo\n");
  (void) fprintf(fp, "75.2529 146.711 26.1162 192.289 -34.4971 192.289 cv\n");
  (void) fprintf(fp, "-95.1064 192.289 -144.243 146.711 -144.243 90.4883 cv\n");
  (void) fprintf(fp, "-144.243 34.2656 -95.1064 -11.3086 -34.4971 -11.3086 cv\n");
  (void) fprintf(fp, "26.1162 -11.3086 75.2529 34.2656 75.2529 90.4883 cv\n");
  (void) fprintf(fp, "0.828126 0.422344 0 0.248438 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.2217 90.4883 mo\n");
  (void) fprintf(fp, "75.2217 146.699 26.1006 192.262 -34.4971 192.262 cv\n");
  (void) fprintf(fp, "-95.0908 192.262 -144.216 146.699 -144.216 90.4883 cv\n");
  (void) fprintf(fp, "-144.216 34.2812 -95.0908 -11.2852 -34.4971 -11.2852 cv\n");
  (void) fprintf(fp, "26.1006 -11.2852 75.2217 34.2812 75.2217 90.4883 cv\n");
  (void) fprintf(fp, "0.822408 0.419428 0 0.246722 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.1982 90.4883 mo\n");
  (void) fprintf(fp, "75.1982 146.684 26.085 192.238 -34.4971 192.238 cv\n");
  (void) fprintf(fp, "-95.0752 192.238 -144.188 146.684 -144.188 90.4883 cv\n");
  (void) fprintf(fp, "-144.188 34.293 -95.0752 -11.2578 -34.4971 -11.2578 cv\n");
  (void) fprintf(fp, "26.085 -11.2578 75.1982 34.293 75.1982 90.4883 cv\n");
  (void) fprintf(fp, "0.816704 0.416519 0 0.245011 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.167 90.4883 mo\n");
  (void) fprintf(fp, "75.167 146.668 26.0693 192.215 -34.4971 192.215 cv\n");
  (void) fprintf(fp, "-95.0596 192.215 -144.161 146.668 -144.161 90.4883 cv\n");
  (void) fprintf(fp, "-144.161 34.3086 -95.0596 -11.2344 -34.4971 -11.2344 cv\n");
  (void) fprintf(fp, "26.0693 -11.2344 75.167 34.3086 75.167 90.4883 cv\n");
  (void) fprintf(fp, "0.811014 0.413617 0 0.243304 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.1436 90.4883 mo\n");
  (void) fprintf(fp, "75.1436 146.656 26.0576 192.188 -34.4971 192.188 cv\n");
  (void) fprintf(fp, "-95.0439 192.188 -144.13 146.656 -144.13 90.4883 cv\n");
  (void) fprintf(fp, "-144.13 34.3242 -95.0439 -11.207 -34.4971 -11.207 cv\n");
  (void) fprintf(fp, "26.0576 -11.207 75.1436 34.3242 75.1436 90.4883 cv\n");
  (void) fprintf(fp, "0.805338 0.410722 0 0.241601 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.1123 90.4883 mo\n");
  (void) fprintf(fp, "75.1123 146.641 26.042 192.16 -34.4971 192.16 cv\n");
  (void) fprintf(fp, "-95.0322 192.16 -144.106 146.641 -144.106 90.4883 cv\n");
  (void) fprintf(fp, "-144.106 34.3359 -95.0322 -11.1836 -34.4971 -11.1836 cv\n");
  (void) fprintf(fp, "26.042 -11.1836 75.1123 34.3359 75.1123 90.4883 cv\n");
  (void) fprintf(fp, "0.799676 0.407835 0 0.239903 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.0889 90.4883 mo\n");
  (void) fprintf(fp, "75.0889 146.629 26.0264 192.137 -34.4971 192.137 cv\n");
  (void) fprintf(fp, "-95.0166 192.137 -144.075 146.629 -144.075 90.4883 cv\n");
  (void) fprintf(fp, "-144.075 34.3516 -95.0166 -11.1562 -34.4971 -11.1562 cv\n");
  (void) fprintf(fp, "26.0264 -11.1562 75.0889 34.3516 75.0889 90.4883 cv\n");
  (void) fprintf(fp, "0.794029 0.404955 0 0.238209 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.0576 90.4883 mo\n");
  (void) fprintf(fp, "75.0576 146.613 26.0107 192.109 -34.4971 192.109 cv\n");
  (void) fprintf(fp, "-95.001 192.109 -144.052 146.613 -144.052 90.4883 cv\n");
  (void) fprintf(fp, "-144.052 34.3633 -95.001 -11.1328 -34.4971 -11.1328 cv\n");
  (void) fprintf(fp, "26.0107 -11.1328 75.0576 34.3633 75.0576 90.4883 cv\n");
  (void) fprintf(fp, "0.788396 0.402082 0 0.236519 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.0342 90.4883 mo\n");
  (void) fprintf(fp, "75.0342 146.598 25.9951 192.086 -34.4971 192.086 cv\n");
  (void) fprintf(fp, "-94.9854 192.086 -144.021 146.598 -144.021 90.4883 cv\n");
  (void) fprintf(fp, "-144.021 34.3789 -94.9854 -11.1055 -34.4971 -11.1055 cv\n");
  (void) fprintf(fp, "25.9951 -11.1055 75.0342 34.3789 75.0342 90.4883 cv\n");
  (void) fprintf(fp, "0.782778 0.399217 0 0.234833 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "75.0029 90.4883 mo\n");
  (void) fprintf(fp, "75.0029 146.586 25.9795 192.059 -34.4971 192.059 cv\n");
  (void) fprintf(fp, "-94.9697 192.059 -143.993 146.586 -143.993 90.4883 cv\n");
  (void) fprintf(fp, "-143.993 34.3945 -94.9697 -11.082 -34.4971 -11.082 cv\n");
  (void) fprintf(fp, "25.9795 -11.082 75.0029 34.3945 75.0029 90.4883 cv\n");
  (void) fprintf(fp, "0.777173 0.396358 0 0.233152 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.9795 90.4883 mo\n");
  (void) fprintf(fp, "74.9795 146.57 25.9639 192.035 -34.4971 192.035 cv\n");
  (void) fprintf(fp, "-94.9541 192.035 -143.966 146.57 -143.966 90.4883 cv\n");
  (void) fprintf(fp, "-143.966 34.4062 -94.9541 -11.0547 -34.4971 -11.0547 cv\n");
  (void) fprintf(fp, "25.9639 -11.0547 74.9795 34.4062 74.9795 90.4883 cv\n");
  (void) fprintf(fp, "0.771583 0.393508 0 0.231475 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.9482 90.4883 mo\n");
  (void) fprintf(fp, "74.9482 146.559 25.9482 192.008 -34.4971 192.008 cv\n");
  (void) fprintf(fp, "-94.9385 192.008 -143.938 146.559 -143.938 90.4883 cv\n");
  (void) fprintf(fp, "-143.938 34.4219 -94.9385 -11.0312 -34.4971 -11.0312 cv\n");
  (void) fprintf(fp, "25.9482 -11.0312 74.9482 34.4219 74.9482 90.4883 cv\n");
  (void) fprintf(fp, "0.766008 0.390664 0 0.229802 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.9209 90.4883 mo\n");
  (void) fprintf(fp, "74.9209 146.543 25.9326 191.984 -34.4971 191.984 cv\n");
  (void) fprintf(fp, "-94.9268 191.984 -143.911 146.543 -143.911 90.4883 cv\n");
  (void) fprintf(fp, "-143.911 34.4336 -94.9268 -11.0039 -34.4971 -11.0039 cv\n");
  (void) fprintf(fp, "25.9326 -11.0039 74.9209 34.4336 74.9209 90.4883 cv\n");
  (void) fprintf(fp, "0.760447 0.387828 0 0.228134 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.8936 90.4883 mo\n");
  (void) fprintf(fp, "74.8936 146.527 25.917 191.957 -34.4971 191.957 cv\n");
  (void) fprintf(fp, "-94.9111 191.957 -143.884 146.527 -143.884 90.4883 cv\n");
  (void) fprintf(fp, "-143.884 34.4492 -94.9111 -10.9805 -34.4971 -10.9805 cv\n");
  (void) fprintf(fp, "25.917 -10.9805 74.8936 34.4492 74.8936 90.4883 cv\n");
  (void) fprintf(fp, "0.754901 0.384999 0 0.22647 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.8662 90.4883 mo\n");
  (void) fprintf(fp, "74.8662 146.516 25.9053 191.934 -34.4971 191.934 cv\n");
  (void) fprintf(fp, "-94.8955 191.934 -143.856 146.516 -143.856 90.4883 cv\n");
  (void) fprintf(fp, "-143.856 34.4648 -94.8955 -10.9531 -34.4971 -10.9531 cv\n");
  (void) fprintf(fp, "25.9053 -10.9531 74.8662 34.4648 74.8662 90.4883 cv\n");
  (void) fprintf(fp, "0.749369 0.382178 0 0.224811 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.8389 90.4883 mo\n");
  (void) fprintf(fp, "74.8389 146.5 25.8896 191.906 -34.4971 191.906 cv\n");
  (void) fprintf(fp, "-94.8799 191.906 -143.829 146.5 -143.829 90.4883 cv\n");
  (void) fprintf(fp, "-143.829 34.4766 -94.8799 -10.9297 -34.4971 -10.9297 cv\n");
  (void) fprintf(fp, "25.8896 -10.9297 74.8389 34.4766 74.8389 90.4883 cv\n");
  (void) fprintf(fp, "0.743851 0.379364 0 0.223155 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.8115 90.4883 mo\n");
  (void) fprintf(fp, "74.8115 146.488 25.874 191.883 -34.4971 191.883 cv\n");
  (void) fprintf(fp, "-94.8643 191.883 -143.802 146.488 -143.802 90.4883 cv\n");
  (void) fprintf(fp, "-143.802 34.4922 -94.8643 -10.9023 -34.4971 -10.9023 cv\n");
  (void) fprintf(fp, "25.874 -10.9023 74.8115 34.4922 74.8115 90.4883 cv\n");
  (void) fprintf(fp, "0.738349 0.376558 0 0.221505 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.7842 90.4883 mo\n");
  (void) fprintf(fp, "74.7842 146.473 25.8584 191.855 -34.4971 191.855 cv\n");
  (void) fprintf(fp, "-94.8486 191.855 -143.774 146.473 -143.774 90.4883 cv\n");
  (void) fprintf(fp, "-143.774 34.5039 -94.8486 -10.8789 -34.4971 -10.8789 cv\n");
  (void) fprintf(fp, "25.8584 -10.8789 74.7842 34.5039 74.7842 90.4883 cv\n");
  (void) fprintf(fp, "0.73286 0.373759 0 0.219858 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.7568 90.4883 mo\n");
  (void) fprintf(fp, "74.7568 146.457 25.8428 191.832 -34.4971 191.832 cv\n");
  (void) fprintf(fp, "-94.833 191.832 -143.747 146.457 -143.747 90.4883 cv\n");
  (void) fprintf(fp, "-143.747 34.5195 -94.833 -10.8516 -34.4971 -10.8516 cv\n");
  (void) fprintf(fp, "25.8428 -10.8516 74.7568 34.5195 74.7568 90.4883 cv\n");
  (void) fprintf(fp, "0.727387 0.370967 0 0.218216 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.7295 90.4883 mo\n");
  (void) fprintf(fp, "74.7295 146.445 25.8271 191.805 -34.4971 191.805 cv\n");
  (void) fprintf(fp, "-94.8174 191.805 -143.72 146.445 -143.72 90.4883 cv\n");
  (void) fprintf(fp, "-143.72 34.5352 -94.8174 -10.8281 -34.4971 -10.8281 cv\n");
  (void) fprintf(fp, "25.8271 -10.8281 74.7295 34.5352 74.7295 90.4883 cv\n");
  (void) fprintf(fp, "0.721928 0.368183 0 0.216578 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.7021 90.4883 mo\n");
  (void) fprintf(fp, "74.7021 146.43 25.8154 191.777 -34.4971 191.777 cv\n");
  (void) fprintf(fp, "-94.8018 191.777 -143.692 146.43 -143.692 90.4883 cv\n");
  (void) fprintf(fp, "-143.692 34.5469 -94.8018 -10.8008 -34.4971 -10.8008 cv\n");
  (void) fprintf(fp, "25.8154 -10.8008 74.7021 34.5469 74.7021 90.4883 cv\n");
  (void) fprintf(fp, "0.716484 0.365407 0 0.214945 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.6748 90.4883 mo\n");
  (void) fprintf(fp, "74.6748 146.418 25.7998 191.754 -34.4971 191.754 cv\n");
  (void) fprintf(fp, "-94.7861 191.754 -143.665 146.418 -143.665 90.4883 cv\n");
  (void) fprintf(fp, "-143.665 34.5625 -94.7861 -10.7773 -34.4971 -10.7773 cv\n");
  (void) fprintf(fp, "25.7998 -10.7773 74.6748 34.5625 74.6748 90.4883 cv\n");
  (void) fprintf(fp, "0.711054 0.362638 0 0.213316 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.6475 90.4883 mo\n");
  (void) fprintf(fp, "74.6475 146.402 25.7842 191.73 -34.4971 191.73 cv\n");
  (void) fprintf(fp, "-94.7744 191.73 -143.638 146.402 -143.638 90.4883 cv\n");
  (void) fprintf(fp, "-143.638 34.5742 -94.7744 -10.75 -34.4971 -10.75 cv\n");
  (void) fprintf(fp, "25.7842 -10.75 74.6475 34.5742 74.6475 90.4883 cv\n");
  (void) fprintf(fp, "0.705639 0.359876 0 0.211692 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.6201 90.4883 mo\n");
  (void) fprintf(fp, "74.6201 146.387 25.7686 191.703 -34.4971 191.703 cv\n");
  (void) fprintf(fp, "-94.7588 191.703 -143.61 146.387 -143.61 90.4883 cv\n");
  (void) fprintf(fp, "-143.61 34.5898 -94.7588 -10.7266 -34.4971 -10.7266 cv\n");
  (void) fprintf(fp, "25.7686 -10.7266 74.6201 34.5898 74.6201 90.4883 cv\n");
  (void) fprintf(fp, "0.700239 0.357122 0 0.210072 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.5928 90.4883 mo\n");
  (void) fprintf(fp, "74.5928 146.375 25.7529 191.676 -34.4971 191.676 cv\n");
  (void) fprintf(fp, "-94.7432 191.676 -143.583 146.375 -143.583 90.4883 cv\n");
  (void) fprintf(fp, "-143.583 34.6055 -94.7432 -10.6992 -34.4971 -10.6992 cv\n");
  (void) fprintf(fp, "25.7529 -10.6992 74.5928 34.6055 74.5928 90.4883 cv\n");
  (void) fprintf(fp, "0.694854 0.354376 0 0.208456 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.5654 90.4883 mo\n");
  (void) fprintf(fp, "74.5654 146.359 25.7373 191.652 -34.4971 191.652 cv\n");
  (void) fprintf(fp, "-94.7275 191.652 -143.556 146.359 -143.556 90.4883 cv\n");
  (void) fprintf(fp, "-143.556 34.6172 -94.7275 -10.6758 -34.4971 -10.6758 cv\n");
  (void) fprintf(fp, "25.7373 -10.6758 74.5654 34.6172 74.5654 90.4883 cv\n");
  (void) fprintf(fp, "0.689484 0.351637 0 0.206845 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.5381 90.4883 mo\n");
  (void) fprintf(fp, "74.5381 146.348 25.7217 191.629 -34.4971 191.629 cv\n");
  (void) fprintf(fp, "-94.7119 191.629 -143.528 146.348 -143.528 90.4883 cv\n");
  (void) fprintf(fp, "-143.528 34.6328 -94.7119 -10.6484 -34.4971 -10.6484 cv\n");
  (void) fprintf(fp, "25.7217 -10.6484 74.5381 34.6328 74.5381 90.4883 cv\n");
  (void) fprintf(fp, "0.684129 0.348906 0 0.205239 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.5107 90.4883 mo\n");
  (void) fprintf(fp, "74.5107 146.332 25.7061 191.602 -34.4971 191.602 cv\n");
  (void) fprintf(fp, "-94.6963 191.602 -143.501 146.332 -143.501 90.4883 cv\n");
  (void) fprintf(fp, "-143.501 34.6445 -94.6963 -10.6211 -34.4971 -10.6211 cv\n");
  (void) fprintf(fp, "25.7061 -10.6211 74.5107 34.6445 74.5107 90.4883 cv\n");
  (void) fprintf(fp, "0.678789 0.346182 0 0.203637 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.4834 90.4883 mo\n");
  (void) fprintf(fp, "74.4834 146.316 25.6904 191.574 -34.4971 191.574 cv\n");
  (void) fprintf(fp, "-94.6807 191.574 -143.474 146.316 -143.474 90.4883 cv\n");
  (void) fprintf(fp, "-143.474 34.6602 -94.6807 -10.5977 -34.4971 -10.5977 cv\n");
  (void) fprintf(fp, "25.6904 -10.5977 74.4834 34.6602 74.4834 90.4883 cv\n");
  (void) fprintf(fp, "0.673463 0.343466 0 0.202039 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.4561 90.4883 mo\n");
  (void) fprintf(fp, "74.4561 146.305 25.6748 191.551 -34.4971 191.551 cv\n");
  (void) fprintf(fp, "-94.6689 191.551 -143.446 146.305 -143.446 90.4883 cv\n");
  (void) fprintf(fp, "-143.446 34.6758 -94.6689 -10.5742 -34.4971 -10.5742 cv\n");
  (void) fprintf(fp, "25.6748 -10.5742 74.4561 34.6758 74.4561 90.4883 cv\n");
  (void) fprintf(fp, "0.668153 0.340758 0 0.200446 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.4287 90.4883 mo\n");
  (void) fprintf(fp, "74.4287 146.289 25.6631 191.527 -34.4971 191.527 cv\n");
  (void) fprintf(fp, "-94.6533 191.527 -143.419 146.289 -143.419 90.4883 cv\n");
  (void) fprintf(fp, "-143.419 34.6875 -94.6533 -10.5469 -34.4971 -10.5469 cv\n");
  (void) fprintf(fp, "25.6631 -10.5469 74.4287 34.6875 74.4287 90.4883 cv\n");
  (void) fprintf(fp, "0.662857 0.338057 0 0.198857 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.4014 90.4883 mo\n");
  (void) fprintf(fp, "74.4014 146.277 25.6475 191.5 -34.4971 191.5 cv\n");
  (void) fprintf(fp, "-94.6377 191.5 -143.392 146.277 -143.392 90.4883 cv\n");
  (void) fprintf(fp, "-143.392 34.7031 -94.6377 -10.5195 -34.4971 -10.5195 cv\n");
  (void) fprintf(fp, "25.6475 -10.5195 74.4014 34.7031 74.4014 90.4883 cv\n");
  (void) fprintf(fp, "0.657577 0.335364 0 0.197273 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.374 90.4883 mo\n");
  (void) fprintf(fp, "74.374 146.262 25.6318 191.473 -34.4971 191.473 cv\n");
  (void) fprintf(fp, "-94.6221 191.473 -143.364 146.262 -143.364 90.4883 cv\n");
  (void) fprintf(fp, "-143.364 34.7148 -94.6221 -10.4961 -34.4971 -10.4961 cv\n");
  (void) fprintf(fp, "25.6318 -10.4961 74.374 34.7148 74.374 90.4883 cv\n");
  (void) fprintf(fp, "0.652312 0.332679 0 0.195694 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.3467 90.4883 mo\n");
  (void) fprintf(fp, "74.3467 146.246 25.6162 191.449 -34.4971 191.449 cv\n");
  (void) fprintf(fp, "-94.6064 191.449 -143.337 146.246 -143.337 90.4883 cv\n");
  (void) fprintf(fp, "-143.337 34.7305 -94.6064 -10.4688 -34.4971 -10.4688 cv\n");
  (void) fprintf(fp, "25.6162 -10.4688 74.3467 34.7305 74.3467 90.4883 cv\n");
  (void) fprintf(fp, "0.647062 0.330001 0 0.194119 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.3193 90.4883 mo\n");
  (void) fprintf(fp, "74.3193 146.234 25.6006 191.426 -34.4971 191.426 cv\n");
  (void) fprintf(fp, "-94.5908 191.426 -143.31 146.234 -143.31 90.4883 cv\n");
  (void) fprintf(fp, "-143.31 34.7461 -94.5908 -10.4453 -34.4971 -10.4453 cv\n");
  (void) fprintf(fp, "25.6006 -10.4453 74.3193 34.7461 74.3193 90.4883 cv\n");
  (void) fprintf(fp, "0.641827 0.327332 0 0.192548 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.292 90.4883 mo\n");
  (void) fprintf(fp, "74.292 146.219 25.585 191.398 -34.4971 191.398 cv\n");
  (void) fprintf(fp, "-94.5752 191.398 -143.282 146.219 -143.282 90.4883 cv\n");
  (void) fprintf(fp, "-143.282 34.7578 -94.5752 -10.418 -34.4971 -10.418 cv\n");
  (void) fprintf(fp, "25.585 -10.418 74.292 34.7578 74.292 90.4883 cv\n");
  (void) fprintf(fp, "0.636607 0.32467 0 0.190982 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.2646 90.4883 mo\n");
  (void) fprintf(fp, "74.2646 146.207 25.5693 191.371 -34.4971 191.371 cv\n");
  (void) fprintf(fp, "-94.5596 191.371 -143.255 146.207 -143.255 90.4883 cv\n");
  (void) fprintf(fp, "-143.255 34.7734 -94.5596 -10.3945 -34.4971 -10.3945 cv\n");
  (void) fprintf(fp, "25.5693 -10.3945 74.2646 34.7734 74.2646 90.4883 cv\n");
  (void) fprintf(fp, "0.631403 0.322015 0 0.189421 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.2373 90.4883 mo\n");
  (void) fprintf(fp, "74.2373 146.191 25.5576 191.348 -34.4971 191.348 cv\n");
  (void) fprintf(fp, "-94.5439 191.348 -143.228 146.191 -143.228 90.4883 cv\n");
  (void) fprintf(fp, "-143.228 34.7852 -94.5439 -10.3672 -34.4971 -10.3672 cv\n");
  (void) fprintf(fp, "25.5576 -10.3672 74.2373 34.7852 74.2373 90.4883 cv\n");
  (void) fprintf(fp, "0.626213 0.319369 0 0.187864 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.21 90.4883 mo\n");
  (void) fprintf(fp, "74.21 146.176 25.542 191.32 -34.4971 191.32 cv\n");
  (void) fprintf(fp, "-94.5322 191.32 -143.2 146.176 -143.2 90.4883 cv\n");
  (void) fprintf(fp, "-143.2 34.8008 -94.5322 -10.3438 -34.4971 -10.3438 cv\n");
  (void) fprintf(fp, "25.542 -10.3438 74.21 34.8008 74.21 90.4883 cv\n");
  (void) fprintf(fp, "0.62104 0.31673 0 0.186312 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.1826 90.4883 mo\n");
  (void) fprintf(fp, "74.1826 146.164 25.5264 191.297 -34.4971 191.297 cv\n");
  (void) fprintf(fp, "-94.5166 191.297 -143.173 146.164 -143.173 90.4883 cv\n");
  (void) fprintf(fp, "-143.173 34.8164 -94.5166 -10.3164 -34.4971 -10.3164 cv\n");
  (void) fprintf(fp, "25.5264 -10.3164 74.1826 34.8164 74.1826 90.4883 cv\n");
  (void) fprintf(fp, "0.615881 0.314099 0 0.184764 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.1553 90.4883 mo\n");
  (void) fprintf(fp, "74.1553 146.148 25.5107 191.27 -34.4971 191.27 cv\n");
  (void) fprintf(fp, "-94.501 191.27 -143.146 146.148 -143.146 90.4883 cv\n");
  (void) fprintf(fp, "-143.146 34.8281 -94.501 -10.293 -34.4971 -10.293 cv\n");
  (void) fprintf(fp, "25.5107 -10.293 74.1553 34.8281 74.1553 90.4883 cv\n");
  (void) fprintf(fp, "0.610738 0.311476 0 0.183221 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.1279 90.4883 mo\n");
  (void) fprintf(fp, "74.1279 146.137 25.4951 191.246 -34.4971 191.246 cv\n");
  (void) fprintf(fp, "-94.4854 191.246 -143.118 146.137 -143.118 90.4883 cv\n");
  (void) fprintf(fp, "-143.118 34.8438 -94.4854 -10.2656 -34.4971 -10.2656 cv\n");
  (void) fprintf(fp, "25.4951 -10.2656 74.1279 34.8438 74.1279 90.4883 cv\n");
  (void) fprintf(fp, "0.60561 0.308861 0 0.181683 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.0967 90.4883 mo\n");
  (void) fprintf(fp, "74.0967 146.121 25.4795 191.219 -34.4971 191.219 cv\n");
  (void) fprintf(fp, "-94.4697 191.219 -143.091 146.121 -143.091 90.4883 cv\n");
  (void) fprintf(fp, "-143.091 34.8555 -94.4697 -10.2422 -34.4971 -10.2422 cv\n");
  (void) fprintf(fp, "25.4795 -10.2422 74.0967 34.8555 74.0967 90.4883 cv\n");
  (void) fprintf(fp, "0.600498 0.306254 0 0.180149 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.0732 90.4883 mo\n");
  (void) fprintf(fp, "74.0732 146.105 25.4639 191.195 -34.4971 191.195 cv\n");
  (void) fprintf(fp, "-94.4541 191.195 -143.063 146.105 -143.063 90.4883 cv\n");
  (void) fprintf(fp, "-143.063 34.8711 -94.4541 -10.2148 -34.4971 -10.2148 cv\n");
  (void) fprintf(fp, "25.4639 -10.2148 74.0732 34.8711 74.0732 90.4883 cv\n");
  (void) fprintf(fp, "0.595401 0.303655 0 0.17862 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.042 90.4883 mo\n");
  (void) fprintf(fp, "74.042 146.094 25.4482 191.168 -34.4971 191.168 cv\n");
  (void) fprintf(fp, "-94.4385 191.168 -143.036 146.094 -143.036 90.4883 cv\n");
  (void) fprintf(fp, "-143.036 34.8867 -94.4385 -10.1914 -34.4971 -10.1914 cv\n");
  (void) fprintf(fp, "25.4482 -10.1914 74.042 34.8867 74.042 90.4883 cv\n");
  (void) fprintf(fp, "0.59032 0.301063 0 0.177096 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "74.0186 90.4883 mo\n");
  (void) fprintf(fp, "74.0186 146.078 25.4326 191.145 -34.4971 191.145 cv\n");
  (void) fprintf(fp, "-94.4268 191.145 -143.005 146.078 -143.005 90.4883 cv\n");
  (void) fprintf(fp, "-143.005 34.8984 -94.4268 -10.1641 -34.4971 -10.1641 cv\n");
  (void) fprintf(fp, "25.4326 -10.1641 74.0186 34.8984 74.0186 90.4883 cv\n");
  (void) fprintf(fp, "0.585254 0.29848 0 0.175576 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.9873 90.4883 mo\n");
  (void) fprintf(fp, "73.9873 146.066 25.417 191.117 -34.4971 191.117 cv\n");
  (void) fprintf(fp, "-94.4111 191.117 -142.981 146.066 -142.981 90.4883 cv\n");
  (void) fprintf(fp, "-142.981 34.9141 -94.4111 -10.1406 -34.4971 -10.1406 cv\n");
  (void) fprintf(fp, "25.417 -10.1406 73.9873 34.9141 73.9873 90.4883 cv\n");
  (void) fprintf(fp, "0.580204 0.295904 0 0.174061 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.9639 90.4883 mo\n");
  (void) fprintf(fp, "73.9639 146.051 25.4053 191.094 -34.4971 191.094 cv\n");
  (void) fprintf(fp, "-94.3955 191.094 -142.95 146.051 -142.95 90.4883 cv\n");
  (void) fprintf(fp, "-142.95 34.9258 -94.3955 -10.1133 -34.4971 -10.1133 cv\n");
  (void) fprintf(fp, "25.4053 -10.1133 73.9639 34.9258 73.9639 90.4883 cv\n");
  (void) fprintf(fp, "0.57517 0.293337 0 0.172551 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.9326 90.4883 mo\n");
  (void) fprintf(fp, "73.9326 146.035 25.3896 191.066 -34.4971 191.066 cv\n");
  (void) fprintf(fp, "-94.3799 191.066 -142.927 146.035 -142.927 90.4883 cv\n");
  (void) fprintf(fp, "-142.927 34.9414 -94.3799 -10.0898 -34.4971 -10.0898 cv\n");
  (void) fprintf(fp, "25.3896 -10.0898 73.9326 34.9414 73.9326 90.4883 cv\n");
  (void) fprintf(fp, "0.570151 0.290777 0 0.171045 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.9092 90.4883 mo\n");
  (void) fprintf(fp, "73.9092 146.023 25.374 191.043 -34.4971 191.043 cv\n");
  (void) fprintf(fp, "-94.3643 191.043 -142.896 146.023 -142.896 90.4883 cv\n");
  (void) fprintf(fp, "-142.896 34.957 -94.3643 -10.0625 -34.4971 -10.0625 cv\n");
  (void) fprintf(fp, "25.374 -10.0625 73.9092 34.957 73.9092 90.4883 cv\n");
  (void) fprintf(fp, "0.565148 0.288226 0 0.169544 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.8779 90.4883 mo\n");
  (void) fprintf(fp, "73.8779 146.008 25.3584 191.016 -34.4971 191.016 cv\n");
  (void) fprintf(fp, "-94.3486 191.016 -142.868 146.008 -142.868 90.4883 cv\n");
  (void) fprintf(fp, "-142.868 34.9688 -94.3486 -10.0391 -34.4971 -10.0391 cv\n");
  (void) fprintf(fp, "25.3584 -10.0391 73.8779 34.9688 73.8779 90.4883 cv\n");
  (void) fprintf(fp, "0.560161 0.285682 0 0.168048 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.8545 90.4883 mo\n");
  (void) fprintf(fp, "73.8545 145.996 25.3428 190.992 -34.4971 190.992 cv\n");
  (void) fprintf(fp, "-94.333 190.992 -142.841 145.996 -142.841 90.4883 cv\n");
  (void) fprintf(fp, "-142.841 34.9844 -94.333 -10.0117 -34.4971 -10.0117 cv\n");
  (void) fprintf(fp, "25.3428 -10.0117 73.8545 34.9844 73.8545 90.4883 cv\n");
  (void) fprintf(fp, "0.55519 0.283147 0 0.166557 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.8232 90.4883 mo\n");
  (void) fprintf(fp, "73.8232 145.98 25.3271 190.965 -34.4971 190.965 cv\n");
  (void) fprintf(fp, "-94.3174 190.965 -142.813 145.98 -142.813 90.4883 cv\n");
  (void) fprintf(fp, "-142.813 34.9961 -94.3174 -9.98828 -34.4971 -9.98828 cv\n");
  (void) fprintf(fp, "25.3271 -9.98828 73.8232 34.9961 73.8232 90.4883 cv\n");
  (void) fprintf(fp, "0.550234 0.280619 0 0.16507 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.7959 90.4883 mo\n");
  (void) fprintf(fp, "73.7959 145.965 25.3154 190.941 -34.4971 190.941 cv\n");
  (void) fprintf(fp, "-94.3018 190.941 -142.786 145.965 -142.786 90.4883 cv\n");
  (void) fprintf(fp, "-142.786 35.0117 -94.3018 -9.96094 -34.4971 -9.96094 cv\n");
  (void) fprintf(fp, "25.3154 -9.96094 73.7959 35.0117 73.7959 90.4883 cv\n");
  (void) fprintf(fp, "0.545294 0.2781 0 0.163588 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.7686 90.4883 mo\n");
  (void) fprintf(fp, "73.7686 145.953 25.2998 190.914 -34.4971 190.914 cv\n");
  (void) fprintf(fp, "-94.2861 190.914 -142.759 145.953 -142.759 90.4883 cv\n");
  (void) fprintf(fp, "-142.759 35.0273 -94.2861 -9.9375 -34.4971 -9.9375 cv\n");
  (void) fprintf(fp, "25.2998 -9.9375 73.7686 35.0273 73.7686 90.4883 cv\n");
  (void) fprintf(fp, "0.540371 0.275589 0 0.162111 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.7412 90.4883 mo\n");
  (void) fprintf(fp, "73.7412 145.938 25.2842 190.887 -34.4971 190.887 cv\n");
  (void) fprintf(fp, "-94.2744 190.887 -142.731 145.938 -142.731 90.4883 cv\n");
  (void) fprintf(fp, "-142.731 35.0391 -94.2744 -9.91016 -34.4971 -9.91016 cv\n");
  (void) fprintf(fp, "25.2842 -9.91016 73.7412 35.0391 73.7412 90.4883 cv\n");
  (void) fprintf(fp, "0.535463 0.273086 0 0.160639 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.7139 90.4883 mo\n");
  (void) fprintf(fp, "73.7139 145.926 25.2686 190.863 -34.4971 190.863 cv\n");
  (void) fprintf(fp, "-94.2588 190.863 -142.704 145.926 -142.704 90.4883 cv\n");
  (void) fprintf(fp, "-142.704 35.0547 -94.2588 -9.88672 -34.4971 -9.88672 cv\n");
  (void) fprintf(fp, "25.2686 -9.88672 73.7139 35.0547 73.7139 90.4883 cv\n");
  (void) fprintf(fp, "0.530571 0.270591 0 0.159171 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.6865 90.4883 mo\n");
  (void) fprintf(fp, "73.6865 145.91 25.2529 190.84 -34.4971 190.84 cv\n");
  (void) fprintf(fp, "-94.2432 190.84 -142.677 145.91 -142.677 90.4883 cv\n");
  (void) fprintf(fp, "-142.677 35.0664 -94.2432 -9.85938 -34.4971 -9.85938 cv\n");
  (void) fprintf(fp, "25.2529 -9.85938 73.6865 35.0664 73.6865 90.4883 cv\n");
  (void) fprintf(fp, "0.525695 0.268105 0 0.157709 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.6592 90.4883 mo\n");
  (void) fprintf(fp, "73.6592 145.895 25.2373 190.812 -34.4971 190.812 cv\n");
  (void) fprintf(fp, "-94.2275 190.812 -142.649 145.895 -142.649 90.4883 cv\n");
  (void) fprintf(fp, "-142.649 35.082 -94.2275 -9.83203 -34.4971 -9.83203 cv\n");
  (void) fprintf(fp, "25.2373 -9.83203 73.6592 35.082 73.6592 90.4883 cv\n");
  (void) fprintf(fp, "0.520836 0.265626 0 0.156251 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.6318 90.4883 mo\n");
  (void) fprintf(fp, "73.6318 145.883 25.2217 190.785 -34.4971 190.785 cv\n");
  (void) fprintf(fp, "-94.2119 190.785 -142.622 145.883 -142.622 90.4883 cv\n");
  (void) fprintf(fp, "-142.622 35.0977 -94.2119 -9.80859 -34.4971 -9.80859 cv\n");
  (void) fprintf(fp, "25.2217 -9.80859 73.6318 35.0977 73.6318 90.4883 cv\n");
  (void) fprintf(fp, "0.515992 0.263156 0 0.154798 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.6045 90.4883 mo\n");
  (void) fprintf(fp, "73.6045 145.867 25.2061 190.762 -34.4971 190.762 cv\n");
  (void) fprintf(fp, "-94.1963 190.762 -142.595 145.867 -142.595 90.4883 cv\n");
  (void) fprintf(fp, "-142.595 35.1094 -94.1963 -9.78516 -34.4971 -9.78516 cv\n");
  (void) fprintf(fp, "25.2061 -9.78516 73.6045 35.1094 73.6045 90.4883 cv\n");
  (void) fprintf(fp, "0.511165 0.260694 0 0.15335 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.5771 90.4883 mo\n");
  (void) fprintf(fp, "73.5771 145.855 25.1904 190.738 -34.4971 190.738 cv\n");
  (void) fprintf(fp, "-94.1807 190.738 -142.567 145.855 -142.567 90.4883 cv\n");
  (void) fprintf(fp, "-142.567 35.125 -94.1807 -9.75781 -34.4971 -9.75781 cv\n");
  (void) fprintf(fp, "25.1904 -9.75781 73.5771 35.125 73.5771 90.4883 cv\n");
  (void) fprintf(fp, "0.506354 0.258241 0 0.151906 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.5498 90.4883 mo\n");
  (void) fprintf(fp, "73.5498 145.84 25.1748 190.711 -34.4971 190.711 cv\n");
  (void) fprintf(fp, "-94.1689 190.711 -142.54 145.84 -142.54 90.4883 cv\n");
  (void) fprintf(fp, "-142.54 35.1367 -94.1689 -9.73047 -34.4971 -9.73047 cv\n");
  (void) fprintf(fp, "25.1748 -9.73047 73.5498 35.1367 73.5498 90.4883 cv\n");
  (void) fprintf(fp, "0.501559 0.255795 0 0.150468 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.5225 90.4883 mo\n");
  (void) fprintf(fp, "73.5225 145.824 25.1592 190.684 -34.4971 190.684 cv\n");
  (void) fprintf(fp, "-94.1533 190.684 -142.513 145.824 -142.513 90.4883 cv\n");
  (void) fprintf(fp, "-142.513 35.1523 -94.1533 -9.70703 -34.4971 -9.70703 cv\n");
  (void) fprintf(fp, "25.1592 -9.70703 73.5225 35.1523 73.5225 90.4883 cv\n");
  (void) fprintf(fp, "0.496781 0.253358 0 0.149034 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.4951 90.4883 mo\n");
  (void) fprintf(fp, "73.4951 145.812 25.1475 190.66 -34.4971 190.66 cv\n");
  (void) fprintf(fp, "-94.1377 190.66 -142.485 145.812 -142.485 90.4883 cv\n");
  (void) fprintf(fp, "-142.485 35.168 -94.1377 -9.68359 -34.4971 -9.68359 cv\n");
  (void) fprintf(fp, "25.1475 -9.68359 73.4951 35.168 73.4951 90.4883 cv\n");
  (void) fprintf(fp, "0.492019 0.25093 0 0.147606 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.4678 90.4883 mo\n");
  (void) fprintf(fp, "73.4678 145.797 25.1318 190.637 -34.4971 190.637 cv\n");
  (void) fprintf(fp, "-94.1221 190.637 -142.458 145.797 -142.458 90.4883 cv\n");
  (void) fprintf(fp, "-142.458 35.1797 -94.1221 -9.65625 -34.4971 -9.65625 cv\n");
  (void) fprintf(fp, "25.1318 -9.65625 73.4678 35.1797 73.4678 90.4883 cv\n");
  (void) fprintf(fp, "0.487273 0.248509 0 0.146182 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.4404 90.4883 mo\n");
  (void) fprintf(fp, "73.4404 145.785 25.1162 190.609 -34.4971 190.609 cv\n");
  (void) fprintf(fp, "-94.1064 190.609 -142.431 145.785 -142.431 90.4883 cv\n");
  (void) fprintf(fp, "-142.431 35.1953 -94.1064 -9.62891 -34.4971 -9.62891 cv\n");
  (void) fprintf(fp, "25.1162 -9.62891 73.4404 35.1953 73.4404 90.4883 cv\n");
  (void) fprintf(fp, "0.482544 0.246097 0 0.144763 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.4131 90.4883 mo\n");
  (void) fprintf(fp, "73.4131 145.77 25.1006 190.582 -34.4971 190.582 cv\n");
  (void) fprintf(fp, "-94.0908 190.582 -142.403 145.77 -142.403 90.4883 cv\n");
  (void) fprintf(fp, "-142.403 35.207 -94.0908 -9.60547 -34.4971 -9.60547 cv\n");
  (void) fprintf(fp, "25.1006 -9.60547 73.4131 35.207 73.4131 90.4883 cv\n");
  (void) fprintf(fp, "0.477831 0.243694 0 0.143349 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.3857 90.4883 mo\n");
  (void) fprintf(fp, "73.3857 145.754 25.085 190.559 -34.4971 190.559 cv\n");
  (void) fprintf(fp, "-94.0752 190.559 -142.376 145.754 -142.376 90.4883 cv\n");
  (void) fprintf(fp, "-142.376 35.2227 -94.0752 -9.57812 -34.4971 -9.57812 cv\n");
  (void) fprintf(fp, "25.085 -9.57812 73.3857 35.2227 73.3857 90.4883 cv\n");
  (void) fprintf(fp, "0.473135 0.241299 0 0.14194 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.3584 90.4883 mo\n");
  (void) fprintf(fp, "73.3584 145.742 25.0693 190.535 -34.4971 190.535 cv\n");
  (void) fprintf(fp, "-94.0596 190.535 -142.349 145.742 -142.349 90.4883 cv\n");
  (void) fprintf(fp, "-142.349 35.2383 -94.0596 -9.55469 -34.4971 -9.55469 cv\n");
  (void) fprintf(fp, "25.0693 -9.55469 73.3584 35.2383 73.3584 90.4883 cv\n");
  (void) fprintf(fp, "0.468455 0.238912 0 0.140537 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.3311 90.4883 mo\n");
  (void) fprintf(fp, "73.3311 145.727 25.0576 190.508 -34.4971 190.508 cv\n");
  (void) fprintf(fp, "-94.0439 190.508 -142.321 145.727 -142.321 90.4883 cv\n");
  (void) fprintf(fp, "-142.321 35.25 -94.0439 -9.52734 -34.4971 -9.52734 cv\n");
  (void) fprintf(fp, "25.0576 -9.52734 73.3311 35.25 73.3311 90.4883 cv\n");
  (void) fprintf(fp, "0.463792 0.236534 0 0.139138 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.3037 90.4883 mo\n");
  (void) fprintf(fp, "73.3037 145.715 25.042 190.48 -34.4971 190.48 cv\n");
  (void) fprintf(fp, "-94.0322 190.48 -142.294 145.715 -142.294 90.4883 cv\n");
  (void) fprintf(fp, "-142.294 35.2656 -94.0322 -9.50391 -34.4971 -9.50391 cv\n");
  (void) fprintf(fp, "25.042 -9.50391 73.3037 35.2656 73.3037 90.4883 cv\n");
  (void) fprintf(fp, "0.459146 0.234164 0 0.137744 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.2764 90.4883 mo\n");
  (void) fprintf(fp, "73.2764 145.699 25.0264 190.457 -34.4971 190.457 cv\n");
  (void) fprintf(fp, "-94.0166 190.457 -142.267 145.699 -142.267 90.4883 cv\n");
  (void) fprintf(fp, "-142.267 35.2773 -94.0166 -9.47656 -34.4971 -9.47656 cv\n");
  (void) fprintf(fp, "25.0264 -9.47656 73.2764 35.2773 73.2764 90.4883 cv\n");
  (void) fprintf(fp, "0.454517 0.231803 0 0.136355 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.249 90.4883 mo\n");
  (void) fprintf(fp, "73.249 145.684 25.0107 190.43 -34.4971 190.43 cv\n");
  (void) fprintf(fp, "-94.001 190.43 -142.239 145.684 -142.239 90.4883 cv\n");
  (void) fprintf(fp, "-142.239 35.293 -94.001 -9.45312 -34.4971 -9.45312 cv\n");
  (void) fprintf(fp, "25.0107 -9.45312 73.249 35.293 73.249 90.4883 cv\n");
  (void) fprintf(fp, "0.449904 0.229451 0 0.134971 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.2217 90.4883 mo\n");
  (void) fprintf(fp, "73.2217 145.672 24.9951 190.406 -34.4971 190.406 cv\n");
  (void) fprintf(fp, "-93.9854 190.406 -142.212 145.672 -142.212 90.4883 cv\n");
  (void) fprintf(fp, "-142.212 35.3086 -93.9854 -9.42578 -34.4971 -9.42578 cv\n");
  (void) fprintf(fp, "24.9951 -9.42578 73.2217 35.3086 73.2217 90.4883 cv\n");
  (void) fprintf(fp, "0.445308 0.227107 0 0.133592 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.1943 90.4883 mo\n");
  (void) fprintf(fp, "73.1943 145.656 24.9795 190.379 -34.4971 190.379 cv\n");
  (void) fprintf(fp, "-93.9697 190.379 -142.185 145.656 -142.185 90.4883 cv\n");
  (void) fprintf(fp, "-142.185 35.3203 -93.9697 -9.40234 -34.4971 -9.40234 cv\n");
  (void) fprintf(fp, "24.9795 -9.40234 73.1943 35.3203 73.1943 90.4883 cv\n");
  (void) fprintf(fp, "0.440729 0.224772 0 0.132219 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.167 90.4883 mo\n");
  (void) fprintf(fp, "73.167 145.645 24.9639 190.355 -34.4971 190.355 cv\n");
  (void) fprintf(fp, "-93.9541 190.355 -142.157 145.645 -142.157 90.4883 cv\n");
  (void) fprintf(fp, "-142.157 35.3359 -93.9541 -9.375 -34.4971 -9.375 cv\n");
  (void) fprintf(fp, "24.9639 -9.375 73.167 35.3359 73.167 90.4883 cv\n");
  (void) fprintf(fp, "0.436167 0.222445 0 0.13085 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.1396 90.4883 mo\n");
  (void) fprintf(fp, "73.1396 145.629 24.9482 190.328 -34.4971 190.328 cv\n");
  (void) fprintf(fp, "-93.9385 190.328 -142.13 145.629 -142.13 90.4883 cv\n");
  (void) fprintf(fp, "-142.13 35.3477 -93.9385 -9.35156 -34.4971 -9.35156 cv\n");
  (void) fprintf(fp, "24.9482 -9.35156 73.1396 35.3477 73.1396 90.4883 cv\n");
  (void) fprintf(fp, "0.431621 0.220127 0 0.129486 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.1123 90.4883 mo\n");
  (void) fprintf(fp, "73.1123 145.613 24.9326 190.305 -34.4971 190.305 cv\n");
  (void) fprintf(fp, "-93.9229 190.305 -142.103 145.613 -142.103 90.4883 cv\n");
  (void) fprintf(fp, "-142.103 35.3633 -93.9229 -9.32422 -34.4971 -9.32422 cv\n");
  (void) fprintf(fp, "24.9326 -9.32422 73.1123 35.3633 73.1123 90.4883 cv\n");
  (void) fprintf(fp, "0.427093 0.217818 0 0.128128 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.085 90.4883 mo\n");
  (void) fprintf(fp, "73.085 145.602 24.917 190.277 -34.4971 190.277 cv\n");
  (void) fprintf(fp, "-93.9111 190.277 -142.075 145.602 -142.075 90.4883 cv\n");
  (void) fprintf(fp, "-142.075 35.3789 -93.9111 -9.30078 -34.4971 -9.30078 cv\n");
  (void) fprintf(fp, "24.917 -9.30078 73.085 35.3789 73.085 90.4883 cv\n");
  (void) fprintf(fp, "0.422582 0.215517 0 0.126775 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.0576 90.4883 mo\n");
  (void) fprintf(fp, "73.0576 145.586 24.9053 190.254 -34.4971 190.254 cv\n");
  (void) fprintf(fp, "-93.8955 190.254 -142.048 145.586 -142.048 90.4883 cv\n");
  (void) fprintf(fp, "-142.048 35.3906 -93.8955 -9.27344 -34.4971 -9.27344 cv\n");
  (void) fprintf(fp, "24.9053 -9.27344 73.0576 35.3906 73.0576 90.4883 cv\n");
  (void) fprintf(fp, "0.418088 0.213225 0 0.125427 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.0303 90.4883 mo\n");
  (void) fprintf(fp, "73.0303 145.574 24.8896 190.227 -34.4971 190.227 cv\n");
  (void) fprintf(fp, "-93.8799 190.227 -142.021 145.574 -142.021 90.4883 cv\n");
  (void) fprintf(fp, "-142.021 35.4062 -93.8799 -9.25 -34.4971 -9.25 cv\n");
  (void) fprintf(fp, "24.8896 -9.25 73.0303 35.4062 73.0303 90.4883 cv\n");
  (void) fprintf(fp, "0.413612 0.210942 0 0.124083 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "73.0029 90.4883 mo\n");
  (void) fprintf(fp, "73.0029 145.559 24.874 190.203 -34.4971 190.203 cv\n");
  (void) fprintf(fp, "-93.8643 190.203 -141.993 145.559 -141.993 90.4883 cv\n");
  (void) fprintf(fp, "-141.993 35.418 -93.8643 -9.22266 -34.4971 -9.22266 cv\n");
  (void) fprintf(fp, "24.874 -9.22266 73.0029 35.418 73.0029 90.4883 cv\n");
  (void) fprintf(fp, "0.409152 0.208668 0 0.122746 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.9756 90.4883 mo\n");
  (void) fprintf(fp, "72.9756 145.543 24.8584 190.176 -34.4971 190.176 cv\n");
  (void) fprintf(fp, "-93.8486 190.176 -141.966 145.543 -141.966 90.4883 cv\n");
  (void) fprintf(fp, "-141.966 35.4336 -93.8486 -9.19922 -34.4971 -9.19922 cv\n");
  (void) fprintf(fp, "24.8584 -9.19922 72.9756 35.4336 72.9756 90.4883 cv\n");
  (void) fprintf(fp, "0.40471 0.206402 0 0.121413 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.9482 90.4883 mo\n");
  (void) fprintf(fp, "72.9482 145.531 24.8428 190.152 -34.4971 190.152 cv\n");
  (void) fprintf(fp, "-93.833 190.152 -141.938 145.531 -141.938 90.4883 cv\n");
  (void) fprintf(fp, "-141.938 35.4492 -93.833 -9.17188 -34.4971 -9.17188 cv\n");
  (void) fprintf(fp, "24.8428 -9.17188 72.9482 35.4492 72.9482 90.4883 cv\n");
  (void) fprintf(fp, "0.400285 0.204145 0 0.120086 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.917 90.4883 mo\n");
  (void) fprintf(fp, "72.917 145.516 24.8271 190.125 -34.4971 190.125 cv\n");
  (void) fprintf(fp, "-93.8174 190.125 -141.911 145.516 -141.911 90.4883 cv\n");
  (void) fprintf(fp, "-141.911 35.4609 -93.8174 -9.14844 -34.4971 -9.14844 cv\n");
  (void) fprintf(fp, "24.8271 -9.14844 72.917 35.4609 72.917 90.4883 cv\n");
  (void) fprintf(fp, "0.395878 0.201898 0 0.118763 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.8936 90.4883 mo\n");
  (void) fprintf(fp, "72.8936 145.504 24.8154 190.098 -34.4971 190.098 cv\n");
  (void) fprintf(fp, "-93.8018 190.098 -141.884 145.504 -141.884 90.4883 cv\n");
  (void) fprintf(fp, "-141.884 35.4766 -93.8018 -9.12109 -34.4971 -9.12109 cv\n");
  (void) fprintf(fp, "24.8154 -9.12109 72.8936 35.4766 72.8936 90.4883 cv\n");
  (void) fprintf(fp, "0.391488 0.199659 0 0.117446 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.8623 90.4883 mo\n");
  (void) fprintf(fp, "72.8623 145.488 24.7998 190.074 -34.4971 190.074 cv\n");
  (void) fprintf(fp, "-93.7861 190.074 -141.856 145.488 -141.856 90.4883 cv\n");
  (void) fprintf(fp, "-141.856 35.4883 -93.7861 -9.09766 -34.4971 -9.09766 cv\n");
  (void) fprintf(fp, "24.7998 -9.09766 72.8623 35.4883 72.8623 90.4883 cv\n");
  (void) fprintf(fp, "0.387115 0.197429 0 0.116135 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.8389 90.4883 mo\n");
  (void) fprintf(fp, "72.8389 145.473 24.7842 190.051 -34.4971 190.051 cv\n");
  (void) fprintf(fp, "-93.7744 190.051 -141.825 145.473 -141.825 90.4883 cv\n");
  (void) fprintf(fp, "-141.825 35.5039 -93.7744 -9.07031 -34.4971 -9.07031 cv\n");
  (void) fprintf(fp, "24.7842 -9.07031 72.8389 35.5039 72.8389 90.4883 cv\n");
  (void) fprintf(fp, "0.38276 0.195208 0 0.114828 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.8076 90.4883 mo\n");
  (void) fprintf(fp, "72.8076 145.461 24.7686 190.023 -34.4971 190.023 cv\n");
  (void) fprintf(fp, "-93.7588 190.023 -141.802 145.461 -141.802 90.4883 cv\n");
  (void) fprintf(fp, "-141.802 35.5195 -93.7588 -9.04688 -34.4971 -9.04688 cv\n");
  (void) fprintf(fp, "24.7686 -9.04688 72.8076 35.5195 72.8076 90.4883 cv\n");
  (void) fprintf(fp, "0.378423 0.192996 0 0.113527 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.7842 90.4883 mo\n");
  (void) fprintf(fp, "72.7842 145.445 24.7529 189.996 -34.4971 189.996 cv\n");
  (void) fprintf(fp, "-93.7432 189.996 -141.771 145.445 -141.771 90.4883 cv\n");
  (void) fprintf(fp, "-141.771 35.5312 -93.7432 -9.01953 -34.4971 -9.01953 cv\n");
  (void) fprintf(fp, "24.7529 -9.01953 72.7842 35.5312 72.7842 90.4883 cv\n");
  (void) fprintf(fp, "0.374103 0.190793 0 0.112231 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.7529 90.4883 mo\n");
  (void) fprintf(fp, "72.7529 145.434 24.7373 189.973 -34.4971 189.973 cv\n");
  (void) fprintf(fp, "-93.7275 189.973 -141.747 145.434 -141.747 90.4883 cv\n");
  (void) fprintf(fp, "-141.747 35.5469 -93.7275 -8.99609 -34.4971 -8.99609 cv\n");
  (void) fprintf(fp, "24.7373 -8.99609 72.7529 35.5469 72.7529 90.4883 cv\n");
  (void) fprintf(fp, "0.369802 0.188599 0 0.11094 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.7295 90.4883 mo\n");
  (void) fprintf(fp, "72.7295 145.418 24.7217 189.949 -34.4971 189.949 cv\n");
  (void) fprintf(fp, "-93.7119 189.949 -141.716 145.418 -141.716 90.4883 cv\n");
  (void) fprintf(fp, "-141.716 35.5586 -93.7119 -8.96875 -34.4971 -8.96875 cv\n");
  (void) fprintf(fp, "24.7217 -8.96875 72.7295 35.5586 72.7295 90.4883 cv\n");
  (void) fprintf(fp, "0.365517 0.186414 0 0.109655 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.6982 90.4883 mo\n");
  (void) fprintf(fp, "72.6982 145.402 24.7061 189.922 -34.4971 189.922 cv\n");
  (void) fprintf(fp, "-93.6963 189.922 -141.688 145.402 -141.688 90.4883 cv\n");
  (void) fprintf(fp, "-141.688 35.5742 -93.6963 -8.94141 -34.4971 -8.94141 cv\n");
  (void) fprintf(fp, "24.7061 -8.94141 72.6982 35.5742 72.6982 90.4883 cv\n");
  (void) fprintf(fp, "0.361251 0.184238 0 0.108375 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.6748 90.4883 mo\n");
  (void) fprintf(fp, "72.6748 145.391 24.6904 189.895 -34.4971 189.895 cv\n");
  (void) fprintf(fp, "-93.6807 189.895 -141.661 145.391 -141.661 90.4883 cv\n");
  (void) fprintf(fp, "-141.661 35.5898 -93.6807 -8.91797 -34.4971 -8.91797 cv\n");
  (void) fprintf(fp, "24.6904 -8.91797 72.6748 35.5898 72.6748 90.4883 cv\n");
  (void) fprintf(fp, "0.357003 0.182072 0 0.107101 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.6436 90.4883 mo\n");
  (void) fprintf(fp, "72.6436 145.375 24.6748 189.871 -34.4971 189.871 cv\n");
  (void) fprintf(fp, "-93.6689 189.871 -141.634 145.375 -141.634 90.4883 cv\n");
  (void) fprintf(fp, "-141.634 35.6016 -93.6689 -8.89453 -34.4971 -8.89453 cv\n");
  (void) fprintf(fp, "24.6748 -8.89453 72.6436 35.6016 72.6436 90.4883 cv\n");
  (void) fprintf(fp, "0.352773 0.179914 0 0.105832 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.6162 90.4883 mo\n");
  (void) fprintf(fp, "72.6162 145.363 24.6592 189.848 -34.4971 189.848 cv\n");
  (void) fprintf(fp, "-93.6533 189.848 -141.606 145.363 -141.606 90.4883 cv\n");
  (void) fprintf(fp, "-141.606 35.6172 -93.6533 -8.86719 -34.4971 -8.86719 cv\n");
  (void) fprintf(fp, "24.6592 -8.86719 72.6162 35.6172 72.6162 90.4883 cv\n");
  (void) fprintf(fp, "0.34856 0.177766 0 0.104568 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.5889 90.4883 mo\n");
  (void) fprintf(fp, "72.5889 145.348 24.6475 189.82 -34.4971 189.82 cv\n");
  (void) fprintf(fp, "-93.6377 189.82 -141.579 145.348 -141.579 90.4883 cv\n");
  (void) fprintf(fp, "-141.579 35.6289 -93.6377 -8.83984 -34.4971 -8.83984 cv\n");
  (void) fprintf(fp, "24.6475 -8.83984 72.5889 35.6289 72.5889 90.4883 cv\n");
  (void) fprintf(fp, "0.344366 0.175627 0 0.10331 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.5615 90.4883 mo\n");
  (void) fprintf(fp, "72.5615 145.332 24.6318 189.793 -34.4971 189.793 cv\n");
  (void) fprintf(fp, "-93.6221 189.793 -141.552 145.332 -141.552 90.4883 cv\n");
  (void) fprintf(fp, "-141.552 35.6445 -93.6221 -8.81641 -34.4971 -8.81641 cv\n");
  (void) fprintf(fp, "24.6318 -8.81641 72.5615 35.6445 72.5615 90.4883 cv\n");
  (void) fprintf(fp, "0.34019 0.173497 0 0.102057 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.5342 90.4883 mo\n");
  (void) fprintf(fp, "72.5342 145.32 24.6162 189.77 -34.4971 189.77 cv\n");
  (void) fprintf(fp, "-93.6064 189.77 -141.524 145.32 -141.524 90.4883 cv\n");
  (void) fprintf(fp, "-141.524 35.6602 -93.6064 -8.78906 -34.4971 -8.78906 cv\n");
  (void) fprintf(fp, "24.6162 -8.78906 72.5342 35.6602 72.5342 90.4883 cv\n");
  (void) fprintf(fp, "0.336033 0.171377 0 0.10081 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.5068 90.4883 mo\n");
  (void) fprintf(fp, "72.5068 145.305 24.6006 189.746 -34.4971 189.746 cv\n");
  (void) fprintf(fp, "-93.5908 189.746 -141.497 145.305 -141.497 90.4883 cv\n");
  (void) fprintf(fp, "-141.497 35.6719 -93.5908 -8.76562 -34.4971 -8.76562 cv\n");
  (void) fprintf(fp, "24.6006 -8.76562 72.5068 35.6719 72.5068 90.4883 cv\n");
  (void) fprintf(fp, "0.331893 0.169265 0 0.0995679 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.4795 90.4883 mo\n");
  (void) fprintf(fp, "72.4795 145.293 24.585 189.719 -34.4971 189.719 cv\n");
  (void) fprintf(fp, "-93.5752 189.719 -141.47 145.293 -141.47 90.4883 cv\n");
  (void) fprintf(fp, "-141.47 35.6875 -93.5752 -8.73828 -34.4971 -8.73828 cv\n");
  (void) fprintf(fp, "24.585 -8.73828 72.4795 35.6875 72.4795 90.4883 cv\n");
  (void) fprintf(fp, "0.327772 0.167164 0 0.0983316 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.4521 90.4883 mo\n");
  (void) fprintf(fp, "72.4521 145.277 24.5693 189.691 -34.4971 189.691 cv\n");
  (void) fprintf(fp, "-93.5596 189.691 -141.442 145.277 -141.442 90.4883 cv\n");
  (void) fprintf(fp, "-141.442 35.6992 -93.5596 -8.71484 -34.4971 -8.71484 cv\n");
  (void) fprintf(fp, "24.5693 -8.71484 72.4521 35.6992 72.4521 90.4883 cv\n");
  (void) fprintf(fp, "0.323669 0.165071 0 0.0971007 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.4248 90.4883 mo\n");
  (void) fprintf(fp, "72.4248 145.262 24.5576 189.668 -34.4971 189.668 cv\n");
  (void) fprintf(fp, "-93.5439 189.668 -141.415 145.262 -141.415 90.4883 cv\n");
  (void) fprintf(fp, "-141.415 35.7148 -93.5439 -8.6875 -34.4971 -8.6875 cv\n");
  (void) fprintf(fp, "24.5576 -8.6875 72.4248 35.7148 72.4248 90.4883 cv\n");
  (void) fprintf(fp, "0.319585 0.162988 0 0.0958755 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.3975 90.4883 mo\n");
  (void) fprintf(fp, "72.3975 145.25 24.542 189.641 -34.4971 189.641 cv\n");
  (void) fprintf(fp, "-93.5322 189.641 -141.388 145.25 -141.388 90.4883 cv\n");
  (void) fprintf(fp, "-141.388 35.7305 -93.5322 -8.66406 -34.4971 -8.66406 cv\n");
  (void) fprintf(fp, "24.542 -8.66406 72.3975 35.7305 72.3975 90.4883 cv\n");
  (void) fprintf(fp, "0.315519 0.160915 0 0.0946558 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.3701 90.4883 mo\n");
  (void) fprintf(fp, "72.3701 145.234 24.5264 189.617 -34.4971 189.617 cv\n");
  (void) fprintf(fp, "-93.5166 189.617 -141.36 145.234 -141.36 90.4883 cv\n");
  (void) fprintf(fp, "-141.36 35.7422 -93.5166 -8.63672 -34.4971 -8.63672 cv\n");
  (void) fprintf(fp, "24.5264 -8.63672 72.3701 35.7422 72.3701 90.4883 cv\n");
  (void) fprintf(fp, "0.311472 0.158851 0 0.0934417 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.3428 90.4883 mo\n");
  (void) fprintf(fp, "72.3428 145.223 24.5107 189.59 -34.4971 189.59 cv\n");
  (void) fprintf(fp, "-93.501 189.59 -141.333 145.223 -141.333 90.4883 cv\n");
  (void) fprintf(fp, "-141.333 35.7578 -93.501 -8.61328 -34.4971 -8.61328 cv\n");
  (void) fprintf(fp, "24.5107 -8.61328 72.3428 35.7578 72.3428 90.4883 cv\n");
  (void) fprintf(fp, "0.307444 0.156796 0 0.0922332 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.3154 90.4883 mo\n");
  (void) fprintf(fp, "72.3154 145.207 24.4951 189.566 -34.4971 189.566 cv\n");
  (void) fprintf(fp, "-93.4854 189.566 -141.306 145.207 -141.306 90.4883 cv\n");
  (void) fprintf(fp, "-141.306 35.7695 -93.4854 -8.58594 -34.4971 -8.58594 cv\n");
  (void) fprintf(fp, "24.4951 -8.58594 72.3154 35.7695 72.3154 90.4883 cv\n");
  (void) fprintf(fp, "0.303434 0.154752 0 0.0910304 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.2881 90.4883 mo\n");
  (void) fprintf(fp, "72.2881 145.191 24.4795 189.539 -34.4971 189.539 cv\n");
  (void) fprintf(fp, "-93.4697 189.539 -141.278 145.191 -141.278 90.4883 cv\n");
  (void) fprintf(fp, "-141.278 35.7852 -93.4697 -8.5625 -34.4971 -8.5625 cv\n");
  (void) fprintf(fp, "24.4795 -8.5625 72.2881 35.7852 72.2881 90.4883 cv\n");
  (void) fprintf(fp, "0.299444 0.152716 0 0.0898331 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.2607 90.4883 mo\n");
  (void) fprintf(fp, "72.2607 145.18 24.4639 189.516 -34.4971 189.516 cv\n");
  (void) fprintf(fp, "-93.4541 189.516 -141.251 145.18 -141.251 90.4883 cv\n");
  (void) fprintf(fp, "-141.251 35.8008 -93.4541 -8.53516 -34.4971 -8.53516 cv\n");
  (void) fprintf(fp, "24.4639 -8.53516 72.2607 35.8008 72.2607 90.4883 cv\n");
  (void) fprintf(fp, "0.295472 0.150691 0 0.0886416 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.2334 90.4883 mo\n");
  (void) fprintf(fp, "72.2334 145.164 24.4482 189.488 -34.4971 189.488 cv\n");
  (void) fprintf(fp, "-93.4385 189.488 -141.224 145.164 -141.224 90.4883 cv\n");
  (void) fprintf(fp, "-141.224 35.8125 -93.4385 -8.51172 -34.4971 -8.51172 cv\n");
  (void) fprintf(fp, "24.4482 -8.51172 72.2334 35.8125 72.2334 90.4883 cv\n");
  (void) fprintf(fp, "0.291519 0.148675 0 0.0874557 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.2061 90.4883 mo\n");
  (void) fprintf(fp, "72.2061 145.152 24.4326 189.465 -34.4971 189.465 cv\n");
  (void) fprintf(fp, "-93.4229 189.465 -141.196 145.152 -141.196 90.4883 cv\n");
  (void) fprintf(fp, "-141.196 35.8281 -93.4229 -8.48438 -34.4971 -8.48438 cv\n");
  (void) fprintf(fp, "24.4326 -8.48438 72.2061 35.8281 72.2061 90.4883 cv\n");
  (void) fprintf(fp, "0.287585 0.146668 0 0.0862756 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.1787 90.4883 mo\n");
  (void) fprintf(fp, "72.1787 145.137 24.417 189.438 -34.4971 189.438 cv\n");
  (void) fprintf(fp, "-93.4111 189.438 -141.169 145.137 -141.169 90.4883 cv\n");
  (void) fprintf(fp, "-141.169 35.8398 -93.4111 -8.46094 -34.4971 -8.46094 cv\n");
  (void) fprintf(fp, "24.417 -8.46094 72.1787 35.8398 72.1787 90.4883 cv\n");
  (void) fprintf(fp, "0.283671 0.144672 0 0.0851012 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.1514 90.4883 mo\n");
  (void) fprintf(fp, "72.1514 145.121 24.4053 189.414 -34.4971 189.414 cv\n");
  (void) fprintf(fp, "-93.3955 189.414 -141.142 145.121 -141.142 90.4883 cv\n");
  (void) fprintf(fp, "-141.142 35.8555 -93.3955 -8.43359 -34.4971 -8.43359 cv\n");
  (void) fprintf(fp, "24.4053 -8.43359 72.1514 35.8555 72.1514 90.4883 cv\n");
  (void) fprintf(fp, "0.279775 0.142685 0 0.0839325 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.124 90.4883 mo\n");
  (void) fprintf(fp, "72.124 145.109 24.3896 189.387 -34.4971 189.387 cv\n");
  (void) fprintf(fp, "-93.3799 189.387 -141.114 145.109 -141.114 90.4883 cv\n");
  (void) fprintf(fp, "-141.114 35.8711 -93.3799 -8.41016 -34.4971 -8.41016 cv\n");
  (void) fprintf(fp, "24.3896 -8.41016 72.124 35.8711 72.124 90.4883 cv\n");
  (void) fprintf(fp, "0.275899 0.140708 0 0.0827697 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.0967 90.4883 mo\n");
  (void) fprintf(fp, "72.0967 145.094 24.374 189.363 -34.4971 189.363 cv\n");
  (void) fprintf(fp, "-93.3643 189.363 -141.087 145.094 -141.087 90.4883 cv\n");
  (void) fprintf(fp, "-141.087 35.8828 -93.3643 -8.38281 -34.4971 -8.38281 cv\n");
  (void) fprintf(fp, "24.374 -8.38281 72.0967 35.8828 72.0967 90.4883 cv\n");
  (void) fprintf(fp, "0.272042 0.138741 0 0.0816126 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.0693 90.4883 mo\n");
  (void) fprintf(fp, "72.0693 145.082 24.3584 189.336 -34.4971 189.336 cv\n");
  (void) fprintf(fp, "-93.3486 189.336 -141.06 145.082 -141.06 90.4883 cv\n");
  (void) fprintf(fp, "-141.06 35.8984 -93.3486 -8.35938 -34.4971 -8.35938 cv\n");
  (void) fprintf(fp, "24.3584 -8.35938 72.0693 35.8984 72.0693 90.4883 cv\n");
  (void) fprintf(fp, "0.268205 0.136784 0 0.0804614 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.042 90.4883 mo\n");
  (void) fprintf(fp, "72.042 145.066 24.3428 189.312 -34.4971 189.312 cv\n");
  (void) fprintf(fp, "-93.333 189.312 -141.032 145.066 -141.032 90.4883 cv\n");
  (void) fprintf(fp, "-141.032 35.9102 -93.333 -8.33203 -34.4971 -8.33203 cv\n");
  (void) fprintf(fp, "24.3428 -8.33203 72.042 35.9102 72.042 90.4883 cv\n");
  (void) fprintf(fp, "0.264387 0.134837 0 0.079316 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.0146 90.4883 mo\n");
  (void) fprintf(fp, "72.0146 145.051 24.3271 189.285 -34.4971 189.285 cv\n");
  (void) fprintf(fp, "-93.3174 189.285 -141.005 145.051 -141.005 90.4883 cv\n");
  (void) fprintf(fp, "-141.005 35.9258 -93.3174 -8.30859 -34.4971 -8.30859 cv\n");
  (void) fprintf(fp, "24.3271 -8.30859 72.0146 35.9258 72.0146 90.4883 cv\n");
  (void) fprintf(fp, "0.260589 0.1329 0 0.0781766 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.9873 90.4883 mo\n");
  (void) fprintf(fp, "71.9873 145.039 24.3154 189.262 -34.4971 189.262 cv\n");
  (void) fprintf(fp, "-93.3018 189.262 -140.978 145.039 -140.978 90.4883 cv\n");
  (void) fprintf(fp, "-140.978 35.9414 -93.3018 -8.28125 -34.4971 -8.28125 cv\n");
  (void) fprintf(fp, "24.3154 -8.28125 71.9873 35.9414 71.9873 90.4883 cv\n");
  (void) fprintf(fp, "0.25681 0.130973 0 0.0770429 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.96 90.4883 mo\n");
  (void) fprintf(fp, "71.96 145.023 24.2998 189.234 -34.4971 189.234 cv\n");
  (void) fprintf(fp, "-93.2861 189.234 -140.95 145.023 -140.95 90.4883 cv\n");
  (void) fprintf(fp, "-140.95 35.9531 -93.2861 -8.25781 -34.4971 -8.25781 cv\n");
  (void) fprintf(fp, "24.2998 -8.25781 71.96 35.9531 71.96 90.4883 cv\n");
  (void) fprintf(fp, "0.253051 0.129056 0 0.0759153 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.9326 90.4883 mo\n");
  (void) fprintf(fp, "71.9326 145.012 24.2842 189.207 -34.4971 189.207 cv\n");
  (void) fprintf(fp, "-93.2744 189.207 -140.923 145.012 -140.923 90.4883 cv\n");
  (void) fprintf(fp, "-140.923 35.9688 -93.2744 -8.23047 -34.4971 -8.23047 cv\n");
  (void) fprintf(fp, "24.2842 -8.23047 71.9326 35.9688 71.9326 90.4883 cv\n");
  (void) fprintf(fp, "0.249312 0.127149 0 0.0747936 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.9053 90.4883 mo\n");
  (void) fprintf(fp, "71.9053 144.996 24.2686 189.184 -34.4971 189.184 cv\n");
  (void) fprintf(fp, "-93.2588 189.184 -140.896 144.996 -140.896 90.4883 cv\n");
  (void) fprintf(fp, "-140.896 35.9805 -93.2588 -8.20703 -34.4971 -8.20703 cv\n");
  (void) fprintf(fp, "24.2686 -8.20703 71.9053 35.9805 71.9053 90.4883 cv\n");
  (void) fprintf(fp, "0.245593 0.125252 0 0.0736778 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.8779 90.4883 mo\n");
  (void) fprintf(fp, "71.8779 144.98 24.2529 189.16 -34.4971 189.16 cv\n");
  (void) fprintf(fp, "-93.2432 189.16 -140.868 144.98 -140.868 90.4883 cv\n");
  (void) fprintf(fp, "-140.868 35.9961 -93.2432 -8.17969 -34.4971 -8.17969 cv\n");
  (void) fprintf(fp, "24.2529 -8.17969 71.8779 35.9961 71.8779 90.4883 cv\n");
  (void) fprintf(fp, "0.241894 0.123366 0 0.0725681 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.8506 90.4883 mo\n");
  (void) fprintf(fp, "71.8506 144.969 24.2373 189.133 -34.4971 189.133 cv\n");
  (void) fprintf(fp, "-93.2275 189.133 -140.841 144.969 -140.841 90.4883 cv\n");
  (void) fprintf(fp, "-140.841 36.0117 -93.2275 -8.15234 -34.4971 -8.15234 cv\n");
  (void) fprintf(fp, "24.2373 -8.15234 71.8506 36.0117 71.8506 90.4883 cv\n");
  (void) fprintf(fp, "0.238215 0.121489 0 0.0714644 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.8232 90.4883 mo\n");
  (void) fprintf(fp, "71.8232 144.957 24.2217 189.105 -34.4971 189.105 cv\n");
  (void) fprintf(fp, "-93.2119 189.105 -140.813 144.957 -140.813 90.4883 cv\n");
  (void) fprintf(fp, "-140.813 36.0234 -93.2119 -8.12891 -34.4971 -8.12891 cv\n");
  (void) fprintf(fp, "24.2217 -8.12891 71.8232 36.0234 71.8232 90.4883 cv\n");
  (void) fprintf(fp, "0.234556 0.119623 0 0.0703667 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.792 90.4883 mo\n");
  (void) fprintf(fp, "71.792 144.941 24.2061 189.082 -34.4971 189.082 cv\n");
  (void) fprintf(fp, "-93.1963 189.082 -140.786 144.941 -140.786 90.4883 cv\n");
  (void) fprintf(fp, "-140.786 36.0391 -93.1963 -8.10547 -34.4971 -8.10547 cv\n");
  (void) fprintf(fp, "24.2061 -8.10547 71.792 36.0391 71.792 90.4883 cv\n");
  (void) fprintf(fp, "0.230917 0.117768 0 0.0692752 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.7686 90.4883 mo\n");
  (void) fprintf(fp, "71.7686 144.926 24.1904 189.059 -34.4971 189.059 cv\n");
  (void) fprintf(fp, "-93.1807 189.059 -140.759 144.926 -140.759 90.4883 cv\n");
  (void) fprintf(fp, "-140.759 36.0508 -93.1807 -8.07812 -34.4971 -8.07812 cv\n");
  (void) fprintf(fp, "24.1904 -8.07812 71.7686 36.0508 71.7686 90.4883 cv\n");
  (void) fprintf(fp, "0.227299 0.115923 0 0.0681897 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.7373 90.4883 mo\n");
  (void) fprintf(fp, "71.7373 144.91 24.1748 189.031 -34.4971 189.031 cv\n");
  (void) fprintf(fp, "-93.1689 189.031 -140.731 144.91 -140.731 90.4883 cv\n");
  (void) fprintf(fp, "-140.731 36.0664 -93.1689 -8.05078 -34.4971 -8.05078 cv\n");
  (void) fprintf(fp, "24.1748 -8.05078 71.7373 36.0664 71.7373 90.4883 cv\n");
  (void) fprintf(fp, "0.223701 0.114088 0 0.0671104 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.7139 90.4883 mo\n");
  (void) fprintf(fp, "71.7139 144.898 24.1592 189.004 -34.4971 189.004 cv\n");
  (void) fprintf(fp, "-93.1533 189.004 -140.7 144.898 -140.7 90.4883 cv\n");
  (void) fprintf(fp, "-140.7 36.082 -93.1533 -8.02734 -34.4971 -8.02734 cv\n");
  (void) fprintf(fp, "24.1592 -8.02734 71.7139 36.082 71.7139 90.4883 cv\n");
  (void) fprintf(fp, "0.220124 0.112263 0 0.0660372 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.6826 90.4883 mo\n");
  (void) fprintf(fp, "71.6826 144.887 24.1475 188.98 -34.4971 188.98 cv\n");
  (void) fprintf(fp, "-93.1377 188.98 -140.677 144.887 -140.677 90.4883 cv\n");
  (void) fprintf(fp, "-140.677 36.0938 -93.1377 -8.00391 -34.4971 -8.00391 cv\n");
  (void) fprintf(fp, "24.1475 -8.00391 71.6826 36.0938 71.6826 90.4883 cv\n");
  (void) fprintf(fp, "0.216568 0.110449 0 0.0649703 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.6592 90.4883 mo\n");
  (void) fprintf(fp, "71.6592 144.871 24.1318 188.957 -34.4971 188.957 cv\n");
  (void) fprintf(fp, "-93.1221 188.957 -140.646 144.871 -140.646 90.4883 cv\n");
  (void) fprintf(fp, "-140.646 36.1094 -93.1221 -7.97656 -34.4971 -7.97656 cv\n");
  (void) fprintf(fp, "24.1318 -7.97656 71.6592 36.1094 71.6592 90.4883 cv\n");
  (void) fprintf(fp, "0.213032 0.108646 0 0.0639095 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.6279 90.4883 mo\n");
  (void) fprintf(fp, "71.6279 144.855 24.1162 188.93 -34.4971 188.93 cv\n");
  (void) fprintf(fp, "-93.1064 188.93 -140.622 144.855 -140.622 90.4883 cv\n");
  (void) fprintf(fp, "-140.622 36.1211 -93.1064 -7.94922 -34.4971 -7.94922 cv\n");
  (void) fprintf(fp, "24.1162 -7.94922 71.6279 36.1211 71.6279 90.4883 cv\n");
  (void) fprintf(fp, "0.209517 0.106854 0 0.062855 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.6045 90.4883 mo\n");
  (void) fprintf(fp, "71.6045 144.84 24.1006 188.902 -34.4971 188.902 cv\n");
  (void) fprintf(fp, "-93.0908 188.902 -140.591 144.84 -140.591 90.4883 cv\n");
  (void) fprintf(fp, "-140.591 36.1367 -93.0908 -7.92578 -34.4971 -7.92578 cv\n");
  (void) fprintf(fp, "24.1006 -7.92578 71.6045 36.1367 71.6045 90.4883 cv\n");
  (void) fprintf(fp, "0.206023 0.105072 0 0.0618068 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.5732 90.4883 mo\n");
  (void) fprintf(fp, "71.5732 144.828 24.085 188.879 -34.4971 188.879 cv\n");
  (void) fprintf(fp, "-93.0752 188.879 -140.563 144.828 -140.563 90.4883 cv\n");
  (void) fprintf(fp, "-140.563 36.1523 -93.0752 -7.89844 -34.4971 -7.89844 cv\n");
  (void) fprintf(fp, "24.085 -7.89844 71.5732 36.1523 71.5732 90.4883 cv\n");
  (void) fprintf(fp, "0.20255 0.1033 0 0.0607649 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.5498 90.4883 mo\n");
  (void) fprintf(fp, "71.5498 144.816 24.0693 188.852 -34.4971 188.852 cv\n");
  (void) fprintf(fp, "-93.0596 188.852 -140.536 144.816 -140.536 90.4883 cv\n");
  (void) fprintf(fp, "-140.536 36.1641 -93.0596 -7.875 -34.4971 -7.875 cv\n");
  (void) fprintf(fp, "24.0693 -7.875 71.5498 36.1641 71.5498 90.4883 cv\n");
  (void) fprintf(fp, "0.199098 0.10154 0 0.0597293 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.5186 90.4883 mo\n");
  (void) fprintf(fp, "71.5186 144.801 24.0576 188.828 -34.4971 188.828 cv\n");
  (void) fprintf(fp, "-93.0439 188.828 -140.509 144.801 -140.509 90.4883 cv\n");
  (void) fprintf(fp, "-140.509 36.1797 -93.0439 -7.84766 -34.4971 -7.84766 cv\n");
  (void) fprintf(fp, "24.0576 -7.84766 71.5186 36.1797 71.5186 90.4883 cv\n");
  (void) fprintf(fp, "0.195667 0.0997903 0 0.0587001 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.4912 90.4883 mo\n");
  (void) fprintf(fp, "71.4912 144.785 24.042 188.801 -34.4971 188.801 cv\n");
  (void) fprintf(fp, "-93.0283 188.801 -140.481 144.785 -140.481 90.4883 cv\n");
  (void) fprintf(fp, "-140.481 36.1914 -93.0283 -7.82422 -34.4971 -7.82422 cv\n");
  (void) fprintf(fp, "24.042 -7.82422 71.4912 36.1914 71.4912 90.4883 cv\n");
  (void) fprintf(fp, "0.192258 0.0980515 0 0.0576774 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.4639 90.4883 mo\n");
  (void) fprintf(fp, "71.4639 144.77 24.0264 188.777 -34.4971 188.777 cv\n");
  (void) fprintf(fp, "-93.0166 188.777 -140.454 144.77 -140.454 90.4883 cv\n");
  (void) fprintf(fp, "-140.454 36.207 -93.0166 -7.79688 -34.4971 -7.79688 cv\n");
  (void) fprintf(fp, "24.0264 -7.79688 71.4639 36.207 71.4639 90.4883 cv\n");
  (void) fprintf(fp, "0.18887 0.0963237 0 0.056661 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.4365 90.4883 mo\n");
  (void) fprintf(fp, "71.4365 144.758 24.0107 188.75 -34.4971 188.75 cv\n");
  (void) fprintf(fp, "-93.001 188.75 -140.427 144.758 -140.427 90.4883 cv\n");
  (void) fprintf(fp, "-140.427 36.2227 -93.001 -7.77344 -34.4971 -7.77344 cv\n");
  (void) fprintf(fp, "24.0107 -7.77344 71.4365 36.2227 71.4365 90.4883 cv\n");
  (void) fprintf(fp, "0.185504 0.0946069 0 0.0556511 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.4092 90.4883 mo\n");
  (void) fprintf(fp, "71.4092 144.746 23.9951 188.727 -34.4971 188.727 cv\n");
  (void) fprintf(fp, "-92.9854 188.727 -140.399 144.746 -140.399 90.4883 cv\n");
  (void) fprintf(fp, "-140.399 36.2344 -92.9854 -7.74609 -34.4971 -7.74609 cv\n");
  (void) fprintf(fp, "23.9951 -7.74609 71.4092 36.2344 71.4092 90.4883 cv\n");
  (void) fprintf(fp, "0.182159 0.0929012 0 0.0546478 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.3818 90.4883 mo\n");
  (void) fprintf(fp, "71.3818 144.73 23.9795 188.699 -34.4971 188.699 cv\n");
  (void) fprintf(fp, "-92.9697 188.699 -140.372 144.73 -140.372 90.4883 cv\n");
  (void) fprintf(fp, "-140.372 36.25 -92.9697 -7.72266 -34.4971 -7.72266 cv\n");
  (void) fprintf(fp, "23.9795 -7.72266 71.3818 36.25 71.3818 90.4883 cv\n");
  (void) fprintf(fp, "0.178836 0.0912066 0 0.0536509 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.3545 90.4883 mo\n");
  (void) fprintf(fp, "71.3545 144.715 23.9639 188.676 -34.4971 188.676 cv\n");
  (void) fprintf(fp, "-92.9541 188.676 -140.345 144.715 -140.345 90.4883 cv\n");
  (void) fprintf(fp, "-140.345 36.2617 -92.9541 -7.69531 -34.4971 -7.69531 cv\n");
  (void) fprintf(fp, "23.9639 -7.69531 71.3545 36.2617 71.3545 90.4883 cv\n");
  (void) fprintf(fp, "0.175536 0.0895231 0 0.0526607 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.3271 90.4883 mo\n");
  (void) fprintf(fp, "71.3271 144.699 23.9482 188.648 -34.4971 188.648 cv\n");
  (void) fprintf(fp, "-92.9385 188.648 -140.317 144.699 -140.317 90.4883 cv\n");
  (void) fprintf(fp, "-140.317 36.2773 -92.9385 -7.67188 -34.4971 -7.67188 cv\n");
  (void) fprintf(fp, "23.9482 -7.67188 71.3271 36.2773 71.3271 90.4883 cv\n");
  (void) fprintf(fp, "0.172257 0.0878509 0 0.051677 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.2998 90.4883 mo\n");
  (void) fprintf(fp, "71.2998 144.688 23.9326 188.625 -34.4971 188.625 cv\n");
  (void) fprintf(fp, "-92.9229 188.625 -140.29 144.688 -140.29 90.4883 cv\n");
  (void) fprintf(fp, "-140.29 36.293 -92.9229 -7.64453 -34.4971 -7.64453 cv\n");
  (void) fprintf(fp, "23.9326 -7.64453 71.2998 36.293 71.2998 90.4883 cv\n");
  (void) fprintf(fp, "0.169 0.08619 0 0.0507 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.2725 90.4883 mo\n");
  (void) fprintf(fp, "71.2725 144.676 23.917 188.598 -34.4971 188.598 cv\n");
  (void) fprintf(fp, "-92.9111 188.598 -140.263 144.676 -140.263 90.4883 cv\n");
  (void) fprintf(fp, "-140.263 36.3047 -92.9111 -7.62109 -34.4971 -7.62109 cv\n");
  (void) fprintf(fp, "23.917 -7.62109 71.2725 36.3047 71.2725 90.4883 cv\n");
  (void) fprintf(fp, "0.165766 0.0845404 0 0.0497296 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.2451 90.4883 mo\n");
  (void) fprintf(fp, "71.2451 144.66 23.9053 188.574 -34.4971 188.574 cv\n");
  (void) fprintf(fp, "-92.8955 188.574 -140.235 144.66 -140.235 90.4883 cv\n");
  (void) fprintf(fp, "-140.235 36.3203 -92.8955 -7.59375 -34.4971 -7.59375 cv\n");
  (void) fprintf(fp, "23.9053 -7.59375 71.2451 36.3203 71.2451 90.4883 cv\n");
  (void) fprintf(fp, "0.162553 0.0829023 0 0.0487661 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.2178 90.4883 mo\n");
  (void) fprintf(fp, "71.2178 144.645 23.8896 188.547 -34.4971 188.547 cv\n");
  (void) fprintf(fp, "-92.8799 188.547 -140.208 144.645 -140.208 90.4883 cv\n");
  (void) fprintf(fp, "-140.208 36.332 -92.8799 -7.57031 -34.4971 -7.57031 cv\n");
  (void) fprintf(fp, "23.8896 -7.57031 71.2178 36.332 71.2178 90.4883 cv\n");
  (void) fprintf(fp, "0.159364 0.0812756 0 0.0478092 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.1904 90.4883 mo\n");
  (void) fprintf(fp, "71.1904 144.629 23.874 188.523 -34.4971 188.523 cv\n");
  (void) fprintf(fp, "-92.8643 188.523 -140.181 144.629 -140.181 90.4883 cv\n");
  (void) fprintf(fp, "-140.181 36.3477 -92.8643 -7.54297 -34.4971 -7.54297 cv\n");
  (void) fprintf(fp, "23.874 -7.54297 71.1904 36.3477 71.1904 90.4883 cv\n");
  (void) fprintf(fp, "0.156197 0.0796605 0 0.0468591 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.1631 90.4883 mo\n");
  (void) fprintf(fp, "71.1631 144.617 23.8584 188.496 -34.4971 188.496 cv\n");
  (void) fprintf(fp, "-92.8486 188.496 -140.153 144.617 -140.153 90.4883 cv\n");
  (void) fprintf(fp, "-140.153 36.3633 -92.8486 -7.51953 -34.4971 -7.51953 cv\n");
  (void) fprintf(fp, "23.8584 -7.51953 71.1631 36.3633 71.1631 90.4883 cv\n");
  (void) fprintf(fp, "0.153053 0.0780569 0 0.0459158 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.1357 90.4883 mo\n");
  (void) fprintf(fp, "71.1357 144.605 23.8428 188.473 -34.4971 188.473 cv\n");
  (void) fprintf(fp, "-92.833 188.473 -140.126 144.605 -140.126 90.4883 cv\n");
  (void) fprintf(fp, "-140.126 36.375 -92.833 -7.49219 -34.4971 -7.49219 cv\n");
  (void) fprintf(fp, "23.8428 -7.49219 71.1357 36.375 71.1357 90.4883 cv\n");
  (void) fprintf(fp, "0.149932 0.0764651 0 0.0449795 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.1084 90.4883 mo\n");
  (void) fprintf(fp, "71.1084 144.59 23.8271 188.445 -34.4971 188.445 cv\n");
  (void) fprintf(fp, "-92.8174 188.445 -140.099 144.59 -140.099 90.4883 cv\n");
  (void) fprintf(fp, "-140.099 36.3906 -92.8174 -7.46875 -34.4971 -7.46875 cv\n");
  (void) fprintf(fp, "23.8271 -7.46875 71.1084 36.3906 71.1084 90.4883 cv\n");
  (void) fprintf(fp, "0.146833 0.074885 0 0.04405 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.0811 90.4883 mo\n");
  (void) fprintf(fp, "71.0811 144.574 23.8115 188.418 -34.4971 188.418 cv\n");
  (void) fprintf(fp, "-92.8018 188.418 -140.071 144.574 -140.071 90.4883 cv\n");
  (void) fprintf(fp, "-140.071 36.4023 -92.8018 -7.44141 -34.4971 -7.44141 cv\n");
  (void) fprintf(fp, "23.8115 -7.44141 71.0811 36.4023 71.0811 90.4883 cv\n");
  (void) fprintf(fp, "0.143758 0.0733168 0 0.0431275 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.0537 90.4883 mo\n");
  (void) fprintf(fp, "71.0537 144.562 23.7998 188.395 -34.4971 188.395 cv\n");
  (void) fprintf(fp, "-92.7861 188.395 -140.044 144.562 -140.044 90.4883 cv\n");
  (void) fprintf(fp, "-140.044 36.418 -92.7861 -7.41797 -34.4971 -7.41797 cv\n");
  (void) fprintf(fp, "23.7998 -7.41797 71.0537 36.418 71.0537 90.4883 cv\n");
  (void) fprintf(fp, "0.140707 0.0717604 0 0.042212 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "71.0264 90.4883 mo\n");
  (void) fprintf(fp, "71.0264 144.547 23.7842 188.371 -34.4971 188.371 cv\n");
  (void) fprintf(fp, "-92.7744 188.371 -140.017 144.547 -140.017 90.4883 cv\n");
  (void) fprintf(fp, "-140.017 36.4336 -92.7744 -7.39062 -34.4971 -7.39062 cv\n");
  (void) fprintf(fp, "23.7842 -7.39062 71.0264 36.4336 71.0264 90.4883 cv\n");
  (void) fprintf(fp, "0.137678 0.0702159 0 0.0413035 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.999 90.4883 mo\n");
  (void) fprintf(fp, "70.999 144.535 23.7686 188.344 -34.4971 188.344 cv\n");
  (void) fprintf(fp, "-92.7588 188.344 -139.989 144.535 -139.989 90.4883 cv\n");
  (void) fprintf(fp, "-139.989 36.4453 -92.7588 -7.36719 -34.4971 -7.36719 cv\n");
  (void) fprintf(fp, "23.7686 -7.36719 70.999 36.4453 70.999 90.4883 cv\n");
  (void) fprintf(fp, "0.134674 0.0686836 0 0.0404021 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.9717 90.4883 mo\n");
  (void) fprintf(fp, "70.9717 144.52 23.7529 188.316 -34.4971 188.316 cv\n");
  (void) fprintf(fp, "-92.7432 188.316 -139.962 144.52 -139.962 90.4883 cv\n");
  (void) fprintf(fp, "-139.962 36.4609 -92.7432 -7.33984 -34.4971 -7.33984 cv\n");
  (void) fprintf(fp, "23.7529 -7.33984 70.9717 36.4609 70.9717 90.4883 cv\n");
  (void) fprintf(fp, "0.131693 0.0671633 0 0.0395079 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.9443 90.4883 mo\n");
  (void) fprintf(fp, "70.9443 144.504 23.7373 188.293 -34.4971 188.293 cv\n");
  (void) fprintf(fp, "-92.7275 188.293 -139.935 144.504 -139.935 90.4883 cv\n");
  (void) fprintf(fp, "-139.935 36.4727 -92.7275 -7.31641 -34.4971 -7.31641 cv\n");
  (void) fprintf(fp, "23.7373 -7.31641 70.9443 36.4727 70.9443 90.4883 cv\n");
  (void) fprintf(fp, "0.128736 0.0656552 0 0.0386208 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.917 90.4883 mo\n");
  (void) fprintf(fp, "70.917 144.492 23.7217 188.27 -34.4971 188.27 cv\n");
  (void) fprintf(fp, "-92.7119 188.27 -139.907 144.492 -139.907 90.4883 cv\n");
  (void) fprintf(fp, "-139.907 36.4883 -92.7119 -7.28906 -34.4971 -7.28906 cv\n");
  (void) fprintf(fp, "23.7217 -7.28906 70.917 36.4883 70.917 90.4883 cv\n");
  (void) fprintf(fp, "0.125803 0.0641595 0 0.0377409 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.8896 90.4883 mo\n");
  (void) fprintf(fp, "70.8896 144.477 23.7061 188.242 -34.4971 188.242 cv\n");
  (void) fprintf(fp, "-92.6963 188.242 -139.88 144.477 -139.88 90.4883 cv\n");
  (void) fprintf(fp, "-139.88 36.5039 -92.6963 -7.26172 -34.4971 -7.26172 cv\n");
  (void) fprintf(fp, "23.7061 -7.26172 70.8896 36.5039 70.8896 90.4883 cv\n");
  (void) fprintf(fp, "0.122894 0.062676 0 0.0368683 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.8623 90.4883 mo\n");
  (void) fprintf(fp, "70.8623 144.465 23.6904 188.215 -34.4971 188.215 cv\n");
  (void) fprintf(fp, "-92.6807 188.215 -139.853 144.465 -139.853 90.4883 cv\n");
  (void) fprintf(fp, "-139.853 36.5156 -92.6807 -7.23828 -34.4971 -7.23828 cv\n");
  (void) fprintf(fp, "23.6904 -7.23828 70.8623 36.5156 70.8623 90.4883 cv\n");
  (void) fprintf(fp, "0.12001 0.061205 0 0.0360029 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.835 90.4883 mo\n");
  (void) fprintf(fp, "70.835 144.449 23.6748 188.191 -34.4971 188.191 cv\n");
  (void) fprintf(fp, "-92.6689 188.191 -139.825 144.449 -139.825 90.4883 cv\n");
  (void) fprintf(fp, "-139.825 36.5312 -92.6689 -7.21484 -34.4971 -7.21484 cv\n");
  (void) fprintf(fp, "23.6748 -7.21484 70.835 36.5312 70.835 90.4883 cv\n");
  (void) fprintf(fp, "0.11715 0.0597465 0 0.035145 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.8076 90.4883 mo\n");
  (void) fprintf(fp, "70.8076 144.434 23.6592 188.168 -34.4971 188.168 cv\n");
  (void) fprintf(fp, "-92.6533 188.168 -139.798 144.434 -139.798 90.4883 cv\n");
  (void) fprintf(fp, "-139.798 36.543 -92.6533 -7.1875 -34.4971 -7.1875 cv\n");
  (void) fprintf(fp, "23.6592 -7.1875 70.8076 36.543 70.8076 90.4883 cv\n");
  (void) fprintf(fp, "0.114315 0.0583006 0 0.0342945 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.7803 90.4883 mo\n");
  (void) fprintf(fp, "70.7803 144.422 23.6475 188.141 -34.4971 188.141 cv\n");
  (void) fprintf(fp, "-92.6377 188.141 -139.771 144.422 -139.771 90.4883 cv\n");
  (void) fprintf(fp, "-139.771 36.5586 -92.6377 -7.16016 -34.4971 -7.16016 cv\n");
  (void) fprintf(fp, "23.6475 -7.16016 70.7803 36.5586 70.7803 90.4883 cv\n");
  (void) fprintf(fp, "0.111505 0.0568674 0 0.0334514 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.7529 90.4883 mo\n");
  (void) fprintf(fp, "70.7529 144.406 23.6318 188.113 -34.4971 188.113 cv\n");
  (void) fprintf(fp, "-92.6221 188.113 -139.743 144.406 -139.743 90.4883 cv\n");
  (void) fprintf(fp, "-139.743 36.5742 -92.6221 -7.13672 -34.4971 -7.13672 cv\n");
  (void) fprintf(fp, "23.6318 -7.13672 70.7529 36.5742 70.7529 90.4883 cv\n");
  (void) fprintf(fp, "0.10872 0.055447 0 0.0326159 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.7256 90.4883 mo\n");
  (void) fprintf(fp, "70.7256 144.395 23.6162 188.09 -34.4971 188.09 cv\n");
  (void) fprintf(fp, "-92.6064 188.09 -139.716 144.395 -139.716 90.4883 cv\n");
  (void) fprintf(fp, "-139.716 36.5859 -92.6064 -7.10938 -34.4971 -7.10938 cv\n");
  (void) fprintf(fp, "23.6162 -7.10938 70.7256 36.5859 70.7256 90.4883 cv\n");
  (void) fprintf(fp, "0.10596 0.0540395 0 0.0317879 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.6982 90.4883 mo\n");
  (void) fprintf(fp, "70.6982 144.379 23.6006 188.066 -34.4971 188.066 cv\n");
  (void) fprintf(fp, "-92.5908 188.066 -139.688 144.379 -139.688 90.4883 cv\n");
  (void) fprintf(fp, "-139.688 36.6016 -92.5908 -7.08594 -34.4971 -7.08594 cv\n");
  (void) fprintf(fp, "23.6006 -7.08594 70.6982 36.6016 70.6982 90.4883 cv\n");
  (void) fprintf(fp, "0.103225 0.052645 0 0.0309677 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.6709 90.4883 mo\n");
  (void) fprintf(fp, "70.6709 144.363 23.585 188.039 -34.4971 188.039 cv\n");
  (void) fprintf(fp, "-92.5752 188.039 -139.661 144.363 -139.661 90.4883 cv\n");
  (void) fprintf(fp, "-139.661 36.6133 -92.5752 -7.05859 -34.4971 -7.05859 cv\n");
  (void) fprintf(fp, "23.585 -7.05859 70.6709 36.6133 70.6709 90.4883 cv\n");
  (void) fprintf(fp, "0.100517 0.0512635 0 0.030155 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.6436 90.4883 mo\n");
  (void) fprintf(fp, "70.6436 144.352 23.5693 188.012 -34.4971 188.012 cv\n");
  (void) fprintf(fp, "-92.5596 188.012 -139.634 144.352 -139.634 90.4883 cv\n");
  (void) fprintf(fp, "-139.634 36.6289 -92.5596 -7.03516 -34.4971 -7.03516 cv\n");
  (void) fprintf(fp, "23.5693 -7.03516 70.6436 36.6289 70.6436 90.4883 cv\n");
  (void) fprintf(fp, "0.0978337 0.0498952 0 0.0293501 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.6123 90.4883 mo\n");
  (void) fprintf(fp, "70.6123 144.336 23.5576 187.988 -34.4971 187.988 cv\n");
  (void) fprintf(fp, "-92.5439 187.988 -139.606 144.336 -139.606 90.4883 cv\n");
  (void) fprintf(fp, "-139.606 36.6445 -92.5439 -7.00781 -34.4971 -7.00781 cv\n");
  (void) fprintf(fp, "23.5576 -7.00781 70.6123 36.6445 70.6123 90.4883 cv\n");
  (void) fprintf(fp, "0.0951768 0.0485402 0 0.028553 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.5889 90.4883 mo\n");
  (void) fprintf(fp, "70.5889 144.324 23.542 187.961 -34.4971 187.961 cv\n");
  (void) fprintf(fp, "-92.5283 187.961 -139.579 144.324 -139.579 90.4883 cv\n");
  (void) fprintf(fp, "-139.579 36.6562 -92.5283 -6.98438 -34.4971 -6.98438 cv\n");
  (void) fprintf(fp, "23.542 -6.98438 70.5889 36.6562 70.5889 90.4883 cv\n");
  (void) fprintf(fp, "0.092546 0.0471985 0 0.0277638 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.5576 90.4883 mo\n");
  (void) fprintf(fp, "70.5576 144.309 23.5264 187.938 -34.4971 187.938 cv\n");
  (void) fprintf(fp, "-92.5166 187.938 -139.552 144.309 -139.552 90.4883 cv\n");
  (void) fprintf(fp, "-139.552 36.6719 -92.5166 -6.95703 -34.4971 -6.95703 cv\n");
  (void) fprintf(fp, "23.5264 -6.95703 70.5576 36.6719 70.5576 90.4883 cv\n");
  (void) fprintf(fp, "0.0899418 0.0458703 0 0.0269825 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.5342 90.4883 mo\n");
  (void) fprintf(fp, "70.5342 144.293 23.5107 187.91 -34.4971 187.91 cv\n");
  (void) fprintf(fp, "-92.501 187.91 -139.521 144.293 -139.521 90.4883 cv\n");
  (void) fprintf(fp, "-139.521 36.6836 -92.501 -6.93359 -34.4971 -6.93359 cv\n");
  (void) fprintf(fp, "23.5107 -6.93359 70.5342 36.6836 70.5342 90.4883 cv\n");
  (void) fprintf(fp, "0.0873642 0.0445557 0 0.0262092 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.5029 90.4883 mo\n");
  (void) fprintf(fp, "70.5029 144.281 23.4951 187.887 -34.4971 187.887 cv\n");
  (void) fprintf(fp, "-92.4854 187.887 -139.497 144.281 -139.497 90.4883 cv\n");
  (void) fprintf(fp, "-139.497 36.6992 -92.4854 -6.90625 -34.4971 -6.90625 cv\n");
  (void) fprintf(fp, "23.4951 -6.90625 70.5029 36.6992 70.5029 90.4883 cv\n");
  (void) fprintf(fp, "0.0848135 0.0432549 0 0.025444 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.4795 90.4883 mo\n");
  (void) fprintf(fp, "70.4795 144.266 23.4795 187.859 -34.4971 187.859 cv\n");
  (void) fprintf(fp, "-92.4697 187.859 -139.466 144.266 -139.466 90.4883 cv\n");
  (void) fprintf(fp, "-139.466 36.7148 -92.4697 -6.88281 -34.4971 -6.88281 cv\n");
  (void) fprintf(fp, "23.4795 -6.88281 70.4795 36.7148 70.4795 90.4883 cv\n");
  (void) fprintf(fp, "0.0822899 0.0419678 0 0.0246869 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.4482 90.4883 mo\n");
  (void) fprintf(fp, "70.4482 144.254 23.4639 187.836 -34.4971 187.836 cv\n");
  (void) fprintf(fp, "-92.4541 187.836 -139.442 144.254 -139.442 90.4883 cv\n");
  (void) fprintf(fp, "-139.442 36.7266 -92.4541 -6.85547 -34.4971 -6.85547 cv\n");
  (void) fprintf(fp, "23.4639 -6.85547 70.4482 36.7266 70.4482 90.4883 cv\n");
  (void) fprintf(fp, "0.0797936 0.0406947 0 0.0239381 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.4248 90.4883 mo\n");
  (void) fprintf(fp, "70.4248 144.238 23.4482 187.809 -34.4971 187.809 cv\n");
  (void) fprintf(fp, "-92.4385 187.809 -139.411 144.238 -139.411 90.4883 cv\n");
  (void) fprintf(fp, "-139.411 36.7422 -92.4385 -6.83203 -34.4971 -6.83203 cv\n");
  (void) fprintf(fp, "23.4482 -6.83203 70.4248 36.7422 70.4248 90.4883 cv\n");
  (void) fprintf(fp, "0.0773249 0.0394357 0 0.0231975 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.3936 90.4883 mo\n");
  (void) fprintf(fp, "70.3936 144.223 23.4326 187.785 -34.4971 187.785 cv\n");
  (void) fprintf(fp, "-92.4229 187.785 -139.384 144.223 -139.384 90.4883 cv\n");
  (void) fprintf(fp, "-139.384 36.7539 -92.4229 -6.80469 -34.4971 -6.80469 cv\n");
  (void) fprintf(fp, "23.4326 -6.80469 70.3936 36.7539 70.3936 90.4883 cv\n");
  (void) fprintf(fp, "0.0748841 0.0381909 0 0.0224652 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.3662 90.4883 mo\n");
  (void) fprintf(fp, "70.3662 144.211 23.417 187.758 -34.4971 187.758 cv\n");
  (void) fprintf(fp, "-92.4111 187.758 -139.356 144.211 -139.356 90.4883 cv\n");
  (void) fprintf(fp, "-139.356 36.7695 -92.4111 -6.78125 -34.4971 -6.78125 cv\n");
  (void) fprintf(fp, "23.417 -6.78125 70.3662 36.7695 70.3662 90.4883 cv\n");
  (void) fprintf(fp, "0.0724713 0.0369604 0 0.0217414 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.3389 90.4883 mo\n");
  (void) fprintf(fp, "70.3389 144.195 23.4053 187.734 -34.4971 187.734 cv\n");
  (void) fprintf(fp, "-92.3955 187.734 -139.329 144.195 -139.329 90.4883 cv\n");
  (void) fprintf(fp, "-139.329 36.7852 -92.3955 -6.75391 -34.4971 -6.75391 cv\n");
  (void) fprintf(fp, "23.4053 -6.75391 70.3389 36.7852 70.3389 90.4883 cv\n");
  (void) fprintf(fp, "0.0700869 0.0357443 0 0.0210261 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.3115 90.4883 mo\n");
  (void) fprintf(fp, "70.3115 144.184 23.3896 187.707 -34.4971 187.707 cv\n");
  (void) fprintf(fp, "-92.3799 187.707 -139.302 144.184 -139.302 90.4883 cv\n");
  (void) fprintf(fp, "-139.302 36.7969 -92.3799 -6.73047 -34.4971 -6.73047 cv\n");
  (void) fprintf(fp, "23.3896 -6.73047 70.3115 36.7969 70.3115 90.4883 cv\n");
  (void) fprintf(fp, "0.0677311 0.0345429 0 0.0203193 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.2842 90.4883 mo\n");
  (void) fprintf(fp, "70.2842 144.168 23.374 187.684 -34.4971 187.684 cv\n");
  (void) fprintf(fp, "-92.3643 187.684 -139.274 144.168 -139.274 90.4883 cv\n");
  (void) fprintf(fp, "-139.274 36.8086 -92.3643 -6.70312 -34.4971 -6.70312 cv\n");
  (void) fprintf(fp, "23.374 -6.70312 70.2842 36.8086 70.2842 90.4883 cv\n");
  (void) fprintf(fp, "0.0654043 0.0333562 0 0.0196213 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.2568 90.4883 mo\n");
  (void) fprintf(fp, "70.2568 144.152 23.3584 187.656 -34.4971 187.656 cv\n");
  (void) fprintf(fp, "-92.3486 187.656 -139.247 144.152 -139.247 90.4883 cv\n");
  (void) fprintf(fp, "-139.247 36.8242 -92.3486 -6.67969 -34.4971 -6.67969 cv\n");
  (void) fprintf(fp, "23.3584 -6.67969 70.2568 36.8242 70.2568 90.4883 cv\n");
  (void) fprintf(fp, "0.0631066 0.0321844 0 0.018932 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.2295 90.4883 mo\n");
  (void) fprintf(fp, "70.2295 144.141 23.3428 187.633 -34.4971 187.633 cv\n");
  (void) fprintf(fp, "-92.333 187.633 -139.22 144.141 -139.22 90.4883 cv\n");
  (void) fprintf(fp, "-139.22 36.8398 -92.333 -6.65234 -34.4971 -6.65234 cv\n");
  (void) fprintf(fp, "23.3428 -6.65234 70.2295 36.8398 70.2295 90.4883 cv\n");
  (void) fprintf(fp, "0.0608384 0.0310276 0 0.0182515 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.2021 90.4883 mo\n");
  (void) fprintf(fp, "70.2021 144.125 23.3271 187.605 -34.4971 187.605 cv\n");
  (void) fprintf(fp, "-92.3174 187.605 -139.192 144.125 -139.192 90.4883 cv\n");
  (void) fprintf(fp, "-139.192 36.8555 -92.3174 -6.62891 -34.4971 -6.62891 cv\n");
  (void) fprintf(fp, "23.3271 -6.62891 70.2021 36.8555 70.2021 90.4883 cv\n");
  (void) fprintf(fp, "0.0586001 0.029886 0 0.01758 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.1748 90.4883 mo\n");
  (void) fprintf(fp, "70.1748 144.113 23.3115 187.582 -34.4971 187.582 cv\n");
  (void) fprintf(fp, "-92.3018 187.582 -139.165 144.113 -139.165 90.4883 cv\n");
  (void) fprintf(fp, "-139.165 36.8672 -92.3018 -6.60156 -34.4971 -6.60156 cv\n");
  (void) fprintf(fp, "23.3115 -6.60156 70.1748 36.8672 70.1748 90.4883 cv\n");
  (void) fprintf(fp, "0.0563918 0.0287598 0 0.0169175 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.1475 90.4883 mo\n");
  (void) fprintf(fp, "70.1475 144.098 23.2998 187.555 -34.4971 187.555 cv\n");
  (void) fprintf(fp, "-92.2861 187.555 -139.138 144.098 -139.138 90.4883 cv\n");
  (void) fprintf(fp, "-139.138 36.8789 -92.2861 -6.57812 -34.4971 -6.57812 cv\n");
  (void) fprintf(fp, "23.2998 -6.57812 70.1475 36.8789 70.1475 90.4883 cv\n");
  (void) fprintf(fp, "0.0542141 0.0276492 0 0.0162642 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.1201 90.4883 mo\n");
  (void) fprintf(fp, "70.1201 144.082 23.2842 187.527 -34.4971 187.527 cv\n");
  (void) fprintf(fp, "-92.2744 187.527 -139.11 144.082 -139.11 90.4883 cv\n");
  (void) fprintf(fp, "-139.11 36.8945 -92.2744 -6.55078 -34.4971 -6.55078 cv\n");
  (void) fprintf(fp, "23.2842 -6.55078 70.1201 36.8945 70.1201 90.4883 cv\n");
  (void) fprintf(fp, "0.0520671 0.0265542 0 0.0156201 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.0928 90.4883 mo\n");
  (void) fprintf(fp, "70.0928 144.07 23.2686 187.504 -34.4971 187.504 cv\n");
  (void) fprintf(fp, "-92.2588 187.504 -139.083 144.07 -139.083 90.4883 cv\n");
  (void) fprintf(fp, "-139.083 36.9102 -92.2588 -6.52734 -34.4971 -6.52734 cv\n");
  (void) fprintf(fp, "23.2686 -6.52734 70.0928 36.9102 70.0928 90.4883 cv\n");
  (void) fprintf(fp, "0.0499513 0.0254751 0 0.0149854 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.0654 90.4883 mo\n");
  (void) fprintf(fp, "70.0654 144.055 23.2529 187.48 -34.4971 187.48 cv\n");
  (void) fprintf(fp, "-92.2432 187.48 -139.056 144.055 -139.056 90.4883 cv\n");
  (void) fprintf(fp, "-139.056 36.9258 -92.2432 -6.5 -34.4971 -6.5 cv\n");
  (void) fprintf(fp, "23.2529 -6.5 70.0654 36.9258 70.0654 90.4883 cv\n");
  (void) fprintf(fp, "0.047867 0.0244122 0 0.0143601 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.0381 90.4883 mo\n");
  (void) fprintf(fp, "70.0381 144.043 23.2373 187.453 -34.4971 187.453 cv\n");
  (void) fprintf(fp, "-92.2275 187.453 -139.028 144.043 -139.028 90.4883 cv\n");
  (void) fprintf(fp, "-139.028 36.9375 -92.2275 -6.47266 -34.4971 -6.47266 cv\n");
  (void) fprintf(fp, "23.2373 -6.47266 70.0381 36.9375 70.0381 90.4883 cv\n");
  (void) fprintf(fp, "0.0458147 0.0233655 0 0.0137444 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "70.0107 90.4883 mo\n");
  (void) fprintf(fp, "70.0107 144.027 23.2217 187.426 -34.4971 187.426 cv\n");
  (void) fprintf(fp, "-92.2119 187.426 -139.001 144.027 -139.001 90.4883 cv\n");
  (void) fprintf(fp, "-139.001 36.9492 -92.2119 -6.44922 -34.4971 -6.44922 cv\n");
  (void) fprintf(fp, "23.2217 -6.44922 70.0107 36.9492 70.0107 90.4883 cv\n");
  (void) fprintf(fp, "0.0437947 0.0223353 0 0.0131384 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.9834 90.4883 mo\n");
  (void) fprintf(fp, "69.9834 144.012 23.2061 187.402 -34.4971 187.402 cv\n");
  (void) fprintf(fp, "-92.1963 187.402 -138.974 144.012 -138.974 90.4883 cv\n");
  (void) fprintf(fp, "-138.974 36.9648 -92.1963 -6.42578 -34.4971 -6.42578 cv\n");
  (void) fprintf(fp, "23.2061 -6.42578 69.9834 36.9648 69.9834 90.4883 cv\n");
  (void) fprintf(fp, "0.0418074 0.0213218 0 0.0125422 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.9561 90.4883 mo\n");
  (void) fprintf(fp, "69.9561 144 23.1904 187.379 -34.4971 187.379 cv\n");
  (void) fprintf(fp, "-92.1807 187.379 -138.946 144 -138.946 90.4883 cv\n");
  (void) fprintf(fp, "-138.946 36.9805 -92.1807 -6.39844 -34.4971 -6.39844 cv\n");
  (void) fprintf(fp, "23.1904 -6.39844 69.9561 36.9805 69.9561 90.4883 cv\n");
  (void) fprintf(fp, "0.0398533 0.0203252 0 0.011956 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.9287 90.4883 mo\n");
  (void) fprintf(fp, "69.9287 143.984 23.1748 187.352 -34.4971 187.352 cv\n");
  (void) fprintf(fp, "-92.1689 187.352 -138.919 143.984 -138.919 90.4883 cv\n");
  (void) fprintf(fp, "-138.919 36.9961 -92.1689 -6.37109 -34.4971 -6.37109 cv\n");
  (void) fprintf(fp, "23.1748 -6.37109 69.9287 36.9961 69.9287 90.4883 cv\n");
  (void) fprintf(fp, "0.0379329 0.0193458 0 0.0113798 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.9014 90.4883 mo\n");
  (void) fprintf(fp, "69.9014 143.973 23.1592 187.324 -34.4971 187.324 cv\n");
  (void) fprintf(fp, "-92.1533 187.324 -138.892 143.973 -138.892 90.4883 cv\n");
  (void) fprintf(fp, "-138.892 37.0078 -92.1533 -6.34766 -34.4971 -6.34766 cv\n");
  (void) fprintf(fp, "23.1592 -6.34766 69.9014 37.0078 69.9014 90.4883 cv\n");
  (void) fprintf(fp, "0.0360465 0.0183837 0 0.010814 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.874 90.4883 mo\n");
  (void) fprintf(fp, "69.874 143.957 23.1475 187.301 -34.4971 187.301 cv\n");
  (void) fprintf(fp, "-92.1377 187.301 -138.864 143.957 -138.864 90.4883 cv\n");
  (void) fprintf(fp, "-138.864 37.0195 -92.1377 -6.32422 -34.4971 -6.32422 cv\n");
  (void) fprintf(fp, "23.1475 -6.32422 69.874 37.0195 69.874 90.4883 cv\n");
  (void) fprintf(fp, "0.0341948 0.0174394 0 0.0102584 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.8467 90.4883 mo\n");
  (void) fprintf(fp, "69.8467 143.941 23.1318 187.277 -34.4971 187.277 cv\n");
  (void) fprintf(fp, "-92.1221 187.277 -138.837 143.941 -138.837 90.4883 cv\n");
  (void) fprintf(fp, "-138.837 37.0352 -92.1221 -6.29688 -34.4971 -6.29688 cv\n");
  (void) fprintf(fp, "23.1318 -6.29688 69.8467 37.0352 69.8467 90.4883 cv\n");
  (void) fprintf(fp, "0.0323782 0.0165129 0 0.00971347 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.8193 90.4883 mo\n");
  (void) fprintf(fp, "69.8193 143.93 23.1162 187.25 -34.4971 187.25 cv\n");
  (void) fprintf(fp, "-92.1064 187.25 -138.81 143.93 -138.81 90.4883 cv\n");
  (void) fprintf(fp, "-138.81 37.0508 -92.1064 -6.26953 -34.4971 -6.26953 cv\n");
  (void) fprintf(fp, "23.1162 -6.26953 69.8193 37.0508 69.8193 90.4883 cv\n");
  (void) fprintf(fp, "0.0305973 0.0156046 0 0.00917917 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.792 90.4883 mo\n");
  (void) fprintf(fp, "69.792 143.914 23.1006 187.223 -34.4971 187.223 cv\n");
  (void) fprintf(fp, "-92.0908 187.223 -138.782 143.914 -138.782 90.4883 cv\n");
  (void) fprintf(fp, "-138.782 37.0664 -92.0908 -6.24609 -34.4971 -6.24609 cv\n");
  (void) fprintf(fp, "23.1006 -6.24609 69.792 37.0664 69.792 90.4883 cv\n");
  (void) fprintf(fp, "0.0288527 0.0147149 0 0.00865579 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.7646 90.4883 mo\n");
  (void) fprintf(fp, "69.7646 143.902 23.085 187.199 -34.4971 187.199 cv\n");
  (void) fprintf(fp, "-92.0752 187.199 -138.755 143.902 -138.755 90.4883 cv\n");
  (void) fprintf(fp, "-138.755 37.0781 -92.0752 -6.21875 -34.4971 -6.21875 cv\n");
  (void) fprintf(fp, "23.085 -6.21875 69.7646 37.0781 69.7646 90.4883 cv\n");
  (void) fprintf(fp, "0.027145 0.013844 0 0.00814348 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.7373 90.4883 mo\n");
  (void) fprintf(fp, "69.7373 143.887 23.0693 187.172 -34.4971 187.172 cv\n");
  (void) fprintf(fp, "-92.0596 187.172 -138.728 143.887 -138.728 90.4883 cv\n");
  (void) fprintf(fp, "-138.728 37.0898 -92.0596 -6.19531 -34.4971 -6.19531 cv\n");
  (void) fprintf(fp, "23.0693 -6.19531 69.7373 37.0898 69.7373 90.4883 cv\n");
  (void) fprintf(fp, "0.0254748 0.0129921 0 0.00764245 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.71 90.4883 mo\n");
  (void) fprintf(fp, "69.71 143.871 23.0576 187.148 -34.4971 187.148 cv\n");
  (void) fprintf(fp, "-92.0439 187.148 -138.7 143.871 -138.7 90.4883 cv\n");
  (void) fprintf(fp, "-138.7 37.1055 -92.0439 -6.16797 -34.4971 -6.16797 cv\n");
  (void) fprintf(fp, "23.0576 -6.16797 69.71 37.1055 69.71 90.4883 cv\n");
  (void) fprintf(fp, "0.0238429 0.0121599 0 0.00715286 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.6826 90.4883 mo\n");
  (void) fprintf(fp, "69.6826 143.859 23.042 187.121 -34.4971 187.121 cv\n");
  (void) fprintf(fp, "-92.0283 187.121 -138.673 143.859 -138.673 90.4883 cv\n");
  (void) fprintf(fp, "-138.673 37.1211 -92.0283 -6.14453 -34.4971 -6.14453 cv\n");
  (void) fprintf(fp, "23.042 -6.14453 69.6826 37.1211 69.6826 90.4883 cv\n");
  (void) fprintf(fp, "0.0222499 0.0113475 0 0.00667495 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.6553 90.4883 mo\n");
  (void) fprintf(fp, "69.6553 143.844 23.0264 187.098 -34.4971 187.098 cv\n");
  (void) fprintf(fp, "-92.0166 187.098 -138.646 143.844 -138.646 90.4883 cv\n");
  (void) fprintf(fp, "-138.646 37.1367 -92.0166 -6.11719 -34.4971 -6.11719 cv\n");
  (void) fprintf(fp, "23.0264 -6.11719 69.6553 37.1367 69.6553 90.4883 cv\n");
  (void) fprintf(fp, "0.0206966 0.0105553 0 0.00620902 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.6279 90.4883 mo\n");
  (void) fprintf(fp, "69.6279 143.832 23.0107 187.07 -34.4971 187.07 cv\n");
  (void) fprintf(fp, "-92.001 187.07 -138.618 143.832 -138.618 90.4883 cv\n");
  (void) fprintf(fp, "-138.618 37.1484 -92.001 -6.09375 -34.4971 -6.09375 cv\n");
  (void) fprintf(fp, "23.0107 -6.09375 69.6279 37.1484 69.6279 90.4883 cv\n");
  (void) fprintf(fp, "0.0191839 0.0097838 0 0.00575519 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.6006 90.4883 mo\n");
  (void) fprintf(fp, "69.6006 143.816 22.9951 187.047 -34.4971 187.047 cv\n");
  (void) fprintf(fp, "-91.9854 187.047 -138.591 143.816 -138.591 90.4883 cv\n");
  (void) fprintf(fp, "-138.591 37.1602 -91.9854 -6.06641 -34.4971 -6.06641 cv\n");
  (void) fprintf(fp, "22.9951 -6.06641 69.6006 37.1602 69.6006 90.4883 cv\n");
  (void) fprintf(fp, "0.0177127 0.0090335 0 0.00531381 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.5732 90.4883 mo\n");
  (void) fprintf(fp, "69.5732 143.801 22.9795 187.02 -34.4971 187.02 cv\n");
  (void) fprintf(fp, "-91.9697 187.02 -138.563 143.801 -138.563 90.4883 cv\n");
  (void) fprintf(fp, "-138.563 37.1758 -91.9697 -6.04297 -34.4971 -6.04297 cv\n");
  (void) fprintf(fp, "22.9795 -6.04297 69.5732 37.1758 69.5732 90.4883 cv\n");
  (void) fprintf(fp, "0.0162839 0.00830477 0 0.0048852 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.5459 90.4883 mo\n");
  (void) fprintf(fp, "69.5459 143.789 22.9639 186.996 -34.4971 186.996 cv\n");
  (void) fprintf(fp, "-91.9541 186.996 -138.536 143.789 -138.536 90.4883 cv\n");
  (void) fprintf(fp, "-138.536 37.1914 -91.9541 -6.01562 -34.4971 -6.01562 cv\n");
  (void) fprintf(fp, "22.9639 -6.01562 69.5459 37.1914 69.5459 90.4883 cv\n");
  (void) fprintf(fp, "0.0148985 0.00759822 0 0.00446957 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.5186 90.4883 mo\n");
  (void) fprintf(fp, "69.5186 143.773 22.9482 186.969 -34.4971 186.969 cv\n");
  (void) fprintf(fp, "-91.9385 186.969 -138.509 143.773 -138.509 90.4883 cv\n");
  (void) fprintf(fp, "-138.509 37.2031 -91.9385 -5.99219 -34.4971 -5.99219 cv\n");
  (void) fprintf(fp, "22.9482 -5.99219 69.5186 37.2031 69.5186 90.4883 cv\n");
  (void) fprintf(fp, "0.0135577 0.00691444 0 0.0040673 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.4873 90.4883 mo\n");
  (void) fprintf(fp, "69.4873 143.762 22.9326 186.945 -34.4971 186.945 cv\n");
  (void) fprintf(fp, "-91.9229 186.945 -138.481 143.762 -138.481 90.4883 cv\n");
  (void) fprintf(fp, "-138.481 37.2188 -91.9229 -5.96484 -34.4971 -5.96484 cv\n");
  (void) fprintf(fp, "22.9326 -5.96484 69.4873 37.2188 69.4873 90.4883 cv\n");
  (void) fprintf(fp, "0.0122626 0.00625396 0 0.0036788 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.4639 90.4883 mo\n");
  (void) fprintf(fp, "69.4639 143.746 22.917 186.918 -34.4971 186.918 cv\n");
  (void) fprintf(fp, "-91.9111 186.918 -138.454 143.746 -138.454 90.4883 cv\n");
  (void) fprintf(fp, "-138.454 37.2305 -91.9111 -5.94141 -34.4971 -5.94141 cv\n");
  (void) fprintf(fp, "22.917 -5.94141 69.4639 37.2305 69.4639 90.4883 cv\n");
  (void) fprintf(fp, "0.0110148 0.0056175 0 0.00330442 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.4326 90.4883 mo\n");
  (void) fprintf(fp, "69.4326 143.73 22.9014 186.895 -34.4971 186.895 cv\n");
  (void) fprintf(fp, "-91.8955 186.895 -138.427 143.73 -138.427 90.4883 cv\n");
  (void) fprintf(fp, "-138.427 37.2461 -91.8955 -5.91406 -34.4971 -5.91406 cv\n");
  (void) fprintf(fp, "22.9014 -5.91406 69.4326 37.2461 69.4326 90.4883 cv\n");
  (void) fprintf(fp, "0.00981539 0.00500584 0 0.00294465 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.4092 90.4883 mo\n");
  (void) fprintf(fp, "69.4092 143.719 22.8896 186.867 -34.4971 186.867 cv\n");
  (void) fprintf(fp, "-91.8799 186.867 -138.396 143.719 -138.396 90.4883 cv\n");
  (void) fprintf(fp, "-138.396 37.2617 -91.8799 -5.89062 -34.4971 -5.89062 cv\n");
  (void) fprintf(fp, "22.8896 -5.89062 69.4092 37.2617 69.4092 90.4883 cv\n");
  (void) fprintf(fp, "0.00866628 0.0044198 0 0.0025999 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.3779 90.4883 mo\n");
  (void) fprintf(fp, "69.3779 143.703 22.874 186.844 -34.4971 186.844 cv\n");
  (void) fprintf(fp, "-91.8643 186.844 -138.372 143.703 -138.372 90.4883 cv\n");
  (void) fprintf(fp, "-138.372 37.2734 -91.8643 -5.86328 -34.4971 -5.86328 cv\n");
  (void) fprintf(fp, "22.874 -5.86328 69.3779 37.2734 69.3779 90.4883 cv\n");
  (void) fprintf(fp, "0.00756919 0.00386029 0 0.00227076 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.3545 90.4883 mo\n");
  (void) fprintf(fp, "69.3545 143.691 22.8584 186.816 -34.4971 186.816 cv\n");
  (void) fprintf(fp, "-91.8486 186.816 -138.341 143.691 -138.341 90.4883 cv\n");
  (void) fprintf(fp, "-138.341 37.2891 -91.8486 -5.83984 -34.4971 -5.83984 cv\n");
  (void) fprintf(fp, "22.8584 -5.83984 69.3545 37.2891 69.3545 90.4883 cv\n");
  (void) fprintf(fp, "0.00652617 0.00332838 0 0.00195783 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.3232 90.4883 mo\n");
  (void) fprintf(fp, "69.3232 143.676 22.8428 186.793 -34.4971 186.793 cv\n");
  (void) fprintf(fp, "-91.833 186.793 -138.317 143.676 -138.317 90.4883 cv\n");
  (void) fprintf(fp, "-138.317 37.3008 -91.833 -5.8125 -34.4971 -5.8125 cv\n");
  (void) fprintf(fp, "22.8428 -5.8125 69.3232 37.3008 69.3232 90.4883 cv\n");
  (void) fprintf(fp, "0.0055396 0.0028252 0 0.0016619 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.2998 90.4883 mo\n");
  (void) fprintf(fp, "69.2998 143.66 22.8271 186.766 -34.4971 186.766 cv\n");
  (void) fprintf(fp, "-91.8174 186.766 -138.286 143.66 -138.286 90.4883 cv\n");
  (void) fprintf(fp, "-138.286 37.3164 -91.8174 -5.78906 -34.4971 -5.78906 cv\n");
  (void) fprintf(fp, "22.8271 -5.78906 69.2998 37.3164 69.2998 90.4883 cv\n");
  (void) fprintf(fp, "0.00461221 0.00235224 0 0.00138366 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.2686 90.4883 mo\n");
  (void) fprintf(fp, "69.2686 143.648 22.8115 186.738 -34.4971 186.738 cv\n");
  (void) fprintf(fp, "-91.8018 186.738 -138.259 143.648 -138.259 90.4883 cv\n");
  (void) fprintf(fp, "-138.259 37.332 -91.8018 -5.76172 -34.4971 -5.76172 cv\n");
  (void) fprintf(fp, "22.8115 -5.76172 69.2686 37.332 69.2686 90.4883 cv\n");
  (void) fprintf(fp, "0.00374711 0.00191104 0 0.00112414 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.2451 90.4883 mo\n");
  (void) fprintf(fp, "69.2451 143.633 22.7998 186.715 -34.4971 186.715 cv\n");
  (void) fprintf(fp, "-91.7861 186.715 -138.231 143.633 -138.231 90.4883 cv\n");
  (void) fprintf(fp, "-138.231 37.3438 -91.7861 -5.73828 -34.4971 -5.73828 cv\n");
  (void) fprintf(fp, "22.7998 -5.73828 69.2451 37.3438 69.2451 90.4883 cv\n");
  (void) fprintf(fp, "0.00294816 0.00150359 0 0.000884473 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.2139 90.4883 mo\n");
  (void) fprintf(fp, "69.2139 143.621 22.7842 186.691 -34.4971 186.691 cv\n");
  (void) fprintf(fp, "-91.7744 186.691 -138.204 143.621 -138.204 90.4883 cv\n");
  (void) fprintf(fp, "-138.204 37.3594 -91.7744 -5.71094 -34.4971 -5.71094 cv\n");
  (void) fprintf(fp, "22.7842 -5.71094 69.2139 37.3594 69.2139 90.4883 cv\n");
  (void) fprintf(fp, "0.00222015 0.00113225 0 0.000666022 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.1865 90.4883 mo\n");
  (void) fprintf(fp, "69.1865 143.605 22.7686 186.664 -34.4971 186.664 cv\n");
  (void) fprintf(fp, "-91.7588 186.664 -138.177 143.605 -138.177 90.4883 cv\n");
  (void) fprintf(fp, "-138.177 37.3711 -91.7588 -5.68359 -34.4971 -5.68359 cv\n");
  (void) fprintf(fp, "22.7686 -5.68359 69.1865 37.3711 69.1865 90.4883 cv\n");
  (void) fprintf(fp, "0.00156903 0.000800192 0 0.000470698 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.1592 90.4883 mo\n");
  (void) fprintf(fp, "69.1592 143.59 22.7529 186.637 -34.4971 186.637 cv\n");
  (void) fprintf(fp, "-91.7432 186.637 -138.149 143.59 -138.149 90.4883 cv\n");
  (void) fprintf(fp, "-138.149 37.3867 -91.7432 -5.66016 -34.4971 -5.66016 cv\n");
  (void) fprintf(fp, "22.7529 -5.66016 69.1592 37.3867 69.1592 90.4883 cv\n");
  (void) fprintf(fp, "0.00100297 0.000511527 0 0.000300884 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.1318 90.4883 mo\n");
  (void) fprintf(fp, "69.1318 143.578 22.7373 186.613 -34.4971 186.613 cv\n");
  (void) fprintf(fp, "-91.7275 186.613 -138.122 143.578 -138.122 90.4883 cv\n");
  (void) fprintf(fp, "-138.122 37.4023 -91.7275 -5.63672 -34.4971 -5.63672 cv\n");
  (void) fprintf(fp, "22.7373 -5.63672 69.1318 37.4023 69.1318 90.4883 cv\n");
  (void) fprintf(fp, "0.00053376 0.000272214 0 0.000160158 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.1045 90.4883 mo\n");
  (void) fprintf(fp, "69.1045 143.562 22.7217 186.59 -34.4971 186.59 cv\n");
  (void) fprintf(fp, "-91.7119 186.59 -138.095 143.562 -138.095 90.4883 cv\n");
  (void) fprintf(fp, "-138.095 37.4141 -91.7119 -5.60938 -34.4971 -5.60938 cv\n");
  (void) fprintf(fp, "22.7217 -5.60938 69.1045 37.4141 69.1045 90.4883 cv\n");
  (void) fprintf(fp, "0.000181556 9.26256e-05 0 5.44786e-05 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "69.0771 90.4883 mo\n");
  (void) fprintf(fp, "69.0771 143.551 22.7061 186.562 -34.4971 186.562 cv\n");
  (void) fprintf(fp, "-91.6963 186.562 -138.067 143.551 -138.067 90.4883 cv\n");
  (void) fprintf(fp, "-138.067 37.4297 -91.6963 -5.58203 -34.4971 -5.58203 cv\n");
  (void) fprintf(fp, "22.7061 -5.58203 69.0771 37.4297 69.0771 90.4883 cv\n");
  (void) fprintf(fp, "0 0 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "grestore\n");
  (void) fprintf(fp, "gsave\n");
  (void) fprintf(fp, "63.8564 75.9854 mo\n");
  (void) fprintf(fp, "63.8564 114.42 30.9141 145.583 -9.72363 145.583 cv\n");
  (void) fprintf(fp, "-50.3613 145.583 -83.3066 114.42 -83.3066 75.9854 cv\n");
  (void) fprintf(fp, "-83.3066 37.5479 -50.3613 6.38477 -9.72363 6.38477 cv\n");
  (void) fprintf(fp, "30.9141 6.38477 63.8564 37.5479 63.8564 75.9854 cv\n");
  (void) fprintf(fp, "clp\n");
  (void) fprintf(fp, "-84.3057 5.82422 mo\n");
  (void) fprintf(fp, "0.249023 5.82422 ln\n");
  (void) fprintf(fp, "0.249023 146.145 ln\n");
  (void) fprintf(fp, "-84.3057 146.145 ln\n");
  (void) fprintf(fp, "-84.3057 5.82422 ln\n");
  (void) fprintf(fp, "0 0 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "0.249023 5.82422 mo\n");
  (void) fprintf(fp, "0.475586 5.82422 ln\n");
  (void) fprintf(fp, "0.475586 146.145 ln\n");
  (void) fprintf(fp, "0.249023 146.145 ln\n");
  (void) fprintf(fp, "0.249023 5.82422 ln\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "0.475586 5.82422 mo\n");
  (void) fprintf(fp, "0.702148 5.82422 ln\n");
  (void) fprintf(fp, "0.702148 146.145 ln\n");
  (void) fprintf(fp, "0.475586 146.145 ln\n");
  (void) fprintf(fp, "0.475586 5.82422 ln\n");
  (void) fprintf(fp, "0 0.00181961 0.00294238 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "0.702148 5.82422 mo\n");
  (void) fprintf(fp, "0.928711 5.82422 ln\n");
  (void) fprintf(fp, "0.928711 146.145 ln\n");
  (void) fprintf(fp, "0.702148 146.145 ln\n");
  (void) fprintf(fp, "0.702148 5.82422 ln\n");
  (void) fprintf(fp, "0 0.00378162 0.00611496 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "0.928711 5.82422 mo\n");
  (void) fprintf(fp, "1.15527 5.82422 ln\n");
  (void) fprintf(fp, "1.15527 146.145 ln\n");
  (void) fprintf(fp, "0.928711 146.145 ln\n");
  (void) fprintf(fp, "0.928711 5.82422 ln\n");
  (void) fprintf(fp, "0 0.00580126 0.00938076 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "1.15527 5.82422 mo\n");
  (void) fprintf(fp, "1.38184 5.82422 ln\n");
  (void) fprintf(fp, "1.38184 146.145 ln\n");
  (void) fprintf(fp, "1.15527 146.145 ln\n");
  (void) fprintf(fp, "1.15527 5.82422 ln\n");
  (void) fprintf(fp, "0 0.00785923 0.0127085 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "1.38184 5.82422 mo\n");
  (void) fprintf(fp, "1.6084 5.82422 ln\n");
  (void) fprintf(fp, "1.6084 146.145 ln\n");
  (void) fprintf(fp, "1.38184 146.145 ln\n");
  (void) fprintf(fp, "1.38184 5.82422 ln\n");
  (void) fprintf(fp, "0 0.00994617 0.0160831 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "1.6084 5.82422 mo\n");
  (void) fprintf(fp, "1.83496 5.82422 ln\n");
  (void) fprintf(fp, "1.83496 146.145 ln\n");
  (void) fprintf(fp, "1.6084 146.145 ln\n");
  (void) fprintf(fp, "1.6084 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0120565 0.0194956 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "1.83496 5.82422 mo\n");
  (void) fprintf(fp, "2.06152 5.82422 ln\n");
  (void) fprintf(fp, "2.06152 146.145 ln\n");
  (void) fprintf(fp, "1.83496 146.145 ln\n");
  (void) fprintf(fp, "1.83496 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0141865 0.0229398 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "2.06152 5.82422 mo\n");
  (void) fprintf(fp, "2.28809 5.82422 ln\n");
  (void) fprintf(fp, "2.28809 146.145 ln\n");
  (void) fprintf(fp, "2.06152 146.145 ln\n");
  (void) fprintf(fp, "2.06152 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0163335 0.0264115 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "2.28809 5.82422 mo\n");
  (void) fprintf(fp, "2.51465 5.82422 ln\n");
  (void) fprintf(fp, "2.51465 146.145 ln\n");
  (void) fprintf(fp, "2.28809 146.145 ln\n");
  (void) fprintf(fp, "2.28809 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0184954 0.0299074 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "2.51465 5.82422 mo\n");
  (void) fprintf(fp, "2.74121 5.82422 ln\n");
  (void) fprintf(fp, "2.74121 146.145 ln\n");
  (void) fprintf(fp, "2.51465 146.145 ln\n");
  (void) fprintf(fp, "2.51465 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0206707 0.0334249 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "2.74121 5.82422 mo\n");
  (void) fprintf(fp, "2.96777 5.82422 ln\n");
  (void) fprintf(fp, "2.96777 146.145 ln\n");
  (void) fprintf(fp, "2.74121 146.145 ln\n");
  (void) fprintf(fp, "2.74121 5.82422 ln\n");
  (void) fprintf(fp, "0 0.022858 0.0369619 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "2.96777 5.82422 mo\n");
  (void) fprintf(fp, "3.19434 5.82422 ln\n");
  (void) fprintf(fp, "3.19434 146.145 ln\n");
  (void) fprintf(fp, "2.96777 146.145 ln\n");
  (void) fprintf(fp, "2.96777 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0250565 0.0405169 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "3.19434 5.82422 mo\n");
  (void) fprintf(fp, "3.41699 5.82422 ln\n");
  (void) fprintf(fp, "3.41699 146.145 ln\n");
  (void) fprintf(fp, "3.19434 146.145 ln\n");
  (void) fprintf(fp, "3.19434 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0272651 0.0440882 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "3.41699 5.82422 mo\n");
  (void) fprintf(fp, "3.64355 5.82422 ln\n");
  (void) fprintf(fp, "3.64355 146.145 ln\n");
  (void) fprintf(fp, "3.41699 146.145 ln\n");
  (void) fprintf(fp, "3.41699 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0294831 0.0476748 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "3.64355 5.82422 mo\n");
  (void) fprintf(fp, "3.87012 5.82422 ln\n");
  (void) fprintf(fp, "3.87012 146.145 ln\n");
  (void) fprintf(fp, "3.64355 146.145 ln\n");
  (void) fprintf(fp, "3.64355 5.82422 ln\n");
  (void) fprintf(fp, "0 0.03171 0.0512757 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "3.87012 5.82422 mo\n");
  (void) fprintf(fp, "4.09668 5.82422 ln\n");
  (void) fprintf(fp, "4.09668 146.145 ln\n");
  (void) fprintf(fp, "3.87012 146.145 ln\n");
  (void) fprintf(fp, "3.87012 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0339451 0.0548899 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "4.09668 5.82422 mo\n");
  (void) fprintf(fp, "4.32324 5.82422 ln\n");
  (void) fprintf(fp, "4.32324 146.145 ln\n");
  (void) fprintf(fp, "4.09668 146.145 ln\n");
  (void) fprintf(fp, "4.09668 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0361879 0.0585167 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "4.32324 5.82422 mo\n");
  (void) fprintf(fp, "4.5498 5.82422 ln\n");
  (void) fprintf(fp, "4.5498 146.145 ln\n");
  (void) fprintf(fp, "4.32324 146.145 ln\n");
  (void) fprintf(fp, "4.32324 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0384381 0.0621552 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "4.5498 5.82422 mo\n");
  (void) fprintf(fp, "4.77637 5.82422 ln\n");
  (void) fprintf(fp, "4.77637 146.145 ln\n");
  (void) fprintf(fp, "4.5498 146.145 ln\n");
  (void) fprintf(fp, "4.5498 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0406952 0.065805 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "4.77637 5.82422 mo\n");
  (void) fprintf(fp, "5.00293 5.82422 ln\n");
  (void) fprintf(fp, "5.00293 146.145 ln\n");
  (void) fprintf(fp, "4.77637 146.145 ln\n");
  (void) fprintf(fp, "4.77637 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0429589 0.0694655 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "5.00293 5.82422 mo\n");
  (void) fprintf(fp, "5.22949 5.82422 ln\n");
  (void) fprintf(fp, "5.22949 146.145 ln\n");
  (void) fprintf(fp, "5.00293 146.145 ln\n");
  (void) fprintf(fp, "5.00293 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0452289 0.0731361 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "5.22949 5.82422 mo\n");
  (void) fprintf(fp, "5.45605 5.82422 ln\n");
  (void) fprintf(fp, "5.45605 146.145 ln\n");
  (void) fprintf(fp, "5.22949 146.145 ln\n");
  (void) fprintf(fp, "5.22949 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0475048 0.0768164 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "5.45605 5.82422 mo\n");
  (void) fprintf(fp, "5.68262 5.82422 ln\n");
  (void) fprintf(fp, "5.68262 146.145 ln\n");
  (void) fprintf(fp, "5.45605 146.145 ln\n");
  (void) fprintf(fp, "5.45605 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0497866 0.0805059 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "5.68262 5.82422 mo\n");
  (void) fprintf(fp, "5.90918 5.82422 ln\n");
  (void) fprintf(fp, "5.90918 146.145 ln\n");
  (void) fprintf(fp, "5.68262 146.145 ln\n");
  (void) fprintf(fp, "5.68262 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0520738 0.0842044 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "5.90918 5.82422 mo\n");
  (void) fprintf(fp, "6.13574 5.82422 ln\n");
  (void) fprintf(fp, "6.13574 146.145 ln\n");
  (void) fprintf(fp, "5.90918 146.145 ln\n");
  (void) fprintf(fp, "5.90918 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0543663 0.0879114 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "6.13574 5.82422 mo\n");
  (void) fprintf(fp, "6.3623 5.82422 ln\n");
  (void) fprintf(fp, "6.3623 146.145 ln\n");
  (void) fprintf(fp, "6.13574 146.145 ln\n");
  (void) fprintf(fp, "6.13574 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0566639 0.0916266 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "6.3623 5.82422 mo\n");
  (void) fprintf(fp, "6.58887 5.82422 ln\n");
  (void) fprintf(fp, "6.58887 146.145 ln\n");
  (void) fprintf(fp, "6.3623 146.145 ln\n");
  (void) fprintf(fp, "6.3623 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0589663 0.0953498 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "6.58887 5.82422 mo\n");
  (void) fprintf(fp, "6.81543 5.82422 ln\n");
  (void) fprintf(fp, "6.81543 146.145 ln\n");
  (void) fprintf(fp, "6.58887 146.145 ln\n");
  (void) fprintf(fp, "6.58887 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0612735 0.0990806 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "6.81543 5.82422 mo\n");
  (void) fprintf(fp, "7.04199 5.82422 ln\n");
  (void) fprintf(fp, "7.04199 146.145 ln\n");
  (void) fprintf(fp, "6.81543 146.145 ln\n");
  (void) fprintf(fp, "6.81543 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0635853 0.102819 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "7.04199 5.82422 mo\n");
  (void) fprintf(fp, "7.26855 5.82422 ln\n");
  (void) fprintf(fp, "7.26855 146.145 ln\n");
  (void) fprintf(fp, "7.04199 146.145 ln\n");
  (void) fprintf(fp, "7.04199 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0659015 0.106564 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "7.26855 5.82422 mo\n");
  (void) fprintf(fp, "7.49512 5.82422 ln\n");
  (void) fprintf(fp, "7.49512 146.145 ln\n");
  (void) fprintf(fp, "7.26855 146.145 ln\n");
  (void) fprintf(fp, "7.26855 5.82422 ln\n");
  (void) fprintf(fp, "0 0.068222 0.110316 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "7.49512 5.82422 mo\n");
  (void) fprintf(fp, "7.72168 5.82422 ln\n");
  (void) fprintf(fp, "7.72168 146.145 ln\n");
  (void) fprintf(fp, "7.49512 146.145 ln\n");
  (void) fprintf(fp, "7.49512 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0705466 0.114075 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "7.72168 5.82422 mo\n");
  (void) fprintf(fp, "7.94824 5.82422 ln\n");
  (void) fprintf(fp, "7.94824 146.145 ln\n");
  (void) fprintf(fp, "7.72168 146.145 ln\n");
  (void) fprintf(fp, "7.72168 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0728753 0.117841 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "7.94824 5.82422 mo\n");
  (void) fprintf(fp, "8.1748 5.82422 ln\n");
  (void) fprintf(fp, "8.1748 146.145 ln\n");
  (void) fprintf(fp, "7.94824 146.145 ln\n");
  (void) fprintf(fp, "7.94824 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0752078 0.121613 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "8.1748 5.82422 mo\n");
  (void) fprintf(fp, "8.40137 5.82422 ln\n");
  (void) fprintf(fp, "8.40137 146.145 ln\n");
  (void) fprintf(fp, "8.1748 146.145 ln\n");
  (void) fprintf(fp, "8.1748 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0775442 0.125391 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "8.40137 5.82422 mo\n");
  (void) fprintf(fp, "8.62793 5.82422 ln\n");
  (void) fprintf(fp, "8.62793 146.145 ln\n");
  (void) fprintf(fp, "8.40137 146.145 ln\n");
  (void) fprintf(fp, "8.40137 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0798843 0.129175 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "8.62793 5.82422 mo\n");
  (void) fprintf(fp, "8.85449 5.82422 ln\n");
  (void) fprintf(fp, "8.85449 146.145 ln\n");
  (void) fprintf(fp, "8.62793 146.145 ln\n");
  (void) fprintf(fp, "8.62793 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0822279 0.132964 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "8.85449 5.82422 mo\n");
  (void) fprintf(fp, "9.08105 5.82422 ln\n");
  (void) fprintf(fp, "9.08105 146.145 ln\n");
  (void) fprintf(fp, "8.85449 146.145 ln\n");
  (void) fprintf(fp, "8.85449 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0845751 0.13676 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "9.08105 5.82422 mo\n");
  (void) fprintf(fp, "9.30762 5.82422 ln\n");
  (void) fprintf(fp, "9.30762 146.145 ln\n");
  (void) fprintf(fp, "9.08105 146.145 ln\n");
  (void) fprintf(fp, "9.08105 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0869257 0.140561 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "9.30762 5.82422 mo\n");
  (void) fprintf(fp, "9.53418 5.82422 ln\n");
  (void) fprintf(fp, "9.53418 146.145 ln\n");
  (void) fprintf(fp, "9.30762 146.145 ln\n");
  (void) fprintf(fp, "9.30762 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0892797 0.144367 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "9.53418 5.82422 mo\n");
  (void) fprintf(fp, "9.76074 5.82422 ln\n");
  (void) fprintf(fp, "9.76074 146.145 ln\n");
  (void) fprintf(fp, "9.53418 146.145 ln\n");
  (void) fprintf(fp, "9.53418 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0916368 0.148179 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "9.76074 5.82422 mo\n");
  (void) fprintf(fp, "9.9873 5.82422 ln\n");
  (void) fprintf(fp, "9.9873 146.145 ln\n");
  (void) fprintf(fp, "9.76074 146.145 ln\n");
  (void) fprintf(fp, "9.76074 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0939972 0.151996 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "9.9873 5.82422 mo\n");
  (void) fprintf(fp, "10.2139 5.82422 ln\n");
  (void) fprintf(fp, "10.2139 146.145 ln\n");
  (void) fprintf(fp, "9.9873 146.145 ln\n");
  (void) fprintf(fp, "9.9873 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0963607 0.155817 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "10.2139 5.82422 mo\n");
  (void) fprintf(fp, "10.4404 5.82422 ln\n");
  (void) fprintf(fp, "10.4404 146.145 ln\n");
  (void) fprintf(fp, "10.2139 146.145 ln\n");
  (void) fprintf(fp, "10.2139 5.82422 ln\n");
  (void) fprintf(fp, "0 0.0987273 0.159644 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "10.4404 5.82422 mo\n");
  (void) fprintf(fp, "10.667 5.82422 ln\n");
  (void) fprintf(fp, "10.667 146.145 ln\n");
  (void) fprintf(fp, "10.4404 146.145 ln\n");
  (void) fprintf(fp, "10.4404 5.82422 ln\n");
  (void) fprintf(fp, "0 0.101097 0.163476 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "10.667 5.82422 mo\n");
  (void) fprintf(fp, "10.8936 5.82422 ln\n");
  (void) fprintf(fp, "10.8936 146.145 ln\n");
  (void) fprintf(fp, "10.667 146.145 ln\n");
  (void) fprintf(fp, "10.667 5.82422 ln\n");
  (void) fprintf(fp, "0 0.103469 0.167312 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "10.8936 5.82422 mo\n");
  (void) fprintf(fp, "11.1201 5.82422 ln\n");
  (void) fprintf(fp, "11.1201 146.145 ln\n");
  (void) fprintf(fp, "10.8936 146.145 ln\n");
  (void) fprintf(fp, "10.8936 5.82422 ln\n");
  (void) fprintf(fp, "0 0.105845 0.171153 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "11.1201 5.82422 mo\n");
  (void) fprintf(fp, "11.3467 5.82422 ln\n");
  (void) fprintf(fp, "11.3467 146.145 ln\n");
  (void) fprintf(fp, "11.1201 146.145 ln\n");
  (void) fprintf(fp, "11.1201 5.82422 ln\n");
  (void) fprintf(fp, "0 0.108223 0.174998 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "11.3467 5.82422 mo\n");
  (void) fprintf(fp, "11.5693 5.82422 ln\n");
  (void) fprintf(fp, "11.5693 146.145 ln\n");
  (void) fprintf(fp, "11.3467 146.145 ln\n");
  (void) fprintf(fp, "11.3467 5.82422 ln\n");
  (void) fprintf(fp, "0 0.110604 0.178848 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "11.5693 5.82422 mo\n");
  (void) fprintf(fp, "11.7959 5.82422 ln\n");
  (void) fprintf(fp, "11.7959 146.145 ln\n");
  (void) fprintf(fp, "11.5693 146.145 ln\n");
  (void) fprintf(fp, "11.5693 5.82422 ln\n");
  (void) fprintf(fp, "0 0.112987 0.182703 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "11.7959 5.82422 mo\n");
  (void) fprintf(fp, "12.0225 5.82422 ln\n");
  (void) fprintf(fp, "12.0225 146.145 ln\n");
  (void) fprintf(fp, "11.7959 146.145 ln\n");
  (void) fprintf(fp, "11.7959 5.82422 ln\n");
  (void) fprintf(fp, "0 0.115373 0.186561 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "12.0225 5.82422 mo\n");
  (void) fprintf(fp, "12.249 5.82422 ln\n");
  (void) fprintf(fp, "12.249 146.145 ln\n");
  (void) fprintf(fp, "12.0225 146.145 ln\n");
  (void) fprintf(fp, "12.0225 5.82422 ln\n");
  (void) fprintf(fp, "0 0.117762 0.190424 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "12.249 5.82422 mo\n");
  (void) fprintf(fp, "12.4756 5.82422 ln\n");
  (void) fprintf(fp, "12.4756 146.145 ln\n");
  (void) fprintf(fp, "12.249 146.145 ln\n");
  (void) fprintf(fp, "12.249 5.82422 ln\n");
  (void) fprintf(fp, "0 0.120153 0.194291 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "12.4756 5.82422 mo\n");
  (void) fprintf(fp, "12.7021 5.82422 ln\n");
  (void) fprintf(fp, "12.7021 146.145 ln\n");
  (void) fprintf(fp, "12.4756 146.145 ln\n");
  (void) fprintf(fp, "12.4756 5.82422 ln\n");
  (void) fprintf(fp, "0 0.122547 0.198161 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "12.7021 5.82422 mo\n");
  (void) fprintf(fp, "12.9287 5.82422 ln\n");
  (void) fprintf(fp, "12.9287 146.145 ln\n");
  (void) fprintf(fp, "12.7021 146.145 ln\n");
  (void) fprintf(fp, "12.7021 5.82422 ln\n");
  (void) fprintf(fp, "0 0.124943 0.202036 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "12.9287 5.82422 mo\n");
  (void) fprintf(fp, "13.1553 5.82422 ln\n");
  (void) fprintf(fp, "13.1553 146.145 ln\n");
  (void) fprintf(fp, "12.9287 146.145 ln\n");
  (void) fprintf(fp, "12.9287 5.82422 ln\n");
  (void) fprintf(fp, "0 0.127342 0.205915 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "13.1553 5.82422 mo\n");
  (void) fprintf(fp, "13.3818 5.82422 ln\n");
  (void) fprintf(fp, "13.3818 146.145 ln\n");
  (void) fprintf(fp, "13.1553 146.145 ln\n");
  (void) fprintf(fp, "13.1553 5.82422 ln\n");
  (void) fprintf(fp, "0 0.129743 0.209798 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "13.3818 5.82422 mo\n");
  (void) fprintf(fp, "13.6084 5.82422 ln\n");
  (void) fprintf(fp, "13.6084 146.145 ln\n");
  (void) fprintf(fp, "13.3818 146.145 ln\n");
  (void) fprintf(fp, "13.3818 5.82422 ln\n");
  (void) fprintf(fp, "0 0.132147 0.213684 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "13.6084 5.82422 mo\n");
  (void) fprintf(fp, "13.835 5.82422 ln\n");
  (void) fprintf(fp, "13.835 146.145 ln\n");
  (void) fprintf(fp, "13.6084 146.145 ln\n");
  (void) fprintf(fp, "13.6084 5.82422 ln\n");
  (void) fprintf(fp, "0 0.134552 0.217574 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "13.835 5.82422 mo\n");
  (void) fprintf(fp, "14.0615 5.82422 ln\n");
  (void) fprintf(fp, "14.0615 146.145 ln\n");
  (void) fprintf(fp, "13.835 146.145 ln\n");
  (void) fprintf(fp, "13.835 5.82422 ln\n");
  (void) fprintf(fp, "0 0.13696 0.221468 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "14.0615 5.82422 mo\n");
  (void) fprintf(fp, "14.2881 5.82422 ln\n");
  (void) fprintf(fp, "14.2881 146.145 ln\n");
  (void) fprintf(fp, "14.0615 146.145 ln\n");
  (void) fprintf(fp, "14.0615 5.82422 ln\n");
  (void) fprintf(fp, "0 0.13937 0.225365 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "14.2881 5.82422 mo\n");
  (void) fprintf(fp, "14.5146 5.82422 ln\n");
  (void) fprintf(fp, "14.5146 146.145 ln\n");
  (void) fprintf(fp, "14.2881 146.145 ln\n");
  (void) fprintf(fp, "14.2881 5.82422 ln\n");
  (void) fprintf(fp, "0 0.141783 0.229266 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "14.5146 5.82422 mo\n");
  (void) fprintf(fp, "14.7412 5.82422 ln\n");
  (void) fprintf(fp, "14.7412 146.145 ln\n");
  (void) fprintf(fp, "14.5146 146.145 ln\n");
  (void) fprintf(fp, "14.5146 5.82422 ln\n");
  (void) fprintf(fp, "0 0.144197 0.23317 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "14.7412 5.82422 mo\n");
  (void) fprintf(fp, "14.9678 5.82422 ln\n");
  (void) fprintf(fp, "14.9678 146.145 ln\n");
  (void) fprintf(fp, "14.7412 146.145 ln\n");
  (void) fprintf(fp, "14.7412 5.82422 ln\n");
  (void) fprintf(fp, "0 0.146614 0.237078 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "14.9678 5.82422 mo\n");
  (void) fprintf(fp, "15.1943 5.82422 ln\n");
  (void) fprintf(fp, "15.1943 146.145 ln\n");
  (void) fprintf(fp, "14.9678 146.145 ln\n");
  (void) fprintf(fp, "14.9678 5.82422 ln\n");
  (void) fprintf(fp, "0 0.149033 0.240989 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "15.1943 5.82422 mo\n");
  (void) fprintf(fp, "15.4209 5.82422 ln\n");
  (void) fprintf(fp, "15.4209 146.145 ln\n");
  (void) fprintf(fp, "15.1943 146.145 ln\n");
  (void) fprintf(fp, "15.1943 5.82422 ln\n");
  (void) fprintf(fp, "0 0.151454 0.244904 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "15.4209 5.82422 mo\n");
  (void) fprintf(fp, "15.6475 5.82422 ln\n");
  (void) fprintf(fp, "15.6475 146.145 ln\n");
  (void) fprintf(fp, "15.4209 146.145 ln\n");
  (void) fprintf(fp, "15.4209 5.82422 ln\n");
  (void) fprintf(fp, "0 0.153876 0.248821 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "15.6475 5.82422 mo\n");
  (void) fprintf(fp, "15.874 5.82422 ln\n");
  (void) fprintf(fp, "15.874 146.145 ln\n");
  (void) fprintf(fp, "15.6475 146.145 ln\n");
  (void) fprintf(fp, "15.6475 5.82422 ln\n");
  (void) fprintf(fp, "0 0.156301 0.252742 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "15.874 5.82422 mo\n");
  (void) fprintf(fp, "16.1006 5.82422 ln\n");
  (void) fprintf(fp, "16.1006 146.145 ln\n");
  (void) fprintf(fp, "15.874 146.145 ln\n");
  (void) fprintf(fp, "15.874 5.82422 ln\n");
  (void) fprintf(fp, "0 0.158728 0.256667 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "16.1006 5.82422 mo\n");
  (void) fprintf(fp, "16.3271 5.82422 ln\n");
  (void) fprintf(fp, "16.3271 146.145 ln\n");
  (void) fprintf(fp, "16.1006 146.145 ln\n");
  (void) fprintf(fp, "16.1006 5.82422 ln\n");
  (void) fprintf(fp, "0 0.161157 0.260594 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "16.3271 5.82422 mo\n");
  (void) fprintf(fp, "16.5537 5.82422 ln\n");
  (void) fprintf(fp, "16.5537 146.145 ln\n");
  (void) fprintf(fp, "16.3271 146.145 ln\n");
  (void) fprintf(fp, "16.3271 5.82422 ln\n");
  (void) fprintf(fp, "0 0.163587 0.264524 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "16.5537 5.82422 mo\n");
  (void) fprintf(fp, "16.7764 5.82422 ln\n");
  (void) fprintf(fp, "16.7764 146.145 ln\n");
  (void) fprintf(fp, "16.5537 146.145 ln\n");
  (void) fprintf(fp, "16.5537 5.82422 ln\n");
  (void) fprintf(fp, "0 0.16602 0.268458 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "16.7764 5.82422 mo\n");
  (void) fprintf(fp, "17.0029 5.82422 ln\n");
  (void) fprintf(fp, "17.0029 146.145 ln\n");
  (void) fprintf(fp, "16.7764 146.145 ln\n");
  (void) fprintf(fp, "16.7764 5.82422 ln\n");
  (void) fprintf(fp, "0 0.168454 0.272394 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "17.0029 5.82422 mo\n");
  (void) fprintf(fp, "17.2295 5.82422 ln\n");
  (void) fprintf(fp, "17.2295 146.145 ln\n");
  (void) fprintf(fp, "17.0029 146.145 ln\n");
  (void) fprintf(fp, "17.0029 5.82422 ln\n");
  (void) fprintf(fp, "0 0.170891 0.276334 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "17.2295 5.82422 mo\n");
  (void) fprintf(fp, "17.4561 5.82422 ln\n");
  (void) fprintf(fp, "17.4561 146.145 ln\n");
  (void) fprintf(fp, "17.2295 146.145 ln\n");
  (void) fprintf(fp, "17.2295 5.82422 ln\n");
  (void) fprintf(fp, "0 0.173329 0.280277 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "17.4561 5.82422 mo\n");
  (void) fprintf(fp, "17.6826 5.82422 ln\n");
  (void) fprintf(fp, "17.6826 146.145 ln\n");
  (void) fprintf(fp, "17.4561 146.145 ln\n");
  (void) fprintf(fp, "17.4561 5.82422 ln\n");
  (void) fprintf(fp, "0 0.175769 0.284222 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "17.6826 5.82422 mo\n");
  (void) fprintf(fp, "17.9092 5.82422 ln\n");
  (void) fprintf(fp, "17.9092 146.145 ln\n");
  (void) fprintf(fp, "17.6826 146.145 ln\n");
  (void) fprintf(fp, "17.6826 5.82422 ln\n");
  (void) fprintf(fp, "0 0.17821 0.28817 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "17.9092 5.82422 mo\n");
  (void) fprintf(fp, "18.1357 5.82422 ln\n");
  (void) fprintf(fp, "18.1357 146.145 ln\n");
  (void) fprintf(fp, "17.9092 146.145 ln\n");
  (void) fprintf(fp, "17.9092 5.82422 ln\n");
  (void) fprintf(fp, "0 0.180654 0.292121 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "18.1357 5.82422 mo\n");
  (void) fprintf(fp, "18.3623 5.82422 ln\n");
  (void) fprintf(fp, "18.3623 146.145 ln\n");
  (void) fprintf(fp, "18.1357 146.145 ln\n");
  (void) fprintf(fp, "18.1357 5.82422 ln\n");
  (void) fprintf(fp, "0 0.183099 0.296075 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "18.3623 5.82422 mo\n");
  (void) fprintf(fp, "18.5889 5.82422 ln\n");
  (void) fprintf(fp, "18.5889 146.145 ln\n");
  (void) fprintf(fp, "18.3623 146.145 ln\n");
  (void) fprintf(fp, "18.3623 5.82422 ln\n");
  (void) fprintf(fp, "0 0.185546 0.300032 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "18.5889 5.82422 mo\n");
  (void) fprintf(fp, "18.8154 5.82422 ln\n");
  (void) fprintf(fp, "18.8154 146.145 ln\n");
  (void) fprintf(fp, "18.5889 146.145 ln\n");
  (void) fprintf(fp, "18.5889 5.82422 ln\n");
  (void) fprintf(fp, "0 0.187995 0.303991 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "18.8154 5.82422 mo\n");
  (void) fprintf(fp, "19.042 5.82422 ln\n");
  (void) fprintf(fp, "19.042 146.145 ln\n");
  (void) fprintf(fp, "18.8154 146.145 ln\n");
  (void) fprintf(fp, "18.8154 5.82422 ln\n");
  (void) fprintf(fp, "0 0.190445 0.307953 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "19.042 5.82422 mo\n");
  (void) fprintf(fp, "19.2686 5.82422 ln\n");
  (void) fprintf(fp, "19.2686 146.145 ln\n");
  (void) fprintf(fp, "19.042 146.145 ln\n");
  (void) fprintf(fp, "19.042 5.82422 ln\n");
  (void) fprintf(fp, "0 0.192897 0.311918 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "19.2686 5.82422 mo\n");
  (void) fprintf(fp, "19.4951 5.82422 ln\n");
  (void) fprintf(fp, "19.4951 146.145 ln\n");
  (void) fprintf(fp, "19.2686 146.145 ln\n");
  (void) fprintf(fp, "19.2686 5.82422 ln\n");
  (void) fprintf(fp, "0 0.19535 0.315886 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "19.4951 5.82422 mo\n");
  (void) fprintf(fp, "19.7217 5.82422 ln\n");
  (void) fprintf(fp, "19.7217 146.145 ln\n");
  (void) fprintf(fp, "19.4951 146.145 ln\n");
  (void) fprintf(fp, "19.4951 5.82422 ln\n");
  (void) fprintf(fp, "0 0.197806 0.319856 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "19.7217 5.82422 mo\n");
  (void) fprintf(fp, "19.9482 5.82422 ln\n");
  (void) fprintf(fp, "19.9482 146.145 ln\n");
  (void) fprintf(fp, "19.7217 146.145 ln\n");
  (void) fprintf(fp, "19.7217 5.82422 ln\n");
  (void) fprintf(fp, "0 0.200262 0.323829 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "19.9482 5.82422 mo\n");
  (void) fprintf(fp, "20.1748 5.82422 ln\n");
  (void) fprintf(fp, "20.1748 146.145 ln\n");
  (void) fprintf(fp, "19.9482 146.145 ln\n");
  (void) fprintf(fp, "19.9482 5.82422 ln\n");
  (void) fprintf(fp, "0 0.202721 0.327804 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "20.1748 5.82422 mo\n");
  (void) fprintf(fp, "20.4014 5.82422 ln\n");
  (void) fprintf(fp, "20.4014 146.145 ln\n");
  (void) fprintf(fp, "20.1748 146.145 ln\n");
  (void) fprintf(fp, "20.1748 5.82422 ln\n");
  (void) fprintf(fp, "0 0.205181 0.331782 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "20.4014 5.82422 mo\n");
  (void) fprintf(fp, "20.6279 5.82422 ln\n");
  (void) fprintf(fp, "20.6279 146.145 ln\n");
  (void) fprintf(fp, "20.4014 146.145 ln\n");
  (void) fprintf(fp, "20.4014 5.82422 ln\n");
  (void) fprintf(fp, "0 0.207642 0.335762 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "20.6279 5.82422 mo\n");
  (void) fprintf(fp, "20.8545 5.82422 ln\n");
  (void) fprintf(fp, "20.8545 146.145 ln\n");
  (void) fprintf(fp, "20.6279 146.145 ln\n");
  (void) fprintf(fp, "20.6279 5.82422 ln\n");
  (void) fprintf(fp, "0 0.210105 0.339745 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "20.8545 5.82422 mo\n");
  (void) fprintf(fp, "21.0811 5.82422 ln\n");
  (void) fprintf(fp, "21.0811 146.145 ln\n");
  (void) fprintf(fp, "20.8545 146.145 ln\n");
  (void) fprintf(fp, "20.8545 5.82422 ln\n");
  (void) fprintf(fp, "0 0.21257 0.34373 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "21.0811 5.82422 mo\n");
  (void) fprintf(fp, "21.3076 5.82422 ln\n");
  (void) fprintf(fp, "21.3076 146.145 ln\n");
  (void) fprintf(fp, "21.0811 146.145 ln\n");
  (void) fprintf(fp, "21.0811 5.82422 ln\n");
  (void) fprintf(fp, "0 0.215036 0.347717 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "21.3076 5.82422 mo\n");
  (void) fprintf(fp, "21.5342 5.82422 ln\n");
  (void) fprintf(fp, "21.5342 146.145 ln\n");
  (void) fprintf(fp, "21.3076 146.145 ln\n");
  (void) fprintf(fp, "21.3076 5.82422 ln\n");
  (void) fprintf(fp, "0 0.217503 0.351707 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "21.5342 5.82422 mo\n");
  (void) fprintf(fp, "21.7607 5.82422 ln\n");
  (void) fprintf(fp, "21.7607 146.145 ln\n");
  (void) fprintf(fp, "21.5342 146.145 ln\n");
  (void) fprintf(fp, "21.5342 5.82422 ln\n");
  (void) fprintf(fp, "0 0.219972 0.3557 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "21.7607 5.82422 mo\n");
  (void) fprintf(fp, "21.9873 5.82422 ln\n");
  (void) fprintf(fp, "21.9873 146.145 ln\n");
  (void) fprintf(fp, "21.7607 146.145 ln\n");
  (void) fprintf(fp, "21.7607 5.82422 ln\n");
  (void) fprintf(fp, "0 0.222443 0.359695 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "21.9873 5.82422 mo\n");
  (void) fprintf(fp, "22.2139 5.82422 ln\n");
  (void) fprintf(fp, "22.2139 146.145 ln\n");
  (void) fprintf(fp, "21.9873 146.145 ln\n");
  (void) fprintf(fp, "21.9873 5.82422 ln\n");
  (void) fprintf(fp, "0 0.224915 0.363692 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "22.2139 5.82422 mo\n");
  (void) fprintf(fp, "22.4404 5.82422 ln\n");
  (void) fprintf(fp, "22.4404 146.145 ln\n");
  (void) fprintf(fp, "22.2139 146.145 ln\n");
  (void) fprintf(fp, "22.2139 5.82422 ln\n");
  (void) fprintf(fp, "0 0.227388 0.367691 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "22.4404 5.82422 mo\n");
  (void) fprintf(fp, "22.667 5.82422 ln\n");
  (void) fprintf(fp, "22.667 146.145 ln\n");
  (void) fprintf(fp, "22.4404 146.145 ln\n");
  (void) fprintf(fp, "22.4404 5.82422 ln\n");
  (void) fprintf(fp, "0 0.229863 0.371693 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "22.667 5.82422 mo\n");
  (void) fprintf(fp, "22.8936 5.82422 ln\n");
  (void) fprintf(fp, "22.8936 146.145 ln\n");
  (void) fprintf(fp, "22.667 146.145 ln\n");
  (void) fprintf(fp, "22.667 5.82422 ln\n");
  (void) fprintf(fp, "0 0.232339 0.375697 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "22.8936 5.82422 mo\n");
  (void) fprintf(fp, "23.1201 5.82422 ln\n");
  (void) fprintf(fp, "23.1201 146.145 ln\n");
  (void) fprintf(fp, "22.8936 146.145 ln\n");
  (void) fprintf(fp, "22.8936 5.82422 ln\n");
  (void) fprintf(fp, "0 0.234816 0.379703 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "23.1201 5.82422 mo\n");
  (void) fprintf(fp, "23.3467 5.82422 ln\n");
  (void) fprintf(fp, "23.3467 146.145 ln\n");
  (void) fprintf(fp, "23.1201 146.145 ln\n");
  (void) fprintf(fp, "23.1201 5.82422 ln\n");
  (void) fprintf(fp, "0 0.237295 0.383711 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "23.3467 5.82422 mo\n");
  (void) fprintf(fp, "23.5732 5.82422 ln\n");
  (void) fprintf(fp, "23.5732 146.145 ln\n");
  (void) fprintf(fp, "23.3467 146.145 ln\n");
  (void) fprintf(fp, "23.3467 5.82422 ln\n");
  (void) fprintf(fp, "0 0.239775 0.387722 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "23.5732 5.82422 mo\n");
  (void) fprintf(fp, "23.7998 5.82422 ln\n");
  (void) fprintf(fp, "23.7998 146.145 ln\n");
  (void) fprintf(fp, "23.5732 146.145 ln\n");
  (void) fprintf(fp, "23.5732 5.82422 ln\n");
  (void) fprintf(fp, "0 0.242257 0.391735 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "23.7998 5.82422 mo\n");
  (void) fprintf(fp, "24.0264 5.82422 ln\n");
  (void) fprintf(fp, "24.0264 146.145 ln\n");
  (void) fprintf(fp, "23.7998 146.145 ln\n");
  (void) fprintf(fp, "23.7998 5.82422 ln\n");
  (void) fprintf(fp, "0 0.24474 0.395749 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "24.0264 5.82422 mo\n");
  (void) fprintf(fp, "24.2529 5.82422 ln\n");
  (void) fprintf(fp, "24.2529 146.145 ln\n");
  (void) fprintf(fp, "24.0264 146.145 ln\n");
  (void) fprintf(fp, "24.0264 5.82422 ln\n");
  (void) fprintf(fp, "0 0.247224 0.399767 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "24.2529 5.82422 mo\n");
  (void) fprintf(fp, "24.4795 5.82422 ln\n");
  (void) fprintf(fp, "24.4795 146.145 ln\n");
  (void) fprintf(fp, "24.2529 146.145 ln\n");
  (void) fprintf(fp, "24.2529 5.82422 ln\n");
  (void) fprintf(fp, "0 0.24971 0.403786 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "24.4795 5.82422 mo\n");
  (void) fprintf(fp, "24.7061 5.82422 ln\n");
  (void) fprintf(fp, "24.7061 146.145 ln\n");
  (void) fprintf(fp, "24.4795 146.145 ln\n");
  (void) fprintf(fp, "24.4795 5.82422 ln\n");
  (void) fprintf(fp, "0 0.252196 0.407807 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "24.7061 5.82422 mo\n");
  (void) fprintf(fp, "24.9326 5.82422 ln\n");
  (void) fprintf(fp, "24.9326 146.145 ln\n");
  (void) fprintf(fp, "24.7061 146.145 ln\n");
  (void) fprintf(fp, "24.7061 5.82422 ln\n");
  (void) fprintf(fp, "0 0.254685 0.41183 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "24.9326 5.82422 mo\n");
  (void) fprintf(fp, "25.1592 5.82422 ln\n");
  (void) fprintf(fp, "25.1592 146.145 ln\n");
  (void) fprintf(fp, "24.9326 146.145 ln\n");
  (void) fprintf(fp, "24.9326 5.82422 ln\n");
  (void) fprintf(fp, "0 0.257174 0.415856 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "25.1592 5.82422 mo\n");
  (void) fprintf(fp, "25.3818 5.82422 ln\n");
  (void) fprintf(fp, "25.3818 146.145 ln\n");
  (void) fprintf(fp, "25.1592 146.145 ln\n");
  (void) fprintf(fp, "25.1592 5.82422 ln\n");
  (void) fprintf(fp, "0 0.259665 0.419883 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "25.3818 5.82422 mo\n");
  (void) fprintf(fp, "25.6084 5.82422 ln\n");
  (void) fprintf(fp, "25.6084 146.145 ln\n");
  (void) fprintf(fp, "25.3818 146.145 ln\n");
  (void) fprintf(fp, "25.3818 5.82422 ln\n");
  (void) fprintf(fp, "0 0.262157 0.423913 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "25.6084 5.82422 mo\n");
  (void) fprintf(fp, "25.835 5.82422 ln\n");
  (void) fprintf(fp, "25.835 146.145 ln\n");
  (void) fprintf(fp, "25.6084 146.145 ln\n");
  (void) fprintf(fp, "25.6084 5.82422 ln\n");
  (void) fprintf(fp, "0 0.26465 0.427944 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "25.835 5.82422 mo\n");
  (void) fprintf(fp, "26.0615 5.82422 ln\n");
  (void) fprintf(fp, "26.0615 146.145 ln\n");
  (void) fprintf(fp, "25.835 146.145 ln\n");
  (void) fprintf(fp, "25.835 5.82422 ln\n");
  (void) fprintf(fp, "0 0.267144 0.431978 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "26.0615 5.82422 mo\n");
  (void) fprintf(fp, "26.2881 5.82422 ln\n");
  (void) fprintf(fp, "26.2881 146.145 ln\n");
  (void) fprintf(fp, "26.0615 146.145 ln\n");
  (void) fprintf(fp, "26.0615 5.82422 ln\n");
  (void) fprintf(fp, "0 0.26964 0.436013 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "26.2881 5.82422 mo\n");
  (void) fprintf(fp, "26.5146 5.82422 ln\n");
  (void) fprintf(fp, "26.5146 146.145 ln\n");
  (void) fprintf(fp, "26.2881 146.145 ln\n");
  (void) fprintf(fp, "26.2881 5.82422 ln\n");
  (void) fprintf(fp, "0 0.272137 0.440051 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "26.5146 5.82422 mo\n");
  (void) fprintf(fp, "26.7412 5.82422 ln\n");
  (void) fprintf(fp, "26.7412 146.145 ln\n");
  (void) fprintf(fp, "26.5146 146.145 ln\n");
  (void) fprintf(fp, "26.5146 5.82422 ln\n");
  (void) fprintf(fp, "0 0.274635 0.44409 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "26.7412 5.82422 mo\n");
  (void) fprintf(fp, "26.9678 5.82422 ln\n");
  (void) fprintf(fp, "26.9678 146.145 ln\n");
  (void) fprintf(fp, "26.7412 146.145 ln\n");
  (void) fprintf(fp, "26.7412 5.82422 ln\n");
  (void) fprintf(fp, "0 0.277134 0.448131 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "26.9678 5.82422 mo\n");
  (void) fprintf(fp, "27.1943 5.82422 ln\n");
  (void) fprintf(fp, "27.1943 146.145 ln\n");
  (void) fprintf(fp, "26.9678 146.145 ln\n");
  (void) fprintf(fp, "26.9678 5.82422 ln\n");
  (void) fprintf(fp, "0 0.279634 0.452175 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "27.1943 5.82422 mo\n");
  (void) fprintf(fp, "27.4209 5.82422 ln\n");
  (void) fprintf(fp, "27.4209 146.145 ln\n");
  (void) fprintf(fp, "27.1943 146.145 ln\n");
  (void) fprintf(fp, "27.1943 5.82422 ln\n");
  (void) fprintf(fp, "0 0.282136 0.45622 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "27.4209 5.82422 mo\n");
  (void) fprintf(fp, "27.6475 5.82422 ln\n");
  (void) fprintf(fp, "27.6475 146.145 ln\n");
  (void) fprintf(fp, "27.4209 146.145 ln\n");
  (void) fprintf(fp, "27.4209 5.82422 ln\n");
  (void) fprintf(fp, "0 0.284639 0.460267 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "27.6475 5.82422 mo\n");
  (void) fprintf(fp, "27.874 5.82422 ln\n");
  (void) fprintf(fp, "27.874 146.145 ln\n");
  (void) fprintf(fp, "27.6475 146.145 ln\n");
  (void) fprintf(fp, "27.6475 5.82422 ln\n");
  (void) fprintf(fp, "0 0.287143 0.464316 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "27.874 5.82422 mo\n");
  (void) fprintf(fp, "28.1006 5.82422 ln\n");
  (void) fprintf(fp, "28.1006 146.145 ln\n");
  (void) fprintf(fp, "27.874 146.145 ln\n");
  (void) fprintf(fp, "27.874 5.82422 ln\n");
  (void) fprintf(fp, "0 0.289648 0.468366 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "28.1006 5.82422 mo\n");
  (void) fprintf(fp, "28.3271 5.82422 ln\n");
  (void) fprintf(fp, "28.3271 146.145 ln\n");
  (void) fprintf(fp, "28.1006 146.145 ln\n");
  (void) fprintf(fp, "28.1006 5.82422 ln\n");
  (void) fprintf(fp, "0 0.292154 0.472419 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "28.3271 5.82422 mo\n");
  (void) fprintf(fp, "28.5537 5.82422 ln\n");
  (void) fprintf(fp, "28.5537 146.145 ln\n");
  (void) fprintf(fp, "28.3271 146.145 ln\n");
  (void) fprintf(fp, "28.3271 5.82422 ln\n");
  (void) fprintf(fp, "0 0.294661 0.476473 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "28.5537 5.82422 mo\n");
  (void) fprintf(fp, "28.7803 5.82422 ln\n");
  (void) fprintf(fp, "28.7803 146.145 ln\n");
  (void) fprintf(fp, "28.5537 146.145 ln\n");
  (void) fprintf(fp, "28.5537 5.82422 ln\n");
  (void) fprintf(fp, "0 0.29717 0.48053 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "28.7803 5.82422 mo\n");
  (void) fprintf(fp, "29.0068 5.82422 ln\n");
  (void) fprintf(fp, "29.0068 146.145 ln\n");
  (void) fprintf(fp, "28.7803 146.145 ln\n");
  (void) fprintf(fp, "28.7803 5.82422 ln\n");
  (void) fprintf(fp, "0 0.299679 0.484588 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "29.0068 5.82422 mo\n");
  (void) fprintf(fp, "29.2334 5.82422 ln\n");
  (void) fprintf(fp, "29.2334 146.145 ln\n");
  (void) fprintf(fp, "29.0068 146.145 ln\n");
  (void) fprintf(fp, "29.0068 5.82422 ln\n");
  (void) fprintf(fp, "0 0.30219 0.488647 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "29.2334 5.82422 mo\n");
  (void) fprintf(fp, "29.46 5.82422 ln\n");
  (void) fprintf(fp, "29.46 146.145 ln\n");
  (void) fprintf(fp, "29.2334 146.145 ln\n");
  (void) fprintf(fp, "29.2334 5.82422 ln\n");
  (void) fprintf(fp, "0 0.304702 0.492709 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "29.46 5.82422 mo\n");
  (void) fprintf(fp, "29.6865 5.82422 ln\n");
  (void) fprintf(fp, "29.6865 146.145 ln\n");
  (void) fprintf(fp, "29.46 146.145 ln\n");
  (void) fprintf(fp, "29.46 5.82422 ln\n");
  (void) fprintf(fp, "0 0.307214 0.496772 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "29.6865 5.82422 mo\n");
  (void) fprintf(fp, "29.9131 5.82422 ln\n");
  (void) fprintf(fp, "29.9131 146.145 ln\n");
  (void) fprintf(fp, "29.6865 146.145 ln\n");
  (void) fprintf(fp, "29.6865 5.82422 ln\n");
  (void) fprintf(fp, "0 0.309728 0.500837 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "29.9131 5.82422 mo\n");
  (void) fprintf(fp, "30.1396 5.82422 ln\n");
  (void) fprintf(fp, "30.1396 146.145 ln\n");
  (void) fprintf(fp, "29.9131 146.145 ln\n");
  (void) fprintf(fp, "29.9131 5.82422 ln\n");
  (void) fprintf(fp, "0 0.312243 0.504904 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "30.1396 5.82422 mo\n");
  (void) fprintf(fp, "30.3662 5.82422 ln\n");
  (void) fprintf(fp, "30.3662 146.145 ln\n");
  (void) fprintf(fp, "30.1396 146.145 ln\n");
  (void) fprintf(fp, "30.1396 5.82422 ln\n");
  (void) fprintf(fp, "0 0.314759 0.508973 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "30.3662 5.82422 mo\n");
  (void) fprintf(fp, "30.5889 5.82422 ln\n");
  (void) fprintf(fp, "30.5889 146.145 ln\n");
  (void) fprintf(fp, "30.3662 146.145 ln\n");
  (void) fprintf(fp, "30.3662 5.82422 ln\n");
  (void) fprintf(fp, "0 0.317276 0.513043 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "30.5889 5.82422 mo\n");
  (void) fprintf(fp, "30.8154 5.82422 ln\n");
  (void) fprintf(fp, "30.8154 146.145 ln\n");
  (void) fprintf(fp, "30.5889 146.145 ln\n");
  (void) fprintf(fp, "30.5889 5.82422 ln\n");
  (void) fprintf(fp, "0 0.319795 0.517115 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "30.8154 5.82422 mo\n");
  (void) fprintf(fp, "31.042 5.82422 ln\n");
  (void) fprintf(fp, "31.042 146.145 ln\n");
  (void) fprintf(fp, "30.8154 146.145 ln\n");
  (void) fprintf(fp, "30.8154 5.82422 ln\n");
  (void) fprintf(fp, "0 0.322314 0.521188 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "31.042 5.82422 mo\n");
  (void) fprintf(fp, "31.2686 5.82422 ln\n");
  (void) fprintf(fp, "31.2686 146.145 ln\n");
  (void) fprintf(fp, "31.042 146.145 ln\n");
  (void) fprintf(fp, "31.042 5.82422 ln\n");
  (void) fprintf(fp, "0 0.324834 0.525264 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "31.2686 5.82422 mo\n");
  (void) fprintf(fp, "31.4951 5.82422 ln\n");
  (void) fprintf(fp, "31.4951 146.145 ln\n");
  (void) fprintf(fp, "31.2686 146.145 ln\n");
  (void) fprintf(fp, "31.2686 5.82422 ln\n");
  (void) fprintf(fp, "0 0.327355 0.529341 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "31.4951 5.82422 mo\n");
  (void) fprintf(fp, "31.7217 5.82422 ln\n");
  (void) fprintf(fp, "31.7217 146.145 ln\n");
  (void) fprintf(fp, "31.4951 146.145 ln\n");
  (void) fprintf(fp, "31.4951 5.82422 ln\n");
  (void) fprintf(fp, "0 0.329878 0.533419 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "31.7217 5.82422 mo\n");
  (void) fprintf(fp, "31.9482 5.82422 ln\n");
  (void) fprintf(fp, "31.9482 146.145 ln\n");
  (void) fprintf(fp, "31.7217 146.145 ln\n");
  (void) fprintf(fp, "31.7217 5.82422 ln\n");
  (void) fprintf(fp, "0 0.332401 0.537499 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "31.9482 5.82422 mo\n");
  (void) fprintf(fp, "32.1748 5.82422 ln\n");
  (void) fprintf(fp, "32.1748 146.145 ln\n");
  (void) fprintf(fp, "31.9482 146.145 ln\n");
  (void) fprintf(fp, "31.9482 5.82422 ln\n");
  (void) fprintf(fp, "0 0.334925 0.541581 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "32.1748 5.82422 mo\n");
  (void) fprintf(fp, "32.4014 5.82422 ln\n");
  (void) fprintf(fp, "32.4014 146.145 ln\n");
  (void) fprintf(fp, "32.1748 146.145 ln\n");
  (void) fprintf(fp, "32.1748 5.82422 ln\n");
  (void) fprintf(fp, "0 0.337451 0.545665 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "32.4014 5.82422 mo\n");
  (void) fprintf(fp, "32.6279 5.82422 ln\n");
  (void) fprintf(fp, "32.6279 146.145 ln\n");
  (void) fprintf(fp, "32.4014 146.145 ln\n");
  (void) fprintf(fp, "32.4014 5.82422 ln\n");
  (void) fprintf(fp, "0 0.339977 0.54975 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "32.6279 5.82422 mo\n");
  (void) fprintf(fp, "32.8545 5.82422 ln\n");
  (void) fprintf(fp, "32.8545 146.145 ln\n");
  (void) fprintf(fp, "32.6279 146.145 ln\n");
  (void) fprintf(fp, "32.6279 5.82422 ln\n");
  (void) fprintf(fp, "0 0.342504 0.553836 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "32.8545 5.82422 mo\n");
  (void) fprintf(fp, "33.0811 5.82422 ln\n");
  (void) fprintf(fp, "33.0811 146.145 ln\n");
  (void) fprintf(fp, "32.8545 146.145 ln\n");
  (void) fprintf(fp, "32.8545 5.82422 ln\n");
  (void) fprintf(fp, "0 0.345032 0.557925 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "33.0811 5.82422 mo\n");
  (void) fprintf(fp, "33.3076 5.82422 ln\n");
  (void) fprintf(fp, "33.3076 146.145 ln\n");
  (void) fprintf(fp, "33.0811 146.145 ln\n");
  (void) fprintf(fp, "33.0811 5.82422 ln\n");
  (void) fprintf(fp, "0 0.347562 0.562014 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "33.3076 5.82422 mo\n");
  (void) fprintf(fp, "33.5342 5.82422 ln\n");
  (void) fprintf(fp, "33.5342 146.145 ln\n");
  (void) fprintf(fp, "33.3076 146.145 ln\n");
  (void) fprintf(fp, "33.3076 5.82422 ln\n");
  (void) fprintf(fp, "0 0.350092 0.566106 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "33.5342 5.82422 mo\n");
  (void) fprintf(fp, "33.7607 5.82422 ln\n");
  (void) fprintf(fp, "33.7607 146.145 ln\n");
  (void) fprintf(fp, "33.5342 146.145 ln\n");
  (void) fprintf(fp, "33.5342 5.82422 ln\n");
  (void) fprintf(fp, "0 0.352623 0.570199 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "33.7607 5.82422 mo\n");
  (void) fprintf(fp, "33.9873 5.82422 ln\n");
  (void) fprintf(fp, "33.9873 146.145 ln\n");
  (void) fprintf(fp, "33.7607 146.145 ln\n");
  (void) fprintf(fp, "33.7607 5.82422 ln\n");
  (void) fprintf(fp, "0 0.355155 0.574293 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "33.9873 5.82422 mo\n");
  (void) fprintf(fp, "34.2139 5.82422 ln\n");
  (void) fprintf(fp, "34.2139 146.145 ln\n");
  (void) fprintf(fp, "33.9873 146.145 ln\n");
  (void) fprintf(fp, "33.9873 5.82422 ln\n");
  (void) fprintf(fp, "0 0.357688 0.578389 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "34.2139 5.82422 mo\n");
  (void) fprintf(fp, "34.4404 5.82422 ln\n");
  (void) fprintf(fp, "34.4404 146.145 ln\n");
  (void) fprintf(fp, "34.2139 146.145 ln\n");
  (void) fprintf(fp, "34.2139 5.82422 ln\n");
  (void) fprintf(fp, "0 0.360222 0.582487 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "34.4404 5.82422 mo\n");
  (void) fprintf(fp, "34.667 5.82422 ln\n");
  (void) fprintf(fp, "34.667 146.145 ln\n");
  (void) fprintf(fp, "34.4404 146.145 ln\n");
  (void) fprintf(fp, "34.4404 5.82422 ln\n");
  (void) fprintf(fp, "0 0.362757 0.586586 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "34.667 5.82422 mo\n");
  (void) fprintf(fp, "34.8936 5.82422 ln\n");
  (void) fprintf(fp, "34.8936 146.145 ln\n");
  (void) fprintf(fp, "34.667 146.145 ln\n");
  (void) fprintf(fp, "34.667 5.82422 ln\n");
  (void) fprintf(fp, "0 0.365293 0.590686 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "34.8936 5.82422 mo\n");
  (void) fprintf(fp, "35.1201 5.82422 ln\n");
  (void) fprintf(fp, "35.1201 146.145 ln\n");
  (void) fprintf(fp, "34.8936 146.145 ln\n");
  (void) fprintf(fp, "34.8936 5.82422 ln\n");
  (void) fprintf(fp, "0 0.36783 0.594788 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "35.1201 5.82422 mo\n");
  (void) fprintf(fp, "35.3467 5.82422 ln\n");
  (void) fprintf(fp, "35.3467 146.145 ln\n");
  (void) fprintf(fp, "35.1201 146.145 ln\n");
  (void) fprintf(fp, "35.1201 5.82422 ln\n");
  (void) fprintf(fp, "0 0.370367 0.598892 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "35.3467 5.82422 mo\n");
  (void) fprintf(fp, "35.5732 5.82422 ln\n");
  (void) fprintf(fp, "35.5732 146.145 ln\n");
  (void) fprintf(fp, "35.3467 146.145 ln\n");
  (void) fprintf(fp, "35.3467 5.82422 ln\n");
  (void) fprintf(fp, "0 0.372906 0.602997 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "35.5732 5.82422 mo\n");
  (void) fprintf(fp, "35.7998 5.82422 ln\n");
  (void) fprintf(fp, "35.7998 146.145 ln\n");
  (void) fprintf(fp, "35.5732 146.145 ln\n");
  (void) fprintf(fp, "35.5732 5.82422 ln\n");
  (void) fprintf(fp, "0 0.375445 0.607103 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "35.7998 5.82422 mo\n");
  (void) fprintf(fp, "36.0264 5.82422 ln\n");
  (void) fprintf(fp, "36.0264 146.145 ln\n");
  (void) fprintf(fp, "35.7998 146.145 ln\n");
  (void) fprintf(fp, "35.7998 5.82422 ln\n");
  (void) fprintf(fp, "0 0.377986 0.611211 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "36.0264 5.82422 mo\n");
  (void) fprintf(fp, "36.2529 5.82422 ln\n");
  (void) fprintf(fp, "36.2529 146.145 ln\n");
  (void) fprintf(fp, "36.0264 146.145 ln\n");
  (void) fprintf(fp, "36.0264 5.82422 ln\n");
  (void) fprintf(fp, "0 0.380527 0.61532 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "36.2529 5.82422 mo\n");
  (void) fprintf(fp, "36.4795 5.82422 ln\n");
  (void) fprintf(fp, "36.4795 146.145 ln\n");
  (void) fprintf(fp, "36.2529 146.145 ln\n");
  (void) fprintf(fp, "36.2529 5.82422 ln\n");
  (void) fprintf(fp, "0 0.383069 0.619431 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "36.4795 5.82422 mo\n");
  (void) fprintf(fp, "36.7061 5.82422 ln\n");
  (void) fprintf(fp, "36.7061 146.145 ln\n");
  (void) fprintf(fp, "36.4795 146.145 ln\n");
  (void) fprintf(fp, "36.4795 5.82422 ln\n");
  (void) fprintf(fp, "0 0.385612 0.623543 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "36.7061 5.82422 mo\n");
  (void) fprintf(fp, "36.9326 5.82422 ln\n");
  (void) fprintf(fp, "36.9326 146.145 ln\n");
  (void) fprintf(fp, "36.7061 146.145 ln\n");
  (void) fprintf(fp, "36.7061 5.82422 ln\n");
  (void) fprintf(fp, "0 0.388156 0.627657 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "36.9326 5.82422 mo\n");
  (void) fprintf(fp, "37.1592 5.82422 ln\n");
  (void) fprintf(fp, "37.1592 146.145 ln\n");
  (void) fprintf(fp, "36.9326 146.145 ln\n");
  (void) fprintf(fp, "36.9326 5.82422 ln\n");
  (void) fprintf(fp, "0 0.390701 0.631772 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "37.1592 5.82422 mo\n");
  (void) fprintf(fp, "37.3857 5.82422 ln\n");
  (void) fprintf(fp, "37.3857 146.145 ln\n");
  (void) fprintf(fp, "37.1592 146.145 ln\n");
  (void) fprintf(fp, "37.1592 5.82422 ln\n");
  (void) fprintf(fp, "0 0.393247 0.635889 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "37.3857 5.82422 mo\n");
  (void) fprintf(fp, "37.6123 5.82422 ln\n");
  (void) fprintf(fp, "37.6123 146.145 ln\n");
  (void) fprintf(fp, "37.3857 146.145 ln\n");
  (void) fprintf(fp, "37.3857 5.82422 ln\n");
  (void) fprintf(fp, "0 0.395794 0.640007 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "37.6123 5.82422 mo\n");
  (void) fprintf(fp, "37.8389 5.82422 ln\n");
  (void) fprintf(fp, "37.8389 146.145 ln\n");
  (void) fprintf(fp, "37.6123 146.145 ln\n");
  (void) fprintf(fp, "37.6123 5.82422 ln\n");
  (void) fprintf(fp, "0 0.398341 0.644126 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "37.8389 5.82422 mo\n");
  (void) fprintf(fp, "38.0654 5.82422 ln\n");
  (void) fprintf(fp, "38.0654 146.145 ln\n");
  (void) fprintf(fp, "37.8389 146.145 ln\n");
  (void) fprintf(fp, "37.8389 5.82422 ln\n");
  (void) fprintf(fp, "0 0.400889 0.648246 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "38.0654 5.82422 mo\n");
  (void) fprintf(fp, "38.292 5.82422 ln\n");
  (void) fprintf(fp, "38.292 146.145 ln\n");
  (void) fprintf(fp, "38.0654 146.145 ln\n");
  (void) fprintf(fp, "38.0654 5.82422 ln\n");
  (void) fprintf(fp, "0 0.403438 0.652369 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "38.292 5.82422 mo\n");
  (void) fprintf(fp, "38.5186 5.82422 ln\n");
  (void) fprintf(fp, "38.5186 146.145 ln\n");
  (void) fprintf(fp, "38.292 146.145 ln\n");
  (void) fprintf(fp, "38.292 5.82422 ln\n");
  (void) fprintf(fp, "0 0.405988 0.656492 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "38.5186 5.82422 mo\n");
  (void) fprintf(fp, "38.7451 5.82422 ln\n");
  (void) fprintf(fp, "38.7451 146.145 ln\n");
  (void) fprintf(fp, "38.5186 146.145 ln\n");
  (void) fprintf(fp, "38.5186 5.82422 ln\n");
  (void) fprintf(fp, "0 0.408539 0.660617 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "38.7451 5.82422 mo\n");
  (void) fprintf(fp, "38.9717 5.82422 ln\n");
  (void) fprintf(fp, "38.9717 146.145 ln\n");
  (void) fprintf(fp, "38.7451 146.145 ln\n");
  (void) fprintf(fp, "38.7451 5.82422 ln\n");
  (void) fprintf(fp, "0 0.411091 0.664743 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "38.9717 5.82422 mo\n");
  (void) fprintf(fp, "39.1982 5.82422 ln\n");
  (void) fprintf(fp, "39.1982 146.145 ln\n");
  (void) fprintf(fp, "38.9717 146.145 ln\n");
  (void) fprintf(fp, "38.9717 5.82422 ln\n");
  (void) fprintf(fp, "0 0.413643 0.66887 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "39.1982 5.82422 mo\n");
  (void) fprintf(fp, "39.4209 5.82422 ln\n");
  (void) fprintf(fp, "39.4209 146.145 ln\n");
  (void) fprintf(fp, "39.1982 146.145 ln\n");
  (void) fprintf(fp, "39.1982 5.82422 ln\n");
  (void) fprintf(fp, "0 0.416197 0.672999 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "39.4209 5.82422 mo\n");
  (void) fprintf(fp, "39.6475 5.82422 ln\n");
  (void) fprintf(fp, "39.6475 146.145 ln\n");
  (void) fprintf(fp, "39.4209 146.145 ln\n");
  (void) fprintf(fp, "39.4209 5.82422 ln\n");
  (void) fprintf(fp, "0 0.418751 0.677129 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "39.6475 5.82422 mo\n");
  (void) fprintf(fp, "39.874 5.82422 ln\n");
  (void) fprintf(fp, "39.874 146.145 ln\n");
  (void) fprintf(fp, "39.6475 146.145 ln\n");
  (void) fprintf(fp, "39.6475 5.82422 ln\n");
  (void) fprintf(fp, "0 0.421306 0.681261 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "39.874 5.82422 mo\n");
  (void) fprintf(fp, "40.1006 5.82422 ln\n");
  (void) fprintf(fp, "40.1006 146.145 ln\n");
  (void) fprintf(fp, "39.874 146.145 ln\n");
  (void) fprintf(fp, "39.874 5.82422 ln\n");
  (void) fprintf(fp, "0 0.423862 0.685393 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "40.1006 5.82422 mo\n");
  (void) fprintf(fp, "40.3271 5.82422 ln\n");
  (void) fprintf(fp, "40.3271 146.145 ln\n");
  (void) fprintf(fp, "40.1006 146.145 ln\n");
  (void) fprintf(fp, "40.1006 5.82422 ln\n");
  (void) fprintf(fp, "0 0.426418 0.689527 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "40.3271 5.82422 mo\n");
  (void) fprintf(fp, "40.5537 5.82422 ln\n");
  (void) fprintf(fp, "40.5537 146.145 ln\n");
  (void) fprintf(fp, "40.3271 146.145 ln\n");
  (void) fprintf(fp, "40.3271 5.82422 ln\n");
  (void) fprintf(fp, "0 0.428976 0.693663 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "40.5537 5.82422 mo\n");
  (void) fprintf(fp, "40.7803 5.82422 ln\n");
  (void) fprintf(fp, "40.7803 146.145 ln\n");
  (void) fprintf(fp, "40.5537 146.145 ln\n");
  (void) fprintf(fp, "40.5537 5.82422 ln\n");
  (void) fprintf(fp, "0 0.431534 0.697799 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "40.7803 5.82422 mo\n");
  (void) fprintf(fp, "41.0068 5.82422 ln\n");
  (void) fprintf(fp, "41.0068 146.145 ln\n");
  (void) fprintf(fp, "40.7803 146.145 ln\n");
  (void) fprintf(fp, "40.7803 5.82422 ln\n");
  (void) fprintf(fp, "0 0.434093 0.701937 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "41.0068 5.82422 mo\n");
  (void) fprintf(fp, "41.2334 5.82422 ln\n");
  (void) fprintf(fp, "41.2334 146.145 ln\n");
  (void) fprintf(fp, "41.0068 146.145 ln\n");
  (void) fprintf(fp, "41.0068 5.82422 ln\n");
  (void) fprintf(fp, "0 0.436653 0.706077 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "41.2334 5.82422 mo\n");
  (void) fprintf(fp, "41.46 5.82422 ln\n");
  (void) fprintf(fp, "41.46 146.145 ln\n");
  (void) fprintf(fp, "41.2334 146.145 ln\n");
  (void) fprintf(fp, "41.2334 5.82422 ln\n");
  (void) fprintf(fp, "0 0.439213 0.710217 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "41.46 5.82422 mo\n");
  (void) fprintf(fp, "41.6865 5.82422 ln\n");
  (void) fprintf(fp, "41.6865 146.145 ln\n");
  (void) fprintf(fp, "41.46 146.145 ln\n");
  (void) fprintf(fp, "41.46 5.82422 ln\n");
  (void) fprintf(fp, "0 0.441775 0.714359 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "41.6865 5.82422 mo\n");
  (void) fprintf(fp, "41.9131 5.82422 ln\n");
  (void) fprintf(fp, "41.9131 146.145 ln\n");
  (void) fprintf(fp, "41.6865 146.145 ln\n");
  (void) fprintf(fp, "41.6865 5.82422 ln\n");
  (void) fprintf(fp, "0 0.444337 0.718502 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "41.9131 5.82422 mo\n");
  (void) fprintf(fp, "42.1396 5.82422 ln\n");
  (void) fprintf(fp, "42.1396 146.145 ln\n");
  (void) fprintf(fp, "41.9131 146.145 ln\n");
  (void) fprintf(fp, "41.9131 5.82422 ln\n");
  (void) fprintf(fp, "0 0.4469 0.722646 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "42.1396 5.82422 mo\n");
  (void) fprintf(fp, "42.3662 5.82422 ln\n");
  (void) fprintf(fp, "42.3662 146.145 ln\n");
  (void) fprintf(fp, "42.1396 146.145 ln\n");
  (void) fprintf(fp, "42.1396 5.82422 ln\n");
  (void) fprintf(fp, "0 0.449463 0.726792 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "42.3662 5.82422 mo\n");
  (void) fprintf(fp, "42.5928 5.82422 ln\n");
  (void) fprintf(fp, "42.5928 146.145 ln\n");
  (void) fprintf(fp, "42.3662 146.145 ln\n");
  (void) fprintf(fp, "42.3662 5.82422 ln\n");
  (void) fprintf(fp, "0 0.452028 0.730938 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "42.5928 5.82422 mo\n");
  (void) fprintf(fp, "42.8193 5.82422 ln\n");
  (void) fprintf(fp, "42.8193 146.145 ln\n");
  (void) fprintf(fp, "42.5928 146.145 ln\n");
  (void) fprintf(fp, "42.5928 5.82422 ln\n");
  (void) fprintf(fp, "0 0.454593 0.735086 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "42.8193 5.82422 mo\n");
  (void) fprintf(fp, "43.0459 5.82422 ln\n");
  (void) fprintf(fp, "43.0459 146.145 ln\n");
  (void) fprintf(fp, "42.8193 146.145 ln\n");
  (void) fprintf(fp, "42.8193 5.82422 ln\n");
  (void) fprintf(fp, "0 0.457159 0.739236 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "43.0459 5.82422 mo\n");
  (void) fprintf(fp, "43.2725 5.82422 ln\n");
  (void) fprintf(fp, "43.2725 146.145 ln\n");
  (void) fprintf(fp, "43.0459 146.145 ln\n");
  (void) fprintf(fp, "43.0459 5.82422 ln\n");
  (void) fprintf(fp, "0 0.459726 0.743386 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "43.2725 5.82422 mo\n");
  (void) fprintf(fp, "43.499 5.82422 ln\n");
  (void) fprintf(fp, "43.499 146.145 ln\n");
  (void) fprintf(fp, "43.2725 146.145 ln\n");
  (void) fprintf(fp, "43.2725 5.82422 ln\n");
  (void) fprintf(fp, "0 0.462293 0.747538 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "43.499 5.82422 mo\n");
  (void) fprintf(fp, "43.7256 5.82422 ln\n");
  (void) fprintf(fp, "43.7256 146.145 ln\n");
  (void) fprintf(fp, "43.499 146.145 ln\n");
  (void) fprintf(fp, "43.499 5.82422 ln\n");
  (void) fprintf(fp, "0 0.464861 0.751691 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "43.7256 5.82422 mo\n");
  (void) fprintf(fp, "43.9521 5.82422 ln\n");
  (void) fprintf(fp, "43.9521 146.145 ln\n");
  (void) fprintf(fp, "43.7256 146.145 ln\n");
  (void) fprintf(fp, "43.7256 5.82422 ln\n");
  (void) fprintf(fp, "0 0.46743 0.755845 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "43.9521 5.82422 mo\n");
  (void) fprintf(fp, "44.1787 5.82422 ln\n");
  (void) fprintf(fp, "44.1787 146.145 ln\n");
  (void) fprintf(fp, "43.9521 146.145 ln\n");
  (void) fprintf(fp, "43.9521 5.82422 ln\n");
  (void) fprintf(fp, "0 0.47 0.76 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "44.1787 5.82422 mo\n");
  (void) fprintf(fp, "64.8545 5.82422 ln\n");
  (void) fprintf(fp, "64.8545 146.145 ln\n");
  (void) fprintf(fp, "44.1787 146.145 ln\n");
  (void) fprintf(fp, "44.1787 5.82422 ln\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "grestore\n");
  (void) fprintf(fp, "grestore\n");
  (void) fprintf(fp, "43.5205 53.6719 mo\n");
  (void) fprintf(fp, "46.2314 53.6719 ln\n");
  (void) fprintf(fp, "51.2441 63.2402 ln\n");
  (void) fprintf(fp, "51.2842 63.2402 ln\n");
  (void) fprintf(fp, "51.2842 53.6719 ln\n");
  (void) fprintf(fp, "53.2959 53.6719 ln\n");
  (void) fprintf(fp, "53.2959 66.6699 ln\n");
  (void) fprintf(fp, "50.8281 66.6699 ln\n");
  (void) fprintf(fp, "45.5752 56.6338 ln\n");
  (void) fprintf(fp, "45.5371 56.6338 ln\n");
  (void) fprintf(fp, "45.5371 66.6699 ln\n");
  (void) fprintf(fp, "43.5205 66.6699 ln\n");
  (void) fprintf(fp, "43.5205 53.6719 ln\n");
  (void) fprintf(fp, "0 0 0 1 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "65.3838 66.0234 mo\n");
  (void) fprintf(fp, "64.7646 66.2852 63.4092 66.7773 61.8447 66.7773 cv\n");
  (void) fprintf(fp, "58.0547 66.7773 56.3848 64.0664 56.3848 60.0146 cv\n");
  (void) fprintf(fp, "56.3848 56.1895 57.793 53.3604 61.5039 53.3604 cv\n");
  (void) fprintf(fp, "64.5176 53.3604 65.876 55.1143 65.876 57.1348 cv\n");
  (void) fprintf(fp, "63.4248 57.1348 ln\n");
  (void) fprintf(fp, "63.4248 56.1143 63.2002 54.9219 61.5039 54.9219 cv\n");
  (void) fprintf(fp, "59.2295 54.9219 58.8125 57.6416 58.8125 59.6758 cv\n");
  (void) fprintf(fp, "58.8125 62.7842 59.6973 65.043 62.3535 65.043 cv\n");
  (void) fprintf(fp, "63.8037 65.043 65.0312 64.291 65.3838 64.0859 cv\n");
  (void) fprintf(fp, "65.3838 66.0234 ln\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "72.2939 55.7637 mo\n");
  (void) fprintf(fp, "72.334 55.7637 ln\n");
  (void) fprintf(fp, "74.123 61.4727 ln\n");
  (void) fprintf(fp, "70.5068 61.4727 ln\n");
  (void) fprintf(fp, "72.2939 55.7637 ln\n");
  (void) fprintf(fp, "cp\n");
  (void) fprintf(fp, "66.5508 66.6699 mo\n");
  (void) fprintf(fp, "68.8457 66.6699 ln\n");
  (void) fprintf(fp, "69.9785 63.1465 ln\n");
  (void) fprintf(fp, "74.6475 63.1465 ln\n");
  (void) fprintf(fp, "75.7822 66.6699 ln\n");
  (void) fprintf(fp, "78.0781 66.6699 ln\n");
  (void) fprintf(fp, "73.6514 53.6719 ln\n");
  (void) fprintf(fp, "70.9971 53.6719 ln\n");
  (void) fprintf(fp, "66.5508 66.6699 ln\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "82.3965 55.2373 mo\n");
  (void) fprintf(fp, "84.124 55.2373 ln\n");
  (void) fprintf(fp, "85.3711 55.2373 86.0674 55.9688 86.0674 57.3242 cv\n");
  (void) fprintf(fp, "86.0674 59.249 84.6338 59.5293 83.2617 59.5293 cv\n");
  (void) fprintf(fp, "82.3965 59.5293 ln\n");
  (void) fprintf(fp, "82.3965 55.2373 ln\n");
  (void) fprintf(fp, "cp\n");
  (void) fprintf(fp, "80.1875 66.6699 mo\n");
  (void) fprintf(fp, "82.3965 66.6699 ln\n");
  (void) fprintf(fp, "82.3965 61.0938 ln\n");
  (void) fprintf(fp, "83.2617 61.0938 ln\n");
  (void) fprintf(fp, "85.2764 61.0938 85.54 61.7891 85.9893 63.8633 cv\n");
  (void) fprintf(fp, "86.5938 66.6699 ln\n");
  (void) fprintf(fp, "88.833 66.6699 ln\n");
  (void) fprintf(fp, "88.0439 63.0146 ln\n");
  (void) fprintf(fp, "87.7461 61.6406 87.5342 60.5293 85.7217 60.1357 cv\n");
  (void) fprintf(fp, "85.7217 60.0957 ln\n");
  (void) fprintf(fp, "87.0674 59.7783 88.3047 59.1338 88.3047 57.0791 cv\n");
  (void) fprintf(fp, "88.3047 54.5391 86.5752 53.6719 84.124 53.6719 cv\n");
  (void) fprintf(fp, "80.1875 53.6719 ln\n");
  (void) fprintf(fp, "80.1875 66.6699 ln\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "gsave\n");
  (void) fprintf(fp, "0.078125 0.0273438 mo\n");
  (void) fprintf(fp, "56.4912 18.8145 ln\n");
  (void) fprintf(fp, "56.4912 44.1738 ln\n");
  (void) fprintf(fp, "0.078125 44.1738 ln\n");
  (void) fprintf(fp, "0.078125 0.0273438 ln\n");
  (void) fprintf(fp, "clp\n");
  (void) fprintf(fp, "gsave\n");
  (void) fprintf(fp, "65.4004 78.3027 mo\n");
  (void) fprintf(fp, "65.4004 116.233 32.2559 146.985 -8.63672 146.985 cv\n");
  (void) fprintf(fp, "-49.5225 146.985 -82.6689 116.233 -82.6689 78.3027 cv\n");
  (void) fprintf(fp, "-82.6689 40.373 -49.5225 9.625 -8.63672 9.625 cv\n");
  (void) fprintf(fp, "32.2559 9.625 65.4004 40.373 65.4004 78.3027 cv\n");
  (void) fprintf(fp, "clp\n");
  (void) fprintf(fp, "-83.6689 10.1133 mo\n");
  (void) fprintf(fp, "-39.583 8.625 ln\n");
  (void) fprintf(fp, "-0.77832 8.625 ln\n");
  (void) fprintf(fp, "4.68262 147.984 ln\n");
  (void) fprintf(fp, "-83.6689 147.984 ln\n");
  (void) fprintf(fp, "-83.6689 10.1133 ln\n");
  (void) fprintf(fp, "0 0 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "-0.625977 8.625 mo\n");
  (void) fprintf(fp, "4.83496 147.984 ln\n");
  (void) fprintf(fp, "4.68262 147.984 ln\n");
  (void) fprintf(fp, "-0.77832 8.625 ln\n");
  (void) fprintf(fp, "-0.625977 8.625 ln\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "-0.477539 8.625 mo\n");
  (void) fprintf(fp, "4.9873 147.984 ln\n");
  (void) fprintf(fp, "4.83496 147.984 ln\n");
  (void) fprintf(fp, "-0.625977 8.625 ln\n");
  (void) fprintf(fp, "-0.477539 8.625 ln\n");
  (void) fprintf(fp, "4.25577e-05 2.00272e-05 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "-0.325195 8.625 mo\n");
  (void) fprintf(fp, "5.13574 147.984 ln\n");
  (void) fprintf(fp, "4.9873 147.984 ln\n");
  (void) fprintf(fp, "-0.477539 8.625 ln\n");
  (void) fprintf(fp, "-0.325195 8.625 ln\n");
  (void) fprintf(fp, "0.000150084 7.05123e-05 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "-0.176758 8.625 mo\n");
  (void) fprintf(fp, "5.28809 147.984 ln\n");
  (void) fprintf(fp, "5.13574 147.984 ln\n");
  (void) fprintf(fp, "-0.325195 8.625 ln\n");
  (void) fprintf(fp, "-0.176758 8.625 ln\n");
  (void) fprintf(fp, "0.00031358 0.000147402 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "-0.0244141 8.625 mo\n");
  (void) fprintf(fp, "5.44043 147.984 ln\n");
  (void) fprintf(fp, "5.28809 147.984 ln\n");
  (void) fprintf(fp, "-0.176758 8.625 ln\n");
  (void) fprintf(fp, "-0.0244141 8.625 ln\n");
  (void) fprintf(fp, "0.000528932 0.000248611 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "0.12793 8.625 mo\n");
  (void) fprintf(fp, "5.58887 147.984 ln\n");
  (void) fprintf(fp, "5.44043 147.984 ln\n");
  (void) fprintf(fp, "-0.0244141 8.625 ln\n");
  (void) fprintf(fp, "0.12793 8.625 ln\n");
  (void) fprintf(fp, "0.000793457 0.000372946 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "0.276367 8.625 mo\n");
  (void) fprintf(fp, "5.74121 147.984 ln\n");
  (void) fprintf(fp, "5.58887 147.984 ln\n");
  (void) fprintf(fp, "0.12793 8.625 ln\n");
  (void) fprintf(fp, "0.276367 8.625 ln\n");
  (void) fprintf(fp, "0.00110525 0.000519454 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "0.428711 8.625 mo\n");
  (void) fprintf(fp, "5.89355 147.984 ln\n");
  (void) fprintf(fp, "5.74121 147.984 ln\n");
  (void) fprintf(fp, "0.276367 8.625 ln\n");
  (void) fprintf(fp, "0.428711 8.625 ln\n");
  (void) fprintf(fp, "0.00146264 0.00068742 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "0.581055 8.625 mo\n");
  (void) fprintf(fp, "6.04199 147.984 ln\n");
  (void) fprintf(fp, "5.89355 147.984 ln\n");
  (void) fprintf(fp, "0.428711 8.625 ln\n");
  (void) fprintf(fp, "0.581055 8.625 ln\n");
  (void) fprintf(fp, "0.00186437 0.000876248 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "0.729492 8.625 mo\n");
  (void) fprintf(fp, "6.19434 147.984 ln\n");
  (void) fprintf(fp, "6.04199 147.984 ln\n");
  (void) fprintf(fp, "0.581055 8.625 ln\n");
  (void) fprintf(fp, "0.729492 8.625 ln\n");
  (void) fprintf(fp, "0.00230944 0.00108546 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "0.881836 8.625 mo\n");
  (void) fprintf(fp, "6.34277 147.984 ln\n");
  (void) fprintf(fp, "6.19434 147.984 ln\n");
  (void) fprintf(fp, "0.729492 8.625 ln\n");
  (void) fprintf(fp, "0.881836 8.625 ln\n");
  (void) fprintf(fp, "0.00279683 0.00131452 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "1.03027 8.625 mo\n");
  (void) fprintf(fp, "6.49512 147.984 ln\n");
  (void) fprintf(fp, "6.34277 147.984 ln\n");
  (void) fprintf(fp, "0.881836 8.625 ln\n");
  (void) fprintf(fp, "1.03027 8.625 ln\n");
  (void) fprintf(fp, "0.00332582 0.00156313 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "1.18262 8.625 mo\n");
  (void) fprintf(fp, "6.64746 147.984 ln\n");
  (void) fprintf(fp, "6.49512 147.984 ln\n");
  (void) fprintf(fp, "1.03027 8.625 ln\n");
  (void) fprintf(fp, "1.18262 8.625 ln\n");
  (void) fprintf(fp, "0.0038957 0.001831 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "1.33496 8.625 mo\n");
  (void) fprintf(fp, "6.7959 147.984 ln\n");
  (void) fprintf(fp, "6.64746 147.984 ln\n");
  (void) fprintf(fp, "1.18262 8.625 ln\n");
  (void) fprintf(fp, "1.33496 8.625 ln\n");
  (void) fprintf(fp, "0.00450575 0.00211769 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "1.4834 8.625 mo\n");
  (void) fprintf(fp, "6.94824 147.984 ln\n");
  (void) fprintf(fp, "6.7959 147.984 ln\n");
  (void) fprintf(fp, "1.33496 8.625 ln\n");
  (void) fprintf(fp, "1.4834 8.625 ln\n");
  (void) fprintf(fp, "0.00515538 0.00242305 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "1.63574 8.625 mo\n");
  (void) fprintf(fp, "7.09668 147.984 ln\n");
  (void) fprintf(fp, "6.94824 147.984 ln\n");
  (void) fprintf(fp, "1.4834 8.625 ln\n");
  (void) fprintf(fp, "1.63574 8.625 ln\n");
  (void) fprintf(fp, "0.00584418 0.00274676 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "1.78418 8.625 mo\n");
  (void) fprintf(fp, "7.24902 147.984 ln\n");
  (void) fprintf(fp, "7.09668 147.984 ln\n");
  (void) fprintf(fp, "1.63574 8.625 ln\n");
  (void) fprintf(fp, "1.78418 8.625 ln\n");
  (void) fprintf(fp, "0.00657147 0.00308859 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "1.93652 8.625 mo\n");
  (void) fprintf(fp, "7.40137 147.984 ln\n");
  (void) fprintf(fp, "7.24902 147.984 ln\n");
  (void) fprintf(fp, "1.78418 8.625 ln\n");
  (void) fprintf(fp, "1.93652 8.625 ln\n");
  (void) fprintf(fp, "0.00733697 0.00344837 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "2.08887 8.625 mo\n");
  (void) fprintf(fp, "7.5498 147.984 ln\n");
  (void) fprintf(fp, "7.40137 147.984 ln\n");
  (void) fprintf(fp, "1.93652 8.625 ln\n");
  (void) fprintf(fp, "2.08887 8.625 ln\n");
  (void) fprintf(fp, "0.00814021 0.0038259 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "2.2373 8.625 mo\n");
  (void) fprintf(fp, "7.70215 147.984 ln\n");
  (void) fprintf(fp, "7.5498 147.984 ln\n");
  (void) fprintf(fp, "2.08887 8.625 ln\n");
  (void) fprintf(fp, "2.2373 8.625 ln\n");
  (void) fprintf(fp, "0.00898075 0.00422096 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "2.38965 8.625 mo\n");
  (void) fprintf(fp, "7.85449 147.984 ln\n");
  (void) fprintf(fp, "7.70215 147.984 ln\n");
  (void) fprintf(fp, "2.2373 8.625 ln\n");
  (void) fprintf(fp, "2.38965 8.625 ln\n");
  (void) fprintf(fp, "0.00985831 0.00463343 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "2.54199 8.625 mo\n");
  (void) fprintf(fp, "8.00293 147.984 ln\n");
  (void) fprintf(fp, "7.85449 147.984 ln\n");
  (void) fprintf(fp, "2.38965 8.625 ln\n");
  (void) fprintf(fp, "2.54199 8.625 ln\n");
  (void) fprintf(fp, "0.0107724 0.00506306 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "2.69043 8.625 mo\n");
  (void) fprintf(fp, "8.15527 147.984 ln\n");
  (void) fprintf(fp, "8.00293 147.984 ln\n");
  (void) fprintf(fp, "2.54199 8.625 ln\n");
  (void) fprintf(fp, "2.69043 8.625 ln\n");
  (void) fprintf(fp, "0.0117229 0.00550973 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "2.84277 8.625 mo\n");
  (void) fprintf(fp, "8.30371 147.984 ln\n");
  (void) fprintf(fp, "8.15527 147.984 ln\n");
  (void) fprintf(fp, "2.69043 8.625 ln\n");
  (void) fprintf(fp, "2.84277 8.625 ln\n");
  (void) fprintf(fp, "0.0127093 0.0059734 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "2.99121 8.625 mo\n");
  (void) fprintf(fp, "8.45605 147.984 ln\n");
  (void) fprintf(fp, "8.30371 147.984 ln\n");
  (void) fprintf(fp, "2.84277 8.625 ln\n");
  (void) fprintf(fp, "2.99121 8.625 ln\n");
  (void) fprintf(fp, "0.0137314 0.00645375 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "3.14355 8.625 mo\n");
  (void) fprintf(fp, "8.60449 147.984 ln\n");
  (void) fprintf(fp, "8.45605 147.984 ln\n");
  (void) fprintf(fp, "2.99121 8.625 ln\n");
  (void) fprintf(fp, "3.14355 8.625 ln\n");
  (void) fprintf(fp, "0.014789 0.00695086 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "3.2959 8.625 mo\n");
  (void) fprintf(fp, "8.75684 147.984 ln\n");
  (void) fprintf(fp, "8.60449 147.984 ln\n");
  (void) fprintf(fp, "3.14355 8.625 ln\n");
  (void) fprintf(fp, "3.2959 8.625 ln\n");
  (void) fprintf(fp, "0.0158817 0.00746441 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "3.44434 8.625 mo\n");
  (void) fprintf(fp, "8.90918 147.984 ln\n");
  (void) fprintf(fp, "8.75684 147.984 ln\n");
  (void) fprintf(fp, "3.2959 8.625 ln\n");
  (void) fprintf(fp, "3.44434 8.625 ln\n");
  (void) fprintf(fp, "0.0170093 0.00799441 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "3.59668 8.625 mo\n");
  (void) fprintf(fp, "9.05762 147.984 ln\n");
  (void) fprintf(fp, "8.90918 147.984 ln\n");
  (void) fprintf(fp, "3.44434 8.625 ln\n");
  (void) fprintf(fp, "3.59668 8.625 ln\n");
  (void) fprintf(fp, "0.0181716 0.00854069 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "3.74512 8.625 mo\n");
  (void) fprintf(fp, "9.20996 147.984 ln\n");
  (void) fprintf(fp, "9.05762 147.984 ln\n");
  (void) fprintf(fp, "3.59668 8.625 ln\n");
  (void) fprintf(fp, "3.74512 8.625 ln\n");
  (void) fprintf(fp, "0.0193684 0.00910312 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "3.89746 8.625 mo\n");
  (void) fprintf(fp, "9.3623 147.984 ln\n");
  (void) fprintf(fp, "9.20996 147.984 ln\n");
  (void) fprintf(fp, "3.74512 8.625 ln\n");
  (void) fprintf(fp, "3.89746 8.625 ln\n");
  (void) fprintf(fp, "0.0205993 0.0096817 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "4.0498 8.625 mo\n");
  (void) fprintf(fp, "9.51074 147.984 ln\n");
  (void) fprintf(fp, "9.3623 147.984 ln\n");
  (void) fprintf(fp, "3.89746 8.625 ln\n");
  (void) fprintf(fp, "4.0498 8.625 ln\n");
  (void) fprintf(fp, "0.0218643 0.0102762 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "4.19824 8.625 mo\n");
  (void) fprintf(fp, "9.66309 147.984 ln\n");
  (void) fprintf(fp, "9.51074 147.984 ln\n");
  (void) fprintf(fp, "4.0498 8.625 ln\n");
  (void) fprintf(fp, "4.19824 8.625 ln\n");
  (void) fprintf(fp, "0.0231631 0.0108867 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "4.35059 8.625 mo\n");
  (void) fprintf(fp, "9.81543 147.984 ln\n");
  (void) fprintf(fp, "9.66309 147.984 ln\n");
  (void) fprintf(fp, "4.19824 8.625 ln\n");
  (void) fprintf(fp, "4.35059 8.625 ln\n");
  (void) fprintf(fp, "0.0244955 0.0115129 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "4.50293 8.625 mo\n");
  (void) fprintf(fp, "9.96387 147.984 ln\n");
  (void) fprintf(fp, "9.81543 147.984 ln\n");
  (void) fprintf(fp, "4.35059 8.625 ln\n");
  (void) fprintf(fp, "4.50293 8.625 ln\n");
  (void) fprintf(fp, "0.0258613 0.0121548 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "4.65137 8.625 mo\n");
  (void) fprintf(fp, "10.1162 147.984 ln\n");
  (void) fprintf(fp, "9.96387 147.984 ln\n");
  (void) fprintf(fp, "4.50293 8.625 ln\n");
  (void) fprintf(fp, "4.65137 8.625 ln\n");
  (void) fprintf(fp, "0.0272603 0.0128124 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "4.80371 8.625 mo\n");
  (void) fprintf(fp, "10.2686 147.984 ln\n");
  (void) fprintf(fp, "10.1162 147.984 ln\n");
  (void) fprintf(fp, "4.65137 8.625 ln\n");
  (void) fprintf(fp, "4.80371 8.625 ln\n");
  (void) fprintf(fp, "0.0286924 0.0134854 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "4.95215 8.625 mo\n");
  (void) fprintf(fp, "10.417 147.984 ln\n");
  (void) fprintf(fp, "10.2686 147.984 ln\n");
  (void) fprintf(fp, "4.80371 8.625 ln\n");
  (void) fprintf(fp, "4.95215 8.625 ln\n");
  (void) fprintf(fp, "0.0301574 0.014174 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "5.10449 8.625 mo\n");
  (void) fprintf(fp, "10.5654 147.984 ln\n");
  (void) fprintf(fp, "10.417 147.984 ln\n");
  (void) fprintf(fp, "4.95215 8.625 ln\n");
  (void) fprintf(fp, "5.10449 8.625 ln\n");
  (void) fprintf(fp, "0.0316552 0.014878 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "5.25684 8.625 mo\n");
  (void) fprintf(fp, "10.7178 147.984 ln\n");
  (void) fprintf(fp, "10.5654 147.984 ln\n");
  (void) fprintf(fp, "5.10449 8.625 ln\n");
  (void) fprintf(fp, "5.25684 8.625 ln\n");
  (void) fprintf(fp, "0.0331855 0.0155972 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "5.40527 8.625 mo\n");
  (void) fprintf(fp, "10.8701 147.984 ln\n");
  (void) fprintf(fp, "10.7178 147.984 ln\n");
  (void) fprintf(fp, "5.25684 8.625 ln\n");
  (void) fprintf(fp, "5.40527 8.625 ln\n");
  (void) fprintf(fp, "0.0347483 0.0163317 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "5.55762 8.625 mo\n");
  (void) fprintf(fp, "11.0186 147.984 ln\n");
  (void) fprintf(fp, "10.8701 147.984 ln\n");
  (void) fprintf(fp, "5.40527 8.625 ln\n");
  (void) fprintf(fp, "5.55762 8.625 ln\n");
  (void) fprintf(fp, "0.0363433 0.0170813 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "5.70605 8.625 mo\n");
  (void) fprintf(fp, "11.1709 147.984 ln\n");
  (void) fprintf(fp, "11.0186 147.984 ln\n");
  (void) fprintf(fp, "5.55762 8.625 ln\n");
  (void) fprintf(fp, "5.70605 8.625 ln\n");
  (void) fprintf(fp, "0.0379704 0.0178461 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "5.8584 8.625 mo\n");
  (void) fprintf(fp, "11.3232 147.984 ln\n");
  (void) fprintf(fp, "11.1709 147.984 ln\n");
  (void) fprintf(fp, "5.70605 8.625 ln\n");
  (void) fprintf(fp, "5.8584 8.625 ln\n");
  (void) fprintf(fp, "0.0396295 0.0186259 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "6.01074 8.625 mo\n");
  (void) fprintf(fp, "11.4717 147.984 ln\n");
  (void) fprintf(fp, "11.3232 147.984 ln\n");
  (void) fprintf(fp, "5.8584 8.625 ln\n");
  (void) fprintf(fp, "6.01074 8.625 ln\n");
  (void) fprintf(fp, "0.0413205 0.0194206 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "6.15918 8.625 mo\n");
  (void) fprintf(fp, "11.624 147.984 ln\n");
  (void) fprintf(fp, "11.4717 147.984 ln\n");
  (void) fprintf(fp, "6.01074 8.625 ln\n");
  (void) fprintf(fp, "6.15918 8.625 ln\n");
  (void) fprintf(fp, "0.0430432 0.0202303 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "6.31152 8.625 mo\n");
  (void) fprintf(fp, "11.7764 147.984 ln\n");
  (void) fprintf(fp, "11.624 147.984 ln\n");
  (void) fprintf(fp, "6.15918 8.625 ln\n");
  (void) fprintf(fp, "6.31152 8.625 ln\n");
  (void) fprintf(fp, "0.0447975 0.0210548 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "6.46387 8.625 mo\n");
  (void) fprintf(fp, "11.9248 147.984 ln\n");
  (void) fprintf(fp, "11.7764 147.984 ln\n");
  (void) fprintf(fp, "6.31152 8.625 ln\n");
  (void) fprintf(fp, "6.46387 8.625 ln\n");
  (void) fprintf(fp, "0.0465832 0.0218941 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "6.6123 8.625 mo\n");
  (void) fprintf(fp, "12.0771 147.984 ln\n");
  (void) fprintf(fp, "11.9248 147.984 ln\n");
  (void) fprintf(fp, "6.46387 8.625 ln\n");
  (void) fprintf(fp, "6.6123 8.625 ln\n");
  (void) fprintf(fp, "0.0484003 0.0227481 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "6.76465 8.625 mo\n");
  (void) fprintf(fp, "12.2256 147.984 ln\n");
  (void) fprintf(fp, "12.0771 147.984 ln\n");
  (void) fprintf(fp, "6.6123 8.625 ln\n");
  (void) fprintf(fp, "6.76465 8.625 ln\n");
  (void) fprintf(fp, "0.0502485 0.0236168 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "6.91699 8.625 mo\n");
  (void) fprintf(fp, "12.3779 147.984 ln\n");
  (void) fprintf(fp, "12.2256 147.984 ln\n");
  (void) fprintf(fp, "6.76465 8.625 ln\n");
  (void) fprintf(fp, "6.91699 8.625 ln\n");
  (void) fprintf(fp, "0.0521279 0.0245001 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "7.06543 8.625 mo\n");
  (void) fprintf(fp, "12.5303 147.984 ln\n");
  (void) fprintf(fp, "12.3779 147.984 ln\n");
  (void) fprintf(fp, "6.91699 8.625 ln\n");
  (void) fprintf(fp, "7.06543 8.625 ln\n");
  (void) fprintf(fp, "0.0540382 0.025398 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "7.21387 8.625 mo\n");
  (void) fprintf(fp, "12.6787 147.984 ln\n");
  (void) fprintf(fp, "12.5303 147.984 ln\n");
  (void) fprintf(fp, "7.06543 8.625 ln\n");
  (void) fprintf(fp, "7.21387 8.625 ln\n");
  (void) fprintf(fp, "0.0559795 0.0263104 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "7.36621 8.625 mo\n");
  (void) fprintf(fp, "12.8311 147.984 ln\n");
  (void) fprintf(fp, "12.6787 147.984 ln\n");
  (void) fprintf(fp, "7.21387 8.625 ln\n");
  (void) fprintf(fp, "7.36621 8.625 ln\n");
  (void) fprintf(fp, "0.0579515 0.0272372 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "7.51855 8.625 mo\n");
  (void) fprintf(fp, "12.9795 147.984 ln\n");
  (void) fprintf(fp, "12.8311 147.984 ln\n");
  (void) fprintf(fp, "7.36621 8.625 ln\n");
  (void) fprintf(fp, "7.51855 8.625 ln\n");
  (void) fprintf(fp, "0.0599541 0.0281785 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "7.66699 8.625 mo\n");
  (void) fprintf(fp, "13.1318 147.984 ln\n");
  (void) fprintf(fp, "12.9795 147.984 ln\n");
  (void) fprintf(fp, "7.51855 8.625 ln\n");
  (void) fprintf(fp, "7.66699 8.625 ln\n");
  (void) fprintf(fp, "0.0619873 0.029134 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "7.81934 8.625 mo\n");
  (void) fprintf(fp, "13.2842 147.984 ln\n");
  (void) fprintf(fp, "13.1318 147.984 ln\n");
  (void) fprintf(fp, "7.66699 8.625 ln\n");
  (void) fprintf(fp, "7.81934 8.625 ln\n");
  (void) fprintf(fp, "0.064051 0.0301039 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "7.97168 8.625 mo\n");
  (void) fprintf(fp, "13.4326 147.984 ln\n");
  (void) fprintf(fp, "13.2842 147.984 ln\n");
  (void) fprintf(fp, "7.81934 8.625 ln\n");
  (void) fprintf(fp, "7.97168 8.625 ln\n");
  (void) fprintf(fp, "0.0661449 0.0310881 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "8.12012 8.625 mo\n");
  (void) fprintf(fp, "13.585 147.984 ln\n");
  (void) fprintf(fp, "13.4326 147.984 ln\n");
  (void) fprintf(fp, "7.97168 8.625 ln\n");
  (void) fprintf(fp, "8.12012 8.625 ln\n");
  (void) fprintf(fp, "0.0682692 0.0320865 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "8.27246 8.625 mo\n");
  (void) fprintf(fp, "13.7373 147.984 ln\n");
  (void) fprintf(fp, "13.585 147.984 ln\n");
  (void) fprintf(fp, "8.12012 8.625 ln\n");
  (void) fprintf(fp, "8.27246 8.625 ln\n");
  (void) fprintf(fp, "0.0704236 0.0330991 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "8.4248 8.625 mo\n");
  (void) fprintf(fp, "13.8857 147.984 ln\n");
  (void) fprintf(fp, "13.7373 147.984 ln\n");
  (void) fprintf(fp, "8.27246 8.625 ln\n");
  (void) fprintf(fp, "8.4248 8.625 ln\n");
  (void) fprintf(fp, "0.0726081 0.0341258 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "8.57324 8.625 mo\n");
  (void) fprintf(fp, "14.0381 147.984 ln\n");
  (void) fprintf(fp, "13.8857 147.984 ln\n");
  (void) fprintf(fp, "8.4248 8.625 ln\n");
  (void) fprintf(fp, "8.57324 8.625 ln\n");
  (void) fprintf(fp, "0.0748225 0.0351666 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "8.72559 8.625 mo\n");
  (void) fprintf(fp, "14.1904 147.984 ln\n");
  (void) fprintf(fp, "14.0381 147.984 ln\n");
  (void) fprintf(fp, "8.57324 8.625 ln\n");
  (void) fprintf(fp, "8.72559 8.625 ln\n");
  (void) fprintf(fp, "0.0770668 0.0362214 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "8.87793 8.625 mo\n");
  (void) fprintf(fp, "14.3389 147.984 ln\n");
  (void) fprintf(fp, "14.1904 147.984 ln\n");
  (void) fprintf(fp, "8.72559 8.625 ln\n");
  (void) fprintf(fp, "8.87793 8.625 ln\n");
  (void) fprintf(fp, "0.0793409 0.0372902 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "9.02637 8.625 mo\n");
  (void) fprintf(fp, "14.4873 147.984 ln\n");
  (void) fprintf(fp, "14.3389 147.984 ln\n");
  (void) fprintf(fp, "8.87793 8.625 ln\n");
  (void) fprintf(fp, "9.02637 8.625 ln\n");
  (void) fprintf(fp, "0.0816447 0.038373 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "9.17871 8.625 mo\n");
  (void) fprintf(fp, "14.6396 147.984 ln\n");
  (void) fprintf(fp, "14.4873 147.984 ln\n");
  (void) fprintf(fp, "9.02637 8.625 ln\n");
  (void) fprintf(fp, "9.17871 8.625 ln\n");
  (void) fprintf(fp, "0.0839781 0.0394697 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "9.32715 8.625 mo\n");
  (void) fprintf(fp, "14.792 147.984 ln\n");
  (void) fprintf(fp, "14.6396 147.984 ln\n");
  (void) fprintf(fp, "9.17871 8.625 ln\n");
  (void) fprintf(fp, "9.32715 8.625 ln\n");
  (void) fprintf(fp, "0.086341 0.0405803 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "9.47949 8.625 mo\n");
  (void) fprintf(fp, "14.9404 147.984 ln\n");
  (void) fprintf(fp, "14.792 147.984 ln\n");
  (void) fprintf(fp, "9.32715 8.625 ln\n");
  (void) fprintf(fp, "9.47949 8.625 ln\n");
  (void) fprintf(fp, "0.0887334 0.0417047 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "9.63184 8.625 mo\n");
  (void) fprintf(fp, "15.0928 147.984 ln\n");
  (void) fprintf(fp, "14.9404 147.984 ln\n");
  (void) fprintf(fp, "9.47949 8.625 ln\n");
  (void) fprintf(fp, "9.63184 8.625 ln\n");
  (void) fprintf(fp, "0.0911552 0.0428429 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "9.78027 8.625 mo\n");
  (void) fprintf(fp, "15.2451 147.984 ln\n");
  (void) fprintf(fp, "15.0928 147.984 ln\n");
  (void) fprintf(fp, "9.63184 8.625 ln\n");
  (void) fprintf(fp, "9.78027 8.625 ln\n");
  (void) fprintf(fp, "0.0936062 0.043995 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "9.93262 8.625 mo\n");
  (void) fprintf(fp, "15.3936 147.984 ln\n");
  (void) fprintf(fp, "15.2451 147.984 ln\n");
  (void) fprintf(fp, "9.78027 8.625 ln\n");
  (void) fprintf(fp, "9.93262 8.625 ln\n");
  (void) fprintf(fp, "0.0960866 0.0451607 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "10.0811 8.625 mo\n");
  (void) fprintf(fp, "15.5459 147.984 ln\n");
  (void) fprintf(fp, "15.3936 147.984 ln\n");
  (void) fprintf(fp, "9.93262 8.625 ln\n");
  (void) fprintf(fp, "10.0811 8.625 ln\n");
  (void) fprintf(fp, "0.098596 0.0463401 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "10.2334 8.625 mo\n");
  (void) fprintf(fp, "15.6982 147.984 ln\n");
  (void) fprintf(fp, "15.5459 147.984 ln\n");
  (void) fprintf(fp, "10.0811 8.625 ln\n");
  (void) fprintf(fp, "10.2334 8.625 ln\n");
  (void) fprintf(fp, "0.101134 0.0475332 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "10.3857 8.625 mo\n");
  (void) fprintf(fp, "15.8467 147.984 ln\n");
  (void) fprintf(fp, "15.6982 147.984 ln\n");
  (void) fprintf(fp, "10.2334 8.625 ln\n");
  (void) fprintf(fp, "10.3857 8.625 ln\n");
  (void) fprintf(fp, "0.103702 0.0487399 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "10.5342 8.625 mo\n");
  (void) fprintf(fp, "15.999 147.984 ln\n");
  (void) fprintf(fp, "15.8467 147.984 ln\n");
  (void) fprintf(fp, "10.3857 8.625 ln\n");
  (void) fprintf(fp, "10.5342 8.625 ln\n");
  (void) fprintf(fp, "0.106298 0.0499602 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "10.6865 8.625 mo\n");
  (void) fprintf(fp, "16.1514 147.984 ln\n");
  (void) fprintf(fp, "15.999 147.984 ln\n");
  (void) fprintf(fp, "10.5342 8.625 ln\n");
  (void) fprintf(fp, "10.6865 8.625 ln\n");
  (void) fprintf(fp, "0.108924 0.0511941 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "10.8389 8.625 mo\n");
  (void) fprintf(fp, "16.2998 147.984 ln\n");
  (void) fprintf(fp, "16.1514 147.984 ln\n");
  (void) fprintf(fp, "10.6865 8.625 ln\n");
  (void) fprintf(fp, "10.8389 8.625 ln\n");
  (void) fprintf(fp, "0.111578 0.0524414 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "10.9873 8.625 mo\n");
  (void) fprintf(fp, "16.4521 147.984 ln\n");
  (void) fprintf(fp, "16.2998 147.984 ln\n");
  (void) fprintf(fp, "10.8389 8.625 ln\n");
  (void) fprintf(fp, "10.9873 8.625 ln\n");
  (void) fprintf(fp, "0.11426 0.0537023 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "11.1396 8.625 mo\n");
  (void) fprintf(fp, "16.6006 147.984 ln\n");
  (void) fprintf(fp, "16.4521 147.984 ln\n");
  (void) fprintf(fp, "10.9873 8.625 ln\n");
  (void) fprintf(fp, "11.1396 8.625 ln\n");
  (void) fprintf(fp, "0.116972 0.0549766 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "11.2881 8.625 mo\n");
  (void) fprintf(fp, "16.7529 147.984 ln\n");
  (void) fprintf(fp, "16.6006 147.984 ln\n");
  (void) fprintf(fp, "11.1396 8.625 ln\n");
  (void) fprintf(fp, "11.2881 8.625 ln\n");
  (void) fprintf(fp, "0.119712 0.0562644 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "11.4404 8.625 mo\n");
  (void) fprintf(fp, "16.9014 147.984 ln\n");
  (void) fprintf(fp, "16.7529 147.984 ln\n");
  (void) fprintf(fp, "11.2881 8.625 ln\n");
  (void) fprintf(fp, "11.4404 8.625 ln\n");
  (void) fprintf(fp, "0.12248 0.0575656 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "11.5889 8.625 mo\n");
  (void) fprintf(fp, "17.0537 147.984 ln\n");
  (void) fprintf(fp, "16.9014 147.984 ln\n");
  (void) fprintf(fp, "11.4404 8.625 ln\n");
  (void) fprintf(fp, "11.5889 8.625 ln\n");
  (void) fprintf(fp, "0.125277 0.0588801 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "11.7412 8.625 mo\n");
  (void) fprintf(fp, "17.2061 147.984 ln\n");
  (void) fprintf(fp, "17.0537 147.984 ln\n");
  (void) fprintf(fp, "11.5889 8.625 ln\n");
  (void) fprintf(fp, "11.7412 8.625 ln\n");
  (void) fprintf(fp, "0.128102 0.0602079 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "11.8936 8.625 mo\n");
  (void) fprintf(fp, "17.3545 147.984 ln\n");
  (void) fprintf(fp, "17.2061 147.984 ln\n");
  (void) fprintf(fp, "11.7412 8.625 ln\n");
  (void) fprintf(fp, "11.8936 8.625 ln\n");
  (void) fprintf(fp, "0.130955 0.0615491 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "12.042 8.625 mo\n");
  (void) fprintf(fp, "17.5068 147.984 ln\n");
  (void) fprintf(fp, "17.3545 147.984 ln\n");
  (void) fprintf(fp, "11.8936 8.625 ln\n");
  (void) fprintf(fp, "12.042 8.625 ln\n");
  (void) fprintf(fp, "0.133837 0.0629035 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "12.1943 8.625 mo\n");
  (void) fprintf(fp, "17.6592 147.984 ln\n");
  (void) fprintf(fp, "17.5068 147.984 ln\n");
  (void) fprintf(fp, "12.042 8.625 ln\n");
  (void) fprintf(fp, "12.1943 8.625 ln\n");
  (void) fprintf(fp, "0.136747 0.0642712 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "12.3467 8.625 mo\n");
  (void) fprintf(fp, "17.8076 147.984 ln\n");
  (void) fprintf(fp, "17.6592 147.984 ln\n");
  (void) fprintf(fp, "12.1943 8.625 ln\n");
  (void) fprintf(fp, "12.3467 8.625 ln\n");
  (void) fprintf(fp, "0.139685 0.0656521 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "12.4951 8.625 mo\n");
  (void) fprintf(fp, "17.96 147.984 ln\n");
  (void) fprintf(fp, "17.8076 147.984 ln\n");
  (void) fprintf(fp, "12.3467 8.625 ln\n");
  (void) fprintf(fp, "12.4951 8.625 ln\n");
  (void) fprintf(fp, "0.142651 0.0670462 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "12.6475 8.625 mo\n");
  (void) fprintf(fp, "18.1084 147.984 ln\n");
  (void) fprintf(fp, "17.96 147.984 ln\n");
  (void) fprintf(fp, "12.4951 8.625 ln\n");
  (void) fprintf(fp, "12.6475 8.625 ln\n");
  (void) fprintf(fp, "0.145646 0.0684534 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "12.7998 8.625 mo\n");
  (void) fprintf(fp, "18.2607 147.984 ln\n");
  (void) fprintf(fp, "18.1084 147.984 ln\n");
  (void) fprintf(fp, "12.6475 8.625 ln\n");
  (void) fprintf(fp, "12.7998 8.625 ln\n");
  (void) fprintf(fp, "0.148668 0.0698738 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "12.9482 8.625 mo\n");
  (void) fprintf(fp, "18.4131 147.984 ln\n");
  (void) fprintf(fp, "18.2607 147.984 ln\n");
  (void) fprintf(fp, "12.7998 8.625 ln\n");
  (void) fprintf(fp, "12.9482 8.625 ln\n");
  (void) fprintf(fp, "0.151718 0.0713073 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "13.1006 8.625 mo\n");
  (void) fprintf(fp, "18.5615 147.984 ln\n");
  (void) fprintf(fp, "18.4131 147.984 ln\n");
  (void) fprintf(fp, "12.9482 8.625 ln\n");
  (void) fprintf(fp, "13.1006 8.625 ln\n");
  (void) fprintf(fp, "0.154795 0.0727539 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "13.249 8.625 mo\n");
  (void) fprintf(fp, "18.7139 147.984 ln\n");
  (void) fprintf(fp, "18.5615 147.984 ln\n");
  (void) fprintf(fp, "13.1006 8.625 ln\n");
  (void) fprintf(fp, "13.249 8.625 ln\n");
  (void) fprintf(fp, "0.157901 0.0742135 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "13.4014 8.625 mo\n");
  (void) fprintf(fp, "18.8623 147.984 ln\n");
  (void) fprintf(fp, "18.7139 147.984 ln\n");
  (void) fprintf(fp, "13.249 8.625 ln\n");
  (void) fprintf(fp, "13.4014 8.625 ln\n");
  (void) fprintf(fp, "0.161034 0.0756862 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "13.5498 8.625 mo\n");
  (void) fprintf(fp, "19.0146 147.984 ln\n");
  (void) fprintf(fp, "18.8623 147.984 ln\n");
  (void) fprintf(fp, "13.4014 8.625 ln\n");
  (void) fprintf(fp, "13.5498 8.625 ln\n");
  (void) fprintf(fp, "0.164195 0.0771718 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "13.7021 8.625 mo\n");
  (void) fprintf(fp, "19.167 147.984 ln\n");
  (void) fprintf(fp, "19.0146 147.984 ln\n");
  (void) fprintf(fp, "13.5498 8.625 ln\n");
  (void) fprintf(fp, "13.7021 8.625 ln\n");
  (void) fprintf(fp, "0.167384 0.0786704 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "13.8545 8.625 mo\n");
  (void) fprintf(fp, "19.3154 147.984 ln\n");
  (void) fprintf(fp, "19.167 147.984 ln\n");
  (void) fprintf(fp, "13.7021 8.625 ln\n");
  (void) fprintf(fp, "13.8545 8.625 ln\n");
  (void) fprintf(fp, "0.1706 0.0801821 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "14.0029 8.625 mo\n");
  (void) fprintf(fp, "19.4678 147.984 ln\n");
  (void) fprintf(fp, "19.3154 147.984 ln\n");
  (void) fprintf(fp, "13.8545 8.625 ln\n");
  (void) fprintf(fp, "14.0029 8.625 ln\n");
  (void) fprintf(fp, "0.173844 0.0817065 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "14.1553 8.625 mo\n");
  (void) fprintf(fp, "19.6201 147.984 ln\n");
  (void) fprintf(fp, "19.4678 147.984 ln\n");
  (void) fprintf(fp, "14.0029 8.625 ln\n");
  (void) fprintf(fp, "14.1553 8.625 ln\n");
  (void) fprintf(fp, "0.177115 0.083244 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "14.3076 8.625 mo\n");
  (void) fprintf(fp, "19.7686 147.984 ln\n");
  (void) fprintf(fp, "19.6201 147.984 ln\n");
  (void) fprintf(fp, "14.1553 8.625 ln\n");
  (void) fprintf(fp, "14.3076 8.625 ln\n");
  (void) fprintf(fp, "0.180413 0.0847943 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "14.4561 8.625 mo\n");
  (void) fprintf(fp, "19.9209 147.984 ln\n");
  (void) fprintf(fp, "19.7686 147.984 ln\n");
  (void) fprintf(fp, "14.3076 8.625 ln\n");
  (void) fprintf(fp, "14.4561 8.625 ln\n");
  (void) fprintf(fp, "0.183739 0.0863574 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "14.6084 8.625 mo\n");
  (void) fprintf(fp, "20.0732 147.984 ln\n");
  (void) fprintf(fp, "19.9209 147.984 ln\n");
  (void) fprintf(fp, "14.4561 8.625 ln\n");
  (void) fprintf(fp, "14.6084 8.625 ln\n");
  (void) fprintf(fp, "0.187092 0.0879334 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "14.7568 8.625 mo\n");
  (void) fprintf(fp, "20.2217 147.984 ln\n");
  (void) fprintf(fp, "20.0732 147.984 ln\n");
  (void) fprintf(fp, "14.6084 8.625 ln\n");
  (void) fprintf(fp, "14.7568 8.625 ln\n");
  (void) fprintf(fp, "0.190473 0.0895222 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "14.9092 8.625 mo\n");
  (void) fprintf(fp, "20.3701 147.984 ln\n");
  (void) fprintf(fp, "20.2217 147.984 ln\n");
  (void) fprintf(fp, "14.7568 8.625 ln\n");
  (void) fprintf(fp, "14.9092 8.625 ln\n");
  (void) fprintf(fp, "0.19388 0.0911238 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "15.0615 8.625 mo\n");
  (void) fprintf(fp, "20.5264 147.984 ln\n");
  (void) fprintf(fp, "20.3701 147.984 ln\n");
  (void) fprintf(fp, "14.9092 8.625 ln\n");
  (void) fprintf(fp, "15.0615 8.625 ln\n");
  (void) fprintf(fp, "0.197315 0.0927382 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "15.21 8.625 mo\n");
  (void) fprintf(fp, "20.6748 147.984 ln\n");
  (void) fprintf(fp, "20.5264 147.984 ln\n");
  (void) fprintf(fp, "15.0615 8.625 ln\n");
  (void) fprintf(fp, "15.21 8.625 ln\n");
  (void) fprintf(fp, "0.200777 0.0943652 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "15.3623 8.625 mo\n");
  (void) fprintf(fp, "20.8232 147.984 ln\n");
  (void) fprintf(fp, "20.6748 147.984 ln\n");
  (void) fprintf(fp, "15.21 8.625 ln\n");
  (void) fprintf(fp, "15.3623 8.625 ln\n");
  (void) fprintf(fp, "0.204266 0.096005 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "15.5107 8.625 mo\n");
  (void) fprintf(fp, "20.9756 147.984 ln\n");
  (void) fprintf(fp, "20.8232 147.984 ln\n");
  (void) fprintf(fp, "15.3623 8.625 ln\n");
  (void) fprintf(fp, "15.5107 8.625 ln\n");
  (void) fprintf(fp, "0.207782 0.0976575 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "15.6631 8.625 mo\n");
  (void) fprintf(fp, "21.1279 147.984 ln\n");
  (void) fprintf(fp, "20.9756 147.984 ln\n");
  (void) fprintf(fp, "15.5107 8.625 ln\n");
  (void) fprintf(fp, "15.6631 8.625 ln\n");
  (void) fprintf(fp, "0.211325 0.0993227 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "15.8154 8.625 mo\n");
  (void) fprintf(fp, "21.2764 147.984 ln\n");
  (void) fprintf(fp, "21.1279 147.984 ln\n");
  (void) fprintf(fp, "15.6631 8.625 ln\n");
  (void) fprintf(fp, "15.8154 8.625 ln\n");
  (void) fprintf(fp, "0.214895 0.101 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "15.9639 8.625 mo\n");
  (void) fprintf(fp, "21.4287 147.984 ln\n");
  (void) fprintf(fp, "21.2764 147.984 ln\n");
  (void) fprintf(fp, "15.8154 8.625 ln\n");
  (void) fprintf(fp, "15.9639 8.625 ln\n");
  (void) fprintf(fp, "0.218491 0.102691 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "16.1162 8.625 mo\n");
  (void) fprintf(fp, "21.5811 147.984 ln\n");
  (void) fprintf(fp, "21.4287 147.984 ln\n");
  (void) fprintf(fp, "15.9639 8.625 ln\n");
  (void) fprintf(fp, "16.1162 8.625 ln\n");
  (void) fprintf(fp, "0.222115 0.104394 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "16.2686 8.625 mo\n");
  (void) fprintf(fp, "21.7295 147.984 ln\n");
  (void) fprintf(fp, "21.5811 147.984 ln\n");
  (void) fprintf(fp, "16.1162 8.625 ln\n");
  (void) fprintf(fp, "16.2686 8.625 ln\n");
  (void) fprintf(fp, "0.225765 0.10611 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "16.417 8.625 mo\n");
  (void) fprintf(fp, "21.8818 147.984 ln\n");
  (void) fprintf(fp, "21.7295 147.984 ln\n");
  (void) fprintf(fp, "16.2686 8.625 ln\n");
  (void) fprintf(fp, "16.417 8.625 ln\n");
  (void) fprintf(fp, "0.229442 0.107838 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "16.5693 8.625 mo\n");
  (void) fprintf(fp, "22.0342 147.984 ln\n");
  (void) fprintf(fp, "21.8818 147.984 ln\n");
  (void) fprintf(fp, "16.417 8.625 ln\n");
  (void) fprintf(fp, "16.5693 8.625 ln\n");
  (void) fprintf(fp, "0.233146 0.109579 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "16.7217 8.625 mo\n");
  (void) fprintf(fp, "22.1826 147.984 ln\n");
  (void) fprintf(fp, "22.0342 147.984 ln\n");
  (void) fprintf(fp, "16.5693 8.625 ln\n");
  (void) fprintf(fp, "16.7217 8.625 ln\n");
  (void) fprintf(fp, "0.236877 0.111332 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "16.8701 8.625 mo\n");
  (void) fprintf(fp, "22.335 147.984 ln\n");
  (void) fprintf(fp, "22.1826 147.984 ln\n");
  (void) fprintf(fp, "16.7217 8.625 ln\n");
  (void) fprintf(fp, "16.8701 8.625 ln\n");
  (void) fprintf(fp, "0.240634 0.113098 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "17.0225 8.625 mo\n");
  (void) fprintf(fp, "22.4834 147.98 ln\n");
  (void) fprintf(fp, "22.335 147.984 ln\n");
  (void) fprintf(fp, "16.8701 8.625 ln\n");
  (void) fprintf(fp, "17.0225 8.625 ln\n");
  (void) fprintf(fp, "0.244417 0.114876 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "17.1748 8.625 mo\n");
  (void) fprintf(fp, "22.6357 147.973 ln\n");
  (void) fprintf(fp, "22.4834 147.98 ln\n");
  (void) fprintf(fp, "17.0225 8.625 ln\n");
  (void) fprintf(fp, "17.1748 8.625 ln\n");
  (void) fprintf(fp, "0.248227 0.116667 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "17.3232 8.625 mo\n");
  (void) fprintf(fp, "22.7842 147.969 ln\n");
  (void) fprintf(fp, "22.6357 147.973 ln\n");
  (void) fprintf(fp, "17.1748 8.625 ln\n");
  (void) fprintf(fp, "17.3232 8.625 ln\n");
  (void) fprintf(fp, "0.252064 0.11847 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "17.4717 8.625 mo\n");
  (void) fprintf(fp, "22.9365 147.965 ln\n");
  (void) fprintf(fp, "22.7842 147.969 ln\n");
  (void) fprintf(fp, "17.3232 8.625 ln\n");
  (void) fprintf(fp, "17.4717 8.625 ln\n");
  (void) fprintf(fp, "0.255927 0.120286 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "17.624 8.625 mo\n");
  (void) fprintf(fp, "23.0889 147.957 ln\n");
  (void) fprintf(fp, "22.9365 147.965 ln\n");
  (void) fprintf(fp, "17.4717 8.625 ln\n");
  (void) fprintf(fp, "17.624 8.625 ln\n");
  (void) fprintf(fp, "0.259817 0.122114 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "17.7764 8.625 mo\n");
  (void) fprintf(fp, "23.2373 147.953 ln\n");
  (void) fprintf(fp, "23.0889 147.957 ln\n");
  (void) fprintf(fp, "17.624 8.625 ln\n");
  (void) fprintf(fp, "17.7764 8.625 ln\n");
  (void) fprintf(fp, "0.263732 0.123954 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "17.9248 8.625 mo\n");
  (void) fprintf(fp, "23.3896 147.949 ln\n");
  (void) fprintf(fp, "23.2373 147.953 ln\n");
  (void) fprintf(fp, "17.7764 8.625 ln\n");
  (void) fprintf(fp, "17.9248 8.625 ln\n");
  (void) fprintf(fp, "0.267675 0.125807 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "18.0771 8.625 mo\n");
  (void) fprintf(fp, "23.5381 147.941 ln\n");
  (void) fprintf(fp, "23.3896 147.949 ln\n");
  (void) fprintf(fp, "17.9248 8.625 ln\n");
  (void) fprintf(fp, "18.0771 8.625 ln\n");
  (void) fprintf(fp, "0.271643 0.127672 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "18.2295 8.625 mo\n");
  (void) fprintf(fp, "23.6904 147.938 ln\n");
  (void) fprintf(fp, "23.5381 147.941 ln\n");
  (void) fprintf(fp, "18.0771 8.625 ln\n");
  (void) fprintf(fp, "18.2295 8.625 ln\n");
  (void) fprintf(fp, "0.275638 0.12955 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "18.3779 8.625 mo\n");
  (void) fprintf(fp, "23.8389 147.934 ln\n");
  (void) fprintf(fp, "23.6904 147.938 ln\n");
  (void) fprintf(fp, "18.2295 8.625 ln\n");
  (void) fprintf(fp, "18.3779 8.625 ln\n");
  (void) fprintf(fp, "0.279659 0.13144 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "18.5303 8.625 mo\n");
  (void) fprintf(fp, "23.9912 147.93 ln\n");
  (void) fprintf(fp, "23.8389 147.934 ln\n");
  (void) fprintf(fp, "18.3779 8.625 ln\n");
  (void) fprintf(fp, "18.5303 8.625 ln\n");
  (void) fprintf(fp, "0.283706 0.133342 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "18.6826 8.625 mo\n");
  (void) fprintf(fp, "24.1436 147.926 ln\n");
  (void) fprintf(fp, "23.9912 147.93 ln\n");
  (void) fprintf(fp, "18.5303 8.625 ln\n");
  (void) fprintf(fp, "18.6826 8.625 ln\n");
  (void) fprintf(fp, "0.287779 0.135256 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "18.8311 8.625 mo\n");
  (void) fprintf(fp, "24.292 147.918 ln\n");
  (void) fprintf(fp, "24.1436 147.926 ln\n");
  (void) fprintf(fp, "18.6826 8.625 ln\n");
  (void) fprintf(fp, "18.8311 8.625 ln\n");
  (void) fprintf(fp, "0.291879 0.137183 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "18.9834 8.625 mo\n");
  (void) fprintf(fp, "24.4404 147.914 ln\n");
  (void) fprintf(fp, "24.292 147.918 ln\n");
  (void) fprintf(fp, "18.8311 8.625 ln\n");
  (void) fprintf(fp, "18.9834 8.625 ln\n");
  (void) fprintf(fp, "0.296004 0.139122 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "19.1318 8.625 mo\n");
  (void) fprintf(fp, "24.5928 147.91 ln\n");
  (void) fprintf(fp, "24.4404 147.914 ln\n");
  (void) fprintf(fp, "18.9834 8.625 ln\n");
  (void) fprintf(fp, "19.1318 8.625 ln\n");
  (void) fprintf(fp, "0.300155 0.141073 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "19.2842 8.625 mo\n");
  (void) fprintf(fp, "24.7451 147.902 ln\n");
  (void) fprintf(fp, "24.5928 147.91 ln\n");
  (void) fprintf(fp, "19.1318 8.625 ln\n");
  (void) fprintf(fp, "19.2842 8.625 ln\n");
  (void) fprintf(fp, "0.304333 0.143036 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "19.4365 8.625 mo\n");
  (void) fprintf(fp, "24.8936 147.898 ln\n");
  (void) fprintf(fp, "24.7451 147.902 ln\n");
  (void) fprintf(fp, "19.2842 8.625 ln\n");
  (void) fprintf(fp, "19.4365 8.625 ln\n");
  (void) fprintf(fp, "0.308536 0.145012 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "19.585 8.625 mo\n");
  (void) fprintf(fp, "25.0459 147.895 ln\n");
  (void) fprintf(fp, "24.8936 147.898 ln\n");
  (void) fprintf(fp, "19.4365 8.625 ln\n");
  (void) fprintf(fp, "19.585 8.625 ln\n");
  (void) fprintf(fp, "0.312765 0.147 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "19.7373 8.625 mo\n");
  (void) fprintf(fp, "25.1982 147.887 ln\n");
  (void) fprintf(fp, "25.0459 147.895 ln\n");
  (void) fprintf(fp, "19.585 8.625 ln\n");
  (void) fprintf(fp, "19.7373 8.625 ln\n");
  (void) fprintf(fp, "0.317021 0.149 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "19.8857 8.625 mo\n");
  (void) fprintf(fp, "25.3467 147.883 ln\n");
  (void) fprintf(fp, "25.1982 147.887 ln\n");
  (void) fprintf(fp, "19.7373 8.625 ln\n");
  (void) fprintf(fp, "19.8857 8.625 ln\n");
  (void) fprintf(fp, "0.321302 0.151012 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "20.0381 8.625 mo\n");
  (void) fprintf(fp, "25.4951 147.879 ln\n");
  (void) fprintf(fp, "25.3467 147.883 ln\n");
  (void) fprintf(fp, "19.8857 8.625 ln\n");
  (void) fprintf(fp, "20.0381 8.625 ln\n");
  (void) fprintf(fp, "0.325609 0.153036 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "20.1904 8.625 mo\n");
  (void) fprintf(fp, "25.6475 147.871 ln\n");
  (void) fprintf(fp, "25.4951 147.879 ln\n");
  (void) fprintf(fp, "20.0381 8.625 ln\n");
  (void) fprintf(fp, "20.1904 8.625 ln\n");
  (void) fprintf(fp, "0.329941 0.155072 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "20.3389 8.625 mo\n");
  (void) fprintf(fp, "25.7998 147.867 ln\n");
  (void) fprintf(fp, "25.6475 147.871 ln\n");
  (void) fprintf(fp, "20.1904 8.625 ln\n");
  (void) fprintf(fp, "20.3389 8.625 ln\n");
  (void) fprintf(fp, "0.3343 0.157121 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "20.4912 8.625 mo\n");
  (void) fprintf(fp, "25.9482 147.863 ln\n");
  (void) fprintf(fp, "25.7998 147.867 ln\n");
  (void) fprintf(fp, "20.3389 8.625 ln\n");
  (void) fprintf(fp, "20.4912 8.625 ln\n");
  (void) fprintf(fp, "0.338684 0.159181 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "20.6436 8.625 mo\n");
  (void) fprintf(fp, "26.1006 147.855 ln\n");
  (void) fprintf(fp, "25.9482 147.863 ln\n");
  (void) fprintf(fp, "20.4912 8.625 ln\n");
  (void) fprintf(fp, "20.6436 8.625 ln\n");
  (void) fprintf(fp, "0.343093 0.161254 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "20.792 8.625 mo\n");
  (void) fprintf(fp, "26.2529 147.852 ln\n");
  (void) fprintf(fp, "26.1006 147.855 ln\n");
  (void) fprintf(fp, "20.6436 8.625 ln\n");
  (void) fprintf(fp, "20.792 8.625 ln\n");
  (void) fprintf(fp, "0.347529 0.163339 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "20.9443 8.625 mo\n");
  (void) fprintf(fp, "26.4014 147.848 ln\n");
  (void) fprintf(fp, "26.2529 147.852 ln\n");
  (void) fprintf(fp, "20.792 8.625 ln\n");
  (void) fprintf(fp, "20.9443 8.625 ln\n");
  (void) fprintf(fp, "0.35199 0.165435 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "21.0928 8.625 mo\n");
  (void) fprintf(fp, "26.5498 147.84 ln\n");
  (void) fprintf(fp, "26.4014 147.848 ln\n");
  (void) fprintf(fp, "20.9443 8.625 ln\n");
  (void) fprintf(fp, "21.0928 8.625 ln\n");
  (void) fprintf(fp, "0.356476 0.167544 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "21.2451 8.625 mo\n");
  (void) fprintf(fp, "26.7021 147.836 ln\n");
  (void) fprintf(fp, "26.5498 147.84 ln\n");
  (void) fprintf(fp, "21.0928 8.625 ln\n");
  (void) fprintf(fp, "21.2451 8.625 ln\n");
  (void) fprintf(fp, "0.360988 0.169665 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "21.3936 8.625 mo\n");
  (void) fprintf(fp, "26.8545 147.832 ln\n");
  (void) fprintf(fp, "26.7021 147.836 ln\n");
  (void) fprintf(fp, "21.2451 8.625 ln\n");
  (void) fprintf(fp, "21.3936 8.625 ln\n");
  (void) fprintf(fp, "0.365526 0.171797 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "21.5459 8.625 mo\n");
  (void) fprintf(fp, "27.0029 147.828 ln\n");
  (void) fprintf(fp, "26.8545 147.832 ln\n");
  (void) fprintf(fp, "21.3936 8.625 ln\n");
  (void) fprintf(fp, "21.5459 8.625 ln\n");
  (void) fprintf(fp, "0.370089 0.173942 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "21.6982 8.625 mo\n");
  (void) fprintf(fp, "27.1553 147.824 ln\n");
  (void) fprintf(fp, "27.0029 147.828 ln\n");
  (void) fprintf(fp, "21.5459 8.625 ln\n");
  (void) fprintf(fp, "21.6982 8.625 ln\n");
  (void) fprintf(fp, "0.374678 0.176099 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "21.8467 8.625 mo\n");
  (void) fprintf(fp, "27.3037 147.816 ln\n");
  (void) fprintf(fp, "27.1553 147.824 ln\n");
  (void) fprintf(fp, "21.6982 8.625 ln\n");
  (void) fprintf(fp, "21.8467 8.625 ln\n");
  (void) fprintf(fp, "0.379292 0.178267 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "21.999 8.625 mo\n");
  (void) fprintf(fp, "27.4561 147.812 ln\n");
  (void) fprintf(fp, "27.3037 147.816 ln\n");
  (void) fprintf(fp, "21.8467 8.625 ln\n");
  (void) fprintf(fp, "21.999 8.625 ln\n");
  (void) fprintf(fp, "0.383931 0.180448 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "22.1514 8.625 mo\n");
  (void) fprintf(fp, "27.6045 147.809 ln\n");
  (void) fprintf(fp, "27.4561 147.812 ln\n");
  (void) fprintf(fp, "21.999 8.625 ln\n");
  (void) fprintf(fp, "22.1514 8.625 ln\n");
  (void) fprintf(fp, "0.388596 0.18264 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "22.2998 8.625 mo\n");
  (void) fprintf(fp, "27.7568 147.801 ln\n");
  (void) fprintf(fp, "27.6045 147.809 ln\n");
  (void) fprintf(fp, "22.1514 8.625 ln\n");
  (void) fprintf(fp, "22.2998 8.625 ln\n");
  (void) fprintf(fp, "0.393286 0.184844 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "22.4521 8.625 mo\n");
  (void) fprintf(fp, "27.9092 147.797 ln\n");
  (void) fprintf(fp, "27.7568 147.801 ln\n");
  (void) fprintf(fp, "22.2998 8.625 ln\n");
  (void) fprintf(fp, "22.4521 8.625 ln\n");
  (void) fprintf(fp, "0.398001 0.187061 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "22.6045 8.625 mo\n");
  (void) fprintf(fp, "28.0576 147.793 ln\n");
  (void) fprintf(fp, "27.9092 147.797 ln\n");
  (void) fprintf(fp, "22.4521 8.625 ln\n");
  (void) fprintf(fp, "22.6045 8.625 ln\n");
  (void) fprintf(fp, "0.402742 0.189289 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "22.7529 8.625 mo\n");
  (void) fprintf(fp, "28.21 147.785 ln\n");
  (void) fprintf(fp, "28.0576 147.793 ln\n");
  (void) fprintf(fp, "22.6045 8.625 ln\n");
  (void) fprintf(fp, "22.7529 8.625 ln\n");
  (void) fprintf(fp, "0.407508 0.191529 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "22.9053 8.625 mo\n");
  (void) fprintf(fp, "28.3584 147.781 ln\n");
  (void) fprintf(fp, "28.21 147.785 ln\n");
  (void) fprintf(fp, "22.7529 8.625 ln\n");
  (void) fprintf(fp, "22.9053 8.625 ln\n");
  (void) fprintf(fp, "0.412299 0.19378 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "23.0576 8.625 mo\n");
  (void) fprintf(fp, "28.5107 147.777 ln\n");
  (void) fprintf(fp, "28.3584 147.781 ln\n");
  (void) fprintf(fp, "22.9053 8.625 ln\n");
  (void) fprintf(fp, "23.0576 8.625 ln\n");
  (void) fprintf(fp, "0.417115 0.196044 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "23.2061 8.625 mo\n");
  (void) fprintf(fp, "28.6592 147.77 ln\n");
  (void) fprintf(fp, "28.5107 147.777 ln\n");
  (void) fprintf(fp, "23.0576 8.625 ln\n");
  (void) fprintf(fp, "23.2061 8.625 ln\n");
  (void) fprintf(fp, "0.421956 0.198319 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "23.3545 8.625 mo\n");
  (void) fprintf(fp, "28.8115 147.766 ln\n");
  (void) fprintf(fp, "28.6592 147.77 ln\n");
  (void) fprintf(fp, "23.2061 8.625 ln\n");
  (void) fprintf(fp, "23.3545 8.625 ln\n");
  (void) fprintf(fp, "0.426823 0.200607 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "23.5068 8.625 mo\n");
  (void) fprintf(fp, "28.9639 147.762 ln\n");
  (void) fprintf(fp, "28.8115 147.766 ln\n");
  (void) fprintf(fp, "23.3545 8.625 ln\n");
  (void) fprintf(fp, "23.5068 8.625 ln\n");
  (void) fprintf(fp, "0.431714 0.202906 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "23.6592 8.625 mo\n");
  (void) fprintf(fp, "29.1123 147.754 ln\n");
  (void) fprintf(fp, "28.9639 147.762 ln\n");
  (void) fprintf(fp, "23.5068 8.625 ln\n");
  (void) fprintf(fp, "23.6592 8.625 ln\n");
  (void) fprintf(fp, "0.436631 0.205217 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "23.8076 8.625 mo\n");
  (void) fprintf(fp, "29.2646 147.75 ln\n");
  (void) fprintf(fp, "29.1123 147.754 ln\n");
  (void) fprintf(fp, "23.6592 8.625 ln\n");
  (void) fprintf(fp, "23.8076 8.625 ln\n");
  (void) fprintf(fp, "0.441573 0.207539 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "23.96 8.625 mo\n");
  (void) fprintf(fp, "29.4131 147.746 ln\n");
  (void) fprintf(fp, "29.2646 147.75 ln\n");
  (void) fprintf(fp, "23.8076 8.625 ln\n");
  (void) fprintf(fp, "23.96 8.625 ln\n");
  (void) fprintf(fp, "0.446539 0.209873 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "24.1123 8.625 mo\n");
  (void) fprintf(fp, "29.5654 147.738 ln\n");
  (void) fprintf(fp, "29.4131 147.746 ln\n");
  (void) fprintf(fp, "23.96 8.625 ln\n");
  (void) fprintf(fp, "24.1123 8.625 ln\n");
  (void) fprintf(fp, "0.451531 0.212219 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "24.2607 8.625 mo\n");
  (void) fprintf(fp, "29.7139 147.734 ln\n");
  (void) fprintf(fp, "29.5654 147.738 ln\n");
  (void) fprintf(fp, "24.1123 8.625 ln\n");
  (void) fprintf(fp, "24.2607 8.625 ln\n");
  (void) fprintf(fp, "0.456547 0.214577 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "24.4131 8.625 mo\n");
  (void) fprintf(fp, "29.8662 147.73 ln\n");
  (void) fprintf(fp, "29.7139 147.734 ln\n");
  (void) fprintf(fp, "24.2607 8.625 ln\n");
  (void) fprintf(fp, "24.4131 8.625 ln\n");
  (void) fprintf(fp, "0.461589 0.216947 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "24.5654 8.625 mo\n");
  (void) fprintf(fp, "30.0186 147.727 ln\n");
  (void) fprintf(fp, "29.8662 147.73 ln\n");
  (void) fprintf(fp, "24.4131 8.625 ln\n");
  (void) fprintf(fp, "24.5654 8.625 ln\n");
  (void) fprintf(fp, "0.466655 0.219328 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "24.7139 8.625 mo\n");
  (void) fprintf(fp, "30.167 147.723 ln\n");
  (void) fprintf(fp, "30.0186 147.727 ln\n");
  (void) fprintf(fp, "24.5654 8.625 ln\n");
  (void) fprintf(fp, "24.7139 8.625 ln\n");
  (void) fprintf(fp, "0.471746 0.221721 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "24.8662 8.625 mo\n");
  (void) fprintf(fp, "30.3193 147.715 ln\n");
  (void) fprintf(fp, "30.167 147.723 ln\n");
  (void) fprintf(fp, "24.7139 8.625 ln\n");
  (void) fprintf(fp, "24.8662 8.625 ln\n");
  (void) fprintf(fp, "0.476862 0.224125 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "25.0146 8.625 mo\n");
  (void) fprintf(fp, "30.4678 147.711 ln\n");
  (void) fprintf(fp, "30.3193 147.715 ln\n");
  (void) fprintf(fp, "24.8662 8.625 ln\n");
  (void) fprintf(fp, "25.0146 8.625 ln\n");
  (void) fprintf(fp, "0.482003 0.226542 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "25.167 8.625 mo\n");
  (void) fprintf(fp, "30.6201 147.707 ln\n");
  (void) fprintf(fp, "30.4678 147.711 ln\n");
  (void) fprintf(fp, "25.0146 8.625 ln\n");
  (void) fprintf(fp, "25.167 8.625 ln\n");
  (void) fprintf(fp, "0.487169 0.228969 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "25.3193 8.625 mo\n");
  (void) fprintf(fp, "30.7686 147.699 ln\n");
  (void) fprintf(fp, "30.6201 147.707 ln\n");
  (void) fprintf(fp, "25.167 8.625 ln\n");
  (void) fprintf(fp, "25.3193 8.625 ln\n");
  (void) fprintf(fp, "0.492359 0.231409 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "25.4678 8.625 mo\n");
  (void) fprintf(fp, "30.9209 147.695 ln\n");
  (void) fprintf(fp, "30.7686 147.699 ln\n");
  (void) fprintf(fp, "25.3193 8.625 ln\n");
  (void) fprintf(fp, "25.4678 8.625 ln\n");
  (void) fprintf(fp, "0.497575 0.23386 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "25.6201 8.625 mo\n");
  (void) fprintf(fp, "31.0732 147.691 ln\n");
  (void) fprintf(fp, "30.9209 147.695 ln\n");
  (void) fprintf(fp, "25.4678 8.625 ln\n");
  (void) fprintf(fp, "25.6201 8.625 ln\n");
  (void) fprintf(fp, "0.502814 0.236323 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "25.7686 8.625 mo\n");
  (void) fprintf(fp, "31.2217 147.684 ln\n");
  (void) fprintf(fp, "31.0732 147.691 ln\n");
  (void) fprintf(fp, "25.6201 8.625 ln\n");
  (void) fprintf(fp, "25.7686 8.625 ln\n");
  (void) fprintf(fp, "0.508079 0.238797 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "25.9209 8.625 mo\n");
  (void) fprintf(fp, "31.3701 147.68 ln\n");
  (void) fprintf(fp, "31.2217 147.684 ln\n");
  (void) fprintf(fp, "25.7686 8.625 ln\n");
  (void) fprintf(fp, "25.9209 8.625 ln\n");
  (void) fprintf(fp, "0.513368 0.241283 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "26.0732 8.625 mo\n");
  (void) fprintf(fp, "31.5225 147.676 ln\n");
  (void) fprintf(fp, "31.3701 147.68 ln\n");
  (void) fprintf(fp, "25.9209 8.625 ln\n");
  (void) fprintf(fp, "26.0732 8.625 ln\n");
  (void) fprintf(fp, "0.518682 0.243781 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "26.2217 8.625 mo\n");
  (void) fprintf(fp, "31.6748 147.668 ln\n");
  (void) fprintf(fp, "31.5225 147.676 ln\n");
  (void) fprintf(fp, "26.0732 8.625 ln\n");
  (void) fprintf(fp, "26.2217 8.625 ln\n");
  (void) fprintf(fp, "0.52402 0.24629 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "26.374 8.625 mo\n");
  (void) fprintf(fp, "31.8232 147.664 ln\n");
  (void) fprintf(fp, "31.6748 147.668 ln\n");
  (void) fprintf(fp, "26.2217 8.625 ln\n");
  (void) fprintf(fp, "26.374 8.625 ln\n");
  (void) fprintf(fp, "0.529383 0.24881 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "26.5264 8.625 mo\n");
  (void) fprintf(fp, "31.9756 147.66 ln\n");
  (void) fprintf(fp, "31.8232 147.664 ln\n");
  (void) fprintf(fp, "26.374 8.625 ln\n");
  (void) fprintf(fp, "26.5264 8.625 ln\n");
  (void) fprintf(fp, "0.534771 0.251342 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "26.6748 8.625 mo\n");
  (void) fprintf(fp, "32.1279 147.652 ln\n");
  (void) fprintf(fp, "31.9756 147.66 ln\n");
  (void) fprintf(fp, "26.5264 8.625 ln\n");
  (void) fprintf(fp, "26.6748 8.625 ln\n");
  (void) fprintf(fp, "0.540183 0.253886 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "26.8271 8.625 mo\n");
  (void) fprintf(fp, "32.2764 147.648 ln\n");
  (void) fprintf(fp, "32.1279 147.652 ln\n");
  (void) fprintf(fp, "26.6748 8.625 ln\n");
  (void) fprintf(fp, "26.8271 8.625 ln\n");
  (void) fprintf(fp, "0.545619 0.256441 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "26.9795 8.625 mo\n");
  (void) fprintf(fp, "32.4248 147.645 ln\n");
  (void) fprintf(fp, "32.2764 147.648 ln\n");
  (void) fprintf(fp, "26.8271 8.625 ln\n");
  (void) fprintf(fp, "26.9795 8.625 ln\n");
  (void) fprintf(fp, "0.551081 0.259008 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "27.1279 8.625 mo\n");
  (void) fprintf(fp, "32.5771 147.637 ln\n");
  (void) fprintf(fp, "32.4248 147.645 ln\n");
  (void) fprintf(fp, "26.9795 8.625 ln\n");
  (void) fprintf(fp, "27.1279 8.625 ln\n");
  (void) fprintf(fp, "0.556566 0.261586 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "27.2764 8.625 mo\n");
  (void) fprintf(fp, "32.7295 147.633 ln\n");
  (void) fprintf(fp, "32.5771 147.637 ln\n");
  (void) fprintf(fp, "27.1279 8.625 ln\n");
  (void) fprintf(fp, "27.2764 8.625 ln\n");
  (void) fprintf(fp, "0.562076 0.264176 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "27.4287 8.625 mo\n");
  (void) fprintf(fp, "32.8779 147.629 ln\n");
  (void) fprintf(fp, "32.7295 147.633 ln\n");
  (void) fprintf(fp, "27.2764 8.625 ln\n");
  (void) fprintf(fp, "27.4287 8.625 ln\n");
  (void) fprintf(fp, "0.56761 0.266777 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "27.5811 8.625 mo\n");
  (void) fprintf(fp, "33.0303 147.625 ln\n");
  (void) fprintf(fp, "32.8779 147.629 ln\n");
  (void) fprintf(fp, "27.4287 8.625 ln\n");
  (void) fprintf(fp, "27.5811 8.625 ln\n");
  (void) fprintf(fp, "0.573169 0.269389 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "27.7295 8.625 mo\n");
  (void) fprintf(fp, "33.1826 147.621 ln\n");
  (void) fprintf(fp, "33.0303 147.625 ln\n");
  (void) fprintf(fp, "27.5811 8.625 ln\n");
  (void) fprintf(fp, "27.7295 8.625 ln\n");
  (void) fprintf(fp, "0.578752 0.272013 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "27.8818 8.625 mo\n");
  (void) fprintf(fp, "33.3311 147.613 ln\n");
  (void) fprintf(fp, "33.1826 147.621 ln\n");
  (void) fprintf(fp, "27.7295 8.625 ln\n");
  (void) fprintf(fp, "27.8818 8.625 ln\n");
  (void) fprintf(fp, "0.584359 0.274649 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "28.0342 8.625 mo\n");
  (void) fprintf(fp, "33.4795 147.609 ln\n");
  (void) fprintf(fp, "33.3311 147.613 ln\n");
  (void) fprintf(fp, "27.8818 8.625 ln\n");
  (void) fprintf(fp, "28.0342 8.625 ln\n");
  (void) fprintf(fp, "0.589991 0.277296 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "28.1826 8.625 mo\n");
  (void) fprintf(fp, "33.6318 147.605 ln\n");
  (void) fprintf(fp, "33.4795 147.609 ln\n");
  (void) fprintf(fp, "28.0342 8.625 ln\n");
  (void) fprintf(fp, "28.1826 8.625 ln\n");
  (void) fprintf(fp, "0.595647 0.279954 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "28.335 8.625 mo\n");
  (void) fprintf(fp, "33.7842 147.598 ln\n");
  (void) fprintf(fp, "33.6318 147.605 ln\n");
  (void) fprintf(fp, "28.1826 8.625 ln\n");
  (void) fprintf(fp, "28.335 8.625 ln\n");
  (void) fprintf(fp, "0.601327 0.282624 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "28.4873 8.625 mo\n");
  (void) fprintf(fp, "33.9326 147.594 ln\n");
  (void) fprintf(fp, "33.7842 147.598 ln\n");
  (void) fprintf(fp, "28.335 8.625 ln\n");
  (void) fprintf(fp, "28.4873 8.625 ln\n");
  (void) fprintf(fp, "0.607032 0.285305 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "28.6357 8.625 mo\n");
  (void) fprintf(fp, "34.085 147.59 ln\n");
  (void) fprintf(fp, "33.9326 147.594 ln\n");
  (void) fprintf(fp, "28.4873 8.625 ln\n");
  (void) fprintf(fp, "28.6357 8.625 ln\n");
  (void) fprintf(fp, "0.61276 0.287997 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "28.7881 8.625 mo\n");
  (void) fprintf(fp, "34.2334 147.582 ln\n");
  (void) fprintf(fp, "34.085 147.59 ln\n");
  (void) fprintf(fp, "28.6357 8.625 ln\n");
  (void) fprintf(fp, "28.7881 8.625 ln\n");
  (void) fprintf(fp, "0.618513 0.290701 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "28.9404 8.625 mo\n");
  (void) fprintf(fp, "34.3857 147.578 ln\n");
  (void) fprintf(fp, "34.2334 147.582 ln\n");
  (void) fprintf(fp, "28.7881 8.625 ln\n");
  (void) fprintf(fp, "28.9404 8.625 ln\n");
  (void) fprintf(fp, "0.62429 0.293416 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "29.0889 8.625 mo\n");
  (void) fprintf(fp, "34.5342 147.574 ln\n");
  (void) fprintf(fp, "34.3857 147.578 ln\n");
  (void) fprintf(fp, "28.9404 8.625 ln\n");
  (void) fprintf(fp, "29.0889 8.625 ln\n");
  (void) fprintf(fp, "0.630091 0.296143 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "29.2412 8.625 mo\n");
  (void) fprintf(fp, "34.6865 147.566 ln\n");
  (void) fprintf(fp, "34.5342 147.574 ln\n");
  (void) fprintf(fp, "29.0889 8.625 ln\n");
  (void) fprintf(fp, "29.2412 8.625 ln\n");
  (void) fprintf(fp, "0.635917 0.298881 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "29.3896 8.625 mo\n");
  (void) fprintf(fp, "34.8389 147.562 ln\n");
  (void) fprintf(fp, "34.6865 147.566 ln\n");
  (void) fprintf(fp, "29.2412 8.625 ln\n");
  (void) fprintf(fp, "29.3896 8.625 ln\n");
  (void) fprintf(fp, "0.641766 0.30163 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "29.542 8.625 mo\n");
  (void) fprintf(fp, "34.9873 147.559 ln\n");
  (void) fprintf(fp, "34.8389 147.562 ln\n");
  (void) fprintf(fp, "29.3896 8.625 ln\n");
  (void) fprintf(fp, "29.542 8.625 ln\n");
  (void) fprintf(fp, "0.64764 0.304391 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "29.6904 8.625 mo\n");
  (void) fprintf(fp, "35.1396 147.551 ln\n");
  (void) fprintf(fp, "34.9873 147.559 ln\n");
  (void) fprintf(fp, "29.542 8.625 ln\n");
  (void) fprintf(fp, "29.6904 8.625 ln\n");
  (void) fprintf(fp, "0.653537 0.307163 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "29.8428 8.625 mo\n");
  (void) fprintf(fp, "35.2881 147.547 ln\n");
  (void) fprintf(fp, "35.1396 147.551 ln\n");
  (void) fprintf(fp, "29.6904 8.625 ln\n");
  (void) fprintf(fp, "29.8428 8.625 ln\n");
  (void) fprintf(fp, "0.659459 0.309946 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "29.9951 8.625 mo\n");
  (void) fprintf(fp, "35.4404 147.543 ln\n");
  (void) fprintf(fp, "35.2881 147.547 ln\n");
  (void) fprintf(fp, "29.8428 8.625 ln\n");
  (void) fprintf(fp, "29.9951 8.625 ln\n");
  (void) fprintf(fp, "0.665404 0.31274 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "30.1436 8.625 mo\n");
  (void) fprintf(fp, "35.5889 147.535 ln\n");
  (void) fprintf(fp, "35.4404 147.543 ln\n");
  (void) fprintf(fp, "29.9951 8.625 ln\n");
  (void) fprintf(fp, "30.1436 8.625 ln\n");
  (void) fprintf(fp, "0.671374 0.315546 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "30.2959 8.625 mo\n");
  (void) fprintf(fp, "35.7412 147.531 ln\n");
  (void) fprintf(fp, "35.5889 147.535 ln\n");
  (void) fprintf(fp, "30.1436 8.625 ln\n");
  (void) fprintf(fp, "30.2959 8.625 ln\n");
  (void) fprintf(fp, "0.677368 0.318363 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "30.4482 8.625 mo\n");
  (void) fprintf(fp, "35.8936 147.527 ln\n");
  (void) fprintf(fp, "35.7412 147.531 ln\n");
  (void) fprintf(fp, "30.2959 8.625 ln\n");
  (void) fprintf(fp, "30.4482 8.625 ln\n");
  (void) fprintf(fp, "0.683385 0.321191 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "30.5967 8.625 mo\n");
  (void) fprintf(fp, "36.042 147.523 ln\n");
  (void) fprintf(fp, "35.8936 147.527 ln\n");
  (void) fprintf(fp, "30.4482 8.625 ln\n");
  (void) fprintf(fp, "30.5967 8.625 ln\n");
  (void) fprintf(fp, "0.689426 0.32403 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "30.749 8.625 mo\n");
  (void) fprintf(fp, "36.1943 147.52 ln\n");
  (void) fprintf(fp, "36.042 147.523 ln\n");
  (void) fprintf(fp, "30.5967 8.625 ln\n");
  (void) fprintf(fp, "30.749 8.625 ln\n");
  (void) fprintf(fp, "0.695492 0.326881 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "30.8975 8.625 mo\n");
  (void) fprintf(fp, "36.3428 147.512 ln\n");
  (void) fprintf(fp, "36.1943 147.52 ln\n");
  (void) fprintf(fp, "30.749 8.625 ln\n");
  (void) fprintf(fp, "30.8975 8.625 ln\n");
  (void) fprintf(fp, "0.701581 0.329743 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "31.0498 8.625 mo\n");
  (void) fprintf(fp, "36.4951 147.508 ln\n");
  (void) fprintf(fp, "36.3428 147.512 ln\n");
  (void) fprintf(fp, "30.8975 8.625 ln\n");
  (void) fprintf(fp, "31.0498 8.625 ln\n");
  (void) fprintf(fp, "0.707694 0.332616 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "31.2021 8.625 mo\n");
  (void) fprintf(fp, "36.6436 147.504 ln\n");
  (void) fprintf(fp, "36.4951 147.508 ln\n");
  (void) fprintf(fp, "31.0498 8.625 ln\n");
  (void) fprintf(fp, "31.2021 8.625 ln\n");
  (void) fprintf(fp, "0.713831 0.335501 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "31.3506 8.625 mo\n");
  (void) fprintf(fp, "36.7959 147.496 ln\n");
  (void) fprintf(fp, "36.6436 147.504 ln\n");
  (void) fprintf(fp, "31.2021 8.625 ln\n");
  (void) fprintf(fp, "31.3506 8.625 ln\n");
  (void) fprintf(fp, "0.719992 0.338396 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "31.5029 8.625 mo\n");
  (void) fprintf(fp, "36.9482 147.492 ln\n");
  (void) fprintf(fp, "36.7959 147.496 ln\n");
  (void) fprintf(fp, "31.3506 8.625 ln\n");
  (void) fprintf(fp, "31.5029 8.625 ln\n");
  (void) fprintf(fp, "0.726177 0.341303 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "31.6514 8.625 mo\n");
  (void) fprintf(fp, "37.0967 147.488 ln\n");
  (void) fprintf(fp, "36.9482 147.492 ln\n");
  (void) fprintf(fp, "31.5029 8.625 ln\n");
  (void) fprintf(fp, "31.6514 8.625 ln\n");
  (void) fprintf(fp, "0.732385 0.344221 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "31.8037 8.625 mo\n");
  (void) fprintf(fp, "37.249 147.48 ln\n");
  (void) fprintf(fp, "37.0967 147.488 ln\n");
  (void) fprintf(fp, "31.6514 8.625 ln\n");
  (void) fprintf(fp, "31.8037 8.625 ln\n");
  (void) fprintf(fp, "0.738617 0.34715 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "31.9561 8.625 mo\n");
  (void) fprintf(fp, "37.3975 147.477 ln\n");
  (void) fprintf(fp, "37.249 147.48 ln\n");
  (void) fprintf(fp, "31.8037 8.625 ln\n");
  (void) fprintf(fp, "31.9561 8.625 ln\n");
  (void) fprintf(fp, "0.744873 0.35009 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "32.1045 8.625 mo\n");
  (void) fprintf(fp, "37.5498 147.473 ln\n");
  (void) fprintf(fp, "37.3975 147.477 ln\n");
  (void) fprintf(fp, "31.9561 8.625 ln\n");
  (void) fprintf(fp, "32.1045 8.625 ln\n");
  (void) fprintf(fp, "0.751153 0.353042 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "32.2568 8.625 mo\n");
  (void) fprintf(fp, "37.6982 147.465 ln\n");
  (void) fprintf(fp, "37.5498 147.473 ln\n");
  (void) fprintf(fp, "32.1045 8.625 ln\n");
  (void) fprintf(fp, "32.2568 8.625 ln\n");
  (void) fprintf(fp, "0.757456 0.356004 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "32.4092 8.625 mo\n");
  (void) fprintf(fp, "37.8506 147.461 ln\n");
  (void) fprintf(fp, "37.6982 147.465 ln\n");
  (void) fprintf(fp, "32.2568 8.625 ln\n");
  (void) fprintf(fp, "32.4092 8.625 ln\n");
  (void) fprintf(fp, "0.763783 0.358978 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "32.5576 8.625 mo\n");
  (void) fprintf(fp, "38.0029 147.457 ln\n");
  (void) fprintf(fp, "37.8506 147.461 ln\n");
  (void) fprintf(fp, "32.4092 8.625 ln\n");
  (void) fprintf(fp, "32.5576 8.625 ln\n");
  (void) fprintf(fp, "0.770133 0.361963 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "32.71 8.625 mo\n");
  (void) fprintf(fp, "38.1514 147.449 ln\n");
  (void) fprintf(fp, "38.0029 147.457 ln\n");
  (void) fprintf(fp, "32.5576 8.625 ln\n");
  (void) fprintf(fp, "32.71 8.625 ln\n");
  (void) fprintf(fp, "0.776508 0.364959 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "32.8623 8.625 mo\n");
  (void) fprintf(fp, "38.2998 147.445 ln\n");
  (void) fprintf(fp, "38.1514 147.449 ln\n");
  (void) fprintf(fp, "32.71 8.625 ln\n");
  (void) fprintf(fp, "32.8623 8.625 ln\n");
  (void) fprintf(fp, "0.782906 0.367966 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "33.0107 8.625 mo\n");
  (void) fprintf(fp, "38.4521 147.441 ln\n");
  (void) fprintf(fp, "38.2998 147.445 ln\n");
  (void) fprintf(fp, "32.8623 8.625 ln\n");
  (void) fprintf(fp, "33.0107 8.625 ln\n");
  (void) fprintf(fp, "0.789327 0.370984 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "33.1592 8.625 mo\n");
  (void) fprintf(fp, "38.6045 147.434 ln\n");
  (void) fprintf(fp, "38.4521 147.441 ln\n");
  (void) fprintf(fp, "33.0107 8.625 ln\n");
  (void) fprintf(fp, "33.1592 8.625 ln\n");
  (void) fprintf(fp, "0.795772 0.374013 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "33.3154 8.625 mo\n");
  (void) fprintf(fp, "38.7529 147.43 ln\n");
  (void) fprintf(fp, "38.6045 147.434 ln\n");
  (void) fprintf(fp, "33.1592 8.625 ln\n");
  (void) fprintf(fp, "33.3154 8.625 ln\n");
  (void) fprintf(fp, "0.802241 0.377053 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "33.4639 8.625 mo\n");
  (void) fprintf(fp, "38.9053 147.426 ln\n");
  (void) fprintf(fp, "38.7529 147.43 ln\n");
  (void) fprintf(fp, "33.3154 8.625 ln\n");
  (void) fprintf(fp, "33.4639 8.625 ln\n");
  (void) fprintf(fp, "0.808733 0.380105 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "33.6123 8.625 mo\n");
  (void) fprintf(fp, "39.0576 147.422 ln\n");
  (void) fprintf(fp, "38.9053 147.426 ln\n");
  (void) fprintf(fp, "33.4639 8.625 ln\n");
  (void) fprintf(fp, "33.6123 8.625 ln\n");
  (void) fprintf(fp, "0.815249 0.383167 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "33.7646 8.625 mo\n");
  (void) fprintf(fp, "39.2061 147.414 ln\n");
  (void) fprintf(fp, "39.0576 147.422 ln\n");
  (void) fprintf(fp, "33.6123 8.625 ln\n");
  (void) fprintf(fp, "33.7646 8.625 ln\n");
  (void) fprintf(fp, "0.821788 0.38624 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "33.917 8.625 mo\n");
  (void) fprintf(fp, "39.3545 147.41 ln\n");
  (void) fprintf(fp, "39.2061 147.414 ln\n");
  (void) fprintf(fp, "33.7646 8.625 ln\n");
  (void) fprintf(fp, "33.917 8.625 ln\n");
  (void) fprintf(fp, "0.828351 0.389325 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "34.0654 8.625 mo\n");
  (void) fprintf(fp, "39.5068 147.406 ln\n");
  (void) fprintf(fp, "39.3545 147.41 ln\n");
  (void) fprintf(fp, "33.917 8.625 ln\n");
  (void) fprintf(fp, "34.0654 8.625 ln\n");
  (void) fprintf(fp, "0.834937 0.39242 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "34.2178 8.625 mo\n");
  (void) fprintf(fp, "39.6592 147.402 ln\n");
  (void) fprintf(fp, "39.5068 147.406 ln\n");
  (void) fprintf(fp, "34.0654 8.625 ln\n");
  (void) fprintf(fp, "34.2178 8.625 ln\n");
  (void) fprintf(fp, "0.841547 0.395527 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "34.3701 8.625 mo\n");
  (void) fprintf(fp, "39.8076 147.395 ln\n");
  (void) fprintf(fp, "39.6592 147.402 ln\n");
  (void) fprintf(fp, "34.2178 8.625 ln\n");
  (void) fprintf(fp, "34.3701 8.625 ln\n");
  (void) fprintf(fp, "0.84818 0.398644 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "34.5186 8.625 mo\n");
  (void) fprintf(fp, "39.96 147.391 ln\n");
  (void) fprintf(fp, "39.8076 147.395 ln\n");
  (void) fprintf(fp, "34.3701 8.625 ln\n");
  (void) fprintf(fp, "34.5186 8.625 ln\n");
  (void) fprintf(fp, "0.854836 0.401773 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "34.6709 8.625 mo\n");
  (void) fprintf(fp, "40.1123 147.387 ln\n");
  (void) fprintf(fp, "39.96 147.391 ln\n");
  (void) fprintf(fp, "34.5186 8.625 ln\n");
  (void) fprintf(fp, "34.6709 8.625 ln\n");
  (void) fprintf(fp, "0.861516 0.404913 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "34.8232 8.625 mo\n");
  (void) fprintf(fp, "40.2607 147.379 ln\n");
  (void) fprintf(fp, "40.1123 147.387 ln\n");
  (void) fprintf(fp, "34.6709 8.625 ln\n");
  (void) fprintf(fp, "34.8232 8.625 ln\n");
  (void) fprintf(fp, "0.868219 0.408063 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "34.9717 8.625 mo\n");
  (void) fprintf(fp, "40.4092 147.375 ln\n");
  (void) fprintf(fp, "40.2607 147.379 ln\n");
  (void) fprintf(fp, "34.8232 8.625 ln\n");
  (void) fprintf(fp, "34.9717 8.625 ln\n");
  (void) fprintf(fp, "0.874946 0.411225 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "35.124 8.625 mo\n");
  (void) fprintf(fp, "40.5615 147.371 ln\n");
  (void) fprintf(fp, "40.4092 147.375 ln\n");
  (void) fprintf(fp, "34.9717 8.625 ln\n");
  (void) fprintf(fp, "35.124 8.625 ln\n");
  (void) fprintf(fp, "0.881696 0.414397 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "35.2725 8.625 mo\n");
  (void) fprintf(fp, "40.7139 147.363 ln\n");
  (void) fprintf(fp, "40.5615 147.371 ln\n");
  (void) fprintf(fp, "35.124 8.625 ln\n");
  (void) fprintf(fp, "35.2725 8.625 ln\n");
  (void) fprintf(fp, "0.888469 0.417581 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "35.4248 8.625 mo\n");
  (void) fprintf(fp, "40.8623 147.359 ln\n");
  (void) fprintf(fp, "40.7139 147.363 ln\n");
  (void) fprintf(fp, "35.2725 8.625 ln\n");
  (void) fprintf(fp, "35.4248 8.625 ln\n");
  (void) fprintf(fp, "0.895266 0.420775 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "35.5732 8.625 mo\n");
  (void) fprintf(fp, "41.0146 147.355 ln\n");
  (void) fprintf(fp, "40.8623 147.359 ln\n");
  (void) fprintf(fp, "35.4248 8.625 ln\n");
  (void) fprintf(fp, "35.5732 8.625 ln\n");
  (void) fprintf(fp, "0.902086 0.42398 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "35.7256 8.625 mo\n");
  (void) fprintf(fp, "41.1631 147.348 ln\n");
  (void) fprintf(fp, "41.0146 147.355 ln\n");
  (void) fprintf(fp, "35.5732 8.625 ln\n");
  (void) fprintf(fp, "35.7256 8.625 ln\n");
  (void) fprintf(fp, "0.908929 0.427197 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "35.8779 8.625 mo\n");
  (void) fprintf(fp, "41.3154 147.344 ln\n");
  (void) fprintf(fp, "41.1631 147.348 ln\n");
  (void) fprintf(fp, "35.7256 8.625 ln\n");
  (void) fprintf(fp, "35.8779 8.625 ln\n");
  (void) fprintf(fp, "0.915796 0.430424 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "36.0264 8.625 mo\n");
  (void) fprintf(fp, "41.4639 147.34 ln\n");
  (void) fprintf(fp, "41.3154 147.344 ln\n");
  (void) fprintf(fp, "35.8779 8.625 ln\n");
  (void) fprintf(fp, "36.0264 8.625 ln\n");
  (void) fprintf(fp, "0.922685 0.433662 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "36.1787 8.625 mo\n");
  (void) fprintf(fp, "41.6162 147.332 ln\n");
  (void) fprintf(fp, "41.4639 147.34 ln\n");
  (void) fprintf(fp, "36.0264 8.625 ln\n");
  (void) fprintf(fp, "36.1787 8.625 ln\n");
  (void) fprintf(fp, "0.929598 0.436911 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "36.3311 8.625 mo\n");
  (void) fprintf(fp, "41.7686 147.328 ln\n");
  (void) fprintf(fp, "41.6162 147.332 ln\n");
  (void) fprintf(fp, "36.1787 8.625 ln\n");
  (void) fprintf(fp, "36.3311 8.625 ln\n");
  (void) fprintf(fp, "0.936534 0.440171 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "36.4795 8.625 mo\n");
  (void) fprintf(fp, "41.917 147.324 ln\n");
  (void) fprintf(fp, "41.7686 147.328 ln\n");
  (void) fprintf(fp, "36.3311 8.625 ln\n");
  (void) fprintf(fp, "36.4795 8.625 ln\n");
  (void) fprintf(fp, "0.943493 0.443442 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "36.6318 8.625 mo\n");
  (void) fprintf(fp, "42.0693 147.32 ln\n");
  (void) fprintf(fp, "41.917 147.324 ln\n");
  (void) fprintf(fp, "36.4795 8.625 ln\n");
  (void) fprintf(fp, "36.6318 8.625 ln\n");
  (void) fprintf(fp, "0.950476 0.446724 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "36.7842 8.625 mo\n");
  (void) fprintf(fp, "42.2178 147.312 ln\n");
  (void) fprintf(fp, "42.0693 147.32 ln\n");
  (void) fprintf(fp, "36.6318 8.625 ln\n");
  (void) fprintf(fp, "36.7842 8.625 ln\n");
  (void) fprintf(fp, "0.957481 0.450016 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "36.9326 8.625 mo\n");
  (void) fprintf(fp, "42.3701 147.309 ln\n");
  (void) fprintf(fp, "42.2178 147.312 ln\n");
  (void) fprintf(fp, "36.7842 8.625 ln\n");
  (void) fprintf(fp, "36.9326 8.625 ln\n");
  (void) fprintf(fp, "0.96451 0.45332 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "37.085 8.625 mo\n");
  (void) fprintf(fp, "42.5186 147.305 ln\n");
  (void) fprintf(fp, "42.3701 147.309 ln\n");
  (void) fprintf(fp, "36.9326 8.625 ln\n");
  (void) fprintf(fp, "37.085 8.625 ln\n");
  (void) fprintf(fp, "0.971562 0.456634 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "37.2334 8.625 mo\n");
  (void) fprintf(fp, "42.6709 147.301 ln\n");
  (void) fprintf(fp, "42.5186 147.305 ln\n");
  (void) fprintf(fp, "37.085 8.625 ln\n");
  (void) fprintf(fp, "37.2334 8.625 ln\n");
  (void) fprintf(fp, "0.978637 0.459959 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "37.3857 8.625 mo\n");
  (void) fprintf(fp, "42.8232 147.293 ln\n");
  (void) fprintf(fp, "42.6709 147.301 ln\n");
  (void) fprintf(fp, "37.2334 8.625 ln\n");
  (void) fprintf(fp, "37.3857 8.625 ln\n");
  (void) fprintf(fp, "0.985735 0.463295 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "37.5342 8.625 mo\n");
  (void) fprintf(fp, "42.9717 147.289 ln\n");
  (void) fprintf(fp, "42.8232 147.293 ln\n");
  (void) fprintf(fp, "37.3857 8.625 ln\n");
  (void) fprintf(fp, "37.5342 8.625 ln\n");
  (void) fprintf(fp, "0.992856 0.466642 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "37.6865 8.625 mo\n");
  (void) fprintf(fp, "43.124 147.285 ln\n");
  (void) fprintf(fp, "42.9717 147.289 ln\n");
  (void) fprintf(fp, "37.5342 8.625 ln\n");
  (void) fprintf(fp, "37.6865 8.625 ln\n");
  (void) fprintf(fp, "1 0.47 0 0 cmyk\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "66.4014 146.496 mo\n");
  (void) fprintf(fp, "43.124 147.285 ln\n");
  (void) fprintf(fp, "37.6865 8.625 ln\n");
  (void) fprintf(fp, "66.4014 8.625 ln\n");
  (void) fprintf(fp, "66.4014 146.496 ln\n");
  (void) fprintf(fp, "f\n");
  (void) fprintf(fp, "stroke\n");
  (void) fprintf(fp, "grestore\n");
  (void) fprintf(fp, "grestore\n");
  (void) fprintf(fp, "grestore\n");

}

/*ARGSUSED*/
int ps_Esc(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        char    *sptr = (char *) gksc->s.list, *strng;
        int     *iptr = (int *) gksc->i.list;

        int     escape_id = iptr[0], plflag;
        float   rscale,logox,logoy,logos;
        static  int     saved_color_index;


        switch (escape_id) {
        case -1450:    /* C-escapes are not implemented here */
				break;
        case -1510:    /* Save color setting before segment copy */
                if (psa->pict_empty) {
                        PSpreamble(psa, FOR_PICTURE);
                        psa->pict_empty = FALSE;
                }
                saved_color_index = psa->attributes.ps_colr_ind;
                (void) fprintf(psa->file_pointer, 
                        "G /bl Ex def /gr Ex def /rd Ex def\n");
                (void) fflush(psa->file_pointer);
                break;
        case -1511:  /* Restore color setting after segment copy */
                psa->attributes.ps_colr_ind = saved_color_index;
                (void) fprintf(psa->file_pointer, "rd gr bl R\n");
                break;
        case -1512:  /* Spacing between fill lines in range 0. to 1. */
                strng = strtok(sptr, " ");
                strng = strtok((char *) NULL, " ");
                psa->sfill_spacing = (float) atof(strng);
                break;
        case -1513:  /* Spacing between hatch lines in range 0. to 1. */
                strng = strtok(sptr, " ");
                strng = strtok((char *) NULL, " ");
                psa->hatch_spacing = (float) atof(strng);
                break;
        case -1514:  /* Size of operand stack */
                strng = strtok(sptr, " ");
                strng = strtok((char *) NULL, " ");
                psa->stack_size = (int) atoi(strng);
                break;
        case -1515:  /* Path size */
                strng = strtok(sptr, " ");
                strng = strtok((char *) NULL, " ");
                psa->path_size = (int) atoi(strng);
                break;
        case -1516:  /* Linewidth scale */
                strng = strtok(sptr, " ");
                strng = strtok((char *) NULL, " ");
                psa->nominal_width_scale = 0.5 * (float) atof(strng);
                break;
        case -1517:  /* Full background */
                strng = strtok(sptr, " ");
                strng = strtok((char *) NULL, " ");
                psa->full_background = (int) atoi(strng);
                break;
        case -1518:  /* Line joins */
                strng = strtok(sptr, " ");
                strng = strtok((char *) NULL, " ");
                psa->line_join = (linejoin_type) atoi(strng);
                if (psa->pict_empty == FALSE) {
                        (void) fprintf(psa->file_pointer, 
                                "%d J\n", psa->line_join);
                        if (psa->line_join == MITER) {
                                (void) fprintf(psa->file_pointer,
                                        "%.1f Ml\n", psa->miter_limit);
                        }
                }
                break;
        case -1519:  /* Line caps */
                strng = strtok(sptr, " ");
                strng = strtok((char *) NULL, " ");
                psa->line_cap = (linecap_type) atoi(strng);
                if (psa->pict_empty == FALSE) {
                        (void) fprintf(psa->file_pointer, 
                                "%d C\n", psa->line_cap);
                }
                break;
        case -1520:  /* Miter limit */
                strng = strtok(sptr, " ");
                strng = strtok((char *) NULL, " ");
                psa->miter_limit = (float) atof(strng);
                if (psa->pict_empty == FALSE) {
                        if (psa->line_join == MITER) {
                                (void) fprintf(psa->file_pointer, 
                                        "%.1f Ml\n", psa->miter_limit);
                        }
                }
                break;
        case -1521:  /* Corner points for positioning plot on the page */
                rscale = 1./psa->scaling;
                strng = strtok(sptr, " ");
                psa->dspace.llx = (int) (rscale * (float) atoi(strng));
                strng = strtok((char *) NULL, " ");
                psa->dspace.lly = (int) (rscale * (float) atoi(strng));
                strng = strtok((char *) NULL, " ");
                psa->dspace.urx = (int) (rscale * (float) atoi(strng));
                strng = strtok((char *) NULL, " ");
                psa->dspace.ury = (int) (rscale * (float) atoi(strng));

                psa->dspace.xspan = ((psa->dspace.urx) - (psa->dspace.llx));
                psa->dspace.yspan = ((psa->dspace.ury) - (psa->dspace.lly));

                psa->ps_clip.llx = psa->dspace.llx;
                psa->ps_clip.lly = psa->dspace.lly;
                psa->ps_clip.urx = psa->dspace.urx;
                psa->ps_clip.ury = psa->dspace.ury;
                psa->ps_clip.null = FALSE;
                break;
        case -1524:  /* Suppress background color */
                strng = strtok(sptr, " ");
                strng = strtok((char *) NULL, " ");
                psa->suppress_flag = (int) atoi(strng);
                break;
        case -1525:  /* Specify portrait/landscape mode */
                strng = strtok(sptr, " ");
                strng = strtok((char *) NULL, " ");
                plflag = (int) atoi(strng);
                if (plflag == 0) {
                  psa->orientation = PORTRAIT;
                }
                else {
                  psa->orientation = LANDSCAPE;
                }
                break;
        case -1526:  /* Corner points for positioning plot on the page */
                rscale = 1./psa->scaling;
                strng = strtok(sptr, " ");  /* Skip over the workstation ID */
                strng = strtok((char *) NULL, " ");
                psa->dspace.llx = (int) (rscale * (float) atoi(strng));
                strng = strtok((char *) NULL, " ");
                psa->dspace.lly = (int) (rscale * (float) atoi(strng));
                strng = strtok((char *) NULL, " ");
                psa->dspace.urx = (int) (rscale * (float) atoi(strng));
                strng = strtok((char *) NULL, " ");
                psa->dspace.ury = (int) (rscale * (float) atoi(strng));

                psa->dspace.xspan = ((psa->dspace.urx) - (psa->dspace.llx));
                psa->dspace.yspan = ((psa->dspace.ury) - (psa->dspace.lly));

                psa->ps_clip.llx = psa->dspace.llx;
                psa->ps_clip.lly = psa->dspace.lly;
                psa->ps_clip.urx = psa->dspace.urx;
                psa->ps_clip.ury = psa->dspace.ury;
                psa->ps_clip.null = FALSE;
    
                (void) fprintf(psa->file_pointer, "initclip\n");

                break;
        case -1527:  /* Positioning and size specifications for NCAR logo */
                rscale = 1./psa->scaling;
                strng = strtok(sptr, " ");  /* Skip over the workstation ID */
                strng = strtok((char *) NULL, " ");
                logox = (float) atof(strng);
                strng = strtok((char *) NULL, " ");
                logoy = (float) atof(strng);
                strng = strtok((char *) NULL, " ");
                logos = (float) atof(strng);
                ps_NcarLogo(gksc,logox,logoy,logos); 
                break;
        case -1528:  /* Corner points for bounding box */
                strng = strtok(sptr, " ");  /* Skip over the workstation ID */
                strng = strtok((char *) NULL, " ");
                psa->bspace.llx = atoi(strng);
                strng = strtok((char *) NULL, " ");
                psa->bspace.lly = atoi(strng);
                strng = strtok((char *) NULL, " ");
                psa->bspace.urx = atoi(strng);
                strng = strtok((char *) NULL, " ");
                psa->bspace.ury = atoi(strng);
                break;
        case -1531:  /* Paper width */
          strng = strtok(sptr, " ");
          strng = strtok((char *) NULL, " ");
          psa->paper_width = (int) atoi(strng);
          break;
        case -1532:  /* Paper height */
          strng = strtok(sptr, " ");
          strng = strtok((char *) NULL, " ");
          psa->paper_height = (int) atoi(strng);
          break;
        default:
                return ERR_INV_ESCAPE;
        }

        return(0);
}

/*ARGSUSED*/
int ps_UpdateWorkstation(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        (void) fflush(psa->file_pointer);
        return(0);
}

/*ARGSUSED*/
int ps_SetViewport(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;
        float   *fptr = (float *) gksc->f.list;

        PSClipRect      *Crect;
        int     rec_chg = 0;

        /*
         *  If the workstation viewport has changed, update the
         *  transformation.
         */
        if ((psa->tsystem.viewport.llx != fptr[0]) ||
            (psa->tsystem.viewport.urx != fptr[1]) ||
            (psa->tsystem.viewport.lly != fptr[2]) ||
            (psa->tsystem.viewport.ury != fptr[3])) {
                TransformSetViewport(&psa->tsystem, fptr[0], fptr[2], 
                        fptr[1], fptr[3]);
                psa->transform = TransformGetTransform(&psa->tsystem);
        }
        else {
                return(0);
        }

        /*
         *  re-evaluate the PostScript clipping rectangle.
         */
        Crect = GetPSClipping (psa, psa->gks_clip, psa->tsystem.window);
        if ((Crect->llx != psa->ps_clip.llx) ||
            (Crect->lly != psa->ps_clip.lly) ||
            (Crect->urx != psa->ps_clip.urx) ||
            (Crect->ury != psa->ps_clip.ury)) {
                psa->ps_clip.llx = Crect->llx;
                psa->ps_clip.lly = Crect->lly;
                psa->ps_clip.urx = Crect->urx;
                psa->ps_clip.ury = Crect->ury;
                psa->ps_clip.null = Crect->null;
                rec_chg = 1;
        }

        if ((psa->attributes.clip_ind == CLIPPING_ON) && (rec_chg == 1) &&
                                         (psa->pict_empty == FALSE)) {
                        (void) fprintf(psa->file_pointer, "Ls Gr Gs Lr\n");
                        OutputClipping (psa, PS_CLIPPING_RECT);
        }

        return(0);
}

/*ARGSUSED*/
int ps_SetWindow(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;
        float   *fptr = (float *) gksc->f.list;

        PSClipRect      *Crect;
        int     rec_chg = 0;

        if ((fptr[0] >= fptr[1]) || (fptr[2] >= fptr[3])) {
                return (ERR_INV_RECT);
        }

        /*
         *  If the workstation window has changed, update the
         *  transformation.
         */
        if ((psa->tsystem.window.llx != fptr[0]) ||
            (psa->tsystem.window.urx != fptr[1]) ||
            (psa->tsystem.window.lly != fptr[2]) ||
            (psa->tsystem.window.ury != fptr[3])) {
                TransformSetWindow(&psa->tsystem, fptr[0], fptr[2], 
                        fptr[1], fptr[3]);
                psa->transform = TransformGetTransform(&psa->tsystem);
        }
        else {
                return(0);
        }

        /*
         *  Calculate the new PostScript clip rectangle and check to
         *  to see if it has changed and store it if it has.
         */
        Crect = GetPSClipping (psa, psa->gks_clip, psa->tsystem.window);
        if ((Crect->llx != psa->ps_clip.llx) ||
            (Crect->lly != psa->ps_clip.lly) ||
            (Crect->urx != psa->ps_clip.urx) ||
            (Crect->ury != psa->ps_clip.ury)) {
                psa->ps_clip.llx = Crect->llx;
                psa->ps_clip.lly = Crect->lly;
                psa->ps_clip.urx = Crect->urx;
                psa->ps_clip.ury = Crect->ury;
                psa->ps_clip.null = Crect->null;
                rec_chg = 1;
        }

        if ((psa->attributes.clip_ind == CLIPPING_ON) && (rec_chg == 1) &&
                                         (psa->pict_empty == FALSE)) {
                        (void) fprintf(psa->file_pointer, "Ls Gr Gs Lr\n");
                        OutputClipping (psa, PS_CLIPPING_RECT);
        }
        return(0);
}

void writePSColor(FILE *fp, int colorIndex) {
    if (colorIndex & ARGB_MASK) {
        float r = ((RED_MASK   & colorIndex) >> 16) / 255.;
        float g = ((GREEN_MASK & colorIndex) >> 8) / 255.;
        float b = ((BLUE_MASK  & colorIndex)) / 255.;
        (void) fprintf(fp, "%f %f %f R\n", r, g, b);
    } else {
        (void) fprintf(fp, "%d O\n", colorIndex);
    }
}
