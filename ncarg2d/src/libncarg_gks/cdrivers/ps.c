/*
 *	$Id: ps.c,v 1.18 1999-03-25 00:23:39 fred Exp $
 */
/*
 *
 *      File:		ps.c
 *
 *      Author:		Fred Clare
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Thu Aug 19 10:30:32 MDT 1993
 *
 *      Description:	This file contains the definition of the PostScript
 *			device driver.
 */
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include <ncarg/gksP.h>
#include <ncarg/c.h>
#include "gksc.h"
#include "gks.h"
#include "common.h"
#include "ps_device.h"
#include "psddi.h"
#include "ps.h"

char    *PSFontNames[] = {
                            "/Times-Roman", "/Times-Bold", "/Times-Italic",
                            "/Times-BoldItalic", "/Helvetica",
                            "/Helvetica-Bold", "/Helvetica-Oblique",
                            "/Helvetica-BoldOblique", "/Courier",
                            "/Courier-Bold", "/Courier-Oblique",
                            "/Courier-BoldOblique", "/Symbol"
		};

extern int	orig_wks_id;
int             c_model;

void PSprint_points(PSddp *psa, PSPoint *points, unsigned num, 
			terminator_type terminator)
{
	int	i,tmpx,tmpy;

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
	static	PSClipRect	rect;
	CoordSpace	rtmp;

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
	const	char 	*PreviewMap[] = {
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
	int	i;

	for (i = 0; i < 200; i++) {
		(void) fprintf (fp, "%% %s\n", PreviewMap[i]);
	}
}

void PSpreamble (PSddp *psa, preamble_type type)
{
	int	i;
	float	scl = psa->scaling;
	FILE	*fp;

	fp = psa->file_pointer;

	if (type == FOR_FILE) {
		if ((psa->type == EPSF) || (psa->type == EPSI)) {
			(void) fprintf(fp, "%%!PS-Adobe-2.0 EPSF-2.0\n");
		}
		else {
			(void) fprintf(fp, "%%!PS-Adobe-2.0\n");
		}
		(void) fprintf(fp, "%%%%DocumentFonts: (at end)\n");
		if (psa->type == RPS) {
			(void) fprintf(fp, "%%%%Pages: (at end)\n");
			(void) fprintf(fp, "%%%%PageOrder: Ascend\n");
		}
		(void) fprintf(fp, "%%%%Creator: NCAR GKS\n");
		(void) fprintf(fp, "%%%%CreationDate: %s %s\n",
					__DATE__,__TIME__);
	       	(void) fprintf(psa->file_pointer, "%%%%BoundingBox: ");
       		(void) fprintf(psa->file_pointer, "%d ",
			(int) ((psa->scaling) * ((float)(psa->dspace.llx))));
       		(void) fprintf(psa->file_pointer, "%d ",
			(int) ((psa->scaling) * ((float)(psa->dspace.lly))));
       		(void) fprintf(psa->file_pointer, "%d ",
			(int) ((psa->scaling) * ((float)(psa->dspace.urx))+1));
       		(void) fprintf(psa->file_pointer, "%d\n",
			(int) ((psa->scaling) * ((float)(psa->dspace.ury))+1));
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
		 *  In --  image
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

		(void) fprintf(fp, "%%%%EndProlog\n\n");

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

		if (psa->background) {
			PSbackground(psa);
		}
		if (psa->color == MONO) {   /* Use foreground color for mono */
			(void) fprintf(fp, "1 O\n");
		}
		else {
			(void) fprintf(fp,"%d O\n", 
				psa->attributes.ps_colr_ind);
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
	int	i, cllx, clly, curx, cury;
        float   rscale;

	psa->hatch_spacing = PS_HATCH_SPACING;
	psa->stack_size = MAX_STACK;
	psa->path_size = MAX_PATH;
	psa->line_join = ROUND;
	psa->line_cap = ROUNDED;
	psa->nominal_width_scale = .5;
	psa->full_background = FALSE;
	psa->miter_limit = MITER_LIMIT_DEFAULT;
	psa->sfill_spacing = PS_FILL_SPACING;

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
	case	DEFAULT: 
		psfc->font = PS_HELVETICA;
		psa->fonts_used[PS_HELVETICA] = 1;
		break;
	case	H_CARTOGRAPHIC_ROMAN:
		psfc->font = PS_HELVETICA;
		psa->fonts_used[PS_HELVETICA] = 1;
		break;
	case	H_CARTOGRAPHIC_GREEK:
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
	case	H_SIMPLEX_ROMAN:
		psfc->font = PS_HELVETICA;
		psa->fonts_used[PS_HELVETICA] = 1;
		break;
	case	H_SIMPLEX_GREEK:
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
	case	H_SIMPLEX_SCRIPT: 
		return_value = -1;
	case	H_COMPLEX_ROMAN:
		psfc->font = PS_TIMES_ROMAN;
		psa->fonts_used[PS_TIMES_ROMAN] = 1;
		break;
	case	H_COMPLEX_GREEK:
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
	case	H_COMPLEX_SCRIPT:
		return_value = -1;
		break;
	case	H_COMPLEX_ITALIC:
		psfc->font = PS_TIMES_ITALIC;
		psa->fonts_used[PS_TIMES_ITALIC] = 1;
		break;
	case	H_COMPLEX_CYRILLIC:
		return_value = -1;
		break;
	case	H_DUPLEX_ROMAN:
		psfc->font = PS_HELVETICA_BOLD;
		psa->fonts_used[PS_HELVETICA_BOLD] = 1;
		break;
	case	H_TRIPLEX_ROMAN: 
		psfc->font = PS_TIMES_BOLD;
		psa->fonts_used[PS_TIMES_BOLD] = 1;
		break;
	case	H_TRIPLEX_ITALIC:
		psfc->font = PS_TIMES_BOLDITALIC;
		psa->fonts_used[PS_TIMES_BOLDITALIC] = 1;
		break;
	case	H_GOTHIC_GERMAN:
		return_value = -1;
		break;
	case	H_GOTHIC_ENGLISH:
		return_value = -1;
		break;
	case	H_GOTHIC_ITALIAN:
		return_value = -1;
		break;
	case    H_MATH_SYMBOLS:
		if ((cnum >= 32) && (cnum <= 127)) {
			psfc->font = hms2psf[cnum-32];
			psfc->char_num = hms2psc[cnum-32];
			psa->fonts_used[psfc->font] = 1;
		}
		break;
	case	H_SYMBOL_SET1:
		return_value = -1;
		break;
	case    H_SYMBOL_SET2:
		return_value = -1;
		break;
	case	NCAR_HELVETICA: 
		psfc->font = PS_HELVETICA;
		psa->fonts_used[PS_HELVETICA] = 1;
		break;
	case	NCAR_HELVETICA_BOLD:
		psfc->font = PS_HELVETICA_BOLD;
		psa->fonts_used[PS_HELVETICA_BOLD] = 1;
		break;
	case	NCAR_HELVETICA_OBLIQUE:
		psfc->font = PS_HELVETICA_OBLIQUE;
		psa->fonts_used[PS_HELVETICA_OBLIQUE] = 1;
		break;
	case	NCAR_HELVETICA_BOLDOBLIQUE:
		psfc->font = PS_HELVETICA_BOLDOBLIQUE;
		psa->fonts_used[PS_HELVETICA_BOLDOBLIQUE] = 1;
		break;
	case	NCAR_TIMES_ROMAN:
		psfc->font = PS_TIMES_ROMAN;
		psa->fonts_used[PS_TIMES_ROMAN] = 1;
		break;
	case	NCAR_TIMES_BOLD: 
		psfc->font = PS_TIMES_BOLD;
		psa->fonts_used[PS_TIMES_BOLD] = 1;
		break;
	case	NCAR_TIMES_ITALIC:
		psfc->font = PS_TIMES_ITALIC;
		psa->fonts_used[PS_TIMES_ITALIC] = 1;
		break;
	case	NCAR_TIMES_BOLDITALIC:
		psfc->font = PS_TIMES_BOLDITALIC;
		psa->fonts_used[PS_TIMES_BOLDITALIC] = 1;
		break;
	case	NCAR_COURIER:
		psfc->font = PS_COURIER;
		psa->fonts_used[PS_COURIER] = 1;
		break;
	case	NCAR_COURIER_BOLD:
		psfc->font = PS_COURIER_BOLD;
		psa->fonts_used[PS_COURIER_BOLD] = 1;
		break;
	case	NCAR_COURIER_OBLIQUE:
		psfc->font = PS_COURIER_OBLIQUE;
		psa->fonts_used[PS_COURIER_OBLIQUE] = 1;
		break;
	case	NCAR_COURIER_BOLDOBLIQUE:
		psfc->font = PS_COURIER_BOLDOBLIQUE;
		psa->fonts_used[PS_COURIER_BOLDOBLIQUE] = 1;
		break;
	case	NCAR_GREEK: 
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
	case	NCAR_MATH_SYMBOLS:
		psfc->font = PS_SYMBOL;
		psa->fonts_used[PS_SYMBOL] = 1;
		if ((cnum >= 32) && (cnum <= 127)) {
			psfc->char_num = nms2psc[cnum-32];
		}
		break;
	case	NCAR_TEXT_SYMBOLS:
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
	case	NCAR_WEATHER1:
		return_value = -1;
		break;
	case	NCAR_WEATHER2:
		return_value = -1;
		break;
	case	NCAR_HELVETICA_O: 
		psfc->font = PS_HELVETICA;
		psa->fonts_used[PS_HELVETICA] = 1;
		psfc->outline = TRUE;
		break;
	case	NCAR_HELVETICA_BOLD_O:
		psfc->font = PS_HELVETICA_BOLD;
		psa->fonts_used[PS_HELVETICA_BOLD] = 1;
		psfc->outline = TRUE;
		break;
	case	NCAR_HELVETICA_OBLIQUE_O:
		psfc->font = PS_HELVETICA_OBLIQUE;
		psa->fonts_used[PS_HELVETICA_OBLIQUE] = 1;
		psfc->outline = TRUE;
		break;
	case	NCAR_HELVETICA_BOLDOBLIQUE_O:
		psfc->font = PS_HELVETICA_BOLDOBLIQUE;
		psa->fonts_used[PS_HELVETICA_BOLDOBLIQUE] = 1;
		psfc->outline = TRUE;
		break;
	case	NCAR_TIMES_ROMAN_O:
		psfc->font = PS_TIMES_ROMAN;
		psa->fonts_used[PS_TIMES_ROMAN] = 1;
		psfc->outline = TRUE;
		break;
	case	NCAR_TIMES_BOLD_O: 
		psfc->font = PS_TIMES_BOLD;
		psa->fonts_used[PS_TIMES_BOLD] = 1;
		psfc->outline = TRUE;
		break;
	case	NCAR_TIMES_ITALIC_O:
		psfc->font = PS_TIMES_ITALIC;
		psa->fonts_used[PS_TIMES_ITALIC] = 1;
		psfc->outline = TRUE;
		break;
	case	NCAR_TIMES_BOLDITALIC_O:
		psfc->font = PS_TIMES_BOLDITALIC;
		psa->fonts_used[PS_TIMES_BOLDITALIC] = 1;
		psfc->outline = TRUE;
		break;
	case	NCAR_COURIER_O:
		psfc->font = PS_COURIER;
		psa->fonts_used[PS_COURIER] = 1;
		psfc->outline = TRUE;
		break;
	case	NCAR_COURIER_BOLD_O:
		psfc->font = PS_COURIER_BOLD;
		psa->fonts_used[PS_COURIER_BOLD] = 1;
		psfc->outline = TRUE;
		break;
	case	NCAR_COURIER_OBLIQUE_O:
		psfc->font = PS_COURIER_OBLIQUE;
		psa->fonts_used[PS_COURIER_OBLIQUE] = 1;
		psfc->outline = TRUE;
		break;
	case	NCAR_COURIER_BOLDOBLIQUE_O:
		psfc->font = PS_COURIER_BOLDOBLIQUE;
		psa->fonts_used[PS_COURIER_BOLDOBLIQUE] = 1;
		psfc->outline = TRUE;
		break;
	case	NCAR_GREEK_O: 
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
	case	NCAR_MATH_SYMBOLS_O:
		psfc->font = PS_SYMBOL;
		psa->fonts_used[PS_SYMBOL] = 1;
		psfc->outline = TRUE;
		if ((cnum >= 32) && (cnum <= 127)) {
			psfc->char_num = nms2psc[cnum-32];
		}
		break;
	case	NCAR_TEXT_SYMBOLS_O:
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
	case	NCAR_WEATHER1_O:
		return_value = -1;
		break;
	case	NCAR_WEATHER2_O:
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
	static int 	first_call = TRUE;

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
 	int	StackSize;

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
	int	StackSize;

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
		case	DOT_MARKER:      /* Dots cannot be scaled */
			tsize = MAX(1, (int) (.28/(psa->scaling)));
			(void) fprintf(psa->file_pointer, "%d B\n", tsize);
			break;
		case	PLUS_MARKER:
			(void) fprintf(psa->file_pointer, "%d Q\n", tsize);
			break;
		case	STAR_MARKER:
			(void) fprintf(psa->file_pointer, "%d V\n", tsize);
			break;
		case	CIRCLE_MARKER:
			(void) fprintf(psa->file_pointer, "%d B\n", tsize);
			break;
		case	X_MARKER:
			(void) fprintf(psa->file_pointer, "%d X\n", tsize);
			break;
		default:
			(void) fprintf(psa->file_pointer, "%d V\n", tsize);
			break;
		}
        }
}

/*ARGSUSED*/
ps_OpenWorkstation(GKSC *gksc)
{
	char	*sptr = (char *) gksc->s.list;
        PSddp	*psa;
	char  	*ctmp;
	FILE    *fp;
	int	*pint, wks_type;
	extern	int	orig_wks_id;
	_NGCesc	*cesc;

	psa = (PSddp *) malloc (sizeof (PSddp));
        if (psa == (PSddp *) NULL) {
		ESprintf(ERR_PS_MEMORY, "PS: malloc(%d)", sizeof(PSddp));
		return(ERR_PS_MEMORY);
	}

	gksc->ddp = (GKSC_Ptr) psa;

	/*
	 * Handle Initial C Escape Elements.
	 *	(none currently defined for ps - so all of them cause gerhnd.)
	 */
	while(cesc = _NGGetCEscInit()){
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
ps_ActivateWorkstation(gksc)
	GKSC	*gksc;
{
	return(0);
}

/*ARGSUSED*/
ps_DeactivateWorkstation(gksc)
	GKSC	*gksc;
{
	return(0);
}

/*ARGSUSED*/
ps_CloseWorkstation(gksc)
	GKSC	*gksc;
{
	PSddp	*psa;
	int	pages, i, j;

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

       	(void) fprintf(psa->file_pointer, "%%%%EOF\n");

        (void) fflush(psa->file_pointer);
	psa->pict_empty = TRUE;
        fclose(psa->file_pointer);
	return(0);
}

/*ARGSUSED*/
ps_ClearWorkstation(gksc)
	GKSC	*gksc;
{
	PSddp	*psa;
	int	ier = 0;
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
ps_Polyline(gksc)
	GKSC	*gksc;
{
	PSddp	*psa;
	int	ps_linewidth, requested_color, current_color, requested_type;
	int	ier = 0;

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
		   (void) fprintf(psa->file_pointer, "%d O\n", requested_color);
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
ps_Polymarker(gksc)
	GKSC	*gksc;
{

	PSddp	*psa;
	int	markersize, markertype, requested_color, current_color;
	int	ier = 0;

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
		   (void) fprintf(psa->file_pointer, "%d O\n", requested_color);
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
ps_Text(gksc)
	GKSC	*gksc;
{
	PSPoint	*pptr = (PSPoint *) gksc->p.list;
	char	*sptr = (char *) gksc->s.list;
	char ctmp, *tptr;
	PSddp	*psa;
	PSCharInfo fc;
	PSTextent  textent;
        int     requested_color, current_color;
	int	PSFontScale, PSCharHeight, PSCharSpace;
	int	i, j, found, x_position, y_position, y_inc, return_value;
	int	string_height=0, max_char_width, char_spacing=0;
	int	num_chars, old_font=0, current_font, strpos, fcount;

	float   xoffset=0.0, yoffset=0.0, vert_offset=0.0;
	float	nominal_vert_adjust, tdiff, bdiff, char_expn;
	float	tm_a, tm_b, tm_c, tm_d, tmp1, aspect_ratio;
	float	fc2wsvp, string_height_wsvp, char_spacing_wsvp;
	float	max_char_width_wsvp;
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
                   (void) fprintf(psa->file_pointer, "%d O\n", requested_color);
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
ps_FillArea(gksc)
	GKSC	*gksc;
{
        PSPoint *pptr = (PSPoint *) gksc->p.list;
        PSddp   *psa = (PSddp *) gksc->ddp;
        int     requested_color, current_color;
	int     npoints = gksc->p.num, i, linewidth, ier;
	int	StackSize, PathSize;

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
		   (void) fprintf(psa->file_pointer, "%d O\n", requested_color);
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
	if ((psa->attributes.fill_int_style != SOLID_FILL) ||
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
	if ((psa->attributes.fill_int_style != SOLID_FILL) ||
			(npoints >= PathSize)) {
		fprintf(psa->file_pointer, "W ");
		fprintf(psa->file_pointer, "D\n");
	}

        return(0);
}


/*ARGSUSED*/
ps_Cellarray(gksc)
	GKSC	*gksc;
{
	PSPoint	*pptr = (PSPoint *) gksc->p.list;
	int		*iptr = (int *) gksc->i.list;
        PSddp   *psa = (PSddp *) gksc->ddp;

	int	*xptr = (int *) gksc->x.list;  /* color index array */

	int	nx = iptr[0];	/* number of cols	*/
	int	ny = iptr[1];	/* number of rows	*/

	int	index, intensity;
	int	i, j, color_index, ier = 0;
	float	x_scale, y_scale, ftmp;

        PSPoint	*Pptr = &pptr[0];
        PSPoint	*Qptr = &pptr[1];
        PSPoint	*Rptr = &pptr[2];

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
			if (psa->color == COLOR) {
			   (void) fprintf(psa->file_pointer, "%02X", 
			      (int)(255. * (psa->color_map[3*color_index  ])));
			   (void) fprintf(psa->file_pointer, "%02X", 
			      (int)(255. * (psa->color_map[3*color_index+1])));
			   (void) fprintf(psa->file_pointer, "%02X", 
			      (int)(255. * (psa->color_map[3*color_index+2])));
			}
			else {
			   ftmp  = 0.30 * (psa->color_map[3*color_index  ]) +
			           0.59 * (psa->color_map[3*color_index+1]) +
			           0.11 * (psa->color_map[3*color_index+2]);
			   intensity = (int) (255. * ftmp);
			   (void) fprintf(psa->file_pointer, "%02X", intensity);
			}
		}
	}
	(void) fprintf(psa->file_pointer, "\nGr\n");
	
	return(0);
}

/*ARGSUSED*/
ps_SetLinetype(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int		*iptr = (int *) gksc->i.list;

	psa->attributes.linetype = iptr[0];
	return(0);
}

/*ARGSUSED*/
ps_SetLineWidthScaleFactor(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	float		*fptr = (float *) gksc->f.list;

        psa->attributes.linewidth = fptr[0];
	return(0);
}


/*ARGSUSED*/
ps_SetPolylineColorIndex(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int		*xptr = (int *) gksc->x.list;

        psa->attributes.line_colr_ind = xptr[0];
	return(0);
}

/*ARGSUSED*/
ps_SetMarkerType(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int		*iptr = (int *) gksc->i.list;

        psa->attributes.marker_type = iptr[0];
	return(0);
}


/*ARGSUSED*/
ps_SetMarkerSizeScaleFactor(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	float		*fptr = (float *) gksc->f.list;

        psa->attributes.marker_size = fptr[0];
	return(0);
}

/*ARGSUSED*/
ps_SetPolymarkerColorIndex(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int		*xptr = (int *) gksc->x.list;

        psa->attributes.marker_colr_ind = xptr[0];
	return(0);
}


/*ARGSUSED*/
ps_SetTextFontAndPrecision(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int		*iptr = (int *) gksc->i.list;

        psa->attributes.text_font = iptr[0];
        psa->attributes.text_prec = iptr[1];

	return(0);
}

/*ARGSUSED*/
ps_SetCharacterExpansionFactor(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	float		*fptr = (float *) gksc->f.list;

        psa->attributes.char_expan = fptr[0];
	return(0);
}


/*ARGSUSED*/
ps_SetCharacterSpacing(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	float		*fptr = (float *) gksc->f.list;

        psa->attributes.char_space = fptr[0];
	return(0);
}

/*ARGSUSED*/
ps_SetTextColorIndex(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int		*xptr = (int *) gksc->x.list;

        psa->attributes.text_colr_ind = xptr[0];
	return(0);
}


/*ARGSUSED*/
ps_SetCharacterHeightAndUpVector(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	float		*fptr = (float *) gksc->f.list;

	double		up_x = (double) fptr[0];
	double		up_y = (double) fptr[2];
	double		base_x = (double) fptr[1];
	double		base_y = (double) fptr[3];

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
ps_SetTextPath(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int		*iptr = (int *) gksc->i.list;

        psa->attributes.text_path = iptr[0];
	return(0);
}


/*ARGSUSED*/
ps_SetTextAlignment(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int		*iptr = (int *) gksc->i.list;

        psa->attributes.text_align_horiz = iptr[0];
        psa->attributes.text_align_vert = iptr[1];
	return(0);
}

/*ARGSUSED*/
ps_SetFillAreaInteriorStyle(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int		*iptr = (int *) gksc->i.list;

        psa->attributes.fill_int_style = iptr[0];
	return(0);
}


/*ARGSUSED*/
ps_SetFillAreaStyleIndex(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int		*iptr = (int *) gksc->i.list;
        psa->attributes.fill_style_ind = iptr[0];
	return(0);
}

/*ARGSUSED*/
ps_SetFillAreaColorIndex(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int		*xptr = (int *) gksc->x.list;

        psa->attributes.fill_colr_ind = xptr[0];
	return(0);
}


/*ARGSUSED*/
ps_SetColorRepresentation(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int	*xptr = (int *) gksc->x.list;
	PSColor	*rgbptr = (PSColor *) gksc->rgb.list;

        unsigned        index   = (unsigned) xptr[0];

	float		r =  rgbptr[0].r;
	float		g =  rgbptr[0].g;
	float		b =  rgbptr[0].b;

	psa->color_map[3*index  ] = r;
	psa->color_map[3*index+1] = g;
	psa->color_map[3*index+2] = b;
	psa->color_map[index+3*MAX_COLORS] = 1;

	if (index == 0) {
		psa->background = TRUE;
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

	  if (index == 0) {
		  PSbackground(psa);
		  psa->background = TRUE;
	  }
	}

	return(0);
}

/*ARGSUSED*/
ps_SetClipIndicator(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	int	*iptr = (int *) gksc->i.list, np = 2;
	int	orig_clip_ind, rec_chg = 0, default_rect = 0;

	float	tclipx[2], tclipy[2];

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
ps_GetColorRepresentation(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

        int     *xptr = (int *) gksc->x.list;
        PSColor *rgbptr = (PSColor *) gksc->rgb.list;

	int	index	= xptr[0];

	rgbptr[0].r = psa->color_map[3*index];
	rgbptr[0].g = psa->color_map[3*index+1];
	rgbptr[0].b = psa->color_map[3*index+2];

	return(0);
}


/*ARGSUSED*/
ps_Esc(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	char	*sptr = (char *) gksc->s.list, *strng;
	int	*iptr = (int *) gksc->i.list;

	int	escape_id = iptr[0];
	float	rscale;
	static	int	saved_color_index;


	switch (escape_id) {
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
		break;
	default:
		return ERR_INV_ESCAPE;
	}

	return(0);
}

/*ARGSUSED*/
ps_UpdateWorkstation(gksc)
	GKSC	*gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;

	(void) fflush(psa->file_pointer);
	return(0);
}

/*ARGSUSED*/
ps_SetViewport(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;
        float   *fptr = (float *) gksc->f.list;

	PSClipRect	*Crect;
	int	rec_chg = 0;

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
ps_SetWindow(gksc)
        GKSC    *gksc;
{
        PSddp   *psa = (PSddp *) gksc->ddp;
        float   *fptr = (float *) gksc->f.list;

	PSClipRect	*Crect;
	int	rec_chg = 0;

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
