/*
 * $Id: color.c,v 1.1.2.1 2010-03-17 20:44:44 brownrig Exp $
 *
 */

#include <stdint.h>
#include <ncarg/hlu/BaseP.h>
#include <ncarg/hlu/WorkstationP.h>
#include <ncarg/gksP.h>


const uint32_t ALPHA_MASK = 0x40000000;
const uint32_t ALPHA_OPAQUE = 0x7f000000;
const uint32_t LEFTMOST_BYTE = 0x000000ff;
const uint32_t LEFTMOST_6BITS = 0x0000003f;

/*
 * _NhlSetOpacity()
 *
 * Worker function for the _NhlSetXXXXXOpacity() functions below.
 *
 */
static void _NhlSetOpacity(void* layer, float opacity, int attrib)
{
	_NGCAlpha alphaInfo;
	NhlWorkstationLayer wksLayer = (NhlWorkstationLayer) ((NhlLayer)layer)->base.wkptr;

	alphaInfo.type = NGC_SETALPHA;
	alphaInfo.work_id = wksLayer->work.gkswksid;
	alphaInfo.graphicAttrib = attrib;
	alphaInfo.alpha = opacity;

	Gescape_in_data gesc;
	gesc.escape_r1.data = &alphaInfo;
	gesc.escape_r1.size = sizeof(alphaInfo);
	gescape(NGESC_CNATIVE, &gesc, NULL, NULL);
}

/*
 * _NhlGetOpacity()
 *
 * Worker function for the _NhlGetXXXXXOpacity() functions below.
 *
 */
static float _NhlGetOpacity(void* layer, int attrib)
{
	_NGCAlpha alphaInfo;
	NhlWorkstationLayer wksLayer = (NhlWorkstationLayer) ((NhlLayer)layer)->base.wkptr;

	alphaInfo.type = NGC_GETALPHA;
	alphaInfo.work_id = wksLayer->work.gkswksid;
	alphaInfo.graphicAttrib = attrib;
	alphaInfo.alpha = 1.0;    /* initialize, as some drivers don't populate this */

	Gescape_in_data gesc;
	gesc.escape_r1.data = &alphaInfo;
	gesc.escape_r1.size = sizeof(alphaInfo);
	gescape(NGESC_CNATIVE, &gesc, NULL, NULL);
	return alphaInfo.alpha;
}


void _NhlSetLineOpacity(void* layer, float opacity) {
	_NhlSetOpacity(layer, opacity, NGC_LINEALPHA);
}

float _NhlGetLineOpacity(void* layer) {
	return _NhlGetOpacity(layer, NGC_LINEALPHA);
}


void _NhlSetFillOpacity(void* layer, float opacity) {
	_NhlSetOpacity(layer, opacity, NGC_FILLALPHA);
}

float _NhlGetFillOpacity(void* layer) {
	return _NhlGetOpacity(layer, NGC_FILLALPHA);
}


void _NhlSetMarkerOpacity(void* layer, float opacity) {
	_NhlSetOpacity(layer, opacity, NGC_MARKERALPHA);
}

float _NhlGetMarkerOpacity(void* layer) {
	return _NhlGetOpacity(layer, NGC_MARKERALPHA);
}

void _NhlSetBackgroundOpacity(void* layer, float opacity) {
	_NhlSetOpacity(layer, opacity, NGC_BACKGROUNDALPHA);
}

float _NhlGetBackgroundOpacity(void* layer) {
	return _NhlGetOpacity(layer, NGC_BACKGROUNDALPHA);
}

int _NhlRGBAToColorIndex(float *rgba, int has_alpha)
{
	int r,g,b,a;
	int tint;

	r = (int)(*(rgba) * 255) << 16;
	g = (int)(*(rgba+1) * 255) << 8;
	b = (int)(*(rgba+2) * 255);
	a = (has_alpha) ? (int)(*(rgba+3) * 63) << 24 | ALPHA_MASK : ALPHA_OPAQUE;
	tint = a | r | g | b;
	return (tint);
}

/* assumes caller allocates memory for rgba array */

void _NhlColorIndexToRGBA(int color_index, float *rgba, int want_alpha)
{
	rgba[0] = ((color_index >> 16) & LEFTMOST_BYTE) / 255.0;
	rgba[1] = ((color_index >> 8) & LEFTMOST_BYTE) / 255.0;
	rgba[2] = (color_index & LEFTMOST_BYTE) / 255.0;
	if (want_alpha) 
		rgba[3] = ((color_index >> 24) & LEFTMOST_6BITS) / 63.0;

	return;
}
