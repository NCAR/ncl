/*
 * $Id: color.h,v 1.1.2.1 2010-03-17 20:44:44 brownrig Exp $
 */

#include <stdint.h>

/* this is fully transparent black -- one of many possible transparent color indexes */

#define NhlTRANSPARENT_CI  1073741824  

extern const uint32_t ALPHA_MASK;
extern const uint32_t ALPHA_OPAQUE;


extern void _NhlSetLineOpacity(void* layer, float opacity);
extern float _NhlGetLineOpacity(void* layer);
extern void _NhlSetFillOpacity(void* layer, float opacity);
extern float _NhlGetFillOpacity(void* layer);
extern void _NhlSetMarkerOpacity(void* layer, float opacity);
extern float _NhlGetMarkerOpacity(void* layer);
extern void _NhlSetBackgroundOpacity(void* layer, float opacity);
extern float _NhlGetBackgroundOpacity(void* layer);
extern int _NhlRGBAToColorIndex(float *rgba, int has_alpha);
extern void _NhlColorIndexToRGBA(int color_index, float *rgba, int want_alpha);
