/*
 * $Id: color.h,v 1.1.2.1 2010-03-17 20:44:44 brownrig Exp $
 */

#include <stdint.h>

extern const uint32_t ALPHA_MASK;
extern const uint32_t ALPHA_OPAQUE;


extern void _NhlSetLineOpacity(void* layer, float opacity);
extern float _NhlGetLineOpacity(void* layer);
extern void _NhlSetFillOpacity(void* layer, float opacity);
extern float _NhlGetFillOpacity(void* layer);
extern void _NhlSetMarkerOpacity(void* layer, float opacity);
extern float _NhlGetMarkerOpacity(void* layer);
extern int _NhlRGBAToColorIndex(float *rgba, int has_alpha);
