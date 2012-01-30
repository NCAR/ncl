
#include "argb.h"

/*
 * Given and index, returns r,g,b components from a color table, or from a packed argb value.
 *
 */
void index2rgb(float* colorMap, int index, float* r, float* g, float* b) {

    if (index & ARGB_MASK) {
        *r = ((RED_MASK   & index) >> 16) / 255.;
        *g = ((GREEN_MASK & index) >>  8) / 255.;
        *b = ((BLUE_MASK  & index))       / 255.;
    }
    else {
        *r = colorMap[3*index];
        *g = colorMap[3*index+1];
        *b = colorMap[3*index+2];
    }

    return;
}

