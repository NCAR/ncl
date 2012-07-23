/* 
 * File:   argb.h
 * Author: brownrig
 *
 * Created on January 9, 2012, 2:13 PM
 */

#ifndef ARGB_H
#define	ARGB_H

#ifdef	__cplusplus
extern "C" {
#endif


#define ARGB_MASK  (0x40000000)
#define ALPHA_MASK (0x3F000000)
#define RED_MASK   (0x00FF0000)
#define GREEN_MASK (0x0000FF00)
#define BLUE_MASK  (0x000000FF)
#define ALPHA_BLEND(a1, a2) (a1 * a2)

/* used by several functions */
extern void index2rgb(float*, int, float*, float*, float*);

#ifdef	__cplusplus
}
#endif

#endif	/* ARGB_H */

