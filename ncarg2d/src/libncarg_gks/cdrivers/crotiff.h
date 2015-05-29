/* 
 * File:   crotiff.h
 * Author: brownrig
 *
 * Created on March 4, 2015, 10:27 AM
 */

#ifndef CROTIFF_H
#define	CROTIFF_H

#ifdef	__cplusplus
extern "C" {
#endif

    struct cairo_surface_t;

    typedef struct TiffHandle {
        FILE* filePtr;
        long nextIFDPointer;
    } TiffHandle;

    typedef struct TiffGeoReference {
        int projCode;
        float worldX[4];
        float worldY[4];
        float ndcX[4];
        float ndcY[4];
        float meridian;
        float stdPar1;
        float stdPar2;
    } TiffGeoReference;

    extern TiffHandle* crotiff_openFile(const char* filename);
    extern void crotiff_closeFile(TiffHandle* closure);
    extern int crotiff_writeImage(TiffHandle* closure, TiffGeoReference* georef, cairo_surface_t* surface);
    extern int crotiff_writeImageCompressed(TiffHandle* closure, TiffGeoReference* georef, cairo_surface_t* surface);

#ifdef	__cplusplus
}
#endif

#endif	/* CROTIFF_H */

