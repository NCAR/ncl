#include <png.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "rgba2png.h"

int rgba2png(unsigned int * Red,
        unsigned int * Green,
        unsigned int * Blue,
        unsigned int * Alpha,
        size_t width,
        size_t height,
        int nFrames,
        char * BaseFileName) 
{
    FILE * fp;
    png_structp png_ptr = NULL;
    png_infop info_ptr = NULL;
    int i, j;
    png_byte ** row_pointers = NULL;
    int nBands = 4;
    int depth = 8;
    char Filename[80];
    int Status;

    int FrameNumber;
    for (FrameNumber = 0; FrameNumber < nFrames; FrameNumber++) {
        Status = 1;
        sprintf(Filename, "%s%4.4d%s", BaseFileName, FrameNumber, ".png");
#ifdef DEBUG
        printf("%s\n", Filename);
#endif
        if ((fp = fopen(Filename, "wb")) == NULL) {
            printf("ERROR: Couldn't open the file, %s\n", Filename);
            break;
        }

        if ((png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL)) == NULL) {
            fclose(fp);
            printf("ERROR: Couldn't creat png structure for %s\n", Filename);
            break;
        }

        if ((info_ptr = png_create_info_struct(png_ptr)) == NULL) {
            png_destroy_write_struct(&png_ptr, &info_ptr);
            fclose(fp);
            printf("ERROR: Couldn't creat png info for %s\n", Filename);
            break;
        }

        /* Set image attributes. */
        png_set_IHDR(png_ptr,
                info_ptr,
                width,
                height,
                depth,
                PNG_COLOR_TYPE_RGB_ALPHA,
                PNG_INTERLACE_NONE,
                PNG_COMPRESSION_TYPE_DEFAULT,
                PNG_FILTER_TYPE_DEFAULT);

        /* Initialize rows of PNG. */
        row_pointers = png_malloc(png_ptr, height * sizeof (png_byte *));
        for (i = 0; i < height; ++i) {
            png_byte *row =
                    png_malloc(png_ptr, sizeof (uint8_t) * width * nBands);
            row_pointers[i] = row;
            for (j = 0; j < width; ++j) {
                *row++ = (uint8_t) Red[FrameNumber * height * width + i * width + j];
                *row++ = (uint8_t) Green[FrameNumber * height * width + i * width + j];
                *row++ = (uint8_t) Blue[FrameNumber * height * width + i * width + j];
                *row++ = (uint8_t) Alpha[FrameNumber * height * width + i * width + j];
            }
        }

        /* Write the image data to the file. */
        png_init_io(png_ptr, fp);
        png_set_rows(png_ptr, info_ptr, row_pointers);
        png_write_png(png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);

        for (i = 0; i < height; i++) {
            png_free(png_ptr, row_pointers[i]);
        }

        png_free(png_ptr, row_pointers);

        png_destroy_write_struct(&png_ptr, &info_ptr);

        fclose(fp);
        Status = 0;
    }

    return (Status);
}
