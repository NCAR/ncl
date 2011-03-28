/*
 * C routines needed to read a PNG file.
 */

#include <math.h>
#include <ncarg/c.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * Include "libpng" definitions.
 */

#include <png.h>

/*
 * The following macro is needed for "png" error handling.
 */

#ifndef png_jmpbuf
#  define png_jmpbuf(png_ptr) ((png_ptr)->jmpbuf)
#endif

/*
 * Declare space to hold a pointer to a gray-scale array and the
 * dimensions of the array.
 */

static int idim,jdim;
static unsigned char *gsva=(unsigned char*)NULL;

/*
 * GSVINI is called to read the PNG file whose name is "fnmp" and
 * extract from it a gray-scale array of dimension mgsv x nsgv, which
 * is placed in memory allocated by "malloc".  The input variables
 * "ired" and "igrn" specify weights to be used in the conversion to
 * gray-scale.  (Note that "iblu" = 100000-ired-igrn.)  If an error
 * occurs, "ierr" is returned non-zero.
 */

void NGCALLF(gsvini,GSVINI)(fnmp,mgsv,ngsv,ired,igrn,ierr)
  char *fnmp;
  int  *mgsv;
  int  *ngsv;
  int  *ired;
  int  *igrn;
  int  *ierr;
{
  FILE *fp;
  char sig[4];
  png_structp png_ptr;
  png_infop read_info_ptr,end_info_ptr;
  png_uint_32 width, height;
  int bit_depth,channels,color_type,interlace_type,j,rowbytes;
  fp=fopen(fnmp,"rb");
  if (!fp) {*ierr=1;return;}
  fread(sig,1,4,fp);
  if (png_sig_cmp((png_bytep)sig,(png_size_t)0,(png_size_t)4))
    {*ierr=2;return;}
  png_ptr=png_create_read_struct(PNG_LIBPNG_VER_STRING,(png_voidp)NULL,
          (png_error_ptr)NULL,(png_error_ptr)NULL);
  if (!png_ptr) {fclose(fp);*ierr=3;return;}
  read_info_ptr=png_create_info_struct(png_ptr);
  if (!read_info_ptr)
  { png_destroy_read_struct(&png_ptr,(png_infopp)NULL,(png_infopp)NULL);
    fclose(fp);
    *ierr=4; return; }
  end_info_ptr=png_create_info_struct(png_ptr);
  if (!end_info_ptr)
  { png_destroy_read_struct(&png_ptr,&read_info_ptr,(png_infopp)NULL);
    fclose(fp);
    *ierr=5;return; }
  if (setjmp(png_jmpbuf(png_ptr)))
  { png_destroy_read_struct(&png_ptr,&read_info_ptr,&end_info_ptr);
    fclose(fp);
    *ierr=6;return; }
  png_set_sig_bytes(png_ptr,4);  
  png_init_io(png_ptr,fp);
  png_read_info(png_ptr,read_info_ptr);
  png_get_IHDR(png_ptr,read_info_ptr,&width,&height,&bit_depth,
               &color_type,&interlace_type,NULL,NULL);
  channels=png_get_channels(png_ptr,read_info_ptr);
  rowbytes=png_get_rowbytes(png_ptr,read_info_ptr);
  if (bit_depth<8) {png_set_packing(png_ptr);}
  else if (bit_depth==16) {png_set_strip_16(png_ptr);}
  if (color_type==PNG_COLOR_TYPE_RGB||
      color_type==PNG_COLOR_TYPE_RGB_ALPHA)
  { png_set_rgb_to_gray_fixed(png_ptr,1,*ired,*igrn); }
  if (color_type==PNG_COLOR_TYPE_GRAY_ALPHA||
      color_type==PNG_COLOR_TYPE_RGB_ALPHA)
    {png_set_strip_alpha(png_ptr);}
  idim=width;
  jdim=height;
  *mgsv=idim;
  *ngsv=jdim;
  gsva=(unsigned char*)malloc(idim*jdim*sizeof(char));
  if (gsva==(unsigned char*)NULL) {*ierr=7; return;}
  for (j=0;j<jdim;j++)
    png_read_row(png_ptr,(png_bytep)gsva+idim*j,NULL);
  png_destroy_read_struct(&png_ptr,&read_info_ptr,&end_info_ptr);
  fclose(fp);
  *ierr=0;
  return;
}

/*
 * A Fortran reference to "GSVALU(IPIX,JPIX)" returns the gray-scale
 * value associated with the PNG indices IPIX and JPIX.
 */

float NGCALLF(gsvalu,GSVALU)(ipix,jpix)
  int *ipix;
  int *jpix;
{
  return (float)gsva[*ipix-1+idim*(jdim-*jpix)]/255.;
}

/*
 * A Fortran CALL GSVEND() frees the space allocated by GSVINI.
 */

void NGCALLF(gsvend,GSVEND)()
{
  if (gsva!=(unsigned char*)NULL) {
    free(gsva);
    gsva=(unsigned char*)NULL;
  }
}
