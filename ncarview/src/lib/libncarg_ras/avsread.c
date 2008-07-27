/*
 * $Id: avsread.c,v 1.4 2008-07-27 03:18:45 haley Exp $
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

 static
read_image_compute(data, filename)
    AVSfield_char **data;
    char *filename;
{
    AVSfield_char *input;
    FILE *pf;
    int dims[2];
    float  min_extent[2], max_extent[2];
    unsigned int cx, cy;
    unsigned char header[8];

    /* No data file name */
    if (!filename)
      return(1);

    /* Attempt to open file */
    if (!(pf = fopen(filename, "r"))) {
	AVSmessage(file_version, AVS_Warning, AVSmodule, "read_image_compute",
		   "Ok", "Can't open data file %s", filename);
	return(0);
    }
    
    /* Free old memory */
    if (*data) 
      AVSfield_free(*data);
    
    /* Read dimensions */

    if (fread(header, sizeof(header), 1, pf) != 1) {
	AVSmessage(file_version, AVS_Warning, AVSmodule, "read_image_compute",
		   "Ok", "Error reading header from file %s", filename);
	fclose(pf);
	return(0);
    }

    cx = (header[2] << 8) + header[3];
    cy = (header[6] << 8) + header[7];
   
    /* Some old image files on little endian machines might have the */
    /* bytes reversed. */

    if (cx == 0 && cy == 0) {
	cx = (header[1] << 8) + header[0];
	cy = (header[5] << 8) + header[4];
    }

    /* Set dimensions */
    dims[0] = cx;
    dims[1] = cy;
    
    /* Allocate space for image data */
    input = (AVSfield_char *)AVSdata_alloc("field 2D 4-vector byte", dims);
    
    /* Read data, 4 bytes per pixel. */
    if (fread(input->data, 4, (int)(cx*cy), pf) != (int) (cx * cy)) {
	AVSmessage(file_version, AVS_Warning, AVSmodule, "read_image_compute",
		   "Ok", "Error reading file %s", filename);
	fclose(pf);
	AVSfield_free(input);
	return(0);
    }
    
    /* Close the file */
    fclose(pf);
    
    /* Set the output pointer */
    *data = input;
    
    /* set the labels for each component */
    AVSfield_set_labels (input, "alpha;red;green;blue", ";");

    /* set the extent for the image */
    min_extent[0] = 0.0;               min_extent[1] = 0.0;
    max_extent[0] = (float) cx - 1.0;  max_extent[1] = (float) cy - 1.0;
    AVSfield_set_extent (input, min_extent, max_extent);

    /* Indicate success */
    return(1);
}
