/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <stdio.h>

extern  double    ds_missing_value, ds_exponent, ds_scale, ds_max_dist,
                  ds_epsilon_test;

extern  int       ds_error_status, ds_set_maxpts, ds_maxpoints,
                  ds_first_call, ds_shadowing, ds_set_max_dist;

extern  char      ds_error_file[], ds_emsg[];

extern  int       *ds_permutation_vector;

extern  DSpointd3 *ds_input_points;
extern  double    *ds_output, *ds_distances, *ds_weights;

extern  DSpoints3 *ds_input_points_s;
extern  float     *ds_output_s, *ds_distances_s, *ds_weights_s;
