/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *  Define all global variables (they all begin with the sentinel
 *  characters "ds_").
 */
double    ds_missing_value = -99999., ds_exponent = 3., ds_scale = 1.,
          ds_max_dist, ds_epsilon_test;
int       ds_error_status = 0, ds_set_maxpts = 0, ds_maxpoints, 
          ds_first_call = 1, ds_shadowing = 0, ds_set_max_dist = 0;

char      ds_error_file[256] = {"stderr"}, ds_emsg[256];

int       *ds_permutation_vector;

DSpointd3 *ds_input_points;
double    *ds_output, *ds_distances, *ds_weights;

DSpoints3 *ds_input_points_s;
float     *ds_output_s, *ds_distances_s, *ds_weights_s;
