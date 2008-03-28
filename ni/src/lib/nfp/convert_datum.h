/*
 * convert_datum.h
 * Peter Daly
 * MIT Ocean Acoustics
 * Copyright (C) 1998 Massachusetts Institute of Technology
 * All Rights Reserved
 */
#define GRID_ZONE_LENGTH 4
#define LOWER_EPS_LIMIT 1e-14	/* kind of arbitrary... */

#define CLARKE_1866_DATUM 0
#define GRS_80_DATUM      1
#define WGS_84_DATUM      2

typedef struct UTM_struct {
  char grid_zone[GRID_ZONE_LENGTH];
  double x, y;
} UTM;

typedef struct LL_struct {
  double latitude, longitude;
} LL;

void get_grid_zone (LL*, char *, double *);
int get_lambda0 (char *, double *);
int ll2utm (LL *, UTM *, unsigned char);
int utm2ll (UTM *, LL *, unsigned char);
