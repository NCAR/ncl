#ifndef _Vincenty_
#define _Vincenty_

void DirectVincenty(double lat_in, double lon_in,
        double alpha, double s,
        double * lat_out, double * lon_out,
        double a, double f);

#endif
