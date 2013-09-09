
#include <math.h>
#include "DirectVincenty.h"

void DirectVincenty(double lat_in, double lon_in,
        double alpha, double s,
        double * lat_out, double * lon_out,
        double a, double f) {

    if (a == 0.0 || f == 0.0) { /*WGS-84 (EPSG4325)*/
        a = 6378137.0;
        f = 1.0 / 298.257223563;
    }

    double b = (1.0 - f) * a;
    double sin_a1 = sin(alpha);
    double cos_a1 = cos(alpha);

    double tan_u1 = (1.0 - f) * tan(lat_in);
    double u1 = atan(tan_u1);
    double sin_u1 = sin(u1);
    double cos_u1 = cos(u1);

    double sigma1 = atan2(tan_u1, cos_a1);
    double sin_alpha = cos_u1*sin_a1;
    double cos2_alpha = (1.0 - sin_alpha)*(1.0 + sin_alpha);

    double u2 = cos2_alpha * (a * a - b * b) / (b * b);

    double sqrt_1pu2 = sqrt(1.0 + u2);
    double k1 = (sqrt_1pu2 - 1.0) / (sqrt_1pu2 + 1.0);
    double k12 = k1*k1;
    double A = (1.0 + 0.25 * k12) / (1.0 - k1);
    double B = k1 * (1.0 - 3.0 / 8.0 * k12);

    double sigma = s / (b * A);
    double sigmaOld, do_sigma_m, d_sigma;
    
    do {
        sigmaOld = sigma;
        do_sigma_m = 2.0 * sigma1 + sigma;
        d_sigma = B * sin(sigma)*(
                cos(do_sigma_m) + 0.25 * B * (
                cos(sigma)*(-1.0 + 2.0 * cos(do_sigma_m) * cos(do_sigma_m))
                -(1.0 / 6.0) * B * cos(do_sigma_m)*(-3.0 + 4.0 * sin(sigma) * sin(sigma))*(-3.0 + 4.0 * cos(do_sigma_m) * cos(do_sigma_m))
                )
                );
        sigma = s / (b * A) + d_sigma;
    } while ((sigmaOld - sigma) > 1.0e-6 || (sigmaOld - sigma)<-1.0e-6);

    do_sigma_m = 2.0 * sigma1 + sigma;
    *lat_out = atan2(sin_u1 * cos(sigma) + cos_u1 * sin(sigma) * cos_a1,
            (1.0 - f) * sqrt(sin_alpha * sin_alpha + pow((sin_u1 * sin(sigma) - cos_u1 * cos(sigma) * cos_a1), 2.0)));
    double lambda = atan2(sin(sigma) * sin_a1,
            cos_u1 * cos(sigma) - sin_u1 * sin(sigma) * cos_a1);
    double C = (f / 16.0) * cos2_alpha * (4.0 + f * (4.0 - 3.0 * cos2_alpha));
    double dLon = lambda - (1.0 - C) * f * sin_alpha * (
            sigma + C * sin(sigma)*(
            cos(do_sigma_m) + C * cos(sigma)*(-1.0 + 2.0 * cos(do_sigma_m) * cos(do_sigma_m))
            )
            );
    *lon_out = lon_in + dLon;
}