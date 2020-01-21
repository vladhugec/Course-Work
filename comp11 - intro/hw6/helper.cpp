/* helper.cpp
 * Aubrey Anderson, COMP11 Spring 2018
 * 03/27/2018
 * Some helper functions for COMP11 homework 6.
 */

#include <iostream>
#include <math.h>
#include <cmath>

#include "helper.h"

const double EARTH_RADIUS_KM = 6371.0;

using namespace std;

/* Purpose: print an ascii art barcode
 * Parameter: a binary string barcode consisting of (0's and 1's)
 */
void print_barcode(string barcode) {
    int barcode_len = barcode.length();

    for (int i = 0; i < barcode_len; i++) {
        // Comp11 students are not expected to understand the line below
        // but if you are curious then google 'ternary operator'.
        // In short, the ternary operator is a conditional operator.
        cout << (barcode[i] == '0' ? " " : "|");
    }
    
    cout << endl;

    for (int i = 0; i < barcode_len; i++) {
        cout << "|";
    }
}


/************ Code below was copied from a Stackoverflow post ****************/
/*                                                                           */
/* https://stackoverflow.com/questions/10198985/                             */
/*         calculating-the-distance-between-2-latitudes-and-longitudes-that  */
/*         -are-saved-in-a/10205532#10205532                                 */
/*                                                                           */
/* Altered code to use Location objects and used floating point constants    */
/*****************************************************************************/

/* Purpose: converts decimal degrees to radians
 * Parameter: value in degrees
 * Returns: value in radians
 */
double deg2rad(double deg) {
    return (deg * M_PI / 180.0);
}

/* Purpose:
 *      Calculates the distance between two points on the Earth (in km)
 *      Direct translation from http://en.wikipedia.org/wiki/Haversine_formula
 * Parameters
 *      loc1: location information (lat, long) for the first point in degrees
 *      loc2: location information  for the second point in degrees
 * Returns:
  *     the distance between the two points in kilometers
 */
double distance(struct Location loc1, struct Location loc2) {
    double lat1r, lon1r, lat2r, lon2r, u, v;

    lat1r = deg2rad(loc1.latitude);
    lon1r = deg2rad(loc1.longitude);
    lat2r = deg2rad(loc2.latitude);
    lon2r = deg2rad(loc2.longitude);
    u = sin((lat2r - lat1r) / 2.0);
    v = sin((lon2r - lon1r) / 2.0);

    return 2.0 * EARTH_RADIUS_KM
               * asin(sqrt(u * u + cos(lat1r) * cos(lat2r) * v * v));
}

