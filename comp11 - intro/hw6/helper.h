/* helper.h
 * Aubrey Anderson, COMP11 Spring 2018
 * 03/27/2018
 * Some helper functions for COMP11 homework 6.
 */
#ifndef __HELPER_H__
#define __HELPER_H__

#include <string>

struct Location {
    double latitude;
    double longitude;
};

/* Purpose: print an ascii art barcode
 * Parameter: a binary string barcode consisting of (0's and 1's)
 */
void print_barcode(std::string barcode);


/* Purpose:
 *      Calculates the distance between two points on the Earth (in km)
 *      Direct translation from http://en.wikipedia.org/wiki/Haversine_formula
 * Parameters
 *      loc1: location information (lat, long) for the first point in degrees
 *		loc2: location information (lat, long) for the second point in degrees
 * Returns:
  *     the distance between the two points in kilometers
 */
double distance(struct Location loc1, struct Location loc2);

#endif
