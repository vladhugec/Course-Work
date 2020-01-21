/* postnet.h
 * Vladimir Hugec, COMP11 Spring 2018
 * 04/4/2018
 * Declarations
 */

#include <string>

class Postnet {
    private: 
        int zip; //stores source
        int destinationZip; //stores destination
    
        int convertToZip(std::string barcode); //converts barcode to zipcode
        std::string convertToBar(int zipcode); //converts zipcode to barcode

    public:
        // make
        Postnet(int zipcode, int destination);
        Postnet(std::string barcode, std::string destination);
        // get:
        int destinationCode(); // returns destinationZip
        int zipcode(); //returns zipcode
        std::string barcode(); // returns barcode
};

struct rawData {
    int zipcodes[5000];
    std::string city[5000];
    std::string state[5000];
    double latitude[5000];
    double longitude[5000];
};

class Postdata {
    private:
        rawData rData[10];
        
        // storing destination info
        int theZip;
        std::string theCity;
        std::string theState;
        double theLat;
        double theLong;

        // storing send from info
        int urZip;
        std::string urCity;
        std::string urState;
        double urLat;
        double urLong;


    public:
        // make:
        Postdata(int zipcode, int destination);
        // do:
        void print_barcode(std::string barcode); // prints the barcode
        void printLabel(); // prints out the label
        // get:
        std::string getCity(int destOrNot); //returns name of city
        std::string getState(int destOrNot); //returns state
        double getLat(int destOrNot); //returns latitude
        double getLong(int destOrNot); //returns longitude 
            // int destOrNot will be [0] if getting source info
            //               will be [1] if getting destination info
};


struct Location {
    double latitude;
    double longitude;
};

std::string formString(int zipcode); //forms a string of [0-9] digits
int formInt(std::string something); //forms an int from string of nums

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

