/* 
 * mid3.cpp
 * Used in Lab 4
 * Created by: Margaret Gorguissian
 * Modified by: Vladimir Hugec
 * Date: Feb 15, 2018
 */

#include <iostream>
using namespace std;

float mid3(float a, float b, float c);

int main()
{
	float a, b, c;
	cin >> a >> b >> c;
	int mid_of_3 = mid3(a, b, c);
	cout << mid_of_3 << endl;
	
	return 0;
}

/*
 * mid3
 * Arguments: three floats
 * Purpose: find the middle value of three floats (compared using <)
 * Returns: a float, the middle value
 */
float mid3(float a, float b, float c)
{
  float x = 0;
  if (a > b and b > c) {
    x = b;
  }
  else if (b > a and a > c) {
    x = a;
  }
   else if (a > c and c > b) {
     x = c;
   }
  else if (c > a and a > b) {
    x = a;
   }
  else if (c > b and b > a) {
    x = b;
   }
  else if (b > c and c > a) {
    x = c;
   }
  return x;
}
