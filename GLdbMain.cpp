#include "GLdbCommon.hpp"

__thread int i;

int main(int, char**) {
  ADDR ab;
  UINT a,b;

  a = 100;
  ab = a;
  b = ab.aLong;
  i = b;
  return 0;
}
