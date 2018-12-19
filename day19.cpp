#include <iostream>

using namespace std;

int main(const int argc, const char** argv) {
    int a = 0,
        b = 0,
        c = 0,
        d = 0,
        e = 0,
        f = 0;


line2:  e = 1; // seti 1 3 4

line3:  c = e * b; // mulr 1 4 2
line4:  if (c == f) goto line7; // c = c == f; // eqrr 2 5 2
line5:  // d = c + d; // addr 2 3 3
line6:  goto line8; // addi 3 1 3

line7:  a = a + b; // addr 1 0 0

line8:  e = e + 1; // addi 4 1 4
line9:  if (e > f) goto line12; // c = e > f; // gtrr 4 5 2
line10: // d = d + c; // addr 3 2 3
line11: goto line3; // seti 2 6 3

line12: b = b + 1; // addi 1 1 1
line13: if (b > f) goto line16; // c = b > f; // gtrr 1 5 2
line14: // d = d + c; // addr 2 3 3;
line15: goto line2; // d = 2; // seti 1 5 3;

line16: return a;
}
