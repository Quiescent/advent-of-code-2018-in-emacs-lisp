int main(int argc, char** argv) {
    // #ip 2
    int a = 0,
        b = 0,
        c = 0,
        d = 0,
        e = 0,
        f = 0;

    // c is the jumper...

line0:  e = 123;          // e seti 123 0 4;
line1:  e = e & 456;      // e bani 4 456 4;
line2:  e = e == 72;       // e eqri 4 72 4;
// line3:  c = e + c;        // c addr 4 2 2;
line3:  goto line5;
line4:  goto line1;
// line4:  c = 0;            // c seti 0 0 2;
line5:  e = 0;            // e seti 0 7 4;
// line6:  d = e & 65536;    // d bori 4 65536 3;
line6:  d = e | 65536;
line7:  e = 10283511;     // e seti 10283511 1 4;
// line8:  b = d & 255;      // b bani 3 255 1;
line8:  b = d % 256;
line9:  e = e + b;        // e addr 4 1 4;
// line10: e = e & 16777215; // e bani 4 16777215 4;
line10: e %= 16777216;
// line11: e = e * 65899;    // e muli 4 65899 4;
line11: e *= 65899;
// line12: e = e & 16777215; // e bani 4 16777215 4;
line12: e %= 16777216;
// line13: b = 256 > d;      // b gtir 256 3 1;
line13: b = d < 256;
// line14: c = b + c;        // c addr 1 2 2;
line14: if (b > 0) goto line16;
// line15: c = c + 1;        // c addi 2 1 2;
line15: goto line17;
// line16: c = 27;           // c seti 27 8 2;
line16: goto line28;
line17: b = 0;            // b seti 0 1 1;
line18: f = b + 1;        // f addi 1 1 5;
line19: f = f * 256;      // f muli 5 256 5;
// line20: f = f > d;        // f gtrr 5 3 5;
line20: if (f > d) goto line23; else f = 0;
line21: c = f + c;        // c addr 5 2 2;
// line22: c = c + 1;        // c addi 2 1 2;
line22: goto line24;
// line23: c = 25;           // c seti 25 3 2;
line23: goto line26;
line24: b = b + 1;        // b addi 1 1 1;
// line25: c = 17;           // c seti 17 0 2;
line25: goto line18;
line26: d = b;            // d setr 1 4 3;
// line27: c = 7;            // c seti 7 6 2;
line27: goto line8;
// line28: b = e == a;        // b eqrr 4 0 1;
line28: if (e == a) goto line31; else b = 0;
// line29: c = b + c;        // c addr 1 2 2;
// line30: c = e + c;        // c seti 5 2 2;
line30: goto line6;
line31: return 0;
}
