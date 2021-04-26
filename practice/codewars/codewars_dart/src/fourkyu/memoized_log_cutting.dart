import 'dart:math';
import 'package:test/test.dart';

int cutLog(List p, int n) {
  var l = p.length;
  var m = List.filled(l+1, List.filled(n+1, 0));
  for(int i=1;i<=l;i++) {
    for(int j=1;j<=n;j++) {
      if(i>j) {
        m[i][j] = m[i-1][j];
      } else {
        m[i][j] = max(m[i-1][j], p[i] + m[i-1][j-i]);
      }
    }
  }
  return m[l][n];
}

void main() {
  var p = [ 0, 1, 5, 8, 9, 10, 17, 17, 20, 24,
    30, 32, 35, 39, 43, 43, 45, 49, 50, 54,
    57, 60, 65, 68, 70, 74, 80, 81, 84, 85,
    87, 91, 95, 99, 101, 104, 107, 112, 115, 116,
    119, 121, 125, 129, 131, 134, 135, 140, 143, 145,
    151
  ];

  group("Example Log", () {
    test("The optimal price for n = 0 should be \$0", () =>
        expect(cutLog(p, 0), equals(0)));
    test("The optimal price for n = 1 should be \$1", () =>
        expect(cutLog(p, 1), equals(1)));
    test("The optimal price for n = 5 should be \$13", () =>
        expect(cutLog(p, 5), equals(13)));
    test("The optimal price for n = 8 should be \$22", () =>
        expect(cutLog(p, 8), equals(22)));
    test("The optimal price for n = 10 should be \$30", () =>
        expect(cutLog(p, 10), equals(30)));
    test("The optimal price for n = 22 should be \$65", () =>
        expect(cutLog(p, 22), equals(65)));
    test("The optimal price for n = 23 should be \$69", () =>
        expect(cutLog(p, 23), equals(69)));
    test("The optimal price for n = 37 should be \$112", () =>
        expect(cutLog(p, 37), equals(112)));
    test("The optimal price for n = 41 should be \$125", () =>
        expect(cutLog(p, 41), equals(125)));
    test("The optimal price for n = 50 should be \$153", () =>
        expect(cutLog(p, 50), equals(153)));
  });
}
