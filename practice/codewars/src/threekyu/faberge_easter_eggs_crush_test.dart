import "package:test/test.dart";

BigInt height(int n, int m) {
  var h = BigInt.from(0);
  var t = BigInt.from(1);
  for(var i=1;i<=n;i++) {
    t = t * BigInt.from(m - i + 1);
    t = t ~/ BigInt.from(i);
    h = h + t;
  }
  return h;
}

void main() {
  test("should work for some basic tests", () {
    expect(height(1, 51), equals(BigInt.from(51)));
    expect(height(2, 1), equals(BigInt.one));
    expect(height(4, 17), equals(BigInt.from(3213)));
    expect(height(16, 19), equals(BigInt.from(524096)));
    expect(height(23, 19), equals(BigInt.from(524287)));
  });
  test("should work for some advanced tests", () {
    expect(height(13, 550), equals(BigInt.parse('60113767426276772744951355')));
    expect(height(271, 550), equals(
        BigInt.parse('1410385042520538326622498273346382708200418583791594039531058458108130216985983794998105636900856496701928202738750818606797013840207721579523618137220278767326000095')));
    expect(height(531, 550), equals(
        BigInt.parse('3685510180489786476798393145496356338786055879312930105836138965083617346086082863365358130056307390177215209990980317284932211552658342317904346433026688858140133147')));
  });
}

