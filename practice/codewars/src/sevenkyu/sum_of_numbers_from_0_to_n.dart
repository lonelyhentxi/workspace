import 'package:test/test.dart';

class SequenceSum {
  static String showSequence(num n) {
    if(n<0) {
      return "${n}<0";
    } else if(n==0) {
      return "0=0";
    } else {
      var sum = 0;
      var sequence = <int>[];
      for(var i=0;i<=n;i++) {
        sum+=i;
        sequence.add(i);
      }
      return '${sequence.map((i){return i.toString();}).join("+")} = ${sum}';
    }
  }
}

void main() {
  test("sum of numbers from 0 to n", () {
    expect(SequenceSum.showSequence(0), equals("0=0"));
    expect(SequenceSum.showSequence(6), equals("0+1+2+3+4+5+6 = 21"));
  });
}
