import 'dart:math';
import 'dart:collection';
import 'package:test/test.dart';

int calculate(List<List<int>> rectangles) {
  var horizontal = SplayTreeMap<int, List<List<int>>>();
  for (var i = 0; i < rectangles.length; i++) {
    var r = rectangles[i];
    var left = r[0];
    var right = r[2];
    if (!horizontal.containsKey(left)) {
      horizontal[left] = [
        [i, 1]
      ];
    } else {
      horizontal[left].add([i, 1]);
    }
    if (!horizontal.containsKey(right)) {
      horizontal[right] = [
        [i, -1]
      ];
    } else {
      horizontal[right].add([i, -1]);
    }
  }
  var segments = Map<int, List<int>>();
  var currentSegment = Set<int>();
  for (var h in horizontal.keys) {
    for (var r in horizontal[h]) {
      if (r[1] == 1) {
        currentSegment.add(r[0]);
      } else {
        currentSegment.remove(r[0]);
      }
    }
    segments[h] = currentSegment.toList();
  }
  var length = -1;
  var area = 0;
  var lastH = 0;
  for (var h in segments.keys) {
    if (length != -1) {
      area += (h - lastH) * length;
    }
    lastH = h;
    length = 0;
    var vertical = SplayTreeMap<int, int>();
    for (var rk in segments[h]) {
      var r = rectangles[rk];
      var top = r[3];
      var bottom = r[1];
      if (!vertical.containsKey(bottom)) {
        vertical[bottom] = 1;
      } else {
        vertical[bottom] += 1;
      }
      if (!vertical.containsKey(top)) {
        vertical[top] = -1;
      } else {
        vertical[top] -= 1;
      }
    }
    var lastStart = 0;
    var counter = 0;
    var flag = true;
    for (var vk in vertical.keys) {
      counter += vertical[vk];
      if (flag && counter > 0) {
        lastStart = vk;
        flag = false;
      } else if (!flag && counter == 0) {
        length += vk - lastStart;
        flag = true;
      }
    }
  }
  return area;
}

int _calculateTwoRectangle(List<int> lhs, List<int> rhs) {
  var left = max(lhs[0], rhs[0]);
  var bottom = max(lhs[1], rhs[1]);
  var right = min(lhs[2], rhs[2]);
  var top = min(lhs[3], rhs[3]);
  var sum = (lhs[2] - lhs[0]) * (lhs[3] - lhs[1]) +
      (rhs[2] - rhs[0]) * (rhs[3] - rhs[1]);
  if (left < right && bottom < top) {
    var intersection = (right - left) * (top - bottom);
    var union = sum - intersection;
    return union;
  } else {
    return sum;
  }
}

void main() {
  test("1 rectangle (version 2)", () {
    expect(
        calculate(const [
          const [0, 4, 11, 6]
        ]),
        equals(22),
        reason: 'calculate([[0, 4, 11, 6]]]) should return 22');
  });
  test("2 rectangles", () {
    expect(calculate(
      const [
        const [0,0,1,1],
        const [1,1,2,2],
      ]
    ), 2);
  });
}
