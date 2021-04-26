import unittest
from codewars.sixkyu.packet_delivery_enforcing_constrains import Package, \
    DimensionsOutOfBoundError


class TestPackage(unittest.TestCase):
    def test_bundle(self):
        allowed_inputs = [
            [20, 30, 10, 10],
            [0.2, 0.2, 0.2, 0.02],
            [350, 300, 150, 40],
            [99, 99, 99, 40]
        ]

        for inp in allowed_inputs:
            p = Package(*inp)
            self.assertEqual(p.length, inp[0])
            self.assertEqual(p.width, inp[1])
            self.assertEqual(p.height, inp[2])
            self.assertEqual(p.weight, inp[3])

        inputs = (0.2, 0.2, 0.2, 0.2)
        p = Package(*inputs)
        p.length = 2
        self.assertEqual(p.length, 2)
        p.width = 100
        self.assertEqual(p.width, 100)
        p.height = 149
        self.assertEqual(p.height, 149)
        p.weight = 40
        self.assertEqual(p.weight, 40)

        inputs = (0.2, 0.2, 0.2, 0.2)
        p = Package(*inputs)
        try:
            p.length = -20
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")

        except DimensionsOutOfBoundError as e:
            self.assertEqual(
                str(e), "Package length==-20 out of bounds, should be: 0 < length <= 350")
        try:
            p.length = 2000
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")

        except DimensionsOutOfBoundError as e:
            self.assertEqual(
                str(e), "Package length==2000 out of bounds, should be: 0 < length <= 350")

        try:
            p.width = -20
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")
        except DimensionsOutOfBoundError as e:
            self.assertEqual(
                str(e), "Package width==-20 out of bounds, should be: 0 < width <= 300")
        try:
            p.width = 2000
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")

        except DimensionsOutOfBoundError as e:
            self.assertEqual(
                str(e), "Package width==2000 out of bounds, should be: 0 < width <= 300")

        try:
            p.height = -20
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")
        except DimensionsOutOfBoundError as e:
            self.assertEqual(
                str(e), "Package height==-20 out of bounds, should be: 0 < height <= 150")
        try:
            p.height = 2000
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")

        except DimensionsOutOfBoundError as e:
            self.assertEqual(
                str(e), "Package height==2000 out of bounds, should be: 0 < height <= 150")

        try:
            p.weight = -20
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")
        except DimensionsOutOfBoundError as e:
            self.assertEqual(
                str(e), "Package weight==-20 out of bounds, should be: 0 < weight <= 40")
        try:
            p.weight = 2000
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")

        except DimensionsOutOfBoundError as e:
            self.assertEqual(
                str(e), "Package weight==2000 out of bounds, should be: 0 < weight <= 40")

        try:
            p = Package(*[-20, 30, 10, 10])
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")
        except DimensionsOutOfBoundError as e:
            self.assertEqual(
                str(e), "Package length==-20 out of bounds, should be: 0 < length <= 350")

        try:
            p = Package(*[351, 30, 10, 10])
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")
        except DimensionsOutOfBoundError as e:
            self.assertEqual(
                str(e), "Package length==351 out of bounds, should be: 0 < length <= 350")

        try:
            p = Package(*[20, -30, 10, 10])
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")
        except DimensionsOutOfBoundError as e:
            self.assertEqual(str(e), "Package width==-30 out of bounds, should be: "
                             "0 < width <= 300")

        try:
            p = Package(*[20, 301, 10, 10])
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")
        except DimensionsOutOfBoundError as e:
            self.assertEqual(str(e), "Package width==301 out of bounds, should be: "
                             "0 < width <= 300")

        try:
            p = Package(*[20, 30, -10, 10])
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")
        except DimensionsOutOfBoundError as e:
            self.assertEqual(
                str(e), "Package height==-10 out of bounds, should be: 0 < height <= 150")

        try:
            p = Package(*[20, 10, 151, 10])
            self.assertEqual(
                True, False, "Should have raised DimensionsOutOfBoundError")
        except DimensionsOutOfBoundError as e:
            self.assertEqual(
                str(e), "Package height==151 out of bounds, should be: 0 < height <= 150")

        p = Package(10, 12, 13, 20)
        self.assertEqual(p.volume, 10 * 12 * 13)
        p.length = 24
        self.assertEqual(p.volume, 24 * 12 * 13)
