import unittest
from codewars.fivekyu.bird_mountain import peak_height


class TestBirdMountain(unittest.TestCase):
    def test_peak_height(self):
        mountain = [
            "^^^^^^        ",
            " ^^^^^^^^     ",
            "  ^^^^^^^     ",
            "  ^^^^^       ",
            "  ^^^^^^^^^^^ ",
            "  ^^^^^^      ",
            "  ^^^^        "
        ]
        self.assertEqual(peak_height(mountain), 3)
