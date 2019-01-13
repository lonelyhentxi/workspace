import unittest
from icmp_new import ping


class TestICMPNew(unittest.TestCase):
    def test_ping_success(self):
        delay_info = ping("baidu.com", timeout=10)
        self.assertAlmostEqual(round(delay_info[3], 2), 1)

    def test_ping_ip(self):
        delay_info = ping("114.114.114.114", timeout=10)
        self.assertAlmostEqual(round(delay_info[3], 2), 1)

    def test_ping_timeout(self):
        delay_info = ping("google.com", timeout=1)
        self.assertAlmostEqual(round(delay_info[3], 2), 0)

    def test_ping_wrong_hostname(self):
        delay_info = ping("xxsaffsaifjdfsajfdf.com", timeout=1)
        self.assertAlmostEqual(round(delay_info[3], 2), 0)

    def test_ping_source_ip(self):
        delay_info = ping("0.0.0.0", timeout=1)
        self.assertAlmostEqual(round(delay_info[3], 2), 0)

    def test_ping_broadcast_ip(self):
        delay_info = ping("255.255.255.255", timeout=1)
        self.assertAlmostEqual(round(delay_info[3], 2), 0)

    def test_ping_wrong_ip(self):
        delay_info = ping("0.257.0.0", timeout=1)
        self.assertAlmostEqual(round(delay_info[3], 2), 0)

    def test_ping_self_ip(self):
        delay_info = ping("127.0.0.1", timeout=1)
        self.assertAlmostEqual(round(delay_info[3], 2), 1)
