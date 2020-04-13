import socket
import time
import sys
import struct
import select
import os
from typing import Optional

PING_SIZE = 8
ICMP_PROTO = socket.getprotobyname("icmp")
ICMP_ECHO_REQUEST = 8
CONNECT_ERROR_INFO = {
    3: {
        0: "Network Unreachable", 1: "Host Unreachable",
        2: "Protocol Unreachable", 3: "Port Unreachable",
        4: "Fragmentation needed but no frag. bit set",
        5: "Source routing failed",
        6: "Destination network unknown",
        7: "Destination host unknown", 9: "Destination network administratively prohibited",
        10: "Destination host administratively prohibited", 11: "Network unreachable for TOS",
        12: "Host unreachable for TOS", 13: "Communication administratively prohibited by filtering",
        14: "Host precedence violation", 15: "Precedence cutoff in effect"},
    11: {0: "TTL equals 0 during transit",
         1: "TTL equals 0 during reassembly"},
    12: {0: "IP header bad (catchall error)",
         1: "Required options missing"},
    18: {0: "Address mask reply"}
}

if sys.platform == "win32":
    default_timer = time.clock
else:
    default_timer = time.time


def checksum(source_string):
    sum = 0
    countTo = (len(source_string) / 2) * 2
    count = 0
    while count < countTo:
        thisVal = source_string[count + 1] * 256 + source_string[count]
        sum = sum + thisVal
        sum = sum & 0xffffffff
        count = count + 2

    if countTo < len(source_string):
        sum = sum + source_string[len(source_string) - 1]
        sum = sum & 0xffffffff

    sum = (sum >> 16) + (sum & 0xffff)
    sum = sum + (sum >> 16)
    answer = ~sum
    answer = answer & 0xffff

    answer = answer >> 8 | (answer << 8 & 0xff00)
    return answer


def receive_one_ping(sock: socket.socket, identity: int, timeout: int) -> float:
    left_time = timeout
    while True:
        start_select = default_timer()
        what_ready = select.select([sock], [], [], left_time)
        how_long_in_select = (default_timer() - start_select)
        if what_ready[0] == []:
            raise TimeoutError("request timeout.")
        recv_time = default_timer()
        recv_packet, recv_addr = sock.recvfrom(1024)
        icmp_header = recv_packet[20:28]
        recv_type, recv_code, recv_checksum, packet_id, sequence = struct.unpack(
            "bbHHh", icmp_header
        )
        if recv_type in CONNECT_ERROR_INFO:
            type_info = CONNECT_ERROR_INFO[recv_type]
            if recv_code in type_info:
                raise ConnectionError(type_info[recv_code] + ".")
        if recv_type != ICMP_ECHO_REQUEST and packet_id == identity:
            bytes_in_double = struct.calcsize('d')
            sent_time = struct.unpack('d', recv_packet[28:28 + bytes_in_double])[0]
            return recv_time - sent_time
        left_time = left_time - how_long_in_select
        if left_time <= 0:
            raise TimeoutError("request timeout.")


def send_one_ping(sock: socket.socket, dest_addr: str, ID: str):
    my_checksum = 0
    header = struct.pack("bbHHh", ICMP_ECHO_REQUEST, 0, my_checksum, ID, 1)
    bytes_in_double = struct.calcsize('d')
    data = bytes((192 - bytes_in_double) * 'Q', 'utf-8')
    data = struct.pack('d', default_timer()) + data
    my_checksum = checksum(header + data)
    header = struct.pack(
        "bbHHh", ICMP_ECHO_REQUEST, 0, socket.htons(my_checksum), ID, 1
    )
    packet = header + data
    sock.sendto(packet, (dest_addr, 1))


def do_one_ping(dest_addr: str, timeout: int) -> float:
    with socket.socket(family=socket.AF_INET,
                       type=socket.SOCK_RAW, proto=ICMP_PROTO) as sock:
        my_id = os.getpid() & 0xFFFF

        send_one_ping(sock, dest_addr, my_id)
        delay = receive_one_ping(sock, my_id, timeout)
    return delay * 1000


def ping(hostname: str, timeout: int) -> [float, float, float, float]:
    try:
        dest = socket.gethostbyname(hostname)
    except socket.error as e:
        print("")
        print(f"ping {hostname} [] using python:")
        print("")
        print(f"domain not found or fail to get address from {hostname}")
        print("ping end.")
        return [0, 0, 0, 0]
    print("")
    print(f"pinging {hostname} [{dest}] using python:")
    print("")
    max_rtt, min_rtt, sum_rtt, success_size = 0, sys.maxsize, 0, 0
    for idx in range(PING_SIZE):
        try:
            delay = do_one_ping(dest, timeout)
        except TimeoutError or ConnectionError as e:
            print(e)
        except OSError as e:
            print(e.args[1])
            print("ping end.")
            return [0, 0, 0, 0]
        except Exception as e:
            print(e)
            print("ping end.")
            return [0, 0, 0, 0]
        else:
            print(f"recv from {dest}: {delay:.2f} ms.")
            min_rtt = min(min_rtt, delay)
            max_rtt = max(max_rtt, delay)
            sum_rtt += delay
            success_size += 1
        time.sleep(1)
    success_per = float(success_size) / float(PING_SIZE)
    ave_rtt = sum_rtt / float(success_size) if success_size != 0 else 0
    print("")
    print(
        f"ping {PING_SIZE} times, {success_size} times success, {success_per * 100}% percent"
        + f", loss {(1 - success_per) * 100}% percent.")
    print(f"min rtt: {min_rtt if min_rtt != sys.maxsize else 0:.2f} ms")
    print(f"max rtt: {max_rtt:.2f} ms")
    print(f"ave rtt: {ave_rtt:.2f} ms")
    print("ping end.")
    return [min_rtt, max_rtt, ave_rtt, success_per]
