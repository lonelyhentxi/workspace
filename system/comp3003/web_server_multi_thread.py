from concurrent.futures import ThreadPoolExecutor
from socket import *

HOST = '10.251.130.39'
PORT = 6789
BUFSIZE = 1024
ADDR = (HOST, PORT)
MAX_THREAD = 8

tcp_server_socket = socket(AF_INET, SOCK_STREAM)
tcp_server_socket.bind(ADDR)
tcp_server_socket.listen(1)


def common_send(cli_socket):
    try:
        data = cli_socket.recv(BUFSIZE)
        if len(data.split()) >= 2:
            filename = data.split()[1]
            print(filename[1:])
            f = open(filename[1:])
            outputdata = f.read()
            header = 'HTTP/1.1 200 OK\r\n\r\n'
            cli_socket.send(header.encode())
            for i in range(0, len(outputdata)):
                cli_socket.send(outputdata[i].encode())
            cli_socket.close()
    except IOError:
        header = 'HTTP/1.1 404 NOT FOUND\r\n\r\n'
        cli_socket.send(header.encode())
        cli_socket.close()


with ThreadPoolExecutor(max_workers=1) as executor:
    while True:
        print('waiting for connection...')
        tcp_client_socket, addr = tcp_server_socket.accept()
        executor.map(common_send,tcp_client_socket,timeout=60)
    tcp_server_socket.close()
