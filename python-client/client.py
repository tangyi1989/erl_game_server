import struct
import StringIO
import socket
import time
from proto import game_pb2


def pack(cmd, packet):
    string_io = StringIO.StringIO()

    proto_buf = packet.SerializeToString()
    string_io.write(struct.pack(">I",  0xffffffff))
    string_io.write(struct.pack(">I",  cmd))
    string_io.write(struct.pack(">I",  len(proto_buf)))
    string_io.write(proto_buf)

    string_io.seek(0)

    return string_io.read()

def hex_str(s):
    return ":".join("{0:x}".format(ord(c)) for c in s)

if __name__ == '__main__':
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(("127.0.0.1", 8888))

    login_packet = game_pb2.Login()
    login_packet.name = 'tang'
    login_packet.password = 'tangwanwan'

    buf = pack(123, login_packet)
    print  hex_str(buf)
    sock.send(buf)

    time.sleep(10)