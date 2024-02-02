FROM archlinux:base-devel
RUN pacman -Syu --noconfirm
RUN pacman -S --noconfirm iproute2 libcap openbsd-netcat

COPY target/debug/toytun_server /usr/bin/toytun_server
COPY scripts/run_server.sh /usr/bin/run_server.sh
CMD bash /usr/bin/run_server.sh