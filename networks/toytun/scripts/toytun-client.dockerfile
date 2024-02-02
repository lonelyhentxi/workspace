FROM archlinux:base-devel
RUN pacman -Syu --noconfirm
RUN pacman -S --noconfirm iproute2 libcap openbsd-netcat

COPY target/debug/toytun_client /usr/bin/toytun_client
COPY scripts/run_client.sh /usr/bin/run_client.sh
CMD bash /usr/bin/run_client.sh