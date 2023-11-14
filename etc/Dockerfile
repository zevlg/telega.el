### Latest image
# docker pull zevlg/telega-server:latest
# docker build -f etc/Dockerfile -t zevlg/telega-server:latest .
# docker tag $(docker images -q zevlg/telega-server:latest) zevlg/telega-server:latest
# docker push zevlg/telega-server:latest
# docker run -v $HOME/.telega:$HOME/.telega zevlg/telega-server:latest /usr/bin/telega-server

### Release image
# docker build --build-arg tdlib_branch=v1.8.0 --build-arg telega_branch=release-0.8.0 -f etc/Dockerfile -t zevlg/telega-server:1.8.0 .
# docker tag $(docker images -q zevlg/telega-server:1.8.0) zevlg/telega-server:1.8.0
# docker push zevlg/telega-server:1.8.0

# NOTE: We use alpine:3.13 because latest image does not have
# libappindicator-dev and, also due to
# https://wiki.alpinelinux.org/wiki/Release_Notes_for_Alpine_3.14.0#faccessat2

FROM alpine:3.13 AS builder
ARG tdlib_branch=master
ARG telega_branch=master

RUN set +x && apk update && apk upgrade && \
    apk add --update alpine-sdk linux-headers git zlib-dev openssl-dev gperf php cmake

RUN echo "TDLib v1.8.21-6ee64289f" > tdlib-version && \
    git clone https://github.com/tdlib/td.git && \
    cd td && \
    git checkout ${tdlib_branch} && \
    mkdir build && \
    cd build && \
    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX:PATH=/usr .. && \
    cmake --build . --target install

RUN set +x && apk update && apk upgrade && \
    apk add --update pkgconfig libappindicator-dev libwebp-tools libpng-dev ffmpeg

# build librlottie from sources
RUN cd / && git clone https://github.com/Samsung/rlottie.git && \
    cd rlottie && \
    mkdir build && \
    cd build && \
    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX:PATH=/usr .. && \
    cmake --build . --target install

RUN echo "tgs2png 0.3.0" > /tgs2png-version && \
    cd / && git clone https://github.com/zevlg/tgs2png && \
    cd tgs2png && \
    mkdir build && \
    cd build && \
    cmake .. && \
    cmake --build . && \
    install tgs2png /usr/bin

ADD server /telega-server
RUN cd /telega-server && \
    make && make INSTALL_PREFIX=/usr/bin install

# ADD "https://www.random.org/cgi-bin/randbyte?nbytes=10&format=h" skipcache
# RUN cd / && git clone https://github.com/zevlg/telega.el && \
#     cd telega.el && \
#     git checkout ${telega_branch} && \
#     cd server && \
#     make && make INSTALL_PREFIX=/usr/bin install

# RUN set +x && apk update && apk upgrade && \
#     apk add --update bash

# second stage
FROM alpine:3.13

# - psmisc (with killall tool) used to kill ffmpeg processes
# - libqrencode (with qrencode tool) used for QR code auth method
RUN set +x && apk update && apk upgrade && \
    apk add --update zlib-dev openssl ffmpeg libwebp-tools psmisc libqrencode bash

# To make ALSA work
COPY etc/asound.conf /etc/asound.conf

COPY --from=builder /lib/libuuid.so* /lib/libmount.so* /lib/
COPY --from=builder /usr/lib/libgcc_s.so* /usr/lib/libstdc++.so* /usr/lib/librlottie.so* /usr/lib/libpng.so* /usr/lib/libpng16.so* /usr/lib/libtdjson.so* /usr/lib/libappindicator3.so* /usr/lib/libgtk-3.so* /usr/lib/libgobject-2.0.so* /usr/lib/libglib-2.0.so* /usr/lib/libindicator3.so* /usr/lib/libdbusmenu-gtk3.so* /usr/lib/libdbusmenu-glib.so* /usr/lib/libgio-2.0.so* /usr/lib/libgdk-3.so* /usr/lib/libgmodule-2.0.so* /usr/lib/libintl.so* /usr/lib/libpangocairo-1.0.so* /usr/lib/libpango-1.0.so* /usr/lib/libharfbuzz.so* /usr/lib/libcairo.so* /usr/lib/libpangoft2-1.0.so* /usr/lib/libfontconfig.so* /usr/lib/libfreetype.so* /usr/lib/libfribidi.so* /usr/lib/libcairo-gobject.so* /usr/lib/libgdk_pixbuf-2.0.so* /usr/lib/libatk-1.0.so* /usr/lib/libepoxy.so* /usr/lib/libXi.so* /usr/lib/libX11.so* /usr/lib/libatk-bridge-2.0.so* /usr/lib/libXfixes.so* /usr/lib/libffi.so* /usr/lib/libpcre.so* /usr/lib/libxkbcommon.so* /usr/lib/libwayland-client.so* /usr/lib/libwayland-cursor.so* /usr/lib/libwayland-egl.so* /usr/lib/libXext.so* /usr/lib/libXcursor.so* /usr/lib/libXdamage.so* /usr/lib/libXcomposite.so* /usr/lib/libXrandr.so* /usr/lib/libXinerama.so* /usr/lib/libgraphite2.so* /usr/lib/libpixman-1.so* /usr/lib/libxcb-shm.so* /usr/lib/libxcb.so* /usr/lib/libxcb-render.so* /usr/lib/libXrender.so* /usr/lib/libexpat.so* /usr/lib/libbz2.so* /usr/lib/libbrotlidec.so* /usr/lib/libdbus-1.so* /usr/lib/libatspi.so* /lib/libblkid.so* /usr/lib/libXau.so* /usr/lib/libXdmcp.so* /usr/lib/libbrotlicommon.so* /usr/lib/libbsd.so* /usr/lib/libmount.so* /usr/lib/
COPY --from=builder /usr/bin/tgs2png /usr/bin/telega-server /usr/bin/

CMD ["/bin/bash"]
