# telega.el
GNU Emacs telegram client (unofficial)

# Dependences

* circe for lui

# Building

* apt-get install libevent-dev
* git clone --recursive https://github.com/zevlg/tg.git && cd tg
* ./configure --disable-libconfig --disable-liblua --disable-json
* make libs/libtgl.so
* cp libs/libtgl.so /usr/local/lib
