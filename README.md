# telega.el
GNU Emacs telegram client (unofficial)

Uses nice https://core.telegram.org/tdlib

# Dependences

* circe for lui

# Building

* apt-get install gperf
* git clone https://github.com/tdlib/td.git
* cd td
* mkdir build && cd build && cmake ../ && make && make install

# Compiling telega-server

* git clone https://github.com/zevlg/telega.el
* cd telega.el/server
* make
