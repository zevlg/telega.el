# telega.el
GNU Emacs telegram client (unofficial)

Join our telegram group https://t.me/emacs_telega

Uses nice https://core.telegram.org/tdlib

# Dependences

* circe for lui

# Building tdlib

* apt-get install gperf
* git clone https://github.com/tdlib/td.git
* cd td
* mkdir build && cd build && cmake ../ && make && make install

Probably install requires root permission

# Building telega-server

* git clone https://github.com/zevlg/telega.el
* cd telega.el
* make && make install

Start with `M-x telega RET` and follow instructions
