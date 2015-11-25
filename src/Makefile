
Y=$(shell yaws --version | sed 's/Yaws //')
YAWS=/usr/local/lib/erlang/lib/yaws-$(Y)/include


all:
	erlc  -I $(YAWS) xeno.erl

