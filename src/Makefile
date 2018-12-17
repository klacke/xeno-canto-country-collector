
Y=$(shell yaws --version | sed 's/Yaws //')
YAWS=$(HOME)/yaws/include


all:
	erlc  -I $(YAWS) xeno.erl
	erlc  -I $(YAWS) xeno_pick.erl


clean:
	rm -f *.beam


