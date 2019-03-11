TARGETS = main.cgi

all : $(TARGETS)

CC = ghc

%.cgi : %.hs
	$(CC) $^ -o root/cgi-bin/$@

test:
	lighttpd -D -f testsvr.conf

clean:
	rm *.o *.hi *.cgi root/cgi-bin/*.cgi
