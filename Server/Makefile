TARGETS = main.cgi

all : $(TARGETS)

primes : all Primes.cgi

CC = ghc

%.cgi : %.hs
	$(CC) $^ -o root/cgi-bin/$@

test:
	lighttpd -D -f testsvr.conf

clean:
	rm *.o *.hi *.cgi root/cgi-bin/*.cgi
