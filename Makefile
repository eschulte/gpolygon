.PHONY: clean

index.html: gpolygon.lisp
	wget http://localhost:4242/

clean:
	rm -f index.html
