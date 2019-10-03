build: test
	sbcl --no-userinit --no-sysinit --non-interactive \
			 --load ~/.quicklisp/setup.lisp \
			 --eval "(ql:quickload :my-portfolio)" \
			 --eval "(asdf:make :my-portfolio)" \

test:
	sbcl --no-userinit --no-sysinit --non-interactive \
			 --load ~/.quicklisp/setup.lisp \
			 --eval "(ql:quickload :my-portfolio)" \
			 --eval "(asdf:test-system :my-portfolio)" \

clean:
	echo "Not yet implemented"

