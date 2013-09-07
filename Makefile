all:
	fbc splinefind.bas

check:
	./splinefind spline.bas > /dev/null
	fbc -x splinetest splinetest.bas spline.bas
	./splinetest

clean:
	rm -f spline splinefind splinetest *~
