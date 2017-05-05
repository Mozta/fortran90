PROGRAM LAGRANGE
	implicit real*8 (a-h,o-z)
	open(unit=15,file='table.txt',status='old')
	open(unit=20,file='Lpoly.dat',status='unknown')
	
	call readinput
	
	call printpolys
	
	call interpol
	
	stop
	end	

	subroutine readinput
	implicit real*8 (a-h,o-z)
	

	parameter (mxpts=100)
	common/bkinput/x(mxpts),y(mxpts),npts
	
	npts = 5

	do 100 i=1,npts
	  read(15,*,end=2000) x(i),y(i)
100	continue

2000	return
	end

	subroutine printpolys
	implicit real*8 (a-h,o-z)


	parameter (mxpts=100)
	common/bkinput/x(mxpts),y(mxpts),npts	
	
	nptsplot=300
	rdelta = (x(npts)-x(1))/nptsplot
	r = x(1)
	do 100 i=1,nptsplot
	 write(20,*)  r,( rLpoly(npts,k,x,r), k=1,npts )
	 r = r + rdelta
100	continue

	return
	end

	subroutine interpol
	implicit real*8 (a-h,o-z)

	parameter (mxpts=100)
	common/bkinput/x(mxpts),y(mxpts),npts	
	data rzero/0.0d0/
	
100	print*,'give the r point for interpolation (le.x(1) for quit):'
	read*,r
	if (r.lt.x(1)) return

	result = rzero
	do 200 i=1,npts
	   result = result + y(i)*rLpoly(npts,i,x,r)
200	continue

	print*,' the interpolated value at r=:',r,' is =:',result
	
	go to 100
	
	return
	end

	double precision function rLpoly(n,k,x,r)
	implicit real*8 (a-h,o-z)


	dimension x(n)
	data one/1.0d0/
	
	rLpoly = one
	do 100 i=1,n	
	  if (i.eq.k) go to 100
	  rLpoly = rLpoly*(r-x(i))/(x(k) - x(i))
100	continue
	
	return
	end	
