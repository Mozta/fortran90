PROGRAM LAGRANGE
	dimension f(0:10),x(0:10)
	data n /3/
	data (x(i),i=0,3) / 1., 2., 3., 4./
	data (f(i),i=0,3) / .671, .620, .567, .512 /
	print *
	print *, 'INTERPOLACION DE LAGRANGE'
	print *
	print *, 'TABLA DE VALORES UTILIZADOS'
	print *, '----------------------------------------------'
	print *, '	   i 	x(i)		f(i)	'
	do 37 i=0,n
		print *, i,x(i),f(i)
37	continue
	print *, '----------------------------------------------'
45	print *, 'Ingrese el valor de x'
	read *, xa
	if (xa.lt.x(0) .OR. xa.gt.x(n)) print *,'ADVERTENCIA: x esta en el rango de extrapolacion'
	yres = 0
	do i=0,n
		z=1.0
		do j=0,n
			if(i.ne.j) z=z*(xa-x(j))/(x(i)-x(j))
		end do
		yres=wyres+z*f(i)
	end do
	print 200,xa,yres
200 format(' Resultado de la interpolacion: g(',1PE12.5,') =',1PE12.5)
	print *
	print *, 'Oprima 1 para continuar o 0 para terminar'
	read *,k
	if (k.eq.1) goto 45
	print*

END PROGRAM LAGRANGE
