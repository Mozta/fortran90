PROGRAM MAIN
	implicit none
	character(len=32) :: inarg
	integer num_args,ctr
	real*8 x0,x_iters(100),err,x_next,x_current,fresult,fprimaresult

	write(*,*) "Newton Rahpson en Fortran"

	num_args = command_argument_count()

	if (num_args .ne. 0) then
		!Obtenemos los argumentos de entrada
		call getarg(1,inarg)
		read(inarg,*) x0
	else
		write(*,*) 'Demasiados argumentos de entrada'
		stop
	endif

	write(*,*) "Estimacion inicial = ",x0
	x_iters(1) = x0
	call f(err,x0)

	ctr = 1
	do while(abs(err) .gt. 1e-5)
		write(*,*) x_iters(ctr)
		x_current = x_iters(ctr)
		call f(fresult,x_current)
		call fprima(fprimaresult,x_current)
		x_next = x_current - fresult/fprimaresult
		x_iters(ctr+1) = x_next
		ctr = ctr + 1
		call f(err,x_next)
	end do

END PROGRAM MAIN

subroutine f(errout,xin)
real*8 errout,xin
errout = xin**3-xin-1
end subroutine f

subroutine fprima(errout,xin)
real*8 errout,xin
errout =  3*(xin**2)-1
end subroutine fprima
