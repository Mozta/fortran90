program matrix_inverse
  implicit real (a-h,o-z)
  real :: c(2,2),d(2,2)
  n = 2

  print *, 'MATRIZ INVERSA USANDO GAUSS-JORDAN'
  print *

! Toma de valores para la matriz a tratar
  do i = 1,n
    do j = 1,n
      c(i,j) = 0.0
      print *, 'Ingrese el valor de :', i, j
      read *, c(i,j)
    end do
  end do

! Llenamos una matriz de 0's
  do i = 1,n
    do j = 1,n
      d(i,j) = 0.0
    end do
  end do

! Mostramos la matriz original
  print *, '               MATRIZ ORIGINAL'
  print *, '----------------------------------------------'
  do i=1,n
    do j = 1,n
      print *, '(',i,',',j,')',' =',c(i,j)
    end do
  end do

! Llamamos a la subrutina matrixinv pasandole las matrices y la dimension
  call matrixinv(c,d,n)

! Mostramos la matriz de identidad
  print *, '               MATRIZ IDENTIDAD'
  print *, '----------------------------------------------'
  do i=1,n
    do j = 1,n
      print *, '(',i,',',j,')',' =',c(i,j)
    end do
  end do

! Mostramos la matriz inversa resultante
  print *, '               MATRIZ INVERSA'
  print *, '----------------------------------------------'
  do i=1,n
    do j = 1,n
      print *, '(',i,',',j,')',' =',d(i,j)
    end do
  end do

end

! Subrutina para calcular la matriz inversa usando el metodo de Gauss Jordan
subroutine matrixinv(a,b,n)
integer :: i,j,k,l,m,n,irow
real:: big,a(n,n),b(n,n),dum

! Generamos la matriz de identidad
do i = 1,n
  do j = 1,n
    b(i,j) = 0.0
  end do
  b(i,i) = 1.0
end do

! Recorremos todas las columnas de la matriz, si a(i,i) es 0 buscamos otro mejor pivote
! El pivote se elige tomando el mayor valor de la columna i de a(j,i) con j=1,n
 do i = 1,n
  big = a(i,i)
    do j = i,n
      if (a(j,i).gt.big) then
        big = a(j,i)
        irow = j
      end if
    end do

! Intercambiamos las lineas i con irow para ambas matrices
    if (big.gt.a(i,i)) then
      do k = 1,n
        dum = a(i,k) ! matrix a()
        a(i,k) = a(irow,k)
        a(irow,k) = dum
        dum = b(i,k) ! matrix b()
        b(i,k) = b(irow,k)
        b(irow,k) = dum
      end do
    end if

! Dividimos todas las entradas en la linea i desde a(i,j) por el valor de a(i,i) y para b
    dum = a(i,i)
    do j = 1,n
      a(i,j) = a(i,j)/dum
      b(i,j) = b(i,j)/dum
    end do

! Hacemos 0's las entradas en la columna de las matrices
    do j = i+1,n
      dum = a(j,i)
      do k = 1,n
        a(j,k) = a(j,k) - dum*a(i,k)
        b(j,k) = b(j,k) - dum*b(i,k)
      end do
    end do
end do

! Restamos el múltiplo apropiado de la fila j con la fila j-1
do i = 1,n-1
  do j = i+1,n
    dum = a(i,j)
    do l = 1,n
      a(i,l) = a(i,l)-dum*a(j,l)
      b(i,l) = b(i,l)-dum*b(j,l)
    end do
  end do
end do

end
