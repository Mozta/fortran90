!---------------------------------------------------------------
!              PROGRAMA   * LAGRANGE *    VERSION 2      
!---------------------------------------------------------------
!                                                                     
!          OBTENCION DE LOS COEFICIENTES  DEL POLINOMIO
!                 DE INTERPOLACION  DE LAGRANGE.
!                                                                       
!                   M. FERNANDEZ, Julio 2003                           
!---------------------------------------------------------------
PROGRAM LAGRANGE2
!DECLARACION DE LAS VARIABLES:
	REAL*8 a(10),x(10),y(10),X0	!El maximo de datos se ha 							
!limitado a 10
	INTEGER*1 N
	CHARACTER*1 SIONO
1	WRITE(*,2)
2	FORMAT(15X,50('-'),/30X,'POLINOMIO DE LAGRANGE')
	CALL LINEAS(2)

!LECTURA DE LOS DATOS:
	WRITE(*,*)' INTRODUCIR EL NUMERO DE DATOS (HASTA 10)'
	READ(*,*)N
	DO I=1,N
		CALL LINEAS(1)
		WRITE(*,'(A31,I3)')'INTRODUCIR VALOR DE x PARA I= ',I
		READ(*,*)X(I)
		CALL LINEAS(1)
		WRITE(*,'(A30,I3)')' INTRODUCIR EL VALOR DE y=f(x)'
		READ(*,*)Y(I)
		CALL LINEAS(1)
	ENDDO
3	CALL LINEAS(20)
	CALL LINEAS(1)

!CALCULO DE LOS COEFICIENTES DEL POLINOMIO QUE PASA POR LOS DATOS:
!        (  P(x)= a(0) + a(1)*x + a(2)*x**2 + ···  )
	CALL COEPOL(X,Y,N,A)

!ESCRITURA DE LOS RESULTADOS EN EL FICHERO LAGRANGE.RES:
	OPEN(1,FILE='LAGRANGE.RES')
	WRITE(1,2)		!(El formato "2", definido antes,
				!puede volverse a usar) 
	WRITE(1,4)
4	FORMAT(13X,54('-'),/32X,'DATOS UTILIZADOS:',/&
				/32x,' x:            y:'/)
	DO I=1,N
		WRITE(1,5)X(i),Y(I)
	ENDDO
5	FORMAT(25X,F12.6,3X,F12.6)
	WRITE(1,6)'COEFICIENTES DEL POLINOMIO:'
6	FORMAT(13X,54('-'),//27X,A27/)
	DO I=1,N
		WRITE(1,'(29X,A2,I2,A6,F12.6)')'A(',I-1,') =  ',A(I)
	ENDDO
	WRITE(1,7)
7	FORMAT(13X,54('-'),//)
	CLOSE(1)

!Edición del fichero con los resultados:
	RESULT=RUNQQ('NOTEPAD.EXE','LAGRANGE.RES')
	STOP
END PROGRAM

!---------------------------------------------------------------
SUBROUTINE COEPOL(XA,YA,N,A)
!---------------------------------------------------------------
!   Calculo del valor de los coeficientes
!   del polinomio de interpolacion de Lagrange:
	PARAMETERCERO=1.0D-38
	REAL*8 XA(10),YA(10),A(10),X(10),Y(10),VALPOL
	INTEGER*1 N
	DO J=1,N
		X(J)=XA(J)
		Y(J)=YA(J)
	ENDDO
	DO J=1,N
		A(J)=VALPOL(X,Y,N+1-J,0.D0)
		XMIN=1.D0/CERO
		K=0
		DO I=1,N+1-J
			IF(DABS(X(I)).LT.XMIN)THEN
				XMIN=DABS(X(I))
				K=I
			ENDIF
			IF(DABS(X(I)).GT.CERO)THEN
				Y(I)=(Y(I)-A(J))/X(I)
			ELSE
				STOP 'DIVISION POR CERO EN COEPOL'
			ENDIF
		ENDDO
		IF(K.LT.N+1-J)THEN
			DO I=K+1,N+1-J
				Y(I-1)=Y(I)
				X(I-1)=X(I)
			ENDDO
		ENDIF
	ENDDO
	RETURN
	END SUBROUTINE

! --------------------------------------------------------------
REAL*8 FUNCTION VALPOL(X,Y,N,X0)
!---------------------------------------------------------------
!Estimacion del valor de una funcion en x=X0
!a partir de su valor en x=X(i).
! **** (Metodo de Lagrange) *****
	REAL*8 X(10),Y(10),X0,AUX
	INTEGER*1 N
	VALPOL=0.0D0
	DO I=1,N
		AUX=1.0D0
		DO J=1,N
			IF(J.NE.I)AUX=AUX*(X0-X(J))/(X(I)-X(J))
		ENDDO
		VALPOL=VALPOL+AUX*Y(I)
	ENDDO
	RETURN
END
! --------------------------
SUBROUTINE LINEAS(N)
! --------------------------
	DO I=1,N
		WRITE(*,1)
	ENDDO
1	FORMAT('  ')
	RETURN
END
