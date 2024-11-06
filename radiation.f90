PROGRAM radiation

!NON SONO SICURA CI VOGLIANO
USE APRI_F
USE INTERPOLATION

!dichiarazione delle variabili
IMPLICIT NONE
INTEGER :: stato, dealloc_error, ind1, ind2, step
INTEGER :: i,j, nrows=0,ncols=7, c, buco, n
REAL, ALLOCATABLE, DIMENSION (:,:) :: dati
INTEGER, ALLOCATABLE, DIMENSION (:) :: missing_data_index
REAL :: sw_radiation, lwl_radiation, a,b, x1, x2, y1, y2, rad, meanx, meany

CALL APRI_FILE(33, 'OLD', 'READ')
!CALL APRI_FILE(34, 'NEW', 'WRITE')

WRITE(*,*)"Crea un file temporaneo:"
CALL APRI_FILE(35, 'SCRATCH', 'READWRITE')

!lettura della prima riga del file per saltarla
READ(33,'(A)')

!trovo la fine del file
length : DO
  READ(33,*, IOSTAT=stato)
  IF (stato==0) THEN
       nrows=nrows+1
    ELSE IF(stato>0) THEN
       WRITE (*,*) "Errore di lettura: IOSTAT=", stato
       EXIT length !uscita dal ciclo DO
    ELSE IF (stato<0) THEN
       WRITE (*,*) "Ho trovato la fine del file ingresso, IOSTAT=", stato
       WRITE (*,*) "Il numero di righe vale = ", nrows       
       EXIT length !uscita dal ciclo DO
    END IF
END DO length

!allocare la memoria
ALLOCATE(dati(nrows,ncols), STAT=stato)
IF(stato /= 0) THEN
  WRITE(*,*) "Non è possibile allocare spazio"
  STOP
ELSE 
  WRITE(*,*) "Allocazione avvenuta correttamente"
END IF

REWIND(33)
READ(33,*)

!ciclo DO: inserire tutti i valori numerici in una matrice
lettura: DO i=1,nrows
  READ(33,*)(dati(i,j),j=1,ncols) !lettura per tutta la riga
END DO lettura

!calcolare la sw e lw radiation
sw_radiation = 0.
lwl_radiation = 0.


c=0
DO i=1,nrows
 ! WRITE(*,*) dati(i,7)
  rad = dati(i,7)
  IF(rad >= 0.) THEN
    sw_radiation = sw_radiation + rad
    !trasformare in MJ/m^2
  ELSE IF (rad < 0. .AND. rad > -997) THEN
    lwl_radiation = lwl_radiation + rad
  ELSE
    WRITE(*,*) "Dato mancante alla riga", i
    WRITE(35,*) i !scrivo su un file gli indici dei buchi
    c=c+1  !contatore  
  END IF
END DO 

WRITE(*,*) 'Il numero di dati mancanti è = ', c

REWIND(35)

ALLOCATE(missing_data_index(c))
DO i=1,c
!trasferisco gli indici dei buchi su un vettore
  READ(35,*) missing_data_index(i) 
END DO

buco = 1
i = 0

!controllo buco e interpolazione
esterno : DO  

  step = buco
  i = i + step
  
  IF(i>=c) THEN
    !WRITE(*,*) "Lunghezza del vettore terminata."
    EXIT esterno
  END IF
  
  ind1 = missing_data_index(i) !indice del buco corrente
  
  IF(ind1 == 1) THEN 
    WRITE(*,*) "Primo dato mancante: indice=", ind1, " Programma terminato"
    EXIT esterno
  ELSE IF (ind1 == nrows) THEN
    WRITE(*,*) "Ultimo dato mancante: indice=", ind1, " Programma terminato"
    EXIT esterno
  END IF
  
  !non calcola la lunghezza del buco se è l'ultimo dato mancante
  IF (i == c) THEN
  
    y1 = dati(ind1-1,7)
    y2 = dati(ind1+1,7)
    x1 = dati(ind1-1,5)
    x2 = dati(ind1+1,5)
    
    CALL INTERPOL(y1,y2,x1,x2,a,b)
    
    rad=a+b*dati(ind1,5)
    IF(rad>=0.) THEN
      sw_radiation=sw_radiation + rad!i valori interpolati
    ELSE
      lwl_radiation=lwl_radiation + rad!val interpol
    END IF
    EXIT
    
  END IF
  
  buco = 1
  interno : DO j=i+buco,c
    ind2 = missing_data_index(j)
    IF((ind2-ind1) == buco) THEN
      buco = buco + 1
    ELSE    
      EXIT interno
    END IF
  END DO interno
    
  IF (buco <= 4) THEN 
    
    !INTERPOLATION 
    y1 = dati(ind1-1,7)
    y2 = dati(ind1+buco,7)
    x1 = dati(ind1-1,5)
    x2 = dati(ind1+buco,5)
     
     
    CALL INTERPOL(y1,y2,x1,x2,a,b)

      
    DO n=0,buco-1
      rad = a + b*dati(ind1+n,5)
      WRITE(*,*) rad, a, b
      IF (rad <= 0.) THEN 
        lwl_radiation = lwl_radiation + rad
      ELSE 
        sw_radiation = sw_radiation + rad
      END IF
    END DO
     
  ELSE 
    WRITE(*,*) "Valori non interpolabili, PIÙ DI 4 DATI MANCANTI"
    STOP 
  END IF
  
END DO esterno

sw_radiation = sw_radiation*60/10E6
lwl_radiation = lwl_radiation*60/10E6



WRITE(*,*) "Short wave radiation:", sw_radiation
WRITE(*,*) "Long wave loss radiation:", lwl_radiation
  

DEALLOCATE(dati, STAT=dealloc_error)
IF(dealloc_error /=0) THEN
  ERROR STOP "Errore di deallocazione"
ELSE
  WRITE(*,*) "Deallocazione avvenuta correttamente"  
END IF

CLOSE(33)
!CLOSE(34)
CLOSE(35)

END PROGRAM radiation
