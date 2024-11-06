MODULE apri_f
  CONTAINS
  
SUBROUTINE APRI_FILE(nu, sta, act)

  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: sta, act
  INTEGER, INTENT(IN) :: nu
  INTEGER :: io_stat
  LOGICAL :: ex
  CHARACTER(len=40) :: etichetta, filename
  
  !apertura del file di ingresso, controllo dell'esistenza
  WRITE(*,*) "Inserire il nome del file ", sta, " ", act, " con estensioni"
  READ(*,'(A)') etichetta
  filename=TRIM(etichetta)
  
  SELECT CASE(sta)
  CASE('OLD')
    !controllo che il file esista e si apra correttamente
    INQUIRE(FILE=filename, EXIST=ex)
    
    IF(ex) THEN
      WRITE(*,'(A,A,A)') "Il file ", filename," esiste "
      OPEN(UNIT=nu, FILE=filename, STATUS=sta, ACTION=act, IOSTAT=io_stat)
    ELSE
      WRITE(*,'(A,A,A)') "Il file dati ", filename, " non esiste"
      ERROR STOP 
    END IF

    IF (io_stat/=0) THEN
        WRITE(*,*) "ERRORE nell'apertura del file: ", filename, " IOSTAT =", io_stat
        ERROR STOP
    END IF
    
  CASE('NEW')
    OPEN(UNIT=nu, FILE=filename, STATUS=sta, ACTION=act, IOSTAT=io_stat)
    INQUIRE(UNIT=nu, EXIST=ex)
    IF(ex) THEN
      WRITE(*,'(A,1X,A,1X,A,1X,I2)')"Il file ",filename," è stato creato correttamente, IOSTAT= ", io_stat
    ELSE
      WRITE(*,*) "Il file ", filename," non esiste"
      ERROR STOP 
    END IF
      
  CASE('SCRATCH')
    !file temporaneo 
    OPEN(UNIT=nu, FILE=filename, STATUS=sta, ACTION=act, IOSTAT=io_stat)
    INQUIRE(UNIT=nu, EXIST=ex)
    IF (ex) THEN
      WRITE(*,*) "Il file ", filename, " è stato creato correttamente, IOSTAT=", io_stat
    ELSE
      WRITE(*,*) "Il file ", filename," non esiste"
      ERROR STOP 
    END IF
    
  END SELECT
  
RETURN
END SUBROUTINE

END MODULE apri_f
        
      
      
      
      
      
      
      
      
