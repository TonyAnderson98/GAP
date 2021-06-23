program Gap
IMPLICIT none

    real :: start, finish


    character(len=30) :: arquivo
    real :: A, B, C, D, E, F, G, H, I, J
    INTEGER :: cod_aber
    CHARACTER(LEN=80) :: msg_erro



    REAL, DIMENSION(10) :: coluna
    REAL, DIMENSION(10) :: gap_minimo
    REAL, DIMENSION(10) :: gap_maximo



    
    
    INTEGER :: K = 0
    REAL :: energia_fermi   = -5.8514

    REAL :: gap_minimo_col_A = -50.0
    REAL :: gap_minimo_col_B = -50.0
    REAL :: gap_minimo_col_C = -50.0
    REAL :: gap_minimo_col_D = -50.0
    REAL :: gap_minimo_col_E = -50.0
    REAL :: gap_minimo_col_F = -50.0
    REAL :: gap_minimo_col_G = -50.0
    REAL :: gap_minimo_col_H = -50.0
    REAL :: gap_minimo_col_I = -50.0
    REAL :: gap_minimo_col_J = -50.0


    REAL :: gap_maximo_col_B =  50.0
    REAL :: gap_maximo_col_C =  50.0
    REAL :: gap_maximo_col_A =  50.0
    REAL :: gap_maximo_col_E =  50.0
    REAL :: gap_maximo_col_F =  50.0
    REAL :: gap_maximo_col_D =  50.0
    REAL :: gap_maximo_col_G =  50.0
    REAL :: gap_maximo_col_H =  50.0
    REAL :: gap_maximo_col_I =  50.0
    REAL :: gap_maximo_col_J =  50.0



    REAL :: gap_mais_baixo = 0.0
    REAL :: gap_mais_alto  = 0.0



    call cpu_time(start)
    arquivo = 'bands.bands' !read(*,*) arquivo


    OPEN(UNIT=10, FILE=arquivo, STATUS='old', IOSTAT=cod_aber, IOMSG=msg_erro)




    
        DO WHILE (K .lt. 11000)


            READ(10,*,iostat=cod_aber) A, B, C, D, E, F, G, H, I, J
                coluna(1)  = A
                coluna(2)  = B
                coluna(3)  = C
                coluna(4)  = D
                coluna(5)  = E
                coluna(6)  = F
                coluna(7)  = G
                coluna(8)  = H
                coluna(9)  = I
                coluna(10) = J



            IF (A .lt. energia_fermi .and. A .ge. gap_minimo_col_A) THEN
                gap_minimo_col_A = A
            END IF

            IF (B .lt. energia_fermi .and. B .ge. gap_minimo_col_B) THEN
                gap_minimo_col_B = B
            END IF

            IF (C .lt. energia_fermi .and. C .ge. gap_minimo_col_C) THEN
                gap_minimo_col_C = C
            END IF

            IF (D .lt. energia_fermi .and. D .ge. gap_minimo_col_D) THEN
                gap_minimo_col_D = D
            END IF
            
            IF (E .lt. energia_fermi .and. E .ge. gap_minimo_col_E) THEN
                gap_minimo_col_E = E
            END IF
            
            IF (F .lt. energia_fermi .and. F .ge. gap_minimo_col_F) THEN
                gap_minimo_col_F = F
            END IF

            IF (G .lt. energia_fermi .and. G .ge. gap_minimo_col_G) THEN
                gap_minimo_col_G = G
            END IF

            IF (H .lt. energia_fermi .and. H .ge. gap_minimo_col_H) THEN
                gap_minimo_col_H = H
            END IF

            IF (I .lt. energia_fermi .and. I .ge. gap_minimo_col_I) THEN
                gap_minimo_col_I = I
            END IF

            IF (J .lt. energia_fermi .and. J .ge. gap_minimo_col_J) THEN
                gap_minimo_col_J = J
            END IF


            K = K+1
        END DO
        
        gap_minimo(1)  = gap_minimo_col_A
        gap_minimo(2)  = gap_minimo_col_B
        gap_minimo(3)  = gap_minimo_col_C
        gap_minimo(4)  = gap_minimo_col_D
        gap_minimo(5)  = gap_minimo_col_E
        gap_minimo(6)  = gap_minimo_col_F
        gap_minimo(7)  = gap_minimo_col_G
        gap_minimo(8)  = gap_minimo_col_H
        gap_minimo(9)  = gap_minimo_col_I
        gap_minimo(10) = gap_minimo_col_J

        gap_mais_baixo = MAXVAL(gap_minimo)

        ! ---------------------------------
     
        k = 0
        DO WHILE (K .lt. 11000)

            READ(10,*,iostat=cod_aber) A, B, C, D, E, F, G, H, I, J
                coluna(1)  = A
                coluna(2)  = B
                coluna(3)  = C
                coluna(4)  = D
                coluna(5)  = E
                coluna(6)  = F
                coluna(7)  = G
                coluna(8)  = H
                coluna(9)  = I
                coluna(10) = J


            IF (A .gt. energia_fermi .and. A .le. gap_maximo_col_A) THEN
                gap_maximo_col_A = A
            END IF

            IF (B .gt. energia_fermi .and. B .le. gap_maximo_col_B) THEN
                gap_maximo_col_B = B
            END IF

            IF (C .gt. energia_fermi .and. C .le. gap_maximo_col_C) THEN
                gap_maximo_col_C = C
            END IF

            IF (D .gt. energia_fermi .and. D .le. gap_maximo_col_D) THEN
                gap_maximo_col_D = D
            END IF
            
            IF (E .gt. energia_fermi .and. E .le. gap_maximo_col_E) THEN
                gap_maximo_col_E = E
            END IF
            
            IF (F .gt. energia_fermi .and. F .le. gap_maximo_col_F) THEN
                gap_maximo_col_F = F
            END IF

            IF (G .gt. energia_fermi .and. G .le. gap_maximo_col_G) THEN
                gap_maximo_col_G = G
            END IF

            IF (H .gt. energia_fermi .and. H .le. gap_maximo_col_H) THEN
                gap_maximo_col_H = H
            END IF

            IF (I .gt. energia_fermi .and. I .le. gap_maximo_col_I) THEN
                gap_maximo_col_I = I
            END IF

            IF (J .gt. energia_fermi .and. J .le. gap_maximo_col_J) THEN
                gap_maximo_col_J = J
            END IF


            K = K+1
        END DO
        
        gap_maximo(1)  = gap_maximo_col_A
        gap_maximo(2)  = gap_maximo_col_B
        gap_maximo(3)  = gap_maximo_col_C
        gap_maximo(4)  = gap_maximo_col_D
        gap_maximo(5)  = gap_maximo_col_E
        gap_maximo(6)  = gap_maximo_col_F
        gap_maximo(7)  = gap_maximo_col_G
        gap_maximo(8)  = gap_maximo_col_H
        gap_maximo(9)  = gap_maximo_col_I
        gap_maximo(10) = gap_maximo_col_J

        gap_mais_alto = MINVAL(gap_maximo)
       






        ! ==========================================
        ! Exibindo o Gap Mínimo
        WRITE(*,*) "Energia Fermi: ", energia_fermi
        WRITE(*,*) "   Gap mínimo: ", gap_mais_baixo
        WRITE(*,*) "   Gap máximo: ", gap_mais_alto
        WRITE(*,*) "         Gap : ", gap_mais_alto-gap_mais_baixo








        close(10)


    call cpu_time(finish)
    print '("Time = ",f6.3," seconds.")',finish-start
end program Gap
