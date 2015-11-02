 IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROGRAM.
	   
       AUTHOR. J Tello y S Josa
	   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT CLIENTESFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS DNI
           FILE STATUS IS FSCF.
           
           SELECT MOVIMIENTOSFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS MID
           FILE STATUS IS FSMF.
           
           SELECT ESPECTACULOSFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS NUM
           FILE STATUS IS FSEF.
           
	   DATA DIVISION.
	   FILE SECTION.

	       FD CLIENTESFILE
                   LABEL RECORDS ARE STANDARD
                   VALUE OF FILE-ID IS "CLIENTES.DAT".
               01 CLIENTESREC.
                 02 DNI      PIC X(9).
                 02 NOMBAPE  PIC X(40).
                 02 TEL      PIC X(12).
                 02 DIR      PIC X(30).
                 02 CUENTA   PIC X(10).
                 02 SALDO    PIC 9999999V99.
                 02 BLOQUEADA PIC X(1). *> Y o N
                 02 CLAVE    PIC X(15).
                 
               FD MOVIMIENTOSFILE
                   LABEL RECORDS ARE STANDARD
                   VALUE OF FILE-ID IS "MOVIMIENTOS.DAT".
               01 MOVIMIENTOSREC.
                 02 MID      PIC 9(9).
                 02 FECHORA  PIC 9999999999. *> AAMMDDHHMM
                 02 CUENTAMOV   PIC X(10).
                 02 CONCEPTO PIC X(40).
                 02 CANTIDAD PIC S999V99.
                 02 DESTINO  PIC X(10) VALUE "".
                 02 SALDOCUENTA    PIC 9999999V99.
                 
               FD ESPECTACULOSFILE
                   LABEL RECORDS ARE STANDARD
                   VALUE OF FILE-ID IS "ESPECTACULOS.DAT".
               01 ESPECTACULOSREC.
                 02 NUM      PIC 9(3).
                 02 NOMBRE   PIC X(20). 
                 02 DESCRIPCION   PIC X(20).
                 02 FECHA PIC 9(6). *> Sin formato
                 02 PRECIO PIC 999V99.
                 02 DISPONIBLES PIC 99999.
               
                 
       WORKING-STORAGE SECTION.
       
       77 ESCAP PIC 99.
       77 FSCF   PIC XX.
       77 FSMF   PIC XX.
       77 FSEF   PIC XX.
       77 TECLA   PIC X(01).
       77 CUENTACT   PIC X(10).
       77 CLAVEACT   PIC X(15).
       77 EXITO PIC X.
       77 DNIACT    PIC X(9).
       77 SALDOACT PIC 9999999V99.
       77 EX PIC 9 VALUE 0.
       77 ULTIMO PIC 9999999.
       77 EOC    PIC X VALUE "N".
       77 INTENTOS PIC 9 VALUE 3.
       
       
       PROCEDURE DIVISION.
                      
       MAIN-PARA.
           CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           
           DISPLAY "Bienvenido a UnizarBank"
                LINE 5 COLUMN 28.
           DISPLAY "Por favor, introduzca tarjeta para operar"
                LINE 9 COLUMN 18.
                
           DISPLAY "Enter - Aceptar"
                LINE 20 COLUMN 29.
                
       REPEAT.
           ACCEPT TECLA WITH AUTO PROMPT " ".
           IF TECLA <> " "
               GO TO REPEAT
           ELSE
               ACCEPT ESCAP FROM ESCAPE KEY
               IF ESCAP <> 13
                   GO TO REPEAT
               END-IF
           END-IF.
           
       PANTALLA2.
           
           DISPLAY ' ' ERASE LINE 1 COLUMN 1.
           CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Numero de tarjeta: "
               LINE 9 COLUMN 20.
      
           DISPLAY "Clave: "
               LINE 11 COLUMN 32.
           
           ACCEPT CUENTACT LINE 9 COLUMN 40.
           ACCEPT CLAVEACT SECURE LINE 11 COLUMN 40.
           
           DISPLAY "Enter - Aceptar"
                LINE 24 COLUMN 10.
           DISPLAY "F1 - Cancelar"
                LINE 24 COLUMN 50.

         REPEAT2.    
           ACCEPT TECLA WITH AUTO PROMPT " ".
           ACCEPT ESCAP FROM ESCAPE KEY.
           IF ESCAP = 01
               GO TO MAIN-PARA
           ELSE
               IF ESCAP = 13
                   GO TO VALIDAR
               ELSE
                   GO TO REPEAT2
               END-IF
           END-IF.   
           
         VALIDAR.
             OPEN I-O CLIENTESFILE.
             GO TO LEERCLIENTES.
         
         LEERCLIENTES.
             READ CLIENTESFILE NEXT RECORD AT END MOVE "Y" TO EOC.
             IF CUENTA = CUENTACT AND EOC <> "Y"
                 IF BLOQUEADA = "Y"
                     CLOSE CLIENTESFILE
                     MOVE 3 TO INTENTOS
                     GO TO PANTALLA4
                 ELSE
                     IF CLAVE = CLAVEACT
                         *> LOGEO EXITOSO
                         MOVE SALDO TO SALDOACT
                         MOVE DNI TO DNIACT
                         CLOSE CLIENTESFILE
                         GO TO PANTALLA5
                     ELSE
                         SUBTRACT 1 FROM INTENTOS GIVING INTENTOS                  
                         IF INTENTOS = 0
                             MOVE "Y" TO BLOQUEADA
                             REWRITE CLIENTESREC
                             CLOSE CLIENTESFILE
                             GO TO PANTALLA4
                         ELSE
                             CLOSE CLIENTESFILE
                             GO TO PANTALLA3
                     END-IF
                 END-IF
                 
             ELSE
                 IF EOC <> "Y"
                     GO TO LEERCLIENTES
                 ELSE
                     MOVE "N" TO EOC
                     CLOSE CLIENTESFILE
                     GO TO PANTALLA35
                 END-IF
             END-IF
             
         PANTALLA3.
             CALL "MOSTRARTITULO".
             CANCEL "MOSTRARTITULO".
             DISPLAY "Clave incorrecta, quedan "INTENTOS" intentos"
                     LINE 6 COLUMN 20.
             DISPLAY "Enter - Aceptar"
                LINE 24 COLUMN 10.
             DISPLAY "F1 - Cancelar"
                LINE 24 COLUMN 50.

         REPEAT4.    
             ACCEPT TECLA WITH AUTO PROMPT " ".
             ACCEPT ESCAP FROM ESCAPE KEY.
             IF ESCAP = 01
                 GO TO MAIN-PARA
             ELSE
                 IF ESCAP = 13
                     GO TO PANTALLA2
                 ELSE
                     GO TO REPEAT4
                 END-IF
             END-IF.  
         PANTALLA35.
             CALL "MOSTRARTITULO".
             CANCEL "MOSTRARTITULO".
             DISPLAY "Tarjeta incorrecta" LINE 6 COLUMN 25.
             DISPLAY "Enter - Aceptar"
                LINE 24 COLUMN 10.
             DISPLAY "F1 - Cancelar"
                LINE 24 COLUMN 50.

         REPEAT3.    
             ACCEPT TECLA WITH AUTO PROMPT " ".
             ACCEPT ESCAP FROM ESCAPE KEY.
             IF ESCAP = 01
                 GO TO MAIN-PARA
             ELSE
                 IF ESCAP = 13
                     GO TO PANTALLA2
                 ELSE
                     GO TO REPEAT3
                 END-IF
             END-IF.   
         
         PANTALLA4. 
             CALL "MOSTRARTITULO".
             CANCEL "MOSTRARTITULO".
             DISPLAY "Se ha sobrepasado el número de intentos" LINE 5 COLUMN 20.
             DISPLAY "Tarjeta bloqueada por seguridad" LINE 7 COLUMN 25.
             DISPLAY "acuda a una oficina" LINE 9 COLUMN 30.
             DISPLAY "Enter - Aceptar"
                LINE 16 COLUMN 32.
                
         REPEAT5.
             ACCEPT TECLA WITH AUTO PROMPT " ".
             IF TECLA <> " "
                 GO TO REPEAT5
             ELSE
                 ACCEPT ESCAP FROM ESCAPE KEY
                 IF ESCAP <> 13
                     GO TO REPEAT5
                 END-IF
             END-IF.
             GO TO MAIN-PARA.
             
         PANTALLA5.
             CALL "MOSTRARTITULO".
             CANCEL "MOSTRARTITULO".
             DISPLAY "1 - Consultar saldo" LINE 8 COLUMN 27.
             DISPLAY "2 - Consultar movimientos" LINE 9 COLUMN 27.
             DISPLAY "3 - Retirar efectivo" LINE 10 COLUMN 27.
             DISPLAY "4 - Ingresar efectivo" LINE 11 COLUMN 27.
             DISPLAY "5 - Ordenar transferencia" LINE 12 COLUMN 27.
             DISPLAY "6 - Comprar entradas espectaculos" LINE 13 COLUMN 27.
             DISPLAY "7 - Cambiar clave" LINE 15 COLUMN 27.
             DISPLAY "F1 - Cancelar"
                LINE 17 COLUMN 27.
                
         REPEAT6.
             ACCEPT TECLA WITH AUTO PROMPT " ".
             IF TECLA = " "
                 ACCEPT ESCAP FROM ESCAPE KEY
                 IF ESCAP <> 01
                     GO TO REPEAT6
                 ELSE
                     GO TO MAIN-PARA     
                 END-IF
             ELSE
                 IF TECLA = 1
                     CALL "CONSULTASALDO" USING DNIACT
                     CANCEL "CONSULTASALDO"
                 ELSE
                     IF TECLA = 2
                         CALL "CONSULTAMOV" USING CUENTACT
                         CANCEL "CONSULTAMOV"
                     ELSE
                         IF TECLA = 3
                             CALL "RETIRAREF"
                                 USING DNIACT
                             CANCEL "RETIRAREF"
                         ELSE
                             IF TECLA = 4
                                 CALL "INGRESAREF"
                                     USING DNIACT
                                 CANCEL "INGRESAREF"
                             ELSE
                                 IF TECLA = 5
                                     CALL "TRANSFER"
                                         USING CUENTACT, SALDOACT,
                                               DNIACT
                                     CANCEL "TRANSFER"
                                 ELSE
                                     IF TECLA = 6
                                         CALL "COMPRAR"
                                         USING CUENTACT, SALDOACT,
                                               DNIACT
                                         CANCEL "COMPRAR"
                                     ELSE
                                         IF TECLA = 7
                                             CALL "CAMBIARCLAVE"
                                             USING CLIENTESREC, EXITO
                                              
                                             IF EXITO = "N"
                                               CANCEL "CAMBIARCLAVE"
                                               GO TO PANTALLA4
                                             END-IF
                                             MOVE CLAVE TO CLAVEACT
                                             CANCEL "CAMBIARCLAVE"
                                         ELSE
                                             GO TO REPEAT6
                                                                
             END-IF.
             GO TO PANTALLA5.
             
       END PROGRAM MAINPROGRAM.