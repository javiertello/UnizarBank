IDENTIFICATION DIVISION.
       PROGRAM-ID. CAMBIARCLAVE.
       AUTHOR. J Tello y S Josa
	   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT CLIENTESFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS DNI
           FILE STATUS IS FSCF.

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
                 
                 
       WORKING-STORAGE SECTION.
           77 FSCF   PIC XX.
           77 FSMF   PIC XX.
           77 FSEF   PIC XX.
           77 TECLA  PIC X(01).
           77 ESCAP  PIC 99.
           77 CLAVEA PIC X(10).
           77 CLAVEN PIC X(10).
           77 CLAVER PIC X(10).
           77 MSG PIC 9 VALUE 1.
           77 INTENTOS PIC 9 VALUE 3.
           77 CAMPO PIC 9 VALUE 0.
       LINKAGE SECTION.
  
           01 CLIENTESRECP.
           77 EXITO PIC X.
           
       PROCEDURE DIVISION USING CLIENTESRECP, EXITO.
       
       MOVE CLIENTESRECP TO CLIENTESREC.
       PANTALLA20.
           CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           
           DISPLAY "Cambio de clave personal"
                LINE 5 COLUMN 25.
           
           DISPLAY "Introduzca la clave actual: "
                LINE 11 COLUMN 25.
                
           DISPLAY "Introduzca la nueva clave: "
                LINE 12 COLUMN 25.
                
           DISPLAY "Repita la nueva clave: "
                LINE 13 COLUMN 25.
                
           DISPLAY "Enter - Confirmar"
                LINE 24 COLUMN 10.
           DISPLAY "F1 - Cancelar"
                LINE 24 COLUMN 50.
          

         REPEAT2.   
           IF CAMPO = 0
             ACCEPT CLAVEA SECURE LINE 11 COLUMN 53   
           ELSE
             IF CAMPO = 1
               ACCEPT CLAVEN SECURE LINE 12 COLUMN 52
             ELSE
               IF CAMPO = 2
                 ACCEPT CLAVER SECURE LINE 13 COLUMN 48
               END-IF
             END-IF
           END-IF.
           
           
           ACCEPT ESCAP FROM ESCAPE KEY.
           IF ESCAP = 01
               MOVE "Y" TO EXITO
               EXIT PROGRAM
           ELSE
               IF ESCAP = 13
                   GO TO VALIDAR
               ELSE
                 IF ESCAP = 9
                   ADD 1 TO CAMPO GIVING CAMPO
                   COMPUTE CAMPO = FUNCTION MOD (CAMPO, 3)
                 END-IF
                 GO TO REPEAT2
               END-IF
           END-IF.   
           
         VALIDAR.    
           IF CLAVEA <> CLAVE
               SUBTRACT 1 FROM INTENTOS GIVING INTENTOS
               IF INTENTOS <> 0
                   MOVE 2 TO MSG
                   GO TO PANTALLA22
               ELSE
                   MOVE 3 TO MSG
                   GO TO BLOQUEAR
               END-IF 
           ELSE
               IF CLAVEN <> CLAVER OR CLAVEN = ""
                   SUBTRACT 1 FROM INTENTOS GIVING INTENTOS
                   IF INTENTOS <> 0
                       MOVE 1 TO MSG
                       GO TO PANTALLA22
                   ELSE
                       MOVE 3 TO MSG
                       GO TO BLOQUEAR
                   END-IF  
               ELSE
                   *> Todo OK
                   GO TO ACTUALIZAR
               END-IF
           END-IF.
           
           
         BLOQUEAR.  
           OPEN I-O CLIENTESFILE.
           MOVE "Y" TO BLOQUEADA.
           REWRITE CLIENTESREC.
           CLOSE CLIENTESFILE.
           MOVE "N" TO EXITO.
           GO TO PANTALLA22.
           
         ACTUALIZAR.
           OPEN I-O CLIENTESFILE.
           MOVE CLAVEN TO CLAVE.
           REWRITE CLIENTESREC.
           CLOSE CLIENTESFILE.
           MOVE "Y" TO EXITO.
           MOVE CLIENTESREC TO CLIENTESRECP. 
           GO TO PANTALLA21.
           
         PANTALLA21.
           CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Cambio de clave personal"
                LINE 5 COLUMN 27.
           DISPLAY "La clave se ha actualizado correctamente!"
                LINE 8 COLUMN 18.
           DISPLAY "Enter - Aceptar"
                LINE 11 COLUMN 30.
         REPEAT3.    
           ACCEPT TECLA WITH AUTO PROMPT " ".
           ACCEPT ESCAP FROM ESCAPE KEY.
           IF ESCAP = 13
               EXIT PROGRAM
           ELSE
               GO TO REPEAT3
           END-IF.
              
         PANTALLA22.
           CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
            DISPLAY "Cambio de clave personal"
                LINE 5 COLUMN 25.
           DISPLAY "Enter - Aceptar"
                LINE 15 COLUMN 31.
           IF MSG = 1
	     DISPLAY "La clave actual no coincide o no es valida"
	         LINE 7 COLUMN 18
	     DISPLAY "Quedan "INTENTOS" intentos"
	         LINE 9 COLUMN 30   
           ELSE
             IF MSG = 2
               DISPLAY "La clave actual indicada no es correcta"
	         LINE 7 COLUMN 20
	       DISPLAY "Quedan "INTENTOS" intentos"
	         LINE 9 COLUMN 30  
             ELSE
               IF MSG = 3
                 DISPLAY "Tarjeta bloqueada por seguridad"
	         LINE 9 COLUMN 22
	         DISPLAY "acuda a una oficina"
	         LINE 11 COLUMN 28   
               ELSE
                 EXIT PROGRAM
               END-IF
             END-IF
           END-IF.
           
         REPEAT4.    
           ACCEPT TECLA WITH AUTO PROMPT " ".
           ACCEPT ESCAP FROM ESCAPE KEY.
           IF ESCAP = 13
             IF MSG= 3
               EXIT PROGRAM
             ELSE
               GO TO PANTALLA20
             END-IF
           ELSE
               GO TO REPEAT4
           END-IF.  

           
       END PROGRAM CAMBIARCLAVE.