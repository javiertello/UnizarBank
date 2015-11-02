IDENTIFICATION DIVISION.
       PROGRAM-ID. RETIRAREF.
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
                 
       WORKING-STORAGE SECTION.
           77 FSCF      PIC XX.
           77 FSMF      PIC XX.
           77 TECLA     PIC X(01).
           77 ESCAP     PIC 99.
           77 REINTEGRO PIC 99V99.
           77 REINTEGRO1 PIC 99.
           77 REINTEGRO2 PIC 99.
           77 CENTIMOS  PIC 9V99.
           77 EOF       PIC X VALUE "N".
           77 MIDACT    PIC 9(9).
           77 FECHAACT  PIC 9(6).
           77 HORA      PIC 9999. 
           77 FECHORAACT  PIC 9999999999.
           77 HORAACT   PIC 9(8).
           77 SALDOFORMA   PIC Z,ZZZ,ZZ9.99.
           77 CAMPO     PIC 9 VALUE 0.
           77 ERROR1    PIC 9 VALUE 0.
       LINKAGE SECTION.
           01 DNIACT    PIC X(9).
           
       PROCEDURE DIVISION USING DNIACT.
           MOVE DNIACT TO DNI.
           OPEN I-O CLIENTESFILE.
	   START CLIENTESFILE
	        KEY = DNI
	   END-START
           READ CLIENTESFILE NEXT
                AT END MOVE "Y" TO EOF
           END-READ
           MOVE SALDO TO SALDOFORMA.
       REPEAT.
           MOVE 0 TO CAMPO.
           DISPLAY ' ' ERASE LINE 1 COLUMN 1.
	   CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Retirar efectivo" 
	        LINE 5 COLUMN 31 UNDERLINE.
           DISPLAY "Saldo actual: " SALDOFORMA "E" 
                LINE 7 COLUMN 25.
           DISPLAY "Por favor, introduzca los billetes" 
                LINE 9 COLUMN 25.
           DISPLAY "Indique la cantidad a reintegrar: __.__E" 
                LINE 11 COLUMN 20.
           IF ERROR1 = 1
                DISPLAY "Indique una cantidad menor o distinto de cero!!"
                        LINE 13 COLUMN 20
           END-IF
           DISPLAY "Enter - Aceptar                        F1 - Cancelar" 
                LINE 19 COLUMN 15.
        REPEAT2.
           IF CAMPO = 0
                ACCEPT REINTEGRO1 LINE 11 COLUMN 54
           ELSE
                IF CAMPO = 1
                        ACCEPT REINTEGRO2 LINE 11 COLUMN 57
                END-IF
           END-IF
           ACCEPT ESCAP FROM ESCAPE KEY.
           IF ESCAP = 01
               EXIT PROGRAM
           ELSE
                IF ESCAP = 13
                     GO TO RETIRADA
                ELSE
                     IF ESCAP = 9 AND CAMPO = 0
                          ADD 1 TO CAMPO GIVING CAMPO
                          COMPUTE CAMPO = FUNCTION MOD (CAMPO, 10)
                          GO TO REPEAT2
                     ELSE
                        GO TO RETIRADA
                     END-IF  
                END-IF
           END-IF
        RETIRADA.
            ADD REINTEGRO1 TO REINTEGRO GIVING REINTEGRO.
            DIVIDE REINTEGRO2 BY 100 GIVING CENTIMOS.
            ADD CENTIMOS TO REINTEGRO GIVING REINTEGRO.
            IF REINTEGRO > SALDO OR REINTEGRO = 0.00
                MOVE 1 TO ERROR1
                GO TO REPEAT
            ELSE
                SUBTRACT REINTEGRO FROM SALDO GIVING SALDO
                REWRITE CLIENTESREC
                CLOSE CLIENTESFILE
                OPEN I-O MOVIMIENTOSFILE
                GO TO LEERMOV
            END-IF   
        LEERMOV.
           READ MOVIMIENTOSFILE NEXT RECORD 
	      AT END MOVE "Y" TO EOF
	   END-READ
	   IF EOF = "N"
	        MOVE MID TO MIDACT
	        GO TO LEERMOV
	   ELSE
	        GO TO SAL
	   END-IF
	SAL.
	   ADD 1 TO MIDACT GIVING MIDACT.
	   MOVE MIDACT TO MID.
	   *>CONSEGUIMOS FECHA
	   ACCEPT FECHAACT FROM DATE.
	   MULTIPLY FECHAACT BY 10000 GIVING FECHORAACT.
	   ACCEPT HORAACT FROM TIME.
	   DIVIDE HORAACT BY 10000 GIVING HORA.
           ADD HORA TO FECHORAACT GIVING FECHORAACT.
           MOVE FECHORAACT TO FECHORA.
           MOVE CUENTA TO CUENTAMOV.
           MOVE "Reintegro" TO CONCEPTO.
           MOVE REINTEGRO TO CANTIDAD.
           MULTIPLY CANTIDAD BY -1 GIVING CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE SALDO TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           CLOSE MOVIMIENTOSFILE.
        SALIDA.
           DISPLAY ' ' ERASE LINE 1 COLUMN 1.
	   CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Retirar efectivo" 
	        LINE 5 COLUMN 31 UNDERLINE.
           DISPLAY "Por favor retire los billetes y el ticket" 
                LINE 7 COLUMN 16.
           MOVE SALDO TO SALDOFORMA.
           DISPLAY "El saldo resultante es de " SALDOFORMA "E" 
                LINE 9 COLUMN 16.
           DISPLAY "Enter - Aceptar"
                LINE 18 COLUMN 30.
        REPEATX.
           ACCEPT TECLA WITH AUTO PROMPT " ".
                ACCEPT ESCAP FROM ESCAPE KEY
                    IF ESCAP <> 13
                        GO TO REPEATX
                    END-IF
       END PROGRAM RETIRAREF.