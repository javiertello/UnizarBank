IDENTIFICATION DIVISION.
       PROGRAM-ID. INGRESAREF.
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
           77 INGRESO   PIC 999V99.
           77 TOTALINGR PIC 99999V99.
           77 EOF       PIC X VALUE "N".
           77 MIDACT    PIC 9(9).
           77 FECHAACT  PIC 9(6).
           77 HORA      PIC 9999. 
           77 FECHORAACT  PIC 9999999999.
           77 HORAACT   PIC 9(8).
           77 SALDOFORMA   PIC Z,ZZZ,ZZ9.99.
           77 SALDOACT  PIC 9999999V99.
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
           MOVE SALDO TO SALDOACT.
       DO.
           DISPLAY ' ' ERASE LINE 1 COLUMN 1.
	   CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Ingresar efectivo" 
	        LINE 5 COLUMN 26 UNDERLINE.
	   MOVE SALDOACT TO SALDOFORMA.
           DISPLAY "Saldo actual: " SALDOFORMA "E"  
                LINE 7 COLUMN 20.
           DISPLAY "Por favor, introduzca los billetes" 
                LINE 9 COLUMN 20.
           DISPLAY "Cantidad introducida: ______E" 
                LINE 11 COLUMN 20.
           DISPLAY "F1 - Finalizar ingreso efectivo"
                LINE 19 COLUMN 20.
           ACCEPT INGRESO LINE 11 COLUMN 42.
           ACCEPT ESCAP FROM ESCAPE KEY.
           IF ESCAP = 01
               GO TO SALIDA 
           END-IF
           IF INGRESO <> 0
                ADD INGRESO TO TOTALINGR GIVING TOTALINGR
                ADD INGRESO TO SALDOACT GIVING SALDOACT
           END-IF
           GO TO DO.
             
        SALIDA.
           ADD TOTALINGR TO SALDO GIVING SALDO.
           REWRITE CLIENTESREC.
           CLOSE CLIENTESFILE.
           OPEN I-O MOVIMIENTOSFILE
           GO TO LEERMOV.
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
           MOVE "Ingreso" TO CONCEPTO.
           MOVE TOTALINGR TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE SALDO TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           CLOSE MOVIMIENTOSFILE.     
           DISPLAY ' ' ERASE LINE 1 COLUMN 1.
	   CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Ingresar efectivo" 
	        LINE 5 COLUMN 26 UNDERLINE.
	   MOVE TOTALINGR TO SALDOFORMA.
           DISPLAY "Se han ingresado correctamente: " SALDOFORMA "E" 
                LINE 7 COLUMN 13.
           MOVE SALDO TO SALDOFORMA.
           DISPLAY "El saldo resultante es de: " SALDOFORMA "E"  
                LINE 9 COLUMN 13.
           DISPLAY "Enter - Aceptar"
                LINE 18 COLUMN 25.
        REPEATX.
           ACCEPT TECLA WITH AUTO PROMPT " ".
                ACCEPT ESCAP FROM ESCAPE KEY
                    IF ESCAP <> 13
                        GO TO REPEATX
                    END-IF            
        
       END PROGRAM INGRESAREF.