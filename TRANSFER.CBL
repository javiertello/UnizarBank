       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSFER.
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
               
       LINKAGE SECTION.
           77 CUENTACT PIC X(10).
           77 SALDOACT PIC 9999999V99. 
           77 DNIACT PIC X(9).
       WORKING-STORAGE SECTION.
           77 FSCF   PIC XX.
           77 FSMF   PIC XX.       
           77 CAMPO PIC 9 VALUE 0.
           77 CDEST PIC X(10).
           77 NOMTIT  PIC X(12).
           77 SALDOFORMA PIC Z,ZZZ,ZZZ.ZZ.
           77 EUROS PIC 999.
           77 CENTIMOS PIC 99.
           77 ESCAP  PIC 99.
           77 CANTOT PIC 999V99.
           77 CANTFOR PIC ZZ9.99.
           77 EOF    PIC X   VALUE "N".
           77 EOM    PIC X   VALUE "N".
           77 FECHORACT  PIC 9999999999. *> AAMMDDHHMM
           77 LASTID PIC 9(9).
           77 TECLA   PIC X(01).
           77 FECHA PIC 9(6).
           77 HORA PIC 9(8).
           77 HORAMIN PIC 9(4).
           77 NOMBACT  PIC X(12).
       PROCEDURE DIVISION USING CUENTACT, SALDOACT, DNIACT.
                      
       MAIN-PARA.
           CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Ordenar transferencia" LINE 5 COLUMN 27.
           MOVE SALDOACT TO SALDOFORMA
           DISPLAY "Saldo actual: "SALDOFORMA"E" LINE 7 COLUMN 26.
           DISPLAY "Indique la cuenta destino: " LINE 9 COLUMN 19.
           DISPLAY "y el nombre del titular: " LINE 10 COLUMN 18.
           
           DISPLAY "Indique la cantidad a transferir: ___.__E"
                   LINE 12 COLUMN 19.
           DISPLAY "Enter - Aceptar"
                LINE 24 COLUMN 10.
           DISPLAY "F1 - Cancelar"
                LINE 24 COLUMN 50.
       REPEAT.
           IF CAMPO = 0
             ACCEPT CDEST LINE 9 COLUMN 46
           ELSE
                 IF CAMPO = 1
                   ACCEPT NOMTIT LINE 10 COLUMN 43
                 ELSE
                   IF CAMPO = 2
                     ACCEPT EUROS LINE 12 COLUMN 53
                   ELSE
                     IF CAMPO = 3
                       ACCEPT CENTIMOS LINE 12 COLUMN 57
                     ELSE
                       EXIT PROGRAM
                     END-IF
                   END-IF
                 END-IF
           END-IF.
           
           ACCEPT ESCAP FROM ESCAPE KEY.
           IF ESCAP = 01
               EXIT PROGRAM
           ELSE
               IF ESCAP = 13
                   GO TO COMPROBAR
               ELSE
                 IF ESCAP = 9
                   ADD 1 TO CAMPO GIVING CAMPO
                   COMPUTE CAMPO = FUNCTION MOD (CAMPO, 4)
                 END-IF
                 GO TO REPEAT
               END-IF
           END-IF.
           
       COMPROBAR.
           DIVIDE CENTIMOS BY 100 GIVING CANTOT.
           ADD EUROS TO CANTOT GIVING CANTOT.
           IF CANTOT > SALDOACT
               DISPLAY "Indique una cantidad menor!!"
                      LINE 20 COLUMN 16
               MOVE 2 TO CAMPO
               GO TO REPEAT
           END-IF
           
       PANTALLA14.
           CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Ordenar transferencia" LINE 5 COLUMN 27.
           MOVE CANTOT TO CANTFOR.
           DISPLAY "Va a transferir "CANTFOR"E de su cuenta"
                  LINE 7 COLUMN 19.
           DISPLAY "a la cuenta "CDEST" cuyo titular es "NOMTIT
                  LINE 8 COLUMN 12.
           DISPLAY "Enter - Confirmar"
                LINE 24 COLUMN 10.
           DISPLAY "F1 - Cancelar"
                LINE 24 COLUMN 50.   
       REPEATX.
           ACCEPT TECLA WITH AUTO PROMPT " ".
           IF TECLA <> " "
               GO TO REPEATX
           ELSE
               ACCEPT ESCAP FROM ESCAPE KEY
               IF ESCAP <> 13 AND ESCAP <> 01
                   GO TO REPEATX
               ELSE
                  IF ESCAP = 01
                      EXIT PROGRAM
                  END-IF
               END-IF
           END-IF.
       PANTALLA15.
           
           OPEN I-O CLIENTESFILE.
           MOVE DNIACT TO DNI
           START CLIENTESFILE 
               KEY = DNI INVALID KEY GO TO ERRORES
           END-START
           READ CLIENTESFILE NEXT RECORD
               AT END MOVE "Y" TO EOF
	   END-READ
           SUBTRACT CANTOT FROM SALDOACT GIVING SALDO.
           MOVE NOMBAPE TO NOMBACT.
           REWRITE CLIENTESREC.
           CLOSE CLIENTESFILE.
           MOVE "N" TO EOF.
       ERRORES.
       
           OPEN I-O CLIENTESFILE.
       REPEAT3.
           READ CLIENTESFILE NEXT
	       AT END MOVE "Y" TO EOF
	   END-READ
           IF CUENTA = CDEST
               ADD CANTOT TO SALDO GIVING SALDO
               REWRITE CLIENTESREC
           ELSE
               IF EOF = "Y"
                   EXIT PROGRAM
               ELSE
                   GO TO REPEAT3
               END-IF
           END-IF
           CLOSE CLIENTESFILE.
           MOVE "N" TO EOF.
           *> Aniadimos 2 entradas a movimientos
           
           *> CALCULAR LASTID y FECHA 
           GO TO CALCULARCOSAS.
       CALCULADAS.
           OPEN I-O MOVIMIENTOSFILE.
           MOVE LASTID TO MID.
           MOVE FECHORACT TO FECHORA.
           MOVE CUENTACT TO CUENTAMOV.
           STRING "Transferencia a "NOMTIT" ("DNI")"
                 DELIMITED BY SIZE INTO CONCEPTO.
           MOVE CANTOT TO CANTIDAD.
           MULTIPLY CANTIDAD BY -1 GIVING CANTIDAD.
           MOVE CDEST TO DESTINO.
           SUBTRACT CANTOT FROM SALDOACT GIVING SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           ADD 1 TO LASTID GIVING LASTID.
           MOVE LASTID TO MID.
           MOVE FECHORACT TO FECHORA.
           MOVE CDEST TO CUENTAMOV.
           STRING "Transferencia recibida de "NOMBACT" ("DNIACT")"
                 DELIMITED BY SIZE INTO CONCEPTO.
           MOVE CANTOT TO CANTIDAD.
           MOVE "" TO DESTINO.
           ADD CANTOT TO SALDO GIVING SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           CLOSE MOVIMIENTOSFILE.
           MOVE "N" TO EOF.
           
           CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Ordenar transferencia" LINE 5 COLUMN 27.
           DISPLAY "Transferencia realizada correctamente!"
                   LINE 9 COLUMN 17.
           DISPLAY "Enter - Aceptar"
                LINE 20 COLUMN 29.
       REPEATB.
           ACCEPT TECLA WITH AUTO PROMPT " ".
           IF TECLA <> " "
               GO TO REPEATB
           ELSE
               ACCEPT ESCAP FROM ESCAPE KEY
               IF ESCAP <> 13
                   GO TO REPEAT
               ELSE
                   EXIT PROGRAM
               END-IF
           END-IF.
       CALCULARCOSAS.
           *> CALCULO FECHACT Y LASTID
           ACCEPT FECHA FROM DATE.
           ACCEPT HORA FROM TIME.
           MULTIPLY FECHA BY 10000 GIVING FECHORACT.
           DIVIDE HORA BY 10000 GIVING HORAMIN.
           ADD HORAMIN TO FECHORACT GIVING FECHORACT.
           
           OPEN I-O MOVIMIENTOSFILE.
       LEER.
           READ MOVIMIENTOSFILE NEXT
	      AT END MOVE "Y" TO EOM
	   END-READ
           IF EOM = "Y"
               MOVE MID TO LASTID
               ADD 1 TO LASTID GIVING LASTID
               MOVE "N" TO EOM
               CLOSE MOVIMIENTOSFILE
               GO TO CALCULADAS
           ELSE
               GO TO LEER
           END-IF.
           
       END PROGRAM TRANSFER.