 IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSULTASALDO.
	   
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
        77 FSCF   	PIC XX.
	77 EOF 		PIC X VALUE "N".
	77 TECLA        PIC X(01).
	77 ESCAP        PIC 99.
	77 SALDOFORMA   PIC Z,ZZZ,ZZ9.99.
       LINKAGE SECTION.
        01 DNIACT   PIC X(9).
       PROCEDURE DIVISION USING DNIACT.
       MAIN-PARA.
	   OPEN INPUT CLIENTESFILE.
	   MOVE DNIACT TO DNI.
	   START CLIENTESFILE
	        KEY = DNI
	   END-START
           READ CLIENTESFILE NEXT
                AT END MOVE "Y" TO EOF
           END-READ
	   DISPLAY ' ' ERASE LINE 1 COLUMN 1.
	   CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Consulta de saldo" 
	        LINE 5 COLUMN 30 UNDERLINE.
           DISPLAY "El saldo de tu cuenta " CUENTA " es: " 
                LINE 7 COLUMN 20.
           MOVE SALDO TO SALDOFORMA.
           DISPLAY SALDOFORMA "E" LINE 8 COLUMN 27.
           DISPLAY "Enter - Aceptar" 
                LINE 17 COLUMN 31.
           CLOSE CLIENTESFILE. 
           REPEATX.
                ACCEPT TECLA WITH AUTO PROMPT " ".
                ACCEPT ESCAP FROM ESCAPE KEY
                    IF ESCAP <> 13
                        GO TO REPEATX
                    END-IF          
	END PROGRAM CONSULTASALDO.