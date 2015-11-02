       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSULTAMOV.
       AUTHOR. J Tello y S Josa
	   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT ORDEREDFILE ASSIGN TO DISK
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS MKEY
           FILE STATUS IS FSOF.

	   DATA DIVISION.
	   FILE SECTION.

	       FD ORDEREDFILE
                   LABEL RECORDS ARE STANDARD
                   VALUE OF FILE-ID IS "ORDEREDMOV.DAT".
               01 MOVIMIENTOSRECOF.
                 02 MIDOF      PIC 9(9).
                 02 FECHORAOF  PIC 9999999999. *> AAMMDDHHMM
                 02 CUENTAMOVOF   PIC X(10).
                 02 CONCEPTOOF PIC X(40).
                 02 CANTIDADOF PIC S999V99.
                 02 DESTINOOF PIC X(10) VALUE "".
                 02 SALDOCUENTAOF    PIC 9999999V99.
                 
       WORKING-STORAGE SECTION.
           77 FSMF   PIC XX.
           77 TECLA  PIC X(01).
           77 ESCAP  PIC 99.
           77 CAMPO PIC 99 VALUE 0.
           77 DI PIC 99 VALUE 12.
           77 MI PIC 99.
           77 AI PIC 9999.
           77 DF PIC 99.
           77 MF PIC 99.
           77 AF PIC 9999.
           77 EI PIC 999.
           77 CI PIC 99.
           77 EF PIC 999.
           77 CFI PIC 99.
           01 MKEY   PIC 9999999 VALUE 1. 
           77 FSOF   PIC XX.
           77 FECHORAI  PIC 9999999999.
           77 FECHORAF  PIC 9999999999.
           77 CANTI     PIC 999V99.
           77 CANTF     PIC 999V99.
           77 ANOTMP PIC 99.
           77 MESTMP PIC 99999999.
           77 DIATMP PIC 999999.
           77 CTMP PIC 9V99.
           77 EOF    PIC X   VALUE "N".
           77 CUENTA PIC 99.
           77 NUMREG PIC 99 VALUE 10.
           77 CANABS PIC 999V99.
           77 NOVALE PIC 999 VALUE 0.
           77 APEARS PIC 99 VALUE 0.
           77 LUGAR  PIC 99.
           77 TODO PIC X(16).
           77 DIA    PIC XX.
           77 MES    PIC XX.
           77 ANO    PIC XX.
           77 HORA   PIC XX.
           77 MINUTOS PIC XX.
           77 TMP PIC 99999999.
           77 TMP2 PIC 999999.
           77 TMP3 PIC 9999.
           77 TMP4 PIC 99.
           77 SALDOFORMA PIC Z,ZZZ,ZZ9.99.
           77 CANTFORMA PIC ---9.99.
       LINKAGE SECTION.
           77 CUENTACT   PIC X(10).
           
       PROCEDURE DIVISION USING CUENTACT.
       
       MAIN-PROGRAM.
           CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Consulta de movimientos" LINE 5 COLUMN 29.
           DISPLAY "Se mostraran los últimos movimientos de "
                    "más a menos recientes."
                    LINE 7 COLUMN 11.
           DISPLAY "Alternativamente, indique un intervalo de fechas "
                   "y/o cantidades" LINE 8 COLUMN 11.
           DISPLAY "Entre las fechas __/__/____ y __/__/____"
                    LINE 11 COLUMN 18.
           DISPLAY "Cantidad entre ___.__E y ___.__E"
                    LINE 12 COLUMN 23.
           DISPLAY "Enter - Aceptar"
                LINE 24 COLUMN 10.
           DISPLAY "F1 - Cancelar"
                LINE 24 COLUMN 50.
           
       REPEAT2.   
           IF CAMPO = 0
             ACCEPT DI LINE 11 COLUMN 35
           ELSE
             IF CAMPO = 1
               ACCEPT MI LINE 11 COLUMN 38
             ELSE
               IF CAMPO = 2
                 ACCEPT AI LINE 11 COLUMN 41
               ELSE
                 IF CAMPO = 3
                   ACCEPT DF LINE 11 COLUMN 48
                 ELSE
                   IF CAMPO = 4
                     ACCEPT MF LINE 11 COLUMN 51
                   ELSE
                     IF CAMPO = 5
                       ACCEPT AF LINE 11 COLUMN 54
                     ELSE
                       IF CAMPO = 6
                         ACCEPT EI LINE 12 COLUMN 38
                       ELSE
                         IF CAMPO = 7
                           ACCEPT CI LINE 12 COLUMN 42
                         ELSE
                           IF CAMPO = 8
                             ACCEPT EF LINE 12 COLUMN 48
                           ELSE
                             IF CAMPO = 9
                               ACCEPT CFI LINE 12 COLUMN 52
                             ELSE
                               EXIT PROGRAM
                             END-IF
                           END-IF
                         END-IF
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF.
           
           ACCEPT ESCAP FROM ESCAPE KEY.
           IF ESCAP = 01
               EXIT PROGRAM
           ELSE
               IF ESCAP = 13
                   GO TO ORDENAR
               ELSE
                 IF ESCAP = 9
                   ADD 1 TO CAMPO GIVING CAMPO
                   COMPUTE CAMPO = FUNCTION MOD (CAMPO, 10)
                 END-IF
                 GO TO REPEAT2
               END-IF
           END-IF.  
            
       ORDENAR.
           CALL "ORDENARMOV"
           CANCEL "ORDENARMOV"

           *> Formateamos correctamente los params
           COMPUTE ANOTMP = FUNCTION MOD (AI, 100).
           MULTIPLY ANOTMP BY 100000000 GIVING FECHORAI.
           MULTIPLY MI BY 1000000 GIVING MESTMP.
           ADD MESTMP TO FECHORAI GIVING FECHORAI.
           MULTIPLY DI BY 10000 GIVING DIATMP.
           ADD DIATMP TO FECHORAI GIVING FECHORAI.

           COMPUTE ANOTMP = FUNCTION MOD (AF, 100).
           MULTIPLY ANOTMP BY 100000000 GIVING FECHORAF.
           MULTIPLY MF BY 1000000 GIVING MESTMP.
           ADD MESTMP TO FECHORAF GIVING FECHORAF.
           MULTIPLY DF BY 10000 GIVING DIATMP.
           ADD DIATMP TO FECHORAF GIVING FECHORAF.
           ADD FECHORAF TO 2359 GIVING FECHORAF.
           
           DIVIDE CI BY 100 GIVING CTMP.
           MOVE EI TO CANTI.
           ADD CANTI TO CTMP GIVING CANTI.
           
           DIVIDE CFI BY 100 GIVING CTMP.
           MOVE EF TO CANTF.
           ADD CANTF TO CTMP GIVING CANTF.

           IF FECHORAF = 2359
               MOVE 9999999999 TO FECHORAF.
           IF CANTF = 0
               MOVE 999.99 TO CANTF
           MOVE 0 TO CUENTA.
         AYLEER.
	   MOVE "N" TO EOF.
	   OPEN INPUT ORDEREDFILE.
	   CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Consulta de movimientos" LINE 5 COLUMN 29.
           DISPLAY "    Fecha           Concepto              "
                   "                Cantidad  Saldo cuenta" 
                   LINE 7 COLUMN 1.
	 LEER. 
               
	       IF CUENTA < NUMREG
	           
	           READ ORDEREDFILE NEXT
	                AT END MOVE "Y" TO EOF
	           END-READ
	           
	           IF CANTIDADOF < 0
	               MULTIPLY CANTIDADOF BY -1 GIVING CANABS
	           ELSE
	               MOVE CANTIDADOF TO CANABS
	           END-IF
	           IF CUENTAMOVOF <> CUENTACT OR FECHORAOF < FECHORAI 
	              OR FECHORAOF > FECHORAF
	              OR CANABS < CANTI OR CANABS > CANTF 
	              MOVE 0 TO APEARS
	           ELSE
	              MOVE 1 TO APEARS
	           END-IF
	           IF APEARS = 0 AND EOF NOT = "Y"
	               ADD 1 TO NOVALE GIVING NOVALE
	               GO TO LEER
	           ELSE
	               MOVE 0 TO APEARS
	           END-IF
	   
                   IF EOF NOT = "Y"
                       ADD CUENTA TO 8 GIVING LUGAR
 
                       ADD 1 TO CUENTA
                       
                       *> Formateamos fecha y hacemos displays
                       MOVE FECHORAOF TO ANO(1:2)
                       COMPUTE TMP = FUNCTION MOD (FECHORAOF, 100000000)
                       MOVE TMP TO MES(1:2)
                       COMPUTE TMP2 = FUNCTION MOD (FECHORAOF, 1000000)
                       MOVE TMP2 TO DIA(1:2)
                       COMPUTE TMP3 = FUNCTION MOD (FECHORAOF, 10000)
                       MOVE TMP3 TO HORA(1:2)
                       COMPUTE TMP4 = FUNCTION MOD (FECHORAOF, 100)
                       MOVE TMP4 TO MINUTOS
                       STRING DIA "/" MES "/" ANO " - " HORA ":" MINUTOS
                              DELIMITED BY SIZE INTO TODO
                       
                       DISPLAY TODO LINE LUGAR COLUMN 1
                       DISPLAY CONCEPTOOF LINE LUGAR COLUMN 19
                       MOVE CANTIDADOF TO CANTFORMA
                       DISPLAY CANTFORMA LINE LUGAR COLUMN 60
                       MOVE SALDOCUENTAOF TO SALDOFORMA

                       DISPLAY SALDOFORMA LINE LUGAR COLUMN 69
                       GO TO LEER
                   ELSE
                       GO TO REPEATX
                   END-IF
               ELSE
                    GO TO REPEATX
               END-IF.  
               
           REPEATX.
                    DISPLAY "F11 - Mov. anteriores"
                            LINE 24 COLUMN 1.
                    DISPLAY "F12 - Mov. siguientes"
                            LINE 24 COLUMN 31.
                    DISPLAY "F1 - Cancelar"
                            LINE 24 COLUMN 61.
                    IF CUENTA = 0
                        CLOSE ORDEREDFILE
                        EXIT PROGRAM.
                    *>ELSE 
                        *>MOVE 0 TO CUENTA
                    *>END-IF
                    
                    ACCEPT TECLA WITH AUTO PROMPT " ".
                    IF TECLA <> " "
                        GO TO REPEATX
                    END-IF
                    ACCEPT ESCAP FROM ESCAPE KEY.
                    IF ESCAP <> 91 AND ESCAP <> 92
                       AND ESCAP <> 01
                        GO TO REPEATX
                    ELSE
                        IF ESCAP = 91
                            ADD NUMREG TO CUENTA GIVING TMP
                            ADD NOVALE TO TMP GIVING TMP
                            GO TO RETROCEDE
                        ELSE
                            IF ESCAP = 01
                                CLOSE ORDEREDFILE
                                EXIT PROGRAM
                            END-IF
                            IF EOF = "N"
                                *> Muestra otra pagina
                                DISPLAY ERASE EOS LINE 8 COLUMN 1 
                                MOVE 0 TO CUENTA
                                GO TO LEER
                            ELSE
                                CLOSE ORDEREDFILE
                                EXIT PROGRAM
                            END-IF
                        END-IF
                    END-IF.
             
            RETROCEDE.   
                IF MKEY <= TMP
                    MOVE 1 TO TMP
                ELSE
                    *> Hay que restar 1 mas si fichero acabado
                    IF EOF = "Y"
                        ADD 1 TO TMP GIVING TMP
                    END-IF
                    SUBTRACT TMP FROM MKEY GIVING TMP
                END-IF.
                IF TMP <= 1
                    CLOSE ORDEREDFILE
                    OPEN INPUT ORDEREDFILE
                ELSE
                    MOVE TMP TO MKEY
                    START ORDEREDFILE
                        KEY = MKEY
                    END-START
                END-IF.
                
                MOVE 0 TO CUENTA.
                MOVE 0 TO TMP.
                MOVE 0 TO NOVALE.
                MOVE "N" TO EOF.      
                
                GO TO LEER.
	           
           
           
       END PROGRAM CONSULTAMOV.