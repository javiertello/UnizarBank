       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPRAR.
       AUTHOR. J Tello y S Josa
	   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT ORDEREDFILEESP ASSIGN TO DISK
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS EKEY
           FILE STATUS IS FSOFE.
           
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

	       FD ORDEREDFILEESP
                   LABEL RECORDS ARE STANDARD
                   VALUE OF FILE-ID IS "ORDEREDESP.DAT".
               01 ESPECTACULOSRECOF.
                 02 NUMOF     PIC 9(3).
                 02 NOMBREOF   PIC X(20). 
                 02 DESCRIPCIONOF   PIC X(20).
                 02 FECHAOF PIC 9(6). *> AAMMDD
                 02 PRECIOOF PIC 999V99.
                 02 DISPONIBLESOF PIC 99999.
                 
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
           77 FSMF   PIC XX.
           77 FSOFE PIC XX.
           77 FSEF PIC XX.
           77 FSCF PIC XX.
           77 TECLA  PIC X(01).
           77 ESCAP  PIC 99.
           77 CAMPO PIC 9 VALUE 0.
           
           01 EKEY   PIC 9999999 VALUE 1. 
           77 FECHAB PIC 9(6).
           77 CTMP PIC 9V99.
           77 EOF    PIC X   VALUE "N".
           77 CUENT PIC 99.
           77 NUMREG PIC 99 VALUE 10.
           77 APEARS PIC 99 VALUE 0.
           77 LUGAR  PIC 99.
           77 TODO PIC X(16).
           77 DIA    PIC XX.
           77 MES    PIC XX.
           77 ANO    PIC XX.
           77 TMP    PIC 9999.
           77 TMP2    PIC 99.
           77 SALDOFORMA PIC Z,ZZZ,ZZ9.99.
           77 PRECIOFORMA PIC ZZ9.99.
           77 CANTACT PIC 99.
           77 NUMACT PIC 9(3).
           77 CANTTOT PIC 9999999V99.
           77 LASTID PIC 9(9).
           77 EOM PIC X.
           77 HORA PIC 9(8).
           77 FECHORACT  PIC 9999999999. *> AAMMDDHHMM
           77 HORAMIN PIC 9(4).
           77 DISPFORMA PIC ZZZZ9.
           77 CANTFORMAT PIC Z,ZZZ,ZZ9.99.
       LINKAGE SECTION.
           77 CUENTACT   PIC X(10).
           77 SALDOACT PIC 9999999V99. 
           77 DNIACT PIC X(9).
       PROCEDURE DIVISION USING CUENTACT, SALDOACT, DNIACT.
            
       ORDENAR.
           CALL "ORDENARESP"
           CANCEL "ORDENARESP"

           MOVE 0 TO CUENT.
         AYLEER.
	   MOVE "N" TO EOF.
	   OPEN INPUT ORDEREDFILEESP.
	   CALL "MOSTRARTITULO".
           CANCEL "MOSTRARTITULO".
           DISPLAY "Compra de entradas de espectaculos" LINE 5 COLUMN 21.
           MOVE SALDOACT TO SALDOFORMA.
           DISPLAY "Saldo actual: "SALDOFORMA"E"
                    LINE 7 COLUMN 26.
           
           DISPLAY "Num   Fecha   Nombre              Descripcion"
                   "              Precio  Disponibles" 
                   LINE 9 COLUMN 1.
	 LEER. 
               
	       IF CUENT < NUMREG
	           
	           READ ORDEREDFILEESP NEXT
	                AT END MOVE "Y" TO EOF
	           END-READ
	   
                   IF EOF NOT = "Y"
                       ADD CUENT TO 10 GIVING LUGAR
 
                       ADD 1 TO CUENT
                       
                       *> Formateamos fecha y hacemos displays
                       
                       MOVE FECHAOF TO ANO(1:2)
                       COMPUTE TMP = FUNCTION MOD (FECHAOF, 10000)
                       MOVE TMP TO MES(1:2)
                       COMPUTE TMP2 = FUNCTION MOD (TMP, 100)
                       MOVE TMP2 TO DIA
                       STRING DIA "/" MES "/" ANO
                              DELIMITED BY SIZE INTO TODO
                       
                       DISPLAY TODO LINE LUGAR COLUMN 5
                       DISPLAY NUMOF LINE LUGAR COLUMN 1
                       DISPLAY NOMBREOF LINE LUGAR COLUMN 15
                       DISPLAY DESCRIPCIONOF LINE LUGAR COLUMN 35
                       MOVE PRECIOOF TO PRECIOFORMA
                       DISPLAY PRECIOFORMA LINE LUGAR COLUMN 60
                       MOVE DISPONIBLESOF TO DISPFORMA
                       DISPLAY DISPFORMA LINE LUGAR COLUMN 70

                       GO TO LEER
                   ELSE
                       GO TO REPEATX
                   END-IF
               ELSE
                    GO TO REPEATX
               END-IF.  
               
           REPEATX.
                    DISPLAY "Comprar __ entradas del espectaculo ___ "
                            "y pulsa enter para proceder"
                            LINE 21 COLUMN 10.
                    DISPLAY "F11 - Mov. anteriores"
                            LINE 24 COLUMN 1.
                    DISPLAY "F12 - Mov. siguientes"
                            LINE 24 COLUMN 31.
                    DISPLAY "F1 - Cancelar"
                            LINE 24 COLUMN 61.
                    IF CUENT = 0
                        CLOSE ORDEREDFILEESP
                        CLOSE ESPECTACULOSFILE
                        EXIT PROGRAM.
                    *>ELSE 
                        *>MOVE 0 TO CUENTA
                    *>END-IF
                    
            ACEPTAR.
                    IF CAMPO = 0
                        ACCEPT CANTACT LINE 21 COLUMN 18
                            ON EXCEPTION GO TO INTERRUPCION
                    ELSE
                        IF CAMPO = 1
                            ACCEPT NUMACT LINE 21 COLUMN 46
                            ON EXCEPTION GO TO INTERRUPCION
                        END-IF
                    END-IF.
                    
            INTERRUPCION.    
                    ACCEPT ESCAP FROM ESCAPE KEY.
                    IF ESCAP <> 91 AND ESCAP <> 92
                       AND ESCAP <> 01 AND ESCAP <> 9
                           AND ESCAP <> 13
                        GO TO ACEPTAR
                    ELSE
                        IF ESCAP = 91
                            ADD NUMREG TO CUENT GIVING TMP
                            GO TO RETROCEDE
                        ELSE
                            IF ESCAP = 01
                                CLOSE ORDEREDFILEESP
                                EXIT PROGRAM
                            ELSE
                                IF ESCAP = 9
                                 ADD 1 TO CAMPO GIVING CAMPO
                                 COMPUTE CAMPO = FUNCTION MOD (CAMPO, 2)
                                 GO TO ACEPTAR
                                ELSE
                                    IF ESCAP = 13
                                        CLOSE ORDEREDFILEESP
                                        GO TO VALIDAR
                                    END-IF
                                    IF EOF = "N"
                                      *> Muestra otra pagina
                                      DISPLAY ERASE EOS LINE 8 COLUMN 1 
                                      MOVE 0 TO CUENT
                                      GO TO LEER
                                    ELSE
                                      CLOSE ORDEREDFILEESP
                                      EXIT PROGRAM
                                    END-IF
                            END-IF
                            END-IF
                        END-IF
                    END-IF.
                
            RETROCEDE.   
                IF EKEY <= TMP
                    MOVE 1 TO TMP
                ELSE
                    *> Hay que restar 1 mas si fichero acabado
                    IF EOF = "Y"
                        ADD 1 TO TMP GIVING TMP
                    END-IF
                    SUBTRACT TMP FROM EKEY GIVING TMP
                END-IF.
                IF TMP <= 1
                    CLOSE ORDEREDFILEESP
                    OPEN INPUT ORDEREDFILEESP
                ELSE
                    MOVE TMP TO EKEY
                    START ORDEREDFILEESP
                        KEY = EKEY
                    END-START
                END-IF.
                
                MOVE 0 TO CUENT.
                MOVE 0 TO TMP.
                MOVE "N" TO EOF.      
                
                GO TO LEER.
	     
	     VALIDAR.     
                 OPEN I-O ESPECTACULOSFILE.
                 MOVE NUMACT TO NUM.
                 START ESPECTACULOSFILE
                    KEY = NUM
                    INVALID KEY GO TO ERR
                 END-START
                 READ ESPECTACULOSFILE NEXT
	                AT END MOVE "Y" TO EOF
	         END-READ
                 IF CANTACT < 1 OR CANTACT > DISPONIBLES
                     GO TO ERR
                 END-IF
                 
                 MULTIPLY CANTACT BY PRECIO GIVING CANTTOT.
                 
                 OPEN I-O CLIENTESFILE.
                 MOVE DNIACT TO DNI.
                 START CLIENTESFILE
                    KEY = DNI
                    INVALID KEY GO TO ERR
                 END-START
                 READ CLIENTESFILE NEXT
	                AT END MOVE "Y" TO EOF
	         END-READ
                 IF CANTTOT > SALDO
                     GO TO SALDOINSUF. 
                 *> Comprar entradas Clientes y espectaculosfile
                 *> abiertos
                 *> Todo OK, se pueden comprar
                 DISPLAY ERASE EOS LINE 7 COLUMN 1
                 DISPLAY "Vas a comprar "CANTACT" entradas"
                         LINE 9 COLUMN 25.
                 MOVE FECHA TO ANO(1:2)
                 COMPUTE TMP = FUNCTION MOD (FECHAOF, 10000)
                 MOVE TMP TO MES(1:2)
                 COMPUTE TMP2 = FUNCTION MOD (TMP, 100)
                 MOVE TMP2 TO DIA
                 STRING DIA "/" MES "/" ANO
                   DELIMITED BY SIZE INTO TODO
                 DISPLAY "del expectaculo "NOMBRE" del "TODO
                         LINE 10 COLUMN 15.
                 MOVE CANTTOT TO CANTFORMAT.
                 DISPLAY "Precio total: "CANTFORMAT"E"
                         LINE 11 COLUMN 25.
                         
                 DISPLAY "Enter - Aceptar"
                     LINE 24 COLUMN 10.
                 DISPLAY "F1 - Cancelar"
                     LINE 24 COLUMN 50.
             REPEATC.
                 ACCEPT TECLA WITH AUTO PROMPT " ".
                 IF TECLA <> " "
                   GO TO REPEATC
                 ELSE
                   ACCEPT ESCAP FROM ESCAPE KEY
                   IF ESCAP = 1
                     CLOSE CLIENTESFILE
                     CLOSE ESPECTACULOSFILE
                     EXIT PROGRAM
                   ELSE
                     IF ESCAP = 13
                       GO TO PROCESAR
                     ELSE
                       GO TO REPEATC
                     END-IF
                   END-IF
                 END-IF.
             PROCESAR.
                 SUBTRACT CANTTOT FROM SALDO GIVING SALDO.
                 REWRITE CLIENTESREC.
                 CLOSE CLIENTESFILE.
             
              CALCULARCOSAS.
                 *> CALCULO FECHACT Y LASTID
                 ACCEPT FECHAB FROM DATE.
                 ACCEPT HORA FROM TIME.
                 MULTIPLY FECHAB BY 10000 GIVING FECHORACT.
                 DIVIDE HORA BY 10000 GIVING HORAMIN.
                 ADD HORAMIN TO FECHORACT GIVING FECHORACT.
                 
                 OPEN INPUT MOVIMIENTOSFILE.
             LEERMOV.
                
                READ MOVIMIENTOSFILE NEXT
	           AT END MOVE "Y" TO EOM
	          END-READ
                  IF EOM = "Y"
                      MOVE MID TO LASTID
                      ADD 1 TO LASTID GIVING LASTID
                      MOVE "N" TO EOM
                      CLOSE MOVIMIENTOSFILE
                      GO TO GUARDARMOVIMIENTOS
                   ELSE
                       GO TO LEERMOV
                   END-IF.
              GUARDARMOVIMIENTOS.
                  OPEN I-O MOVIMIENTOSFILE.
                  MOVE LASTID TO MID.
                  MOVE FECHORACT TO FECHORA.
                  MOVE CUENTACT TO CUENTAMOV.
                  STRING "Compra de "CANTACT" entradas de "
                         NOMBRE
                        DELIMITED BY SIZE INTO CONCEPTO.
                  MOVE CANTTOT TO CANTIDAD.
                  MULTIPLY CANTIDAD BY -1 GIVING CANTIDAD.
                  MOVE "" TO DESTINO.
                  SUBTRACT CANTTOT FROM SALDOACT GIVING SALDOCUENTA.
                  MOVE SALDOCUENTA TO SALDOACT.
                  WRITE MOVIMIENTOSREC.  
                  CLOSE MOVIMIENTOSFILE.
                  
             ACTUALIZARESP.
                  SUBTRACT CANTACT FROM DISPONIBLES GIVING DISPONIBLES.
                  REWRITE ESPECTACULOSREC.
                  CLOSE ESPECTACULOSFILE.
                  *> ToDo OKEYY 
                 DISPLAY ERASE EOS LINE 7 COLUMN 1.
                 DISPLAY "Por favor, retire las entradas" 
                        LINE 9 COLUMN 18.
                 MOVE SALDOCUENTA TO SALDOFORMA.
                 DISPLAY "El saldo resultante es de:"SALDOFORMA"E"
                          LINE 11 COLUMN 15.
                 DISPLAY "Enter - Aceptar"
                     LINE 20 COLUMN 30.
             REPEATD.
                 ACCEPT TECLA WITH AUTO PROMPT " ".
                 IF TECLA <> " "
                   GO TO REPEATD
                 ELSE
                   ACCEPT ESCAP FROM ESCAPE KEY
                   IF ESCAP <> 13
                     GO TO REPEATD
                   END-IF
                 END-IF.
                 CLOSE ESPECTACULOSFILE.
                 CLOSE ORDEREDFILEESP.
                 EXIT PROGRAM.
                 *> FIN
                 
             ERR.
                 CLOSE CLIENTESFILE.
                 CLOSE ESPECTACULOSFILE.
                 DISPLAY ' ' ERASE LINE 1 COLUMN 1.
                 DISPLAY "Lo siento, no se pudo procesar su peticion"
                     LINE 12 COLUMN 15.
                 DISPLAY "Enter - Aceptar"
                     LINE 20 COLUMN 29.
             REPEATB.
                 ACCEPT TECLA WITH AUTO PROMPT " ".
                 IF TECLA <> " "
                   GO TO REPEATB
                 ELSE
                   ACCEPT ESCAP FROM ESCAPE KEY
                   IF ESCAP <> 13
                     GO TO REPEATB
                   END-IF
                 END-IF.
                 EXIT PROGRAM.
             
             
             
             SALDOINSUF.
                 CLOSE CLIENTESFILE.
                 CLOSE ESPECTACULOSFILE.
                 DISPLAY ERASE EOS LINE 6 COLUMN 1 
                 DISPLAY "Lo sentimos mucho, pero el "
                        "saldo es insuficiente"
                     LINE 12 COLUMN 15.
                 DISPLAY "Enter - Aceptar"
                     LINE 20 COLUMN 29.
             REPEATA.
                 ACCEPT TECLA WITH AUTO PROMPT " ".
                 IF TECLA <> " "
                   GO TO REPEATA
                 ELSE
                   ACCEPT ESCAP FROM ESCAPE KEY
                   IF ESCAP <> 13
                     GO TO REPEATA
                   END-IF
                 END-IF.
                 GO TO ORDENAR.
                 
             
       END PROGRAM COMPRAR.