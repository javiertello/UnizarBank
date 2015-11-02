IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDENARMOV.
	   
       AUTHOR. J Tello y S Josa
	   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT MOVIMIENTOSFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS MID
           FILE STATUS IS FSMF.
	   
	   SELECT ORDEREDMOV ASSIGN TO SORT.
	   
	   SELECT ORDEREDFILE ASSIGN TO DISK
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS MKEY
           FILE STATUS IS FSOF.
           
	   DATA DIVISION.
	   FILE SECTION.
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
               SD ORDEREDMOV.            
               01 MOVIMIENTOSRECO.
                 02 MIDO      PIC 9(9).
                 02 FECHORAO  PIC 9999999999. *> AAMMDDHHMM
                 02 CUENTAMOVO   PIC X(10).
                 02 CONCEPTOO PIC X(40).
                 02 CANTIDADO PIC S999V99.
                 02 DESTINOO  PIC X(10) VALUE "".  
                 02 SALDOCUENTAO    PIC 9999999V99.
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
       
       01 MKEY   PIC 9999999 VALUE 1. 
       77 FSMF  PIC XX.
       77 FSOF   PIC XX.
       

       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 1 TO MKEY.
       SORTING.
           
          SORT ORDEREDMOV
               ON DESCENDING FECHORA
            INPUT PROCEDURE CARGAR
            OUTPUT PROCEDURE SALIR
     
            EXIT PROGRAM.
      
       CARGAR SECTION.
            INI-CAR.
                OPEN INPUT MOVIMIENTOSFILE.
            LEER1.
                READ MOVIMIENTOSFILE NEXT RECORD 
                    AT END GO FIN-CAR
                END-READ.
                MOVE MID TO MIDO.
                MOVE FECHORA TO FECHORAO.
                MOVE CUENTAMOV TO CUENTAMOVO.
                MOVE CONCEPTO TO CONCEPTOO.
                MOVE CANTIDAD TO CANTIDADO.
                MOVE DESTINO TO DESTINOO.
                MOVE SALDOCUENTA TO SALDOCUENTAO.
                RELEASE MOVIMIENTOSRECO.
                GO TO LEER1.
            FIN-CAR.
                CLOSE MOVIMIENTOSFILE.
                EXIT.
                
            SALIR SECTION.
                OPEN OUTPUT ORDEREDFILE.
                GO TO COPIAF.
            COPIAF.
                RETURN ORDEREDMOV AT END GO FIN-COPIAF.
                MOVE MIDO TO MIDOF.
                MOVE FECHORAO TO FECHORAOF.
                MOVE CUENTAMOVO TO CUENTAMOVOF.
                MOVE CONCEPTOO TO CONCEPTOOF.
                MOVE CANTIDADO TO CANTIDADOF.
                MOVE DESTINOO TO DESTINOOF.
                MOVE SALDOCUENTAO TO SALDOCUENTAOF.
                WRITE MOVIMIENTOSRECOF.
                ADD 1 TO MKEY GIVING MKEY.
                GO TO COPIAF.
            FIN-COPIAF.
                CLOSE ORDEREDFILE.
                EXIT PROGRAM.
                        
       END PROGRAM ORDENARMOV.