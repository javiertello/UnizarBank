IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDENARESP.
	   
       AUTHOR. J Tello y S Josa
	   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
	   SELECT ESPECTACULOSFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS NUM
           FILE STATUS IS FSEF.
	   
	   SELECT ORDEREDESP ASSIGN TO SORT.
	   
	   SELECT ORDEREDFILEESP ASSIGN TO DISK
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS EKEY
           FILE STATUS IS FSOFE.
           
	   DATA DIVISION.
	   FILE SECTION.
	       FD ESPECTACULOSFILE
                   LABEL RECORDS ARE STANDARD
                   VALUE OF FILE-ID IS "ESPECTACULOS.DAT".
               01 ESPECTACULOSREC.
                 02 NUM      PIC 9(3).
                 02 NOMBRE   PIC X(20). 
                 02 DESCRIPCION   PIC X(20).
                 02 FECHA PIC 9(6). *> AAMMDD
                 02 PRECIO PIC 999V99.
                 02 DISPONIBLES PIC 99999.
                 
               SD ORDEREDESP.            
               01 ESPECTACULOSRECO.
                 02 NUMO      PIC 9(3).
                 02 NOMBREO   PIC X(20). 
                 02 DESCRIPCIONO   PIC X(20).
                 02 FECHAO PIC 9(6). *> AAMMDD
                 02 PRECIOO PIC 999V99.
                 02 DISPONIBLESO PIC 99999.
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
               
                 
       WORKING-STORAGE SECTION.
       
       01 EKEY   PIC 9999999 VALUE 1. 
       77 FSEF   PIC XX.
       77 FSOFE   PIC XX.
       77 FECHACT PIC 9(6).

       PROCEDURE DIVISION.

       MAIN-PARA.
           MOVE 1 TO EKEY.
           ACCEPT FECHACT FROM DATE.
       SORTING.
           
          SORT ORDEREDESP
               ON ASCENDING FECHA
            INPUT PROCEDURE CARGAR
            OUTPUT PROCEDURE SALIR
     
            EXIT PROGRAM.
      
       CARGAR SECTION.
            INI-CAR.
                OPEN INPUT ESPECTACULOSFILE.
            LEER1.
                READ ESPECTACULOSFILE NEXT RECORD 
                    AT END GO FIN-CAR
                END-READ.
                IF FECHA < FECHACT
                    GO TO LEER1
                END-IF.
                MOVE NUM TO NUMO.
                MOVE NOMBRE TO NOMBREO.
                MOVE DESCRIPCION TO DESCRIPCIONO.
                MOVE FECHA TO FECHAO.
                MOVE PRECIO TO PRECIOO.
                MOVE DISPONIBLES TO DISPONIBLESO.
                RELEASE ESPECTACULOSRECO.
                GO TO LEER1.
            FIN-CAR.
                CLOSE ESPECTACULOSFILE.
                EXIT.
                
            SALIR SECTION.
                OPEN OUTPUT ORDEREDFILEESP.
                GO TO COPIAF.
            COPIAF.
                RETURN ORDEREDESP AT END GO FIN-COPIAF.
                MOVE NUMO TO NUMOF.
                MOVE NOMBREO TO NOMBREOF.
                MOVE DESCRIPCIONO TO DESCRIPCIONOF.
                MOVE FECHAO TO FECHAOF.
                MOVE PRECIOO TO PRECIOOF.
                MOVE DISPONIBLESO TO DISPONIBLESOF.
                WRITE ESPECTACULOSRECOF.
                ADD 1 TO EKEY GIVING EKEY.
                GO TO COPIAF.
            FIN-COPIAF.
                CLOSE ORDEREDFILEESP.
                EXIT PROGRAM.
                        
       END PROGRAM ORDENARESP.