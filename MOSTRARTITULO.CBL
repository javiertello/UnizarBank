 IDENTIFICATION DIVISION.
       PROGRAM-ID. MOSTRARTITULO.
	   
       AUTHOR. J Tello y S Josa
	   
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
               
                 
       WORKING-STORAGE SECTION.
       
       77 FECHAACT  PIC 9(6).
       77 HORAACT   PIC 9(8). 
       77 DIA    PIC XX.
       77 MES    PIC XX.
       77 ANO    PIC XX.
       77 HORA   PIC XX.
       77 MINUTOS PIC XX.
       77 TODO PIC X(16).
       77 TMP PIC 9(4).
       77 TMP2 PIC 9(2).
       77 TMP3 PIC 9(6).
       
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY ' ' ERASE LINE 1 COLUMN 1.
           DISPLAY "Cajero Automático UnizarBank" 
                    LINE 2 COLUMN 25.
                    
           ACCEPT FECHAACT FROM DATE.
           ACCEPT HORAACT FROM TIME.
            
           MOVE FECHAACT TO ANO(1:2).
           COMPUTE TMP = FUNCTION MOD (FECHAACT, 10000).
           MOVE TMP TO MES(1:2). 
           COMPUTE TMP2 = FUNCTION MOD (FECHAACT, 100).
           MOVE TMP2 TO DIA.
           
           
           MOVE HORAACT TO HORA(1:2).
           
           COMPUTE TMP3 = FUNCTION MOD (HORAACT, 1000000).
           MOVE TMP3 TO MINUTOS(1:2).
           
           STRING DIA "/" MES "/" ANO " - " HORA ":" MINUTOS
                 DELIMITED BY SIZE INTO TODO.
           
           DISPLAY TODO LINE 3 COLUMN 31.
           
       END PROGRAM MOSTRARTITULO.