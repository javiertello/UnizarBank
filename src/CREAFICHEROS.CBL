       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREAFICHEROS.
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
           
           SELECT ESPECTACULOSFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS NUM
           FILE STATUS IS FSEF.
           
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
           77 FSCF   PIC XX.
           77 FSMF   PIC XX.
           77 FSEF   PIC XX.
       
       
       PROCEDURE DIVISION.
                      
       MAIN-PARA.
           OPEN OUTPUT CLIENTESFILE.
           MOVE "72673837X" TO DNI.
           MOVE "Javier Tello" TO NOMBAPE.
           MOVE "978838473" TO TEL.
           MOVE "Avda/ Sin calle" TO DIR.
           MOVE "1111111111" TO CUENTA.
           MOVE 1000 TO SALDO.
           MOVE "N" TO BLOQUEADA.
           MOVE "cobol" TO CLAVE.
           WRITE CLIENTESREC.
           
           MOVE "82673837X" TO DNI.
           MOVE "Sergio Josa" TO NOMBAPE.
           MOVE "978838473" TO TEL.
           MOVE "Avda/ Sin calle" TO DIR.
           MOVE "2222222222" TO CUENTA.
           MOVE 2050 TO SALDO.
           MOVE "Y" TO BLOQUEADA.
           MOVE "cobol" TO CLAVE.
           WRITE CLIENTESREC.
           
           MOVE "18384932C" TO DNI.
           MOVE "Sergio Gil" TO NOMBAPE.
           MOVE "976838475" TO TEL.
           MOVE "Avda/ Sin calle 2" TO DIR.
           MOVE "3333333333" TO CUENTA.
           MOVE 2.05 TO SALDO.
           MOVE "N" TO BLOQUEADA.
           MOVE "cobol" TO CLAVE.
           WRITE CLIENTESREC.
           CLOSE CLIENTESFILE.
           
           
           OPEN OUTPUT MOVIMIENTOSFILE.
           
           MOVE 1 TO MID.
           MOVE 1411081538 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Ingreso" TO CONCEPTO.
           MOVE +50.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 1050 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 2 TO MID.
           MOVE 1411071538 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Reintegro" TO CONCEPTO.
           MOVE -5.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 1045 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 3 TO MID.
           MOVE 1411071538 TO FECHORA.
           MOVE "2222222222" TO CUENTAMOV.
           MOVE "Reintegro" TO CONCEPTO.
           MOVE -50.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 2000 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           *> Cada transferencia se insertan 2 movimientos
           *> Transferencia a y transferencia recibida
           MOVE 4 TO MID.
           MOVE 1411021538 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Transferencia a Sergio Josa (82673837X)" TO CONCEPTO.
           MOVE -20.00 TO CANTIDAD.
           MOVE "2222222222" TO DESTINO.
           MOVE 1025 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 5 TO MID.
           MOVE 1411021538 TO FECHORA.
           MOVE "2222222222" TO CUENTAMOV.
           MOVE "Transferencia recibida de Javier Tello (72673837X)" 
                TO CONCEPTO.
           MOVE 20.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 2020 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 6 TO MID.
           MOVE 1410021538 TO FECHORA.
           MOVE "2222222222" TO CUENTAMOV.
           MOVE "Ingreso" TO CONCEPTO.
           MOVE 200.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 2220 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 7 TO MID.
           MOVE 1310021538 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Ingreso" TO CONCEPTO.
           MOVE 100.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 1125 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 8 TO MID.
           MOVE 1405071538 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Ingreso" TO CONCEPTO.
           MOVE 100.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 1225 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 9 TO MID.
           MOVE 1406071538 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Ingreso" TO CONCEPTO.
           MOVE 900.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 2125 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 10 TO MID.
           MOVE 1406072338 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Ingreso" TO CONCEPTO.
           MOVE 500.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 2625 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 11 TO MID.
           MOVE 1407171538 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Ingreso" TO CONCEPTO.
           MOVE 10.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 2635 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 12 TO MID.
           MOVE 1408171538 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Rintegro" TO CONCEPTO.
           MOVE -635.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 2000 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 13 TO MID.
           MOVE 1408181538 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Rintegro" TO CONCEPTO.
           MOVE -500.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 1500 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 14 TO MID.
           MOVE 1409181538 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Rintegro" TO CONCEPTO.
           MOVE -500.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 1000 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 15 TO MID.
           MOVE 1409181531 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Ingreso" TO CONCEPTO.
           MOVE 500.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 1500 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           MOVE 16 TO MID.
           MOVE 1410181531 TO FECHORA.
           MOVE "1111111111" TO CUENTAMOV.
           MOVE "Ingreso" TO CONCEPTO.
           MOVE 500.00 TO CANTIDAD.
           MOVE "" TO DESTINO.
           MOVE 2000 TO SALDOCUENTA.
           WRITE MOVIMIENTOSREC.
           
           CLOSE MOVIMIENTOSFILE.
           
           OPEN OUTPUT ESPECTACULOSFILE.

           MOVE 1 TO NUM.
           MOVE "Concierto Gaga" TO NOMBRE.
           MOVE "Muy guai" TO DESCRIPCION.
           MOVE 141231 TO FECHA.
           MOVE 20.00 TO PRECIO.
           MOVE 20 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           MOVE 2 TO NUM.
           MOVE "Concierto Melendi" TO NOMBRE.
           MOVE "NO SALE" TO DESCRIPCION.
           MOVE 141031 TO FECHA.
           MOVE 90.00 TO PRECIO.
           MOVE 100 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           MOVE 3 TO NUM.
           MOVE "Parking Norte" TO NOMBRE.
           MOVE "Muy feo" TO DESCRIPCION.
           MOVE 141029 TO FECHA.
           MOVE 10.00 TO PRECIO.
           MOVE 700 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           MOVE 4 TO NUM.
           MOVE "Parking Sur" TO NOMBRE.
           MOVE "Muy feo" TO DESCRIPCION.
           MOVE 151029 TO FECHA.
           MOVE 10.00 TO PRECIO.
           MOVE 700 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           MOVE 5 TO NUM.
           MOVE "Parking Este" TO NOMBRE.
           MOVE "Muy feo" TO DESCRIPCION.
           MOVE 151029 TO FECHA.
           MOVE 10.00 TO PRECIO.
           MOVE 700 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           MOVE 6 TO NUM.
           MOVE "Parking Oeste" TO NOMBRE.
           MOVE "Muy feo" TO DESCRIPCION.
           MOVE 151029 TO FECHA.
           MOVE 10.00 TO PRECIO.
           MOVE 700 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           MOVE 7 TO NUM.
           MOVE "Estopa en directo" TO NOMBRE.
           MOVE "Muy feo" TO DESCRIPCION.
           MOVE 151029 TO FECHA.
           MOVE 10.00 TO PRECIO.
           MOVE 700 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           MOVE 8 TO NUM.
           MOVE "Bolinxe" TO NOMBRE.
           MOVE "Muy feo" TO DESCRIPCION.
           MOVE 151029 TO FECHA.
           MOVE 10.00 TO PRECIO.
           MOVE 700 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           MOVE 9 TO NUM.
           MOVE "Compinches" TO NOMBRE.
           MOVE "Muy guay si" TO DESCRIPCION.
           MOVE 151029 TO FECHA.
           MOVE 1.00 TO PRECIO.
           MOVE 1 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           MOVE 10 TO NUM.
           MOVE "Timo" TO NOMBRE.
           MOVE "Muy guay si" TO DESCRIPCION.
           MOVE 151029 TO FECHA.
           MOVE 900.00 TO PRECIO.
           MOVE 800 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           
           MOVE 11 TO NUM.
           MOVE "Banda loca" TO NOMBRE.
           MOVE "Nuevo disco" TO DESCRIPCION.
           MOVE 151029 TO FECHA.
           MOVE 10.50 TO PRECIO.
           MOVE 100 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           MOVE 12 TO NUM.
           MOVE "Amaral" TO NOMBRE.
           MOVE "Nuevo disco" TO DESCRIPCION.
           MOVE 151029 TO FECHA.
           MOVE 10.50 TO PRECIO.
           MOVE 100 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           MOVE 13 TO NUM.
           MOVE "Fito y Fitipaldis" TO NOMBRE.
           MOVE "Nuevo disco" TO DESCRIPCION.
           MOVE 151029 TO FECHA.
           MOVE 13.50 TO PRECIO.
           MOVE 103 TO DISPONIBLES.
           WRITE ESPECTACULOSREC.
           
           CLOSE ESPECTACULOSFILE.
       END PROGRAM CREAFICHEROS.