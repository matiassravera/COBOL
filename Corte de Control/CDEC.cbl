     ******************************************************************
      * Author: Matías Sebastian Ravera
      * Date: 05/10/2018
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CortesDeControl.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ENTRADA ASSIGN TO "C:\Users\musashi\bin\FEC-Y-SUC.txt"
       ORGANIZATION SEQUENTIAL
       ACCESS MODE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ENTRADA.

       01  REGISTRO-ENTRADA.
           03  FECHA               PIC 9(6).
           03  NRO-SUCURSAL        PIC 9(2).
           03  NOM-SUC             PIC X(10).
           03  CLIENTE             PIC 9(5).
           03  CHEQUES             PIC 9(3).

       WORKING-STORAGE SECTION.

       01  VARIABLES.
           03  WS-FEC-ANTERIOR     PIC 9(10).
           03  WS-SUC-ANTERIOR     PIC 9(10).
           03  QTOT                PIC 9(10).
           03  QSUC                PIC 9(10).
           03  QFEC                PIC 9(10).


       01  FS-ENTRADA              PIC X(2).
           88 FS-OK                VALUE '00'.
           88 FS-EOF               VALUE '10'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           INITIALIZE VARIABLES.
           PERFORM 1000-ABERTURA-DE-ARCHIVO.
           PERFORM 2000-LECTURA-DE-ARCHIVO.
           PERFORM 3000-PROCESO UNTIL FS-EOF.
           PERFORM 3700-BALANCE-DE-CHEQUES.
           PERFORM 4000-CIERRE-DE-ARCHIVOS.

            STOP RUN.

       1000-ABERTURA-DE-ARCHIVO.
           OPEN INPUT ENTRADA
           EVALUATE FS-ENTRADA
               WHEN '00'
               CONTINUE
               WHEN '10'
               CONTINUE
               WHEN OTHER
               DISPLAY "ERROR AL ABRIR EL ARCHIVO"
               DISPLAY "FILE STATUS ERROR" FS-ENTRADA
           END-EVALUATE
           .
       1000-FIN-ABERTURA-DE-ARCHIVO.
           EXIT.

       2000-LECTURA-DE-ARCHIVO.

           READ ENTRADA.
           EVALUATE FS-ENTRADA
               WHEN '00'
               CONTINUE
               WHEN '10'
               CONTINUE
               WHEN OTHER
               DISPLAY "ERROR EN LECTURA DE ARCHIVO"
               DISPLAY "FILE STATUS ERROR" FS-ENTRADA
           END-EVALUATE
           .
       2000-FIN-LECTURA-DE-ARCHIVO.
           EXIT.

       3000-PROCESO.

           3300-CHEQUES-POR-SUCURSALES.

       3000-FIN-PROCESO.
           EXIT.

       3300-CHEQUES-POR-SUCURSALES.
           IF NRO-SUCURSAL EQUAL WS-SUC-ANTERIOR
               ADD CHEQUES TO QSUC
               PERFORM 3500-CHEQUES-POR-FECHAS
               ELSE
                   MOVE NRO-SUCURSAL TO WS-SUC-ANTERIOR
                   INITIALIZE QSUC
                   ADD CHEQUES TO QSUC
                   PERFORM 3500-CHEQUES-POR-FECHAS
           END-IF.
       3300-FIN-CHEQUES-POR-SUCURSALES.
           EXIT.

       3500-CHEQUES-POR-FECHAS.
           IF FECHA EQUAL WS-FEC-ANTERIOR
               ADD CHEQUES TO QFEC
               ADD CHEQUES TO QTOT
               PERFORM 2000-LECTURA-DE-ARCHIVO
               ELSE
                   MOVE FECHA TO WS-FEC-ANTERIOR
                   INITIALIZE QFEC
                   ADD CHEQUES TO QFEC
                   ADD CHEQUES TO QTOT
                   PERFORM 2000-LECTURA-DE-ARCHIVO
           END-IF.
       3500-FIN-CHEQUES-POR-FECHAS.
           EXIT.

       3700-BALANCE-DE-CHEQUES.
           IF FS-EOF
              DISPLAY "TOTAL GENERAL DE CHEQUES: "      QTOT
              DISPLAY "TOTAL DE CHEQUES POR FECHA: "    QFEC
              DISPLAY "TOTAL DE CHEQUES POR SUCURSAL: " QSUC
           END-IF
           .
       3700-FIN-BALANCE-DE-CHEQUES.
           EXIT.

       4000-CIERRE-DE-ARCHIVOS.
           CLOSE  ENTRADA
           EVALUATE FS-ENTRADA
               WHEN "00"
                    CONTINUE
               WHEN "10"
                    CONTINUE
               WHEN OTHER
                    DISPLAY "ERROR EN EL CIERRE DE ARCHIVO"
           END-EVALUATE.

       4000-FIN-CIERRE-DE-ARCHIVOS.
           EXIT.

       END PROGRAM CortesDeControl.
