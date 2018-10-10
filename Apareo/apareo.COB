      ******************************************************************
      * Author:ELIAS TRONCOSO
      * Date:05/10/18
      * Purpose:
      * Tectonics: cobc
      ******************************************************************

       PROGRAM-ID. Apareo.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT archivo-a    ASSIGN TO
           "clientes.txt"
           ORGANISATION SEQUENTIAL
           ACCESS MODE  SEQUENTIAL
           FILE STATUS IS FS-a.

           SELECT archivo-b    ASSIGN TO
           "saldo-mod2.txt"
           ORGANISATION SEQUENTIAL
           ACCESS MODE  SEQUENTIAL
           FILE STATUS IS FS-b.

           SELECT archivo-c    ASSIGN TO
           "APAREO.txt"
           ORGANISATION SEQUENTIAL
           ACCESS MODE  SEQUENTIAL
           FILE STATUS IS FS-c.

           SELECT archivo-d    ASSIGN TO
           "NO-APAREO.txt"
           ORGANISATION SEQUENTIAL
           ACCESS MODE  SEQUENTIAL
           FILE STATUS IS FS-d.

       DATA DIVISION.
       FILE SECTION.

           FD archivo-a.

           01 REGIS-A.

           02 NUMERO-A   PIC 9(5).
           02 NOMBRE-A   PIC X(10).
           02 APELLIDO-A PIC X(10).
           02 LETRA-A    PIC X(3).
           02 FILLER pic XXX.

           FD archivo-b.
  v
           01 REGIS-B.
           02 BASURA-B PIC X(15).
           02 NUMERO-B  PIC 9(5).
           02 BASURA2-B PIC X(12).
           02 SALDO-B  PIC 9(7).
           02 FILLER pic XXX.

           FD archivo-c
           LABEL RECORD IS STANDARD.

           01 GRABAR-C PIC X(10).

           FD archivo-d
           LABEL RECORD IS STANDARD.

           01 GRABAR-D-DESDE-A PIC X(10).
           01 GRABAR-D-DESDE-B PIC X(10).

       WORKING-STORAGE SECTION.

           01 FS-a PIC X(2).
               88 FS-a-OK   VALUE "00".
               88 FS-a-EOF  VALUE "10".

           01 FS-b PIC X(2).
               88 FS-b-OK   VALUE "00".
               88 FS-b-EOF  VALUE "10".

           01 FS-c PIC X(2).
               88 FS-c-OK   VALUE "00".
               88 FS-c-EOF  VALUE "10".

           01 FS-d PIC X(2).
               88 FS-d-OK   VALUE "00".
               88 FS-d-EOF  VALUE "10".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM 1000-INICIO

           PERFORM 1500-LEER-CLIENTES

           PERFORM 1600-LEER-SALDO

           PERFORM 2000-PROCESO THRU F-2000-FIN
             UNTIL FS-a-EOF AND FS-b-EOF

           PERFORM 6000-CERRAR-PROCESO

           STOP RUN.

       1000-INICIO.

           OPEN INPUT archivo-a.

           EVALUATE FS-a
               WHEN "00"
                    CONTINUE
               WHEN "10"
                    CONTINUE
               WHEN OTHER
                    DISPLAY "ERROR EN APERTURA archivo-a"
                    DISPLAY "FILE STATUS ERROR " FS-a

           END-EVALUATE.

           OPEN INPUT archivo-b.
           EVALUATE FS-b
               WHEN "00"
                    CONTINUE
               WHEN "10"
                    CONTINUE
               WHEN OTHER
                    DISPLAY "ERROR EN APERTURA archivo-b"
                    DISPLAY "FILE STATUS ERROR " FS-b

           END-EVALUATE

           OPEN OUTPUT archivo-c.
            EVALUATE FS-c
               WHEN "00"
                    CONTINUE
               WHEN "10"
                    CONTINUE
               WHEN OTHER
                    DISPLAY "ERROR EN APERTURA archivo-c"
                    DISPLAY "FILE STATUS ERROR " FS-c
           END-EVALUATE.

           OPEN OUTPUT archivo-d
           EVALUATE FS-d
              WHEN "00"
                    CONTINUE
               WHEN "10"
                    CONTINUE
               WHEN OTHER
                    DISPLAY "ERROR EN APERTURA archivo-d"
                    DISPLAY "FILE STATUS ERROR " FS-d

           END-EVALUATE.


       1000-FIN-INICIO. EXIT.

       1500-LEER-CLIENTES.
           IF NOT FS-a-EOF
              READ archivo-a
           END-IF
           DISPLAY "REGIS-A " REGIS-A

           EVALUATE FS-a
               WHEN "00"
                    CONTINUE
               WHEN "10"
                    CONTINUE
               WHEN OTHER
                    DISPLAY "ERROR EN LECTURA archivo-a"
                    DISPLAY "FILE STATUS ERROR " FS-a

           END-EVALUATE
           .

       1500-FIN-LEER-CLIENTES.
       EXIT.

       1600-LEER-SALDO.
           IF NOT FS-b-EOF
              READ archivo-b
           END-IF
           DISPLAY "REGIS-B " REGIS-B.
           EVALUATE FS-b
               WHEN "00"
                    CONTINUE
               WHEN "10"
                    CONTINUE
               WHEN OTHER
                    DISPLAY "ERROR EN LECTURA archivo-b"
                    DISPLAY "FILE STATUS ERROR " FS-b


           END-EVALUATE
           .
       1600-fin-LEER-SALDO.
           EXIT.

       2000-PROCESO.

           IF NUMERO-A EQUAL NUMERO-B
               PERFORM 3000-GRABAR-C
               PERFORM 1500-LEER-CLIENTES
               PERFORM 1600-LEER-SALDO
           ELSE
               IF NUMERO-A LESS THAN NUMERO-B
                  PERFORM 3100-GRABAR-D-DESDE-A
                  PERFORM 1500-LEER-CLIENTES
               ELSE
                  PERFORM 3200-GRABAR-D-DESDE-B
                  PERFORM 1600-LEER-SALDO
               END-IF

           END-IF .

       F-2000-FIN.
           EXIT.

       3000-GRABAR-C.
           WRITE GRABAR-C FROM REGIS-A .

       3000-fin-GRABAR-C. EXIT.

       3100-GRABAR-D-DESDE-A.
           WRITE GRABAR-D-DESDE-A FROM REGIS-A.
       3100-fin-GRABAR-D-DESDE-A. EXIT.

       3200-GRABAR-D-DESDE-B.
             WRITE GRABAR-D-DESDE-B FROM REGIS-B .
       3200-fin-GRABAR-D-DESDE-B. EXIT.

       2000-fin-PROCESO.
           EXIT.

       5000-MOSTRAR-RESULTADO.
               DISPLAY "LOS APAREOS SON: ",GRABAR-C.
       5000-fin-MOSTRAR-RESULTADO.
       6000-CERRAR-PROCESO.

           CLOSE archivo-a.

           EVALUATE FS-a
               WHEN "00"
                    CONTINUE
               WHEN "10"
                    CONTINUE
               WHEN OTHER
                    DISPLAY "ERROR EN APERTURA archivo-a"
                    DISPLAY "FILE STATUS ERROR " FS-a
                    STOP RUN
           END-EVALUATE.

           CLOSE archivo-b.
           EVALUATE FS-b
               WHEN "00"
                    CONTINUE
               WHEN "10"
                    CONTINUE
               WHEN OTHER
                    DISPLAY "ERROR EN APERTURA archivo-b"
                    DISPLAY "FILE STATUS ERROR " FS-b
                    STOP RUN
           END-EVALUATE

           CLOSE archivo-c.
           EVALUATE FS-c
               WHEN "00"
                    CONTINUE
               WHEN "10"
                    CONTINUE
               WHEN OTHER
                    DISPLAY "ERROR EN APERTURA archivo-c"
                    DISPLAY "FILE STATUS ERROR " FS-c
                    STOP RUN
           END-EVALUATE
           .

           CLOSE archivo-d

           EVALUATE FS-d
               WHEN "00"
                    CONTINUE
               WHEN "10"
                    CONTINUE
               WHEN OTHER
                    DISPLAY "ERROR EN APERTURA archivo-d"
                    DISPLAY "FILE STATUS ERROR " FS-d
                    STOP RUN
           END-EVALUATE.

       6000-CERRAR-PROCESO-fin.
           EXIT.

       END PROGRAM Apareo.
