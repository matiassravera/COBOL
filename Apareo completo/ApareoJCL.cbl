                                                                        00010000
******************************************************************      00020000
      * AUTHOR: MARTIN                                                  00030000
      * DATE: 03-10-18                                                  00040000
      * PURPOSE:                                                        00050000
      * TECTONICS: COBC                                                 00060000
                                                                        00070000
******************************************************************      00080000
      * ARCHIVO PLANO SECUENCIAL                                        00090000
      * ARCHIVO VSAM INDEXADO                                           00100000
       IDENTIFICATION DIVISION.                                         00110000
       PROGRAM-ID. Apareo1.                                             00120000
       ENVIRONMENT DIVISION.                                            00130000
       CONFIGURATION SECTION.                                           00140000
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.                           00150000
       INPUT-OUTPUT SECTION.                                            00160000
       FILE-CONTROL.                                                    00170000
           SELECT ARCHIVO-A ASSIGN TO "CLIENTES"                        00180000
           ORGANIZATION SEQUENTIAL                                      00190000
           ACCESS MODE SEQUENTIAL                                       00200000
           FILE STATUS IS FS-A.                                         00210000
           SELECT ARCHIVO-B ASSIGN TO "SALDO"                           00220000
           ORGANIZATION SEQUENTIAL                                      00230000
           ACCESS MODE SEQUENTIAL                                       00240000
           FILE STATUS IS FS-B.                                         00250000
           SELECT ARCHIVO-C ASSIGN TO "APAREO"                          00260000
           ORGANIZATION SEQUENTIAL                                      00270000
           ACCESS MODE SEQUENTIAL                                       00280000
           FILE STATUS IS FS-C.                                         00290000
           SELECT ARCHIVO-D ASSIGN TO "DIFFERS"                         00300000
           ORGANIZATION SEQUENTIAL                                      00310000
           ACCESS MODE SEQUENTIAL                                       00320000
           FILE STATUS IS FS-D.                                         00330000
       DATA DIVISION.                                                   00340000
       FILE SECTION.                                                    00350000
       FD  ARCHIVO-A.                                                   00360000
       01  REGISTRO-A.                                                  00370000
               03 FILLER   PIC X(5).                                    00380000
               03 IDN      PIC 9(5).                                    00390000
               03 NOMBRE   PIC X(10).                                   00400000
               03 APELLIDO PIC X(10).                                   00410000
               03 FILLER   PIC X(3).                                    00420000
       FD  ARCHIVO-B.                                                   00430000
       01  REGISTRO-B.                                                  00440000
               03 FILLER PIC X(15).                                     00450000
               03 IDN    PIC 9(5).                                      00460000
               03 FILLER PIC X(12).                                     00470000
               03 SALDOS PIC 9(5).V99.                                  00480000
       FD  ARCHIVO-C.                                                   00490000
       01  REGISTRO-C.                                                  00500000
               03 APELLIDO PIC X(10).                                   00510000
               03 NOMBRE   PIC X(10).                                   00520000
               03 SALDOS   PIC 9(5).V99.                                00530000
       FD  ARCHIVO-D.                                                   00540000
       01  REGISTRO-D.                                                  00550000
               03 APELLIDO PIC X(10).                                   00560000
               03 NOMBRE   PIC X(10).                                   00570000
               03 SALDOS   PIC 9(5).V99.                                00580000
       WORKING-STORAGE SECTION.                                         00590000
       01  FS-A PIC X(2).                                               00600000
               88 FS-A-OK    VALUE "00".                                00610000
               88 FS-A-EOF   VALUE "10".                                00620000
       01  FS-B PIC X(2).                                               00630000
               88 FS-B-OK    VALUE "00".                                00640000
               88 FS-B-EOF   VALUE "10".                                00650000
       01  FS-C PIC X(2).                                               00660000
               88 FS-C-OK    VALUE "00".                                00670000
               88 FS-C-EOF   VALUE "10".                                00680000
       01  FS-D PIC X(2).                                               00690000
               88 FS-D-OK    VALUE "00".                                00700000
               88 FS-D-EOF   VALUE "10".                                00710000
       01  CONTADORES.                                                  00720000
               03 LEIDOSA   PIC 9(3).                                   00730000
               03 LEIDOSB   PIC 9(3).                                   00740000
               03 CARGADOSC PIC 9(3).                                   00750000
               03 CARGADOSD PIC 9(3).                                   00760000
       PROCEDURE DIVISION.                                              00770000
           PERFORM 1000-INICIO THRU 1000-FIN-INICIO                     00780000
           PERFORM 1500-LEER-A THRU 1500-FIN-LEER-A                     00790000
           PERFORM 1600-LEER-B THRU 1600-FIN-LEER-B                     00800000
           PERFORM 2000-PROCESO UNTIL FS-A-EOF AND FS-B-EOF.            00810000
           PERFORM 3500-CIERRE-ARCHIVOS THRU 3500-FIN-CIERRE-ARCHIVOS.  00820000
           PERFORM 4000-MOSTRAR-DATOS THRU 4000-FIN-MOSTRAR-DATOS.      00830000
                                                                        00840000
       1000-INICIO.                                                     00850000
           INITIALIZE CONTADORES.                                       00860000
           OPEN INPUT ARCHIVO-A.                                        00870000
           EVALUATE FS-A                                                00880000
               WHEN "00"                                                00890000
                    CONTINUE                                            00900000
               WHEN "10"                                                00910000
                    CONTINUE                                            00920000
               WHEN OTHER                                               00930000
                    DISPLAY "ERROR EN APERTURA ARCHIVO-A"               00940000
                    DISPLAY "FILE STATUS ERROR " FS-A                   00950000
                    STOP RUN                                            00960000
           END-EVALUATE                                                 00970000
           .                                                            00980000
           OPEN INPUT ARCHIVO-B.                                        00990000
           EVALUATE FS-B                                                01000000
               WHEN "00"                                                01010000
                    CONTINUE                                            01020000
               WHEN "10"                                                01030000
                    CONTINUE                                            01040000
               WHEN OTHER                                               01050000
                    DISPLAY "ERROR EN APERTURA ARCHIVO-B"               01060000
                    DISPLAY "FILE STATUS ERROR " FS-B                   01070000
                    STOP RUN                                            01080000
           END-EVALUATE                                                 01090000
           .                                                            01100000
           OPEN OUTPUT ARCHIVO-C.                                       01110000
           EVALUATE FS-C                                                01120000
               WHEN "00"                                                01130000
                    CONTINUE                                            01140000
               WHEN OTHER                                               01150000
                    DISPLAY "ERROR EN APERTURA ARCHIVO-C"               01160000
                    DISPLAY "FILE STATUS ERROR " FS-C                   01170000
                    STOP RUN                                            01180000
           END-EVALUATE                                                 01190000
           .                                                            01200000
           OPEN OUTPUT ARCHIVO-D.                                       01210000
           EVALUATE FS-D                                                01220000
               WHEN "00"                                                01230000
                    CONTINUE                                            01240000
               WHEN OTHER                                               01250000
                    DISPLAY "ERROR EN APERTURA ARCHIVO-D"               01260000
                    DISPLAY "FILE STATUS ERROR " FS-D                   01270000
                    STOP RUN                                            01280000
           END-EVALUATE                                                 01290000
           .                                                            01300000
       1000-FIN-INICIO.                                                 01310000
           EXIT.                                                        01320000
       1500-LEER-A.                                                     01330000
           READ ARCHIVO-A                                               01340000
           EVALUATE FS-A                                                01350000
               WHEN "00"                                                01360000
                    ADD 1 TO LEIDOSA                                    01370000
               WHEN "10"                                                01380000
                    MOVE 99999 TO IDN OF ARCHIVO-A                      01390000
               WHEN OTHER                                               01400000
                    DISPLAY "ERROR EN LECTURA ARCHIVO-A"                01410000
                    DISPLAY "FILE STATUS ERROR " FS-A                   01420000
                    STOP RUN                                            01430000
           END-EVALUATE                                                 01440000
           .                                                            01450000
       1500-FIN-LEER-A.                                                 01460000
           EXIT.                                                        01470000
       1600-LEER-B.                                                     01480000
           READ ARCHIVO-B                                               01490000
           EVALUATE FS-B                                                01500000
               WHEN "00"                                                01510000
                    ADD 1 TO LEIDOSB                                    01520000
               WHEN "10"                                                01530000
                    MOVE 99999 TO IDN OF ARCHIVO-B                      01540000
               WHEN OTHER                                               01550000
                    DISPLAY "ERROR EN LECTURA ARCHIVO-B"                01560000
                    DISPLAY "FILE STATUS ERROR " FS-B                   01570000
                    STOP RUN                                            01580000
           END-EVALUATE                                                 01590000
           .                                                            01600000
       1600-FIN-LEER-B.                                                 01610000
           EXIT.                                                        01620000
       2000-PROCESO.                                                    01630000
           IF IDN OF REGISTRO-A EQUAL IDN OF REGISTRO-B                 01640000
                   PERFORM 2500-ARMAR-C                                 01650000
                   PERFORM 3000-GRABA-C                                 01660000
                   PERFORM 1500-LEER-A                                  01670000
                   PERFORM 1600-LEER-B                                  01680000
           ELSE                                                         01690000
               IF IDN OF REGISTRO-A LESS THAN IDN OF REGISTRO-B         01700000
                  PERFORM 3100-GRABA-D-DESDE-A                          01710000
                  PERFORM 1500-LEER-A                                   01720000
               ELSE                                                     01730000
                  PERFORM 3200-GRABA-D-DESDE-B                          01740000
                  PERFORM 1600-LEER-B                                   01750000
               END-IF                                                   01760000
           END-IF                                                       01770000
           .                                                            01780000
       2000-FIN-PROCESO.                                                01790000
           EXIT.                                                        01800000
       2500-ARMAR-C.                                                    01810000
           MOVE APELLIDO OF REGISTRO-A TO APELLIDO OF REGISTRO-C        01820000
           MOVE NOMBRE   OF REGISTRO-A TO NOMBRE   OF REGISTRO-C        01830000
           MOVE SALDOS   OF REGISTRO-B TO SALDOS   OF REGISTRO-C.       01840000
       2500-FIN-ARMAR-C.                                                01850000
           EXIT.                                                        01860000
       3000-GRABA-C.                                                    01870000
           WRITE REGISTRO-C.                                            01880000
           EVALUATE FS-C                                                01890000
               WHEN "00"                                                01900000
                    ADD 1 TO CARGADOSC                                  01910000
                    CONTINUE                                            01920000
               WHEN OTHER                                               01930000
                    DISPLAY "ERROR EN ESCRITURA ARCHIVO-C"              01940000
                    DISPLAY "FILE STATUS ERROR " FS-C                   01950000
                    STOP RUN                                            01960000
           END-EVALUATE.                                                01970000
       3000-FIN-GRABA-C.                                                01980000
           EXIT.                                                        01990000
       3100-GRABA-D-DESDE-A.                                            02000000
           WRITE REGISTRO-D.                                            02010000
           EVALUATE FS-D                                                02020000
               WHEN "00"                                                02030000
                    ADD 1 TO CARGADOSD                                  02040000
                    CONTINUE                                            02050000
               WHEN OTHER                                               02060000
                    DISPLAY "ERROR EN ESCRITURA ARCHIVO-D"              02070000
                    DISPLAY "FILE STATUS ERROR " FS-D                   02080000
                    STOP RUN                                            02090000
           END-EVALUATE.                                                02100000
       3100-FIN-GRABA-D-DESDE-A.                                        02110000
           EXIT.                                                        02120000
       3200-GRABA-D-DESDE-B.                                            02130000
           WRITE REGISTRO-D.                                            02140000
           EVALUATE FS-D                                                02150000
               WHEN "00"                                                02160000
                    ADD 1 TO CARGADOSD                                  02170000
                    CONTINUE                                            02180000
               WHEN OTHER                                               02190000
                    DISPLAY "ERROR EN ESCRITURA ARCHIVO-D"              02200000
                    DISPLAY "FILE STATUS ERROR " FS-D                   02210000
                    STOP RUN                                            02220000
           END-EVALUATE.                                                02230000
       3200-FIN-GRABA-D-DESDE-B.                                        02240000
           EXIT.                                                        02250000
       3500-CIERRE-ARCHIVOS.                                            02260000
           CLOSE ARCHIVO-A.
           EVALUATE FS-A                                                00880000
               WHEN "00"                                                00890000
                    CONTINUE                                            00900000
               WHEN "10"                                                00910000
                    CONTINUE                                            00920000
               WHEN OTHER                                               00930000
                    DISPLAY "ERROR EN CLAUSURA ARCHIVO-A"               00940000
                    DISPLAY "FILE STATUS ERROR " FS-A                   00950000
                    STOP RUN                                            00960000
           END-EVALUATE                                                 00970000
           .                                                            00980000
           CLOSE ARCHIVO-B.                                             00990000
           EVALUATE FS-B                                                01000000
               WHEN "00"                                                01010000
                    CONTINUE                                            01020000
               WHEN "10"                                                01030000
                    CONTINUE                                            01040000
               WHEN OTHER                                               01050000
                    DISPLAY "ERROR EN CLAUSURA ARCHIVO-B"               01060000
                    DISPLAY "FILE STATUS ERROR " FS-B                   01070000
                    STOP RUN                                            01080000
           END-EVALUATE                                                 01090000
           .                                                            01100000
           CLOSE ARCHIVO-C.                                             01110000
           EVALUATE FS-C                                                01120000
               WHEN "00"                                                01130000
                    CONTINUE                                            01140000
               WHEN OTHER                                               01150000
                    DISPLAY "ERROR EN CLAUSURA ARCHIVO-C"               01160000
                    DISPLAY "FILE STATUS ERROR " FS-C                   01170000
                    STOP RUN                                            01180000
           END-EVALUATE                                                 01190000
           .                                                            01200000
           CLOSE ARCHIVO-D.                                             01210000
           EVALUATE FS-D                                                01220000
               WHEN "00"                                                01230000
                    CONTINUE                                            01240000
               WHEN OTHER                                               01250000
                    DISPLAY "ERROR EN CLAUSURA ARCHIVO-D"               01260000
                    DISPLAY "FILE STATUS ERROR " FS-D                   01270000
                    STOP RUN                                            01280000
           END-EVALUATE
           .
           3500-FIN-CIERRE-ARCHIVOS.                                    02310000
           EXIT.                                                        02320000
       4000-MOSTRAR-DATOS.                                              02330000
           DISPLAY "CANTIDAD DE ARCHIVOS LEIDOS DE A  : " LEIDOSA       02340000
           DISPLAY "CANTIDAD DE ARCHIVOS LEIDOS DE B  : " LEIDOSB       02350000
           DISPLAY "CANTIDAD DE ARCHIVOS CARGADOS EN C: " CARGADOSC     02360000
           DISPLAY "CANTIDAD DE ARCHIVOS CARGADOS EN D: " CARGADOSD     02370000
           STOP RUN.                                                    02380000
       4000-FIN-MOSTRAR-DATOS.EXIT.
