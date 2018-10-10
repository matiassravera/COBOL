                                                                        03620000
******************************************************************      03630000
      * AUTHOR: MARTIN                                                  03640000
      * DATE: 03-10-18                                                  03650000
      * PURPOSE:                                                        03660000
      * TECTONICS: COBC                                                 03670000
                                                                        03680000
******************************************************************      03690000
      * ARCHIVO PLANO SECUENCIAL                                        03700000
      * ARCHIVO VSAM INDEXADO                                           03710000
       IDENTIFICATION DIVISION.                                         03720000
       PROGRAM-ID. ApareoaN.                                            03730000
       ENVIRONMENT DIVISION.                                            03740000
       CONFIGURATION SECTION.                                           03750000
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.                           03760000
       INPUT-OUTPUT SECTION.                                            03770000
       FILE-CONTROL.                                                    03780000
           SELECT ARCHIVO-A ASSIGN TO "CLIENTES"                        03790000
           ORGANIZATION SEQUENTIAL                                      03800000
           ACCESS MODE SEQUENTIAL                                       03810000
           FILE STATUS IS FS-A.                                         03820000
           SELECT ARCHIVO-B ASSIGN TO "SALDO"                           03830000
           ORGANIZATION SEQUENTIAL                                      03840000
           ACCESS MODE SEQUENTIAL                                       03850000
           FILE STATUS IS FS-B.                                         03860000
           SELECT ARCHIVO-C ASSIGN TO "APAREO"                          03870000
           ORGANIZATION SEQUENTIAL                                      03880000
           ACCESS MODE SEQUENTIAL                                       03890000
           FILE STATUS IS FS-C.                                         03900000
           SELECT ARCHIVO-D ASSIGN TO "DIFFERS"                         03910000
           ORGANIZATION SEQUENTIAL                                      03920000
           ACCESS MODE SEQUENTIAL                                       03930000
           FILE STATUS IS FS-D.                                         03940000
       DATA DIVISION.                                                   03950000
       FILE SECTION.                                                    03960000
       FD  ARCHIVO-A.                                                   03970000
       01  REGISTRO-A.                                                  03980000
               03 FILLER   PIC X(5).                                    03990000
               03 IDN      PIC 9(5).                                    04000000
               03 NOMBRE   PIC X(10).                                   04010000
               03 APELLIDO PIC X(10).                                   04020000
               03 FILLER   PIC X(3).                                    04030000
       FD  ARCHIVO-B.                                                   04040000
       01  REGISTRO-B.                                                  04050000
               03 FILLER PIC X(15).                                     04060000
               03 IDN    PIC 9(5).                                      04070000
               03 FILLER PIC X(12).                                     04080000
               03 SALDOS PIC 9(5).V99.                                  04090000
       FD  ARCHIVO-C.                                                   04100000
       01  REGISTRO-C.                                                  04110000
               03 APELLIDO PIC X(10).                                   04120000
               03 NOMBRE   PIC X(10).                                   04130000
               03 SALDOS   PIC 9(5).V99.                                04140000
       FD  ARCHIVO-D.                                                   04150000
       01  REGISTRO-D.                                                  04160000
               03 APELLIDO PIC X(10).                                   04170000
               03 NOMBRE   PIC X(10).                                   04180000
               03 SALDOS   PIC 9(5).V99.                                04190000
       WORKING-STORAGE SECTION.                                         04200000
       01  FS-A PIC X(2).                                               04210000
               88 FS-A-OK    VALUE "00".                                04220000
               88 FS-A-EOF   VALUE "10".                                04230000
       01  FS-B PIC X(2).                                               04240000
               88 FS-B-OK    VALUE "00".                                04250000
               88 FS-B-EOF   VALUE "10".                                04260000
       01  FS-C PIC X(2).                                               04270000
               88 FS-C-OK    VALUE "00".                                04280000
               88 FS-C-EOF   VALUE "10".                                04290000
       01  FS-D PIC X(2).                                               04300000
               88 FS-D-OK    VALUE "00".                                04310000
               88 FS-D-EOF   VALUE "10".                                04320000
       01  CONTADORES.                                                  04330000
               03 LEIDOSA   PIC 9(3).                                   04340000
               03 LEIDOSB   PIC 9(3).                                   04350000
               03 CARGADOSC PIC 9(3).                                   04360000
               03 CARGADOSD PIC 9(3).                                   04370000
       PROCEDURE DIVISION.                                              04380000
           PERFORM 1000-INICIO THRU 1000-FIN-INICIO                     04390000
           PERFORM 1500-LEER-A THRU 1500-FIN-LEER-A                     04400000
           PERFORM 1600-LEER-B THRU 1600-FIN-LEER-B                     04410000
           PERFORM 2000-PROCESO UNTIL FS-A-EOF AND FS-B-EOF.            04420000
           PERFORM 3500-CIERRE-ARCHIVOS THRU 3500-FIN-CIERRE-ARCHIVOS.  04430000
           PERFORM 4000-MOSTRAR-DATOS THRU 4000-FIN-MOSTRAR-DATOS.      04440000
                                                                        04450000
       1000-INICIO.                                                     04460000
           INITIALIZE CONTADORES.                                       04470000
           OPEN INPUT ARCHIVO-A.                                        04480000
           EVALUATE FS-A                                                04490000
               WHEN "00"                                                04500000
                    CONTINUE                                            04510000
               WHEN "10"                                                04520000
                    CONTINUE                                            04530000
               WHEN OTHER                                               04540000
                    DISPLAY "ERROR EN APERTURA ARCHIVO-A"               04550000
                    DISPLAY "FILE STATUS ERROR " FS-A                   04560000
                    STOP RUN                                            04570000
           END-EVALUATE                                                 04580000
           .                                                            04590000
           OPEN INPUT ARCHIVO-B.                                        04600000
           EVALUATE FS-B                                                04610000
               WHEN "00"                                                04620000
                    CONTINUE                                            04630000
               WHEN "10"                                                04640000
                    CONTINUE                                            04650000
               WHEN OTHER                                               04660000
                    DISPLAY "ERROR EN APERTURA ARCHIVO-B"               04670000
                    DISPLAY "FILE STATUS ERROR " FS-B                   04680000
                    STOP RUN                                            04690000
           END-EVALUATE                                                 04700000
           .                                                            04710000
           OPEN OUTPUT ARCHIVO-C.                                       04720000
           EVALUATE FS-C                                                04730000
               WHEN "00"                                                04740000
                    CONTINUE                                            04750000
               WHEN OTHER                                               04760000
                    DISPLAY "ERROR EN APERTURA ARCHIVO-C"               04770000
                    DISPLAY "FILE STATUS ERROR " FS-C                   04780000
                    STOP RUN                                            04790000
           END-EVALUATE                                                 04800000
           .                                                            04810000
           OPEN OUTPUT ARCHIVO-D.                                       04820000
           EVALUATE FS-D                                                04830000
               WHEN "00"                                                04840000
                    CONTINUE                                            04850000
               WHEN OTHER                                               04860000
                    DISPLAY "ERROR EN APERTURA ARCHIVO-D"               04870000
                    DISPLAY "FILE STATUS ERROR " FS-D                   04880000
                    STOP RUN                                            04890000
           END-EVALUATE                                                 04900000
           .                                                            04910000
       1000-FIN-INICIO.                                                 04920000
           EXIT.                                                        04930000
       1500-LEER-A.                                                     04940000
           READ ARCHIVO-A                                               04950000
           EVALUATE FS-A                                                04960000
               WHEN "00"                                                04970000
                    ADD 1 TO LEIDOSA                                    04980000
               WHEN "10"                                                04990000
                    MOVE 99999 TO IDN OF ARCHIVO-A                      05000000
               WHEN OTHER                                               05010000
                    DISPLAY "ERROR EN LECTURA ARCHIVO-A"                05020000
                    DISPLAY "FILE STATUS ERROR " FS-A                   05030000
                    STOP RUN                                            05040000
           END-EVALUATE                                                 05050000
           .                                                            05060000
       1500-FIN-LEER-A.                                                 05070000
           EXIT.                                                        05080000
       1600-LEER-B.                                                     05090000
           READ ARCHIVO-B                                               05100000
           EVALUATE FS-B                                                05110000
               WHEN "00"                                                05120000
                    ADD 1 TO LEIDOSB                                    05130000
               WHEN "10"                                                05140000
                    MOVE 99999 TO IDN OF ARCHIVO-B                      05150000
               WHEN OTHER                                               05160000
                    DISPLAY "ERROR EN LECTURA ARCHIVO-B"                05170000
                    DISPLAY "FILE STATUS ERROR " FS-B                   05180000
                    STOP RUN                                            05190000
           END-EVALUATE                                                 05200000
           .                                                            05210000
       1600-FIN-LEER-B.                                                 05220000
           EXIT.                                                        05230000
       2000-PROCESO.                                                    05240000
           IF IDN OF REGISTRO-A EQUAL IDN OF REGISTRO-B                 05250000
                   PERFORM UNTIL IDN OF REGISTRO-A NOT EQUAL            05260000
                                 IDN OF REGISTRO-B                      05270000
                      PERFORM 2500-ARMAR-C                              05280000
                      PERFORM 3000-GRABA-C                              05290000
                      PERFORM 1600-LEER-B                               05300000
                   END-PERFORM                                          05310000
                   PERFORM 1500-LEER-A                                  05320000
                                                                        05330000
           ELSE                                                         05340000
               IF IDN OF REGISTRO-A LESS THAN IDN OF REGISTRO-B         05350000
                  PERFORM 3100-GRABA-D-DESDE-A                          05360000
                  PERFORM 1500-LEER-A                                   05370000
               ELSE                                                     05380000
                  PERFORM 3200-GRABA-D-DESDE-B                          05390000
                  PERFORM 1600-LEER-B                                   05400000
               END-IF                                                   05410000
           END-IF                                                       05420000
           .                                                            05430000
       2000-FIN-PROCESO.                                                05440000
           EXIT.                                                        05450000
       2500-ARMAR-C.                                                    05460000
           MOVE APELLIDO OF REGISTRO-A TO APELLIDO OF REGISTRO-C        05470000
           MOVE NOMBRE   OF REGISTRO-A TO NOMBRE   OF REGISTRO-C        05480000
           MOVE SALDOS   OF REGISTRO-B TO SALDOS   OF REGISTRO-C.       05490000
       2500-FIN-ARMAR-C.                                                05500000
           EXIT.                                                        05510000
       3000-GRABA-C.                                                    05520000
           WRITE REGISTRO-C.                                            05530000
           EVALUATE FS-C                                                05540000
               WHEN "00"                                                05550000
                    ADD 1 TO CARGADOSC                                  05560000
                    CONTINUE                                            05570000
               WHEN OTHER                                               05580000
                    DISPLAY "ERROR EN ESCRITURA ARCHIVO-C"              05590000
                    DISPLAY "FILE STATUS ERROR " FS-C                   05600000
                    STOP RUN                                            05610000
           END-EVALUATE.                                                05620000
       3000-FIN-GRABA-C.                                                05630000
           EXIT.                                                        05640000
       3100-GRABA-D-DESDE-A.                                            05650000
           WRITE REGISTRO-D.                                            05660000
           EVALUATE FS-D                                                05670000
               WHEN "00"                                                05680000
                    ADD 1 TO CARGADOSD                                  05690000
                    CONTINUE                                            05700000
               WHEN OTHER                                               05710000
                    DISPLAY "ERROR EN ESCRITURA ARCHIVO-D"              05720000
                    DISPLAY "FILE STATUS ERROR " FS-D                   05730000
                    STOP RUN                                            05740000
           END-EVALUATE.                                                05750000
       3100-FIN-GRABA-D-DESDE-A.                                        05760000
           EXIT.                                                        05770000
       3200-GRABA-D-DESDE-B.                                            05780000
           WRITE REGISTRO-D.                                            05790000
           EVALUATE FS-D                                                05800000
               WHEN "00"                                                05810000
                    ADD 1 TO CARGADOSD                                  05820000
                    CONTINUE                                            05830000
               WHEN OTHER                                               05840000
                    DISPLAY "ERROR EN ESCRITURA ARCHIVO-D"              05850000
                    DISPLAY "FILE STATUS ERROR " FS-D                   05860000
                    STOP RUN                                            05870000
           END-EVALUATE.                                                05880000
       3200-FIN-GRABA-D-DESDE-B.                                        05890000
           EXIT.                                                        05900000
       3500-CIERRE-ARCHIVOS.                                            05910000
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
           .                                                            05950000
       3500-FIN-CIERRE-ARCHIVOS.                                        05960000
           EXIT.                                                        05970000
       4000-MOSTRAR-DATOS.                                              05980000
           DISPLAY "CANTIDAD DE ARCHIVOS LEIDOS DE A  : " LEIDOSA       05990000
           DISPLAY "CANTIDAD DE ARCHIVOS LEIDOS DE B  : " LEIDOSB       06000000
           DISPLAY "CANTIDAD DE ARCHIVOS CARGADOS EN C: " CARGADOSC     06010000
           DISPLAY "CANTIDAD DE ARCHIVOS CARGADOS EN D: " CARGADOSD     06020000
           STOP RUN.                                                    06030000
       4000-FIN-MOSTRAR-DATOS.EXIT.                                     06040000
                                                                        06050000

