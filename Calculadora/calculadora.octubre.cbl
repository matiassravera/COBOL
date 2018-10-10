      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 calculo.
           05 numero1           PIC S9(16)V99.
           05 resultado         PIC S9(16)V99.
           05 resultadolindo    PIC ZZZ9.99-.

       01 operando              PIC x(1).
           88 suma             VALUE 's'.
           88 resta            VALUE 'r'.
           88 multiplicacion   VALUE 'm'.
           88 divis            VALUE 'd'.
           88 terminar         VALUE 't'.

       PROCEDURE DIVISION.

           PERFORM 1000-inicio.


           PERFORM 1500-iterar  UNTIL terminar.



           STOP RUN.








           1000-inicio.

           INITIALIZE calculo.

           fin-1000-inicio. exit.

           1500-iterar.

               PERFORM 3100-ingresar-num1yoper.

               PERFORM 3200-ingresar-resultado.

                 IF operando EQUALS 'd'

                    PERFORM 3300-validardivis
                    until resultado NOT EQUALS zero

                 END-IF.

              PERFORM 4000-ejecutarcalculo.

              PERFORM 5000-mostrarresultado.

           fin-1500-iterar. exit.

           3100-ingresar-num1yoper.

           DISPLAY "ingrese numero 1: ".
           ACCEPT numero1.

           DISPLAY "ingrese operando: ".
           ACCEPT operando.

           if operando EQUALS 't'
           DISPLAY "Operacion finalizada"
               STOP RUN.

           fin-3100-ingresar-num1yoper. exit.


           3200-ingresar-resultado.
           if resultado EQUALs ZERO
               DISPLAY "ingrese numero 2: "
               ACCEPT resultado
               .

           fin-3200-ingresar-resultado. exit.


           3300-validardivis.

                   DISPLAY "no se puede dividir por cero"
                   PERFORM 3200-ingresar-resultado
               .

           fin-3300-validardivis. exit.

           4000-ejecutarcalculo.

               EVALUATE operando
                   WHEN 's'
                       ADD numero1 to resultado
                   WHEN 'r'
                       SUBTRACT resultado from numero1
                   WHEN 'm'
                       MULTIPLY resultado by numero1
                   WHEN 'd'
                       DIVIDE resultado INTO numero1
                   WHEN 't'
                       SET terminar to TRUE
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE.


           fin-4000-ejecutarcalculo. exit.


           5000-mostrarresultado.

           if not terminar
            move resultado to resultadolindo
            DISPLAY "resultado: " resultadolindo

           else
               DISPLAY "Operacion finalizada"

           END-IF.

           fin-5000-mostrarresultado. exit.


       END PROGRAM YOUR-PROGRAM-NAME.
