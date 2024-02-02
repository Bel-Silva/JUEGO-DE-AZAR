      ******************************************************************
      * Author:SILVA, D. BELEN
      * Date: 29/01/2024
      * IDENTIFICATION DIVISION.
       PROGRAM-ID. EJEAZAR1.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

      *****COMUNICACION CON LA RUTINA **********

           77  WS-RUTINA PIC X(8)  VALUE 'AZARPRUE'.

           01 AREA-COMUNICACION.
               05 CAMPO-INCOGNITO  PIC X(04).

      ******VARIABLE FLAG DE CONTROL ***********

           01  WS-FLAG-FIN    PIC X.
               88 WS-SI-INTENTOS    VALUE 'T'.
               88 WS-FIN-INTENTOS    VALUE 'F'.

           01  WS-VALIDO     PIC X.
               88 VALIDO       VALUE 'T'.
               88 NO-VALIDO    VALUE 'F'.

           01  WS-CONTINUA.
               05 WS-SI      PIC X.
               05 WS-NO      PIC X.

      *****VARIBLES AUXILIARES PARA CALCULOS ****

           01  WS-NUM-INTENTOS PIC 9(03).
           01  WP-NUM-INTENTOS PIC ZZZ.
           01  WS-NUM-REG      PIC 9.

           01  WS-RESULTADOS.
               05  WS-ACERTADOS    PIC 9.
               05  WS-REGULARES    PIC 9.
               05  WS-ERRADOS      PIC 9.

           01 WP-MASCARA      PIC X(04) VALUE 'XXXX'.

      *****TABLAS PARA POSICIONAR EL NUMERO *****

           01 TABLA-INCOG OCCURS 4 TIMES.
               05 DIGITO-INCOG PIC X.

           01  WS-I PIC 9 VALUE 1.

           01  WS-NUMERO  PIC 9(04).

           01 TABLA-INTENTO OCCURS 4 TIMES.
               05 DIGITO-INTENTO PIC X.

           01  WS-J PIC 9 VALUE 1.




        PROCEDURE DIVISION.

      ***************************************
      *    CUERPOR PRINCIPAL DEL PROGRAMA   *
      ***************************************


       MAIN-PROCEDURE.

           PERFORM 1000-INICIO.

           PERFORM 2000-PROCESOS UNTIL WS-FIN-INTENTOS


           PERFORM 9999-CIERRE.

           STOP RUN.


      ******************************************
      **     INICIO JUEGO, LLAMO RUINA        **
      ******************************************
           1000-INICIO.

           SET WS-SI-INTENTOS TO TRUE

           CALL WS-RUTINA USING AREA-COMUNICACION.

           MOVE 1 TO WS-I

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE CAMPO-INCOGNITO(WS-I:1) TO DIGITO-INCOG(WS-I)
           END-PERFORM


           INITIALIZE  WS-NUM-INTENTOS

           DISPLAY 'BIENVENIDO AL JUEGO!'
           DISPLAY 'NUMERO INCOGNITO: ' WP-MASCARA
           DISPLAY 'ADIVINA QUE NUMERO ES...'
           DISPLAY 'PISTA: SUS DIGITOS SON DISTINTOS ENTRE SI. '
           DISPLAY '****MUCHA SUERTE *****'.



       2000-PROCESOS.


           PERFORM 2300-INGRESA-NUM

           PERFORM 2500-COMPARAR-NUM

           IF WS-ACERTADOS = 4 THEN
               SET WS-FIN-INTENTOS TO TRUE
           ELSE
               DISPLAY 'NUMERO INGRESADO: ' WS-NUMERO
               DISPLAY 'CANTIDAD DE ACIERTOS: ' WS-ACERTADOS
               DISPLAY 'ACIERTOS REGULARES (POSICION INCORRECTA): '
                                   WS-REGULARES
               DISPLAY 'CANTIDAD DE ERRADOS: ' WS-ERRADOS
               DISPLAY '**********************'
               SET NO-VALIDO TO TRUE
               PERFORM 2100-CONTINUIDAD UNTIL VALIDO

           END-IF.

      **************************************************
       2100-CONTINUIDAD.


           DISPLAY '¿Desea intentar nuevamente? '
                      'Ingrese Y para Sí o N para No: '
           ACCEPT WS-CONTINUA

           EVALUATE WS-CONTINUA
             WHEN 'Y'
              SET VALIDO TO TRUE
             WHEN 'N'
               SET VALIDO TO TRUE
               SET WS-FIN-INTENTOS TO TRUE
             WHEN OTHER
                DISPLAY 'Por favor, ingrese Y para SI o N para No.'
         END-EVALUATE.

      ******************************************************
       2300-INGRESA-NUM.

           DISPLAY  'INGRESE NUMERO DE 4 DIGITOS: '
           ACCEPT WS-NUMERO

           ADD 1 TO WS-NUM-INTENTOS
           MOVE 1 TO WS-J

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE WS-NUMERO(WS-J:1) TO DIGITO-INTENTO(WS-J)
           END-PERFORM.

      ***************************************************

       2500-COMPARAR-NUM.

           MOVE 0 TO WS-ACERTADOS
                     WS-REGULARES
                     WS-ERRADOS


           PERFORM 2600-ACERTADOS

           IF WS-ACERTADOS < 4 THEN
                PERFORM 2700-REGULARES
                PERFORM 2800-ERRADOS
           ELSE
               SET WS-FIN-INTENTOS TO TRUE
           END-IF.


     *************************************************

       2600-ACERTADOS.

           MOVE 1 TO WS-I
                     WS-J


           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               IF DIGITO-INTENTO(WS-J) = DIGITO-INCOG(WS-I)
                  ADD 1 TO WS-ACERTADOS
               END-IF
               ADD 1 TO WS-J
           END-PERFORM.


      ************************************************

       2700-REGULARES.

           MOVE 1 TO WS-I
           MOVE 1 TO WS-J

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
            MOVE 1 TO WS-J
            PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
             IF DIGITO-INTENTO(WS-J) = DIGITO-INCOG(WS-I)
                  AND WS-J NOT EQUAL WS-I
                      ADD 1 TO WS-REGULARES
             END-IF
            END-PERFORM
           END-PERFORM.



      ************************************************

       2800-ERRADOS.
           MOVE 1 TO WS-I
                     WS-J
           MOVE 0 TO WS-ERRADOS

           PERFORM UNTIL WS-J > 4 AND WS-I> 4
              IF DIGITO-INTENTO(WS-J) NOT EQUAL DIGITO-INCOG(WS-I)
                ADD 1 TO WS-ERRADOS
              END-IF
              ADD 1 TO WS-J
              ADD 1 TO WS-I
           END-PERFORM

           SUBTRACT WS-REGULARES FROM WS-ERRADOS.



      ***********************************************

       9999-CIERRE.

           IF WS-ACERTADOS = 4 THEN
               DISPLAY 'HAS ADIVINADO EL NUMERO: ' WS-NUMERO

           ELSE
              DISPLAY 'TE RENDISTE :(. EL NUMERO OCULTO ERA: '
                                            CAMPO-INCOGNITO

           END-IF.

           MOVE WS-NUM-INTENTOS TO WP-NUM-INTENTOS
           DISPLAY 'REALIZASTE ' WP-NUM-INTENTOS ' INTENTOS.'.

       END PROGRAM EJEAZAR1.
