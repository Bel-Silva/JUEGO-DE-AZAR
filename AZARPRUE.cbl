       IDENTIFICATION DIVISION.
       PROGRAM-ID. AZARPRUE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 MASCARA           PIC XXXX VALUE 'xxxx'.

       01 HORARIO           PIC 9(8).
       01 HORA REDEFINES HORARIO.
         02 HH              PIC 99.
         02 HM              PIC 99.
         02 HS              PIC 99.
         02 HX              PIC 99.
       01 NUMERO            PIC 9(6).

       01 CAMPO-AZAR.
         05 CAMPO-01        PIC 9.
         05 CAMPO-02        PIC 9.
         05 CAMPO-03        PIC 9.
         05 CAMPO-04        PIC 9.

        01 VERIFICA           PIC X.
          88 VALIDO          VALUE 'T'.
          88 NOVALIDO        VALUE 'F'.

        LINKAGE SECTION.
       01 AREA-COMUNICACION.
         05 CAMPO-INCOGNITO PIC X(4).

       PROCEDURE DIVISION USING AREA-COMUNICACION.

       0000-CONTROL.

        PERFORM 1000-INICIO
        PERFORM 2000-PROCESO-CENTRAL
        PERFORM 3000-FINAL.

       1000-INICIO.

       2000-PROCESO-CENTRAL.

        ACCEPT HORARIO FROM TIME
       COMPUTE NUMERO = (HH * 1000) + (HX * 100) + (HM * 10) + HS

      * DISPLAY NUMERO

       MOVE NUMERO(5:1) TO CAMPO-01
       SET NOVALIDO TO TRUE
       PERFORM 2500-SEGUNDO UNTIL VALIDO

       SET NOVALIDO TO TRUE
       PERFORM 2600-TERCERO UNTIL VALIDO

       SET NOVALIDO TO TRUE
       PERFORM 2700-CUARTO UNTIL VALIDO.

      *********************************************

       2500-SEGUNDO.

       MOVE NUMERO(2:1) TO CAMPO-02
       IF CAMPO-01 NOT EQUAL CAMPO-02
         SET VALIDO TO TRUE
       ELSE
          ACCEPT HORARIO FROM TIME
          COMPUTE NUMERO = (HH * 1000) + (HX * 100) + (HM * 10) + HS
          MOVE NUMERO(2:1) TO CAMPO-02
       END-IF.

      ********************************************

       2600-TERCERO.

        ACCEPT HORARIO FROM TIME.
       COMPUTE HH =  HH + HX
       COMPUTE HX = HX + HM
       COMPUTE HS = HS + HH
       COMPUTE NUMERO = (HH * 1000) + (HX * 100) + (HM * 10) + HS

       MOVE NUMERO(4:1)  TO CAMPO-03

       IF CAMPO-03 NOT EQUAL CAMPO-01 AND
           CAMPO-03 NOT EQUAL CAMPO-02
           SET VALIDO TO TRUE
       ELSE
        ACCEPT HORARIO FROM TIME
        COMPUTE NUMERO = (HH * 1000) + (HX * 100) + (HM * 10) + HS
        MOVE NUMERO(4:1) TO CAMPO-03
       END-IF.

      ******************************************

       2700-CUARTO.

        ACCEPT HORARIO FROM TIME.
       COMPUTE HH =  HH + HX
       COMPUTE HX = HX + HM
       COMPUTE HS = HS + HH
       COMPUTE NUMERO = (HH * 1000) + (HX * 100) + (HM * 10) + HS

       MOVE NUMERO(4:1)  TO CAMPO-04

       IF CAMPO-04 NOT EQUAL CAMPO-01 AND
          CAMPO-04 NOT EQUAL CAMPO-02 AND
          CAMPO-04 NOT EQUAL CAMPO-03
        SET VALIDO TO TRUE
       ELSE
        ACCEPT HORARIO FROM TIME
        COMPUTE NUMERO = (HH * 1000) + (HX * 100) + (HM * 10) + HS
        MOVE NUMERO(6:1) TO CAMPO-04
       END-IF.

********************************************

       3000-FINAL.

       MOVE CAMPO-AZAR  TO CAMPO-INCOGNITO

       GOBACK.
