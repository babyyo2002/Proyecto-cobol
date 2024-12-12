      ******************************************************************
      * Author:Maria Romero
      * Date:12/12/2024
      * Purpose: Dado el archivo VENTAS.DAT con copy:
      *    codigo de producto (5 caracteres)
      *    cantidad vendida (3 digitos)
      *    precio unitario (5 digitos con dos decimales)
      *    se solicita leer el archivo, calcular el total vendido y
      *    guardar la informacion en el archivo TOTALVENTAS.DAT con copy
      *    codigo de producto (5 caracteres)
      *    total vendido con dos decimales
      *    reto adicional calcula y muestra el producto mas vendido
      *    en cantidad

      *    A falta de excepciones y manejo de errores
      *    se tomara un maximo de ILIMITADOS registros en el archivo VENTAS.DAT
      *    Se ordena con el comando:
      *        "sort ventas.dat > ventas_sorted.dat"

      ******************************************************************
      *                 IDENTIFICATION DIVISION
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESAR-VENTAS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VENTAS ASSIGN TO "VENTAS_sorted.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TOTAL ASSIGN TO "TOTAL.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD VENTAS.
       01 REG-VENTAS.
           05 CODIGO-VENTAS   PIC X(5).
           05 CANT-VENTAS     PIC 9(3).
           05 PRECIO-VENTAS   PIC 9(3)V99.
       FD TOTAL.
       01 REG-TOTAL.
           05 CODIGO-TOTAL    PIC X(5).
           05 INGRESO-TOTAL   PIC 9(8)V99.

       WORKING-STORAGE SECTION.
       01 EOF-SWITCH          PIC X VALUE "N".

       01 ACUMULADOR.
           05 ACTUAL-CODIGO   PIC X(5) VALUE SPACES.
           05 ACTUAL-CANTIDAD PIC 9(5) VALUE ZEROS.
           05 ACTUAL-INGRESO  PIC 9(8)V99 VALUE ZEROS.

       01 REGISTRO-LECTURA.
           05 LECTURA-CODIGO  PIC X(5).
           05 LECTURA-CANT    PIC 9(3).
           05 LECTURA-PRECIO  PIC 9(3)V99.

       01 MAS-VENDIDO.
           05 MAS-VENDIDO-CODIGO   PIC X(5) VALUE SPACES.
           05 MAS-VENDIDO-CANTIDAD PIC zzzz9 VALUE ZEROS.

       PROCEDURE DIVISION.
       INICIO.
      *    Ordenar el archivo ventas.dat con el comando
      *    "sort ventas.dat > ventas_sorted.dat"
           OPEN INPUT VENTAS
           OPEN OUTPUT TOTAL
           PERFORM LEER-REGISTRO
           PERFORM PROCESAR-VENTAS UNTIL EOF-SWITCH = "S"
           PERFORM GRABAR-TOTAL
           CLOSE VENTAS
           CLOSE TOTAL
           PERFORM MOSTRAR-RESULTADO

           STOP RUN.

       LEER-REGISTRO.
           READ VENTAS INTO REG-VENTAS
               AT END MOVE "S" TO EOF-SWITCH
           NOT AT END
               MOVE CODIGO-VENTAS TO LECTURA-CODIGO
               MOVE CANT-VENTAS TO LECTURA-CANT
               MOVE PRECIO-VENTAS TO LECTURA-PRECIO
           END-READ.

       PROCESAR-VENTAS.
           IF ACTUAL-CODIGO = SPACES
               MOVE LECTURA-CODIGO TO ACTUAL-CODIGO
           END-IF

           IF LECTURA-CODIGO NOT = ACTUAL-CODIGO
               PERFORM GRABAR-TOTAL
               MOVE LECTURA-CODIGO TO ACTUAL-CODIGO
               MOVE ZEROS TO ACTUAL-CANTIDAD
               MOVE ZEROS TO ACTUAL-INGRESO
           END-IF

           ADD LECTURA-CANT TO ACTUAL-CANTIDAD
           COMPUTE ACTUAL-INGRESO = ACTUAL-INGRESO +
                                    (LECTURA-CANT * LECTURA-PRECIO)

      * Comparar con el más vendido actual

           IF ACTUAL-CANTIDAD > MAS-VENDIDO-CANTIDAD
               MOVE ACTUAL-CODIGO TO MAS-VENDIDO-CODIGO
               MOVE ACTUAL-CANTIDAD TO MAS-VENDIDO-CANTIDAD
           END-IF

           PERFORM LEER-REGISTRO.

       GRABAR-TOTAL.
           IF ACTUAL-CODIGO NOT = SPACES
               MOVE ACTUAL-CODIGO TO CODIGO-TOTAL
               MOVE ACTUAL-INGRESO TO INGRESO-TOTAL
               WRITE REG-TOTAL
               DISPLAY REG-TOTAL
           END-IF.

       MOSTRAR-RESULTADO.
           DISPLAY "************************************************"
                   "**************************"
           DISPLAY "EL PRODUCTO MAS VENDIDO ES: " MAS-VENDIDO-CODIGO
                   " CON UN TOTAL DE UNIDADES VENDIDAS: "
                   MAS-VENDIDO-CANTIDAD.
           DISPLAY "                                  *  *** ***  *"
                   "                           "
           DISPLAY "                                  * *   *   * *"
                   "                           "
           DISPLAY "                                  *  *     *  *"
                   "                           "
           DISPLAY "                                  *   *   *   *"
                   "                           "
           DISPLAY "                                  *     *     *"
                   "                           "
           DISPLAY "************************************************"
                   "**************************".
