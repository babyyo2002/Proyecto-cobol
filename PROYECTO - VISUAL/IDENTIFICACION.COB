      ******************************************************************
      * AUTORES:AITOR, MARILUZ, NOELIA
      * PROGRAMA: PROYECTO
      * FECHA:01/12/23
      * OBJETIVO: SE IDENTIFICARA EL USUARIO Y DECIDE SI ES ALTA, BAJA
      * O MODIFICACION DE DATOS

      ******************************************************************
      *                 IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.IDENTIFICACION.
      ******************************************************************
      *                   ENVIRONMENT DIVISION
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


      ******************************************************************
      *                       DATA DIVISION
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
      *----FICHERO DE ENTRADA.

      *----FICHERO DE SALIDA.

      ******************************************************************
      *                  WORKING-STORAGE SECTION
      ******************************************************************
       WORKING-STORAGE SECTION.
      *----FILE STATUS
      * 01  FS-USUARIES                       PIC 9(02).
      * 01  FS-USUARIES-X                     PIC 9(02).
      *----COPYS DE ARCHIVOS(PARA DEFINIR EL AREA DEL REGISTRO)

      *----TABLAS.

      *----AUXILIARES.
       01  AUXILIARES.
           03  ACCION                        PIC 9(01) VALUE ZEROS.

      *----SWITCHES.
      * 01  SWITCHES.
      *     03  SW-EOF-USUARIES               PIC X(02)   VALUE "NO".

      *----CONTADORES-ESTADISTICAS.
      * 01  CONTADORES.

      *----CABECERA

      *----DETALLE

      *----PIE
       01  WS-REGUSUA.
           05 WS-REGUSUA-CODIGO              PIC 9(03)     VALUE ZEROS.
           05 WS-REGUSUA-NOMBRE              PIC X(10)     VALUE SPACES.
           05 WS-REGUSUA-ALIAS               PIC X(10)     VALUE SPACES.
           05 WS-REGUSUA-LOCALICAD           PIC X(10)     VALUE SPACES.
           05 WS-REGUSUA-TLF                 PIC 9(09)     VALUE ZEROS.
           05 WS-REGUSUA-FECHA-NACI          PIC 9(08)     VALUE ZEROS.
           05 WS-REGUSUA-CONTRASENA          PIC 9(08)     VALUE ZEROS.
       01  WS-REGUSUA-ACT                    PIC X(58)     VALUE SPACES.
      ******************************************************************
      *                    PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION.
      *-------------------
           PERFORM 1000-INICIO  THRU 1000-FIN-INICIO

           PERFORM 2000-PROCESO THRU 2000-FIN-PROCESO

           PERFORM 3000-FIN     THRU 3000-FIN-FIN.

       1000-INICIO.
      *------------
           DISPLAY "COMIENZA EL PROGRAMA"
           DISPLAY " "

           INITIALIZE  AUXILIARES.

       1000-FIN-INICIO.
      *----------------
           EXIT.

       2000-PROCESO.
      *-------------
           PERFORM 2100-TRATAR-ALTA THRU 2100-FIN-TRATAR-ALTA

           PERFORM 2200-ACTUALIZAR-ALTA THRU 2200-FIN-ACTUALIZAR-ALTA
           
           DISPLAY "ELIGE ACCION: "
           DISPLAY "PULSA 1 SI QUIERES DARTE DE ALTA"
           DISPLAY "PULSA 2 SI QUIERES MODIFICAR "
           DISPLAY "PULSA 3 SI QUIERES DARTE BAJA"
           ACCEPT ACCION

           EVALUATE ACCION
           WHEN = 1
      *         PERFORM 2100-TRATAR-ALTA THRU 2100-FIN-TRATAR-ALTA

      *         PERFORM 2200-ACTUALIZAR-ALTA
      *            THRU 2200-FIN-ACTUALIZAR-ALTA
           WHEN = 2
               PERFORM 2300-TRATAR-MODIF THRU 2300-FIN-TRATAR-MODIF

           WHEN = 3
               PERFORM 2400-TRATAR-BAJA THRU 2400-FIN-TRATAR-BAJA
           END-EVALUATE.
               DISPLAY "111111".

       2000-FIN-PROCESO.
      *-----------------
           EXIT.

       2100-TRATAR-ALTA.
      *-----------------
           CALL 'ALTA_USUARIO' USING WS-REGUSUA.
      *     CALL 'ACT_ALTA_USUARIO' USING WS-REGUSUA.

       2100-FIN-TRATAR-ALTA.
      *---------------------
           EXIT.

       2200-ACTUALIZAR-ALTA.
      *     CALL 'ACT_ALTA_USUARIO' USING WS-REGUSUA.

       2200-FIN-ACTUALIZAR-ALTA.
           EXIT.

       2300-TRATAR-MODIF.
      *---------------------
           CALL "MODIF_USUARIO" USING WS-REGUSUA.

       2300-FIN-TRATAR-MODIF.
      *---------------------
           EXIT.

       2400-TRATAR-BAJA.
      *-----------------
           CALL "BAJA_USUARIO" USING WS-REGUSUA.

       2400-FIN-TRATAR-BAJA.
      *---------------------
           EXIT.

       3000-FIN.
      *---------
           DISPLAY "COMPLETADO"
           STOP RUN.
       3000-FIN-FIN.
      *-------------
           EXIT.


       END PROGRAM IDENTIFICACION.
