      ******************************************************************
      * AUTORES:AITOR, MARILUZ, NOELIA
      * PROGRAMA: PROYECTO
      * FECHA:01/12/23
      * OBJETIVO: CREAR FICHERO ALTA_USUARIO CON LOS DATOS INTRODUCIDOS
      * POR TECLADO.

      ******************************************************************
      *                 IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.ALTA_USUARIO.
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
           SELECT USUARIES ASSIGN TO 'USUARIES.TXT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS FS-USUARIES.

      *     SELECT SORT-USUARIES  ASSIGN TO SORTUSUARIES.

           SELECT ALTA ASSIGN TO 'ALTA_USUARIO.TXT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS FS-ALTA.
      ******************************************************************
      *                       DATA DIVISION
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
      *----FICHERO DE ENTRADA.
       FD  USUARIES
       BLOCK CONTAINS 0 RECORDS
       RECORDING MODE IS F.
       01  REG-USUARIES                       PIC X(58).

      *----FICHERO DE SALIDA.
       FD  ALTA
       BLOCK CONTAINS 0 RECORDS
       RECORDING MODE IS F.
       01  REG-ALTA                       PIC X(58).
      ******************************************************************
      *                  WORKING-STORAGE SECTION
      ******************************************************************
       WORKING-STORAGE SECTION.
      *----FILE STATUS
       01  FS-USUARIES                       PIC 9(02).
       01  FS-ALTA                           PIC 9(02).
      *----COPYS DE ARCHIVOS(PARA DEFINIR EL AREA DEL REGISTRO)
           COPY REGUSUARIES.
           COPY REGUSU_ALTA.

      *----TABLAS.

      *----AUXILIARES.
       01  AUXILIARES.
           03  ULTIMO-CODIGO                 PIC 9(03) VALUE ZEROS.
           03  AUXILIAR-ALIAS                PIC X(10) VALUE SPACES.
           03  AUXILIAR-CONTRASEÑA           PIC X(08) VALUE SPACES.

      *----SWITCHES.
       01  SWITCHES.
           03  SW-EOF-USUARIES               PIC X(02)   VALUE "NO".

      *----CONTADORES-ESTADISTICAS.
       01  CONTADORES.

           03  C-ESTADISTICAS.
               05  CONTA-USUARIOS            PIC 9(03)   VALUE ZEROS.
       01  WS-REGUSUA.
           05 WS-REGUSUA-CODIGO              PIC 9(03)     VALUE ZEROS.
           05 WS-REGUSUA-NOMBRE              PIC X(10)     VALUE SPACES.
           05 WS-REGUSUA-ALIAS               PIC X(10)     VALUE SPACES.
           05 WS-REGUSUA-LOCALICAD           PIC X(10)     VALUE SPACES.
           05 WS-REGUSUA-TLF                 PIC 9(09)     VALUE ZEROS.
           05 WS-REGUSUA-FECHA-NACI          PIC 9(08)     VALUE ZEROS.
           05 WS-REGUSUA-CONTRASENA          PIC 9(08)     VALUE ZEROS.
      ******************************************************************
       LINKAGE SECTION.
       01  LS-REGUSUA.
           05 LS-REGUSUA-CODIGO              PIC 9(03)     VALUE ZEROS.
           05 LS-REGUSUA-NOMBRE              PIC X(10)     VALUE SPACES.
           05 LS-REGUSUA-ALIAS               PIC X(10)     VALUE SPACES.
           05 LS-REGUSUA-LOCALICAD           PIC X(10)     VALUE SPACES.
           05 LS-REGUSUA-TLF                 PIC 9(09)     VALUE ZEROS.
           05 LS-REGUSUA-FECHA-NACI          PIC 9(08)     VALUE ZEROS.
           05 LS-REGUSUA-CONTRASENA          PIC 9(08)     VALUE ZEROS.
      *----CABECERA

      *----DETALLE

      *----PIE

      ******************************************************************
      *                    PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION USING LS-REGUSUA.
      *-------------------
           PERFORM 1000-INICIO  THRU 1000-FIN-INICIO

           PERFORM 2000-PROCESO THRU 2000-FIN-PROCESO
      *                            UNTIL SW-EOF-USUARIES ="SI"
           PERFORM 3000-FIN     THRU 3000-FIN-FIN.

       1000-INICIO.
      *------------
           DISPLAY "COMIENZA EL PROGRAMA"
           DISPLAY " "

           INITIALIZE  AUXILIARES
      *                 CONTADORES

           PERFORM 1200-ABRIR-FICHEROS THRU 1200-FIN-ABRIR-FICHEROS.

       1000-FIN-INICIO.
      *----------------
           EXIT.

       1200-ABRIR-FICHEROS.
      *--------------------
           OPEN INPUT USUARIES
               OUTPUT ALTA.

       1200-FIN-ABRIR-FICHEROS.
      *------------------------
           EXIT.

       2000-PROCESO.
      *-------------
           PERFORM 2100-LEER-USUARIES THRU 2100-FIN-LEER-USUARIES
                           UNTIL SW-EOF-USUARIES = "SI"

           PERFORM 2200-TRATAR-ALTA THRU 2200-FIN-TRATAR-ALTA
           PERFORM 2300-GRABAR-ALTA THRU 2300-FIN-GRABAR-ALTA.

       2000-FIN-PROCESO.
      *-----------------
           EXIT.

       2100-LEER-USUARIES.
      *------------------
           READ USUARIES INTO REGUSUA
              AT END MOVE "SI" TO SW-EOF-USUARIES
                     ADD 1     TO ULTIMO-CODIGO
           END-READ

           IF SW-EOF-USUARIES = "NO"
               MOVE REGUSUA-CODIGO TO ULTIMO-CODIGO
           END-IF.

       2100-FIN-LEER-USUARIES.
      *-----------------------
           EXIT.

       2200-TRATAR-ALTA.
      *-----------------
           MOVE ULTIMO-CODIGO TO REGUSU_ALTA-CODIGO
           DISPLAY "INTRODUCE TU NOMBRE COMPLETO: "
           ACCEPT REGUSU_ALTA-NOMBRE
           DISPLAY "INTRODUCE TU ALIAS: "
           ACCEPT REGUSU_ALTA-ALIAS
           DISPLAY "INTRODUCE TU LOCALIDAD: "
           ACCEPT REGUSU_ALTA-LOCALICAD
           DISPLAY "INTRODUCE TU TELEFONO(XXXXXXXXX): "
           ACCEPT REGUSU_ALTA-TLF
           DISPLAY "INTRODUCE TU FECHA NACIMIENTO (DDMMYYYY): "
           ACCEPT REGUSU_ALTA-FECHA-NACI
           DISPLAY "INTRODUCE TU CONTRASENNA: "
           ACCEPT REGUSU_ALTA-CONTRASENA.

       2200-FIN-TRATAR-ALTA.
      *---------------------
           EXIT.

       2300-GRABAR-ALTA.
      *-----------------
           WRITE REG-ALTA FROM REGUSU_ALTA.

       2300-FIN-GRABAR-ALTA.
      *---------------------
           EXIT.

       3000-FIN.
      *---------
           PERFORM 3100-CERRAR-FICHERO  THRU 3100-FIN-CERRAR-FICHERO
           CALL 'ACT_ALTA_USUARIO' USING WS-REGUSUA
           STOP RUN.
       3000-FIN-FIN.
      *-------------
           EXIT.

       3100-CERRAR-FICHERO.
      *--------------------
           CLOSE USUARIES
                 ALTA.

       3100-FIN-CERRAR-FICHERO.
      *------------------------
           EXIT.

       END PROGRAM ALTA_USUARIO.
