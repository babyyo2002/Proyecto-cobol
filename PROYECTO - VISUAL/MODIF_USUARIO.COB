      ******************************************************************
      * AUTORES:AITOR, MARILUZ, NOELIA
      * PROGRAMA: PROYECTO
      * FECHA:01/12/23
      * OBJETIVO: CREAR FICHERO MODIF_USUARIO.TXT INTRODUCIENDO LOS
      * DATOS POR TECLADO

      ******************************************************************
      *                 IDENTIFICATION DIVISION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.MODIF_USUARIO.
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

           SELECT MODIFICACION ASSIGN TO 'MODIF_USUARIO.TXT'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS FS-MODIFICACION.
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
       FD  MODIFICACION
       BLOCK CONTAINS 0 RECORDS
       RECORDING MODE IS F.
       01  REG-MODIFICACION                       PIC X(58).
      ******************************************************************
      *                  WORKING-STORAGE SECTION
      ******************************************************************
       WORKING-STORAGE SECTION.
      *----FILE STATUS
       01  FS-USUARIES                       PIC 9(02).
       01  FS-MODIFICACION                   PIC 9(02).

      *----COPYS DE ARCHIVOS(PARA DEFINIR EL AREA DEL REGISTRO)
           COPY REGUSUARIES.
           COPY REGUSU_MODIF.

      *----TABLAS.

      *----AUXILIARES.
       01  AUXILIARES.
      *     03  ULTIMO-CODIGO                 PIC 9(03) VALUE ZEROS.
           03  ACCION                        PIC 9(01) VALUE ZEROS.
           03  AUXILIAR-ALIAS                PIC X(10) VALUE SPACES.
           03  AUXILIAR-CONTRASENA           PIC X(08) VALUE SPACES.

      *----SWITCHES.
       01  SWITCHES.
           03  SW-EOF-USUARIES               PIC X(02)   VALUE "NO".

      *----CONTADORES-ESTADISTICAS.
       01  CONTADORES.

           03  C-ESTADISTICAS.
               05  CONTA-USUARIOS            PIC 9(03)   VALUE ZEROS.
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
               OUTPUT MODIFICACION.

       1200-FIN-ABRIR-FICHEROS.
      *------------------------
           EXIT.

       2000-PROCESO.
      *-------------
      *     DISPLAY "INTRODUCE TU ALIAS: "
      *     ACCEPT AUXILIAR-ALIAS
      *     DISPLAY "INTRODUCE TU CONTRASENNA: "
      *     ACCEPT AUXILIAR-CONTRASENA

           PERFORM 2100-LEER-USUARIES THRU 2100-FIN-LEER-USUARIES
                           UNTIL SW-EOF-USUARIES = "SI"
      *                     OR REGUSUA-ALIAS = AUXILIAR-ALIAS
                            OR REGUSUA-ALIAS = LS-REGUSUA-ALIAS

      *     IF AUXILIAR-ALIAS = REGUSUA-ALIAS
      *         AND AUXILIAR-CONTRASENA = REGUSUA-CONTRASENA

               PERFORM 2200-TRATAR-MODIF THRU 2200-FIN-TRATAR-MODIF
               PERFORM 2300-GRABAR-MODIF THRU 2300-FIN-GRABAR-MODIF.
      *     ELSE
      *         DISPLAY "USUARIO O CONTRASENA INCORRECTO"
      *     END-IF.

       2000-FIN-PROCESO.
      *-----------------
           EXIT.

       2100-LEER-USUARIES.
      *------------------
           READ USUARIES INTO REGUSUA
              AT END MOVE "SI" TO SW-EOF-USUARIES
           END-READ.

       2100-FIN-LEER-USUARIES.
      *-----------------------
           EXIT.

       2200-TRATAR-MODIF.
      *-----------------
           DISPLAY "ELIGE CAMPO A MODIFICAR: "
           DISPLAY "INTRODUCE 1 PARA NOMBRE. "
           DISPLAY "INTRODUCE 2 PARA ALIAS. "
           DISPLAY "INTRODUCE 3 PARA LOCALIDAD. "
           DISPLAY "INTRODUCE 4 PARA TELEFONO. "
           DISPLAY "INTRODUCE 5 PARA FECHA NACIMIENTO. "
           DISPLAY "INTRODUCE 6 PARA CONTRASENNA. "
           ACCEPT ACCION

           MOVE REGUSUA TO REGUSU_MODIF

           EVALUATE ACCION
           WHEN 1
               DISPLAY "INTRODUCE TU NOMBRE: "
               ACCEPT REGUSU_MODIF-NOMBRE
           WHEN 2
               DISPLAY "INTRODUCE TU ALIAS: "
               ACCEPT REGUSU_MODIF-ALIAS
           WHEN 3
               DISPLAY "INTRODUCE TU LOCALIDAD: "
               ACCEPT REGUSU_MODIF-LOCALICAD
           WHEN 4
               DISPLAY "INTRODUCE TU TELEFONO: "
               ACCEPT REGUSU_MODIF-TLF
           WHEN 5
               DISPLAY "INTRODUCE TU FECHA NACIMIENTO: "
               ACCEPT REGUSU_MODIF-FECHA-NACI
           WHEN 6
               DISPLAY "INTRODUCE TU CONTRASENNA: "
               ACCEPT REGUSU_MODIF-CONTRASENA
           END-EVALUATE.

       2200-FIN-TRATAR-MODIF.
      *---------------------
           EXIT.

       2300-GRABAR-MODIF.
      *-----------------
           WRITE REG-MODIFICACION FROM REGUSU_MODIF.

       2300-FIN-GRABAR-MODIF.
      *---------------------
           EXIT.

       3000-FIN.
      *---------
           PERFORM 3100-CERRAR-FICHERO  THRU 3100-FIN-CERRAR-FICHERO

           STOP RUN.

       3000-FIN-FIN.
      *-------------
           EXIT.

       3100-CERRAR-FICHERO.
      *--------------------
           CLOSE USUARIES
                 MODIFICACION.

       3100-FIN-CERRAR-FICHERO.
      *------------------------
           EXIT.

       END PROGRAM MODIF_USUARIO.
