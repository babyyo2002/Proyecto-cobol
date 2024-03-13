      ******************************************************************
      * AUTOR: MARILUZ
      * FECHA:13/10/2023
      * OBJETIVO: USANDO LOS FICHEROS ALTA_USUARIO Y USUARIES, CREA EL
      * FICHERO NUEVO_USUARIO Y RENOMBRALO CON EL NOMBRE DE USUARIES.TXT
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID.ACT_ALTA_USUARIO.

      ******************************************************************
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *-----FICHEROS DE ENTRADA
           SELECT USUARIO ASSIGN TO "USUARIES.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS FS-USUARIO.

           SELECT ALTA ASSIGN TO "ALTA_USUARIO.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS FS-ALTA.

           SELECT N-USUARIO ASSIGN TO "NUEVO-USUARIO.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS FS-N-USUARIO.

           SELECT SORT-USUARIES  ASSIGN TO SORTUSUARIES.

      *     SELECT N-USUARIO-1 ASSIGN TO "NUEVO-USUARIO-1.TXT"
      *            ORGANIZATION IS LINE SEQUENTIAL
      *            FILE STATUS FS-N-USUARIO-1.

      ******************************************************************
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.
       FD  USUARIO
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
      *     LABEL RECORD IS STANDARD.
       01  REGUSUARIES                   PIC X(58).
       FD  ALTA
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
      *     LABEL RECORD IS STANDARD.
       01  REGALTA                       PIC X(58).

       FD  N-USUARIO
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
      *     LABEL RECORD IS STANDARD.
       01  REGNUSUARIO                  PIC X(58).
       SD  SORT-USUARIES
           RECORD CONTAINS 58 CHARACTERS.
       01  SORT-REGUSUA.
           05 SORT-REGUSUA-CODIGO            PIC 9(03) VALUE ZEROS.
           05 SORT-REGUSUA-NOMBRE            PIC X(10) VALUE SPACES.
           05 SORT-REGUSUA-ALIAS             PIC X(10) VALUE SPACES.
           05 SORT-REGUSUA-LOCALICAD         PIC X(10) VALUE SPACES.
           05 SORT-REGUSUA-TLF               PIC 9(09) VALUE ZEROS.
           05 SORT-REGUSUA-FECHA-NACI        PIC 9(08) VALUE ZEROS.
           05 SORT-REGUSUA-CONTRASENA        PIC 9(08) VALUE ZEROS.
      * FD  N-USUARIO-1
      *     BLOCK CONTAINS 0 RECORDS
      *     RECORDING MODE IS F.
      *     LABEL RECORD IS STANDARD.
      * 01  REGNUSUARIO-1                  PIC X(58).
      ******************************************************************
      *                  WORKING-STORAGE SECTION
      ******************************************************************
       WORKING-STORAGE SECTION.
      *----REGISTROS DE ESTADO.
       01  FS-USUARIO                   PIC 9(02)  VALUE ZEROS.
       01  FS-ALTA                      PIC 9(02)  VALUE ZEROS.
       01  FS-N-USUARIO                 PIC 9(02)  VALUE ZEROS.
      * 01  FS-N-USUARIO-1               PIC 9(02)  VALUE ZEROS.

      *----COPYS DE ARCHIVOS(PARA DEFINIR EL AREA DEL REGISTRO)
           COPY REGUSUARIES.
           COPY REGUSU_ALTA.
           COPY REG_N_USUARIES.
      *----TABLAS PARA CARGAR Y ACTUALIZAR FICHERO DE STOCK (S)

      *----SWITCHES FINAL DE FICHERO
       01  SW-EOF-USUARIO                PIC X(02)  VALUE "NO".
       01  SW-EOF-ALTA                   PIC X(02)  VALUE "NO".
       01  SW-EOF-BAJA                   PIC X(02)  VALUE "NO".
       01  SW-EOF-MODIF                  PIC X(02)  VALUE "NO".
      * 01  SW-VACIO                      PIC X(02)  VALUE "NO".

       LINKAGE SECTION.
       01  LS-REGUSUA.
           05 LS-REGUSUA-CODIGO              PIC 9(03)     VALUE ZEROS.
           05 LS-REGUSUA-NOMBRE              PIC X(10)     VALUE SPACES.
           05 LS-REGUSUA-ALIAS               PIC X(10)     VALUE SPACES.
           05 LS-REGUSUA-LOCALICAD           PIC X(10)     VALUE SPACES.
           05 LS-REGUSUA-TLF                 PIC 9(09)     VALUE ZEROS.
           05 LS-REGUSUA-FECHA-NACI          PIC 9(08)     VALUE ZEROS.
           05 LS-REGUSUA-CONTRASENA          PIC 9(08)     VALUE ZEROS.

      * 01  LS-REGUSUA-ACT                    PIC X(58)     VALUE SPACES.
      ******************************************************************
      *                    PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION USING LS-REGUSUA.
      *                         LS-REGUSUA-ACT.
      *                          LS-MOVIMIENTO.
      *-------------------
           DISPLAY "ENTRA EN PROGRAMA ACT_ALTA_USUARIO"
           PERFORM 1000-INICIO  THRU 1000-FIN-INICIO

           PERFORM 2000-PROCESO THRU 2000-FIN-PROCESO
                                UNTIL SW-EOF-USUARIO = "SI"
                                AND   SW-EOF-ALTA = "SI"
           PERFORM 3000-FIN     THRU 3000-FIN-FIN.

       1000-INICIO.
      *------------
           DISPLAY "COMIENZA EL PROGRAMA"
           DISPLAY " "

           INITIALIZE REGUSUARIES
                      REGUSU_ALTA.
                      REG_N_USUARIES.
      *                CONTADORES.

           PERFORM 1100-ABRIR-FICHEROS THRU 1100-FIN-ABRIR-FICHEROS
           PERFORM 9000-LEER-USUARIO    THRU 9000-FIN-LEER-USUARIO
           PERFORM 9100-LEER-ALTA     THRU 9100-FIN-LEER-ALTA.

       1000-FIN-INICIO.
      *----------------
           EXIT.

       1100-ABRIR-FICHEROS.
      *--------------------
           OPEN INPUT  USUARIO
                       ALTA
                OUTPUT N-USUARIO.
      *     DISPLAY "FICHEROS ABIERTOS".
       1100-FIN-ABRIR-FICHEROS.
      *------------------------
           EXIT.

       2000-PROCESO.
      *-------------
           IF REGUSU_ALTA-CODIGO = REGUSUA-CODIGO
               DISPLAY "CODIGO ERRONEO: " REGUSU_ALTA-CODIGO
               PERFORM 3000-FIN     THRU 3000-FIN-FIN
           ELSE
               IF REGUSUA-CODIGO < REGUSU_ALTA-CODIGO
                   MOVE REGUSUA TO REG_N_USUA
                   PERFORM 2100-GRABAR   THRU 2100-FIN-GRABAR
                   PERFORM 9000-LEER-USUARIO THRU 9000-FIN-LEER-USUARIO
               ELSE
                   MOVE REGUSU_ALTA TO REG_N_USUA
                   PERFORM 2100-GRABAR   THRU 2100-FIN-GRABAR
                   PERFORM 9100-LEER-ALTA THRU 9100-FIN-LEER-ALTA
               END-IF
           END-IF.

       2000-FIN-PROCESO.
      *-----------------
           EXIT.

       2100-GRABAR.
      *-------------
           WRITE REGNUSUARIO FROM REG_N_USUA.

       2100-FIN-GRABAR.
      *----------------
           EXIT.

       3000-FIN.
      *---------
           PERFORM 3100-CERRAR-FICHEROS THRU 3100-FIN-CERRAR-FICHEROS

           PERFORM 3200-RENOMBRAR       THRU 3200-FIN-RENOMBRAR.

           STOP RUN.

       3000-FIN-FIN.
      *-------------
           EXIT.

       3100-CERRAR-FICHEROS.
      *---------------------
           CLOSE   USUARIO
                   ALTA
                   N-USUARIO.

       3100-FIN-CERRAR-FICHEROS.
      *------------------------
           EXIT.

       3200-RENOMBRAR.
      *----------------
           OPEN INPUT N-USUARIO
               OUTPUT USUARIO
           SORT SORT-USUARIES
                ON ASCENDING KEY SORT-REGUSUA-CODIGO

                WITH DUPLICATES IN ORDER USING N-USUARIO
                GIVING USUARIO.
           CLOSE N-USUARIO
                 USUARIO.

       3200-FIN-RENOMBRAR.
      *----------------
           EXIT.

       9000-LEER-USUARIO.
      *-------------------
           READ USUARIO INTO REGUSUA
                        AT END MOVE "SI" TO SW-EOF-USUARIO
           END-READ.
           IF  SW-EOF-USUARIO = "SI"
               MOVE HIGH-VALUES TO REGUSUA
           END-IF.

       9000-FIN-LEER-USUARIO.
      *----------------------
           EXIT.

       9100-LEER-ALTA.
      *----------------
           READ ALTA INTO REGUSU_ALTA
                        AT END MOVE "SI" TO SW-EOF-ALTA
           END-READ.

       9100-FIN-LEER-ALTA.
      *-------------------
           EXIT.

       END PROGRAM ACT_ALTA_USUARIO.
