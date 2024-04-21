       ID DIVISION.
       PROGRAM-ID.  SYSPUT06.

      *-------------------------------------------------------------*
      *       SYSPUT06 - CONVERSIONE STRINGA HEX <--> DISPLAY       *
      *-------------------------------------------------------------*
      *-------------------------------------------------------------*
      * PARAMETRI:                                                  *
      *   1. FUNZIONE          - 1 BYTE   - VALORI '1' O '2'        *
      *   2. LUNGHEZZA STRINGA - 2 BYTES  - VALORI DA 1 A 10 PER F1 *
      *                                     VALORI DA 2 A 20 PER F2 *
      *   3. STRINGA           - 20 BYTES                           *
      *                                                             *
      * FUNZIONE 1                                                  *
      *   CONVERSIONE DA X'NM' IN 'NM' - ES. X'A1'   --> X'C1F1'    *
      *   LA STRINGA CONVERTITA CORRETTAMENTE VIENE MOSSA           *
      *   NEL TERZO PARAMETRO                                       *
      *                                                             *
      * FUNZIONE 2                                                  *
      *   CONVERSIONE DA 'NM' IN X'NM' - ES. X'C1F1' --> X'A1'      *
      *   LA STRINGA CONVERTITA CORRETTAMENTE VIENE MOSSA           *
      *   NEL TERZO PARAMETRO                                       *
      *                                                             *
      * IN CASO DI ERRORE                                           *
      *   IL TERZO PARAMETRO VIENE LASCIATO INALTERATO              *
      *-------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

      *---------------------------------------------------------------
        WORKING-STORAGE SECTION.
      *---------------------------------------------------------------

      * -- TABELLA BYTES HEX FORMAT
       01  HEX-BYTES.
           05 HBYTE0           PIC  X(16)
                             VALUE X'000102030405060708090A0B0C0D0E0F'.
           05 HBYTE1           PIC  X(16)
                             VALUE X'101112131415161718191A1B1C1D1E1F'.
           05 HBYTE2           PIC  X(16)
                             VALUE X'202122232425262728292A2B2C2D2E2F'.
           05 HBYTE3           PIC  X(16)
                             VALUE X'303132333435363738393A3B3C3D3E3F'.
           05 HBYTE4           PIC  X(16)
                             VALUE X'404142434445464748494A4B4C4D4E4F'.
           05 HBYTE5           PIC  X(16)
                             VALUE X'505152535455565758595A5B5C5D5E5F'.
           05 HBYTE6           PIC  X(16)
                             VALUE X'606162636465666768696A6B6C6D6E6F'.
           05 HBYTE7           PIC  X(16)
                             VALUE X'707172737475767778797A7B7C7D7E7F'.
           05 HBYTE8           PIC  X(16)
                             VALUE X'808182838485868788898A8B8C8D8E8F'.
           05 HBYTE9           PIC  X(16)
                             VALUE X'909192939495969798999A9B9C9D9E9F'.
           05 HBYTEA           PIC  X(16)
                             VALUE X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'.
           05 HBYTEB           PIC  X(16)
                             VALUE X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'.
           05 HBYTEC           PIC  X(16)
                             VALUE X'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'.
           05 HBYTED           PIC  X(16)
                             VALUE X'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'.
           05 HBYTEE           PIC  X(16)
                             VALUE X'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'.
           05 HBYTEF           PIC  X(16)
                             VALUE X'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'.
       01  FILLER REDEFINES HEX-BYTES.
           05 HEX-BYTE         PIC  X(01)  OCCURS 256.
       01  IHEX                PIC S9(04)  COMP VALUE +0.

      * -- TABELLA BYTES DISPLAY FORMAT
       01  DIS-BYTES.
           05 DBYTE0           PIC  X(32)
                             VALUE  '000102030405060708090A0B0C0D0E0F'.
           05 DBYTE1           PIC  X(32)
                             VALUE  '101112131415161718191A1B1C1D1E1F'.
           05 DBYTE2           PIC  X(32)
                             VALUE  '202122232425262728292A2B2C2D2E2F'.
           05 DBYTE3           PIC  X(32)
                             VALUE  '303132333435363738393A3B3C3D3E3F'.
           05 DBYTE4           PIC  X(32)
                             VALUE  '404142434445464748494A4B4C4D4E4F'.
           05 DBYTE5           PIC  X(32)
                             VALUE  '505152535455565758595A5B5C5D5E5F'.
           05 DBYTE6           PIC  X(32)
                             VALUE  '606162636465666768696A6B6C6D6E6F'.
           05 DBYTE7           PIC  X(32)
                             VALUE  '707172737475767778797A7B7C7D7E7F'.
           05 DBYTE8           PIC  X(32)
                             VALUE  '808182838485868788898A8B8C8D8E8F'.
           05 DBYTE9           PIC  X(32)
                             VALUE  '909192939495969798999A9B9C9D9E9F'.
           05 DBYTEA           PIC  X(32)
                             VALUE  'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'.
           05 DBYTEB           PIC  X(32)
                             VALUE  'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'.
           05 DBYTEC           PIC  X(32)
                             VALUE  'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'.
           05 DBYTED           PIC  X(32)
                             VALUE  'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'.
           05 DBYTEE           PIC  X(32)
                             VALUE  'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'.
           05 DBYTEF           PIC  X(32)
                             VALUE  'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'.
       01  FILLER REDEFINES DIS-BYTES.
           05 DIS-BYTE         PIC  X(02)  OCCURS 256.
       01  IDIS                PIC S9(04)  COMP VALUE +0.

      * -- COMODI

       01  SW-PARM             PIC  X(01).
           88 PARM-KO                      VALUE '0'.
           88 PARM-OK                      VALUE '1'.

       01  SW-ELAB             PIC  X(01).
           88 ELAB-KO                      VALUE '0'.
           88 ELAB-OK                      VALUE '1'.

       01  SW-SEARCH           PIC  X(01).
           88 SEARCH-RESET                 VALUE '0'.
           88 SEARCH-OK                    VALUE '1'.

       01  IX                  PIC S9(04)  COMP VALUE +0.
       01  IX-MAX              PIC S9(04)  COMP VALUE ZERO.

       01  W-HEX-AREA.
           03 W-HEX-BYTE       PIC  X(01)  OCCURS 20.

       01  W-DIS-AREA.
           03 W-DIS-BYTE       PIC  X(02)  OCCURS 10.

      *---------------------------------------------------------------
        LINKAGE SECTION.
      *---------------------------------------------------------------

       01  PARM-FUN            PIC  X(01).
       01  PARM-LEN            PIC S9(04)  COMP.
       01  PARM-AREA           PIC  X(20).

      ********************************************************
       PROCEDURE DIVISION USING PARM-FUN PARM-LEN PARM-AREA.
      ********************************************************

      * -- PULIZIA
           MOVE SPACE                    TO W-HEX-AREA
                                            W-DIS-AREA
                                            SW-PARM
                                            SW-ELAB
                                            SW-SEARCH.

           MOVE ZERO                     TO IX
                                            IX-MAX
                                            IHEX
                                            IDIS.

      * -- CONTROLLO PARAMETRI
           PERFORM 010-CTRL-PARMS

           IF PARM-OK
              SET ELAB-OK                TO TRUE
              EVALUATE PARM-FUN
      * -- FUNZIONE 1 - CONVERSIONE DA HEX A DISPLAY
                  WHEN '1'
                     MOVE PARM-AREA      TO W-HEX-AREA
                     MOVE PARM-LEN       TO IX-MAX
                     MOVE SPACE          TO W-DIS-AREA
                     PERFORM 100-CERCA-HEX
                             VARYING IX FROM 1 BY 1
                               UNTIL IX > IX-MAX
                                  OR ELAB-KO
                     IF ELAB-OK
                        MOVE W-DIS-AREA  TO PARM-AREA
                     ELSE
                        MOVE ALL '-'     TO PARM-AREA
                     END-IF
      * -- FUNZIONE 2 - CONVERSIONE DA DISPLAY A HEX
                  WHEN '2'
                     MOVE PARM-AREA      TO W-DIS-AREA
                     COMPUTE IX-MAX ROUNDED = PARM-LEN / 2
                     MOVE SPACE          TO W-HEX-AREA
                     PERFORM 200-CERCA-DIS
                             VARYING IX FROM 1 BY 1
                               UNTIL IX > IX-MAX
                                  OR ELAB-KO
                     IF ELAB-OK
                        MOVE W-HEX-AREA  TO PARM-AREA
                     ELSE
                        MOVE ALL '-'     TO PARM-AREA
                     END-IF
              END-EVALUATE
           END-IF.

           GOBACK.


      * -- CONTROLLO PARAMETRI
      * -- PER LA FUNZ. '1' LA LUNGHEZZA NON PUO' SUPERARE I 10 CHARS
      * -- PER LA FUNZ. '2' LA LUNGHEZZA NON PUO' SUPERARE I 20 CHARS
       010-CTRL-PARMS.
           EVALUATE PARM-FUN ALSO PARM-LEN
               WHEN '1'      ALSO 1 THRU 10
               WHEN '2'      ALSO 2 THRU 20
                  SET PARM-OK            TO TRUE
               WHEN OTHER
                  SET PARM-KO            TO TRUE
                  MOVE ALL '*'           TO PARM-AREA
           END-EVALUATE.


      * -- CICLO DI TRASCODIFICA HEX --> DISPLAY
       100-CERCA-HEX.
           SET  SEARCH-RESET             TO TRUE.
           PERFORM VARYING IHEX FROM 1 BY 1
                     UNTIL IHEX > 256
                        OR SEARCH-OK
              IF HEX-BYTE(IHEX) = W-HEX-BYTE(IX)
                 SET  SEARCH-OK          TO TRUE
                 MOVE DIS-BYTE(IHEX)     TO W-DIS-BYTE(IX)
              END-IF
           END-PERFORM.

           IF NOT SEARCH-OK
              SET ELAB-KO                TO TRUE
              MOVE 'XX'                  TO W-DIS-BYTE(IX)
           END-IF.


      * -- CICLO DI TRASCODIFICA DISPLAY --> HEX
       200-CERCA-DIS.
           SET  SEARCH-RESET             TO TRUE.
           PERFORM VARYING IDIS FROM 1 BY 1
                     UNTIL IDIS > 256
                        OR SEARCH-OK
              IF DIS-BYTE(IDIS) = W-DIS-BYTE(IX)
                 SET  SEARCH-OK          TO TRUE
                 MOVE HEX-BYTE(IDIS)     TO W-HEX-BYTE(IX)
              END-IF
           END-PERFORM.

           IF NOT SEARCH-OK
              SET ELAB-KO                TO TRUE
              MOVE X'FF'                 TO W-HEX-BYTE(IX)
           END-IF.

      * -- SYSPUT06 - END PROG *---------------------------------------*
