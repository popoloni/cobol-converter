       IDENTIFICATION DIVISION.
       PROGRAM-ID. WSOC83.
      * -------------------------------------------------------------- *
      * -------------------------------------------------------------- *
      *                       PROGETTO MEF                             *
      *                       ============                             *
      *                                                                *
      *  PROGRAMMA: WSOC83 - GESTIONE DEI MESSAGGI ANS PROVENIENTI     *
      *                      DA MITX RELATIVI ALLE ISTRUZIONI DI       *
      *                      STRIPPING.                                *
      *  TIPOLOGIA: RETE (TRANSAZIONALE) COBOL-CICS-DB2                *
      *                                                                *
      * -------------------------------------------------------------- *

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           CLASS NUMERICA   IS '0' THRU '9'
           CLASS ALFABETICA IS 'A' THRU 'Z'
           CLASS ALFANUM    IS 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY K2SIWRKC.
           COPY WORKERR.

      * COPY TESTATA MESSAGGIO
       01 WS-MSG-TEST.
           COPY WSC01I.

      * COPY BODY MESSAGGIO
       01 WS-MSG-CORP.
           COPY WSC83I.

       77  FILLER                        PIC X(16) VALUE
                                         'INIZIO WORKING->'.
       77  W-NOME-MODULO                 PIC X(08) VALUE SPACES.
       77  W-CODA-TD                     PIC 9(01) VALUE ZERO.
           88 NO-FINE-CODA-TD                      VALUE ZERO.
           88 FINE-CODA-TD                         VALUE 1.
       77  W-LETTURA-TD                  PIC 9(01) VALUE ZERO.
           88 LETTURA-TD-DONE                      VALUE 1.
       77  MTAUT320                      PIC  X(08)  VALUE 'MTAUT320'.
       01  AREA-MTAUT320.
            05 R-YNAZTIT.
               10 R-YNAZIONE        PIC  X(02).
               10 R-YSPECIE         PIC  X(09).
            05 R-YCHKDIG            PIC  9(01).
            05 R-YFUNZ              PIC  X(01).
       01   WS-CHKDGTIT             PIC  9(01).
      *
       01  W-LETTURE-CUR-PDA         PIC X VALUE SPACE.
           88 PRIMA-LETT-CUR-PDA     VALUE 'S'.
           88 NO-PRIMA-LETT-CUR-PDA VALUE 'N'.
      *
       01  W-CUR-PDA                 PIC X VALUE ZERO.
           88 NO-FINE-CUR-PDA        VALUE ZERO.
           88 FINE-CUR-PDA           VALUE '1'.
      *
      * SWITCH INDICATORE DI TITOLO SHORT LONG
NEWC   01  SW-SL                     PIC X VALUE ZERO.
NEWC       88 SW-SL-NO               VALUE 'N'.
NEWC       88 SW-SL-SI               VALUE 'S'.
      *
       01  W-STATO-TAF               PIC X VALUE ZERO.
           88 TAF-OK                 VALUE ZERO.
           88 TAF-KO                 VALUE '1'.
      *
       01  W-CNTR-TATS               PIC X VALUE ZERO.
           88 CNTR-TATS-OK           VALUE ZERO.
           88 CNTR-TATS-KO           VALUE '1'.
      *
       01  W-STATO-IAT               PIC X VALUE ZERO.
           88 IAT-OK                 VALUE ZERO.
           88 IAT-KO                 VALUE '1'.
      *
      *
       01  W-STATO-WRS               PIC X VALUE ZERO.
           88 WRS-OK                 VALUE ZERO.
           88 WRS-KO                 VALUE '1'.
      *
       01  W-STATO-WSL               PIC X VALUE ZERO.
           88 WSL-OK                 VALUE ZERO.
           88 WSL-KO                 VALUE '1'.
      *
       01  W-STATO-TATS              PIC X VALUE ZERO.
           88 TATS-OK                VALUE ZERO.
           88 TATS-KO                VALUE '1'.
      *
       01  W-STATO-TA-TAF            PIC X VALUE ZERO.
           88 TA-TAF-OK              VALUE ZERO.
           88 TA-TAF-KO              VALUE '1'.
      *
       01  W-STATO-CED-PIANO         PIC X VALUE ZERO.
           88 CED-PIANO-OK           VALUE ZERO.
           88 CED-PIANO-KO           VALUE '1'.
      *
       01  W-TRATTA-DP               PIC X VALUE ZERO.
           88 TRATTA-DP-OK           VALUE ZERO.
           88 TRATTA-DP-KO           VALUE '1'.
      *
       01  PARAMETRI.
           05 FUNZIONE            PIC X     VALUE '1'.
           05 LUNGHEZZA           PIC S9(4) COMP.
           05 IOAREA              PIC X(20).
       01  W-PARM-ROUTE.
           05  W-TERMID           PIC X(4).
           05  FILLER             PIC X(12) VALUE SPACES.
           05  FILLER             PIC S9(4) COMP VALUE -1.
       01 W-AREA-KEYMSG-WS83.
           03 W-IDN-RICH-MSG       PIC 9(10).
           03 W-IDN-TIPO-ESEC      PIC X(01).
               88 I-ESECUZ             VALUE '1'.
               88 RIESECUZ             VALUE '2'.
               88 ESECUZ-DIFF          VALUE '3'.
           03 FILLER               PIC X(09).
       01 W-AREA-KEYMSG-WS22.
           03 W-IDN-RICH-MSG       PIC X(10).
           03 W-IDN-TIPO-ESEC      PIC X(01).
           03 FILLER               PIC X(09).
      *
      *----------------------------------------------------------------*
           COPY RIC00W.
      *
      * -- AREA DA PASSARE A ARMP3025 PER ARCHIVIAZIONE LOG-ALLARMI
       01  FILLER                  PIC X(16) VALUE 'START ARE33I--->'.
       01  AREA-ARMP3025.
           COPY ARE33I.
           02 ARMP3025-COD-RIT PIC XX.
       01  FILLER                  PIC X(16) VALUE 'START WK.SEGN-->'.
       01  W-SEGNALAZIONI.
           02  FILLER                  PIC XXX    VALUE SPACES.
           02  W-SEGN-DATA             PIC XX/XX/XX.
           02  FILLER                  PIC XXX    VALUE SPACES.
           02  W-SEGN-ORA              PIC X(8)   VALUE '  .  .  '.
           02  W-SEGN-ORA-R REDEFINES W-SEGN-ORA.
               05  W-SEGN-ORA-HH       PIC XX.
               05  FILLER              PIC X.
               05  W-SEGN-ORA-MM       PIC XX.
               05  FILLER              PIC X.
               05  W-SEGN-ORA-SS       PIC XX.
           02  FILLER                  PIC X(11)  VALUE '   TRANSID='.
           02  W-SEGN-TRANSID          PIC XXXX.
           02  FILLER                  PIC X(17)  VALUE '   CAT.APPL ='.
           02  W-SEGN-CATAPP           PIC X(04).
           02  FILLER                  PIC X(13)  VALUE '   TASK N.RO='.
           02  W-SEGN-TASKN            PIC 9(7).
           02  FILLER                  PIC X(3)   VALUE SPACES.
           02  FILLER                  PIC X(80)  VALUE SPACES.
           02  W-TAB-SEGNALAZIONI      PIC X(800) VALUE SPACES.
           02  FILLER REDEFINES W-TAB-SEGNALAZIONI.
               05  FILLER OCCURS 10.
                   10  FILLER          PIC X(05).
                   10  W-SEGN-RIGA     PIC X(50).
                   10  FILLER          PIC X(25).
      *
       01  W-COSTANTI.
           03  W-PRGNAME                     PIC X(8)  VALUE 'WSOC83'.
           03  W-ID-CODA-TD-WS83             PIC X(04) VALUE 'WS83'.
           03  W-ID-CODA-TD-WS22             PIC X(04) VALUE 'WS22'.
           03  W-ID-CODA-TS-ER.
               04 FILLER                     PIC X(01) VALUE 'M'.
               04 W-ID-CODA-TS-TASK          PIC X(07).
           03  W-FINE-ANOMALA                PIC X(8)  VALUE '3'.
           03  W-TIMESTAMP-DEFAULT           PIC X(26) VALUE
              '9999-12-31-23.59.59.999999'.
      *
      *---------------------------------------------------------------
      * TABELLA PER SALVARE LE DATE DEL PIANO DI AMMORTAMENTO
      *---------------------------------------------------------------
       01  IND-DP-MAX               PIC 9(04)    VALUE 1000.
       01  IND-DP                   PIC 9(04)    VALUE 0.
       01  TAB-DP.
           03  EL-TAB-DP         OCCURS 1000.
               05 TAB-DPAGCES       PIC S9(8)V USAGE COMP-3.
NEWC           05 TAB-QTASINT       PIC S9(2)V9(5) USAGE COMP-3.
      *
      *---------------------------------------------------------------
      *  VARIABILI
      *---------------------------------------------------------------
       01  W-VARIABILI.
           03  W-TAB-DPAGCES                 PIC S9(8)V USAGE COMP-3.
           03  W-SQLCODE                     PIC ---9.
           03  W-FINE                        PIC X(1)  VALUE SPACE.
               88 FINE-ABEND                 VALUE 'A'.
               88 FINE-REGOLARE              VALUE ' '.
           03  W-CUR-WSRR                    PIC S9(9)V COMP-3.
               88  CUR-WSRR-OK               VALUE ZEROES.
               88  CUR-WSRR-NF               VALUE +100.
           03  W-TABELLA-ERRORI.
               05 W-ELE-IDC-ERR OCCURS 5.
                  07 W-ELE-IDC               PIC X(003).
                  07 W-ELE-ERR               PIC X(004).
           03  IND-ERR                       PIC S9(004) COMP.
           03  W-IDC                         PIC X(003).
           03  W-ERR                         PIC X(004).
           03  W-CICLO-ERRORI                PIC X(01) VALUE SPACES.
               88 ENTRA-CICLO-ERRORI                   VALUE 'I'.
               88 ESCI-CICLO-ERRORI                    VALUE 'F'.
           03  W-DESCR1                      PIC X(50) VALUE SPACES.
           03  W-DESCR2                      PIC X(50) VALUE SPACES.
           03  W-DESCR3                      PIC X(50) VALUE SPACES.
           03  W-CONTROLLI                   PIC X(01) VALUE ZERO.
               88 CONTROLLI-FORM-KO          VALUE '1'.
               88 CONTROLLI-APPL-KO          VALUE '2'.
               88 CONTROLLI-KO               VALUE '1' '2'.
               88 CONTROLLI-OK               VALUE '0'.
            03  W-EIBFN                PIC X(04) VALUE ZERO.
            03  W-EIBRCODE             PIC X(12) VALUE ZERO.
            03  W-DATA-APPOGGIO        PIC 9(8)  VALUE ZEROES.
            03  W-RESP                 PIC S9(8) COMP.
            03  W-IND-ERRORI                PIC S9(4) COMP.
            03  W-MAX-ERRORI                PIC S9(4) COMP VALUE +2480.
            03  W-IND-APP                   PIC S9(4) COMP.
            03  W-PIDMTX-X                  PIC X(10).
            03  W-DATA-IPL.
                05 W-DATA-IPL-SS            PIC X(02).
                05 W-DATA-IPL-AAMMGG.
                   10 W-DATA-IPL-AA         PIC X(02).
                   10 W-DATA-IPL-MM         PIC X(02).
                   10 W-DATA-IPL-GG         PIC X(02).
      *
            03  W-CONTA-CED                  PIC 9(04)  VALUE 0.
            03  W-SALVA-IND-DP               PIC 9(04)  VALUE 0.
NEWC        03  W-QTASINT                   PIC S9(2)V9(5) USAGE COMP-3.
            03  W-CSTRIP                     PIC X(01)  VALUE SPACES.
            03  W-CNAZTIT-CED                PIC X(02)  VALUE SPACES.
            03  W-CSPTIT-CED                 PIC X(09)  VALUE SPACES.
            03  W-DGIOR                      PIC S9(8)V USAGE COMP-3.
            03  W-IND-IDC                    PIC X(004) VALUE SPACES.
            03  W-APPOGGIO-F04.
                05  W-ISIN-F04.
                   10  W-CNAZTIT-F04         PIC X(02)  VALUE SPACES.
                   10  W-CSPTIT-F04          PIC X(09)  VALUE SPACES.
                   10  W-CHKDGTIT-F04        PIC 9(01)  VALUE ZEROES.
                05  W-CTMAUP-F04             PIC X(01)  VALUE SPACES.
                05  W-CDIVEM-F04             PIC X(03)  VALUE SPACES.
                05  W-CTAMM-F04              PIC X(02)  VALUE SPACES.
                05  W-CTINTER-F04            PIC X(01)  VALUE SPACES.
                05  W-CINDICIZ-F04           PIC X(01)  VALUE SPACES.
                05  W-ICACTDS-F04            PIC X(01)  VALUE SPACES.
                05  W-DFIOPER-F04            PIC S9(8)V USAGE COMP-3.
                05  W-CSTRIP-F04             PIC X(01)  VALUE SPACES.
            03  W-APPOGGIO-AS4.
                05  W-ISIN-PADRE-AS4.
                   10  W-CNAZTIT-PADRE-AS4   PIC X(02)  VALUE SPACES.
                   10  W-CSPTIT-PADRE-AS4    PIC X(09)  VALUE SPACES.
                   10  W-CHKDGTIT-PADRE-AS4  PIC 9(01)  VALUE ZEROES.
                05  W-CDIVEM-AS4             PIC X(03)  VALUE SPACES.
                05  W-CINDICIZ-AS4           PIC X(01)  VALUE SPACES.
                05  W-ICACTDS-AS4            PIC X(01)  VALUE SPACES.
                05  W-DFIOPER-AS4            PIC S9(8)V USAGE COMP-3.
                05  W-CTAMM-AS4              PIC X(02)  VALUE SPACES.
                05  W-CTINTER-AS4            PIC X(01)  VALUE SPACES.
            03 W-APPOGGIO-AS7.
                05  W-ISIN-CIBR-AS7.
                   10  W-CNAZTIT-CIBR-AS7      PIC X(02)  VALUE SPACES.
                   10  W-CSPTIT-CIBR-AS7       PIC X(09)  VALUE SPACES.
                   10  W-CHKDGTIT-CIBR-AS7     PIC X(09)  VALUE SPACES.
      *
      *-------------------------------------------------------------*
      * -- AREE PASSAGGIO DATI AL JOB DI STAMPA
      *-------------------------------------------------------------*
       01 W-PAR-83.
         05 W-PAR-83-CNAZTIT             PIC X(02).
         05 W-PAR-83-CSPTIT              PIC X(09).
         05 W-PAR-83-CHKDGTIT            PIC 9(01).
         05 W-PAR-83-CINDICIZ            PIC X(01).
         05 W-PAR-83-ICACTDS             PIC X(01).
         05 FILLER                       PIC X(66).
      *
      *-------------------------------------------------------------*
      * -- AREE PER RICHIESTA ESECUZIONE JOB
      *-------------------------------------------------------------*
       01  PGM-RNOC14                   PIC  X(08) VALUE 'RNOC14  '.
           COPY RNC14I.
      *
      *-------------------------------------------------------------*
      * -- TABELLE E CAMPI PER DB2
      *-------------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA  END-EXEC.
           EXEC SQL INCLUDE DGC    END-EXEC.
           EXEC SQL INCLUDE WSRR   END-EXEC.
           EXEC SQL INCLUDE WSRS   END-EXEC.
           EXEC SQL INCLUDE WSL    END-EXEC.
           EXEC SQL INCLUDE BDTA   END-EXEC.
           EXEC SQL INCLUDE TA     END-EXEC.
           EXEC SQL INCLUDE TAF    END-EXEC.
           EXEC SQL INCLUDE TATS   END-EXEC.
           EXEC SQL INCLUDE IAM    END-EXEC.
           EXEC SQL INCLUDE IAT    END-EXEC.
           EXEC SQL INCLUDE CPPO   END-EXEC.
NEWC       EXEC SQL INCLUDE PPO    END-EXEC.
      *
       01  SICD-ACCDATE     PIC X(6) VALUE SPACES.
       01  SICD-ABS-TIME    PIC S9(7) COMP-3    VALUE ZERO.
      *
      * CURSORI DB2
      *
      *----SONO DA ELABORARE I MSG PER I QUALI CVALMSG SIA ' '
           EXEC SQL DECLARE CUR-WSRR CURSOR WITH HOLD FOR
                SELECT WTIMECAR
                      ,CMSGMTX
                      ,CMITT
                      ,CFAMAPP
                      ,CAMBOP
                      ,CCATAPPL
                      ,CPRIOMS
                      ,XERRMTX
                      ,CTSOGMTX
                      ,CVETT
                      ,CESMTX
                      ,CVALMSG
                      ,XTESTMTX
                      ,XMSGRISE
                      ,PPROG
                  FROM WSRR
                 WHERE PIDMTX   = :DCLWSRR.PIDMTX
                   AND CVALMSG IN (' ')
           ORDER BY PIDMTX, PPROG
           WITH UR
           END-EXEC.
      *
      *----LETTURA DEL PIANO DI AMMORTAMENTO PER SALVARE LE DATE
      *----DPAGCES E QTASINT IN TABELLA DI WORKING
           EXEC SQL DECLARE CUR-PDA CURSOR WITH HOLD FOR
                SELECT A.DPAGCES
NEWC                 , QTASINT
                  FROM CPPO  A
NEWC                 , PPO   B
NEWC             WHERE A.CNAZTIT  = B.CNAZTIT
NEWC               AND A.CSPTIT   = B.CSPTIT
NEWC               AND A.DPAGCES  = B.DPAGCES
                   AND A.CNAZTIT  = :DCLCPPO.CNAZTIT
                   AND A.CSPTIT   = :DCLCPPO.CSPTIT
                   AND A.CTCESPAG = :DCLCPPO.CTCESPAG
                   AND A.DPAGCES  > :W-DGIOR
           ORDER BY DPAGCES
           WITH UR
           END-EXEC.
      *
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
      *
           PERFORM OPERAZIONI-INIZIALI.
      *
           PERFORM ELABORA
             UNTIL FINE-CODA-TD.
      *
           PERFORM OPERAZIONI-FINALI.
      *
           GOBACK.
      *----------------------------------------------------------------*
       OPERAZIONI-INIZIALI.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - OPERAZIONI-INIZIALI         '.
      *

           EXEC CICS
                HANDLE   ABEND   LABEL (FINE-ANOMALA)
           END-EXEC.

           EXEC CICS
                HANDLE CONDITION ERROR (FINE-ANOMALA)
           END-EXEC.


           PERFORM SEGNALA-MONITOR-APRE-ATT.

           PERFORM LETTURA-DATA-CONTABILE.

           PERFORM LETTURA-CODA-TD-WS83.

           SET CONTROLLI-OK TO TRUE.

      *----------------------------------------------------------------*
       LETTURA-DATA-CONTABILE.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - LETTURA-DATA-CONTABILE      '.
      *
           EXEC SQL
                SELECT DGIOR
                  INTO :DCLDGC.DGIOR
                  FROM DGC
                WITH UR
           END-EXEC.

           EVALUATE SQLCODE
               WHEN ZERO
                    MOVE DGIOR OF DCLDGC       TO W-DGIOR
TEST                DISPLAY 'W-DGIOR = '          W-DGIOR
               WHEN OTHER
                    MOVE DGIOR OF DCLDGC       TO W-DGIOR
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2- LETTURA TAB. DGC   ' TO W-DESCR1
                    MOVE W-DGIOR                           TO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.

           PERFORM SICD001P-IMPOSTA-ACCDATE
           MOVE    SICD-ACCDATE
           TO      W-DATA-IPL-AAMMGG.
           IF W-DATA-IPL-AA NOT LESS  THAN  W-SI-PIVOT
              MOVE 19 TO W-DATA-IPL-SS
           ELSE
              MOVE 20 TO W-DATA-IPL-SS
           END-IF.

      *----------------------------------------------------------------*
       LETTURA-CODA-TD-WS83.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - LETTURA-CODA-TD-WS83        '.
      *
           SET LETTURA-TD-DONE TO TRUE.

           EXEC CICS
                READQ  TD QUEUE(W-ID-CODA-TD-WS83)
                INTO           (W-AREA-KEYMSG-WS83)
                LENGTH         (LENGTH OF W-AREA-KEYMSG-WS83)
                RESP           (W-RESP)
           END-EXEC.

           IF W-RESP = DFHRESP(NORMAL)
           OR W-RESP = DFHRESP(LENGERR)
              ADD 1 TO W-NUM-MSG-ELAB
           ELSE
              IF W-RESP = DFHRESP(QZERO)
                 SET FINE-CODA-TD   TO TRUE
              ELSE
                 INITIALIZE W-DESCR1 W-DESCR2
                 MOVE 'ERRORE LETTURA CODA TD WS83' TO W-DESCR1
                 PERFORM CONSOLIDAMENTO-DATI
                 PERFORM FINE-ANOMALA
              END-IF
           END-IF.

      *    -- ELIMINA DATI DA CODA DI TD
           PERFORM CONSOLIDAMENTO-DATI.


      *----------------------------------------------------------------*
       SCRITTURA-CODA-TD-WS22.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - SCRITTURA-CODA-TD-WS22      '.
      *
           MOVE W-IDN-RICH-MSG OF  W-AREA-KEYMSG-WS83
                               TO W-IDN-RICH-MSG  OF W-AREA-KEYMSG-WS22.
           MOVE '1'            TO W-IDN-TIPO-ESEC OF W-AREA-KEYMSG-WS22.

           EXEC CICS
                WRITEQ TD QUEUE(W-ID-CODA-TD-WS22)
                FROM           (W-AREA-KEYMSG-WS22)
                LENGTH         (LENGTH OF W-AREA-KEYMSG-WS22)
                RESP           (W-RESP)
           END-EXEC.

           IF W-RESP = DFHRESP(NORMAL)
              CONTINUE
           ELSE
              INITIALIZE W-DESCR1 W-DESCR2
              MOVE 'ERRORE SCRITTURA CODA TD WS22' TO W-DESCR1
              PERFORM FINE-ANOMALA
           END-IF.

      *----------------------------------------------------------------*
       ELABORA.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - ELABORA                     '.
      *
           INITIALIZE W-IND-ERRORI.
           MOVE SPACE                TO W-TABELLA-ERRORI.
           SET WSL-OK                TO TRUE.
           SET CONTROLLI-OK          TO TRUE.
           MOVE EIBTASKN             TO W-ID-CODA-TS-TASK.
      *
           PERFORM CONTROLLO-AREA-DATI-TD.
      *
           PERFORM APERTURA-CUR-WSRR.
      *
           PERFORM LETTURA-TABELLA-WSRR.
           IF SQLCODE = +100
              INITIALIZE W-DESCR1 W-DESCR2
              MOVE 'NON TROVATO IN TABELLA WSRR' TO W-DESCR1
              MOVE PIDMTX OF DCLWSRR             TO W-PIDMTX-X
              STRING 'PIDMTX: ' W-PIDMTX-X
              DELIMITED BY SIZE                INTO W-DESCR2
              PERFORM FINE-ANOMALA
           END-IF.
      *
           PERFORM ELABORA-WSRR
             UNTIL CUR-WSRR-NF
      *
           PERFORM CHIUSURA-CUR-WSRR.
      *
           IF W-CONTROLLI = 0
              PERFORM PREPARA-INVIO-MAIL
           END-IF.
      *
           PERFORM AGGIORNAMENTI-ARCHITETTURALI.
      *
      *
           PERFORM LETTURA-CODA-TD-WS83.
      *
      *----------------------------------------------------------------*
       CONTROLLO-AREA-DATI-TD.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLO-AREA-DATI-TD      '.
      *
           IF W-AREA-KEYMSG-WS83  = SPACES OR LOW-VALUE
              INITIALIZE W-DESCR1 W-DESCR2
              MOVE 'AREA DATI TD WS83 NON VALORIZZATA '  TO W-DESCR1
              PERFORM FINE-ANOMALA
           END-IF.
      *
      *----------------------------------------------------------------*
       APERTURA-CUR-WSRR.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - APERTURA-CUR-WSRR           '.
      *
           INITIALIZE W-CUR-WSRR.
           MOVE W-IDN-RICH-MSG OF  W-AREA-KEYMSG-WS83
             TO PIDMTX  OF DCLWSRR.
      *
           EXEC SQL
                OPEN CUR-WSRR
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN OTHER
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-OPEN CUR WSRR' TO W-DESCR1
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       LETTURA-TABELLA-WSRR.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - LETTURA-TABELLA-WSRR        '.
      *
           EXEC SQL
                FETCH  CUR-WSRR
                 INTO :DCLWSRR.WTIMECAR
                     ,:DCLWSRR.CMSGMTX
                     ,:DCLWSRR.CMITT
                     ,:DCLWSRR.CFAMAPP
                     ,:DCLWSRR.CAMBOP
                     ,:DCLWSRR.CCATAPPL
                     ,:DCLWSRR.CPRIOMS
                     ,:DCLWSRR.XERRMTX
                     ,:DCLWSRR.CTSOGMTX
                     ,:DCLWSRR.CVETT
                     ,:DCLWSRR.CESMTX
                     ,:DCLWSRR.CVALMSG
                     ,:DCLWSRR.XTESTMTX
                     ,:DCLWSRR.XMSGRISE
                     ,:DCLWSRR.PPROG
           END-EXEC.
      *
           MOVE SQLCODE TO W-CUR-WSRR.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    MOVE XTESTMTX      OF DCLWSRR  TO WS-MSG-TEST
                    MOVE XMSGRISE-TEXT OF DCLWSRR  TO WS-MSG-CORP
               WHEN +100
                    CONTINUE
               WHEN OTHER
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-LETTURA TABELLA WSRR' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR         TO W-PIDMTX-X
                    STRING 'PIDMTX: '                 W-PIDMTX-X
                    DELIMITED BY SIZE            INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       CHIUSURA-CUR-WSRR.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CHIUSURA-CUR-WSRR           '.
      *
           EXEC SQL
                CLOSE CUR-WSRR
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN OTHER
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-CLOSE CUR WSRR' TO W-DESCR1
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       ELABORA-WSRR.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - ELABORA-WSRR                '.
TEST       DISPLAY ' WSC83I-ISIN   = '  WSC83I-ISIN.
TEST       DISPLAY ' WSC83I-CSTRIP = '  WSC83I-CSTRIP.
      *
TEST       DISPLAY 'W-CONTROLLI ANTE CONTROLLI-FORMALI  ' W-CONTROLLI.
           IF CONTROLLI-OK
               PERFORM CONTROLLI-FORMALI
           END-IF.
      *
TEST       DISPLAY 'W-TABELLA-ERRORI     = ' W-TABELLA-ERRORI
TEST  *    DISPLAY 'W-SEGN-RIGA (1)      = ' W-SEGN-RIGA (1)
TEST  *    DISPLAY 'W-SEGN-RIGA (2)      = ' W-SEGN-RIGA (2)
TEST  *    DISPLAY 'W-SEGN-RIGA (3)      = ' W-SEGN-RIGA (3)
TEST  *    DISPLAY 'W-SEGN-RIGA (4)      = ' W-SEGN-RIGA (4)
TEST  *    DISPLAY 'W-SEGN-RIGA (5)      = ' W-SEGN-RIGA (5)
TEST  *    DISPLAY 'W-SEGN-RIGA (6)      = ' W-SEGN-RIGA (6)
TEST  *    DISPLAY 'W-SEGN-RIGA (7)      = ' W-SEGN-RIGA (7)
TEST  *    DISPLAY 'W-SEGN-RIGA (8)      = ' W-SEGN-RIGA (8)
TEST  *    DISPLAY 'W-SEGN-RIGA (9)      = ' W-SEGN-RIGA (9)
TEST  *    DISPLAY 'W-SEGN-RIGA (10)     = ' W-SEGN-RIGA (10)
TEST  *    DISPLAY 'W-CONTROLLI POST CONTROLLI-FORMALI   ' W-CONTROLLI.
           IF CONTROLLI-OK
               PERFORM CONTROLLI-APPLICATIVI
           END-IF.
      *
TEST       DISPLAY 'W-TABELLA-ERRORI     = ' W-TABELLA-ERRORI
           IF CONTROLLI-OK
               PERFORM AGGIORNAMENTI-APPLICATIVI
           END-IF.
      *
           PERFORM LETTURA-TABELLA-WSRR.
      *
      *----------------------------------------------------------------*
       CONTROLLI-FORMALI.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-FORMALI           '.
      *
      * IDC.F04   CODICE ISIN
           PERFORM CONTROLLI-F-TITOLO.

      * IDC.AS2   PERC. MASSIMA STRIPPABILE
           IF WSC83I-QPMSTRIPX NOT = LOW-VALUE
              IF WSC83I-QPMSTRIPX NOT NUMERIC
      *-----  07026.VALORE NON AMMESSO
                 SET CONTROLLI-FORM-KO TO TRUE
                 MOVE 'AS2'            TO W-IDC
                 MOVE '7026'           TO W-ERR
                 PERFORM MEMORIZZA-ERRORE
              END-IF
           END-IF.
      *
      * IDC.AS3   LOTTO MINIMO ASSOGGETTABILE
           IF WSC83I-QLMSTRIPX NOT = LOW-VALUE
              IF WSC83I-QLMSTRIP   IS NOT NUMERIC
      *-----  07026.VALORE NON AMMESSO
                 SET CONTROLLI-FORM-KO TO TRUE
                 MOVE 'AS3'            TO W-IDC
                 MOVE '7026'           TO W-ERR
                 PERFORM MEMORIZZA-ERRORE
              END-IF
           END-IF.
      *
      * IDC.AS4   CODICE ISIN PADRE
           PERFORM CONTROLLI-F-ISIN-PADRE.
      *
      * IDC.AS5   CODICE ISIN MANTELLO
           PERFORM CONTROLLI-F-ISIN-MANT.
      *
      * IDC.AS6   CODICE ISIN MANTELLO UPLIF
           PERFORM CONTROLLI-F-ISIN-MAUP.
      *
      * IDC.AS7   CODICE ISIN CEDOLA IBRIDA
           PERFORM CONTROLLI-F-ISIN-CIBR.
      *
      * IDC.AS9  QUOTA CARTOLARE
           IF WSC83I-QQUOCAR NOT = LOW-VALUE
              IF WSC83I-QQUOCAR    IS NOT NUMERIC
      *-----  07026.VALORE NON AMMESSO
                 SET CONTROLLI-FORM-KO TO TRUE
                 MOVE 'AS9'            TO W-IDC
                 MOVE '7026'           TO W-ERR
                 PERFORM MEMORIZZA-ERRORE
               END-IF
           END-IF.
      *
      *----------------------------------------------------------------*
       CONTROLLI-F-TITOLO.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-F-TITOLO          '.
      *
           EVALUATE WSC83I-ISIN
                  WHEN SPACES
                  WHEN LOW-VALUES
      *------          0404.OBBLIGATORIO
                       SET CONTROLLI-FORM-KO TO TRUE
                       MOVE '0404'           TO W-ERR
                       MOVE 'F04'            TO W-IDC
                       PERFORM MEMORIZZA-ERRORE
                  WHEN OTHER
                       EVALUATE WSC83I-CNAZTIT
                            WHEN SPACES
                            WHEN LOW-VALUE
      *------------------------- 0404.OBBLIGATORIO
                                 SET CONTROLLI-FORM-KO TO TRUE
                                 MOVE 'F04'            TO W-IDC
                                 MOVE '0404'           TO W-ERR
                                 PERFORM MEMORIZZA-ERRORE
                            WHEN OTHER
                                 IF WSC83I-CNAZTIT        IS NOT
                                    ALFABETICA
      *---------------------------- 7026.VALORE NON AMMESSO
                                    SET CONTROLLI-FORM-KO TO TRUE
                                    MOVE 'F04'            TO W-IDC
                                    MOVE '7026'           TO W-ERR
                                    PERFORM MEMORIZZA-ERRORE
                                 END-IF
                       END-EVALUATE
                       EVALUATE WSC83I-CSPTIT
                           WHEN SPACES
                           WHEN LOW-VALUE
      *------------------------- 0404.OBBLIGATORIO
                                SET CONTROLLI-FORM-KO TO TRUE
                                MOVE 'F04'            TO W-IDC
                                MOVE '0404'           TO W-ERR
                                PERFORM MEMORIZZA-ERRORE
                           WHEN OTHER
                                 IF WSC83I-CSPTIT       IS NOT
                                    ALFANUM
      *--------------------------   0490.SPECIE TITOLO NON AMMESSA
                                    SET CONTROLLI-FORM-KO TO TRUE
                                    MOVE 'F04'            TO W-IDC
                                    MOVE '0490'           TO W-ERR
                                    PERFORM MEMORIZZA-ERRORE
                                 END-IF
                       END-EVALUATE
                       EVALUATE WSC83I-CHKDGTIT
                          WHEN SPACES
                          WHEN LOW-VALUE
      *-------------------------- 0404.OBBLIGATORIO
                                  SET CONTROLLI-FORM-KO TO TRUE
                                  MOVE 'F04'            TO W-IDC
                                  MOVE '0404'           TO W-ERR
                                  PERFORM MEMORIZZA-ERRORE
                          WHEN OTHER
                                  IF WSC83I-CHKDGTIT     IS NOT
                                     NUMERIC
      *---------------------------   07026.VALORE NON AMMESSO
                                     SET CONTROLLI-FORM-KO TO TRUE
                                     MOVE 'F04'            TO W-IDC
                                     MOVE '7026'           TO W-ERR
                                     PERFORM MEMORIZZA-ERRORE
                                  END-IF
                       END-EVALUATE
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       CONTROLLI-F-ISIN-PADRE.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-F-ISIN-PADRE      '.
      *
           EVALUATE WSC83I-ISIN-PADRE
                  WHEN SPACES
                  WHEN LOW-VALUES
                       CONTINUE
                  WHEN OTHER
                       EVALUATE WSC83I-CNAZTIT-PADRE
                            WHEN SPACES
                            WHEN LOW-VALUE
      *------------------------- 7026.OBBLIGATORIO
                                 SET CONTROLLI-FORM-KO TO TRUE
                                 MOVE 'AS4'            TO W-IDC
                                 MOVE '7026'           TO W-ERR
                                 PERFORM MEMORIZZA-ERRORE
                            WHEN OTHER
                                 IF WSC83I-CNAZTIT-PADRE  IS NOT
                                    ALFABETICA
      *---------------------------- 7026.VALORE NON AMMESSO
                                    SET CONTROLLI-FORM-KO TO TRUE
                                    MOVE 'AS4'            TO W-IDC
                                    MOVE '0490'           TO W-ERR
                                    PERFORM MEMORIZZA-ERRORE
                                 END-IF
                       END-EVALUATE
                       EVALUATE WSC83I-CSPTIT-PADRE
                           WHEN SPACES
                           WHEN LOW-VALUE
      *------------------------- 7026.OBBLIGATORIO
                                SET CONTROLLI-FORM-KO TO TRUE
                                MOVE 'AS4'            TO W-IDC
                                MOVE '7026'           TO W-ERR
                                PERFORM MEMORIZZA-ERRORE
                           WHEN OTHER
                                 IF WSC83I-CSPTIT-PADRE IS NOT
                                    ALFANUM
      *--------------------------   0490.SPECIE TITOLO NON AMMESSA
                                    SET CONTROLLI-FORM-KO TO TRUE
                                    MOVE 'AS4'            TO W-IDC
                                    MOVE '0490'           TO W-ERR
                                    PERFORM MEMORIZZA-ERRORE
                                 END-IF
                       END-EVALUATE
                       EVALUATE WSC83I-CHKDGTIT-PADRE
                          WHEN SPACES
                          WHEN LOW-VALUE
      *-------------------------- 7026.OBBLIGATORIO
                                  SET CONTROLLI-FORM-KO TO TRUE
                                  MOVE 'AS4'            TO W-IDC
                                  MOVE '7026'           TO W-ERR
                                  PERFORM MEMORIZZA-ERRORE
                          WHEN OTHER
                                  IF WSC83I-CHKDGTIT-PADRE  NOT
                                     NUMERIC
      *---------------------------   07026.VALORE NON AMMESSO
                                     SET CONTROLLI-FORM-KO TO TRUE
                                     MOVE 'AS4'            TO W-IDC
                                     MOVE '7026'           TO W-ERR
                                     PERFORM MEMORIZZA-ERRORE
                                  END-IF
                       END-EVALUATE
           END-EVALUATE.
      *
      *
      *----------------------------------------------------------------*
       CONTROLLI-F-ISIN-MANT.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-F-ISIN-MANT       '.
      *
           EVALUATE WSC83I-ISIN-MANT
                  WHEN SPACES
                  WHEN LOW-VALUES
                       CONTINUE
                  WHEN OTHER
                       EVALUATE WSC83I-CNAZTIT-MANT
                            WHEN SPACES
                            WHEN LOW-VALUE
      *------------------------- 7026.OBBLIGATORIO
                                 SET CONTROLLI-FORM-KO TO TRUE
                                 MOVE 'AS5'            TO W-IDC
                                 MOVE '7026'           TO W-ERR
                                 PERFORM MEMORIZZA-ERRORE
                            WHEN OTHER
                                 IF WSC83I-CNAZTIT-MANT   IS NOT
                                    ALFABETICA
      *---------------------------- 7026.VALORE NON AMMESSO
                                    SET CONTROLLI-FORM-KO TO TRUE
                                    MOVE 'AS5'            TO W-IDC
                                    MOVE '0490'           TO W-ERR
                                    PERFORM MEMORIZZA-ERRORE
                                 END-IF
                       END-EVALUATE
                       EVALUATE WSC83I-CSPTIT-MANT
                           WHEN SPACES
                           WHEN LOW-VALUE
      *------------------------- 7026.OBBLIGATORIO
                                SET CONTROLLI-FORM-KO TO TRUE
                                MOVE 'AS5'            TO W-IDC
                                MOVE '7026'           TO W-ERR
                                PERFORM MEMORIZZA-ERRORE
                           WHEN OTHER
                                 IF WSC83I-CSPTIT-MANT IS NOT
                                    ALFANUM
      *--------------------------   0490.SPECIE TITOLO NON AMMESSA
                                    SET CONTROLLI-FORM-KO TO TRUE
                                    MOVE 'AS5'            TO W-IDC
                                    MOVE '0490'           TO W-ERR
                                    PERFORM MEMORIZZA-ERRORE
                                 END-IF
                       END-EVALUATE
                       EVALUATE WSC83I-CHKDGTIT-MANT
                          WHEN SPACES
                          WHEN LOW-VALUE
      *-------------------------- 7026.OBBLIGATORIO
                                  SET CONTROLLI-FORM-KO TO TRUE
                                  MOVE 'AS5'            TO W-IDC
                                  MOVE '7026'           TO W-ERR
                                  PERFORM MEMORIZZA-ERRORE
                          WHEN OTHER
                                  IF WSC83I-CHKDGTIT-MANT   NOT
                                     NUMERIC
      *---------------------------   07026.VALORE NON AMMESSO
                                     SET CONTROLLI-FORM-KO TO TRUE
                                     MOVE 'AS5'            TO W-IDC
                                     MOVE '7026'           TO W-ERR
                                     PERFORM MEMORIZZA-ERRORE
                                  END-IF
                       END-EVALUATE
           END-EVALUATE.
      *
      *
      *----------------------------------------------------------------*
       CONTROLLI-F-ISIN-MAUP.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-F-ISIN-MAUP       '.
      *
      *
           EVALUATE WSC83I-ISIN-MAUP
                  WHEN SPACES
                  WHEN LOW-VALUES
                       CONTINUE
                  WHEN OTHER
                       EVALUATE WSC83I-CNAZTIT-MAUP
                            WHEN SPACES
                            WHEN LOW-VALUE
      *------------------------- 7026.OBBLIGATORIO
                                 SET CONTROLLI-FORM-KO TO TRUE
                                 MOVE 'AS6'            TO W-IDC
                                 MOVE '7026'           TO W-ERR
                                 PERFORM MEMORIZZA-ERRORE
                            WHEN OTHER
                                 IF WSC83I-CNAZTIT-MAUP   IS NOT
                                    ALFABETICA
      *---------------------------- 7026.VALORE NON AMMESSO
                                    SET CONTROLLI-FORM-KO TO TRUE
                                    MOVE 'AS6'            TO W-IDC
                                    MOVE '0490'           TO W-ERR
                                    PERFORM MEMORIZZA-ERRORE
                                 END-IF
                       END-EVALUATE
                       EVALUATE WSC83I-CSPTIT-MAUP
                           WHEN SPACES
                           WHEN LOW-VALUE
      *------------------------- 7026.OBBLIGATORIO
                                SET CONTROLLI-FORM-KO TO TRUE
                                MOVE 'AS6'            TO W-IDC
                                MOVE '7026'           TO W-ERR
                                PERFORM MEMORIZZA-ERRORE
                           WHEN OTHER
                                 IF WSC83I-CSPTIT-MAUP IS NOT
                                    ALFANUM
      *--------------------------   0490.SPECIE TITOLO NON AMMESSA
                                    SET CONTROLLI-FORM-KO TO TRUE
                                    MOVE 'AS6'            TO W-IDC
                                    MOVE '0490'           TO W-ERR
                                    PERFORM MEMORIZZA-ERRORE
                                 END-IF
                       END-EVALUATE
                       EVALUATE WSC83I-CHKDGTIT-MAUP
                          WHEN SPACES
                          WHEN LOW-VALUE
      *-------------------------- 7026.OBBLIGATORIO
                                  SET CONTROLLI-FORM-KO TO TRUE
                                  MOVE 'AS6'            TO W-IDC
                                  MOVE '7026'           TO W-ERR
                                  PERFORM MEMORIZZA-ERRORE
                          WHEN OTHER
                                  IF WSC83I-CHKDGTIT-MAUP   NOT
                                     NUMERIC
      *---------------------------   07026.VALORE NON AMMESSO
                                     SET CONTROLLI-FORM-KO TO TRUE
                                     MOVE 'AS6'            TO W-IDC
                                     MOVE '7026'           TO W-ERR
                                     PERFORM MEMORIZZA-ERRORE
                                  END-IF
                       END-EVALUATE
           END-EVALUATE.
      *
      *
      *----------------------------------------------------------------*
       CONTROLLI-F-ISIN-CIBR.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-F-ISIN-CIBR       '.
      *
      *
           EVALUATE WSC83I-ISIN-CIBR
                  WHEN SPACES
                  WHEN LOW-VALUES
                       CONTINUE
                  WHEN OTHER
                       EVALUATE WSC83I-CNAZTIT-CIBR
                            WHEN SPACES
                            WHEN LOW-VALUE
      *------------------------- 7026.OBBLIGATORIO
                                 SET CONTROLLI-FORM-KO TO TRUE
                                 MOVE 'AS7'            TO W-IDC
                                 MOVE '7026'           TO W-ERR
                                 PERFORM MEMORIZZA-ERRORE
                            WHEN OTHER
                                 IF WSC83I-CNAZTIT-CIBR   IS NOT
                                    ALFABETICA
      *---------------------------- 7026.VALORE NON AMMESSO
                                    SET CONTROLLI-FORM-KO TO TRUE
                                    MOVE 'AS7'            TO W-IDC
                                    MOVE '0490'           TO W-ERR
                                    PERFORM MEMORIZZA-ERRORE
                                 END-IF
                       END-EVALUATE
                       EVALUATE WSC83I-CSPTIT-CIBR
                           WHEN SPACES
                           WHEN LOW-VALUE
      *------------------------- 7026.OBBLIGATORIO
                                SET CONTROLLI-FORM-KO TO TRUE
                                MOVE 'AS7'            TO W-IDC
                                MOVE '7026'           TO W-ERR
                                PERFORM MEMORIZZA-ERRORE
                           WHEN OTHER
                                 IF WSC83I-CSPTIT-CIBR IS NOT
                                    ALFANUM
      *--------------------------   0490.SPECIE TITOLO NON AMMESSA
                                    SET CONTROLLI-FORM-KO TO TRUE
                                    MOVE 'AS7'            TO W-IDC
                                    MOVE '0490'           TO W-ERR
                                    PERFORM MEMORIZZA-ERRORE
                                 END-IF
                       END-EVALUATE
                       EVALUATE WSC83I-CHKDGTIT-CIBR
                          WHEN SPACES
                          WHEN LOW-VALUE
      *-------------------------- 7026.OBBLIGATORIO
                                  SET CONTROLLI-FORM-KO TO TRUE
                                  MOVE 'AS7'            TO W-IDC
                                  MOVE '7026'           TO W-ERR
                                  PERFORM MEMORIZZA-ERRORE
                          WHEN OTHER
                                  IF WSC83I-CHKDGTIT-CIBR   NOT
                                     NUMERIC
      *---------------------------   07026.VALORE NON AMMESSO
                                     SET CONTROLLI-FORM-KO TO TRUE
                                     MOVE 'AS7'            TO W-IDC
                                     MOVE '7026'           TO W-ERR
                                     PERFORM MEMORIZZA-ERRORE
                                  END-IF
                       END-EVALUATE
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       CONTROLLI-APPLICATIVI.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-APPLICATIVI       '.
      *
           INITIALIZE W-IND-IDC
                      W-CSTRIP.
      *
           MOVE WSC83I-CSTRIP             TO W-CSTRIP.
           DISPLAY 'W-CSTRIP    = ' W-CSTRIP.
      *
      * IDC.F04  CODICE ISIN
           PERFORM CONTROLLI-A-ISIN-F04.
      *
      * IDC.AS0  CODICE STRIPPING
           EVALUATE WSC83I-CSTRIP
               WHEN '0'
                    PERFORM CONTROLLI-A-CSTRIP-0
               WHEN '1'
                    PERFORM CONTROLLI-A-CSTRIP-1
               WHEN '2'
                    PERFORM CONTROLLI-A-CSTRIP-2
               WHEN '3'
                    PERFORM CONTROLLI-A-CSTRIP-3
               WHEN OTHER
      *-----        07026.VALORE NON AMMESSO
                    SET CONTROLLI-APPL-KO TO TRUE
                    MOVE 'AS0'            TO W-IDC
                    MOVE '7026'           TO W-ERR
                    PERFORM MEMORIZZA-ERRORE
           END-EVALUATE.

      * IDC.AS1  COD. STRIPPING OPERABILE
      *
TEST  *    DISPLAY 'WSC83I-CSTRIOP   = ' WSC83I-CSTRIOP.
      *    IF  WSC83I-CSTRIOP NOT = '0'
      *    AND WSC83I-CSTRIOP NOT = '1'
      *    AND WSC83I-CSTRIOP NOT = '2'
      *------ 07026.VALORE NON AMMESSO
      *       SET CONTROLLI-APPL-KO       TO TRUE
      *       MOVE 'AS1'                  TO W-IDC
      *       MOVE '7026'                 TO W-ERR
      *       PERFORM MEMORIZZA-ERRORE
      *    END-IF.
      *
      *----------------------------------------------------------------*
       CONTROLLI-A-ISIN-F04.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-A-ISIN-F04        '.
      *
      * VALORIZZO L' INDICARORE DI IDC DA USARE IN CASO DI ERRORE
           MOVE 'F04'                    TO W-IND-IDC.
      *
      * SALVATAGGIO IN CAMPI DI APPOGGIO
           INITIALIZE W-APPOGGIO-F04.
           MOVE WSC83I-CNAZTIT             TO W-CNAZTIT-F04
           MOVE WSC83I-CSPTIT              TO W-CSPTIT-F04
           MOVE WSC83I-CHKDGTIT            TO W-CHKDGTIT-F04
      *
      * VERIFICO L'ESISTENZA E LA TIPOLOGIA  DELL' ISIN IN BDTA
           PERFORM LETTURA-BDTA-F04
      *
      * VERIFICO L'ESISTENZA DELL' ISIN IN TATS
           PERFORM LETTURA-TATS-F04
      *
      * VERIFICO L'ESISTENZA DELL' ISIN IN TA-TAF
      * E SALVO DEI CAMPI DI TAB. CHE USERO' NEI CONTROLLI SUCCESSIVI
      *--- IMPOSTO LA CHIAVE DI LETTURA
           INITIALIZE DCLTA
                      DCLTAF
                      W-STATO-TA-TAF.

           MOVE WSC83I-CNAZTIT           TO CNAZTIT  OF DCLTA.
           MOVE WSC83I-CSPTIT            TO CSPTIT   OF DCLTA.
           MOVE WSC83I-CHKDGTIT          TO CHKDGTIT OF DCLTA.
           MOVE '00'                     TO CEMTIT   OF DCLTAF.
      *
           PERFORM LETTURA-TA-TAF-F04-AS4.
      *
           IF TA-TAF-OK
               MOVE CDIVEM   OF DCLTA    TO W-CDIVEM-F04
               MOVE CTAMM    OF DCLTA    TO W-CTAMM-F04
               MOVE CTINTER  OF DCLTA    TO W-CTINTER-F04
               MOVE CINDICIZ OF DCLTAF   TO W-CINDICIZ-F04
               MOVE ICACTDS  OF DCLTAF   TO W-ICACTDS-F04
               MOVE DFIOPER  OF DCLTAF   TO W-DFIOPER-F04
           END-IF.
      *
      *----------------------------------------------------------------*
       LETTURA-BDTA-F04.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - LETTURA-BDTA-F04            '.
      *
           INITIALIZE DCLBDTA.
      *
           MOVE WSC83I-CNAZTIT           TO CNAZTIT  OF DCLBDTA.
           MOVE WSC83I-CSPTIT            TO CSPTIT   OF DCLBDTA.
           MOVE WSC83I-CHKDGTIT          TO CHKDGTIT OF DCLBDTA.
      *
           EXEC SQL
              SELECT CTIPOL
                INTO :DCLBDTA.CTIPOL
                FROM BDTA
               WHERE CNAZTIT  = :DCLBDTA.CNAZTIT
                 AND CSPTIT   = :DCLBDTA.CSPTIT
                 AND CHKDGTIT = :DCLBDTA.CHKDGTIT
                WITH UR
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    IF CTIPOL OF DCLBDTA NOT = '03'
                       INITIALIZE W-DESCR1 W-DESCR2
                       MOVE 'ISIN NON IN BDTA CON TIPOLOGIA 03'
                                                  TO W-DESCR1
                       MOVE PIDMTX OF DCLWSRR     TO W-PIDMTX-X
                       STRING 'PIDMTX:  ' W-PIDMTX-X
                              ' TITOLO: ' WSC83I-ISIN
                       DELIMITED BY SIZE        INTO W-DESCR2
      *------------    7108.TITOLO NON CENSITO
                       SET CONTROLLI-APPL-KO      TO TRUE
                       MOVE '7108'                TO W-ERR
                       MOVE 'F04'                 TO W-IDC
                       PERFORM MEMORIZZA-ERRORE
                    END-IF
               WHEN +100
TEST                DISPLAY 'ISN NON IN BDTA = '     SQLCODE
TEST                DISPLAY 'ISN             = '     WSC83I-ISIN
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ISIN NON IN BDTA'       TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  ' W-PIDMTX-X
                           ' TITOLO: ' WSC83I-ISIN
                    DELIMITED BY SIZE           INTO W-DESCR2
      *------       7108.TITOLO NON CENSITO
                    SET CONTROLLI-APPL-KO         TO TRUE
                    MOVE '7108'                   TO W-ERR
                    MOVE 'F04'                    TO W-IDC
                    PERFORM MEMORIZZA-ERRORE
               WHEN OTHER
TEST                DISPLAY 'ERRORE DB2-SELECT BDTA'
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-SELECT BDTA' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  '               W-PIDMTX-X
                           ' TITOLO: '               WSC83I-ISIN
                    DELIMITED BY SIZE           INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       LETTURA-TATS-F04.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - LETTURA-TATS-F04            '.
      *
           INITIALIZE DCLTATS.
      *               W-CTMAUP-F04
      *               W-CSTRIP-F04.
      *
           MOVE WSC83I-CNAZTIT           TO CNAZTIT  OF DCLTATS.
           MOVE WSC83I-CSPTIT            TO CSPTIT   OF DCLTATS.
           MOVE '00'                     TO CEMTIT   OF DCLTATS.
      *
           EXEC SQL
              SELECT CNAZTIT
                    ,CSPTIT
                    ,CSTRIP
                    ,CTMAUP
                INTO :DCLTATS.CNAZTIT
                    ,:DCLTATS.CSPTIT
                    ,:DCLTATS.CSTRIP
                    ,:DCLTATS.CTMAUP
                FROM TATS
               WHERE CNAZTIT  = :DCLTATS.CNAZTIT
                 AND CSPTIT   = :DCLTATS.CSPTIT
                 AND CEMTIT   = :DCLTATS.CEMTIT
                WITH UR
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN +100
TEST                DISPLAY 'ISIN NON TATS = '       SQLCODE
TEST                DISPLAY 'ISIN          = '       WSC83I-ISIN
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ISIN NON IN TATS'       TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  ' W-PIDMTX-X
                           ' TITOLO: ' WSC83I-ISIN
                    DELIMITED BY SIZE           INTO W-DESCR2
      *------       1105.TITOLO NON STRIPPING
                    SET CONTROLLI-APPL-KO TO TRUE
                    MOVE '1105'           TO W-ERR
                    MOVE 'F04'            TO W-IDC
                    PERFORM MEMORIZZA-ERRORE
               WHEN OTHER
TEST                DISPLAY 'ERRORE DB2-SELECT TATS' SQLCODE
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-SELECT TATS' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  '               W-PIDMTX-X
                           ' TITOLO: '               WSC83I-ISIN
                    DELIMITED BY SIZE           INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       LETTURA-TA-TAF-F04-AS4.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - LETTURA-TA-TAF-F04-AS4      '.
      *
           EXEC SQL
              SELECT  A.CNAZTIT
                    , A.CSPTIT
                    , A.CHKDGTIT
                    , A.CDIVEM
                    , A.CTAMM
                    , A.CTINTER
                    , B.CINDICIZ
                    , B.ICACTDS
                    , B.DFIOPER
                INTO :DCLTA.CNAZTIT
                    ,:DCLTA.CSPTIT
                    ,:DCLTA.CHKDGTIT
                    ,:DCLTA.CDIVEM
                    ,:DCLTA.CTAMM
                    ,:DCLTA.CTINTER
                    ,:DCLTAF.CINDICIZ
                    ,:DCLTAF.ICACTDS
                    ,:DCLTAF.DFIOPER
                FROM TA  A
                    ,TAF B
               WHERE A.CNAZTIT  =  B.CNAZTIT
                 AND A.CSPTIT   =  B.CSPTIT
                 AND A.CNAZTIT  = :DCLTA.CNAZTIT
                 AND A.CSPTIT   = :DCLTA.CSPTIT
                 AND A.CHKDGTIT = :DCLTA.CHKDGTIT
                 AND B.CEMTIT   = :DCLTAF.CEMTIT
                WITH UR
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    SET TA-TAF-OK                   TO TRUE
               WHEN +100
TEST                DISPLAY 'ISIN NON IN TA-TAF = ' SQLCODE
TEST                DISPLAY 'CNAZTIT            = ' CNAZTIT OF DCLTA
TEST                DISPLAY 'CSPTIT             = ' CSPTIT  OF DCLTA
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ISIN NON IN TA-TAF'     TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX: '                W-PIDMTX-X
                           ' NAZ: '                  CNAZTIT OF DCLTA
                           ' TIT: '                  CSPTIT  OF DCLTA
                    DELIMITED BY SIZE           INTO W-DESCR2
      *------       7108.TITOLO NON CENSITO
                    SET CONTROLLI-APPL-KO         TO TRUE
                    MOVE '7108'                   TO W-ERR
                    MOVE W-IND-IDC                TO W-IDC
                    PERFORM MEMORIZZA-ERRORE
               WHEN OTHER
TEST                DISPLAY 'ERRORE DB2-SELECT TA-TAF =' SQLCODE
TEST                DISPLAY 'CNAZTIT           =' CNAZTIT OF DCLTA
TEST                DISPLAY 'CSPTIT            =' CSPTIT  OF DCLTA
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-SELECT TA-TAF' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR          TO W-PIDMTX-X
                    STRING 'PIDMTX:  '                 W-PIDMTX-X
                           ' NAZ   : '                 CNAZTIT OF DCLTA
                           ' TIT   : '                 CSPTIT  OF DCLTA
                    DELIMITED BY SIZE             INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       CONTROLLI-A-CSTRIP-0.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-A-CSTRIP-0        '.
      *
      *
      * IDC.AS1  CODICE STRIPPING OPERABILE
           IF  WSC83I-CSTRIOP NOT = '0'
      *------ 07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO       TO TRUE
              MOVE 'AS1'                  TO W-IDC
              MOVE '7026'                 TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS2  PERC. MASSIMA STRIPPABILE
           IF WSC83I-QPMSTRIP NOT = 0
      *------ 7006.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '7026'           TO W-ERR
              MOVE 'AS2'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS3  LOTTO MINIMO ASSOGGETTABILE
           IF WSC83I-QLMSTRIP NOT = 0
      *------ 7006.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '7026'           TO W-ERR
              MOVE 'AS3'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS4  ISIN PADRE
           IF  WSC83I-ISIN-PADRE NOT = SPACES
           AND WSC83I-ISIN-PADRE NOT = LOW-VALUE
      *------ 7006.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '7026'           TO W-ERR
              MOVE 'AS4'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS5  ISIN MANTELLO
           IF  WSC83I-ISIN-MANT  NOT = SPACES
           AND WSC83I-ISIN-MANT  NOT = LOW-VALUE
      *------ 7006.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '7026'           TO W-ERR
              MOVE 'AS5'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS6  ISIN MANTELLO UPLIF
           IF  WSC83I-ISIN-MAUP  NOT = SPACES
           AND WSC83I-ISIN-MAUP  NOT = LOW-VALUE
      *------ 7006.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '7026'           TO W-ERR
              MOVE 'AS6'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS7  ISIN CEDOLA IBRIDA
           IF  WSC83I-ISIN-CIBR  NOT = SPACES
           AND WSC83I-ISIN-CIBR  NOT = LOW-VALUE
      *------ 7006.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '7026'           TO W-ERR
              MOVE 'AS7'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS8   TIPO MANTELLO
           IF  WSC83I-CTMAUP     NOT = SPACES
           AND WSC83I-CTMAUP     NOT = LOW-VALUE
      *------ 7006.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '7026'           TO W-ERR
              MOVE 'AS8'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      *----------------------------------------------------------------*
       CONTROLLI-A-CSTRIP-1.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-A-CSTRIP-1        '.
      *
      * IDC.F04  ISIN
      * L'IDC.F04  E' OBBLIGATORIO, SE NON FOSSE VALORIZZATO MI SAREI
      * GIA' FERMATA AI CONTROLLI FORMALI CON ERRORE
           IF W-CTINTER-F04           NOT = '1'
      *------ 1109.TITOLO NON A REDDITO FISSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '1109'           TO W-ERR
              MOVE 'F04'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.

           IF W-CTAMM-F04          NOT = '01'
      *------ 1109.TITOLO NON A REDDITO FISSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '1110'           TO W-ERR
              MOVE 'F04'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS1  CODICE STRIPPING OPERABILE
           IF  WSC83I-CSTRIOP NOT = '0'
           AND WSC83I-CSTRIOP NOT = '1'
           AND WSC83I-CSTRIOP NOT = '2'
      *------ 07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO       TO TRUE
              MOVE 'AS1'                  TO W-IDC
              MOVE '7026'                 TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS2  PERC. MASSIMA STRIPPABILE
           IF WSC83I-QPMSTRIP < 0
           OR WSC83I-QPMSTRIP > 100
      *------ 0098.VALORE AL DI FUORI DEL LIMITE PREVISTO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '0098'           TO W-ERR
              MOVE 'AS2'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS3  LOTTO MINIMO ASSOGGETTAB.
           IF WSC83I-QLMSTRIP < 0
           OR WSC83I-QLMSTRIP = 0
      *------ 0270.VALORE AL DI FUORI DEL LIMITE PREVISTO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '0270'           TO W-ERR
              MOVE 'AS3'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS4  ISIN PADRE
           IF  WSC83I-ISIN-PADRE NOT =  SPACES
           AND WSC83I-ISIN-PADRE NOT =  LOW-VALUE
      *------ 7026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '7026'           TO W-ERR
              MOVE 'AS4'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS5  ISIN MANTELLO
      * -CONTROLLI SPECIFICI PER IDC AS5 CSTRIP-1
           PERFORM CONTROLLI-A-AS5-CSTRIP-1
      *
      * IDC.AS6  ISIN MANTELLO UPLIF
      * -CONTROLLI SPECIFICI PER IDC AS6 CSTRIP-1
           PERFORM CONTROLLI-A-AS6-CSTRIP-1
      *
      * IDC.AS7  ISIN CEDOLA IBRIDA
      * -CONTROLLI SPECIFICI PER IDC AS7 CSTRIP-1
           PERFORM CONTROLLI-A-AS7-CSTRIP-1
      *
      * IDC.AS8  TIPO MANTELLO
           IF  WSC83I-CTMAUP    NOT = SPACES
           AND WSC83I-CTMAUP    NOT = LOW-VALUE
      *------ 7006.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '7026'           TO W-ERR
              MOVE 'AS8'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      *----------------------------------------------------------------*
       CONTROLLI-A-AS5-CSTRIP-1.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-A-AS5-CSTRIP-1    '.
TEST       DISPLAY 'W-CINDICIZ-F04 = ' W-CINDICIZ-F04.
      *
      * VALORIZZO L' INDICARORE DI IDC DA USARE IN CASO DI ERRORE
           MOVE 'AS5'                    TO W-IND-IDC.
      *
           IF W-CINDICIZ-F04 = 'N'
      *------ ISIN IN F04 NON INDICIZZATO
TEST          DISPLAY ' TITOLO NON INDICIZZATO'
              IF  WSC83I-ISIN-MANT  NOT =  SPACES
              AND WSC83I-ISIN-MANT  NOT =  LOW-VALUE
      *----       7026.VALORE NON AMMESSO
                  SET CONTROLLI-APPL-KO TO TRUE
                  MOVE '7026'           TO W-ERR
                  MOVE 'AS5'            TO W-IDC
                  PERFORM MEMORIZZA-ERRORE
              END-IF
           ELSE
TEST          DISPLAY ' TITOLO INDICIZZATO'
      *------ ISIN IN F04 INDICIZZATO
              IF  WSC83I-ISIN-MANT      = SPACES
              OR  WSC83I-ISIN-MANT      = LOW-VALUE
      *------     0404.OBBLIGATORIO
                  SET CONTROLLI-APPL-KO TO TRUE
                  MOVE '0404'           TO W-ERR
                  MOVE 'AS5'            TO W-IDC
                  PERFORM MEMORIZZA-ERRORE
              END-IF
           END-IF.

      *
           IF W-CINDICIZ-F04 NOT = 'N'
TEST          DISPLAY ' TITOLO INDICIZZATO'
              IF  WSC83I-ISIN-MANT NOT = SPACES
              AND WSC83I-ISIN-MANT NOT = LOW-VALUES
      *       CONTROLLO LA PRESENZA DEL MANTELLO IN TATS
      *---    IMPOSTO LA CHIAVE DI LETTURA
                 INITIALIZE DCLTATS
                 MOVE WSC83I-CNAZTIT-MANT   TO CNAZTIT  OF DCLTATS
                 MOVE WSC83I-CSPTIT-MANT    TO CSPTIT   OF DCLTATS
                 MOVE '2'                   TO CSTRIP   OF DCLTATS
                 MOVE 'P'                   TO CTMAUP   OF DCLTATS
TEST             DISPLAY ' PREPARO LETTURA TATS'
                 PERFORM CONTROLLO-TATS
      *
      * CONTROLLO CHE IL MANTELLO NON SIA GIA' ASSOCIATO AD ALTRO PADRE
      *---    IMPOSTO LA CHIAVE DI LETTURA
                 INITIALIZE DCLTATS
                 MOVE WSC83I-CNAZTIT-MANT   TO CNAZMANT OF DCLTATS
                 MOVE WSC83I-CSPTIT-MANT    TO CSPTMANT OF DCLTATS
                 MOVE '1'                   TO CSTRIP   OF DCLTATS
                 PERFORM CNTR-UNICITA-MANT
      *
      * CONTROLLI SU TA-TAF PER MANTELLO E MANTELLO UPLIF
      *--- IMPOSTO LA CHIAVE DI LETTURA
                 INITIALIZE DCLTA
                            DCLTAF
                 MOVE WSC83I-CNAZTIT-MANT   TO CNAZTIT  OF DCLTA
                 MOVE WSC83I-CSPTIT-MANT    TO CSPTIT   OF DCLTA
                 MOVE WSC83I-CHKDGTIT-MANT  TO CHKDGTIT OF DCLTA
                 MOVE '00'                  TO CEMTIT   OF DCLTAF
                 PERFORM CONTROLLO-TA-TAF
              END-IF
           END-IF.

      *
      *----------------------------------------------------------------*
       CONTROLLI-A-AS6-CSTRIP-1.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-A-AS6-CSTRIP-1    '.
      *
      * VALORIZZO L' INDICARORE DI IDC DA USARE IN CASO DI ERRORE
           MOVE 'AS6'                    TO W-IND-IDC.
      *
           IF W-CINDICIZ-F04 = 'N'
      * SE ISIN F04 NON INDICIZZATO IL MANTELLO UPLIF NON DEVE ESSERCI
              IF  WSC83I-ISIN-MAUP  NOT =  SPACES
              AND WSC83I-ISIN-MAUP  NOT =  LOW-VALUE
      *----       7026.VALORE NON AMMESSO
                  SET CONTROLLI-APPL-KO TO TRUE
                  MOVE '7026'           TO W-ERR
                  MOVE 'AS6'            TO W-IDC
                  PERFORM MEMORIZZA-ERRORE
              END-IF
           ELSE
      * SE ISIN F04 INDICIZZATO IL MANTELLO UP. DEVE ESSERE VALORIZZATO
              IF  WSC83I-ISIN-MAUP      = SPACES
              OR  WSC83I-ISIN-MAUP      = LOW-VALUE
      *------     0404.OBBLIGATORIO
                  SET CONTROLLI-APPL-KO TO TRUE
                  MOVE '0404'           TO W-ERR
                  MOVE 'AS6'            TO W-IDC
                  PERFORM MEMORIZZA-ERRORE
              END-IF
           END-IF.
      *
           IF W-CINDICIZ-F04 NOT = 'N'
              IF  WSC83I-ISIN-MAUP    NOT = SPACES
              AND WSC83I-ISIN-MAUP    NOT = LOW-VALUES
      *       CONTROLLO LA PRESENZA DEL MANTELLO UPLIF IN TATS
      *---    IMPOSTO LA CHIAVE DI LETTURA
                  INITIALIZE DCLTATS
                  MOVE WSC83I-CNAZTIT-MAUP   TO CNAZTIT  OF DCLTATS
                  MOVE WSC83I-CSPTIT-MAUP    TO CSPTIT   OF DCLTATS
                  MOVE '2'                   TO CSTRIP   OF DCLTATS
                  MOVE 'U'                   TO CTMAUP   OF DCLTATS
                  PERFORM CONTROLLO-TATS
      *
      * CONTROLLO CHE IL MANTELLO UPLIF NON SIA ASSOCIATO AD ALTRO PADRE
      *---    IMPOSTO LA CHIAVE DI LETTURA
                 INITIALIZE DCLTATS
                 MOVE WSC83I-CNAZTIT-MAUP   TO CNAZMAUP OF DCLTATS
                 MOVE WSC83I-CSPTIT-MAUP    TO CSPTMAUP OF DCLTATS
                 MOVE '1'                   TO CSTRIP   OF DCLTATS
                 PERFORM CNTR-UNICITA-MAUP
      *
      * CONTROLLI SU TA-TAF PER MANTELLO UPLIF
      *---    IMPOSTO LA CHIAVE DI LETTURA
                 INITIALIZE DCLTA
                            DCLTAF
                 MOVE WSC83I-CNAZTIT-MAUP      TO CNAZTIT  OF DCLTA
                 MOVE WSC83I-CSPTIT-MAUP       TO CSPTIT   OF DCLTA
                 MOVE WSC83I-CHKDGTIT-MAUP     TO CHKDGTIT OF DCLTA
                 MOVE '00'                     TO CEMTIT   OF DCLTAF
                 PERFORM CONTROLLO-TA-TAF
      *
      * IL MANTELLO UPLIF DEVE ESSERE DIVERSO DAL MANTELLO
                 IF WSC83I-ISIN-MAUP = WSC83I-ISIN-MANT
                    MOVE 'ISIN-MAUP NON DEVE ESSERE UGUALE A ISIN-MANT'
                                               TO W-DESCR1
                    MOVE  WSC83I-ISIN-MAUP     TO W-DESCR2
TEST                DISPLAY 'WSC83I-ISIN-MAUP  = ' WSC83I-ISIN-MAUP
TEST                DISPLAY 'WSC83I-ISIN-MANT  = ' WSC83I-ISIN-MANT
      *----        1140 MANCA IN T135
                    SET CONTROLLI-APPL-KO      TO TRUE
                    MOVE '1140'                TO W-ERR
                    MOVE 'AS6'                 TO W-IDC
                    PERFORM MEMORIZZA-ERRORE
                 END-IF
              END-IF
           END-IF.

      *
      *----------------------------------------------------------------*
       CONTROLLI-A-AS7-CSTRIP-1.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-A-AS7-CSTRIP-1    '.
      *
      * VALORIZZO L' INDICARORE DI IDC DA USARE IN CASO DI ERRORE
           MOVE 'AS7'                    TO W-IND-IDC.
      *
           IF W-CINDICIZ-F04 = 'N'
              IF  WSC83I-ISIN-CIBR   = SPACES
              OR  WSC83I-ISIN-CIBR   = LOW-VALUE
      *------     0404.OBBLIGATORIO
                  SET CONTROLLI-APPL-KO TO TRUE
                  MOVE '0404'        TO W-ERR
                  MOVE 'AS7'         TO W-IDC
                  PERFORM MEMORIZZA-ERRORE
              END-IF
           ELSE
              IF  WSC83I-ISIN-CIBR  NOT = SPACES
              AND WSC83I-ISIN-CIBR  NOT = LOW-VALUE
      *----       7026.VALORE NON AMMESSO
                  SET CONTROLLI-APPL-KO TO TRUE
                  MOVE '7026'           TO W-ERR
                  MOVE 'AS7'            TO W-IDC
                  PERFORM MEMORIZZA-ERRORE
              END-IF
           END-IF.
      *
      * SALVATAGGIO IN CAMPI DI APPOGGIO
           INITIALIZE W-APPOGGIO-AS7.
           MOVE WSC83I-CNAZTIT-CIBR      TO W-CNAZTIT-CIBR-AS7.
           MOVE WSC83I-CSPTIT-CIBR       TO W-CSPTIT-CIBR-AS7.
           MOVE WSC83I-CHKDGTIT-CIBR     TO W-CHKDGTIT-CIBR-AS7.
      *
      * CONTROLLI SOLO PER TITOLI NON INDICIZZATI
           IF W-CINDICIZ-F04 = 'N'
      *       CONTROLLO LA PRESENZA DELLA CEDOLA IBRIDA IN TATS
      *---     IMPOSTO LA CHIAVE DI LETTURA
              INITIALIZE DCLTATS
                         W-STATO-TATS
              MOVE WSC83I-CNAZTIT-CIBR   TO CNAZTIT  OF DCLTATS
              MOVE WSC83I-CSPTIT-CIBR    TO CSPTIT   OF DCLTATS
              MOVE '3'                   TO CSTRIP   OF DCLTATS
              MOVE ' '                   TO CTMAUP   OF DCLTATS
              PERFORM CONTROLLO-TATS
      *
      * CONTROLLO CHE LA CEDOLA IBRIDA NON SIA ASSOCIATO AD ALTRO PADRE
      *--- IMPOSTO LA CHIAVE DI LETTURA
              INITIALIZE DCLTATS
              MOVE WSC83I-CNAZTIT-CIBR   TO CNAZCIBR OF DCLTATS
              MOVE WSC83I-CSPTIT-CIBR    TO CSPTCIBR OF DCLTATS
              MOVE '1'                   TO CSTRIP   OF DCLTATS
              PERFORM CNTR-UNICITA-CIBR
      *
      * CONTROLLI SU TA-TAF
              INITIALIZE DCLTA
                         DCLTAF
              MOVE WSC83I-CNAZTIT-CIBR      TO CNAZTIT  OF DCLTA
              MOVE WSC83I-CSPTIT-CIBR       TO CSPTIT   OF DCLTA
              MOVE WSC83I-CHKDGTIT-CIBR     TO CHKDGTIT OF DCLTA
              MOVE '00'                     TO CEMTIT   OF DCLTAF
              PERFORM CONTROLLO-TA-TAF
           END-IF.

      *
      *----------------------------------------------------------------*
       CONTROLLI-A-CSTRIP-2.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-A-CSTRIP-2        '.
      *
      * LETTURA TA-TAF CON IDC.AS4 CODICE ISIN PADRE CHE MI SERVE
      * PER DEI CONTROLLI SU ALCUNI IDC IN - CONTROLLI-A-CSTRIP-2
           IF  WSC83I-ISIN-PADRE    NOT = SPACES
           AND WSC83I-ISIN-PADRE    NOT = LOW-VALUES
              PERFORM SALVATAGGIO-PER-AS4
           END-IF.
      *
      * IDC.F04  CODICE ISIN
           PERFORM CONTROLLI-A-F04-CSTRIP-2
      *
      * IDC.AS1  COD. STRIPPING OPERABILE
           IF WSC83I-CSTRIOP    NOT = 0
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS1'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS2   PERC. MASSIMA STRIPPABILE
           IF WSC83I-QPMSTRIP   NOT = 0
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS2'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS3   LOTTO MINIMO ASSOGGETTAB.
           IF WSC83I-QLMSTRIP   NOT = 0
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS3'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS4   CODICE ISIN PADRE
           PERFORM CONTROLLI-A-AS4-CSTRIP-2
      *
      * IDC.AS5   CODICE ISIN MANTELLO
           IF  WSC83I-ISIN-MANT  NOT = SPACES
           AND WSC83I-ISIN-MANT  NOT = LOW-VALUE
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS5'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS6   CODICE ISIN MANTELLO UPLIF
           IF  WSC83I-ISIN-MAUP  NOT = SPACES
           AND WSC83I-ISIN-MAUP  NOT = LOW-VALUE
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS6'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS7   CODICE ISIN CEDOLA IBRIDA
           IF  WSC83I-ISIN-CIBR  NOT = SPACES
           AND WSC83I-ISIN-CIBR  NOT = LOW-VALUE
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS7'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS8   TIPO MANTELLO
           IF  WSC83I-CTMAUP         = SPACES
           OR  WSC83I-CTMAUP         = LOW-VALUE
      *-----  0404 .OBBLIGATORIO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS8'            TO W-IDC
              MOVE '0404'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           ELSE
              IF  WSC83I-CTMAUP    NOT = 'P'
              AND WSC83I-CTMAUP    NOT = 'U'
      *-----      07026.VALORE NON AMMESSO
                  SET CONTROLLI-APPL-KO TO TRUE
                  MOVE 'AS8'        TO W-IDC
                  MOVE '7026'       TO W-ERR
                  PERFORM MEMORIZZA-ERRORE
              END-IF
           END-IF.
      *
      * IDC.AS9   QUOTA CARTOLARE
           IF WSC83I-QQUOCAR    NOT = 0
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS9'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      *----------------------------------------------------------------*
       SALVATAGGIO-PER-AS4.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - SALVATAGGIO-PER-AS4         '.
      *
      *--- IMPOSTO LA CHIAVE DI LETTURA
           INITIALIZE DCLTA
                      DCLTAF
                      W-STATO-TA-TAF
                      W-APPOGGIO-AS4.

      * SALVATAGGIO IN CAMPI DI APPOGGIO
           MOVE WSC83I-CNAZTIT-PADRE     TO W-CNAZTIT-PADRE-AS4
           MOVE WSC83I-CSPTIT-PADRE      TO W-CSPTIT-PADRE-AS4
           MOVE WSC83I-CHKDGTIT-PADRE    TO W-CHKDGTIT-PADRE-AS4
      *
      * IMPOSTO LA CHIAVE DI LETTURA CON ISIN-PADRE DI AS4
           MOVE WSC83I-CNAZTIT-PADRE     TO CNAZTIT  OF DCLTA.
           MOVE WSC83I-CSPTIT-PADRE      TO CSPTIT   OF DCLTA.
           MOVE WSC83I-CHKDGTIT-PADRE    TO CHKDGTIT OF DCLTA.
           MOVE '00'                     TO CEMTIT   OF DCLTAF.
      *
TEST       DISPLAY 'WSC83I-ISIN-PADRE = '   WSC83I-ISIN-PADRE
      *
      * VALORIZZO L' INDICARORE DI IDC DA USARE IN CASO DI ERRORE
           MOVE 'AS4'                    TO W-IND-IDC.
           PERFORM LETTURA-TA-TAF-F04-AS4.

           IF TA-TAF-OK
              MOVE DFIOPER  OF DCLTAF   TO W-DFIOPER-AS4
              MOVE CINDICIZ OF DCLTAF   TO W-CINDICIZ-AS4
              MOVE CDIVEM   OF DCLTA    TO W-CDIVEM-AS4
              MOVE CTAMM    OF DCLTA    TO W-CTAMM-AS4
              MOVE CTINTER  OF DCLTA    TO W-CTINTER-AS4
              MOVE CINDICIZ OF DCLTAF   TO W-CINDICIZ-AS4
              MOVE ICACTDS  OF DCLTAF   TO W-ICACTDS-AS4
              MOVE DFIOPER  OF DCLTAF   TO W-DFIOPER-AS4
           END-IF.
      *
      *----------------------------------------------------------------*
       CONTROLLI-A-F04-CSTRIP-2.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-A-F04-CSTRIP-2    '.
      *
      * VALORIZZO L' INDICARORE DI IDC DA USARE IN CASO DI ERRORE
           MOVE 'F04'                    TO W-IND-IDC.
      *
      * CONTROLLO CHE L'ISIN NON SIA GIA' ASSOCIATO COME MANTELLO
      * PRINCIPALE AD ALTRO TITOLO PADRE
      *--- IMPOSTO LA CHIAVE DI LETTURA
           INITIALIZE DCLTATS.
           MOVE WSC83I-CNAZTIT           TO CNAZMANT OF DCLTATS.
           MOVE WSC83I-CSPTIT            TO CSPTMANT OF DCLTATS.
           MOVE '1'                      TO CSTRIP   OF DCLTATS.
      *
TEST       DISPLAY ' WSC83I-ISIN = '        WSC83I-ISIN
      *
           PERFORM CNTR-UNICITA-MANT.
TEST       DISPLAY 'W-TABELLA-ERRORI     = ' W-TABELLA-ERRORI
      *
      * CONTROLLO CHE L'ISIN NON SIA GIA' ASSOCIATO COME MANTELLO
      * UPLIF AD ALTRO TITOLO PADRE
      *--- IMPOSTO LA CHIAVE DI LETTURA
           INITIALIZE DCLTATS.
           MOVE WSC83I-CNAZTIT           TO CNAZMAUP OF DCLTATS.
           MOVE WSC83I-CSPTIT            TO CSPTMAUP OF DCLTATS.
           MOVE '1'                      TO CSTRIP   OF DCLTATS.
      *
           PERFORM CNTR-UNICITA-MAUP.
TEST       DISPLAY 'W-TABELLA-ERRORI     = ' W-TABELLA-ERRORI
      *
      * CONTROLLI SPECIFICI SU TA-TAF PER IDC F04 DI CSTRIP-2
      *------- IMPOSTO LA CHIAVE DI LETTURA
      *    MOVE WSC83I-CNAZTIT           TO CNAZTIT  OF DCLTA.
      *    MOVE WSC83I-CSPTIT            TO CSPTIT   OF DCLTA.
      *    MOVE WSC83I-CHKDGTIT          TO CHKDGTIT OF DCLTA.
      *    MOVE '00'                     TO CEMTIT   OF DCLTAF.
      *    PERFORM LETTURA-TA-TAF-F04-AS4.
      *
           IF  W-CTAMM-F04  NOT = '01'
      *-----  01110. MANCA IN T135
              SET CONTROLLI-APPL-KO      TO TRUE
              MOVE 'F04'                 TO W-IDC
              MOVE '1110'                TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
           IF W-DFIOPER-F04 NOT = W-DFIOPER-AS4
      *-----  01138. MANCA IN T135
              SET CONTROLLI-APPL-KO      TO TRUE
              MOVE 'F04'                 TO W-IDC
              MOVE '1138'                TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      *    IF W-CINDICIZ-F04      = 'N'
      *-----  ?????. MANCA IN T135
      *       SET CONTROLLI-APPL-KO      TO TRUE
      *       MOVE 'F04'                 TO W-IDC
      *       MOVE '????'                TO W-ERR
      *       PERFORM MEMORIZZA-ERRORE
      *    END-IF.
      *
      *----------------------------------------------------------------*
       CONTROLLI-A-AS4-CSTRIP-2.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-A-AS4-CSTRIP-2    '.
      *
      * VALORIZZO L' INDICARORE DI IDC DA USARE IN CASO DI ERRORE
           MOVE 'AS4'                    TO W-IND-IDC.
      *
           IF  WSC83I-ISIN-PADRE         = SPACES
           OR  WSC83I-ISIN-PADRE         = LOW-VALUE
      *-----  0404 .OBBLIGATORIO
              SET CONTROLLI-APPL-KO      TO TRUE
              MOVE 'AS4'                 TO W-IDC
              MOVE '0404'                TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           ELSE
      *-----  SE ISIN PADRE VALORIZZATO
      *-----  CONTROLLO LA PRESENZA DI ISIN PADRE IN TATS CON CSTRIP=1
      *-----  IMPOSTO LA CHIAVE DI LETTURA
              INITIALIZE DCLTATS
              MOVE WSC83I-CNAZTIT-PADRE  TO CNAZTIT OF DCLTATS
              MOVE WSC83I-CSPTIT-PADRE   TO CSPTIT  OF DCLTATS
TOGLI *       MOVE '1'                   TO CSTRIP  OF DCLTATS
              PERFORM CONTROLLO-TATS-AS4
      *
      *-----  SE ISIN PADRE VALORIZZATO CONTROLLO SIA NON INDICIZZATO
      *       IF W-CINDICIZ-AS4 = 'N'
      *-----     ???? .OBBLIGATORIO
      *          SET CONTROLLI-APPL-KO   TO TRUE
      *          MOVE 'AS4'              TO W-IDC
      *          MOVE '????'             TO W-ERR
      *          PERFORM MEMORIZZA-ERRORE
      *       END-IF
           END-IF.
      *
      *----------------------------------------------------------------*
       CONTROLLI-A-CSTRIP-3.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-A-CSTRIP-3        '.
      *
      * LETTURA TA-TAF CON IDC.AS4 CODICE ISIN PADRE CHE MI SERVE
      * PER DEI CONTROLLI SU ALCUNI IDC IN - CONTROLLI-A-CSTRIP-2
           IF  WSC83I-ISIN-PADRE NOT = SPACES
           AND WSC83I-ISIN-PADRE NOT = LOW-VALUES
               PERFORM SALVATAGGIO-PER-AS4
           END-IF.
      *
      * IDC.F04  CODICE ISIN
           PERFORM CONTROLLI-A-F04-CSTRIP-3
      *
      * IDC.AS1  COD. STRIPPING OPERABILE
           IF WSC83I-CSTRIOP    NOT = 0
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS1'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS2   PERC. MASSIMA STRIPPABILE
           IF WSC83I-QPMSTRIP   NOT = 0
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS2'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS3   LOTTO MINIMO ASSOGGETTAB.
           IF WSC83I-QLMSTRIP   NOT = 0
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS3'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS4   CODICE ISIN PADRE
           PERFORM CONTROLLI-A-AS4-CSTRIP-3
      *
      * IDC.AS5   CODICE ISIN MANTELLO
           IF  WSC83I-ISIN-MANT  NOT = SPACES
           AND WSC83I-ISIN-MANT  NOT = LOW-VALUE
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS5'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS6   CODICE ISIN MANTELLO UPLIF
           IF  WSC83I-ISIN-MAUP  NOT = SPACES
           AND WSC83I-ISIN-MAUP  NOT = LOW-VALUE
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS6'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS7   CODICE ISIN CEDOLA IBRIDA
           IF  WSC83I-ISIN-CIBR  NOT = SPACES
           AND WSC83I-ISIN-CIBR  NOT = LOW-VALUE
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS7'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS8   TIPO MANTELLO
           IF  WSC83I-CTMAUP     NOT = SPACES
           AND WSC83I-CTMAUP     NOT = LOW-VALUE
      *-----  0404 .OBBLIGATORIO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS8'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      * IDC.AS9   QUOTA CARTOLARE
           IF WSC83I-QQUOCAR    NOT = 0
      *-----  07026.VALORE NON AMMESSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE 'AS9'            TO W-IDC
              MOVE '7026'           TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           END-IF.
      *
      *----------------------------------------------------------------*
       CONTROLLI-A-F04-CSTRIP-3.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-A-F04-CSTRIP-3    '.
      *
      * VALORIZZO L' INDICARORE DI IDC DA USARE IN CASO DI ERRORE
           MOVE 'F04'                    TO W-IND-IDC.
      *
      * CONTROLLO CHE LA CEDOLA IBR. NON SIA GIA' ASSOCIATA COME CEDOLA
      * AD ALTRO TITOLO PADRE
      *--- IMPOSTO LA CHIAVE DI LETTURA
      *    INITIALIZE DCLTATS.
      *    MOVE WSC83I-CNAZTIT           TO CNAZCIBR OF DCLTATS.
      *    MOVE WSC83I-CSPTIT            TO CSPTCIBR OF DCLTATS.
      *    MOVE '1'                      TO CSTRIP   OF DCLTATS.
      *
      *    PERFORM CNTR-UNICITA-CIBR.
      *
           IF W-CTAMM-F04          NOT = '01'
      *------ 1109.TITOLO NON A REDDITO FISSO
              SET CONTROLLI-APPL-KO TO TRUE
              MOVE '1110'           TO W-ERR
              MOVE 'F04'            TO W-IDC
              PERFORM MEMORIZZA-ERRORE
      *
      * CONTROLLO ESISTENZA SU CPPO
           INITIALIZE DCLCPPO.
           MOVE W-CNAZTIT-PADRE-AS4      TO CNAZTIT  OF DCLCPPO.
           MOVE W-CSPTIT-PADRE-AS4       TO CSPTIT   OF DCLCPPO.
           MOVE '00'                     TO CEMTIT   OF DCLCPPO.
           MOVE '1'                      TO CTCESPAG OF DCLCPPO.
           MOVE W-DFIOPER-F04            TO DPAGCES  OF DCLCPPO.
TEST       DISPLAY 'W-CNAZTIT-PADRE-AS4  = ' W-CNAZTIT-PADRE-AS4
TEST       DISPLAY 'W-CSPTIT-PADRE-AS4   = ' W-CSPTIT-PADRE-AS4
TEST       DISPLAY 'CTCESPAG             = ' CTCESPAG OF DCLCPPO
TEST       DISPLAY 'W-DFIOPER-F04        = ' W-DFIOPER-F04
           PERFORM CONTROLLO-CPPO.
      *
      *----------------------------------------------------------------*
       CONTROLLI-A-AS4-CSTRIP-3.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLI-A-AS4-CSTRIP-3    '.
      *
      * VALORIZZO L' INDICARORE DI IDC DA USARE IN CASO DI ERRORE
           MOVE 'AS4'                    TO W-IND-IDC.
      *
           IF  WSC83I-ISIN-PADRE         = SPACES
           OR  WSC83I-ISIN-PADRE         = LOW-VALUE
      *-----  0404 .OBBLIGATORIO
              SET CONTROLLI-APPL-KO      TO TRUE
              MOVE 'AS4'                 TO W-IDC
              MOVE '0404'                TO W-ERR
              PERFORM MEMORIZZA-ERRORE
           ELSE
      *-----  SE ISIN PADRE VALORIZZATO
      *-----  CONTROLLO LA PRESENZA DI ISIN PADRE IN TATS CON CSTRIP=1
      *-----  IMPOSTO LA CHIAVE DI LETTURA
              INITIALIZE DCLTATS
              MOVE WSC83I-CNAZTIT-PADRE  TO CNAZTIT OF DCLTATS
              MOVE WSC83I-CSPTIT-PADRE   TO CSPTIT  OF DCLTATS
TOGLI *       MOVE '1'                   TO CSTRIP  OF DCLTATS
TOGLI *       MOVE ' '                   TO CTMAUP  OF DCLTATS
TOGLI *       PERFORM CONTROLLO-TATS
              PERFORM CONTROLLO-TATS-AS4
           END-IF.
      *
      *----------------------------------------------------------------*
       CONTROLLO-TATS.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLO-TATS              '.
      *
           EXEC SQL
              SELECT CNAZTIT
                    ,CSPTIT
                    ,CSTRIP
                    ,CTMAUP
                INTO :DCLTATS.CNAZTIT
                    ,:DCLTATS.CSPTIT
                    ,:DCLTATS.CSTRIP
                    ,:DCLTATS.CTMAUP
                FROM TATS
               WHERE CNAZTIT  = :DCLTATS.CNAZTIT
                 AND CSPTIT   = :DCLTATS.CSPTIT
                 AND CSTRIP   = :DCLTATS.CSTRIP
                 AND CTMAUP   = :DCLTATS.CTMAUP
                WITH UR
           END-EXEC.
      *
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN +100
TEST                DISPLAY 'ISIN NON IN TATS'      SQLCODE
TEST                DISPLAY 'PIDMTX        = '      W-PIDMTX-X
TEST                DISPLAY 'TITOLO        = '      CSPTIT OF DCLTATS
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ISIN NON IN TATS'       TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  ' W-PIDMTX-X
                           ' NAZ   : ' CNAZTIT    OF DCLTATS
                           ' TIT   : ' CSPTIT     OF DCLTATS
                    DELIMITED BY SIZE           INTO W-DESCR2
      *------       07108.TITOLO NON CENSITO
                    SET CONTROLLI-APPL-KO         TO TRUE
                    MOVE '7108'                   TO W-ERR
                    MOVE W-IND-IDC                TO W-IDC
                    PERFORM MEMORIZZA-ERRORE
               WHEN OTHER
TEST                DISPLAY 'ERRORE DB2-SELECT TATS'
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-SELECT TATS' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  '               W-PIDMTX-X
                           ' NAZ   : '               CNAZTIT OF DCLTATS
                           ' TIT   : '               CSPTIT  OF DCLTATS
                    DELIMITED BY SIZE           INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       CONTROLLO-TATS-AS4.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLO-TATS-AS4          '.
      *
           EXEC SQL
              SELECT CNAZTIT
                    ,CSPTIT
                    ,CSTRIP
                    ,CTMAUP
                INTO :DCLTATS.CNAZTIT
                    ,:DCLTATS.CSPTIT
                    ,:DCLTATS.CSTRIP
                    ,:DCLTATS.CTMAUP
                FROM TATS
               WHERE CNAZTIT  = :DCLTATS.CNAZTIT
                 AND CSPTIT   = :DCLTATS.CSPTIT
                WITH UR
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN +100
TEST                DISPLAY 'ISIN NON IN TATS'       SQLCODE
TEST                DISPLAY 'PIDMTX        = '       W-PIDMTX-X
TEST                DISPLAY 'TITOLO        = '       CSPTIT OF DCLTATS
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ISIN NON IN TATS'       TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  ' W-PIDMTX-X
                           ' NAZ   : ' CNAZTIT    OF DCLTATS
                           ' TIT   : ' CSPTIT     OF DCLTATS
                    DELIMITED BY SIZE           INTO W-DESCR2
      *------       07108.TITOLO NON CENSITO
                    SET CONTROLLI-APPL-KO         TO TRUE
                    MOVE '7108'                   TO W-ERR
                    MOVE W-IND-IDC                TO W-IDC
                    PERFORM MEMORIZZA-ERRORE
               WHEN OTHER
TEST                DISPLAY 'ERRORE DB2-SELECT TATS'
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-SELECT TATS' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  '               W-PIDMTX-X
                           ' NAZ   : '               CNAZTIT OF DCLTATS
                           ' TIT   : '               CSPTIT  OF DCLTATS
                    DELIMITED BY SIZE           INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       CNTR-UNICITA-MANT.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CNTR-UNICITA-MANT           '.
      *
           EXEC SQL
              SELECT CNAZMANT
                    ,CSPTMANT
                    ,CSTRIP
                INTO :DCLTATS.CNAZMANT
                    ,:DCLTATS.CSPTMANT
                    ,:DCLTATS.CSTRIP
                FROM TATS
               WHERE CNAZMANT = :DCLTATS.CNAZMANT
                 AND CSPTMANT = :DCLTATS.CSPTMANT
                 AND CSTRIP   = :DCLTATS.CSTRIP
                WITH UR
           END-EXEC.
      *
TEST       DISPLAY 'SQLCODE = '  SQLCODE
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    INITIALIZE W-DESCR1 W-DESCR2
TEST                DISPLAY 'MANTELLO GIA'' ASSOCIATO AD ALTRO PADRE'
                    MOVE    'MANTELLO GIA'' ASSOCIATO AD ALTRO PADRE'
                                                  TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  '               W-PIDMTX-X
                           ' TITOLO: '               WSC83I-ISIN-MANT
                    DELIMITED BY SIZE           INTO W-DESCR2
      *------       1141 MANCA IN T135
                    SET CONTROLLI-APPL-KO         TO TRUE
                    MOVE '1141'                   TO W-ERR
                    MOVE W-IND-IDC                TO W-IDC
                    PERFORM MEMORIZZA-ERRORE
               WHEN +100
                    CONTINUE
               WHEN OTHER
TEST                DISPLAY 'ERRORE DB2-SELECT TATS'
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-SELECT TATS' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  '               W-PIDMTX-X
                           ' TITOLO: '               WSC83I-ISIN-MANT
                    DELIMITED BY SIZE           INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       CNTR-UNICITA-MAUP.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CNTR-UNICITA-MAUP           '.
      *
           EXEC SQL
              SELECT CNAZMAUP
                    ,CSPTMAUP
                    ,CSTRIP
                INTO :DCLTATS.CNAZMAUP
                    ,:DCLTATS.CSPTMAUP
                    ,:DCLTATS.CSTRIP
                FROM TATS
               WHERE CNAZMAUP = :DCLTATS.CNAZMAUP
                 AND CSPTMAUP = :DCLTATS.CSPTMAUP
                 AND CSTRIP   = :DCLTATS.CSTRIP
                WITH UR
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'MANTELLO UP GIA''ASSOCIATO AD ALTRO PADRE'
                                                  TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  '               W-PIDMTX-X
                           ' TITOLO: '               WSC83I-ISIN-MAUP
                    DELIMITED BY SIZE           INTO W-DESCR2
      *------       1141 MANCA IN T135
                    SET CONTROLLI-APPL-KO         TO TRUE
                    MOVE '1141'                   TO W-ERR
                    MOVE 'AS6'                    TO W-IDC
                    PERFORM MEMORIZZA-ERRORE
               WHEN +100
                    CONTINUE
               WHEN OTHER
TEST                DISPLAY 'ERRORE DB2-SELECT TATS'
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-SELECT TATS' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  '               W-PIDMTX-X
                           ' TITOLO: '               WSC83I-ISIN-MAUP
                    DELIMITED BY SIZE           INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       CNTR-UNICITA-CIBR.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CNTR-UNICITA-CIBR           '.
      *
           EXEC SQL
              SELECT CNAZCIBR
                    ,CSPTCIBR
                    ,CSTRIP
                INTO :DCLTATS.CNAZCIBR
                    ,:DCLTATS.CSPTCIBR
                    ,:DCLTATS.CSTRIP
                FROM TATS
               WHERE CNAZCIBR = :DCLTATS.CNAZCIBR
                 AND CSPTCIBR = :DCLTATS.CSPTCIBR
                 AND CSTRIP   = :DCLTATS.CSTRIP
                WITH UR
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'CEDOLA IBR GIA'' ASSOCIATA AD ALTRO PADRE'
                                                  TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR     TO W-PIDMTX-X
                    STRING 'PIDMTX:  '            W-PIDMTX-X
                           ' ISIN-CIBR :'          WSC83I-ISIN-CIBR
                    DELIMITED BY SIZE        INTO W-DESCR2
      *------       1141 MANCA IN T135
                    SET CONTROLLI-APPL-KO         TO TRUE
                    MOVE '1141'                   TO W-ERR
                    MOVE W-IND-IDC                TO W-IDC
                    PERFORM MEMORIZZA-ERRORE
               WHEN +100
                    CONTINUE
               WHEN OTHER
TEST                DISPLAY 'ERRORE DB2-SELECT TATS' SQLCODE
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-SELECT TATS' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  '               W-PIDMTX-X
                           ' ISIN-CIBR :'            WSC83I-ISIN-CIBR
                    DELIMITED BY SIZE           INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       CONTROLLO-TA-TAF.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLO-TA-TAF            '.
      *
           EXEC SQL
              SELECT  A.CNAZTIT
                    , A.CSPTIT
                    , A.CHKDGTIT
                    , A.CDIVEM
                    , B.CINDICIZ
                    , B.ICACTDS
                    , B.DFIOPER
                INTO :DCLTA.CNAZTIT
                    ,:DCLTA.CSPTIT
                    ,:DCLTA.CHKDGTIT
                    ,:DCLTA.CDIVEM
                    ,:DCLTAF.CINDICIZ
                    ,:DCLTAF.ICACTDS
                    ,:DCLTAF.DFIOPER
                FROM TA  A
                    ,TAF B
               WHERE A.CNAZTIT  =  B.CNAZTIT
                 AND A.CSPTIT   =  B.CSPTIT
                 AND A.CNAZTIT  = :DCLTA.CNAZTIT
                 AND A.CSPTIT   = :DCLTA.CSPTIT
                 AND A.CHKDGTIT = :DCLTA.CHKDGTIT
                 AND B.CEMTIT   = :DCLTAF.CEMTIT
                WITH UR
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    IF W-IND-IDC        = 'AS7'
      *------          CONTROLLO SPECIFICO PER CEDOLA IBRIDA
                       IF CINDICIZ OF DCLTAF  NOT = 'N'
                          MOVE 'ISIN CON CINDICIZ N '
                                                  TO W-DESCR1
                          MOVE PIDMTX OF DCLWSRR  TO W-PIDMTX-X
                          STRING 'PIDMTX:  '         W-PIDMTX-X
                                ' TITOLO: '          WSC83I-ISIN-CIBR
                          DELIMITED BY SIZE     INTO W-DESCR2
TEST                      DISPLAY 'CINDICIZ OF DCLTAF '
TEST                               CINDICIZ OF DCLTAF
      *------             1141 MANCA IN T135
                          SET CONTROLLI-APPL-KO   TO TRUE
                          MOVE '1136'             TO W-ERR
                          MOVE W-IND-IDC          TO W-IDC
                          PERFORM MEMORIZZA-ERRORE
                       END-IF
                    ELSE
      *------          ALTRI IDC
                       IF CINDICIZ OF DCLTAF NOT = W-CINDICIZ-F04
                          MOVE 'ISIN CON ICACTDS DIVERSO DA ISIN-F04'
                                                  TO W-DESCR1
                          MOVE PIDMTX OF DCLWSRR  TO W-PIDMTX-X
                          STRING 'PIDMTX:  '         W-PIDMTX-X
                                 ' NAZ : '           CNAZTIT OF DCLTATS
                                 ' TIT : '           CSPTIT  OF DCLTATS
                          DELIMITED BY SIZE     INTO W-DESCR2
TEST                      DISPLAY 'CINDICIZ OF DCLTAF = '
TEST                               CINDICIZ OF DCLTAF
TEST                      DISPLAY 'CW-CINDICIZ-F04   = '
TEST                               W-CINDICIZ-F04
      *------             1141 MANCA IN T135
                          SET CONTROLLI-APPL-KO   TO TRUE
                          MOVE '1136'             TO W-ERR
                          MOVE W-IND-IDC          TO W-IDC
                          PERFORM MEMORIZZA-ERRORE
                        END-IF
                    END-IF
                    IF ICACTDS OF DCLTAF NOT = W-ICACTDS-F04
                       MOVE 'ISIN CON ICACTDS DIVERSO DA ISIN-F04'
                                                  TO W-DESCR1
                       MOVE PIDMTX OF DCLWSRR     TO W-PIDMTX-X
                       STRING 'PIDMTX:  '            W-PIDMTX-X
                              ' NAZ : '              CNAZTIT OF DCLTATS
                              ' TIT : '              CSPTIT  OF DCLTATS
                       DELIMITED BY SIZE        INTO W-DESCR2
TEST                   DISPLAY 'ICACTDS OF DCLTAF = ' ICACTDS OF DCLTAF
TEST                   DISPLAY 'W-ICACTDS-F04    =  ' W-ICACTDS-F04
      *------          1137 MANCA IN T135
                       SET CONTROLLI-APPL-KO      TO TRUE
                       MOVE '1137'                TO W-ERR
                       MOVE W-IND-IDC             TO W-IDC
                       PERFORM MEMORIZZA-ERRORE
                    END-IF
                    IF DFIOPER OF DCLTAF NOT = W-DFIOPER-F04
                       MOVE 'ISIN CON DFIOPER DIVERSA DA ISIN-F04'
                                                  TO W-DESCR1
                       MOVE PIDMTX OF DCLWSRR     TO W-PIDMTX-X
                       STRING 'PIDMTX: '             W-PIDMTX-X
                              ' NAZ : '              CNAZTIT OF DCLTATS
                              ' TIT : '              CSPTIT  OF DCLTATS
                       DELIMITED BY SIZE        INTO W-DESCR2
TEST                   DISPLAY 'DFIOPER  OF DCLTAF = ' DFIOPER OF DCLTAF
TEST                   DISPLAY 'W-DFIOPER-F04      = ' W-DFIOPER-F04
      *------          1138 MANCA IN T135
                       SET CONTROLLI-APPL-KO      TO TRUE
                       MOVE '1138'                TO W-ERR
                       MOVE W-IND-IDC             TO W-IDC
                       PERFORM MEMORIZZA-ERRORE
                    END-IF
                    IF CDIVEM  OF DCLTA  NOT = W-CDIVEM-F04
                       MOVE 'ISIN CON DIVISA DIVERSA DA ISIN-F04'
                                                  TO W-DESCR1
                       MOVE PIDMTX OF DCLWSRR     TO W-PIDMTX-X
                       STRING 'PIDMTX: '             W-PIDMTX-X
                              ' NAZ : '              CNAZTIT OF DCLTATS
                              ' TIT : '              CSPTIT  OF DCLTATS
                       DELIMITED BY SIZE        INTO W-DESCR2
TEST                   DISPLAY 'CDIVEM  OF DCLTA  = ' CDIVEM  OF DCLTA
TEST                   DISPLAY 'W-CDIVEM-F04      = ' W-CDIVEM-F04
      *------          1139 MANCA IN T135
                       SET CONTROLLI-APPL-KO      TO TRUE
                       MOVE '1139'                TO W-ERR
                       MOVE W-IND-IDC             TO W-IDC
                       PERFORM MEMORIZZA-ERRORE
                    END-IF
               WHEN +100
TEST                DISPLAY 'ISIN NON IN TA-TAF = ' SQLCODE
TEST                DISPLAY 'CNAZTIT            = ' CNAZTIT OF DCLTA
TEST                DISPLAY 'CSPTIT             = ' CSPTIT  OF DCLTA
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ISIN NON IN TA-TAF'     TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX: '                W-PIDMTX-X
                           ' NAZ:'                   CNAZTIT OF DCLTA
                           ' TIT:'                   CSPTIT  OF DCLTA
                    DELIMITED BY SIZE           INTO W-DESCR2
      *------       7108.TITOLO NON CENSITO
                    SET CONTROLLI-APPL-KO         TO TRUE
                    MOVE '7108'                   TO W-ERR
                    MOVE W-IND-IDC                TO W-IDC
                    PERFORM MEMORIZZA-ERRORE
               WHEN OTHER
TEST                DISPLAY 'ERRORE DB2-SELECT TA-TAF '
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-SELECT TA-TAF ' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX: '                W-PIDMTX-X
                           ' NAZ : '                 CNAZTIT OF DCLTATS
                           ' TIT : '                 CSPTIT  OF DCLTATS
                    DELIMITED BY SIZE           INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       CONTROLLO-CPPO.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CONTROLLO-CPPO              '.
      *
           EXEC SQL
              SELECT CNAZTIT
                    ,CSPTIT
                    ,CNAZTIT
                    ,CSPTIT
                    ,CEMTIT
                    ,CTCESPAG
                    ,DPAGCES
                INTO :DCLCPPO.CNAZTIT
                    ,:DCLCPPO.CSPTIT
                    ,:DCLCPPO.CEMTIT
                    ,:DCLCPPO.CTCESPAG
                    ,:DCLCPPO.DPAGCES
                FROM CPPO
               WHERE CNAZTIT  = :DCLCPPO.CNAZTIT
                 AND CSPTIT   = :DCLCPPO.CSPTIT
                 AND CEMTIT   = :DCLCPPO.CEMTIT
                 AND CTCESPAG = :DCLCPPO.CTCESPAG
                 AND DPAGCES  = :DCLCPPO.DPAGCES
                WITH UR
           END-EXEC.
      *
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN +100
TEST                DISPLAY 'ISIN NON PRESENTE IN CPPO = ' SQLCODE
TEST                DISPLAY 'PIDMTX OF DCLWSRR = ' PIDMTX OF DCLWSRR
TEST                DISPLAY 'TITOLO            = ' CSPTIT  OF DCLCPPO
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ISIN NON IN CPPO'       TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  '               W-PIDMTX-X
                           ' NAZ   : '               CNAZTIT OF DCLCPPO
                           ' TIT   : '               CSPTIT  OF DCLCPPO
                    DELIMITED BY SIZE           INTO W-DESCR2
      *------       01142.
                    SET CONTROLLI-APPL-KO         TO TRUE
                    MOVE '1142'                   TO W-ERR
                    MOVE W-IND-IDC                TO W-IDC
                    PERFORM MEMORIZZA-ERRORE
               WHEN OTHER
TEST                DISPLAY 'ERRORE DB2-SELECT CPPO'
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-SELECT CPPO' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR        TO W-PIDMTX-X
                    STRING 'PIDMTX:  '               W-PIDMTX-X
                           ' NAZ   : '               CNAZTIT OF DCLCPPO
                           ' TIT   : '               CSPTIT  OF DCLCPPO
                    DELIMITED BY SIZE           INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       AGGIORNAMENTI-APPLICATIVI.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - AGGIORNAMENTI-APPLICATIVI   '.
      *
           EVALUATE WSC83I-CSTRIP
               WHEN '0'
                    PERFORM AGGIORNA-TATS-CSTRIP-03
               WHEN '1'
                    PERFORM AGGIORNA-TATS-CSTRIP-1
               WHEN '2'
                    PERFORM AGGIORNA-TATS-CSTRIP-2
                    PERFORM AGGIORNA-TAF-CSTRIP-23
                    PERFORM AGGIORNA-IAT
               WHEN '3'
                    PERFORM AGGIORNA-TATS-CSTRIP-03
                    PERFORM AGGIORNA-TAF-CSTRIP-23
                    PERFORM AGGIORNA-IAT
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       AGGIORNA-TATS-CSTRIP-03.
      *----------------------------------------------------------------*
      * AGGIORNAMENTO TABELLA TATS PER CSTRIP 0 E 3
      *
TEST       DISPLAY 'WSOC83 - AGGIORNA-TATS-CSTRIP-03     '.
      *
      * IMPOSTO LA CHIAVE PER L'AGGIORNAMENTO
           MOVE WSC83I-CNAZTIT         TO CNAZTIT OF DCLTATS
           MOVE WSC83I-CSPTIT          TO CSPTIT  OF DCLTATS
           MOVE '00'                   TO CEMTIT  OF DCLTATS
      *
      * CAMPI DA AGGIORNARE
           MOVE WSC83I-CSTRIP          TO CSTRIP  OF DCLTATS
      *
           EXEC SQL
                UPDATE TATS
                SET CSTRIP      = :DCLTATS.CSTRIP
                WHERE CNAZTIT   = :DCLTATS.CNAZTIT
                  AND CSPTIT    = :DCLTATS.CSPTIT
                  AND CEMTIT    = :DCLTATS.CEMTIT
           END-EXEC.

           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN OTHER
                    IF FINE-REGOLARE
                       SET TATS-KO                          TO TRUE
                       INITIALIZE W-DESCR1 W-DESCR2
                       MOVE 'ERRORE DB2 - UPDATE TAB. TATS' TO W-DESCR1
                       PERFORM FINE-ANOMALA
                    END-IF
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       AGGIORNA-TATS-CSTRIP-1.
      *----------------------------------------------------------------*
      * AGGIORNAMENTO TABELLA TATS PER CSTRIP 1
      *
TEST       DISPLAY 'WSOC83 - AGGIORNA-TATS-CSTRIP-1      '.
      *
      * IMPOSTO LA CHIAVE PER L'AGGIORNAMENTO
           MOVE WSC83I-CNAZTIT         TO CNAZTIT   OF DCLTATS
           MOVE WSC83I-CSPTIT          TO CSPTIT    OF DCLTATS
           MOVE '00'                   TO CEMTIT    OF DCLTATS
TEST  *    DISPLAY 'WSC83I-CNAZTIT   = ' WSC83I-CNAZTIT
TEST  *    DISPLAY 'WSC83I-CSPTIT    = ' WSC83I-CSPTIT
      *
      * CAMPI DA AGGIORNARE
           MOVE WSC83I-CSTRIP          TO CSTRIP    OF DCLTATS
           MOVE WSC83I-CSTRIOP         TO CSTRIOP   OF DCLTATS
           MOVE WSC83I-QPMSTRIP        TO QPMSTRIP  OF DCLTATS
           MOVE WSC83I-QLMSTRIP        TO QLMSTRIP  OF DCLTATS
TEST  *    DISPLAY 'WSC83I-CSTRIP    = ' WSC83I-CSTRIP
TEST  *    DISPLAY 'WSC83I-CSTRIOP   = ' WSC83I-CSTRIOP
TEST  *    DISPLAY 'WSC83I-QPMSTRIP  = ' WSC83I-QPMSTRIP
TEST  *    DISPLAY 'WSC83I-QLMSTRIP  = ' WSC83I-QLMSTRIP
TEST  *    DISPLAY 'CSTRIP    OF DCLTATS =' CSTRIP    OF DCLTATS
TEST  *    DISPLAY 'CSTRIOP   OF DCLTATS =' CSTRIOP   OF DCLTATS
TEST  *    DISPLAY 'QPMSTRIP  OF DCLTATS =' QPMSTRIP  OF DCLTATS
TEST  *    DISPLAY 'QLMSTRIP  OF DCLTATS =' QLMSTRIP  OF DCLTATS
      *
      * PER TITOLI INDICIZZATI VALORIZZO MANTELLO, MANTELLO UP.
           IF W-CINDICIZ-F04 NOT = 'N'
              MOVE WSC83I-CNAZTIT-MANT TO CNAZMANT  OF DCLTATS
              MOVE WSC83I-CSPTIT-MANT  TO CSPTMANT  OF DCLTATS
              MOVE '00'                TO CEMTMANT  OF DCLTATS
              MOVE WSC83I-CNAZTIT-MAUP TO CNAZMAUP  OF DCLTATS
              MOVE WSC83I-CSPTIT-MAUP  TO CSPTMAUP  OF DCLTATS
              MOVE '00'                TO CEMTMAUP  OF DCLTATS
           ELSE
      * PER TITOLI INDICIZZATI VALORIZZO LA CEDOLA IBRIDA
              MOVE WSC83I-CNAZTIT-CIBR TO CNAZCIBR  OF DCLTATS
              MOVE WSC83I-CSPTIT-CIBR  TO CSPTCIBR  OF DCLTATS
TEST  *       DISPLAY 'WSC83I-CNAZTIT-CIBR =' WSC83I-CNAZTIT-CIBR
TEST  *       DISPLAY 'WSC83I-CSPTIT-CIBR ='  WSC83I-CSPTIT-CIBR
              MOVE '00'                TO CEMTCIBR  OF DCLTATS
           END-IF.
      *
           MOVE WSC83I-QQUOCAR         TO QQUOCAR   OF DCLTATS
TEST  *    DISPLAY 'WSC83I-QQUOCAR     =' WSC83I-QQUOCAR
      *
TEST  *    DISPLAY 'W-CINDICIZ-F04     =' W-CINDICIZ-F04.
           IF W-CINDICIZ-F04 NOT = 'N'
TEST          DISPLAY 'AGG TATS INDICIZZATI'
              EXEC SQL
                   UPDATE TATS
                   SET CSTRIP      = :DCLTATS.CSTRIP
                     , CSTRIOP     = :DCLTATS.CSTRIOP
                     , QPMSTRIP    = :DCLTATS.QPMSTRIP
                     , QLMSTRIP    = :DCLTATS.QLMSTRIP
                     , CNAZMANT    = :DCLTATS.CNAZMANT
                     , CSPTMANT    = :DCLTATS.CSPTMANT
                     , CEMTMANT    = :DCLTATS.CEMTMANT
                     , CNAZMAUP    = :DCLTATS.CNAZMAUP
                     , CSPTMAUP    = :DCLTATS.CSPTMAUP
                     , CEMTMAUP    = :DCLTATS.CEMTMAUP
                     , QQUOCAR     = :DCLTATS.QQUOCAR
                   WHERE CNAZTIT   = :DCLTATS.CNAZTIT
                     AND CSPTIT    = :DCLTATS.CSPTIT
                     AND CEMTIT    = :DCLTATS.CEMTIT
              END-EXEC
           ELSE
TEST          DISPLAY 'AGG TATS NON INDICIZZATI'
              EXEC SQL
                   UPDATE TATS
                   SET CSTRIP      = :DCLTATS.CSTRIP
                     , CSTRIOP     = :DCLTATS.CSTRIOP
                     , QPMSTRIP    = :DCLTATS.QPMSTRIP
                     , QLMSTRIP    = :DCLTATS.QLMSTRIP
                     , CNAZCIBR    = :DCLTATS.CNAZCIBR
                     , CSPTCIBR    = :DCLTATS.CSPTCIBR
                     , CEMTCIBR    = :DCLTATS.CEMTCIBR
                     , QQUOCAR     = :DCLTATS.QQUOCAR
                   WHERE CNAZTIT   = :DCLTATS.CNAZTIT
                     AND CSPTIT    = :DCLTATS.CSPTIT
                     AND CEMTIT    = :DCLTATS.CEMTIT
              END-EXEC
           END-IF.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
TEST                DISPLAY 'AGGIORNAMENTO TATS OK'
               WHEN OTHER
                    IF FINE-REGOLARE
                       SET TATS-KO                          TO TRUE
                       INITIALIZE W-DESCR1 W-DESCR2
                       MOVE 'ERRORE DB2 - UPDATE TAB. TATS' TO W-DESCR1
                       PERFORM FINE-ANOMALA
                    END-IF
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       AGGIORNA-TATS-CSTRIP-2.
      *----------------------------------------------------------------*
      * AGGIORNAMENTO TABELLA TATS PER CSTRIP 2
      *
TEST       DISPLAY 'WSOC83 - AGGIORNA-TATS-CSTRIP-2      '.
      *
      * IMPOSTO LA CHIAVE PER L'AGGIORNAMENTO
           MOVE WSC83I-CNAZTIT         TO CNAZTIT   OF DCLTATS
           MOVE WSC83I-CSPTIT          TO CSPTIT    OF DCLTATS
           MOVE '00'                   TO CEMTIT    OF DCLTATS
      *
      * CAMPI DA AGGIORNARE
           MOVE WSC83I-CSTRIP          TO CSTRIP    OF DCLTATS
           MOVE WSC83I-CTMAUP          TO CTMAUP    OF DCLTATS
      *
           EXEC SQL
                UPDATE TATS
                SET CSTRIP      = :DCLTATS.CSTRIP
                  , CTMAUP      = :DCLTATS.CTMAUP
                WHERE CNAZTIT   = :DCLTATS.CNAZTIT
                  AND CSPTIT    = :DCLTATS.CSPTIT
                  AND CEMTIT    = :DCLTATS.CEMTIT
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN OTHER
                    IF FINE-REGOLARE
                       SET TATS-KO                          TO TRUE
                       INITIALIZE W-DESCR1 W-DESCR2
                       MOVE 'ERRORE DB2 - UPDATE TAB. TATS' TO W-DESCR1
                       PERFORM FINE-ANOMALA
                    END-IF
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       AGGIORNA-TAF-CSTRIP-23.
      *----------------------------------------------------------------*
      * AGGIORNAMENTO TABELLA TAF PER CSTRIP 2 E 3
      *
TEST       DISPLAY 'WSOC83 - AGGIORNA-TAF-CSTRIP-23      '.
      *
      * IMPOSTO LA CHIAVE PER L'AGGIORNAMENTO
           MOVE WSC83I-CNAZTIT         TO CNAZTIT   OF DCLTAF
           MOVE WSC83I-CSPTIT          TO CSPTIT    OF DCLTAF
           MOVE '00'                   TO CEMTIT    OF DCLTAF
      *
      * CAMPI DA AGGIORNARE
           MOVE WSC83I-CNAZTIT-PADRE   TO CNAZTIPX  OF DCLTAF.
           MOVE WSC83I-CSPTIT-PADRE    TO CSPTIPX   OF DCLTAF.
      *
           EXEC SQL
                UPDATE TAF
                SET CNAZTIPX    = :DCLTAF.CNAZTIPX
                  , CSPTIPX     = :DCLTAF.CSPTIPX
                WHERE CNAZTIT   = :DCLTAF.CNAZTIT
                  AND CSPTIT    = :DCLTAF.CSPTIT
                  AND CEMTIT    = :DCLTAF.CEMTIT
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN OTHER
                    IF FINE-REGOLARE
                       SET TAF-KO                           TO TRUE
                       INITIALIZE W-DESCR1 W-DESCR2
                       MOVE 'ERRORE DB2 - UPDATE TAB. TAF' TO W-DESCR1
                       PERFORM FINE-ANOMALA
                    END-IF
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       AGGIORNA-IAT.
      *----------------------------------------------------------------*
      * AGGIORNAMENTO TABELLA IAT PER CSTRIP 2 E 3
      *
TEST       DISPLAY 'WSOC83 - AGGIORNA-IAT                '.
      *
      * IMPOSTO LA CHIAVE PER L'AGGIORNAMENTO
           MOVE WSC83I-CNAZTIT         TO CNAZTIT   OF DCLIAT
           MOVE WSC83I-CSPTIT          TO CSPTIT    OF DCLIAT
           MOVE WSC83I-CHKDGTIT        TO CHKDGTIT  OF DCLIAT
      *
      * CAMPI DA AGGIORNARE
           MOVE 'C'                    TO CSCERXT   OF DCLIAT.
      *
           EXEC SQL
                UPDATE IAT
                SET CSCERXT     = :DCLIAT.CSCERXT
                  , WTIMECEXT   =  CURRENT TIMESTAMP
                WHERE CNAZTIT   = :DCLIAT.CNAZTIT
                  AND CSPTIT    = :DCLIAT.CSPTIT
                  AND CHKDGTIT  = :DCLIAT.CHKDGTIT
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN OTHER
                    IF FINE-REGOLARE
                       SET IAT-KO                           TO TRUE
                       INITIALIZE W-DESCR1 W-DESCR2
                       MOVE 'ERRORE DB2 - UPDATE TAB. IAT' TO W-DESCR1
                       PERFORM FINE-ANOMALA
                    END-IF
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       AGGIORNAMENTI-ARCHITETTURALI.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - AGGIORNAMENTI-ARCHITETTURALI'.
      *
           PERFORM COMPONI-RISPOSTA.
      *
           PERFORM SCRITTURA-CODA-TD-WS22.
      *
           PERFORM AGGIORNAMENTO-TABELLA-WSRR.
      *
           PERFORM GESTIONE-WSL.
      *
      *----------------------------------------------------------------*
       COMPONI-RISPOSTA.
      *----------------------------------------------------------------*

           INITIALIZE DCLWSRS.

           MOVE PIDMTX  OF DCLWSRR         TO PIDMTX       OF DCLWSRS.
           MOVE CMSGMTX OF DCLWSRR         TO CMSGMTX      OF DCLWSRS.
           MOVE CMITT   OF DCLWSRR         TO CDEST        OF DCLWSRS.
           IF CONTROLLI-KO
              MOVE 'MTXE0'                 TO CFAMAPP      OF DCLWSRS
           ELSE
              MOVE 'MTX00'                 TO CFAMAPP      OF DCLWSRS
           END-IF.
           MOVE CCATAPPL      OF DCLWSRR   TO CCATAPPL     OF DCLWSRS.
           MOVE CAMBOP        OF DCLWSRR   TO CAMBOP       OF DCLWSRS.
           MOVE CPRIOMS       OF DCLWSRR   TO CPRIOMS      OF DCLWSRS.
           MOVE CTSOGMTX      OF DCLWSRR   TO CTSOGMTX     OF DCLWSRS.
           MOVE CVETT         OF DCLWSRR   TO CVETT        OF DCLWSRS.
           MOVE '0'                        TO CINVMSG      OF DCLWSRS.
           MOVE W-TIMESTAMP-DEFAULT        TO WTIMEST      OF DCLWSRS.
           MOVE XTESTMTX OF DCLWSRR        TO XTESTMTX     OF DCLWSRS.
           MOVE WSC83I                     TO XMSGRISE-TEXT OF DCLWSRS.
           MOVE LENGTH OF WSC83I           TO XMSGRISE-LEN OF DCLWSRS.

           EXEC SQL
                INSERT  INTO WSRS
                       ( PIDMTX
                       , WTIMECAR
                       , CMSGMTX
                       , CDEST
                       , CFAMAPP
                       , CAMBOP
                       , CCATAPPL
                       , CPRIOMS
                       , CTSOGMTX
                       , CVETT
                       , CINVMSG
                       , WTIMEST
                       , XTESTMTX
                       , XMSGRISE
                       , PPROG    )
                VALUES (:DCLWSRS.PIDMTX
                       ,CURRENT TIMESTAMP
                       ,:DCLWSRS.CMSGMTX
                       ,:DCLWSRS.CDEST
                       ,:DCLWSRS.CFAMAPP
                       ,:DCLWSRS.CAMBOP
                       ,:DCLWSRS.CCATAPPL
                       ,:DCLWSRS.CPRIOMS
                       ,:DCLWSRS.CTSOGMTX
                       ,:DCLWSRS.CVETT
                       ,:DCLWSRS.CINVMSG
                       ,:DCLWSRS.WTIMEST
                       ,:DCLWSRS.XTESTMTX
                       ,:DCLWSRS.XMSGRISE
                       ,:DCLWSRS.PPROG    )
           END-EXEC.

           EVALUATE SQLCODE
               WHEN ZERO
V     *             DISPLAY 'WS83 - WSRS: OK '
                    CONTINUE
               WHEN OTHER
V     *             DISPLAY 'WS83 - WSRS: KO ' SQLCODE
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-INSERT TAB. WSRS' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRS    TO W-PIDMTX-X
                    STRING 'PIDMTX: ' W-PIDMTX-X
                    DELIMITED BY SIZE INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.

      *----------------------------------------------------------------*
       AGGIORNAMENTO-TABELLA-WSRR.
      *----------------------------------------------------------------*

           MOVE W-IDN-RICH-MSG OF  W-AREA-KEYMSG-WS83
                                         TO PIDMTX  OF DCLWSRR.

           EVALUATE TRUE
               WHEN CONTROLLI-OK
                    MOVE '3'             TO CVALMSG OF DCLWSRR
               WHEN CONTROLLI-FORM-KO
                    MOVE '1'             TO CVALMSG OF DCLWSRR
               WHEN CONTROLLI-APPL-KO
                    MOVE '2'             TO CVALMSG OF DCLWSRR
           END-EVALUATE.

           MOVE W-TABELLA-ERRORI         TO XERRMTX OF DCLWSRR.         00321400
           MOVE '0'                      TO CESMTX  OF DCLWSRR.

           EXEC SQL
                UPDATE WSRR
                SET XERRMTX    = :DCLWSRR.XERRMTX
                   ,CVALMSG    = :DCLWSRR.CVALMSG
                   ,CESMTX     = :DCLWSRR.CESMTX
                WHERE PIDMTX   = :DCLWSRR.PIDMTX
                  AND CVALMSG IN (' ')
           END-EXEC.

           EVALUATE SQLCODE
               WHEN ZERO
V     *             DISPLAY 'WS83 - WSRR: OK '
                    CONTINUE
               WHEN OTHER
V     *             DISPLAY 'WS83 - WSRR: KO ' SQLCODE
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2 - UPDATE TAB. WSRR' TO W-DESCR1
                    MOVE PIDMTX OF DCLWSRR    TO W-PIDMTX-X
                    STRING 'PIDMTX: ' W-PIDMTX-X
                    DELIMITED BY SIZE INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.

      *----------------------------------------------------------------*
       PREPARA-INVIO-MAIL.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - PREPARA-INVIO-MAIL          '.
      *
      * INIZIALIZZA LA TABELLA CON LE DATE DEL PIANO DI AMMORTAMENTO
           PERFORM VARYING IND-DP FROM 1 BY 1
                   UNTIL   IND-DP > IND-DP-MAX
                   MOVE ZEROES                TO TAB-DPAGCES(IND-DP)
NEWC               MOVE ZEROES                TO TAB-QTASINT(IND-DP)
           END-PERFORM.
      *
           DISPLAY 'W-CSTRIP    = ' W-CSTRIP.
           EVALUATE W-CSTRIP
               WHEN '1'
                    PERFORM PREPARA-INVIO-CSTRIP-1
               WHEN '3'
                    PERFORM PREPARA-INVIO-CSTRIP-3
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       PREPARA-INVIO-CSTRIP-1.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - PREPARA-INVIO-CSTRIP-1      '.
      *
      * CARICA LA TABELLA CONTENENTE I DATI DEL PIANO AMMORTAMENTO
           PERFORM CARICA-DATI-PIANO.
      *
      * PER OGNUNA DELLE DATE DEL PIANO SALVATA IN TABELLA, CONTROLLA
      * L'ESISTENZA DELLA CEDOLA.
      *
           INITIALIZE W-CONTA-CED
                      W-CNAZTIT-CED
                      W-CSPTIT-CED.
      *
           INITIALIZE W-TRATTA-DP
      *
           INITIALIZE IND-DP.
           PERFORM TRATTA-DATI-PIANO
                   VARYING IND-DP FROM 1 BY 1
                     UNTIL IND-DP > IND-DP-MAX
                        OR  TAB-DPAGCES(IND-DP) = 0.
      *
      * CONTROLLO CHE LA CEDOLA AVENTE LA DATA MAGGIORE SUL PIANO
      * SIA LA STESSA PRESENTE IN IDC AS7 DEL FLUSSO.
TEST       DISPLAY 'W-SALVA-IND-DP        = ' W-SALVA-IND-DP
TEST       DISPLAY 'W-CONTA-CED           = ' W-CONTA-CED
TEST       DISPLAY 'W-CINDICIZ-F04        = ' W-CINDICIZ-F04
TEST       DISPLAY 'W-CNAZTIT-CED         = ' W-CNAZTIT-CED
TEST       DISPLAY 'W-CSPTIT-CED          = ' W-CSPTIT-CED
TEST       DISPLAY 'W-CNAZTIT-CIBR-AS7    = ' W-CNAZTIT-CIBR-AS7
TEST       DISPLAY 'W-CSPTIT-CIBR-AS7     = ' W-CSPTIT-CIBR-AS7
           IF W-SALVA-IND-DP        = W-CONTA-CED
              IF W-CINDICIZ-F04     = 'N'
      *--------- TITOLI NON INDICIZZATI
                 IF  W-CNAZTIT-CED  = W-CNAZTIT-CIBR-AS7
                 AND W-CSPTIT-CED   = W-CSPTIT-CIBR-AS7
                     PERFORM INNESCA-INVIO
                 END-IF
              ELSE
      *--------- TITOLI INDICIZZATI
                 PERFORM INNESCA-INVIO
              END-IF
           END-IF.
      *
      *----------------------------------------------------------------*
       PREPARA-INVIO-CSTRIP-3.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - PREPARA-INVIO-CSTRIP-3      '.
      *
      *    CONTROLLO ESISTENZA DELL' ISIN-PADRE-AS4 IN TATS
      *    SE LO TROVO, PROCEDO CON LA MEMORIZZAZIONE DEL PIANO DI
      *    AMMORTAMENTO DEL PADRE
           PERFORM CNTR-ESISTENZA-TATS.
      *
           IF CNTR-TATS-OK
      *       CARICA LA TABELLA CON LE DATE DEL PIANO AMMORTAMENTO
              PERFORM CARICA-DATI-PIANO
      *       PER OGNI DATA DEL PIANO CONTROLLO CHE ESISTA LA CEDOLA
              INITIALIZE W-CONTA-CED
                         W-CNAZTIT-CED
                         W-CSPTIT-CED
                         W-TRATTA-DP
              PERFORM TRATTA-DATI-PIANO
                      VARYING IND-DP FROM 1 BY 1
                        UNTIL IND-DP > IND-DP-MAX
                          OR  TAB-DPAGCES(IND-DP) = 0
           END-IF.
      *    SE OK  SALVA LE DATE DEL CPPO CON ISIN PADRE AS4
      *    PER OGNI DATA CONTROLLA ESISTENZA CED COME NEL CASO DEL F04
      *    CONTROLLI PER INVIO COME NEL CSTRIP-1
      *
TEST       DISPLAY 'W-SALVA-IND-DP        = ' W-SALVA-IND-DP
TEST       DISPLAY 'W-CONTA-CED           = ' W-CONTA-CED
TEST       DISPLAY 'W-CINDICIZ-F04        = ' W-CINDICIZ-F04
TEST       DISPLAY 'W-CNAZTIT-CED         = ' W-CNAZTIT-CED
TEST       DISPLAY 'W-CSPTIT-CED          = ' W-CSPTIT-CED
TEST       DISPLAY 'W-CNAZTIT-CIBR-AS7    = ' W-CNAZTIT-CIBR-AS7
TEST       DISPLAY 'W-CSPTIT-CIBR-AS7     = ' W-CSPTIT-CIBR-AS7
           IF CNTR-TATS-OK
              IF W-SALVA-IND-DP        = W-CONTA-CED
                 IF W-CINDICIZ-F04     = 'N'
      *---------    TITOLI NON INDICIZZATI
                    IF  W-CNAZTIT-CED  = W-CNAZTIT-CIBR-AS7
                    AND W-CSPTIT-CED   = W-CSPTIT-CIBR-AS7
                        PERFORM INNESCA-INVIO
                    END-IF
                 ELSE
      *---------    TITOLI INDICIZZATI
                    PERFORM INNESCA-INVIO
                 END-IF
              END-IF
           END-IF.
      *
      *----------------------------------------------------------------*
       CARICA-DATI-PIANO.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CARICA-DATI-PIANO           '.
      *
      * LETTURA PIANO AMMORTAMENTO DA SALVARE IN  TABELLA (TAB-DP)
           PERFORM OPEN-CUR-PDA.
      *
           SET PRIMA-LETT-CUR-PDA           TO TRUE.
           SET NO-FINE-CUR-PDA              TO TRUE.
           PERFORM FETCH-CUR-PDA.
      *
NEWC       SET SW-SL-NO                     TO TRUE.
           MOVE 0                           TO IND-DP
           MOVE 0                           TO W-SALVA-IND-DP
           PERFORM SALVA-DATI-PIANO
                   UNTIL FINE-CUR-PDA.
      *
TEST       DISPLAY 'W-SALVA-IND-DP  = '        W-SALVA-IND-DP.
      *
           PERFORM CLOSE-CUR-PDA.
      *
      *----------------------------------------------------------------*
       OPEN-CUR-PDA.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - OPEN-CURR-CPPO              '.
      *
           INITIALIZE W-CUR-PDA.
      *
           EVALUATE W-CSTRIP
               WHEN '1'
                    MOVE W-CNAZTIT-F04         TO CNAZTIT  OF DCLCPPO
                    MOVE W-CSPTIT-F04          TO CSPTIT   OF DCLCPPO
                    MOVE '1'                   TO CTCESPAG OF DCLCPPO
               WHEN '3'
                    MOVE W-CNAZTIT-PADRE-AS4   TO CNAZTIT  OF DCLCPPO
                    MOVE W-CSPTIT-PADRE-AS4    TO CSPTIT   OF DCLCPPO
                    MOVE '1'                   TO CTCESPAG OF DCLCPPO
           END-EVALUATE.
      *
           EXEC SQL
                OPEN CUR-PDA
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN OTHER
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-OPEN CUR CPPO' TO W-DESCR1
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       FETCH-CUR-PDA.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - FETCH-CUR-PDA              '.
      *
           EXEC SQL
                FETCH  CUR-PDA
                 INTO :DCLCPPO.DPAGCES
NEWC                 ,:DCLPPO.QTASINT
           END-EXEC.
      *
           MOVE SQLCODE TO W-CUR-PDA.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    SET NO-PRIMA-LETT-CUR-PDA        TO TRUE
               WHEN +100
                    IF PRIMA-LETT-CUR-PDA
TEST                   DISPLAY 'ERRORE NO RECORD SU CUR-PDA'
TEST                   DISPLAY 'TITOLO = ' CSPTIT   OF DCLCPPO
                       MOVE 'ERRORE DB2-LETTURA TABELLA CPPO'
                                                      TO W-DESCR1
                       STRING 'CNAZTIT  :' CNAZTIT  OF DCLCPPO
                              ' CSPTIT  :' CSPTIT   OF DCLCPPO
                       DELIMITED BY SIZE INTO W-DESCR2
                       PERFORM FINE-ANOMALA
                    ELSE
                       DISPLAY 'FINE CUR-PDA   '
                       SET FINE-CUR-PDA           TO TRUE
                       MOVE 1001                  TO IND-DP
                    END-IF
               WHEN OTHER
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-LETTURA TABELLA CPPO' TO W-DESCR1
                       STRING 'CNAZTIT  :' CNAZTIT  OF DCLCPPO
                              ' CSPTIT  :' CSPTIT   OF DCLCPPO
                       DELIMITED BY SIZE INTO W-DESCR2
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       CLOSE-CUR-PDA.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CLOSE-CUR-PDA              '.
      *
           EXEC SQL
                CLOSE CUR-PDA
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN OTHER
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-CLOSE CUR CPPO' TO W-DESCR1
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       SALVA-DATI-PIANO.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - SALVA-DATI-PIANO            '.
      *
      *
           IF IND-DP              > IND-DP-MAX
           OR DPAGCES  OF DCLCPPO = 0
NEWC       OR QTASINT  OF DCLPPO  = 0
              SET FINE-CUR-PDA              TO TRUE
              DISPLAY ' FINE TAB-DP          =' IND-DP
           ELSE
              ADD  1                        TO IND-DP
NEWC  * SALVO QTASINT X CAPIRE (IN CASO VARI) CHE HO 1 ISIN SHORT-LONG
NEWC          IF IND-DP  = 1
NEWC          AND DPAGCES  OF DCLCPPO NOT = 0
NEWC          AND QTASINT  OF DCLPPO  NOT = 0
NEWC              MOVE QTASINT  OF DCLPPO     TO W-QTASINT
TEST              DISPLAY 'IND-DP          = ' IND-DP
TEST              DISPLAY 'W-QTASINT       = ' W-QTASINT
NEWC          END-IF
              MOVE DPAGCES  OF DCLCPPO      TO TAB-DPAGCES(IND-DP)
NEWC          MOVE QTASINT  OF DCLPPO       TO TAB-QTASINT(IND-DP)
              MOVE IND-DP                   TO W-SALVA-IND-DP
TEST          IF  IND-DP                    = 2
TEST           DISPLAY 'IND-DP          = ' IND-DP
TEST           DISPLAY 'QTASINT  OF DCLPPO = ' QTASINT  OF DCLPPO
TEST           DISPLAY 'W-QTASINT          = ' W-QTASINT
TEST          END-IF
NEWC          IF  IND-DP                    = 2
NEWC          AND W-QTASINT                NOT = QTASINT  OF DCLPPO
NEWC  *       SE IL TASSO è VARIATO ALLORA è UN TITOLO SHORT-LONG
NEWC              SET SW-SL-SI              TO TRUE
TEST              DISPLAY ' TITOLO SHORT-LONG ' SW-SL
NEWC          END-IF
              DISPLAY ' INDICE TAB-DP        =' IND-DP
              DISPLAY ' PREGRESSIVO CARICATO =' TAB-DPAGCES(IND-DP)
           END-IF.
      *
           PERFORM FETCH-CUR-PDA.
      *
      *----------------------------------------------------------------*
       CHIUSURA-CUR-PDA.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CHIUSURA-CUR-PDA           '.
      *
           EXEC SQL
                CLOSE CUR-PDA
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    CONTINUE
               WHEN OTHER
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-CLOSE CUR CPPO' TO W-DESCR1
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       TRATTA-DATI-PIANO.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - TRATTA-DATI-PIANO           '.
      *
TEST       DISPLAY 'IND-DP        = ' IND-DP.
      *
           INITIALIZE DCLTA
                      DCLTATS
                      DCLTAF
                      DCLCPPO
                      W-TAB-DPAGCES.
      *
      *  IMPOSTA LA CHIAVE DI LETTURA
           EVALUATE W-CSTRIP
               WHEN '1'
                    MOVE W-CINDICIZ-F04        TO CINDICIZ OF DCLTAF
                    MOVE W-ICACTDS-F04         TO ICACTDS  OF DCLTAF
                    MOVE W-CDIVEM-F04          TO CDIVEM   OF DCLTA
                    MOVE TAB-DPAGCES(IND-DP)   TO W-TAB-DPAGCES
               WHEN '3'
                    MOVE W-CINDICIZ-AS4        TO CINDICIZ OF DCLTAF
                    MOVE W-ICACTDS-AS4         TO ICACTDS  OF DCLTAF
                    MOVE W-CDIVEM-AS4          TO CDIVEM   OF DCLTA
                    MOVE TAB-DPAGCES(IND-DP)   TO W-TAB-DPAGCES
           END-EVALUATE.
      *
TEST       DISPLAY 'CINDICIZ OF DCLTAF   =' CINDICIZ OF DCLTAF
TEST       DISPLAY 'ICACTDS  OF DCLTAF   =' ICACTDS  OF DCLTAF
TEST       DISPLAY 'CDIVEM   OF DCLTA    =' CDIVEM   OF DCLTA
TEST       DISPLAY 'W-TAB-DPAGCES        =' W-TAB-DPAGCES
           PERFORM CNTR-ESISTENZA-CED.
      *
      *----------------------------------------------------------------*
       CNTR-ESISTENZA-CED.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CNTR-ESISTENZA-CED          '.
      *
           EXEC SQL
                SELECT A.CNAZTIT
                     , A.CSPTIT
                  INTO :DCLTA.CNAZTIT
                     , :DCLTA.CSPTIT
                  FROM TA    A
                     , TATS  B
                     , TAF   C
                     , CPPO  D
                WHERE A.CNAZTIT  = B.CNAZTIT
                  AND A.CSPTIT   = B.CSPTIT
                  AND A.CNAZTIT  = C.CNAZTIT
                  AND A.CSPTIT   = C.CSPTIT
                  AND A.CNAZTIT  = D.CNAZTIT
                  AND A.CSPTIT   = D.CSPTIT
                  AND A.CDIVEM   = :DCLTA.CDIVEM
                  AND C.ICACTDS  = :DCLTAF.ICACTDS
                  AND C.CINDICIZ = :DCLTAF.CINDICIZ
                  AND   CSTRIP   = '3'
                  AND   CTCESPAG = '2'
                  AND   DPAGCES  = :W-TAB-DPAGCES
                WITH UR
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    SET  TRATTA-DP-OK         TO TRUE
                    MOVE CNAZTIT OF DCLTA     TO W-CNAZTIT-CED
                    MOVE CSPTIT  OF DCLTA     TO W-CSPTIT-CED
TEST                DISPLAY 'TITOLO SU PPO-CPPO'
TEST                DISPLAY 'CNAZTIT OF DCLTA =' CNAZTIT OF DCLTA
TEST                DISPLAY 'CSPTIT  OF DCLTA =' CSPTIT  OF DCLTA
                    ADD 1                     TO W-CONTA-CED
               WHEN +100
NEWC  *             MOVE 1001                 TO IND-DP
TEST                DISPLAY 'TITOLO NON TROVATO SU PPO-CPPO'
NEWC                IF SW-SL-SI
TEST                   DISPLAY 'TITOLO SHORT-LONG '
TEST                   DISPLAY 'W-CONTA-CED  =    ' W-CONTA-CED
NEWC                   ADD 1                  TO W-CONTA-CED
NEWC                ELSE
TEST                   DISPLAY 'TITOLO NON SHORT-L' W-CONTA-CED
NEWC                   MOVE 1001              TO IND-DP
NEWC                END-IF
               WHEN OTHER
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-SELECT TA-TAF-TATS-CPPO'
                                                    TO W-DESCR1
                    STRING 'CNAZTIT ' CNAZTIT       OF DCLTA
                           ' CSPTIT ' CSPTIT        OF DCLTA
                    DELIMITED BY SIZE             INTO W-DESCR2
TEST                DISPLAY 'ERRORE DB2-SELECT TA-TAF  '
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       CNTR-ESISTENZA-TATS.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - CNTR-ESISTENZA-TATS         '.
      *
           INITIALIZE DCLTATS
                      W-CNTR-TATS
                      W-APPOGGIO-AS7.
      *
TEST       DISPLAY 'W-CNAZTIT-PADRE-AS4 =' W-CNAZTIT-PADRE-AS4
TEST       DISPLAY 'W-CSPTIT-PADRE-AS4  =' W-CSPTIT-PADRE-AS4
           MOVE W-CNAZTIT-PADRE-AS4      TO CNAZTIT  OF DCLTATS.
           MOVE W-CSPTIT-PADRE-AS4       TO CSPTIT   OF DCLTATS.
           MOVE '1'                      TO CSTRIP   OF DCLTATS.
      *
           EXEC SQL
              SELECT CNAZTIT
                    ,CSPTIT
                    ,CSTRIP
                    ,CTMAUP
                    ,CNAZCIBR
                    ,CSPTCIBR
                INTO :DCLTATS.CNAZTIT
                    ,:DCLTATS.CSPTIT
                    ,:DCLTATS.CSTRIP
                    ,:DCLTATS.CTMAUP
                    ,:DCLTATS.CNAZCIBR
                    ,:DCLTATS.CSPTCIBR
                FROM TATS
               WHERE CNAZTIT  = :DCLTATS.CNAZTIT
                 AND CSPTIT   = :DCLTATS.CSPTIT
                 AND CSTRIP   = :DCLTATS.CSTRIP
                WITH UR
           END-EXEC.
      *
           EVALUATE SQLCODE
               WHEN ZERO
                    SET CNTR-TATS-OK          TO TRUE
                    MOVE CNAZCIBR OF DCLTATS  TO W-CNAZTIT-CIBR-AS7
                    MOVE CSPTCIBR OF DCLTATS  TO W-CSPTIT-CIBR-AS7
TEST  *             DISPLAY 'W-CNAZTIT-CIBR-AS7 =' W-CNAZTIT-CIBR-AS7
TEST  *             DISPLAY 'W-CSPTIT-CIBR-AS7  =' W-CSPTIT-CIBR-AS7
               WHEN +100
TEST                DISPLAY 'ISIN NON IN TATS'         SQLCODE
TEST                DISPLAY 'TITOLO        = '         W-ISIN-PADRE-AS4
                    SET CNTR-TATS-KO          TO TRUE
               WHEN OTHER
                    INITIALIZE W-DESCR1 W-DESCR2
                    MOVE 'ERRORE DB2-SELECT TATS  ' TO W-DESCR1
                    MOVE W-ISIN-PADRE-AS4           TO W-DESCR2
TEST                DISPLAY 'ERRORE DB2-SELECT TATS    '
                    PERFORM FINE-ANOMALA
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       INNESCA-INVIO.
      *----------------------------------------------------------------*
      *
TEST       DISPLAY 'WSOC83 - INNESCA-INVIO              '.
      *
           INITIALIZE  RNC14I.
      *
           MOVE 'WSOC83  '                     TO RNC14I-PGM-CHIAMANTE.
           MOVE 'ANBC83  '                     TO RNC14I-PGM-CHIAMATO.
      *
           EVALUATE W-CSTRIP
               WHEN '1'
                    MOVE W-CNAZTIT-F04         TO W-PAR-83-CNAZTIT
                    MOVE W-CSPTIT-F04          TO W-PAR-83-CSPTIT
                    MOVE W-CHKDGTIT-F04        TO W-PAR-83-CHKDGTIT
                    MOVE W-CINDICIZ-F04        TO W-PAR-83-CINDICIZ
                    MOVE W-ICACTDS-F04         TO W-PAR-83-ICACTDS
               WHEN '3'
                    MOVE W-CNAZTIT-PADRE-AS4   TO W-PAR-83-CNAZTIT
                    MOVE W-CSPTIT-PADRE-AS4    TO W-PAR-83-CSPTIT
                    MOVE W-CHKDGTIT-PADRE-AS4  TO W-PAR-83-CHKDGTIT
                    MOVE W-CINDICIZ-AS4        TO W-PAR-83-CINDICIZ
                    MOVE W-ICACTDS-AS4         TO W-PAR-83-ICACTDS
           END-EVALUATE.
      *
           MOVE W-PAR-83                 TO RNC14I-PARAM(1).

      * -- LANCIA TRANSAZIONE UTJ0 PER ESECUZIONE PROCEDURA BATCH
           EXEC CICS START TRANSID ('UTJ0')
                              FROM (RNC14I)
                            LENGTH (LENGTH OF RNC14I)
           END-EXEC.

      *----------------------------------------------------------------*
       CONSOLIDAMENTO-DATI.
      *----------------------------------------------------------------*
      *
           EXEC CICS SYNCPOINT   END-EXEC.
      *
      *----------------------------------------------------------------*
       SEGNALAZIONE-FINE-OK-TASK.
      *----------------------------------------------------------------*
      *
           PERFORM SEGNALA-MONITOR-APRE-DIS.
      *
      *----------------------------------------------------------------*
       OPERAZIONI-FINALI.
      *----------------------------------------------------------------*
      *
      *    IF STATO-TRANSAZIONE-OK
              PERFORM SEGNALAZIONE-FINE-OK-TASK
      *    END-IF.

           PERFORM CONSOLIDAMENTO-DATI.

      *----------------------------------------------------------------*
       FINE-ANOMALA.
      *----------------------------------------------------------------*

           PERFORM SALVA-DATI-SYS.

           PERFORM CONTROAGGIORNAMENTO-DATI.

           SET FINE-ABEND           TO TRUE.

           IF W-NUM-MSG-ELAB NOT = ZEROES
           AND WSL-OK
              PERFORM GESTIONE-WSL
           END-IF.

           PERFORM SEGNALA-MONITOR-APRE-ABE.

           PERFORM COMPOSIZIONE-SEGNALAZIONE.

           PERFORM GESTIONE-ABEND

           IF  LETTURA-TD-DONE
           AND NO-FINE-CODA-TD
              PERFORM RESTART-TRANSID
           END-IF.

           PERFORM CONSOLIDAMENTO-DATI.
           PERFORM CHIUDI-TRAN.

      *----------------------------------------------------------------*
       SALVA-DATI-SYS.
      *----------------------------------------------------------------*

           MOVE SQLCODE                  TO W-SQLCODE.

           MOVE EIBFN                    TO IOAREA.
           MOVE 2                        TO LUNGHEZZA.
           CALL 'SYSPUT06' USING FUNZIONE LUNGHEZZA IOAREA.
           MOVE IOAREA                   TO W-EIBFN.

           MOVE EIBRCODE                 TO IOAREA.
           MOVE 6                        TO LUNGHEZZA.
           CALL 'SYSPUT06' USING FUNZIONE LUNGHEZZA IOAREA.
           MOVE IOAREA                   TO W-EIBRCODE.

      *----------------------------------------------------------------*
       CONTROAGGIORNAMENTO-DATI.
      *----------------------------------------------------------------*

           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.
      *
      *-----------------------------------------------------------------
       GESTIONE-WSL.
      *-----------------------------------------------------------------
TEST       DISPLAY 'WSOC83 - GESTIONE-WSL         '.
      *
           PERFORM LETTURA-WSL.

           IF WSL-OK
              PERFORM AGGIORNAMENTO-WSL
           END-IF.

           IF WSL-OK
              PERFORM INSERIMENTO-WSL
           END-IF.

      *-----------------------------------------------------------------
       LETTURA-WSL.
      *----------------------------------------------------------------*
TEST       DISPLAY 'WSOC83 - LETTURA-WSL          '.
      *
           MOVE W-IDN-RICH-MSG OF  W-AREA-KEYMSG-WS83
             TO PIDMTX OF DCLWSL.

TEST       DISPLAY 'PIDMTX OF DCLWSL = 'PIDMTX OF DCLWSL
      *
           EXEC SQL
                SELECT WTIMECAR
                      ,CRICOP
                      ,CTMINVOP
                      ,DRICMSG
                      ,ORICMSGU
                      ,OATCONCL
                      ,CSTMSG
                 INTO  :DCLWSL.WTIMECAR
                      ,:DCLWSL.CRICOP
                      ,:DCLWSL.CTMINVOP
                      ,:DCLWSL.DRICMSG
                      ,:DCLWSL.ORICMSGU
                      ,:DCLWSL.OATCONCL
                      ,:DCLWSL.CSTMSG
                 FROM WSL
                WHERE PIDMTX    = :DCLWSL.PIDMTX
                  AND CSTMSG    = 'C'
                  AND OATCONCL  =  0
                  AND CTMINVOP  = 'R'
                WITH UR
           END-EXEC.

           EVALUATE SQLCODE
               WHEN ZERO
V                   DISPLAY 'WS83 - WSL SEL: OK '
      *             CONTINUE
               WHEN OTHER
                    IF FINE-REGOLARE
V                   DISPLAY 'WS83 - WSL SEL: KO ' SQLCODE
                       SET WSL-KO TO TRUE
                       INITIALIZE W-DESCR1 W-DESCR2
                       MOVE 'ERRORE DB2 - LETTURA TAB. WSL' TO W-DESCR1
                       MOVE PIDMTX OF DCLWSL     TO W-PIDMTX-X
                       STRING 'PIDMTX: ' W-PIDMTX-X
                       DELIMITED BY SIZE INTO W-DESCR2
                       PERFORM FINE-ANOMALA
                    END-IF
           END-EVALUATE.

      *----------------------------------------------------------------*
       AGGIORNAMENTO-WSL.
      *----------------------------------------------------------------*

           IF FINE-ABEND
              MOVE 'E'            TO CSTMSG OF DCLWSL
           ELSE
              MOVE 'I'            TO CSTMSG OF DCLWSL
           END-IF.

           EXEC CICS ASKTIME END-EXEC.
           MOVE EIBTIME           TO OATCONCL OF DCLWSL.

           EXEC SQL
                UPDATE WSL
                SET OATCONCL    = :DCLWSL.OATCONCL
                   ,CSTMSG      = :DCLWSL.CSTMSG
                WHERE PIDMTX    = :DCLWSL.PIDMTX
                  AND WTIMECAR  = :DCLWSL.WTIMECAR
           END-EXEC.

           EVALUATE SQLCODE
               WHEN ZERO
V     *             DISPLAY 'WS83 - WSL UPD: OK '
                    CONTINUE
               WHEN OTHER
                    IF FINE-REGOLARE
V     *             DISPLAY 'WS83 - WSL UPD: KO ' SQLCODE
                       SET WSL-KO TO TRUE
                       INITIALIZE W-DESCR1 W-DESCR2
                       MOVE 'ERRORE DB2 - UPDATE TAB. WSL' TO W-DESCR1
                       MOVE PIDMTX OF DCLWSL TO W-PIDMTX-X
                       STRING 'PIDMTX: ' W-PIDMTX-X
                       DELIMITED BY SIZE INTO W-DESCR2
                       PERFORM FINE-ANOMALA
                    END-IF
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       INSERIMENTO-WSL.
      *----------------------------------------------------------------*

           MOVE ZERO                        TO OATCONCL  OF DCLWSL.
           MOVE 'C'                         TO CSTMSG    OF DCLWSL.

           IF FINE-REGOLARE
              MOVE EIBTIME                  TO ORICMSGU  OF DCLWSL
              MOVE '12930'                  TO CRICOP    OF DCLWSL
              MOVE DGIOR          OF DCLDGC TO DRICMSG   OF DCLWSL
              MOVE 'S'                      TO CTMINVOP  OF DCLWSL
           END-IF.

           EXEC SQL
                INSERT INTO WSL
                VALUES (:DCLWSL.PIDMTX
                       ,CURRENT TIMESTAMP
                       ,:DCLWSL.CRICOP
                       ,:DCLWSL.CTMINVOP
                       ,:DCLWSL.DRICMSG
                       ,:DCLWSL.ORICMSGU
                       ,:DCLWSL.OATCONCL
                       ,:DCLWSL.CSTMSG)
           END-EXEC.

           EVALUATE SQLCODE
               WHEN ZERO
V     *             DISPLAY 'WS83 - WSL INS: OK '
                    CONTINUE
               WHEN OTHER
                    IF FINE-REGOLARE
V     *             DISPLAY 'WS83 - WSL INS: KO ' SQLCODE
                       SET WSL-KO TO TRUE
                       INITIALIZE W-DESCR1 W-DESCR2
                       MOVE 'ERRORE DB2 - INSERT TAB. WSL' TO W-DESCR1
                       MOVE PIDMTX OF DCLWSL TO W-PIDMTX-X
                       STRING 'PIDMTX: ' W-PIDMTX-X
                       DELIMITED BY SIZE INTO W-DESCR2
                       PERFORM FINE-ANOMALA
                    END-IF
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       MEMORIZZA-ERRORE.
      *----------------------------------------------------------------*
           SET  ENTRA-CICLO-ERRORI TO TRUE

           PERFORM VARYING IND-ERR
              FROM 1 BY 1
              UNTIL ESCI-CICLO-ERRORI
                 IF W-ELE-IDC-ERR(IND-ERR) = SPACE
                    MOVE W-IDC      TO W-ELE-IDC(IND-ERR)
                    MOVE W-ERR      TO W-ELE-ERR(IND-ERR)
                    SET ESCI-CICLO-ERRORI TO TRUE
                 END-IF
           END-PERFORM.

      *-----------------------------------------------------------------
       COMPOSIZIONE-SEGNALAZIONE.
      *-----------------------------------------------------------------

           MOVE DGIOR OF DCLDGC         TO W-DATA-APPOGGIO.

           STRING 'MT-X ANOMALIA ELABORAZIONE FLUSSO ANS ' W-PRGNAME
           DELIMITED BY SIZE  INTO W-SEGN-RIGA (1).

           MOVE EIBTASKN             TO W-ZONED-7S.
           STRING 'N.RO TASK = ' W-ZONED-7S    ' '
                  'DATA: '       W-DATA-APPOGGIO(7:2) '/'
                                 W-DATA-APPOGGIO(5:2) '/'
                                 W-DATA-APPOGGIO(3:2)
                  ' ORA: '       W-HH-ABEND    ':'
                                 W-MM-ABEND    ':'
                                 W-SS-ABEND    ' '
                  DELIMITED BY SIZE  INTO W-SEGN-RIGA (2).

           STRING 'FUNZIONE=X' QUOTE W-EIBFN    QUOTE '   '
                  'R.C.=X'     QUOTE W-EIBRCODE QUOTE
                  DELIMITED BY SIZE  INTO W-SEGN-RIGA (3).

           MOVE 2                        TO LUNGHEZZA.
           CALL 'SYSPUT06' USING FUNZIONE LUNGHEZZA IOAREA.
           STRING 'SQLCODE= '  W-SQLCODE
                  DELIMITED BY SIZE  INTO W-SEGN-RIGA (4).

           STRING 'DATI CODA TD WS83: ' W-AREA-KEYMSG-WS83
                  DELIMITED BY SIZE  INTO W-SEGN-RIGA (5).

           MOVE W-DESCR1             TO W-SEGN-RIGA (6).
           MOVE W-DESCR2             TO W-SEGN-RIGA (7).
           MOVE W-DESCR3             TO W-SEGN-RIGA (8).
      *----------------------------------------------------------------
       GESTIONE-ABEND.
      *----------------------------------------------------------------

           MOVE SPACES               TO ARE33I.

           MOVE 'S0001'               TO ARE33I-COD-ARCH.

           MOVE W-DATA-APPOGGIO(7:2)  TO ARE33I-GGMSG
           MOVE W-DATA-APPOGGIO(5:2)  TO ARE33I-MMMSG
           MOVE W-DATA-APPOGGIO(3:2)  TO ARE33I-AAMSG

           MOVE W-ORA-ABEND-NUM       TO ARE33I-ORARIO.
           MOVE ZERO                  TO ARE33I-PROGRESSIVO.

           MOVE EIBTRNID              TO ARE33I-TRANSID.
           MOVE '0001'                TO ARE33I-GRUPPO.
      *    MOVE CCATAPPL OF DCLWSRR   TO ARE33I-CATAPI.
           MOVE 'MT-X'                TO ARE33I-CATAPI.

           MOVE W-TAB-SEGNALAZIONI    TO ARE33I-MESSAGGIO.

           MOVE W-PRGNAME               TO ARE33I-PGRMID.
           MOVE WSC83I-CMSGMTX          TO ARE33I-TIPOMSG.

           EXEC CICS
                LINK PROGRAM  ('ARMP3025')
                     COMMAREA (AREA-ARMP3025)
                     LENGTH   (LENGTH OF AREA-ARMP3025)
           END-EXEC.

           IF ARMP3025-COD-RIT NOT = ZEROS
              PERFORM YABEND-ARMP3025.
      *----------------------------------------------------------------
       YABEND-ARMP3025.
      *----------------------------------------------------------------

           PERFORM STAMPA-ERRORE.

           EXEC CICS ABEND ABCODE ('WS83') CANCEL END-EXEC.

      *----------------------------------------------------------------
       STAMPA-ERRORE.
      *----------------------------------------------------------------

           MOVE DGIOR OF DCLDGC           TO W-SEGN-DATA.

           MOVE W-HH-ABEND                TO W-SEGN-ORA-HH.
           MOVE W-MM-ABEND                TO W-SEGN-ORA-MM.
           MOVE W-SS-ABEND                TO W-SEGN-ORA-SS.
           MOVE EIBTRNID                  TO W-SEGN-TRANSID.
      *    MOVE CCATAPPL OF DCLWSRR       TO W-SEGN-CATAPP.
           MOVE 'MT-X'                    TO W-SEGN-CATAPP.
           MOVE EIBTASKN                  TO W-SEGN-TASKN.

           MOVE EIBTRMID                  TO W-TERMID.

           EXEC CICS ROUTE LIST (W-PARM-ROUTE) END-EXEC.
           EXEC CICS SEND TEXT FROM (W-SEGNALAZIONI) LENGTH (880)
                     PAGING   ACCUM   PRINT  L80  ERASE  END-EXEC.
           EXEC CICS SEND PAGE   END-EXEC.

      *----------------------------------------------------------------
       SICD001P-IMPOSTA-ACCDATE.
      *----------------------------------------------------------------

      *--- OTTENGO LA CURRENTE DATE DA CICS
           EXEC CICS ASKTIME
           ABSTIME(SICD-ABS-TIME)
           END-EXEC.
           EXEC CICS FORMATTIME
           ABSTIME(SICD-ABS-TIME)
           YYMMDD(SICD-ACCDATE)
           END-EXEC.
      *----------------------------------------------------------------
       RESTART-TRANSID.
      *----------------------------------------------------------------

           EXEC CICS
                START TRANSID (EIBTRNID)
           END-EXEC.
      *----------------------------------------------------------------
       CHIUDI-TRAN.
      *----------------------------------------------------------------
           EXEC CICS
                RETURN
           END-EXEC.
      * ----------------------------------------------------------------
           EXEC SQL INCLUDE RIC00I END-EXEC.
      * ----------------------------------------------------------------
      * ================>> FINE PROGRAMMA WSOC83 <<=================== *
      * ----------------------------------------------------------------
