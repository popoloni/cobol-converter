       ID DIVISION.                                                     00010000
                                                                        00020000
       PROGRAM-ID.      ARMP3025.                                       00030000
      *              PROGRAM CONVERTED BY                               00040000
      *              COBOL CONVERSION AID PO 5785-ABJ                   00050000
      *              CONVERSION DATE 01/15/98 15:48:06.                 00060000
      *DATE-WRITTEN.    MARZO  1988.                                    00070000
      *AUTHOR.          CANZI.                                          00080000
      *                                                                 00090000
      ***************************************************************   00100000
      *REMARKS.         M O N T E  T I T O L I                          00110000
      *                 ------------------------                        00120000
      *                 GESTIONE LOG ALLARMI TP.                        00130000
      *                 ARCHIVIAZIONE E STAMPA MESSAGGI ANOMALI.        00140000
      *                 -------------------------------------------     00150000
      *                 RICHIAMATO DA MAIN LIVELLO "1" PGM=ARMP3020     00160000
      *                 -------------------------------------------     00170000
      *                 E DA PROGRAMMI APPLICATIVI                      00180000
      *                 ---------------------------------------------   00190000
      *                 VERSIONE CON ARCHIVIAZIONE SEGNALAZIONI         00200000
      *                 ('C','S') SU DB2. (TRANS TES0 - PGM TESCU020)   00210000
      *                 E ARCHIVIAZIONE MESSAGGI                        00210100
      *                 SU MRI (TRANS MTS0 - PGM RIOC21)                00210300
      ***************************************************************   00220000
RG0316* INTRODOTTO LIMITE NUMERO SEGNALAZIONI DI ERRORE AL MINUTO   *   00220100
RG0316* PER CODICE TRANSAZIONE, ONDE EVITARE ECCESSIVO START        *   00220110
RG0316* DI TRANSAZIONI TES0 IN CASO DI ABEND CICLICI                *   00220120
      ***************************************************************   00220200
                                                                        00230000
       ENVIRONMENT DIVISION.                                            00240000
                                                                        00250000
       CONFIGURATION SECTION.                                           00260000
       SPECIAL-NAMES.                                                   00270000
           DECIMAL-POINT IS COMMA.                                      00280000
                                                                        00290000
       DATA DIVISION.                                                   00300000
                                                                        00310000
       WORKING-STORAGE SECTION.                                         00320000
                                                                        00330000
                                                                        00450000
       77  ESDS                  PIC S9(8) COMP  VALUE +0.              00460000
       77  LL80                  PIC S9(4) COMP  VALUE +80.             00470000
       77  LL95                  PIC S9(4) COMP  VALUE +95.             00480000
RG0316 77  W-ARCH-SEGN                 PIC X  VALUE SPACES.             00480100
RG0316     88  NO-ARCH-SEGN                    VALUE 'N'.               00480200
RG0316     88  SI-ARCH-SEGN                    VALUE 'S'.               00480300
RG0316 77  W-NUM-SEGN-MM               PIC S9(5) COMP-3 VALUE ZEROES.   00480500
RG0316*  -- CONCORDATO CON MAX/DARIO: RAGIONEVOLE NON + DI 1 SEGNALAZ.  00480600
RG0316*  --   OGNI 10 SECONDI PER TRANSAZIONE, CIOE' 6 AL MINUTO        00480601
RG0316 77  W-NUM-SEGN-MM-MAX           PIC S9(5) COMP-3 VALUE 6.        00480610
RG0316 77  W-SQLCODE                     PIC ---9.                      00480700
                                                                        00490000
       01  SICD-ABSOLUTE-P       PIC S9(15) COMP-3 VALUE ZERO.          00500000
       01  SICD-ABSOLUTE-N       PIC S9(15)  VALUE ZERO.                00510000
       01  SICD-ABSOLUTE-C   REDEFINES SICD-ABSOLUTE-N.                 00520000
           02 FILLER             PIC X(12).                             00530000
           02 SICD-ABSOLUTE-CC   PIC 9(2).                              00540000
           02 FILLER             PIC X.                                 00550000
       01  W-EIBTASKN            PIC 9(05).                             00560000
                                                                        00570000
DVPTES*01  WS-ILBOABN0           PIC X(8) VALUE 'ILBOABN0'.             00580000
DVPTES*01  WS-MTAUT999           PIC X(8) VALUE 'MTAUT999'.             00590000
                                                                        00600000
       01  SW-STAMPA-ERRORI      PIC 9.                                 00610000
           88 DA-STAMPARE                 VALUE 1.                      00620000
           88 NON-STAMPARE                VALUE 0.                      00630000
                                                                        00640000
       01  COM-CODARC            PIC X    VALUE SPACES.                 00650000
                                                                        00660000
       01  W-CODARC.                                                    00670000
           05 W-CODARC2.                                                00680000
              10 W-CODARC-LETT   PIC X(01).                             00690000
              10 W-CODARC-NUM    PIC X(01).                             00700000
           05 FILLER             PIC X(03).                             00710000
                                                                        00720000
       01  SW-NOTROV-T101        PIC 9    VALUE ZEROS.                  00730000
                                                                        00740000
      ***************************************************************   00750000
      *** CAMPI PER COSTRUIRE MSG-ERRORI DA PASSARE A PGM.CHIAMANTE**   00760000
      ***************************************************************   00770000
                                                                        00780000
       01  CMD-EIBFN               PIC X(4).                            00790000
       01  CMD-EIBRCODE            PIC X(12).                           00800000
       01  NR-ERR                  PIC 9.                               00810000
                                                                        00820000
       01  PARAMETRI.                                                   00830000
           02  FUNZIONE            PIC X     VALUE '1'.                 00840000
           02  LUNGHEZZA           PIC S9(4) COMP.                      00850000
           02  IOAREA              PIC X(20).                           00860000
                                                                        00870000
      ******************************************************************00880000
      *    RICERCA CODICI ARCHIVIAZIONE PER INDIRIZZAMENTO TESTO SU    *00890000
      *    STAMPANTE ASSOCIATA.                                        *00900000
      ******************************************************************00910000
           COPY ARET101.                                                00920000
                                                                        00930000
      ******************************************************************01000000
      *         AREE  DI  WORKING  RISERVATE  AL  ROUTING              *01010000
      ******************************************************************01020000
                                                                        01030000
       01  SEGNALAZIONE.                                                01040000
           05  RIGA1.                                                   01050000
               10 FILLER              PIC X(79).                        01060000
               10 CAMPO-FF-1          PIC X.                            01070000
           05  RIGA2.                                                   01080000
               10 FILLER              PIC X(79).                        01090000
               10 CR2                 PIC X.                            01100000
           05  RIGA3.                                                   01110000
               10 FILLER              PIC X.                            01120000
               10 CATAPP              PIC X(16).                        01130000
               10 FILLER              PIC X.                            01140000
               10 TUR-TESTATA         PIC X(27).                        01150000
               10 DESCR-DATA          PIC X(6).                         01160000
               10 DATA-SEG            PIC X(8).                         01170000
               10 DESCR-ORA           PIC X(6).                         01180000
               10 ORA-SEG             PIC X(8).                         01190000
               10 FILLER              PIC X(06).                        01200000
               10 CR3                 PIC X.                            01210000
           05  RIGA4.                                                   01220000
               10 FILLER              PIC X.                            01230000
               10 TRANS               PIC X(16).                        01240000
               10 FILLER              PIC X.                            01250000
               10 NUMTASK             PIC X(16).                        01260000
               10 FILLER              PIC X.                            01270000
               10 GRPAB               PIC X(16).                        01280000
               10 FILLER              PIC X.                            01290000
               10 PGM                 PIC X(14).                        01300000
               10 FILLER              PIC X(13).                        01310000
               10 CR4                 PIC X.                            01320000
           05  RIGA5.                                                   01330000
               10 FILLER              PIC X(79).                        01340000
               10 CR5                 PIC X.                            01350000
           05  RIGA6.                                                   01360000
               10 FILLER              PIC X(79).                        01370000
               10 CR6                 PIC X.                            01380000
           05  RIGA7.                                                   01390000
               10 FILLER              PIC X(79).                        01400000
               10 CR7                 PIC X.                            01410000
           05  RIGA8.                                                   01420000
               10 FILLER              PIC X(79).                        01430000
               10 CR8                 PIC X.                            01440000
           05  RIGA9.                                                   01450000
               10 FILLER              PIC X(79).                        01460000
               10 CR9                 PIC X.                            01470000
           05  RIGA10 OCCURS 45 TIMES.                                  01480000
               10 FILLER              PIC X(4).                         01490000
               10 SEGNAL              PIC X(75).                        01500000
               10 CR10                PIC X.                            01510000
                                                                        01520000
      ****************************************************              01530000
      *** LUNGHEZZA MASSIMA RIGA SEGNALAZIONE          ***              01540000
      ****************************************************              01550000
                                                                        01560000
       01  LL-MAX                     PIC 9(4) VALUE 1920.              01570000
                                                                        01580000
      *******************************************************           01590000
      **** LL-FIX = 80  X 9  (LUNGHEZZA RIGA X RIGHE FISSE **           01600000
      *******************************************************           01610000
       01  LL-FIX                     PIC 9(4) VALUE 720.               01620000
       01  LL-RIGA                    PIC 9(4) VALUE 80.                01630000
      ****************************************************              01640000
       01  LL-CALC                    PIC 9(4) VALUE ZEROES.            01650000
       01  LL-REALE                   PIC S9(4) COMP VALUE +4320.       01660000
      *01  LL-REALE                   PIC S9(4) COMP VALUE ZEROES.      01670000
       01  IND                        PIC S9(4) COMP VALUE ZEROS.       01680000
       01  IND1                       PIC S9(4) COMP VALUE ZEROS.       01690000
                                                                        01700000
       01  PARM-ROUTE.                                                  01710000
           05  TERMID                 PIC X(4).                         01720000
           05  FILLER                 PIC X(12)  VALUE SPACES.          01730000
           05  FILLER                 PIC S9(4)  COMP VALUE -1.         01740000
                                                                        01750000
      ****************************************************              01760000
      **** CARATTERE DI NEW LINE SU RIGHE              ***              01770000
      ****************************************************              01780000
       01  W-NL                       PIC 9(2)   COMP VALUE 21.         01790000
       01  R-NL REDEFINES W-NL.                                         01800000
           05  FILLER                 PIC X.                            01810000
           05  NL                     PIC X.                            01820000
                                                                        01830000
      ****************************************************              01840000
      **** CARATTERE DI SALTO PAGINA                   ***              01850000
      ****************************************************              01860000
       01  W-FF                       PIC 9(2)   COMP VALUE 12.         01870000
       01  R-FF REDEFINES W-FF.                                         01880000
           05  FILLER                 PIC X.                            01890000
           05  FF                     PIC X.                            01900000
                                                                        01910000
       01  W-SR                       PIC 9(2)   COMP VALUE 29.         01920000
       01  R-SR REDEFINES W-SR.                                         01930000
           05  FILLER                 PIC X.                            01940000
           05  SR                     PIC X.                            01950000
                                                                        01960000
       01  COM-UTIP0060               PIC 9(5) COMP-3 VALUE 0.          01970000
                                                                        01980000
       01  COM-INTESTM.                                                 01990000
           05  FILLER                 PIC X     VALUE SPACES.           02000000
           05  FILLER                 PIC X(31) VALUE SPACES.           02010000
           05  FILLER                 PIC X(10) VALUE 'MESSAGGIO-'.     02020000
           05  FILLER                 PIC X(11) VALUE 'LOG-ALLARMI'.    02030000
           05  FILLER                 PIC X(26) VALUE SPACES.           02040000
                                                                        02050000
       01  COM-DATA.                                                    02060000
           05  COM-GG                 PIC 99 VALUE 0.                   02070000
           05  COM-MM                 PIC 99 VALUE 0.                   02080000
           05  COM-AA                 PIC 99 VALUE 0.                   02090000
                                                                        02100000
       01  COM-DESCR-ORA              PIC X(6)  VALUE ' ORA= '.         02110000
                                                                        02120000
       01  COM-ORA                    PIC 9(6) VALUE 0.                 02130000
       01  FILLER REDEFINES COM-ORA.                                    02140000
           05  COM-HH                 PIC 99.                           02150000
           05  COM-MN                 PIC 99.                           02160000
           05  COM-SS                 PIC 99.                           02170000
                                                                        02180000
                                                                        02190000
       01  COM-DESCR-DATA.                                              02200000
           05  FILLER                 PIC X(06) VALUE 'DATA= '.         02210000
                                                                        02220000
       01  COM1-DATA.                                                   02230000
           05  COM1-GG                PIC XX.                           02240000
           05  FILLER                 PIC X  VALUE '/'.                 02250000
           05  COM1-MM                PIC XX.                           02260000
           05  FILLER                 PIC X  VALUE '/'.                 02270000
           05  COM1-AA                PIC XX.                           02280000
                                                                        02290000
       01  COM1-ORA.                                                    02300000
           05  COM1-HH                PIC XX.                           02310000
           05  FILLER                 PIC X  VALUE '.'.                 02320000
           05  COM1-MN                PIC XX.                           02330000
           05  FILLER                 PIC X  VALUE '.'.                 02340000
           05  COM1-SS                PIC XX.                           02350000
                                                                        02360000
       01  COM-TRANS.                                                   02370000
           05  FILLER                 PIC X(12) VALUE '* TRANSID = '.   02380000
           05  COM-TRANSID            PIC X(4).                         02390000
                                                                        02400000
       01  COM-CODA.                                                    02410000
           05  FILLER                 PIC X(12) VALUE 'GRUPPO-AB = '.   02420000
           05  COM-NOMECODA           PIC X(4).                         02430000
                                                                        02440000
       01  COM-PGM.                                                     02450000
           05  FILLER                 PIC X(6)  VALUE 'PGM = '.         02460000
           05  NPGM                   PIC X(8).                         02470000
                                                                        02480000
       01  COM-CATAPP.                                                  02490000
           05  FILLER                 PIC X(12) VALUE '* CAT-APP.= '.   02500000
           05  COM-CAP                PIC X(4).                         02510000
                                                                        02520000
       01  COM-TUR-TESTATA.                                             02530000
           05  FILLER                 PIC X(10) VALUE 'TUR-TEST= '.     02540000
           05  COM-TURT               PIC X(16).                        02550000
           05  FILLER                 PIC X.                            02560000
                                                                        02570000
       01  COM-TASKN.                                                   02580000
           05  FILLER                 PIC X(9)  VALUE 'N.TASK = '.      02590000
           05  NTASK                  PIC 9(7)  VALUE ZEROES.           02600000
                                                                        02610000
       01  COM-MITTENTE.                                                02620000
           05  FILLER                 PIC X     VALUE SPACES.           02630000
           05  FILLER                 PIC X(15) VALUE '* DATI ID-MITTE'.02640000
           05  FILLER                 PIC X(10) VALUE 'NTE =====>'.     02650000
           05  FILLER                 PIC X(12) VALUE '   ID-MIT = '.   02660000
           05  COM-CODMIT             PIC X(5)  VALUE SPACES.           02670000
           05  FILLER                 PIC X(12) VALUE '  LOC-MIT = '.   02680000
           05  COM-LOCMIT             PIC X(5)  VALUE SPACES.           02690000
           05  FILLER                 PIC X(12) VALUE '  NUM-MIT = '.   02700000
           05  COM-NUMMIT             PIC XX    VALUE SPACES.           02710000
           05  FILLER                 PIC X(05) VALUE SPACES.           02720000
                                                                        02730000
       01  COM-RICEVENTE.                                               02740000
           05  FILLER                 PIC X     VALUE SPACES.           02750000
           05  FILLER                 PIC X(15) VALUE '* DATI ID-RICEV'.02760000
           05  FILLER                 PIC X(10) VALUE 'ENTE ====>'.     02770000
           05  FILLER                 PIC X(12) VALUE '   ID-RIC = '.   02780000
           05  COM-CODRIC             PIC X(5)  VALUE SPACES.           02790000
           05  FILLER                 PIC X(12) VALUE '  LOC-RIC = '.   02800000
           05  COM-LOCRIC             PIC X(5)  VALUE SPACES.           02810000
           05  FILLER                 PIC X(12) VALUE '  NUM-RIC = '.   02820000
           05  COM-NUMRIC             PIC XX    VALUE SPACES.           02830000
           05  FILLER                 PIC X(05) VALUE SPACES.           02840000
                                                                        02850000
       01  COM-DATEMSG.                                                 02860000
           05  FILLER                 PIC X     VALUE SPACES.           02870000
           05  FILLER                 PIC X(12) VALUE '* DATAINT = '.   02880000
           05  COM-DATAINT            PIC 9(6)  VALUE ZEROES.           02890000
           05  FILLER                 PIC X(12) VALUE '  TIPOMSG = '.   02900000
           05  COM-TIPOMSG            PIC XXX   VALUE SPACES.           02910000
           05  FILLER                 PIC X(12) VALUE '  COD-ARC.= '.   02920000
           05  COM-CODARCHIV          PIC X(5)  VALUE SPACES.           02930000
           05  FILLER                 PIC X(12) VALUE '  N.PROGR.= '.   02940000
           05  COM-PROGRES            PIC 9(08) VALUE ZEROS.            02950000
           05  FILLER                 PIC X(8)  VALUE SPACES.           02960000
                                                                        02970000
       01  COM-DESCRMES.                                                02980000
           05  FILLER                 PIC X     VALUE SPACES.           02990000
           05  FILLER                 PIC X(30) VALUE SPACES.           03000000
           05  FILLER                 PIC X(10) VALUE ' DATI-MESS'.     03010000
           05  FILLER                 PIC X(06) VALUE 'AGGIO '.         03020000
           05  FILLER                 PIC X(32) VALUE SPACES.           03030000
                                                                        03040000
       01  COM-DESCRERR.                                                03050000
           05  FILLER                 PIC X     VALUE SPACES.           03060000
           05  FILLER                 PIC X(14) VALUE '* IDC-CODICI E'. 03070000
           05  FILLER                 PIC X(08) VALUE 'RRORI : '.       03080000
           05  COM-IDCCODER           PIC X(50) VALUE SPACES.           03090000
           05  FILLER                 PIC X(06) VALUE SPACES.           03100000
                                                                        03110000
       01  COM-DESCRANOM.                                               03120000
           05  FILLER                 PIC X     VALUE SPACES.           03130000
           05  FILLER                 PIC X(14) VALUE '* DESCRIZIONE '. 03140000
           05  FILLER                 PIC X(08) VALUE '      : '.       03150000
           05  COM-DESANOM            PIC X(50) VALUE SPACES.           03160000
           05  FILLER                 PIC X(06) VALUE SPACES.           03170000
                                                                        03180000
PASS   01  TAB-SEGNALAZIONI.                                            03190000
           02  O-TAB-SEGNALAZIONI      PIC X(800) VALUE SPACES.         03200000
           02  FILLER REDEFINES O-TAB-SEGNALAZIONI.                     03210000
               05  FILLER OCCURS 10.                                    03220000
                   10  FILLER          PIC X(05).                       03230000
                   10  O-SEGNALAZIONE  PIC X(50).                       03240000
                   10  FILLER          PIC X(25).                       03250000
PASS   01  AREA-SGNZ-24.                                                03260000
           02  AREA-SGNZ-1             PIC X(77) OCCURS 24.             03270000
PASS   01  W-IND                       PIC 9(03) VALUE ZERO.            03280000
PASS   01  W-IND2                      PIC 9(03) VALUE ZERO.            03290000
                                                                        03300000
PASS   01  MSG-RIGA-AST.                                                03310000
           05  FILLER         PIC X(76) VALUE ALL '-'.                  03320000
           05  FILLER         PIC X     VALUE SPACES.                   03330000
                                                                        03340000
PASS   01  MSG-RIGA1.                                                   03350000
           05  TIPO-MAPPA     PIC X(2).                                 03360000
           05  FILLER         PIC X(23) VALUE SPACES.                   03370000
           05  FILLER         PIC X(19) VALUE 'INTERROGAZIONE LOG '.    03380000
           05  FILLER         PIC X(7)  VALUE 'ALLARMI'.                03390000
           05  FILLER         PIC X(26) VALUE SPACES.                   03400000
                                                                        03410000
       01  AREA-SEGNALAZ.                                               03420000
           05  AREA-TESTO  OCCURS 45  PIC X(75).                        03430000
                                                                        03440000
       01  LL-SSCKKI                    PIC S9(4) COMP VALUE +2259.     03450000
                                                                        03460000
       01  AREA-SSCKKI.                                                 03470000
           02 SSCKKI-ERR-GRAVE    PIC X(02).                            03480000
           02 SSCKKI-NTASK        PIC X(04).                            03490000
           02 SSCKKI-NUM-MSG      PIC 9(03).                            03500000
           COPY SSCKKI.                                                 03510000
                                                                        03520000
RG0316     EXEC SQL INCLUDE TES900B END-EXEC.                           03520100
RG0316     EXEC SQL INCLUDE SQLCA   END-EXEC.                           03520110
                                                                        03520200
       01  INIZ-PGM PIC X(10) VALUE 'ARMP3025  '.                       03530000
                                                                        03540000
      ******************                                                03550000
       LINKAGE SECTION.                                                 03560000
      ******************                                                03570000
                                                                        03580000
       01  DFHCOMMAREA.                                                 03590000
           COPY ARE33I.                                                 03600000
           02 ARMP3025-COD-RIT PIC XX.                                  03610000
                                                                        03620000
      *********************                                             03630000
       PROCEDURE DIVISION.                                              03640000
      *********************                                             03650000
                                                                        03660000
      *****EXEC CICS SEND FROM(INIZ-PGM) LENGTH(10) ERASE END-EXEC.     03670000
      *****EXEC CICS RECEIVE END-EXEC.                                  03680000
                                                                        03690000
CR0400     EXEC CICS HANDLE CONDITION PGMIDERR (W003-ERRORE) END-EXEC.  03700000
                                                                        03710000
           EXEC CICS HANDLE CONDITION ERROR    (W005-ERRORE) END-EXEC.  03720000
                                                                        03730000
           EXEC CICS HANDLE ABEND  LABEL    (W006-ABEND)  END-EXEC.     03740000
                                                                        03760000
      **********************************************************        03770000
      * SE EIBCALEN = 0 SIGNIFICA CHE IL PROGRAMMA CHIAMANTE ***        03780000
      * NON HA PASSATO LA COMMAREA                           ***        03790000
      **********************************************************        03800000
                                                                        03810000
           IF EIBCALEN = ZEROES                                         03820000
              PERFORM W001-NO-COMMAREA                                  03830000
           END-IF.                                                      03840000
                                                                        03850000
       INIZIO.                                                          03860000
                                                                        03870000
           MOVE '00'   TO ARMP3025-COD-RIT.                             03880000
                                                                        03890000
           PERFORM ACQUISISCI-DATI THRU END-DATI.                       03900000
                                                                        03910000
           PERFORM RICERCA-XSTAMPA THRU END-RICERCA-XSTAMPA.            03920000
                                                                        03930000
           IF ARET101-RC EQUAL '2'                                      03940000
              PERFORM W003-RICHIESTA-ERRATA.                            03950000
                                                                        03960000
           IF ARET101-NO-TP                                             03970000
161199*       PERFORM AGGIORNA-PROG-DATAI THRU END-PROG-DATAI           03980000
161199        PERFORM ACQUISISCI-PROG THRU END-ACQUISISCI-PROG          03990000
              PERFORM ARCHIVIA THRU END-ARCHIVIA                        04000000
              GO TO FINE.                                               04010000
                                                                        04020000
           PERFORM CARICA-DATI-SEGNALAZ.                                04030000
                                                                        04040000
           EVALUATE TRUE                                                04050000
               WHEN ARET101-FL-ISP                                      04060000
                    MOVE ARET101-TERM-ISP TO TERMID                     04070000
                    PERFORM SCRIVI-TESTO  THRU END-SCRIVI-TESTO         04080000
               WHEN ARET101-FL-ORG                                      04090000
                    MOVE ARET101-TERM-ORG TO TERMID                     04100000
                    PERFORM SCRIVI-TESTO  THRU END-SCRIVI-TESTO         04110000
               WHEN ARET101-ENTRAMBE                                    04120000
                    MOVE ARET101-TERM-ISP TO TERMID                     04130000
                    PERFORM SCRIVI-TESTO  THRU END-SCRIVI-TESTO         04140000
                    MOVE ARET101-TERM-ORG TO TERMID                     04150000
                    PERFORM SCRIVI-TESTO  THRU END-SCRIVI-TESTO         04160000
           END-EVALUATE.                                                04170000
                                                                        04180000
           PERFORM ARCHIVIA        THRU END-ARCHIVIA.                   04190000
                                                                        04200000
      *-------------------------*                                       04210000
      * RITORNO A PGM. CHIAMANTE                                        04220000
      *-------------------------*                                       04230000
       FINE.                                                            04240000
                                                                        04250000
           EXEC CICS RETURN END-EXEC.                                   04260000
           GOBACK.                                                      04270000
                                                                        04280000
      *-----------------------------------------------------*           04290000
      * ACQUISISCE ORA PER SCRITTURA SU FILE LOG-ALLARMI  ***           04300000
      *-----------------------------------------------------*           04310000
                                                                        04320000
       ACQUISISCI-DATI.                                                 04330000
                                                                        04340000
           MOVE ZEROS          TO SW-STAMPA-ERRORI.                     04350000
                                                                        04360000
           EXEC CICS ASKTIME END-EXEC.                                  04370000
                                                                        04380000
           MOVE EIBTIME        TO COM-ORA.                              04390000
           MOVE COM-ORA        TO ARE33I-ORARIO.                        04400000
                                                                        04410000
           MOVE EIBDATE        TO COM-UTIP0060.                         04420000
           CALL 'UTIP0060' USING COM-UTIP0060 COM-DATA.                 04430000
                                                                        04440000
           MOVE EIBTRNID       TO COM-TRANSID.                          04450000
           MOVE EIBTASKN       TO NTASK.                                04460000
                                                                        04470000
           MOVE SPACES         TO SEGNALAZIONE.                         04480000
           MOVE ARE33I-GRUPPO  TO COM-NOMECODA.                         04490000
           MOVE ARE33I-CATAPI  TO COM-CAP.                              04500000
           MOVE ARE33I-PGRMID  TO NPGM.                                 04510000
                                                                        04520000
       END-DATI.                                                        04530000
           EXIT.                                                        04540000
                                                                        04550000
      *--------------------------------------------------------*        04560000
      * RICERCA CODICE DI ARCHIVIAZIONE SU ARET101 PER       ***        04570000
      * RICAVARE STAMPANTE ASSOCIATA E CODICI DA SEGNALARE   ***        04580000
      *--------------------------------------------------------*        04590000
       RICERCA-XSTAMPA.                                                 04600000
                                                                        04610000
           IF ARE33I-COD-ARCH = LOW-VALUE OR SPACES                     04620000
             MOVE  'XXXXX'        TO ARET101-CODARCHI                   04630000
           ELSE                                                         04640000
             MOVE ARE33I-COD-ARCH TO ARET101-CODARCHI                   04650000
           END-IF.                                                      04660000
                                                                        04670000
       SE-NONTROV.                                                      04680000
                                                                        04690000
           IF SW-NOTROV-T101 = 2                                        04700000
              MOVE ZEROS TO SW-NOTROV-T101                              04710000
              GO TO END-RICERCA-XSTAMPA.                                04720000
                                                                        04730000
           EXEC CICS HANDLE CONDITION NOTOPEN (W004-NOTOPEN) END-EXEC.  04740000
                                                                        04750000
           MOVE  'RND'         TO ARET101-FUNZ.                         04760000
                                                                        04770000
           EXEC CICS LINK PROGRAM ('SYSPTAB5')                          04780000
                     COMMAREA (ARET101)                                 04790000
                     LENGTH (LENGTH OF ARET101)                         04800000
           END-EXEC.                                                    04810000
                                                                        04820000
           IF ARET101-RC   = ZEROS                                      04830000
              NEXT SENTENCE                                             04840000
           ELSE                                                         04850000
              MOVE 'XXXXX' TO ARET101-CODARCHI                          04860000
              ADD    1     TO SW-NOTROV-T101                            04870000
              GO TO SE-NONTROV                                          04880000
           END-IF.                                                      04890000
                                                                        04900000
       END-RICERCA-XSTAMPA.                                             04910000
           EXIT.                                                        04920000
                                                                        04930000
      *----------------------------------------------*                  04940000
      *** PREPARA I DATI PER LA STAMPA DEL MESSAGGIO                    04950000
      *----------------------------------------------*                  04960000
                                                                        04970000
       CARICA-DATI-SEGNALAZ.                                            04980000
                                                                        04990000
      *--------------------------------------------------------*        05000000
      ***  IMPOSTA I CARATTERI DI SALTO PAGINA SU PRIMA RIGA ***        05010000
      *--------------------------------------------------------*        05020000
                                                                        05030000
      *    MOVE FF             TO CAMPO-FF-1.                           05040000
                                                                        05050000
      *--------------------------------------------------------*        05060000
      ***  IMPOSTA SPACES PER CODICI ARCHIVIAZIONI "C" E "S" ***        05070000
      *--------------------------------------------------------*        05080000
                                                                        05090000
           MOVE ARE33I-COD-ARCH TO COM-CODARC.                          05100000
                                                                        05110000
           IF COM-CODARC = 'C' OR 'S' OR 'D'                            05120000
              MOVE SPACES TO COM-CODMIT COM-LOCMIT COM-NUMMIT           05130000
                             COM-CODRIC COM-LOCRIC COM-NUMRIC           05140000
                             COM-TURT.                                  05150000
                                                                        05160000
      *-----------------------------------*                             05170000
      ***       PREPARA  RIGA  -2-      ***                             05180000
      *-----------------------------------*                             05190000
                                                                        05200000
           MOVE COM-INTESTM    TO RIGA2.                                05210000
                                                                        05220000
      *-----------------------------------*                             05230000
      ***       PREPARA  RIGA  -3-      ***                             05240000
      *-----------------------------------*                             05250000
                                                                        05260000
           MOVE COM-CATAPP     TO CATAPP.                               05270000
                                                                        05280000
           MOVE ARE33I-TUR-TESTATA TO COM-TURT.                         05290000
                                                                        05300000
           IF COM-CODARC = 'C' OR 'S' OR 'D'                            05310000
              MOVE SPACES TO COM-TURT.                                  05320000
      *    ENDIF                                                        05330000
                                                                        05340000
           MOVE COM-TUR-TESTATA    TO TUR-TESTATA.                      05350000
                                                                        05360000
           MOVE COM-DESCR-DATA TO DESCR-DATA.                           05370000
           MOVE COM-GG         TO COM1-GG.                              05380000
           MOVE COM-MM         TO COM1-MM.                              05390000
           MOVE COM-AA         TO COM1-AA.                              05400000
           MOVE COM1-DATA      TO DATA-SEG.                             05410000
                                                                        05420000
           MOVE COM-DESCR-ORA  TO DESCR-ORA.                            05430000
           MOVE COM-HH         TO COM1-HH.                              05440000
           MOVE COM-MN         TO COM1-MN.                              05450000
           MOVE COM-SS         TO COM1-SS.                              05460000
           MOVE COM1-ORA       TO ORA-SEG.                              05470000
                                                                        05480000
      *-----------------------------------*                             05490000
      ****      PREPARA  RIGA  -4-      ***                             05500000
      *-----------------------------------*                             05510000
                                                                        05520000
           MOVE COM-CODA       TO GRPAB.                                05530000
           MOVE COM-PGM        TO PGM.                                  05540000
           MOVE COM-TRANS      TO TRANS.                                05550000
           MOVE COM-TASKN      TO NUMTASK.                              05560000
                                                                        05570000
      *-----------------------------------*                             05580000
      ****      PREPARA  RIGA  -5-      ***                             05590000
      *-----------------------------------*                             05600000
                                                                        05610000
           MOVE ARE33I-IDABMIT    TO COM-CODMIT                         05620000
           MOVE ARE33I-ID-LOCMIT  TO COM-LOCMIT                         05630000
           MOVE ARE33I-ID-NUMMIT  TO COM-NUMMIT.                        05640000
                                                                        05650000
           IF COM-CODARC = 'C' OR 'S' OR 'D'                            05660000
              MOVE SPACES TO COM-CODMIT COM-LOCMIT COM-NUMMIT           05670000
           END-IF.                                                      05680000
                                                                        05690000
           MOVE COM-MITTENTE      TO RIGA5.                             05700000
                                                                        05710000
      *-----------------------------------*                             05720000
      ****      PREPARA  RIGA  -6-      ***                             05730000
      *-----------------------------------*                             05740000
                                                                        05750000
           MOVE ARE33I-IDABRIC    TO COM-CODRIC                         05760000
           MOVE ARE33I-ID-LOCRIC  TO COM-LOCRIC                         05770000
           MOVE ARE33I-ID-NUMRIC  TO COM-NUMRIC.                        05780000
                                                                        05790000
           IF COM-CODARC = 'C' OR 'S' OR 'D'                            05800000
              MOVE SPACES TO COM-CODRIC COM-LOCRIC COM-NUMRIC           05810000
           END-IF.                                                      05820000
                                                                        05830000
           MOVE COM-RICEVENTE     TO RIGA6.                             05840000
                                                                        05850000
      *-----------------------------------*                             05860000
      ****      PREPARA  RIGA  -7-      ***                             05870000
      *-----------------------------------*                             05880000
                                                                        05890000
161199     PERFORM ACQUISISCI-PROG THRU END-ACQUISISCI-PROG.            05900000
                                                                        05910000
           MOVE ARE33I-DATA-INTARCH      TO COM-DATAINT.                05920000
           MOVE ARE33I-TIPOMSG           TO COM-TIPOMSG.                05930000
           MOVE ARE33I-COD-ARCH          TO COM-CODARCHIV.              05940000
161199     MOVE SICD-ABSOLUTE-CC         TO COM-PROGRES(1:2).           05950000
220301     MOVE ARE33I-TUR-TESTATA(9:1)  TO COM-PROGRES(3:1).           05960000
061299     MOVE EIBTASKN                 TO W-EIBTASKN.                 05970000
220301     MOVE W-EIBTASKN               TO COM-PROGRES(4:5).           05980000
           MOVE COM-DATEMSG              TO RIGA7.                      05990000
                                                                        06000000
      *-----------------------------------*                             06010000
      ****      PREPARA  RIGA  -8-      ***                             06020000
      *-----------------------------------*                             06030000
                                                                        06040000
           IF ARE33I-ERRORIX NOT = SPACES                               06050000
              PERFORM CERCA-CODERR THRU END-CODERR                      06060000
           END-IF.                                                      06070000
                                                                        06080000
           MOVE COM-DESCRERR   TO RIGA8.                                06090000
                                                                        06100000
      *-----------------------------------*                             06110000
      ****      PREPARA  RIGA  -9-      ***                             06120000
      *-----------------------------------*                             06130000
                                                                        06140000
           EVALUATE COM-CODARC                                          06150000
              WHEN 'B'                                                  06160000
               MOVE '*** RICEZIONE MSG. LIQUIDAZIONE TITOLI DA B.I. ***'06170000
                                                TO COM-DESANOM          06180000
              WHEN 'C'                                                  06190000
               MOVE  1  TO SW-STAMPA-ERRORI                             06200000
               MOVE '*** SEGNALAZIONI DI SERVIZIO ***'                  06210000
                                                TO COM-DESANOM          06220000
              WHEN 'D'                                                  06230000
               MOVE  1  TO SW-STAMPA-ERRORI                             06240000
               MOVE '*** SEGNALAZIONI SERVIZIO DVP *** '                06250000
                                                TO COM-DESANOM          06260000
              WHEN 'M'                                                  06270000
               MOVE  1  TO SW-STAMPA-ERRORI                             06280000
               MOVE '*** RICEZIONE MESSAGGI LIBERI ***'                 06290000
                                                     TO COM-DESANOM     06300000
              WHEN 'R'                                                  06310000
               MOVE  1  TO SW-STAMPA-ERRORI                             06320000
               MOVE '*** RICEZIONE MESSAGGI ERRATI (00MTE-RE01) ***'    06330000
                                                     TO COM-DESANOM     06340000
              WHEN 'J'                                                  06350000
               MOVE  1  TO SW-STAMPA-ERRORI                             06360000
               MOVE '*** RICEZIONE MESSAGGI DEL C.A.M.T. ***'           06370000
                                                     TO COM-DESANOM     06380000
              WHEN 'S'                                                  06390000
               MOVE  1  TO SW-STAMPA-ERRORI                             06400000
               MOVE '*** ERRORI CICS - MODULI DI CONTROLLO ***'         06410000
                                                     TO COM-DESANOM     06420000
              WHEN 'X'                                                  06430000
               MOVE  1  TO SW-STAMPA-ERRORI                             06440000
               MOVE '*** INVIO MESSAGGI CREATI DA BATCH ***'            06450000
                                               TO COM-DESANOM           06460000
              WHEN 'W'                                                  06470000
               MOVE  1  TO SW-STAMPA-ERRORI                             06480000
               MOVE '*** RICEZIONE MESSAGI ERRATI DA BANCA ***'         06490000
                                                     TO COM-DESANOM     06500000
           END-EVALUATE.                                                06510000
                                                                        06520000
290101*    IF ARE33I-COD-ARCH = 'E0002' OR 'E1002' OR 'E2002'           06530000
290101*       MOVE '*** RIFIUTO MESSAGGIO A ENTE GESTORE ***'           06540000
290101*                                         TO COM-DESANOM          06550000
290101*    END-IF.                                                      06560000
           IF ARE33I-COD-ARCH = 'L0003' OR 'L1003' OR 'L2003'           06570000
              MOVE '*** RIFIUTO MESSAGGIO A BANCA ***' TO COM-DESANOM   06580000
           END-IF.                                                      06590000
                                                                        06600000
           MOVE COM-DESCRANOM  TO RIGA9.                                06610000
                                                                        06620000
      *--------------------------------------------------------*        06630000
      ***  IMPOSTA IL CARATTERE DI NEW LINE SU RIGHE FISSE   ***        06640000
      *--------------------------------------------------------*        06650000
                                                                        06660000
           MOVE NL             TO CR2 CR3 CR4 CR5 CR6 CR7 CR8           06670000
                                  CR9.                                  06680000
                                                                        06690000
      *--------------------------------------------------------*        06700000
      ***  PREPARA RIGHE SUCCESSIVE                          ***        06710000
      ***  SE E' UN RIFIUTO "E-L"   SPOSTA CORPO-MESSAGGIO   ***        06720000
      ***  SE E' RICEZIONE  "J-R-W" DI CATAPP "RE01"         ***        06730000
      ***                           SPOSTA CORPO-MESSAGGIO   ***        06740000
      ***  SE E' RICEZIONE  "B"     DI CATAPP "RT01" MSG-X01-***        06750000
      ***                           SPOSTA CORPO-MESSAGGIO   ***        06760000
      ***  SE INVIO DA SIA  "X"     SPOSTA CORPO-MESSAGGIO   ***        06770000
      ***  SE E' SEGNALAZ.  "C-S"   SPOSTA TESTATA-CORPO MSG ***        06780000
      *--------------------------------------------------------*        06790000
                                                                        06800000
           IF COM-CODARC = 'E' OR 'L' OR 'R' OR 'X' OR 'W' OR 'J'       06810000
                        OR 'B' OR 'M'                                   06820000
              MOVE ARE33I-MSG-VARIABILE TO AREA-SEGNALAZ                06830000
           ELSE                                                         06840000
              MOVE ARE33I-MESSAGGIO     TO AREA-SEGNALAZ                06850000
           END-IF.                                                      06860000
                                                                        06870000
           MOVE ZEROES             TO IND IND1.                         06880000
           MOVE 45                 TO IND.                              06890000
                                                                        06900000
           IF AREA-SEGNALAZ = SPACES  OR LOW-VALUE                      06910000
              MOVE ZERO TO IND IND1                                     06920000
           ELSE                                                         06930000
              PERFORM CARICA-TAB-SEGN UNTIL IND < 1                     06940000
           END-IF.                                                      06950000
                                                                        06960000
      *------------------------------------------------------*          06970000
      ***  COMPARA IDC-CODICI ERRORI TABELLA - AREA ERRORI  **          06980000
      ***  SE ARET101-CODERT1 = SPACES STAMPA TUTTO         **          06990000
      *------------------------------------------------------*          07000000
                                                                        07010000
       CERCA-CODERR.                                                    07020000
                                                                        07030000
           MOVE ZEROS TO SW-STAMPA-ERRORI.                              07040000
                                                                        07050000
           IF  ARE33I-CODERR (1) = SPACES OR LOW-VALUE                  07060000
               GO TO END-CODERR.                                        07070000
                                                                        07080000
           IF  ARET101-CODERT1 = SPACES                                 07090000
               MOVE ARE33I-ERRORIX TO COM-IDCCODER                      07100000
               MOVE      1        TO SW-STAMPA-ERRORI                   07110000
               GO TO END-CODERR.                                        07120000
                                                                        07130000
           IF (ARE33I-CODERR (1) = ARET101-CODERT1) OR                  07140000
              (ARE33I-CODERR (1) = ARET101-CODERT2) OR                  07150000
              (ARE33I-CODERR (1) = ARET101-CODERT3) OR                  07160000
              (ARE33I-CODERR (1) = ARET101-CODERT4) OR                  07170000
              (ARE33I-CODERR (1) = ARET101-CODERT5) OR                  07180000
              (ARE33I-CODERR (1) = ARET101-CODERT6) OR                  07190000
              (ARE33I-CODERR (1) = ARET101-CODERT7) OR                  07200000
              (ARE33I-CODERR (1) = ARET101-CODERT8)                     07210000
               MOVE ARE33I-ERRORIX TO COM-IDCCODER                      07220000
               MOVE      1        TO SW-STAMPA-ERRORI                   07230000
               GO TO END-CODERR.                                        07240000
                                                                        07250000
           IF  ARE33I-CODERR (2) = SPACES                               07260000
               GO TO END-CODERR.                                        07270000
                                                                        07280000
           IF ARE33I-CODERR (2) = ARET101-CODERT1                       07290000
                               OR ARET101-CODERT2                       07300000
                               OR ARET101-CODERT3                       07310000
                               OR ARET101-CODERT4                       07320000
                               OR ARET101-CODERT5                       07330000
                               OR ARET101-CODERT6                       07340000
                               OR ARET101-CODERT7                       07350000
                               OR ARET101-CODERT8                       07360000
               MOVE ARE33I-ERRORIX TO COM-IDCCODER                      07370000
               MOVE      1        TO SW-STAMPA-ERRORI                   07380000
               GO TO END-CODERR.                                        07390000
                                                                        07400000
           IF  ARE33I-CODERR (3) = SPACES                               07410000
               GO TO END-CODERR.                                        07420000
                                                                        07430000
           IF ARE33I-CODERR (3) = ARET101-CODERT1                       07440000
                               OR ARET101-CODERT2                       07450000
                               OR ARET101-CODERT3                       07460000
                               OR ARET101-CODERT4                       07470000
                               OR ARET101-CODERT5                       07480000
                               OR ARET101-CODERT6                       07490000
                               OR ARET101-CODERT7                       07500000
                               OR ARET101-CODERT8                       07510000
               MOVE ARE33I-ERRORIX TO COM-IDCCODER                      07520000
               MOVE      1        TO SW-STAMPA-ERRORI                   07530000
               GO TO END-CODERR.                                        07540000
                                                                        07550000
           IF  ARE33I-CODERR (4) = SPACES                               07560000
               GO TO END-CODERR.                                        07570000
                                                                        07580000
           IF ARE33I-CODERR (4) = ARET101-CODERT1                       07590000
                               OR ARET101-CODERT2                       07600000
                               OR ARET101-CODERT3                       07610000
                               OR ARET101-CODERT4                       07620000
                               OR ARET101-CODERT5                       07630000
                               OR ARET101-CODERT6                       07640000
                               OR ARET101-CODERT7                       07650000
                               OR ARET101-CODERT8                       07660000
               MOVE ARE33I-ERRORIX TO COM-IDCCODER                      07670000
               MOVE      1        TO SW-STAMPA-ERRORI                   07680000
               GO TO END-CODERR.                                        07690000
                                                                        07700000
           IF  ARE33I-CODERR (5) = SPACES                               07710000
               GO TO END-CODERR.                                        07720000
                                                                        07730000
           IF ARE33I-CODERR (5) = ARET101-CODERT1                       07740000
                               OR ARET101-CODERT2                       07750000
                               OR ARET101-CODERT3                       07760000
                               OR ARET101-CODERT4                       07770000
                               OR ARET101-CODERT5                       07780000
                               OR ARET101-CODERT6                       07790000
                               OR ARET101-CODERT7                       07800000
                               OR ARET101-CODERT8                       07810000
               MOVE ARE33I-ERRORIX TO COM-IDCCODER                      07820000
               MOVE      1        TO SW-STAMPA-ERRORI.                  07830000
                                                                        07840000
       END-CODERR.                                                      07850000
           EXIT.                                                        07860000
                                                                        07870000
      *---------------------------------------------------------*       07880000
      *** CARICA DATI-TESTATA   O DATI-SEGNALAZIONE          ****       07890000
      *---------------------------------------------------------*       07900000
                                                                        07910000
       CARICA-TAB-SEGN.                                                 07920000
                                                                        07930000
           PERFORM POSIZIONA-INDICE UNTIL                               07940000
                             AREA-TESTO (IND) NOT EQUAL SPACES          07950000
                                OR    IND < 1.                          07960000
                                                                        07970000
           IF IND < 1                                                   07980000
               MOVE ZERO               TO IND IND1                      07990000
           ELSE                                                         08000000
               MOVE IND                TO IND1                          08010000
               PERFORM SCARICA-SEGN UNTIL IND < 1                       08020000
           END-IF.                                                      08030000
                                                                        08040000
       SCARICA-SEGN.                                                    08050000
                                                                        08060000
      *--------------------------------------------------------*        08070000
      *** IMPOSTA I CAMPI DI SEGNALAZIONE DEL TESTO MESSAGGIO **        08080000
      *** E NEW LINE SU RIGHE VARIABILI                       **        08090000
      *--------------------------------------------------------*        08100000
                                                                        08110000
           MOVE AREA-TESTO (IND)       TO SEGNAL (IND).                 08120000
                                                                        08130000
           MOVE NL                     TO CR10 (IND).                   08140000
                                                                        08150000
           PERFORM POSIZIONA-INDICE.                                    08160000
                                                                        08170000
       POSIZIONA-INDICE.                                                08180000
                                                                        08190000
           SUBTRACT 1 FROM IND.                                         08200000
                                                                        08210000
      *CALCOLA-LUNG.                                                    08220000
                                                                        08230000
      ***  COMPUTE LL-CALC = (LL-RIGA * IND1).                          08240000
      ***  ADD  LL-FIX  TO  LL-CALC.                                    08250000
      ***  MOVE LL-CALC TO  LL-REALE.                                   08260000
                                                                        08270000
      *--------------------------------------------------------*        08280000
      *  ROUTINE SCRITTURA TESTO    SU  VIDEO / STAMPANTE    ***        08290000
      *--------------------------------------------------------*        08300000
       SCRIVI-TESTO.                                                    08310000
                                                                        08320000
           IF NON-STAMPARE                                              08330000
              GO TO END-SCRIVI-TESTO.                                   08340000
                                                                        08350000
      *--------------------------------------------------------*        08360000
      *  ATTIVAZIONE  DEL  ROUTING  SU  VIDEO / STAMPANTE    ***        08370000
      *--------------------------------------------------------*        08380000
                                                                        08390000
           EXEC CICS ROUTE LIST (PARM-ROUTE)                            08400000
                           NLEOM                                        08410000
                           END-EXEC.                                    08420000
                                                                        08430000
      *--------------------------------------------------------*        08440000
      ***   SCRITTURA DELLE RIGHE SU VIDEO / STAMPANTE       ***        08450000
      *--------------------------------------------------------*        08460000
                                                                        08470000
           EXEC CICS SEND TEXT FROM (SEGNALAZIONE)                      08480000
                             LENGTH (LL-REALE)                          08490000
                             PRINT                                      08500000
                             PAGING                                     08510000
                             ACCUM                                      08520000
                             ERASE                                      08530000
           END-EXEC.                                                    08540000
                                                                        08550000
      *--------------------------------------------------------*        08560000
      *    CHIUSURA  DEL  ROUTING  SU  VIDEO / STAMPANTE     ***        08570000
      *--------------------------------------------------------*        08580000
                                                                        08590000
           EXEC CICS                                                    08600000
                SEND PAGE                                               08610000
           END-EXEC.                                                    08620000
                                                                        08630000
       END-SCRIVI-TESTO.                                                08640000
           EXIT.                                                        08650000
                                                                        08660000
      *----------------*                                                08670000
271099 ACQUISISCI-PROG.                                                 08680000
      *----------------*                                                08690000
                                                                        08700000
           EXEC CICS ASKTIME                                            08710000
                ABSTIME(SICD-ABSOLUTE-P)                                08720000
           END-EXEC.                                                    08730000
                                                                        08740000
           MOVE    SICD-ABSOLUTE-P TO SICD-ABSOLUTE-N.                  08750000
                                                                        08760000
       END-ACQUISISCI-PROG.                                             08770000
           EXIT.                                                        08780000
                                                                        08790000
      *----------------------------*                                    08800000
      *    ARCHIVIAZIONE MESSAGGI                                       08810000
      *----------------------------*                                    08820000
       ARCHIVIA.                                                        08830000
                                                                        08840000
           MOVE ARE33I-COD-ARCH TO COM-CODARC.                          08850000
                                                                        08860000
           EVALUATE COM-CODARC                                          08860100
               WHEN 'S'                                                 08860200
RG0316              SET SI-ARCH-SEGN TO TRUE                            08860210
RG0316              PERFORM VERIFICA-LIMITE-SEGN                        08860300
RG0316              IF SI-ARCH-SEGN                                     08860400
                       PERFORM ARCHIVIA-DB2    THRU END-ARCHIVIA-DB2    08860500
RG0316              END-IF                                              08860600
               WHEN 'C'                                                 08870000
               WHEN 'D'                                                 08870100
                    PERFORM ARCHIVIA-DB2    THRU END-ARCHIVIA-DB2       08880000
               WHEN OTHER                                               08880100
                    PERFORM ARCHIVIA-MRI    THRU END-ARCHIVIA-MRI       08900000
           END-EVALUATE.                                                08910000
                                                                        08920000
       END-ARCHIVIA.                                                    08930000
           EXIT.                                                        08940000
                                                                        08950000
      *--------------------------------------------------------*        08960000
      *    ARCHIVIAZIONE MESSAGGI SU TABELLA MRI              **        08970000
      *--------------------------------------------------------*        08990000
       ARCHIVIA-MRI.                                                    09000000
                                                                        09010000
           MOVE ZEROS TO SW-STAMPA-ERRORI.                              09020000
                                                                        09030200
           EXEC CICS START TRANSID ('MTS0')                             09030300
                           FROM    (DFHCOMMAREA)                        09030400
                           LENGTH  (LENGTH OF DFHCOMMAREA)              09030500
           END-EXEC.                                                    09030600
                                                                        09380000
       END-ARCHIVIA-MRI.                                                09390000
           EXIT.                                                        09400000
                                                                        09410000
      *--------------------------------------------------------*        09420000
      *    ARCHIVIAZIONE  SEGNALAZIONI SU TABELLA DB2         **        09430000
      *                   TES_SGNZ_LOG                        **        09440000
      *--------------------------------------------------------*        09450000
       ARCHIVIA-DB2.                                                    09460000
                                                                        09470000
           MOVE ZEROS TO SW-STAMPA-ERRORI.                              09480000
                                                                        09490000
           PERFORM COMPONI-LOG-SEGNALAZIONI.                            09500000
                                                                        09510000
           PERFORM INVIO-LOG.                                           09520000
                                                                        09530000
       END-ARCHIVIA-DB2.                                                09540000
           EXIT.                                                        09550000
                                                                        09560000
      *-------------------------*                                       09570000
       COMPONI-LOG-SEGNALAZIONI.                                        09580000
                                                                        09590000
           MOVE SPACES                TO SSCKKI.                        09600000
           MOVE ARE33I-COD-ARCH       TO W-CODARC.                      09610000
           MOVE W-CODARC2             TO SSCKKI-COD-ARCH.               09620000
           MOVE ARE33I-IDENTIFICATIVO TO SSCKKI-IDENTIFICATIVO.         09630000
           EVALUATE ARE33I-COD-ARCH                                     09640000
               WHEN 'D0001'                                             09650000
                    MOVE 'I'          TO SSCKKI-E1-SISTEMA              09660000
               WHEN 'D0002'                                             09670000
                    MOVE 'E'          TO SSCKKI-E1-SISTEMA              09680000
               WHEN OTHER                                               09690000
                    MOVE SPACES       TO SSCKKI-E1-SISTEMA              09700000
           END-EVALUATE.                                                09710000
           MOVE '00'                  TO SSCKKI-E1-PROC.                09720000
           MOVE ARE33I-DATA-INTARCH   TO SSCKKI-E1-DSTANZA.             09730000
PASS  *    MOVE ARE33I-MESSAGGIO      TO SSCKKI-MSG-VARIABILE.          09740000
PASS       INITIALIZE                 TAB-SEGNALAZIONI.                 09750000
PASS       INITIALIZE                 AREA-SGNZ-24.                     09760000
PASS       MOVE ARE33I-MESSAGGIO      TO TAB-SEGNALAZIONI.              09770000
PASS       PERFORM CARICA-SEGN-COMUNE.                                  09780000
PASS       PERFORM CARICA-SEGN-DETT                                     09790000
PASS       VARYING W-IND FROM 1 BY 1                                    09800000
PASS         UNTIL W-IND > 10.                                          09810000
PASS       MOVE AREA-SGNZ-24          TO SSCKKI-MSG-VARIABILE.          09820000
                                                                        09830000
           MOVE ARE33I-TRANSID        TO SSCKKI-TRANSID.                09840000
           MOVE ARE33I-GRUPPO         TO SSCKKI-GRUPPO.                 09850000
           MOVE ARE33I-CATAPI         TO SSCKKI-CATAPI.                 09860000
           MOVE ARE33I-TIPOMSG        TO SSCKKI-TIPOMSG.                09870000
           MOVE ARE33I-PGRMID         TO SSCKKI-PGRMID.                 09880000
                                                                        09890000
      *-------------------*                                             09900000
       CARICA-SEGN-COMUNE.                                              09910000
                                                                        09920000
           MOVE MSG-RIGA1             TO AREA-SGNZ-1(1).                09930000
           MOVE MSG-RIGA-AST          TO AREA-SGNZ-1(2).                09940000
           EVALUATE W-CODARC-LETT                                       09950000
              WHEN 'C'                                                  09960000
               MOVE '*** SEGNALAZIONI DI SERVIZIO ***'                  09970000
                                                TO AREA-SGNZ-1(3)       09980000
              WHEN 'D'                                                  09990000
               MOVE '*** SEGNALAZIONI CHIUSURA DVP *** '                10000000
                                                TO AREA-SGNZ-1(3)       10010000
              WHEN 'S'                                                  10020000
               MOVE '*** ERRORI DI SISTEMA ***'                         10030000
                                                TO AREA-SGNZ-1(3)       10040000
           END-EVALUATE.                                                10050000
                                                                        10060000
      *-----------------*                                               10070000
       CARICA-SEGN-DETT.                                                10080000
                                                                        10090000
           COMPUTE W-IND2 = W-IND + 10                                  10100000
           MOVE O-SEGNALAZIONE(W-IND) TO AREA-SGNZ-1(W-IND2).           10110000
                                                                        10120000
      *-----------*                                                     10130000
       INVIO-LOG.                                                       10140000
                                                                        10150000
           MOVE EIBTASKN                   TO SSCKKI-NTASK.             10160000
           MOVE ZEROES                     TO SSCKKI-NUM-MSG.           10170000
           EXEC CICS START TRANSID ('TES0')                             10180000
                           FROM    (AREA-SSCKKI)                        10190000
                           LENGTH  (LL-SSCKKI)                          10200000
           END-EXEC.                                                    10210000
      *=====================*                                           10210100
RG0316 VERIFICA-LIMITE-SEGN.                                            10210200
      *=====================*                                           10210210
                                                                        10210400
           MOVE ARE33I-TRANSID         TO CTRANS    OF TES-SGNZ-LOG.    10210500
           MOVE ZERO                   TO W-NUM-SEGN-MM.                10210510
                                                                        10210600
           EXEC SQL                                                     10210700
                SELECT COUNT(*)                                         10210800
                INTO :W-NUM-SEGN-MM                                     10210900
                FROM TES_SGNZ_LOG                                       10211000
                WHERE C_ARCH IN('S0','S1')                              10211100
                  AND S_TMST > CURRENT TIMESTAMP - 1 MINUTE             10211300
                  AND CTRANS = :TES-SGNZ-LOG.CTRANS                     10211400
                  AND P_SGNZ      = 1                                   10211500
           END-EXEC.                                                    10211600
                                                                        10211700
           IF SQLCODE EQUAL ZERO                                        10211800
              IF W-NUM-SEGN-MM >= W-NUM-SEGN-MM-MAX                     10211900
                 SET NO-ARCH-SEGN TO TRUE                               10211910
              ELSE                                                      10211911
                 SET SI-ARCH-SEGN TO TRUE                               10211920
              END-IF                                                    10211930
           ELSE                                                         10212000
              PERFORM W007-ABEND-DB2                                    10212200
           END-IF.                                                      10212300
                                                                        10212400
                                                                        10220000
      *---------------------------------------------------------*       10230000
      *** ROUTINE GENERICA PER ERRORE RICERCA ADERENTE       ***        10240000
      *---------------------------------------------------------*       10250000
                                                                        10260000
       W001-NO-COMMAREA.                                                10270000
                                                                        10280000
           STRING 'IL PGM.CHIAMANTE NON HA PASSATO LA COMMAREA'         10290000
                  DELIMITED BY SIZE      INTO ARE33I-ERRORIX.           10300000
                                                                        10310000
           MOVE '01'                       TO ARMP3025-COD-RIT.         10320000
                                                                        10330000
                                                                        10340000
           EXEC CICS RETURN END-EXEC.                                   10350000
           GOBACK.                                                      10360000
                                                                        10370000
      *---------------------------------------------------------*       10380000
      *** ROUTINE GENERICA PER ERRORE RICERCA UFFICIO        ***        10390000
      *---------------------------------------------------------*       10400000
                                                                        10410000
       W003-RICHIESTA-ERRATA.                                           10420000
                                                                        10430000
      **** STRING 'CODICE NON TROVATO IN TAB ARET101 KEY-RICERCA '      10440000
      ****                                          ARET101-CODARCHI    10450000
           STRING 'RICHIESTA ERRATA ARET101, RETURN-CODE = '            10460000
                                                    ARET101-RC          10470000
                  DELIMITED BY SIZE      INTO ARE33I-ERRORIX.           10480000
                                                                        10490000
           MOVE '02'                       TO ARMP3025-COD-RIT.         10500000
                                                                        10510000
           EXEC CICS RETURN END-EXEC.                                   10520000
           GOBACK.                                                      10530000
                                                                        10540000
      *---------------------------------------------------------*       10550000
      *** ROUTINE GENERICA PER FILE     NON APERTO            ***       10560000
      *---------------------------------------------------------*       10570000
                                                                        10580000
       W004-NOTOPEN.                                                    10590000
                                                                        10600000
           STRING 'FILE TABELLE  (** TAB.  ARET101  **) CHIUSO '        10610000
                      DELIMITED BY SIZE  INTO ARE33I-ERRORIX.           10620000
                                                                        10630000
           MOVE '03'                       TO ARMP3025-COD-RIT.         10640000
                                                                        10650000
           EXEC CICS RETURN END-EXEC.                                   10660000
           GOBACK.                                                      10670000
                                                                        10680000
CR0400*-------------------------------*                                 10690000
CR0400*** ROUTINE  ERRORE    PGMIDERR                                   10700000
CR0400*-------------------------------*                                 10710000
                                                                        10720000
CR0400 W003-ERRORE.                                                     10730000
                                                                        10740000
CR0400     MOVE   SPACES                   TO ARE33I-ERRORIX.           10750000
CR0400     MOVE   'PGM : ARMP3025  ERRORE LINK A PGM. '                 10760000
CR0400                                     TO ARE33I-ERRORIX.           10770000
                                                                        10780000
CR0400     MOVE '04'                       TO ARMP3025-COD-RIT.         10790000
                                                                        10800000
CR0400     EXEC CICS RETURN END-EXEC.                                   10810000
                                                                        10820000
CR0400     GOBACK.                                                      10830000
                                                                        10840000
      *------------------------------------------*                      10850000
      *** ROUTINE GENERICA ERRORE ES :  -INVREQ-                        10860000
      *------------------------------------------*                      10870000
                                                                        10880000
       W005-ERRORE.                                                     10890000
                                                                        10900000
           MOVE   SPACES                   TO ARE33I-ERRORIX.           10910000
           MOVE   'PGM : ARMP3025  ERRORE  GENERICO '                   10920000
                                           TO ARE33I-ERRORIX.           10930000
                                                                        10940000
           MOVE '04'                       TO ARMP3025-COD-RIT.         10950000
                                                                        10960000
           EXEC CICS RETURN END-EXEC.                                   10970000
                                                                        10980000
           GOBACK.                                                      10990000
                                                                        11000000
      *-------------------------------------*                           11120000
      *** ROUTINE GENERICA ABEND   ES. ASRA                             11130000
      *-------------------------------------*                           11140000
                                                                        11150000
       W006-ABEND.                                                      11160000
                                                                        11170000
           MOVE   SPACES                   TO ARE33I-ERRORIX.           11180000
           MOVE   'PGM : ARMP3025  ABEND GENERICO'                      11190000
                                           TO ARE33I-ERRORIX.           11200000
                                                                        11210000
           MOVE '07'                     TO  ARMP3025-COD-RIT.          11220000
                                                                        11230000
           EXEC CICS RETURN END-EXEC.                                   11240000
                                                                        11250000
           GOBACK.                                                      11260000
                                                                        11270000
      *-------------------------------------*                           11270100
      *   ROUTINE GENERICA ERRORE SU DB2                                11270200
      *-------------------------------------*                           11270300
                                                                        11270400
RG0316 W007-ABEND-DB2.                                                  11270500
                                                                        11270600
           MOVE SQLCODE TO W-SQLCODE                                    11270610
           MOVE   SPACES                   TO ARE33I-ERRORIX.           11270700
           STRING 'PGM:ARMP3025 ERRORE DB2. SQLCODE = '                 11270930
                   W-SQLCODE                                            11270940
             DELIMITED BY SIZE INTO ARE33I-ERRORIX.                     11270950
                                                                        11271000
           MOVE '07'                     TO  ARMP3025-COD-RIT.          11271100
                                                                        11271200
           EXEC CICS RETURN END-EXEC.                                   11271300
                                                                        11271400
           GOBACK.                                                      11271500
                                                                        11271600
