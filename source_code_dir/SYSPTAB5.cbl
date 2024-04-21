000010 IDENTIFICATION DIVISION.                                         00079109
000020     SKIP3
000030 PROGRAM-ID.         SYSPTAB5.
000040     SKIP3
000050***************************************************************
000060* PROGRAMMA DI RICERCA TABELLARE CICS SU UN FILE VSAM KSDS.   *
000070* IL PROGRAMMA CHIAMANTE STABILISCE IL TIPO DI RICERCA        *
000080* DA EFFETUARSI : RANDOM         (RND)                        *
000090*                 SEQUENZIALE    (SEQ)                        *
000100*                 POSIZIONAMENTO (PNT)                        *
000110*                 WRITE          (WR )                        *
000120*                 REWRITE        (RWR)                        *
000130*                 DELETE         (DLT)                        *
000140* PASSANDO I SEGUENTI DATI                                    *
000150*      - NOME MEMBRO  ========================== 8  CARATTERI *
000160*      - AREA DI I/O (I PRIMI 20 CARATTERI SONO LA CHIAVE)    *
000170*                     ========================== 80 CARATTERI *
000180*      - TIPO RICERCA ========================== 3  CARATTERI *
000190*      - CODICE-ERRORE========================== 1  CARATTERE *
000200*      - FILLER       ========================== 2  CARATTERI *
000210*                                                             *
000220* CODICE-ERRORE IMPOSTATI DAL PROGRAMMA                       *
000230*     - 0  = TUTTO OK                                         *
000240*     - 1  = ELEMENTO NON TROVATO O EOF                       *
000250*     - 2  = RICHIESTA ERRATA                                 *
000260*     - 3  = POINT MANCANTE                                   *
000270*     - 4  = RECORD DOPPIO SU WRITE                           *
000280*                                                             *
000290*                                                             *
000300***************************************************************
000310     EJECT
000320 ENVIRONMENT DIVISION.
000330 DATA DIVISION.
000340     EJECT
000350 WORKING-STORAGE SECTION.
000360 77  LENG-KEY                    PIC S9(3) COMP VALUE +29.
000370 77  LENG-RECORD                 PIC S9(3) COMP VALUE +89.
000380 77  LENG-COMM                   PIC S9(3) COMP VALUE +95.
000390*--------------------------------------------------------------*
000400*            CODICI DI RITORNO                                 *
000410*--------------------------------------------------------------*
000420* ELEMENTO NON TROVATO O EOF
000430 77  ZERO1                       PIC S9(1)      VALUE +1.
000440* RICHIESTA ERRATA
000450 77  ZERO2                       PIC S9(1)      VALUE +2.
000460* MANCA POINT
000470 77  ZERO3                       PIC S9(1)      VALUE +3.
000480* RECORD DOPPIO SU WRITE
000490 77  ZERO4                       PIC S9(1)      VALUE +4.
000500*--------------------------------------------------------------*
000510     SKIP3
000520*--------------------------------------------------------------*
000530*     AREA DI PASSAGGIO DATI AL PROGRAMMA CHIAMANTE            *
000540*--------------------------------------------------------------*
000550     SKIP3
000560 01  AREA-COMUNE.
000570     05  W-COMMA-REC.
000580         10  W-COMMA-CHIAVE.
000590             20  W-COMMA-NOME        PIC X(8).
000600             20  W-COMMA-CHIAVE20    PIC X(20).
000610         10  W-COMMA-DESCR           PIC X(60).
000620     05  W-TIPO-RIC                  PIC X(3).
000630     05  W-CODICE-ERRORE             PIC 9.
000640     05  W-TIPO-RIC-PNT              PIC X(3).
000650     SKIP3
000660     COPY TABGENW.
000670     EJECT
000680 LINKAGE SECTION.
000690 01  DFHCOMMAREA.
000700     05  L-COMMA-REC.
000710         10  L-COMMA-CHIAVE.
000720             20  L-COMMA-NOME        PIC X(8).
000730             20  L-COMMA-CHIAVE20    PIC X(20).
000740         10  L-COMMA-DESCR           PIC X(60).
000750     05  L-TIPO-RIC-PNT              PIC X(3).
000760     05  L-CODICE-ERRORE             PIC 9.
000770     05  L-TIPO-RIC-PNT              PIC X(3).
000780     EJECT
000790 PROCEDURE DIVISION.
000800     SKIP3
000810*--------------------------------------------------------------*
000820* INIZIO PROGRAMMA.                                            *
000830*                                                              *
000840* SE TIPO RICERCA RANDOM (RND)                                 *
000850*    ALLORA ROUTINE RANDOM                                     *
000860* ALTRIMENTI                                                   *
000870* SE TIPO RICERCA SEQUENZIALE (SEQ)                            *
000880*    ALLORA ROUTINE SEQUENZIALE                                *
000890* SE TIPO RICERCA POSIZIONAMENTO (PNT)                         *
000900*    ALLORA ROUTINE DI POSIZIONAMENTO                          *
000910* ALTRIMENTI                                                   *
000920* SE TIPO RICERCA WRITE (WR )                                  *
000930*    ALLORA ROUTINE WRITE                                      *
000940* ALTRIMENTI                                                   *
000950* SE TIPO RICERCA REWRITE (RWR)                                *
000960*    ALLORA ROUTINE REWRITE                                    *
000970* SE TIPO RICERCA DELETE (DLT)                                 *
000980*    ALLORA ROUTINE DI DELETE                                  *
000990* IMPOSTA RETURN CODE A ZERO2.                                 *
001000* GO BACK AL PROGRAMMA CHIAMANTE.                              *
001010*                                                              *
001020*--------------------------------------------------------------*
001030     SKIP3
001040 INIZIO-PROG.
001050     MOVE DFHCOMMAREA TO AREA-COMUNE.
001060     EXEC CICS HANDLE CONDITION NOTFND (RECORD-NOT-FOUND)
001070                                DUPREC (RECORD-DOPPIO)
001080                                END-EXEC.
001090     MOVE ZERO    TO W-CODICE-ERRORE.
001100     IF W-TIPO-RIC EQUAL 'RND'
001110          PERFORM RICERCA-RANDOM
001120     ELSE
001130     IF W-TIPO-RIC EQUAL 'SEQ'
001140          PERFORM RICERCA-SEQUENZIALE
001150     ELSE
001160     IF W-TIPO-RIC EQUAL 'PNT'
001170          PERFORM POSIZIONAMENTO
001180     ELSE
001190     IF W-TIPO-RIC EQUAL 'WR '
001200          PERFORM SCRIVI-NUOVO-RECORD
001210     ELSE
001220     IF W-TIPO-RIC EQUAL 'RWR'
001230          PERFORM RISCRIVI-RECORD
001240     ELSE
001250     IF W-TIPO-RIC EQUAL 'DLT'
001260          PERFORM CANCELLA-RECORD
001270     ELSE
001280     MOVE ZERO2 TO W-CODICE-ERRORE.
001290     PERFORM RITORNO-MAIN.
001300     STOP RUN.
001310     EJECT
001320*--------------------------------------------------------------*
001330* RITORNO AL PROGRAMMA CHIAMANTE                               *
001340*--------------------------------------------------------------*
001350     SKIP3
001360 RITORNO-MAIN.
001370      MOVE AREA-COMUNE TO DFHCOMMAREA
001380      EXEC CICS RETURN END-EXEC.
001390 RITORNO-MAIN-EX.
001400     EXIT.
001410     EJECT
001420*--------------------------------------------------------------*
001430* TEST PER RICERCA RANDOM                                      *
001440*                                                              *
001450* FORMA CHIAVE DI RICERCA.                                     *
001460* LETTURA DIRECT DEL RECORD.                                   *
001470* SE READ DIRECT ERRATA (HANDLE-CONDITION)                     *
001480*    ALLORA IMPOSTA RETURN-CODE A ZERO1                        *
001490*           RITORNO AL PROGRAMMA CHIAMANTE.                    *
001500* MUOVO IL RECORD INTERESSATO NELLA AREA DI INPUT-OUTPUT.      *
001510* EXIT (RITORNO ALLA ROUTINE INIZIO PROG)                      *
001520*                                                              *
001530*--------------------------------------------------------------*
001540     SKIP3
001550 RICERCA-RANDOM.
001560     MOVE W-COMMA-NOME TO W-REC-TAB-NOME.
001570     MOVE W-COMMA-CHIAVE20 TO W-REC-TAB-CHIAVE20.
001580     MOVE ZERO TO W-REC-TAB-CHIAVEZERO.
001590     PERFORM READ-DIRECT.
001600     MOVE W-REC-TAB-DESCRIZIONE TO W-COMMA-DESCR.
001610 RICERCA-RANDOM-EX.
001620     EXIT.
001630     EJECT
001640*--------------------------------------------------------------*
001650* TEST DI RICERCA SEQUENZIALE                                  *
001660*                                                              *
001670* SE LA PRECEDENETE CALL ERA DI TIPO POINT (PNT)               *
001680*    ALLORA START BROWSE CON LA CHIAVE PASSATA                 *
001690*           READ NEXT                                          *
001700*           SE TABELLA LETTA UGUALE ALLA RICHIESTA             *
001710*              EXIT                                            *
001720*           ALTRIMENTI                                         *
001730*              IMPOSTO CODICE-ERRORE ZERO1                     *
001740*              EXIT                                            *
001750* ALTRIMENTI                                                   *
001760* SE C'E STATA ALMENO UN POSIZIONAMENTO (PNT)                  *
001770*    ALLORA START BROWSE CON LA CHIAVE MAGGIORE                *
001780*           READ NEXT                                          *
001790*           SE TABELLA LETTA UGUALE ALLA RICHIESTA             *
001800*              EXIT                                            *
001810* ALTRIMENTI                                                   *
001820*   IMPOSTO CODICE-ERRORE ZERO3.                               *
001830* EXIT.                                                        *
001840*                                                              *
001850*--------------------------------------------------------------*
001860     SKIP3
001870 RICERCA-SEQUENZIALE.
001880     IF W-TIPO-RIC-PNT EQUAL 'PNT'
001890        MOVE 'SUC' TO W-TIPO-RIC-PNT
001900        MOVE ZERO TO W-REC-TAB-CHIAVEZERO
001910        PERFORM START-BROWSE
001920        PERFORM READ-NEXT
001930        PERFORM END-BROWSE
001940        IF W-REC-TAB-NOME EQUAL W-COMMA-NOME
001950           MOVE W-REC-TAB-CHIAVE20 TO W-COMMA-CHIAVE20
001960           MOVE W-REC-TAB-DESCRIZIONE TO W-COMMA-DESCR
001970        ELSE
001980           MOVE ZERO1 TO W-CODICE-ERRORE
001990     ELSE
002000     IF W-TIPO-RIC-PNT EQUAL 'SUC' THEN
002010        MOVE '1' TO W-REC-TAB-CHIAVEZERO
002020        PERFORM START-BROWSE
002030        PERFORM READ-NEXT
002040        PERFORM END-BROWSE
002050        IF W-REC-TAB-NOME EQUAL W-COMMA-NOME
002060           MOVE W-REC-TAB-CHIAVE20 TO W-COMMA-CHIAVE20
002070           MOVE W-REC-TAB-DESCRIZIONE TO W-COMMA-DESCR
002080        ELSE
002090           MOVE ZERO1 TO W-CODICE-ERRORE
002100     ELSE
002110     MOVE ZERO3 TO W-CODICE-ERRORE.
002120 RICERCA-SEQUENZIALE-EX.
002130     EXIT.
002140     EJECT
002150*--------------------------------------------------------------*
002160* POSIZIONAMENTO                                               *
002170*                                                              *
002180* MEMORIZZA CHE C'E STATO UNA POINT (PNT)                      *
002190* EXIT.                                                        *
002200*                                                              *
002210*--------------------------------------------------------------*
002220     SKIP3
002230 POSIZIONAMENTO.
002240     MOVE 'PNT' TO W-TIPO-RIC-PNT.
002250 POSIZIONAMENTO-EX.
002260     EXIT.
002270     EJECT
002280*--------------------------------------------------------------*
002290* SCRIVI NUOVO RECORD                                          *
002300*                                                              *
002310* FORMA CHIAVE DI SCRITTURA.                                   *
002320* SCRITTURA DEL RECORD.                                        *
002330* SE WRITE ERRATA (HANDLE-CONDITION)                           *
002340*    ALLORA IMPOSTA RETURN-CODE A ZERO4                        *
002350*           RITORNO AL PROGRAMMA CHIAMANTE.                    *
002360* EXIT (RITORNO ALLA ROUTINE INIZIO PROG)                      *
002370*                                                              *
002380*--------------------------------------------------------------*
002390     SKIP3
002400 SCRIVI-NUOVO-RECORD.
002410     MOVE W-COMMA-NOME TO W-REC-TAB-NOME.
002420     MOVE W-COMMA-CHIAVE20 TO W-REC-TAB-CHIAVE20.
002430     MOVE ZERO TO W-REC-TAB-CHIAVEZERO.
002440     MOVE W-COMMA-DESCR TO W-REC-TAB-DESCRIZIONE.
002450     PERFORM WRITE-RECORD.
002460 SCRIVI-NUOVO-RECORD-EX.
002470     EXIT.
002480     EJECT
002490*--------------------------------------------------------------*
002500* RISCRIVI RECORD                                              *
002510*                                                              *
002520* FORMA CHIAVE DI LETTURA.                                     *
002530* LETTURA DIRECT DEL RECORD.                                   *
002540* SE READ DIRECT ERRATA (HANDLE-CONDITION)                     *
002550*    ALLORA IMPOSTA RETURN-CODE A ZERO1                        *
002560*           RITORNO AL PROGRAMMA CHIAMANTE.                    *
002570* MUOVO IL RECORD INTERESSATO NELLA AREA DI INPUT-OUTPUT.      *
002580* REWRITE DEL NUOVO RECORD.                                    *
002590* EXIT (RITORNO ALLA ROUTINE INIZIO PROG).                     *
002600*                                                              *
002610*--------------------------------------------------------------*
002620     SKIP3
002630 RISCRIVI-RECORD.
002640     MOVE W-COMMA-NOME TO W-REC-TAB-NOME.
002650     MOVE W-COMMA-CHIAVE20 TO W-REC-TAB-CHIAVE20.
002660     MOVE ZERO TO W-REC-TAB-CHIAVEZERO.
002670     PERFORM READ-DIRECT-UPDATE.
002680     MOVE W-COMMA-DESCR TO W-REC-TAB-DESCRIZIONE.
002690     PERFORM REWRITE-RECORD.
002700 RESCRIVI-RECORD-EX.
002710     EXIT.
002720     EJECT
002730*--------------------------------------------------------------*
002740* CANCELLA RECORD                                              *
002750*                                                              *
002760* FORMA CHIAVE DI RICERCA.                                     *
002770* LETTURA DIRECT DEL RECORD.                                   *
002780* SE READ DIRECT ERRATA (HANDLE-CONDITION)                     *
002790*    ALLORA IMPOSTA RETURN-CODE A ZERO1                        *
002800*           RITORNO AL PROGRAMMA CHIAMANTE.                    *
002810* DELETE DEL RECORD.                                           *
002820* EXIT (RITORNO ALLA ROUTINE INIZIO PROG).                     *
002830*                                                              *
002840*--------------------------------------------------------------*
002850     SKIP3
002860 CANCELLA-RECORD.
002870     MOVE W-COMMA-NOME TO W-REC-TAB-NOME.
002880     MOVE W-COMMA-CHIAVE20 TO W-REC-TAB-CHIAVE20.
002890     MOVE ZERO TO W-REC-TAB-CHIAVEZERO.
002900     PERFORM READ-DIRECT-UPDATE.
002910     PERFORM DELETE-RECORD.
002920 CANCELLA-RECORD-EX.
002930     EXIT.
002940     EJECT
002950*--------------------------------------------------------------*
002960*                                                              *
002970*                                                              *
002980* ROUTINE DI INPUT-OUTPUT SUL FILE TABELLE (KSDS)              *
002990*                                                              *
003000*                                                              *
003010*--------------------------------------------------------------*
003020     SKIP3
003030 START-BROWSE.
003040     MOVE W-COMMA-NOME TO W-REC-TAB-NOME.
003050     MOVE W-COMMA-CHIAVE20 TO W-REC-TAB-CHIAVE20.
003060     EXEC CICS STARTBR
003070                       DATASET   ('TABELLE')
003080                       RIDFLD    (W-REC-TAB-CHIAVE)
003090                       KEYLENGTH (LENG-KEY)
003100                       END-EXEC.
003110 START-BROWSE-EX.
003120     EXIT.
003130     SKIP1
003140 WRITE-RECORD.
003150     EXEC CICS WRITE FROM      (W-REC-TABELLE)
003160                     LENGTH    (LENG-RECORD)
003170                     DATASET   ('TABELLE')
003180                     RIDFLD    (W-REC-TAB-CHIAVE)
003190                     KEYLENGTH (LENG-KEY)
003200                     END-EXEC.
003210 WRITE-RECORD-EX.
003220     EXIT.
003230     SKIP1
003240 REWRITE-RECORD.
003250     EXEC CICS REWRITE FROM     (W-REC-TABELLE)
003260                       LENGTH   (LENG-RECORD)
003270                       DATASET  ('TABELLE')
003280                       END-EXEC.
003290 REWRITE-RECORD-EX.
003300     EXIT.
003310     SKIP1
003320 DELETE-RECORD.
003330     EXEC CICS DELETE DATASET ('TABELLE')
003340                      END-EXEC.
003350 DELETE-RECORD-EX.
003360     EXIT.
003370     SKIP1
003380 READ-NEXT.
003390     EXEC CICS READNEXT INTO      (W-REC-TABELLE)
003400                        LENGTH    (LENG-RECORD)
003410                        DATASET   ('TABELLE')
003420                        RIDFLD    (W-REC-TAB-CHIAVE)
003430                        KEYLENGTH (LENG-KEY)
003440                        END-EXEC.
003450 READ-NEXT-EX.
003460     EXIT.
003470     SKIP1
003480 READ-DIRECT.
003490     EXEC CICS READ INTO      (W-REC-TABELLE)
003500                    LENGTH    (LENG-RECORD)
003510                    DATASET   ('TABELLE')
003520                    RIDFLD    (W-REC-TAB-CHIAVE)
003530                    KEYLENGTH (LENG-KEY)
003540                    END-EXEC.
003550 READ-DIRECT-EX.
003560     EXIT.
003570     SKIP1
003580 READ-DIRECT-UPDATE.
003590     EXEC CICS READ INTO      (W-REC-TABELLE)
003600                    LENGTH    (LENG-RECORD)
003610                    DATASET   ('TABELLE')
003620                    RIDFLD    (W-REC-TAB-CHIAVE)
003630                    KEYLENGTH (LENG-KEY)
003640                    UPDATE
003650                    END-EXEC.
003660 READ-DIRECT-UPDATE-EX.
003670     EXIT.
003680     SKIP1
003690 END-BROWSE.
003700     EXEC CICS ENDBR DATASET('TABELLE')
003710                     END-EXEC.
003720 END-BROWSE-EX.
003730     EXIT.
003740     EJECT
003750*--------------------------------------------------------------*
003760*                                                              *
003770*                                                              *
003780* PUNTI DI USCITA                                              *
003790*                                                              *
003800*                                                              *
003810*--------------------------------------------------------------*
003820   SKIP3
003830 RECORD-NOT-FOUND.
003840     MOVE ZERO1 TO W-CODICE-ERRORE.
003850     PERFORM RITORNO-MAIN.
003860     STOP RUN.
003870 RECORD-NOT-FOUND-EX.
003880     EXIT.
003890     SKIP3
003900 RECORD-DOPPIO.
003910     MOVE ZERO4 TO W-CODICE-ERRORE.
003920     PERFORM RITORNO-MAIN.
003930     STOP RUN.
003940 RECORD-DOPPIO-EX.
003950     EXIT.
