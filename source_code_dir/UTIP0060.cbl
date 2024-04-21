000010 IDENTIFICATION DIVISION.                                         00079087
000020 PROGRAM-ID.                                                      00079087
000030             UTIP0060.                                            00079087
000040 DATA DIVISION.                                                   00079087
000050 EJECT                                                            00079087
000060 WORKING-STORAGE SECTION.                                         00079087
000070 SKIP2                                                            00079087
000080 01  IND                 PIC 9999 COMP.                           00079087
000090 01  DATA5               PIC 9(5).                                00079087
000100 01  DATA5-R     REDEFINES  DATA5.                                00079087
BP2000     05 DATA5-AA         PIC 99.                                  00079087
000120     05 DATA5-DDD        PIC 999.                                 00079087
000130 01  COMODOA             PIC S9(7)  COMP-3.                       00079087
000140 01  COMODOB             PIC S9(7)  COMP-3.                       00079087
000150 01  GIORNI-PER-MESE.                                             00079087
000160     05 GENNAIO          PIC 99  VALUE 31.                        00079087
000170     05 FEBBRAIO         PIC 99  VALUE 28.                        00079087
000180     05 MARZO            PIC 99  VALUE 31.                        00079087
000190     05 APRILE           PIC 99  VALUE 30.                        00079087
000200     05 MAGGIO           PIC 99  VALUE 31.                        00079087
000210     05 GIUGNO           PIC 99  VALUE 30.                        00079087
000220     05 LUGLIO           PIC 99  VALUE 31.                        00079087
000230     05 AGOSTO           PIC 99  VALUE 31.                        00079087
000240     05 SETTEMBRE        PIC 99  VALUE 30.                        00079087
000250     05 OTTOBRE          PIC 99  VALUE 31.                        00079087
000260     05 NOVEMBRE         PIC 99  VALUE 30.                        00079087
000270     05 DICEMBRE         PIC 99  VALUE 31.                        00079087
000280*                                                                 00079087
000290 01  GIORNI-PER-MESE-R   REDEFINES GIORNI-PER-MESE.               00079087
000300     05 MESEX            PIC 99  OCCURS 12.                       00079087
000310*                                                                 00079087
000320 LINKAGE SECTION.                                                 00079087
000330*                                                                 00079087
BP2000 01  DATA-YYDDD          PIC S9(5)   COMP-3.                      00079087
000350 01  DATA-GGMMAA.                                                 00079087
000360     05  DATA-GG         PIC 99.                                  00079087
000370     05  DATA-MM         PIC 99.                                  00079087
BP2000     05  DATA-AA         PIC 99.                                  00079087
000390 EJECT                                                            00079087
000400 PROCEDURE DIVISION                                               00079087
BP2000         USING   DATA-YYDDD                                       00079087
BP2000                 DATA-GGMMAA.                                     00079087
000430 SKIP2                                                            00079087
000440*                                                                 00079087
000450*            CALCOLO ANNO BISESTILE                               00079087
000460*                                                                 00079087
BP2000     MOVE DATA-YYDDD TO DATA5                                     00079087
000480     MOVE 28 TO FEBBRAIO                                          00079087
BP2000*    IF DATA5-AA EQUAL ZERO                                       00079087
000500*        GO TO CALCOLA-GIORNO.                                    00079087
BP2000     COMPUTE COMODOA = DATA5-AA / 4                               00079087
000520     COMPUTE COMODOB = COMODOA * 4                                00079087
BP2000     IF COMODOB NOT EQUAL DATA5-AA                                00079087
000540         GO TO CALCOLA-GIORNO.                                    00079087
000550     MOVE 29 TO FEBBRAIO.                                         00079087
000560 SKIP2                                                            00079087
000570 CALCOLA-GIORNO.                                                  00079087
000580 SKIP1                                                            00079087
000590     MOVE 1 TO IND.                                               00079087
000600     MOVE DATA5-DDD TO COMODOA.                                   00079087
000610 GIRO-GIORNO-A.                                                   00079087
000620     SUBTRACT MESEX (IND) FROM COMODOA                            00079087
000630     IF COMODOA GREATER ZERO                                      00079087
000640         NEXT SENTENCE                                            00079087
000650     ELSE                                                         00079087
000660         GO TO GIRO-GIORNO-X.                                     00079087
000670     IF IND NOT EQUAL 12                                          00079087
000680         ADD 1 TO IND                                             00079087
000690         GO TO GIRO-GIORNO-A.                                     00079087
000700     DISPLAY '*** ROUTINE UTIP      ****'                         00079087
000710     DISPLAY 'ERRORE PARAMETRO O CALCOLO'                         00079087
000720     DISPLAY DATA5 ' DATA PASSATA COME PARAMETRO'                 00079087
000730     DISPLAY 'ABNORMAL END 0C7 CAUSATO DI PROPOSITO'              00079087
BP2000     MOVE SPACE TO DATA-GGMMAA                                    00079087
000750     ADD 1 TO DATA-GG.                                            00079087
000760 SKIP1                                                            00079087
000770 GIRO-GIORNO-X.                                                   00079087
000780 SKIP1                                                            00079087
000790     ADD MESEX (IND) TO COMODOA                                   00079087
000800     MOVE COMODOA TO DATA-GG                                      00079087
000810     MOVE IND TO DATA-MM                                          00079087
BP2000     MOVE DATA5-AA TO DATA-AA.                                    00079087
000830 SKIP2                                                            00079087
000840 RITORNO.                                                         00079087
000850     GOBACK.                                                      00079087
