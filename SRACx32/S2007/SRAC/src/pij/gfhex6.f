C***********************************************************************
                      SUBROUTINE GFHEX6
     *    (NRX,JRPH,NTYZ,NTPIN,IXPIN,IYPIN)
C***********************************************************************
C          FIGURE A HEXANT OF THE HEXAGONAL CELL
C***********************************************************************
      DIMENSION IXPIN(*),IYPIN(*)
      CHARACTER *1 LETTER(132,60),DIGID(36),AST,BLK
      COMMON /MAINC/ DUMY1(63),NOUT1,NOUT2,DUMMY2(435)
C ***************** G-REGION MAP OF A HEXANT ***************************
      DATA DIGID/'0','1','2','3','4','5','6','7','8','9',
     *           'A','B','C','D','E','F','G','H','I','J',
     *           'K','L','M','N','O','P','Q','R','S','T',
     *           'U','V','W','X','Y','Z'/,
     *            AST/'*'/,BLK/' '/
C***********************************************************************
                       WRITE(NOUT2,50)
   50 FORMAT('1 *** TRIANGULAR  MAP FOR THE HEXAGONAL CELL************'/
     *      ,'      GRID NUMBER AND G-REGION NUMBER         ')
                         WRITE(NOUT2,40)
   40                    FORMAT(' ')
                         LMAX=7+6*NRX
                         MMAX=7+6*JRPH
                           JL=0
                           NBL=4
                        DO 200 J=1,MAX0(JRPH,NTYZ)+1
                        IMAX=JRPH+2-J
                           JL=JL+1
C   ***  BASE LINE ***
                          NBL=NBL+1
                        LENG=6*IMAX-4
                            L=1
                        DO 100 I=1,LMAX
                            L=L+1
                        LETTER(L,JL)=BLK
  100                     CONTINUE
                            L=NBL
                        DO 110 I=1,LENG
                            L=L+1
                        LETTER(L,JL)=AST
  110                     CONTINUE
           IF(J.LE.NTYZ+1 .AND. LMAX.GT.MMAX ) THEN
                      DO 120 I=MMAX,LMAX
                       LETTER(I,JL)=AST
  120                     CONTINUE
                          L=MMAX+4
                     DO 125 I=JRPH+2,NRX+2
                          L=L+1
                     LETTER(L,JL)=DIGID(I)
                          L=L+1
                     LETTER(L,JL)=DIGID(J)
                          L=L+4
  125                     CONTINUE
                          ENDIF
                             L=NBL
                         DO 130 I=1,IMAX
                             L=L+1
                     LETTER(L,JL)=DIGID(I+J-1)
                             L=L+1
                     LETTER(L,JL)=DIGID(J)
                             L=L+4
  130                       CONTINUE
C   ***  MIDDLE LINE ***
                       IF(J.EQ.MAX0(JRPH,NTYZ)+1) GO TO 200
                           JL=JL+1
                           NBL=NBL+1
                           LENG=LENG-2
                        DO 140 I=1,LMAX
                        LETTER(I,JL)=BLK
  140                     CONTINUE
                            L=NBL+1
                        DO 150 I=1,LENG/6
                        LETTER(L,JL)=AST
                            L=L+6
  150                     CONTINUE
           IF(J.LE.NTYZ .AND. LMAX.GT.MMAX ) THEN
                        L=MMAX
                        DO 155 I=JRPH+1,NRX+2
                        LETTER(L,JL)=AST
                        L=L+6
  155                    CONTINUE
                                             ENDIF
                        L=NBL+LENG
                        LETTER(L,JL)=AST
                            L=NBL+1
                        DO 160 I=1,IMAX-1
                        IR=(I+J-2)**2+2*J-1
                        I1=IR/100
                        IR=IR-100*I1
                        I2=IR/10
                        IR=IR-10*I2
                             L=L+1
                   LETTER(L,JL)=DIGID(I1+1)
                             L=L+1
                   LETTER(L,JL)=DIGID(I2+1)
                             L=L+1
                   LETTER(L,JL)=DIGID(IR+1)
                             L=L+3
  160                       CONTINUE
           IF(J.LE.NTYZ .AND. LMAX.GT.MMAX ) THEN
                        L=MMAX+1
                        DO 165 I=JRPH+1,NRX
                        IR=JRPH**2+NTYZ*(I-JRPH-1)+J
                        I1=IR/100
                        IR=IR-100*I1
                        I2=IR/10
                        IR=IR-10*I2
                             L=L+1
                   LETTER(L,JL)=DIGID(I1+1)
                             L=L+1
                   LETTER(L,JL)=DIGID(I2+1)
                             L=L+1
                   LETTER(L,JL)=DIGID(IR+1)
                             L=L+3
  165                       CONTINUE
                                             ENDIF
C   ***  LAST LINE ***
                             JL=JL+1
                            NBL=NBL+1
                            LENG=LENG-2
                          DO 170 I=1,LMAX
                          LETTER(I,JL)=BLK
  170                     CONTINUE
                            L=NBL+3
                          DO 180 I=1,LENG/6
                          LETTER(L,JL)=AST
                            L=L+6
  180                     CONTINUE
                        LETTER(NBL+   1,JL)=AST
                        LETTER(NBL+LENG,JL)=AST
           IF(J.LE.NTYZ .AND. LMAX.GT.MMAX ) THEN
                        L=MMAX
                        DO 185 I=JRPH+1,NRX+1
                        LETTER(L,JL)=AST
                        L=L+6
  185                    CONTINUE
                                             ENDIF
                        IF(J.EQ.NRX) GO TO 200
                             L=NBL+3
                        DO 190 I=1,IMAX-2
                        IR=(I+J-1)**2+2*J
                        I1=IR/100
                        IR=IR-100*I1
                        I2=IR/10
                        IR=IR-10*I2
                             L=L+1
                    LETTER(L,JL)=DIGID(I1+1)
                             L=L+1
                    LETTER(L,JL)=DIGID(I2+1)
                             L=L+1
                    LETTER(L,JL)=DIGID(IR+1)
                             L=L+3
  190                       CONTINUE
  200                       CONTINUE
C                      *** PRINT MAP ***
                          JLMAX=JL
                          DO 300 J=1,JLMAX
                          JL=JLMAX-J+1
               WRITE(NOUT2,210)(LETTER(L,JL),L=1,LMAX)
  210                     FORMAT(132A1)
  300                      CONTINUE
                          WRITE(NOUT2,40)
C ****************** PRINT PIN NUMBER ON MAP ***************************
                     IF(NTPIN.EQ.0) RETURN
                          WRITE(NOUT2,60)
                          WRITE(NOUT2,40)
   60 FORMAT('0*** PIN ROD NUMBER ON TRIANGULAR MAP FOR ************')
C
                           NBL=4
                        DO 350 J=1,MAX0(JRPH,NTYZ)+1
                        IMAX=JRPH+2-J
                           JL=3*J-2
C   ***  CLEAR BASE LINE ***
                          NBL=3*J+2
                        LENG=6*IMAX-4
                            L=1
                        DO 310 I=1,LMAX
                            L=L+1
                        LETTER(L,JL)=BLK
  310                     CONTINUE
                            L=NBL
                        DO 320 I=1,LENG
                            L=L+1
                        LETTER(L,JL)=AST
  320                     CONTINUE
           IF(J.LE.NTYZ+1 .AND. LMAX.GT.MMAX ) THEN
                      DO 340 I=MMAX,LMAX
                       LETTER(I,JL)=AST
  340                     CONTINUE
                                               ENDIF
  350                     CONTINUE
                        DO 400 NP=1,NTPIN
                        JL=3*IYPIN(NP)+1
                   L=6*(IXPIN(NP)-IYPIN(NP))+JL+5
                      IF(IXPIN(NP).GT.JRPH)
     *             L=MMAX+6*(IXPIN(NP)-JRPH)-1
                        I1=NP/10
                        I2=NP-10*I1
                    LETTER(L  ,JL)=DIGID(I1+1)
                    LETTER(L+1,JL)=DIGID(I2+1)
  400                       CONTINUE
C *** PRINT MAP
                          DO 500 J=1,JLMAX
                         JL=JLMAX-J+1
               WRITE(NOUT2,210)(LETTER(L,JL),L=1,LMAX)
  500                       CONTINUE
                          WRITE(NOUT2,40)
C ****************** PRINT IDPIN ON MAP ********************************
                       WRITE(NOUT2,70)
                       WRITE(NOUT2,40)
   70 FORMAT('0*** PIN ROD DIVISION ON TRIANGULAR MAP FOR ******')
                        DO 600 NP=1,NTPIN
                        JL=3*IYPIN(NP)+1
                   L=6*(IXPIN(NP)-IYPIN(NP))+JL+5
                      IF(IXPIN(NP).GT.JRPH)
     *             L=MMAX+6*(IXPIN(NP)-JRPH)-1
                        I1 = 0
                    LETTER(L  ,JL)=DIGID(1)
                    LETTER(L+1,JL)=DIGID(I1+1)
  600                      CONTINUE
C *** PRINT MAP
                          DO 700 J=1,JLMAX
                          JL=JLMAX-J+1
               WRITE(NOUT2,210)(LETTER(L,JL),L=1,LMAX)
  700                     CONTINUE
                          WRITE(NOUT2,40)
                           RETURN
                            END
