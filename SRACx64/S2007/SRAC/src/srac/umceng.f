      SUBROUTINE       UMCENG(IC8)
C
      REAL*8          EBOUND,UIGP,UFGP,UBGP
      REAL*8          DELUU,DSAVE,ENGPSQ
      CHARACTER*4     IDTMP,NFILE
      CHARACTER*8     MEMBER
C
      COMMON /UMCPIN/ ITAPE,MATNO,NPE,NPF,NPC,IZA,ISWUN,AM,ELOW,EHI,
     +                RFTEMP,QVALE,QVALF,QVALC,EULOW,EUHIGH
      COMMON /UMCINT/ NPMAX,IFISS,IENDUN,IDPOS,ISWTMP,MF,MT,NP,IMAX,
     +                NSYSI,NSYSO,RQTEMP,NPRT,IPRINT
C
      COMMON /UMC001/ LIBTYP,NEF,ISTART,NG,NOMESH,KFGP,NGMAX,MAXINT,NSET
      COMMON /UMC002/ ENERGY(75),EE(47),NI(46),INTNO(46),
     +                NXG(10),NFI(10),NOIG(10),MST(46),MEND(46)
      COMMON /UMC003/ EBOUND(11),UIGP(10),UFGP(10),UBGP(46)
      COMMON /UMC004/ INTBL(2000),ENGD(2000)
      COMMON /UMCTMP/ NTEMP,TMPSET(40),IDTMP(40)
      COMMON /PCOWK3/ WORK(500),EWORK(500),IWORK(46)
C
      COMMON /PDSPDS/ BUFFER(540),IFLSW,NFILE(3),ECODE,TEMPPP
C
C     INITIAL SET
C
      LIBTYP  = 0
      NGMAX   = 0
      ISTART  = 0
      NG      = 0
      NOMESH  = 0
      MAXINT  = 0
      NSET    = 0
      LOOP    = 75 + 47 + 4*46 + 30
C
      DO 10   I = 1 , LOOP
      ENERGY(I) = 0.0
   10 CONTINUE
      DO 20   I = 1 , 10
      EBOUND(I) = 0.0
      UIGP  (I) = 0.0
      UFGP  (I) = 0.0
   20 CONTINUE
      EBOUND(11)= 0.0
      DO 30   I = 1 ,46
      UBGP  (I) = 0.0
   30 CONTINUE
C
      NTEMP     = 0
      DO 40   I = 1 , 40
      TMPSET(I) = -1000.0
   40 CONTINUE
C
      CALL CLEA(  INTBL , 2000 , 0.0 )
      CALL CLEA(  ENGD  , 2000 , 0.0 )
      CALL CLEA(  EWORK ,  500 , 0.0 )
C
      IDTMP(01) = '1   '
      IDTMP(02) = '2   '
      IDTMP(03) = '3   '
      IDTMP(04) = '4   '
      IDTMP(05) = '5   '
      IDTMP(06) = '6   '
      IDTMP(07) = '7   '
      IDTMP(08) = '8   '
      IDTMP(09) = '9   '
      IDTMP(10) = 'A   '
      IDTMP(11) = 'B   '
      IDTMP(12) = 'C   '
      IDTMP(13) = 'D   '
      IDTMP(14) = 'E   '
      IDTMP(15) = 'F   '
      IDTMP(16) = 'G   '
      IDTMP(17) = 'H   '
      IDTMP(18) = 'I   '
      IDTMP(19) = 'J   '
      IDTMP(20) = 'K   '
      IDTMP(21) = 'L   '
      IDTMP(22) = 'M   '
      IDTMP(23) = 'N   '
      IDTMP(24) = 'O   '
      IDTMP(25) = 'P   '
      IDTMP(26) = 'Q   '
      IDTMP(27) = 'R   '
      IDTMP(28) = 'S   '
      IDTMP(29) = 'T   '
      IDTMP(30) = 'U   '
      IDTMP(31) = 'V   '
      IDTMP(32) = 'W   '
      IDTMP(33) = 'X   '
      IDTMP(34) = 'Y   '
      IDTMP(35) = 'Z   '
      IDTMP(36) = 'A   '
      IDTMP(37) = 'B   '
      IDTMP(38) = 'C   '
      IDTMP(39) = 'D   '
      IDTMP(40) = 'E   '
C
C     READ FAST ENERGY STRUCTURE FORM 'FASTU' LIBRARY
C
      IFLSW     = 1
      NFILE(1)  = 'FAST'
      NFILE(2)  = 'U   '
      MEMBER    = 'FASTLIB '
      IRC       = 0
C
      CALL SEARCH( MEMBER , LENG , IRC )
      IF(IRC.EQ.1) GO TO 911
      CALL  READ( MEMBER , NEF   , 1 )
C
      LENG = NEF*2 + 5
      CALL  READ( MEMBER , WORK  , LENG )
C
      DO 50 I = 1 , NEF + 1
      ENERGY(I) = WORK(4+NEF+I)
   50 CONTINUE
C
      IFLSW     = 1
      NFILE(1)  = 'UMCR'
      NFILE(2)  = 'OSS '
C
C     CHECK PRESENCE OF 130.07 EV ENERGY BOUNDARY
C
      ISW130  = 0
      ECHECK  = 130.07
      DO 60 I = 1 , NEF
      EEE     = ENERGY(I)
      DELE    = EEE - ECHECK
      IF(ABS(DELE).LT.0.01) THEN
                            ISW130 = I
                            ELSE
         IF(EEE.GT.ECHECK.AND.ENERGY(I+1).LT.ECHECK) THEN
                                                     ISW130 = -I
                                                     ENDIF
                            ENDIF
   60 CONTINUE
C
C     CHECK PRESENCE OF 961.12 EV ENERGY BOUNDARY
C
      ISW961  = 0
      ECHECK  = 1.000E+7*EXP(-9.25)
      DO 65 I = 1 , NEF
      EEE     = ENERGY(I)
      DELE    =(EEE - ECHECK)/ECHECK
      IF(ABS(DELE).LT.0.001) THEN
                             ISW961 = I
                             ELSE
         IF(EEE.GT.ECHECK.AND.ENERGY(I+1).LT.ECHECK) THEN
                                                     ISW961 = I + 1
                                                     ENDIF
                             ENDIF
   65 CONTINUE
C
      IF(IC8.LT.0) IC8    = 0
      IF(IC8.GT.3) IC8    = 3
      IF(IC8.GT.1.AND.ISW130.LT.0)  IC8 = 1
      LIBTYP  = IC8
C     RESET ENERGY ARRAY
      DO 70 I = 1 , NEF
      SAVE    = LOG(ENERGY(1)/ENERGY(I+1))
      ISAVE   = (SAVE + 0.01) / 0.125
      WORK(I) = 0.125*FLOAT(ISAVE)
   70 CONTINUE
C
      EWORK(1)   = 1.0000E+7
      DO 80   I  = 1 , NEF
      EWORK(I+1) = EWORK(1)*EXP(-WORK(I))
   80 CONTINUE
C
      IF(IC8.EQ.0) THEN
                   ISTART = IABS(ISW130)
                   IF(ISW130.LT.0)  ISTART = ISTART + 1
                   NG     = NEF - ISTART + 1
                   NOMESH = 1
                   EBOUND(1) = DBLE(EWORK(ISTART))
                   EBOUND(2) = DBLE(EWORK(NEF+1))
                   NFI   (1) = 10
                   UIGP  (1) = 1.250000D-2
                   UFGP  (1) = 1.250000D-3
                   GO TO 101
                   ENDIF
C
      IF(IC8.EQ.1) THEN
                   ISTART = ISW961
                   NG     = NEF - ISTART + 1
                   NOMESH = 1
                   EBOUND(1) = DBLE(EWORK(ISTART))
                   EBOUND(2) = DBLE(EWORK(NEF+1))
                   NFI   (1) = 10
                   UIGP  (1) = 5.000000D-3
                   UFGP  (1) = 5.000000D-4
                   GO TO 101
                   ENDIF
C
      IF(IC8.EQ.2) THEN
                   ISTART = ISW961
                   NG     = NEF - ISTART + 1
                   NOMESH = 2
                   EBOUND(1) = DBLE(EWORK(ISTART))
                   EBOUND(2) = DBLE(EWORK(ISW130))
                   EBOUND(3) = DBLE(EWORK(NEF+1))
                   NFI   (1) = 10
                   UIGP  (1) = 6.250000D-3
                   UFGP  (1) = 6.250000D-4
                   NFI   (2) = 10
                   UIGP  (2) = 1.250000D-2
                   UFGP  (2) = 1.250000D-3
                   GO TO 101
                   ENDIF
C
      IF(IC8.EQ.3) THEN
                   ISTART = ISW961
                   NG     = NEF - ISTART + 1
                   NOMESH = 2
                   EBOUND(1) = DBLE(EWORK(ISTART))
                   EBOUND(2) = DBLE(EWORK(ISW130))
                   EBOUND(3) = DBLE(EWORK(NEF+1))
                   NFI   (1) = 10
                   UIGP  (1) = 2.500000D-3
                   UFGP  (1) = 2.500000D-4
                   NFI   (2) = 10
                   UIGP  (2) = 5.000000D-3
                   UFGP  (2) = 5.000000D-4
                   ENDIF
C
C
C
  101 CONTINUE
      EE(  1)  = EWORK(ISTART)
      DO 110 I = 1 , NG
      EE(I+1)  = EWORK(ISTART+I)
      SAVE     = LOG(EE(I)/EE(I+1))
      ISAVE   = (SAVE + 0.01) / 0.125
      UBGP(I)  = DFLOAT(ISAVE)*1.2500000D-1
  110 CONTINUE
C
      NIO      = 0
      MAXINT   = 0
C     SET INTNO & NI & NGMAX & MAXINT
      IF(LIBTYP.LE.1) THEN
                      DELUU    = UIGP(1)
                      DO 120 I = 1 , NG
                      ISAVE    = UBGP(I)/DELUU + 0.5
                      INTNO(I) = ISAVE
                      IF(ISAVE.GT.MAXINT) MAXINT = ISAVE
                      NIO      = NIO + ISAVE
                      NI(I)    = NIO
  120                 CONTINUE
                      NGMAX    = NIO
C
                      ELSE
                      DELUU    = UIGP(1)
                      ISW      = ISTART - 1
                      DO 140 I = 1 , NG
                      ISW      = ISW + 1
                      IF(ISW.EQ.ISW130) THEN
                                        DELUU = UIGP(2)
                                        ENDIF
                      ISAVE    = UBGP(I)/DELUU + 0.5
                      INTNO(I) = ISAVE
                      IF(ISAVE.GT.MAXINT) MAXINT = ISAVE
                      NIO      = NIO + ISAVE
                      NI(I)    = NIO
  140                 CONTINUE
                      NGMAX    = NIO
                      ENDIF
C
      IF(NGMAX.GT.2000) GO TO 901
C
      KFGP     = NFI(1)
      DO 150 I = 1 , NOMESH
      DSAVE    = LOG(EBOUND(I)/EBOUND(I+1))
      NOIG(I)  = DSAVE/UIGP(I) + 0.2
  150 CONTINUE
C
      IF(NGMAX.LE.500) THEN
                       MAXINT = NGMAX
                       ELSE
                       ISAVE = 500 / MAXINT
                       MAXINT = MAXINT * ISAVE
                       ENDIF
C
      ISUM     = 0
      JSUM     = INTNO(1)
      IWORK(1) = 1
      ISW      = 1
      DO 160 I = 1 , NG-1
      ISUM     = ISUM + INTNO(I)
      JSUM     = JSUM + INTNO(I+1)
      IF(JSUM.GT.MAXINT.AND.ISUM.LE.MAXINT) THEN
                 JSUM = INTNO(I+1)
                 ISUM = 0
                 ISW  = ISW + 1
                 IWORK(ISW) = I + 1
                 ENDIF
  160 CONTINUE
      IWORK(ISW+1) = NG + 1
C
      NSET     = ISW
      JSUM     = 0
      DO 200 LOP = 1 , NSET
      IST      = IWORK(LOP)
      IEND     = IWORK(LOP+1) - 1
      ISUM     = 0
      DO 190 J = IST , IEND
      ISUM     = ISUM + INTNO(J)
      INTNO(J) = 0
  190 CONTINUE
      INTNO(IST) = ISUM
      MST(LOP)   = JSUM + 1
      JSUM       = JSUM + ISUM
      MEND(LOP)  = JSUM
  200 CONTINUE
C     SET INTBL & ENGD & NXG
      INTSQ    = 0
      NISQ     = 1
      JSW      = 1
      DELUU    = UIGP(1)
      DSAVE    = EBOUND(1)
      ENGPSQ   = DSAVE*DEXP(0.5*DELUU)
      DO 300 I = 1,NGMAX
      IF(I.LE.NI(NISQ))    GO TO 250
      NISQ     = NISQ  + 1
      IF(INTNO(NISQ).LE.0) GO TO 250
      INTSQ    = 0
  250 CONTINUE
      INTSQ    = INTSQ + 1
      INTBL(I) = INTSQ
      DSAVE    = ENGPSQ*DEXP(-DELUU)
      IF(DSAVE.LT.EBOUND(JSW+1)) THEN
                                 DELUU    = 0.5*(UIGP(JSW+1)+DELUU)
                                 DSAVE    = ENGPSQ*DEXP(-DELUU)
                                 NXG(JSW) = I - 1
                                 JSW      = JSW + 1
                                 DELUU    = UIGP(JSW)
                                 ENDIF
      ENGD(I)  = DSAVE
      ENGPSQ   = DSAVE
  300 CONTINUE
      NXG(JSW) = NGMAX
C
C     PRINT THE  USER'S MCROSS ENERGY STRUCTURE
C
      IF(IPRINT.GT.0 ) THEN
C
      WRITE(NPRT,703) NEF,NG,ISTART,LIBTYP,MAXINT,NGMAX,KFGP,NOMESH
      WRITE(NPRT,704)
      DO 710           I = 1 , NOMESH
      WRITE(NPRT,705) I,EBOUND(I),EBOUND(I+1),UIGP(I),UFGP(I),
     +                   NFI(I),NXG(I)
  710 CONTINUE
      WRITE(NPRT,704)
                       ENDIF
C
      ISUM     = 1
      DSAVE    = 0.0
      ISAVE    = 0
      IF(IPRINT.GT.0) WRITE(NPRT,701)
      DO 700 I = 1,NG
      JSAVE    = NI(I) - ISAVE
      IF(IPRINT.LE.0) GO TO 699
C
      IF(INTNO(I).GT.0) THEN
                        WRITE(NPRT,702) I,EE(I),EE(I+1),UBGP(I),JSAVE,
     +                                   NI(I),INTBL(ISUM),INTNO(I)
                        ELSE
                        WRITE(NPRT,702) I,EE(I),EE(I+1),UBGP(I),JSAVE,
     +                                   NI(I),INTBL(ISUM)
                        ENDIF
C
  699 CONTINUE
      ISUM     = 1     +  NI(I)
      DSAVE    = DSAVE + UBGP(I)
      UBGP(I)  = DSAVE
      ISAVE    = NI(I)
  700 CONTINUE
C
CDEL  WRITE(NPRT,707) (NOIG(I),I=1,NOMESH)
CDEL  WRITE(NPRT,708) (UBGP(I),I=1,NG)
CDEL  WRITE(NPRT,711)   NSET
CDEL  WRITE(NPRT,712) (MST(I),I=1,NSET)
CDEL  WRITE(NPRT,713) (MEND(I),I=1,NSET)
C
CDEL  WRITE(NPRT,714)
CDEL  WRITE(NPRT,706) (I,ENGD(I),INTBL(I),I=1,NGMAX)
C
C     OUTPUT 'CONT0001' TO USER'S MCROSS LIBRARY
C
      LENG     = 9
      IWORK(1) = LIBTYP
      IWORK(2) = NEF
      IWORK(3) = ISTART
      IWORK(4) = NG
      IWORK(5) = NOMESH
      IWORK(6) = KFGP
      IWORK(7) = NGMAX
      IWORK(8) = MAXINT
      IWORK(9) = NSET
C
      MEMBER   = 'CONT0001'
      IRC      = 0
      CALL SEARCH( MEMBER , LEN   , IRC )
      IF(IRC.EQ.0) CALL DELETE ( MEMBER )
      CALL WRITE ( MEMBER , IWORK , LENG )
C
C     OUTPUT 'CONT0002' TO USER'S MCROSS LIBRARY
C
      LENG     = 75 + 47 + 4*46 + 30
      MEMBER   = 'CONT0002'
      IRC      = 0
      CALL SEARCH( MEMBER , LEN   , IRC )
      IF(IRC.EQ.0) CALL DELETE ( MEMBER )
      CALL WRITE ( MEMBER , ENERGY, LENG )
C
C     OUTPUT 'CONT0003' TO USER'S MCROSS LIBRARY
C
      LENG     = 2 * ( 11 + 10 + 10 + 46 )
      MEMBER   = 'CONT0003'
      IRC      = 0
      CALL SEARCH( MEMBER , LEN   , IRC )
      IF(IRC.EQ.0) CALL DELETE ( MEMBER )
      CALL WRITE ( MEMBER , EBOUND, LENG )
C
C     OUTPUT 'CONT0004' TO USER'S MCROSS LIBRARY
C
      LENG     =  NGMAX
      MEMBER   = 'CONT0004'
      IRC      = 0
      CALL SEARCH( MEMBER , LEN   , IRC )
      IF(IRC.EQ.0) CALL DELETE ( MEMBER )
      CALL WRITE ( MEMBER , INTBL , LENG )
C
C     OUTPUT 'CONT0005' TO USER'S MCROSS LIBRARY
C
      LENG     =  NGMAX
      MEMBER   = 'CONT0005'
      IRC      = 0
      CALL SEARCH( MEMBER , LEN   , IRC )
      IF(IRC.EQ.0) CALL DELETE ( MEMBER )
      CALL WRITE ( MEMBER , ENGD  , LENG )
C
C     OUTPUT 'CONTTEMP' TO USER'S MCROSS LIBRARY
C
      LENG     =  81
      MEMBER   = 'CONTTEMP'
      IRC      = 0
      CALL SEARCH( MEMBER , LEN   , IRC )
      IF(IRC.EQ.0) CALL DELETE ( MEMBER )
      CALL WRITE ( MEMBER , NTEMP , LENG )
C
C     END OF PROCESS
C
      RETURN
C
  701 FORMAT(//1H ,35X,'ENERGY GROUP STRUCTURE FOR USER''S MCROSS',
     1/1H0,30X,'ENERGY (EV)',11X,'LETHAGY',9X,'NUMBER OF   START   '
     2,2X,'LENGTH OF'/1H ,22X,'NO.',2X,'UPPER   -    LOWER'
     3,7X,'WIDTH',11X,'I.M. GROUP  POSITION  SUBSECTION ')
  702 FORMAT(1H ,20X,I4,1X,F8.4,1X,' - ',1X,F8.4,4X,F9.5,6X,3I6,I12)
  703 FORMAT(//1H ,25X,
     +     ' << INFORMATION OF USER''S MCROSS ENERGY MESH >>'/
     +/1H ,20X,'NEF    ; NO. OF NERGY GROUP IN FAST-LIB. --------- ',I6,
     +/1H ,20X,'NG     ; NO. OF ENERGY GROUP IN PEACO ------------ ',I6,
     +/1H ,20X,'ISTART ; INITIAL GROUP NO. AT WHICH PEACO START -- ',I6,
     +/1H ,20X,'LIBTYP ; ENERGY MESH DEVIDION TYPE --------------- ',I6,
     +/1H ,20X,'MAXINT ; MAXIMUN NO. OF INERMEDIATE GROUP -------- ',I6,
     +/1H ,20X,'NGMAX  ; TOTAL INTERMEDIATE GROUP NO. ------------ ',I6,
     +/1H ,20X,'KFGP   ; NO. OF FINE GROUP IN A INTERMEDIATE GRP.- ',I6,
     +/1H ,20X,'NOMESH ; NO. OF BROAD GROUP IN MCROSS LIB.-------- ',I6,
     +//1H ,10X,'MESH',2X,' UPPER    ',2X,'LOWER     ',2X,'  UIGP    ',
     +                 2X,'  UFGP    ',2X,' NFI    CUMULATIVE'
     + /1H ,10X,' NO ',2X,'ENERGY(EV)',2X,'ENERGY(EV)',2X,'(INTERME.)',
     +                 2X,' (FINE)   ',2X,'        MESH NO. ')
  704 FORMAT(1H , 9X,9(8H--------),4H----)
  705 FORMAT(1H ,10X,I3,4F12.6,3(I6,6X))
  706 FORMAT(1H ,10X,I4,E12.5,I12,10X,I4,E12.5,I12,10X,I4,E12.5,I12)
  707 FORMAT(1H ,' ## NOIG ## ',10I10)
  708 FORMAT(1H ,' ## UBGP ## ',1P10E11.4)
  709 FORMAT(1H ,' ERROR STOP AT SUBROUTINE(UMCENG) BECAUSE OF FATAL ',
     +           'PROGRAMING ERROR ]] ',
     +/1H ,' LENGTH OF INTBL OR ENGD ARRAY IS NEEDED ',I6,' WARDS.')
  711 FORMAT(1H ,' ## NSET ## ',10I10)
  712 FORMAT(1H ,' ## MST  ## ',10I10)
  713 FORMAT(1H ,' ## MEND ## ',10I10)
  714 FORMAT(/1H ,' ## NO ENGD INTBL ## ')
  719 FORMAT(1H ,' ERROR STOP AT SUBROUTINE(UMCENG) BECAUSE ',A8,
     +           ' IS NOT FOUND IN FASTU LIBRARY ]] '/)
C
  901 WRITE(NSYSO,709) NGMAX
      WRITE(NPRT ,709) NGMAX
      STOP 901
C
  911 WRITE(NSYSO,719) MEMBER
      WRITE(NPRT ,719) MEMBER
      STOP 911
C
      END
