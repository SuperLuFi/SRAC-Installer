      SUBROUTINE PCOPLT(MAR   ,MTNAME,TEMP  ,FLUXS ,X1   ,Y1    )
C
      CHARACTER*4     TIL,ID
C
      COMMON /PCOWK1/ TIL(18),ID(2)
      COMMON /PCOWK2/ KCOMP,KCOMPF,DELBEF,KSREG,KMAT,KNMAX,KRES,NPROB,
     1                NDOUBL,NOUT1,NOUT2,NBB,NBH,MAXN,MAXP,KDAN,
     3                KPIJ1,KPIJ2,ESCAPA,ESCAPF,GUZAI,IPLOT,MAIN(2)
      COMMON /PCOWK3/ X(2000),Y(2000),ICUMN(5),IMAX1(5),JPLOT(5),
     +                IIP(5),NNP(5),IIST(5),A(5),B(5),NEWP(5)
C
      COMMON /UMC001/ LIBTYP,NEF,ISTART,NG,NOMESH,KFGP,NGMAX,MAXINT,NSET
      COMMON /UMC004/ INTBL(2000),ENGD(2000)
C
      DIMENSION       MAR(KSREG),TEMP(KCOMP),
     +                FLUXS(NGMAX,KSREG),X1(NGMAX,5),Y1(NGMAX,5)
      CHARACTER*4     MTNAME(2,KCOMP)
C
      CHARACTER*8     IDA(5),IDR(5),IDN(5)
      CHARACTER*4     TITLE(10),XTITLE(10),YTITLE(10)
      DIMENSION       PELOW( 5),PEUP  ( 5)
C
CKSK  DATA  YTITLE   /3*4H    ,4HARBI,4HTARY,4H UNI,4HT    ,3*4H    /
CKSK  DATA  XTITLE   /3*4H    ,4HENER,4HGY (,4HEV) ,4*4H    /
CKSK  DATA  TITLE    /4H    ,4HNEUT,4HRON ,4HSPEC,4HTRA ,5*4H    /
      DATA  YTITLE   /3*'    ','ARBI','TARY',' UNI','T   ',3*'    '/
      DATA  XTITLE   /3*'    ','ENER','GY (','EV) ',4*'    '/
      DATA  TITLE    /'    ','NEUT','RON ','SPEC','TRA ',5*'    '/
C
C     INITIAL SET
C
      DO 10 I = 1,5
      IIP(I)  = 1
      IF(I.GT.1) IIP(I)=1-I
      NNP(I)  = 0
      IIST(I) = 0
      A(I)    = 0.0
      B(I)    = 0.0
      NEWP(I) = 1
      JPLOT(I)= 1
   10 CONTINUE
      RATIO   = 1.0
      IF(DELBEF.NE.0.0)  RATIO=1.0/DELBEF
C
      WITHX   = 240.0
      WITHY   = 200.0
      MSCALE  =   0
      RATIOY  =  1.0
      RATIOX  =  5.0
      MAXD    = NGMAX
      TITLE(8)= ID(1)
      TITLE(9)= ID(2)
      CALL  CLEA(X1    ,NGMAX*5,0.0)
C
      DO 100 NN=1,5
      DO 100 I=1,NGMAX
      J        = NGMAX-I+1
      X1(I,NN) = ENGD(J)
  100 CONTINUE
C
      NCASE  =KSREG
      IF(IPLOT.EQ.1) NCASE=FLOAT(KSREG)/5.0 + 0.99
      JMAX0  =1
      IF(IPLOT.EQ.1) JMAX0=5
      N0     =0
      A(1)   =ENGD(NGMAX)
      B(1)   =ENGD(1)
      NNN    = 1
C
C-----IPLOT=3 CASE
C
      IF(IPLOT.EQ.3) THEN
      ELOW   = A(1)
      EHI    = B(1)
      NCASE = FLOAT(KSREG)/5.0 + 0.99
      JMAX0 = 5
      WRITE(NOUT1,110)
CKSK  CALL REAG( PELOW , 4 , 4HLOW , 4HENG. )
      CALL REAG( PELOW , 4 , 'LOW ', 'ENG.' )
      WRITE(NOUT1,120)
CKSK  CALL REAG( PEUP  , 4 , 4HHI. , 4HENG. )
      CALL REAG( PEUP  , 4 , 'HI. ', 'ENG.' )
C
      DO 140 NN = 1 , 4
      IF(PELOW(NN).LE.0.0.OR.PEUP(NN).LE.0.0) GO TO 140
      IF(PEUP (NN).LE.PELOW(NN))              GO TO 140
      IF(PELOW(NN).LT.ELOW) PELOW(NN) = ELOW
      IF(PEUP (NN).GT.EHI ) PEUP (NN) = EHI
      NNN = NNN + 1
      A(NNN) = PELOW(NN)
      B(NNN) = PEUP (NN)
  140 CONTINUE
      ENDIF
C
      DO 1000 NN=1,NCASE
      N1 = (NN-1)*JMAX0 + 1
      N2 =   NN  *JMAX0
      IF(N2.GT.KSREG) N2=KSREG
      DO  200 J=N1,N2
      ICUMN(J-N0) = 1
      IF(J.GT.N0+1) ICUMN(J-N0) = ICUMN(J-N0-1)+NGMAX
      IMAX1(J-N0) = NGMAX
  200 CONTINUE
C
      DO 300  J=N1,N2
      DO 300  I=1,NGMAX
      Y1(I,J-N0)=FLUXS(I,J)*RATIO
  300 CONTINUE
C
      JMAX1=N2-N1+1
      DO 400 J=N1,N2
      M=MAR(J)
      WRITE(IDA(J-N0),401) J
      WRITE(IDN(J-N0),402) TEMP(M)
      IDR(J-N0) = MTNAME(1,M) // MTNAME(2,M)
  400 CONTINUE
C
      A(1)=ENGD(NGMAX)
      B(1)=ENGD(1)
      CALL CLEA(X    ,2000,0.0)
      CALL CLEA(Y    ,2000,0.0)
C
      CALL GPLOTZ(JMAX1,X,Y,X1,Y1,ICUMN,IMAX1,IDA,IDR,IDN,
     1            XTITLE,YTITLE,TITLE,JPLOT,NNN,IIP,NNP,IIST,
     2            A,B,WITHX,WITHY,MSCALE,RATIOX,RATIOY,MAXD,NEWP)
      CALL PLOT(0.,0.,444)
      CALL PLOT(0.,0.,777)
      CALL PLOT(0.,0.,444)
      N0=N2
 1000 CONTINUE
      RETURN
C
  110 FORMAT(1H ,'  ENTER (PELOW(I),I=1,4) IN FREE FORMAT  ',
     *      /1H ,' RELOW : LOWER ENERGY TO BE PLOTTED (EV) ',
     *           '   EX. < 5.0 10.0 2(0.0) > ')
  120 FORMAT(1H ,'  ENTER (PEUP (I),I=1,4) IN FREE FORMAT  ',
     *      /1H ,' REUP  : HIGHER ENERGY TO BE PLOTTED (EV) ',
     *           '   EX. < 8.0 15.0 2(0.0) > ')
  401 FORMAT(6HR-REG-,I2)
  402 FORMAT(F7.1,1HK)
C
      END
