      SUBROUTINE UMCPLT(MEMBER,X,Y,NP,IMAX,X1,Y1,NPMAX,TEMP,MT)
C
      CHARACTER*8    MEMBER
      CHARACTER*10   TITLE(4),YTITLE(4),XTITLE(4)
      CHARACTER*8    IDA,IDR,IDN
C
      REAL*4         X(IMAX),Y(IMAX),X1(NPMAX),Y1(NPMAX)
C
      DIMENSION      ICUMN(1),IMAX1(1),JPLOT(1),IIP(1),NNP(1),
     1               IIST(1),NEWP(1),A(2),B(2)
C
CKSK  DATA           IDA,IDR,IDN / 8HTEMP.   ,8H  =     ,8H        /
      DATA   IDA,IDR,IDN / 'TEMP.   ','  =     ','        ' /
C
C     INITIAL SET
C
      NP1  = NP + 1
C
CDEL  WRITE(6,7) MT,NP,IMAX,NPMAX,TEMP,MEMBER
CDEL  WRITE(6,8) (X(I),I=1,100)
CDEL  WRITE(6,9) (Y(I),I=1,100)
CDEL  WRITE(6,8) (X(NP1-I),I=1,100)
CDEL  WRITE(6,9) (Y(NP1-I),I=1,100)
C
CDEL7 FORMAT(1H ,' ## MT NP IMAX NPMAX TEMP MEMBER ## ',4I6,F8.2,2X,A8)
CDEL8 FORMAT(1H ,' ## X ## ',1P10E11.4)
CDEL9 FORMAT(1H ,' ## Y ## ',1P10E11.4)
C
      DO 10 I = 1 , NP
      X1(I)    = X(NP1-I)
      Y1(I)    = Y(NP1-I)
   10 CONTINUE
C
      CALL CLEA(X,IMAX,0.0)
      CALL CLEA(Y,IMAX,0.0)
C
      DO 20 I = 1 , NP
      X (I)    = X1(I)
      Y (I)    = Y1(I)
   20 CONTINUE
C
      CALL  CLEA( X1 , NPMAX , 0.0 )
      CALL  CLEA( Y1 , NPMAX , 0.0 )
C
      WRITE(IDN,403) TEMP
  403 FORMAT(F7.2,1HK)
C
      JMAX1    = 1
      NNN      = 1
      MAXD     = NPMAX
      MSCALE   = 0
C
      JPLOT(1) = 1
      NNP(1)   = 1
      IIP(1)   = 1
      NEWP(1)  = 1
      IIST(1)  = 0
      A(1)     = X(1)
      B(1)     = X(NP)
      IF(B(1).GT.200.0) THEN
                        NNN    = 2
                        A(2)   = 10.0000
                        B(1)   = 10.0000
                        B(2)   = X(NP)
                        ENDIF
C
      ICUMN(1) = 1
      IMAX1(1) = NP
      WITHX    = 240.
      WITHY    = 200.
      RATIOX   = 10.0
      RATIOY   = 1.0
CMOD HOLLERITH CONST --> CHARACTER CONST
      YTITLE(1) = '          '
      YTITLE(2) = ' CROSS SEC'
      YTITLE(3) = 'TION (BARN'
      YTITLE(4) = ')         '
      XTITLE(1) = '          '
      XTITLE(2) = ' ENERGY (E'
      XTITLE(3) = 'V)        '
      XTITLE(4) = '          '
       TITLE(1) = ' FIG.  ' //  MEMBER (2:4)
       TITLE(3) = 'ROSS SECTI'
       TITLE(4) = 'ON        '
      IF(MT.EQ.  2) TITLE(2) = ' ELASTIC C'
      IF(MT.EQ. 18) TITLE(2) = ' FISSION C'
      IF(MT.EQ.102) TITLE(2) = ' CAPTURE C'
C
      CALL GPLOTZ(JMAX1,X1,Y1,X,Y,ICUMN,IMAX1,IDA,IDR,IDN,
     +            XTITLE,YTITLE,TITLE,JPLOT,NNN,IIP,NNP,IIST,
     +            A,B,WITHX,WITHY,MSCALE,RATIOX,RATIOY,MAXD,NEWP)
      CALL PLOT(0.,0.,444)
      CALL PLOT(0.,0.,777)
      CALL PLOT(0.,0.,444)
C
      RETURN
      END
