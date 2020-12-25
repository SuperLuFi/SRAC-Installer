C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C     MAIN PROGRAM TO TEST SUBROUTINE ZZMMM
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
*     INTEGER  IC,NUM(10)
*     CHARACTER*6 SYMB(10)
C
*     NISO = 6
*     IC=0
*     SYMB(1) = 'D0002'
*     SYMB(2) = 'T-003'
*     SYMB(3) = 'C-000'
*     SYMB(4) = 'XE135'
*     SYMB(5) = 'AM241M'
*     SYMB(6) = 'ZZ115'
*     call ZZMMM(IC,NISO,NUM,SYMB)
*     do I=1,NISO
*       write(6,*) ' No. SYMB NUM =', i,' ',SYMB(i), NUM(i)
*     end do
C
*     IC = 1
*     call ZZMMM(IC,NISO,NUM,SYMB)
*     write(6,*) '**************** IC=+1 ************************ '
*     write(6,600) (SYMB(i),i=1,NISO)
* 600 format(10(A6,X))
C
*     IC = -1
*     call ZZMMM(IC,NISO,NUM,SYMB)
*     write(6,*) '**************** IC=-1 ************************ '
*     write(6,600) (SYMB(i),i=1,NISO)
C
*     IC = 2
*     call ZZMMM(IC,NISO,NUM,SYMB)
*     write(6,*) '**************** IC=+2 ************************ '
*     write(6,600) (SYMB(i),i=1,NISO)
C
*     IC = -2
*     call ZZMMM(IC,NISO,NUM,SYMB)
*     write(6,*) '**************** IC=-2 ************************ '
*     write(6,600) (SYMB(i),i=1,NISO)
C      
*     stop
*     end
C***********************************************************************
      SUBROUTINE ZZMMM(IC,NISO,NUM,SYMB)
C***********************************************************************
C  Conversion between Nuclide symbol(A6) and nuclide number(7 digits)
C  by Keisuke OKUMURA (JAERI)    Last modified : 12 July 2002
C***********************************************************************
C
C          ZZMMML <=> zz*10000+mmm*10+l
C     e.g. U-235  : 922350
C          AM242M : 952421
C          H-001  :  10010
C          D0000  :  10020
C          C-000  :  60000 (Nat.)
C          MT266M :1092661
C          ZZ1010 :   1010 (one of a Psudo F.P. for fast reactor)
C     
C  IC  : CONTROL INTEGER (INPUT)
C        IC  = 0 : from Symbol to Number
C                  "U0" or "U-" are treated as the same input
C                  An output symbol is not changed
C                  Different symbols of Hydrogen(H,D,T) are acceptable
C        IC != 0 : from Number to Symbol
C            = 1 : Symbol "0" is used for a brank character (U0235)
C            =-1 : ibid but Different symbols(H/D/T) of Hydrogen are
C                  used according to mass number (D002 in SRAC)
C            = 2 : Symbol "-" is used for a brank character (U-235)
C            =-2 : ibid but Symbols(H/D/T) of Hydrogen are used 
C                  Memo : IC=-1(for SRAC) and IC=1(for MVP-BURN)
C
C  NISO: NUMBER OF NUCLIDES
C
C  SYMB(NISO):NUCLIDE SYMBOL (INPUT when IC=0, OUTPUT when IC !=0)
C             SYMBOL = ZZ//MMM//L
C             ZZ : Chemical symbol (U-, H0, PU,...)
C             MMM: Mass number (001-999)
C             L  : =' '(ground state), ='M'(excited state)
C             Exceptionals:
C               1) Pseudo F.P. nuclides (ZZsnn)
C                  ZZ : Pseudo FP
C                  s  : =0 for thermal reactor / =1 for fast reactor
C                  nn : ID number for Pseudo types (chain model types)
C
C  NUM(NISO) :NUCLIDE NUMBERS (OUTPUT when IC=0, INPUT when IC!=0)
C             zzmmml ( zz*10000 + mmm*10 + l )
C             zz : Atomic number
C             mmm: Mass number
C             l  : =0(ground sate), =1 (excited state)
C             Exceptionals: from 0 to 9999
C               1) Pseudo FP (from 1 to 1999)
C                  zz=0
C                  mmm = snn (ID number of Pseudo types)
C                    10 -  990 : Pseudo for thermal reactors
C                  1010 - 1990 : Pseudo for fast reactors
C
C***********************************************************************
C
      CHARACTER*6 SYMB(*)
      INTEGER I,NUM(*),N1,N2,N3
      CHARACTER*2 S1
      CHARACTER*3 S2
      CHARACTER*1 S3
      CHARACTER*2 ZSYMB(110)
      DATA ZSYMB/'H-','HE','LI','BE','B-','C-','N-','O-','F-','NE',
     1           'NA','MG','AL','SI','P-','S-','CL','AR','K-','CA',
     2           'SC','TI','V-','CR','MN','FE','CO','NI','CU','ZN',
     3           'GA','GE','AS','SE','BR','KR','RB','SR','Y-','ZR',
     4           'NB','MO','TC','RU','RH','PD','AG','CD','IN','SN',
     5           'SB','TE','I-','XE','CS','BA','LA','CE','PR','ND',
     6           'PM','SM','EU','GD','TB','DY','HO','ER','TM','YB',
     7           'LU','HF','TA','W-','RE','OS','IR','PT','AU','HG',
     8           'TL','PB','BI','PO','AT','RN','FR','RA','AC','TH',
     9           'PA','U-','NP','PU','AM','CM','BK','CF','ES','FM',
     &           'MD','NO','LR','RF','DB','SG','BH','HS','MT','ZZ'/
C
C     Note : ZSYMB(110)=ZZ : Symbol for Pseudo Fission Products
C
C=======================================================================
C
      MAXZ = 110
      IF (IC.NE.0) GOTO 2000
C
C-----IC=0 : SYMBOL => NUMBER ------------------------------------------
C
      DO 1100 J=1,NISO
        S1=SYMB(J)(1:2)
        S2=SYMB(J)(3:5)
        S3=SYMB(J)(6:)
        IF(S1(2:2).EQ.'0') S1(2:2)='-'
        IF(S1.EQ.'D-' .OR. S1.EQ.'T-') THEN
          S1 = 'H-'
        ENDIF
        N1 = 0
        DO 100 I=1,MAXZ
          IF (S1.EQ.ZSYMB(I)) N1=I
  100   CONTINUE
        IF(N1.EQ.0) THEN
          WRITE(6,6000) SYMB(J)
          STOP 999
        ENDIF
        IF(N1.EQ.110) N1=0
C
        READ(S2,'(I3)') N2
        IF (S3.EQ.' ') THEN
          N3=0
        ELSEIF (S3.EQ.'M') THEN
          N3=1
        ELSE
          WRITE(6,6000) SYMB(J)
          STOP 999
        ENDIF
        NUM(J) = N1*10000+N2*10+N3
 1100 CONTINUE
      GOTO 9999
C
C6000 FORMAT(//1H ,'<<<  ERROR STOP (ZZZMM)  >>>',/,1X,
 6000 FORMAT(//1H ,'XXX(ZZZMM):',
     &'Input nuclide symbol(',A,') can not be identified')
C
C-----IC!=0 : NUMBER => SYMBOL -----------------------------------------
C
 2000 CONTINUE
      IF (IABS(IC).GT.2) THEN
        WRITE(6,6100) IC
        STOP 999
      ENDIF
C
      DO 2100 J=1,NISO
        N1=INT(NUM(J)/10000)
        N2=INT((NUM(J)-N1*10000)/10)
        N3=MOD(NUM(J),10)
C
        IF (N1.GT.0 .AND. N1.LT.MAXZ) THEN
          S1=ZSYMB(N1)
        ELSEIF (N1.EQ.0) THEN
          S1=ZSYMB(110)
        ELSE
          WRITE(6,6200) NUM(J)
          STOP 999
        ENDIF
        IF(IABS(IC).EQ.1 .AND. S1(2:2).EQ.'-') S1(2:2)='0'
C
        WRITE(S2,'(I3.3)') N2
        IF(IC.LT.0 .AND. N2.EQ.2) S1(1:1)='D'
        IF(IC.LT.0 .AND. N2.EQ.3) S1(1:1)='T'
C
        IF (N3.EQ.0) THEN
          S3=' '
        ELSEIF (N3.EQ.1) THEN
          S3='M'
        ELSE
          WRITE(6,6200) NUM(J)
          STOP 999
        ENDIF
        SYMB(J)=S1//S2//S3
C
 2100 continue
C
C6100 FORMAT(//1H ,'<<<  ERROR STOP (ZZZMM)  >>>',/,1X,
 6100 FORMAT(//1H ,'XXX(ZZZMM):',
     &'Input control integer (',I10,') is invalid')
C6200 FORMAT(//1H ,'<<<  ERROR STOP (ZZZMM)  >>>',/,1X,
 6200 FORMAT(//1H ,'XXX(ZZZMM):',
     &'Input nuclide number(',I10,') can not be identified')
C
 9999 RETURN
      END
