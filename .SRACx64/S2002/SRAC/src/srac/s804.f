C             S804                LEVEL=2        DATE=85.01.25
      SUBROUTINE S804(DSN,W,WD,MR,PNC,Z,AB,CP,PN,A,M1,ISC,IFM)
C   *** S804 CHECKS SN CONSTANTS, COMPUTES PL CONSTANTS
      DIMENSION DSN(1),W(1),WD(1),MR(1),PNC(M1,1),Z(1),AB(1),CP(1),
     & PN(M1,ISC,ISC),A(1)
      COMMON /SN1C/
     &              D(1),LIM1,LR,LW,LDSN,LMA,LMZ,LMB,LMC,LXMD,LFIX,LFLT,
     &       LJ5,LRM,LDF,LJ3,LJ4,LIGT,LART,LALFT,
     &LFGP,LFGG,LEND,LV,LAA,LWD,LMR,LPNC,
     &ID,ITH,ISCT,ISN,IGE,IBL,IBR,IZM,IM,IEVT,IGM,IHT,IHS,IHM,MS,MCR,MTP
     &,MT,IDFM,IPVT,IQM,IPM,IPP,IIM,ID1,ID2,ID3,ID4,ICM,IDAT1,IDAT2,IFG,
     &IFLU,IFN,IPRT,IXTR,
     &EV,EVM,EPS,BF,DY,DZ,DFM1,XNF,PV,RYF,XLAL,XLAH,EQL,XNPM,
     &T(12),NIN,NOU,MM,JT
C *****
      I09=ISC
C *****
  804 E1=0.0
      M=MM
    1 WD(M)=DSN(M)*W(M)
      E1=E1+WD(M)+W(M)
      IF(WD(M))2,3,4
C   3 IF(DSN(M).EQ.0.0)CALL ERRO(  'SN-1',M)
    3 IF(DSN(M).EQ.0.0) THEN
               WRITE(NOU,3000) M
               STOP 7
               ENDIF
 3000 FORMAT('0***ERROR*** S-N COSINES MU(',I4,' ) = 0.0')
      MR(M)=MR(M+1)
    4 M=M-1
      IF(M.GT.0)GO TO 1
C     IF(.0001.LT.ABS(1.0-E1))CALL ERRO(  'SN-2',0)
      IF(.0001.LT.ABS(1.0-E1)) THEN
               WRITE(NOU,3010)
               STOP 7
               ENDIF
 3010 FORMAT('0***ERROR*** S-N WEIGHTS DO NOT SUM TO 1.0')
    5 GO TO 7
    2 K=M
    6 K=K+1
C     IF(K.GT.MM)CALL ERRO(  'SN-3',0)
      IF(K.GT.MM) THEN
               WRITE(NOU,3020)
               STOP 7
               ENDIF
 3020 FORMAT('0***ERROR*** S-N CONSTANTS ARE NOT SYMMETRIC ABOUT MU',
     &       ' = 0.0'   )
      IF(.0001.LT.ABS(DSN(M)+DSN(K)))GO TO 6
      MR(K)=M
      MR(M)=K
      GO TO 4
    7 IF(ISCT.EQ.0 .OR. IXTR.NE.0)GO TO 999
C ------ THE FOLLOWING SECTION STOLEN FROM K D LATHROP OF LASL
      IF(JT.GT.ISCT)GO TO 10
      DO 8 M=1,MM
    8 PNC(M,1)=DSN(M)
      IF(ISCT.EQ.1)GO TO 999
      DO 9 M=1,MM
    9 PNC(M,2)=1.5*DSN(M)*DSN(M) - 0.5
      IF(ISCT.EQ.2)GO TO 999
      DO 11 N=3,JT
      E1=1.0 - 1.0/FLOAT(N)
      E2=E1+1.0
      DO 11 M=1,MM
      PNC(M,N)=E2*DSN(M)*PNC(M,N-1) - E1*PNC(M,N-2)
   11 CONTINUE
      GO TO 999
   10 DO 12 M=1,MM
      E1=DSN(M)
      IF(W(M).NE.0.0)GO TO 13
      Z(M)=SQRT(1.0 - E1*E1)
      AB(M)=ABS(E1)
      GO TO 14
   13 Z(M)=Z(M-1)
      AB(M)=AB(M-1)
   14 CP(M)=0.0
      IF(W(M).NE.0.0)CP(M)=ATAN(SQRT(1.0 - Z(M)*Z(M) - E1*E1)/E1)
      IF(E1.LT.0.0)CP(M)=CP(M)+3.1415927
   12 CONTINUE
        DO 511 M=1,MM
        DO 511 N=1,I09
        DO 511 K=1,I09
  511 PN(M,N,K)=0.0
      DO 15 M=1,MM
      PN(M,1,1)=1.0
      PN(M,2,1)=Z(M)
      DO 15 N=2,ISCT
      E1=1.0 - 1.0/FLOAT(N)
      E2=E1 + 1.0
   15 PN(M,N+1,1)=E2*Z(M)*PN(M,N,1) - E1*PN(M,N-1,1)
      DO 16 M=1,MM
      DO 16 J=2,ISC
      E1=2*J-3
      DO 16 N=1,ISC
      IF(N-J)16,17,18
   17 PN(M,N,J)=AB(M)*PN(M,N-1,J-1)*E1
   18 IF(N.EQ.ISC)GO TO 16
      E1=N+J-2
      E2=N-J+1
      E3=2*N-1
      PN(M,N+1,J)=(E3*Z(M)*PN(M,N,J) - E1*PN(M,N-1,J))/E2
   16 CONTINUE
      A(1)=1.0
      DO 19 I=2,IFM
   19 A(I)=FLOAT(I-1)*A(I-1)
      DO 20 J=2,ISC
      E1=J-1
      DO 20 N=J,ISC
      K=N+J-1
      KA=N-J+1
      E2=SQRT(2.0*A(KA)/A(K))
      DO 20 M=1,MM
      PN(M,N,J)=E2*PN(M,N,J)*COS(E1*CP(M))
   20 CONTINUE
      II=1
      DO 21 N=2,ISC
      DO 21 J=1,N
      K=(N+J)/2
      KA=(N+J+1)/2
      IF(K.NE.KA)GO TO 21
      DO 22 M=1,MM
      IF(ABS(PN(M,N,J)).LE.0.00001)PN(M,N,J)=0.0
   22 PNC(M,II)=PN(M,N,J)
      II=II+1
   21 CONTINUE
  999 RETURN
      END
