      SUBROUTINE PCOFIN(IDENT ,IFISS ,SAA   ,SFF   ,SSS   ,
     +                  KINT  ,KFGP  ,MAXINT,KRESP ,KRES  )
C
C     READ UMCROSS-LIB
C
      COMMON /PCOWK3/ A(14500)
C
      DIMENSION       SAA(KRES,MAXINT,KFGP),SFF(KRES,MAXINT,KFGP),
     +                SSS(KRES,MAXINT,KFGP)
C
      CHARACTER*8     IDENT,NAMEP
C
      NAMEP = IDENT
      LENG  = KINT*KFGP
C
      IF(IFISS.EQ.0)  GO TO 101
      IST1       =   1
      NAMEP(1:1) = 'F'
      CALL READ(NAMEP,A,LENG)
      DO 50    K = 1,KINT
      DO 50    I = 1,KFGP
      SFF(KRESP,K,I)=A(IST1)
      IST1       = IST1 + 1
   50 CONTINUE
C
  101 IST1       =   1
      NAMEP(1:1) = 'C'
      CALL READ(NAMEP,A,LENG)
      DO 150   K = 1,KINT
      DO 150   I = 1,KFGP
      SAA(KRESP,K,I)=A(IST1)
      IST1       = IST1 + 1
  150 CONTINUE
C
      IST1       =   1
      NAMEP(1:1) = 'E'
      CALL READ(NAMEP,A,LENG)
      DO   250 K = 1,KINT
      DO   250 I = 1,KFGP
      SSS(KRESP,K,I)=A(IST1)
      IST1       = IST1 + 1
  250 CONTINUE
C
      CONTINUE
      RETURN
      END
