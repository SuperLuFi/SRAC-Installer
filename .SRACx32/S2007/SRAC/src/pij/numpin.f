      SUBROUTINE  NUMPIN (INST,RR,THE,CYLINP,NDPIN,LL,NUM,IFTYPE,ISIZE)
C
C***********************************************************************
C                                                                      *
C       NUMPIN    : NUMBERNING  PIN POSITION                           *
C                                                                      *
C                                                                      *
C       MODULE NO.  :                                                  *
C       MODULE TYPE : SUBROUTINE                                       *
C       CODER       : HATA KENICHROU                                   *
C       DATE        : 1982.02.03                                       *
C                                                                      *
C***********************************************************************
C                                                                      *
C    -- CALLED BY --                                                   *
C                                                                      *
C        GEOM11                                                        *
C                                                                      *
C    -- CALLS --                                                       *
C                                                                      *
C        INUM                                                          *
C                                                                      *
C......................................................................*
C                                                                      *
C    -- INPUT --                                                       *
C                                                                      *
C       (ARGUMENT)                                                     *
C          INST   :  KIND OF NUMBERING PIN FIGURE                      *
C          RR     :  TABLE OF RADIUS OF REGULAR ARRAYS OF PIN RODS     *
C          THE    :  ANGLE OF PIN POSITION                             *
C          CYLINP :  TABLE OF PIN DIVIDE RADIUS MESH                   *
C          NDPIN  :  NUMBER OF RADIUS FOR PIN ROD DIVIDING             *
C                 :  =1 OR =2 ONLY ACCEPTED
C          LL     :  NUMBERING  POINT                                  *
C          NUM    :  TABLE OF NUMBERS                                  *
C          IFTYPE :  FIGURING SACALE TYPE                              *
C          ISIZE  :  NUMBERING CHARACTER SIZE                          *
C                                                                      *
C                                                                      *
C***********************************************************************
C
C     DIMENSION  NUM(300),CYLINP(10)
      DIMENSION  NUM(1000),CYLINP(50)
C
      DATA   PAI / 3.14159265 /
C
C
C
      L = LL
  100 IF ( NDPIN .GE. 2 )  GO TO 200
         RPIN = CYLINP(NDPIN)
C
  110    IF ( INST .NE. 1 )  GO TO 120
            XC = RR*COS(THE)
            YC = RR*SIN(THE)
            CALL  INUM (XC,YC,IFTYPE,ISIZE,NUM(L))
            RETURN
C
  120    IF ( INST .NE. 2 )  GO TO 130
            R1 = RR-RPIN/2
            R2 = RR+RPIN/2
            CALL  INUM (R1*COS(THE),R1*SIN(THE),IFTYPE,ISIZE,NUM(L))
         IF (RR.GT.0.)
     *      CALL  INUM (R2*COS(THE),R2*SIN(THE),IFTYPE,ISIZE,
     *                     NUM(L+NDPIN-1))
            RETURN
C
C
C
C
  130    CONTINUE
            X0 = RR*COS(THE)
            Y0 = RR*SIN(THE)
            X1 = X0+CYLINP(1)*SIN(THE)/2
            Y1 = Y0-CYLINP(1)*COS(THE)/2
            X2 = X0-CYLINP(1)*SIN(THE)/2
            Y2 = Y0+CYLINP(1)*COS(THE)/2
            CALL  INUM (X1,Y1,IFTYPE,ISIZE,NUM(L))
            CALL  INUM (X2,Y2,IFTYPE,ISIZE,NUM(L+1))
            RETURN
C
  200 CONTINUE
C
  210    IF ( INST .NE. 1 )  GO TO 220
            XC = RR*COS(THE)
            YC = RR*SIN(THE)
            CALL  INUM (XC,YC,IFTYPE,ISIZE,NUM(L))
            YC = YC+(CYLINP(NDPIN)+CYLINP(NDPIN-1))/2
            CALL  INUM (XC,YC,IFTYPE,ISIZE,NUM(L+NDPIN-1))
            RETURN
C
  220    IF ( INST .NE. 2 )  GO TO 230
            R1 = RR-(CYLINP(NDPIN)+CYLINP(NDPIN-1))/2
            R2 = RR-(CYLINP(1))/2
            R3 = RR+(CYLINP(1))/2
            R4 = RR+(CYLINP(NDPIN)+CYLINP(NDPIN-1))/2
         IF( RR .EQ. 0.) GO TO 225
C
            CALL  INUM (R1*COS(THE),R1*SIN(THE),IFTYPE,ISIZE,NUM(LL))
            L = LL+NDPIN-1
            CALL  INUM (R2*COS(THE),R2*SIN(THE),IFTYPE,ISIZE,NUM(L))
            L = LL+NDPIN
            CALL  INUM (R3*COS(THE),R3*SIN(THE),IFTYPE,ISIZE,NUM(L))
            L = LL+2*NDPIN-1
            CALL  INUM (R4*COS(THE),R4*SIN(THE),IFTYPE,ISIZE,NUM(L))
            RETURN
C
  225    CONTINUE
            CALL  INUM (R3*COS(THE),R3*SIN(THE),IFTYPE,ISIZE,NUM(LL))
            L = LL+1
            CALL  INUM (R4*COS(THE),R4*SIN(THE),IFTYPE,ISIZE,NUM(L))
            RETURN
C
  230    CONTINUE
            X0 = RR*COS(THE)
            Y0 = RR*SIN(THE)
            X1 = X0+(CYLINP(NDPIN)+CYLINP(NDPIN-1))*SIN(THE)/2
            Y1 = Y0-(CYLINP(NDPIN)+CYLINP(NDPIN-1))*COS(THE)/2
            X2 = X0+CYLINP(  1  )*SIN(THE)/2
            Y2 = Y0-CYLINP(  1  )*COS(THE)/2
            X3 = X0-CYLINP(  1  )*SIN(THE)/2
            Y3 = Y0+CYLINP(  1  )*COS(THE)/2
            X4 = X0-(CYLINP(NDPIN)+CYLINP(NDPIN-1))*SIN(THE)/2
            Y4 = Y0+(CYLINP(NDPIN)+CYLINP(NDPIN-1))*COS(THE)/2
            CALL  INUM (X1,Y1,IFTYPE,ISIZE,NUM(L))
            CALL  INUM (X2,Y2,IFTYPE,ISIZE,NUM(L+1))
            CALL  INUM (X3,Y3,IFTYPE,ISIZE,NUM(L+2))
            CALL  INUM (X4,Y4,IFTYPE,ISIZE,NUM(L+3))
C
      RETURN
      END
