      SUBROUTINE PCOEXP(N ,M  ,PO,PN ,IA ,SIGF,SIGM,SIGFE,VF,VM,
     *                  ALPHAF,ALPHAM,PFF,PFM ,PMF ,PMM  ,
     *                  SIG   ,MAR0  ,VOLR0   ,IDRREG    ,KCOMP)
C
C
C
      DIMENSION  PO(N,N),PN(M,M)
      DIMENSION  IA(N)
      DIMENSION  SIG(KCOMP),MAR0(N),VOLR0(N),IDRREG(M)
C
      VCELL  = VF + VM
      VFVF   = VF / VCELL
      VMVM   = VM / VCELL
      SHIELD = SIGFE / SIGF
      ISHIFT = 0
C
C----- LOOP OF SOURCE REGION
C
      DO 1000 I = 1 , N
      JSHIFT = 0
      IF(IA(I).EQ.1)  GO TO 600
C    *************************
C    * FROM MODERATOR REGION *
C    *************************
              DO 500 J=1,N
              IF(IA(J).EQ.1)  GO TO 100
C         ===> TO MODERATOR REGION
              PN(I+ISHIFT,J+JSHIFT) = PO(I,J)
              GO TO 500
C         ===> TO RESONANT REGION
C                  ===> TO ABSORBER LUMP
  100                  PN(I+ISHIFT,J+JSHIFT  )=PO(I,J)*ALPHAF
C                  ===> TO MICROSCOPIC MODERATOR )
                       PN(I+ISHIFT,J+JSHIFT+1)=PO(I,J)*ALPHAM
C             ***************************************
C             *RECIPROCITY RELATION TO THE ABOVE TWO*
C             *   ABSORBER LUMP ===> OUTER MODERATOR*
C             ***************************************
C                      PN(J+JSHIFT  ,I+ISHIFT) = PO(J,I)*SHIELD
C                      PN(J+JSHIFT+1,I+ISHIFT) = PO(J,I)
         IM    = MAR0(I)
         SIGMM = SIG(IM)
         SIGMMV= SIGMM*VOLR0(I)
         PN(J+JSHIFT  ,I+ISHIFT) = PN(I+ISHIFT,J+JSHIFT  )*
     *                             SIGMMV / ( VFVF*VOLR0(J)*SIGF )
         PN(J+JSHIFT+1,I+ISHIFT) = PN(I+ISHIFT,J+JSHIFT+1)*
     *                             SIGMMV / ( VMVM*VOLR0(J)*SIGM )
         JSHIFT = JSHIFT + 1
  500    CONTINUE
         GO TO 1000
C    ************************
C    * FROM RESONANT REGION *
C    ************************
  600 JSHIFT = 0
      DO 900 J = 1 , N
      IF(J.EQ.I)      GO TO 800
      IF(IA(J).EQ.0)  GO TO 900
C          ************************
C          * OTHER RESONAT REGION *
C          ************************
C   ABSORBER  ===>  ABSORBER LUMP
        PN(I+ISHIFT  ,J+JSHIFT  ) = PO(I,J)*ALPHAF*SHIELD
C   ABSORBER  ===>  MICROSCOPIC MODERATOR
        PN(I+ISHIFT  ,J+JSHIFT+1) = PO(I,J)*ALPHAM*SHIELD
C   MODERATOR ===>  ABSORBER LUMP
        PN(I+ISHIFT+1,J+JSHIFT  ) = PO(I,J)*ALPHAF
C   MODERATOR ===>  MICROSCOPIC MODERATOR
        PN(I+ISHIFT+1,J+JSHIFT+1) = PO(I,J)*ALPHAM
        GO TO 890
C        ****************************************************
C        * FROM RESONANT REGION TO THE SAME RESONANT REGION *
C        ****************************************************
C   ABSORBER  ===>  ABSORBER
  800   PN(I+ISHIFT,I+ISHIFT  )   = PFF - (1.0-PO(I,I))*ALPHAF*SHIELD
C   ABSORBER  ===>  MODERATOR
        PN(I+ISHIFT,I+ISHIFT+1)   = PFM - (1.0-PO(I,I))*ALPHAM*SHIELD
C   MODERATOR ===>  ABSORBER
        PN(I+ISHIFT+1,I+ISHIFT  ) = PMF - (1.0-PO(I,I))*ALPHAF
C   MODERATOR ===>  MODERATOR
        PN(I+ISHIFT+1,I+ISHIFT+1) = PMM - (1.0-PO(I,I))*ALPHAM
  890   JSHIFT = JSHIFT + 1
  900 CONTINUE
      ISHIFT = ISHIFT + 1
 1000 CONTINUE
C
C----- TEST NORMALIZATION
C
                   DO 2000 I = 1 , M
                   SUM = 0.0
                   DO 1500 J = 1 , M
                   SUM = SUM + PN(I,J)
 1500              CONTINUE
C                  WRITE(6,2001) I,SUM
                   IF(SUM.EQ.1.0)  GO TO 2000
C
                        DO 1700 J = 1 , M
                        PN(I,J) = PN(I,J)/SUM
 1700                   CONTINUE
 2000              CONTINUE
C
C                  WRITE(6,2002) SIGF,SIGFE,SHIELD,VFVF,VMVM
C                  WRITE(6,2003) MAR0
C                  WRITE(6,2004) VOLR0
C                  WRITE(6,2005) SIG
C2001 FORMAT(1H ,' ## I SUM ## ',I10,1P3E12.5)
C2002 FORMAT(1H ,' ## SIGFE SIGF SHIELD VFVF VMVM ## ',1P5E12.5)
C2003 FORMAT(1H ,' ## MAR0   ## ',9I12)
C2004 FORMAT(1H ,' ## VOLR0  ## ',1P9E12.5)
C2005 FORMAT(1H ,' ## SIG    ## ',1P9E12.5)
C
C---- END
C
      RETURN
      END
