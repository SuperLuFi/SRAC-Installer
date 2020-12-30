      SUBROUTINE REACT1(MTNAME,NMESH ,IREACT,MESH  ,FG    ,
     1                  SIGMA ,FLUX  ,BUFF  ,IBUFF ,R,
     2                  NCASE ,IMESH ,IGMAXC,LBUFF ,IFILTR,
     3                  NOUT1 ,NOUT2 ,MEMORY,LSIGB ,IEND  ,
     4                  NGSTRT,IRANG ,ICF   ,IGMAX ,NGF   ,NGT    )
C
C     CALCULATE  DETECTOR REACTION RATE
C
      COMMON /MAINC/ IOPTS(100)
      DIMENSION R(2,IMESH),MTNAME(2,NCASE),NMESH(NCASE),IREACT(NCASE),
     1          MESH(3,IMESH),FG(IGMAX,NCASE),SIGMA(2,IGMAXC),
     2          FLUX(IGMAXC,IMESH),BUFF(LBUFF),IBUFF(LBUFF)
      CHARACTER * 28 TITL(2)
      CHARACTER * 4 MTNAME
      DATA TITL/'NON FILTERED DETECTOR ----- ',
     1          'FILTERED DETECTOR -----     '        /
C
      L1 = 1
      IF (IEND.EQ.1) WRITE(NOUT2,6000) TITL(IFILTR+1)
      DO 1000 I=1,NCASE
          IREAC1=IREACT(I)+1
          CALL LSIGMA(MTNAME(1,I),SIGMA,BUFF,IBUFF,IGMAXC,LBUFF,
     1                NOUT1,MEMORY,LSIGB,IRANG,ICF ,NGF ,NGT   )
                    IF(IOPTS(18).LT.0) THEN
              WRITE(7,10) MTNAME(1,I),MTNAME(2,I)
   10 FORMAT(2A4)
                     GO TO (30,40,50),IREAC1
   30         WRITE(7,20) (SIGMA(1,NG),NG=1,IGMAXC)
                     GO TO 60
   40         WRITE(7,20) (SIGMA(2,NG),NG=1,IGMAXC)
                     GO TO 60
   50         WRITE(7,20) (SIGMA(1,NG),NG=1,IGMAXC)
              WRITE(7,20) (SIGMA(2,NG),NG=1,IGMAXC)
   60 CONTINUE
   20 FORMAT(6E12.5)
                                       ENDIF
          IF (IEND.EQ.0) GO TO 100
          WRITE(NOUT2,6100) (MTNAME(J,I),J=1,2),IREACT(I)
  100     CONTINUE
          L2=L1+NMESH(I)-1
          IF (IEND.EQ.0) GO TO 130
          GO TO (105,110,120),IREAC1
  105     CONTINUE
          WRITE(NOUT2,6200)
          GO TO 130
  110     CONTINUE
          WRITE(NOUT2,6300)
          GO TO 130
  120     CONTINUE
          WRITE(NOUT2,6400)
  130     CONTINUE
          DO 2000 J=L1,L2
          IF (IFILTR.EQ.1) THEN
             IDO = 2
             ELSE
               IDO = 1
          ENDIF
          DO 1910 K=1,IDO
              IFILT1 = K-1
              IF(IREAC1.LT.1 .OR. IREAC1.GT.3) GO TO 300
              GO TO (140,200,300),IREAC1
  140         CONTINUE
C----- FISSION REACTION
              CALL DREACT(IGMAXC,0,IFILT1,FG(NGSTRT,I),SIGMA,FLUX(1,J),
     1                    R(1,J),JE)
              CALL REAPRT(MESH(1,J),MESH(2,J),MESH(3,J),1,R(1,J),JE,
     1                    IEND     ,NOUT2    ,IFILTR   ,IFILT1        )
              GO TO 1900
  200         CONTINUE
C----- CAPTURE REACTION
              CALL DREACT(IGMAXC,1,IFILT1,FG(NGSTRT,I),SIGMA,FLUX(1,J),
     1                    R(2,J),JE)
              CALL REAPRT(MESH(1,J),MESH(2,J),MESH(3,J),2,R(1,J),JE,
     1                    IEND     ,NOUT2    ,IFILTR   ,IFILT1        )
              GO TO 1900
  300         CONTINUE
C----- FISSION & CAPTURE
              CALL DREACT(IGMAXC,0,IFILT1,FG(NGSTRT,I),SIGMA,FLUX(1,J),
     1                    R(1,J),JE)
              CALL DREACT(IGMAXC,1,IFILT1,FG(NGSTRT,I),SIGMA,FLUX(1,J),
     1                    R(2,J),JE)
              CALL REAPRT(MESH(1,J),MESH(2,J),MESH(3,J),3,R(1,J),JE,
     1                    IEND     ,NOUT2    ,IFILTR   ,IFILT1        )
 1900         CONTINUE
 1910     CONTINUE
 2000     CONTINUE
          L1=L2+1
 1000 CONTINUE
      RETURN
 6000 FORMAT(1H1,10X,
     1          '----- REACTION RATE OF ',A28                      )
 6100 FORMAT(1H0,10X,'MEMBER NAME : ',2A4,'   REACTION :',I2)
 6200 FORMAT(1H0,10X,'X-MESH',4X,'Y-MESH',4X,'Z-MESH',12X,'FISSION')
 6300 FORMAT(1H0,10X,'X-MESH',4X,'Y-MESH',4X,'Z-MESH',12X,'CAPTURE')
 6400 FORMAT(1H0,10X,'X-MESH',4X,'Y-MESH',4X,'Z-MESH',
     1           12X,'FISSION',10X,'CAPTURE')
      END
