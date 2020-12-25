      SUBROUTINE REACT3(NREC  ,MPOSI ,LU235 ,LU238 ,FGS   ,
     1                  NMESH ,MESH  ,R     ,SIGMA ,PHMVM ,
     2                  PHXVX ,FLUX  ,MTNAME,NISO  ,IDENT  ,
     3                  IXMC  ,DN    ,NMAT  ,NTISO ,
     4                  MREC  ,NCASE ,IGMAX ,IGMAXC,NOUT2 ,
     5                  IEND  ,NGSTRT,VOLUME,ISTEP ,IOPT2  )
C
C     CALCULATE INTEGRATIONAL PARAMETER OF FISSILE NUCLIDE
C
      DIMENSION NREC(MREC),MPOSI(NCASE),LU235(NCASE),LU238(NCASE),
     1          FGS(IGMAX,NCASE),NMESH(NCASE),MESH(3,NCASE),
     2          R(8,NCASE),SIGMA(IGMAXC,4),PHMVM(IGMAXC),PHXVX(IGMAXC),
     3          FLUX(IGMAXC,NCASE),VOLUME(NCASE)
      DIMENSION MTNAME(2,NMAT),NISO(NMAT),IDENT(2,NTISO),IXMC(NTISO),
     *          DN(NTISO),DN58(2),NNAME(2)
      DIMENSION ID(8),ITHERM(8),RR(8),VD(2),IZERO(3)
      CHARACTER * 4 NNAME
      DATA ID / 2*1 , 2*2 ,2*3 , 2*4 /
      DATA ITHERM / 0 , 1 , 0 , 1 , 0 , 1 ,0 , 1 /
      DATA IZERO  / 0 , 0 , 0 /
C
      IF(IEND.NE.0) WRITE(NOUT2,6000)
      NNAME(1) = '1-ST'
      NNAME(2) = '2-ND'
      CALL PHXRD(MREC,IGMAXC,PHXVX,VX)
      MPOSIP = 0
      LU235P = 0
      LU238P = 0
C
CM    WRITE(6,*) ' ** CHECK WRITE AT REACT3 : IEND & NCAE = ',IEND,NCASE
C
      DO 1000 I = 1,NCASE
      JERR      = 0
      IF (IEND.EQ.0) GO TO 90
C
      IF (MPOSI(I).EQ.MPOSIP .AND. LU235(I).EQ.LU235P .AND.
     *    LU238(I).EQ.LU238P) GO TO 90
C
      IF (I.GT.1) THEN
                  DO 70 J = 1,8
                  RR(J) = RR(J)/VD((J+3)/4)
   70             CONTINUE
                  WRITE(NOUT2,6010)
                  CALL INTEGP(IZERO,0,RR(1),DN58,NOUT2,JERR,IEND,
     *                        NNAME(1) ,VOLUME(I),RR,VD )
                  ENDIF
C
      CALL IVALUE(RR,8,0.0)
      CALL IVALUE(VD,2,0.0)
C
   90 CONTINUE
C         IF (IOPT2.NE.0)
          CALL REAC31(MTNAME,NISO  ,IDENT  ,IXMC  ,DN    ,
     2                DN58  ,MPOSI(I),LU235(I),LU238(I),NMAT,
     3                NTISO ,NOUT2   ,JERR    ,NNAME(1)      )
          II       = MPOSI(I)
CMOD      CALL SSIGMA(MREC,IGMAXC,MPOSI(I),LU235(I),LU238(I),
CMOD 1                NREC(II),SIGMA,PHMVM,VM ,JERR)
          CALL SSIGMA(IGMAXC,MPOSI(I),LU235(I),LU238(I),
     1                SIGMA,PHMVM,VM ,PHXVX,VX,JERR)
C
CM        WRITE(6,*) ' ** I MPOSI(I) JERR = ',I,II,JERR
C
          IF(JERR.NE.0) GO TO 110
          DO 100 J=1,8
              ID1 = ID(J)
              CALL FREACT(ITHERM(J),FGS(NGSTRT,I),SIGMA(1,ID1),
     1                    PHMVM(1) ,PHXVX(1)     ,VM,VX,FLUX(1,I),
     2                    R(J,I),JERR,IGMAXC,ISTEP                    )
              IF(JERR.NE.0) GO TO 110
  100         CONTINUE
  110         CONTINUE
C
          CALL INTEGP(MESH(1,I),MPOSI(I),R(1,I),DN58,NOUT2,JERR,IEND,
     *                NNAME(1) ,VOLUME(I),RR,VD                       )
      MPOSIP = MPOSI(I)
      LU235P = LU235(I)
      LU238P = LU238(I)
 1000 CONTINUE
      IF (IEND.EQ.0) GO TO 130
      DO 120 J = 1,8
      RR(J) = RR(J)/VD((J+3)/4)
  120 CONTINUE
      WRITE(NOUT2,6010)
      CALL INTEGP(IZERO,0,RR(1),DN58,NOUT2,JERR,IEND,
     *                NNAME(1) ,VOLUME(I),RR,VD                       )
  130 CONTINUE
      RETURN
 6000 FORMAT(1H1,10X,
     1          '----- INTEGRATED PARAMETER OF FISSILE NUCLIDE -----')
 6010 FORMAT('0  == AVERAGE REACTION RATE =='                         )

      END
