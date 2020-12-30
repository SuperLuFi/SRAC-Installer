C***********************************************************************
C  PROGRAM TO EDIT NUCLIDE-WISE DELAYED NEUTRON DATA FROM FASTP        *
C  MEMBER NAME YZZM0000 IF IFISS=1                                     *
C  VERSION : SRAC95 FOR UNIX                                           *
C***********************************************************************
C
      SUBROUTINE DLYEDT(DIRNAM, IOUT, IPRN, MEMNAM, NGFP,
     &                  EE, XNUT, XLMD, RBETA, FNUD, TNUD, XKAID )
C
C============================== FOR MAIN ===============================
CDEL  PARAMETER ( MAXNG=74 )
      INCLUDE  'PARAMINC'
      DIMENSION  XLMD(6), RBETA(6) ,FNUD(MAXNG),
     &           XKAID(6,MAXNG)
      CHARACTER*72  DIRNAM
      CHARACTER* 8  MEMNAM
C=======================================================================
CDEL  PARAMETER  (MAXWRK=1000)
      COMMON /WKPDS/ WORK(MAXWRK)  
      DIMENSION      IWORK(1)
      EQUIVALENCE ( WORK(1),IWORK(1) )
      CHARACTER* 8  MEMBER
C------------------------------ INPUT ----------------------------------
C     DIRNAM     : DIRECTORY NAME (A72) OF PDS : /XXX/XXX/PFAST
C     IOUT       : LOGICAL DEVICE FOR OUTPUT
C     IPRN       : =0(NO PRINT), =1(PRINT OUT IN DEVICE IOUT)
C     MEMNAM     : PDS MEMBER NAME TO EDIT(A8)
C     NGFP       : NUMBER OF FAST ENERGY GROUP IN PUBLIC FAST LIBRARY
C                  NOT EFFECTIVE BECAUSE SET INTERNALY NGFP=74
C------------------------------ OUTPUT ---------------------------------
C     EE         : NEUTRON ENERGY FOR THE FOLLOWING DELAYED NEUTRON DATA
C                  ALWAYS EE=0.0253EV
C     XNUT       : TOTAL NYU-VALUE FOR THERMAL FISSION
C     XLMD(J)    : DECAY CONSTANTS FOR J-TH FAMILY
C     RBETA(J)   : NORMALIZED DELAYED NEUTRON FRACTION FOR J-TH FAMILY
C                  SUM(RBETA(J))=1.0
C     FNUD(G)    : NYU-VALUE OF DELAYED NEUTRON FOR G-TH FAST GROUP
C     TNUD       : NYU-VALUE OF DELAYED NEUTRON FOR THERMAL GROUP(CONST)
C     XKAID(J,G) : DELAYED NEUTRON SPECTRUM FOR J-TH FAMILY , G-TH GROUP
C-----------------------------------------------------------------------
      NGFP=74
C-------------------
      IF(MEMNAM(1:1).NE.'Y') THEN
        WRITE(IOUT,*) ' ERROR(DLYEDT): MEMBER NAME IS NOT CONSISTENT '
        if(iout.ne.6) then 
        WRITE(6,*) ' ERROR(DLYEDT): MEMBER NAME IS NOT CONSISTENT '
        endif
        STOP
      ENDIF
***************************
*     ZERO SET            *---------------------------------------------
***************************
      EE = 0.0
      XNUT = 0.0
      TNUD = 0.0
      DO 100 I=1,MAXWRK
        WORK(I)=0.0
  100 CONTINUE
      DO 110 J=1,6
        XLMD(J) = 0
        RBETA(J) = 0
  110 CONTINUE
      DO 120 I=1,MAXNG
        FNUD(I) = 0
        DO 130 J = 1,6
          XKAID(J,I) = 0.0
  130   CONTINUE
  120 CONTINUE
*****************************
* EDIT DELAYED NEUTRON DATA *-------------------------------------------
*****************************
      MEMBER = MEMNAM
      CALL PDSIN (DIRNAM,MEMBER,WORK,LENG,IRC,IOUT)
      IF(IRC.NE.0) THEN
        WRITE(IOUT,*) ' ERROR(DLYEDT): PDSIN ERROR, CODE=', IRC
        if(iout.ne.6) then 
        WRITE(6,*)    ' ERROR(DLYEDT): PDSIN ERROR, CODE=', IRC
        endif
        STOP
      ENDIF
      EE  = WORK(1)
      XNUT = WORK(2)
      IPOS = 2
      DO 200 J=1,6
        XLMD(J)  = WORK(J+IPOS)
        RBETA(J) = WORK(J+IPOS+6)
  200 CONTINUE
      IPOS = 14
      DO 210 I=1,NGFP
        FNUD(I) = WORK(I+IPOS)
  210 CONTINUE
      IPOS = IPOS + NGFP
      TNUD = WORK(1+IPOS)
      IPOS = IPOS + 1
      DO 220 J=1,6
        DO 220 I=1,NGFP
          II = IPOS + ((J-1)*NGFP+I)
          XKAID(J,I) = WORK(II)
  220 CONTINUE
      IPOS = IPOS+6*NGFP
***************************
*      PRINT OUT          *---------------------------------------------
***************************
      IF(IPRN.EQ.0) GOTO 9000
      WRITE(IOUT,*)
      WRITE(IOUT,7000)
      WRITE(IOUT,*)
      WRITE(IOUT,*) '   ********** MEMBER NAME = ',MEMNAM, ' **********'
      WRITE(IOUT,*)
      WRITE(IOUT,*) '   EE(EV)= ', EE
      WRITE(IOUT,*) '   XNUT  = ', XNUT
      WRITE(IOUT,*) '   TNUD  = ', TNUD
      WRITE(IOUT,*)
      WRITE(IOUT,*)
      WRITE(IOUT,6000)
      WRITE(IOUT,*)
      WRITE(IOUT,6010) (XLMD(J),J=1,6)
      WRITE(IOUT,*)
      WRITE(IOUT,6020)
      WRITE(IOUT,*)
      WRITE(IOUT,6010) (RBETA(J),J=1,6)
      WRITE(IOUT,*)
      WRITE(IOUT,*)
      WRITE(IOUT,6030)
      WRITE(IOUT,*)
      WRITE(IOUT,6040) (FNUD(I),I=1,NGFP)
      WRITE(IOUT,*)
      WRITE(IOUT,*)
      WRITE(IOUT,6050)
      WRITE(IOUT,*)
      DO 1000 J=1,6
        WRITE(IOUT,6060) J,(XKAID(J,I),I=1,NGFP)
 1000 CONTINUE
 6000 FORMAT(1H ,'   DECAY CONSTANTS (J=1,6)   ')
 6010 FORMAT(1H ,3X,6(1PE12.5,2X))
 6020 FORMAT(1H ,'   NORMALIZED BETA (J=1,6)   ')
 6030 FORMAT(1H ,'   NYU-VALUE OF DELAYED NEUTRON FOR THE G-TH FAST',
     &' GROUP')
 6040 FORMAT(1H ,6X,1P10E12.5,10(/,7X,1P10E12.5))
 6050 FORMAT(1H ,'   DELAYED NEUTRON SPECTRUM FOR THE J-TH FAMILY ',
     &'AND G-TH GROUP')
 6060 FORMAT(1H ,I4,2X,1P10E12.5,10(/,7X,1P10E12.5))
      WRITE(IOUT,*)
      WRITE(IOUT,7010)
      WRITE(IOUT,*)
 7000 FORMAT(1H ,'DLYEDT',114(1H=))
 7010 FORMAT(1H ,114(1H=),'DLYEDT')
C
 9000 RETURN
      END
