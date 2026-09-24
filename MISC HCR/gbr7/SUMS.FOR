      REAL*8 SUMD(4,10)
      

      OPEN(UNIT=14,FILE='SUMM.1')
      OPEN(UNIT=15,FILE='SUMM.2')
      CALL READIN('deriso1a.rep','deriso1a.std',SUMD,1,24)
      CALL READIN('deriso1b.rep','deriso1b.std',SUMD,2,24)
      CALL READIN('deriso1c.rep','deriso1c.std',SUMD,3,24)
      CALL READIN('deriso1d.rep','deriso1d.std',SUMD,4,24)
      CALL OUTS(SUMD)
      CALL READIN('deriso2a.rep','deriso2a.std',SUMD,1,24)
      CALL READIN('deriso2b.rep','deriso2b.std',SUMD,2,24)
      CALL READIN('deriso2c.rep','deriso2c.std',SUMD,3,24)
      CALL READIN('deriso2d.rep','deriso2d.std',SUMD,4,24)
      CALL OUTS(SUMD)
      CALL READIN('deriso3a.rep','deriso3a.std',SUMD,1,24)
      CALL READIN('deriso3b.rep','deriso3b.std',SUMD,2,24)
      CALL READIN('deriso3c.rep','deriso3c.std',SUMD,3,24)
      CALL READIN('deriso3d.rep','deriso3d.std',SUMD,4,24)
      CALL OUTS(SUMD)
      CALL READIN('deriso4a.rep','deriso4a.std',SUMD,1,28)
      CALL READIN('deriso4b.rep','deriso4b.std',SUMD,2,28)
      CALL READIN('deriso4c.rep','deriso4c.std',SUMD,3,28)
      CALL READIN('deriso4d.rep','deriso4d.std',SUMD,4,28)
      CALL OUTS(SUMD)
      CALL READIN('deriso5a.rep','deriso5a.std',SUMD,1,22)
      CALL READIN('deriso5b.rep','deriso5b.std',SUMD,2,22)
      CALL READIN('deriso5c.rep','deriso5c.std',SUMD,3,22)
      CALL READIN('deriso5d.rep','deriso5d.std',SUMD,4,22)
      CALL OUTS(SUMD)
      CALL READIN('deriso6a.rep','deriso6a.std',SUMD,1,28)
      CALL READIN('deriso6b.rep','deriso6b.std',SUMD,2,28)
      CALL READIN('deriso6c.rep','deriso6c.std',SUMD,3,28)
      CALL READIN('deriso6d.rep','deriso6d.std',SUMD,4,28)
      CALL OUTS(SUMD)
      CALL READIN('deriso7a.rep','deriso7a.std',SUMD,1,28)
      CALL READIN('deriso7b.rep','deriso7b.std',SUMD,2,28)
      CALL READIN('deriso7c.rep','deriso7c.std',SUMD,3,28)
      CALL READIN('deriso7d.rep','deriso7d.std',SUMD,4,28)
      CALL OUTS(SUMD)
      CALL READIN('deriso8a.rep','deriso8a.std',SUMD,1,28)
      CALL READIN('deriso8b.rep','deriso8b.std',SUMD,2,28)
C
C     NOTE THE CHANGE 
      CALL READIN('deriso8c.rep','deriso8a.std',SUMD,3,28)
      CALL READIN('deriso8d.rep','deriso8d.std',SUMD,4,28)
      CALL OUTS(SUMD)
      CLOSE(14)
      CLOSE(15)
                   
      STOP
      END
C
C ==========================================================================
C      
      SUBROUTINE READIN(File1,File2,SUMD,II,Skip)
C
      IMPLICIT NONE
C
      REAL*8 SUMD(4,10),D1,Est(10),CV(10)
      INTEGER II,SKIP,IJ
      CHARACTER*12 File1,File2
      
      OPEN(UNIT=13,FILE=File1)
      READ(13,*) D1,SUMD(2,II),D1,SUMD(3,II)
      READ(13,*) SUMD(4,II)
      READ(13,*)
      READ(13,*)
      READ(13,*) D1,D1,D1,D1,SUMD(1,II)
      CLOSE(13)
      
      OPEN(UNIT=13,FILE=File2)
      DO 1000 IJ = 1,Skip
       READ(13,*)
1000  CONTINUE
      DO 2000 IJ = 1,7
       READ(13,'(19X,F10.0,F11.0)') Est(IJ),CV(IJ)
       CV(IJ) = CV(IJ)/Est(IJ)
2000  CONTINUE 
      WRITE(15,601) NINT(EST(2)),NINT(EST(3)),EST(4),EST(1),
     +              NINT(EST(5)),NINT(EST(6)),EST(7)
      WRITE(15,602) (CV(IJ),IJ=1,7)
      CLOSE(13)
C
      RETURN
601   FORMAT(1x,2(2(I5,','),F5.1,',',F5.3,','))      
602   FORMAT(1x,7("[",F4.2,"],"))
      END
C
C ========================================================================
C      
      SUBROUTINE OUTS(SUMD)
C
      IMPLICIT NONE
      
      REAL*8 SUMD(4,10)
      INTEGER II,IJ
      
      WRITE(14,600) ((SUMD(II,IJ),II=1,4),IJ=1,4)

      RETURN
600   FORMAT(1x,4(F3.2,3(',',F5.1),','))
      END


