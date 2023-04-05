Program filewrite

  Use ISO_FORTRAN_ENV
  Use omp_lib
  Implicit none
  
  Integer, Parameter                            :: ndim=150
  Integer (kind=int16)                          :: i, j, k
  Real (kind=real64), dimension(ndim,ndim,ndim) :: x, y, z, p
  Real (kind=real64)                            :: timestart, timeend 

! Remove old files
 call execute_command_line ("rm data.csv data.dat", wait=.true.)
! Initialize "p" field to random numbers between 0 and 1
 call random_number(p)
! Initialize x,y,and z files, add part of p at each point to get non-integer numbers  
!$omp parallel do shared(x,y,z,p) collapse(2)  
  do i = 1,ndim
   do j = 1, ndim
    do k = 1, ndim
      x(i,j,k) = (i - 1) + p(i,j,k)/10.0
      y(i,j,k) = (j - 1) - p(i,j,k)/20.0
      z(i,j,k) = (k - 1) + p(i,j,k)/50.0
    enddo
   enddo
  enddo
!$omp end parallel do  


! Write data to a simple comma-separated ASCII file
 timestart = omp_get_wtime()
 Open (8,file="data.csv",form="formatted",access="sequential",recl=132)
 Write (8,*) "x,y,z,p"
 
   do i = 1,ndim
   do j = 1, ndim
    do k = 1, ndim
     Write (8,*) x(i,j,k),",",y(i,j,k),",",z(i,j,k),",",p(i,j,k)
    enddo
   enddo
  enddo
 Close (8)
 timeend = omp_get_wtime()
 Write(*,"(A22,I8,A13,F6.1,A8)") "Writing csv file for ", ndim*ndim*ndim," points took ", timeend - timestart," seconds"
 

! Write data to an unformatted file
 timestart = omp_get_wtime()
 Open (9,file="data.dat",form="unformatted",access="stream")
 Write (9) "x,y,z,p"
 
   do i = 1,ndim
   do j = 1, ndim
    do k = 1, ndim
     Write (9) x(i,j,k),y(i,j,k),z(i,j,k),p(i,j,k)
    enddo
   enddo
  enddo
 Close (8)
 timeend = omp_get_wtime()
 Write(*,"(A30,I8,A13,F6.1,A8)") "Writing unformatted file for ", ndim*ndim*ndim," points took ", timeend - timestart," seconds"

end program filewrite
