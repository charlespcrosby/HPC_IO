program writeFile_nc
    Use ISO_FORTRAN_ENV
    use netcdf
    Use omp_lib    
    implicit none
    Integer, Parameter                   :: ndim=200    
    integer                              :: status, ncid
    integer                              :: iidim, jjdim, kkdim, ii, jj, kk
    integer                              :: dimid_i, dimid_j, dimid_k, dimid_p
    integer                              :: varid_x, varid_y, varid_z, varid_p
    Real (kind=real64)                            :: timestart, timeend 
    real (kind=real64), dimension(ndim,ndim,ndim) :: x, y, z, p
    
! Set dimensions of data set
    iidim = ndim
    jjdim = ndim
    kkdim = ndim

! Create a NetCDF datafile and get its handle, the integer variable ncis
    status = nf90_create('Pxyz_data.nc', NF90_NETCDF4, ncid)
! Check that the file has been opened correctly    
    call check(status, 'open')

! Define the dimensions of the data-set associated with the datafile referred to with ncid, 
! and get their handles, dimid_i, dimid_j, dimid_k    
    status = nf90_def_dim(ncid, 'i-index', iidim, dimid_i)
    status = nf90_def_dim(ncid, 'j-index', jjdim, dimid_j)
    status = nf90_def_dim(ncid, 'k-index', kkdim, dimid_k)  
! Define each NetCDF variable, specify their dimesion handles and get the handle for each variable, varid_*
!  Also set chunking and compression parameters      
    status = nf90_def_var(ncid, 'X-position', NF90_DOUBLE, [dimid_i, dimid_j, dimid_k], varid_x)
    status = nf90_def_var_chunking(ncid, varid_x, NF90_CHUNKED, [10, 10, 10])
    status = nf90_def_var_deflate(ncid, varid_x, shuffle = 1, deflate = 1, deflate_level = 5  )    
    status = nf90_def_var(ncid, 'Y-position', NF90_DOUBLE, [dimid_i, dimid_j, dimid_k], varid_y)
    status = nf90_def_var_chunking(ncid, varid_y, NF90_CHUNKED, [10, 10, 10])
    status = nf90_def_var_deflate(ncid, varid_y, shuffle = 1, deflate = 1, deflate_level = 5  )    
    status = nf90_def_var(ncid, 'Z-position', NF90_DOUBLE, [dimid_i, dimid_j, dimid_k], varid_z)
    status = nf90_def_var_chunking(ncid, varid_z, NF90_CHUNKED, [10, 10, 10])
    status = nf90_def_var_deflate(ncid, varid_z, shuffle = 1, deflate = 1, deflate_level = 5  )    
    status = nf90_def_var(ncid, 'Pressure',   NF90_DOUBLE, [dimid_i, dimid_j, dimid_k], varid_p)
    status = nf90_def_var_chunking(ncid, varid_p, NF90_CHUNKED, [10, 10, 10])
    status = nf90_def_var_deflate(ncid, varid_p, shuffle = 1, deflate = 1, deflate_level = 5  )
! Add some notes and and units to the file   
    status = nf90_put_att(ncid, NF90_GLOBAL, 'note', 'Testing file, consisting of x,y,z and p, in F90')
    status = nf90_put_att(ncid, varid_x, 'units', 'm')
    status = nf90_put_att(ncid, varid_y, 'units', 'm')
    status = nf90_put_att(ncid, varid_z, 'units', 'm')
    status = nf90_put_att(ncid, varid_p, 'units', 'Pa')        


! Initialize "p" field to random numbers between 0 and 1
 call random_number(p)
! Initialize x,y,and z files, add part of p at each point to get non-integer numbers  
!$omp parallel do shared(x,y,z,p) collapse(2)  
  do ii = 1,iidim
   do jj = 1, jjdim
    do kk = 1, kkdim
      x(ii,jj,kk) = (ii - 1) + p(ii,jj,kk)/10.0
      y(ii,jj,kk) = (jj - 1) - p(ii,jj,kk)/20.0
      z(ii,jj,kk) = (kk - 1) + p(ii,jj,kk)/50.0
    enddo
   enddo
  enddo
!$omp end parallel do 
    
! Store the variables in the file and time the operation
    timestart = omp_get_wtime()
    status = nf90_put_var(ncid, varid_x, x)
    status = nf90_put_var(ncid, varid_y, y)
    status = nf90_put_var(ncid, varid_z, z)   
    status = nf90_put_var(ncid, varid_p, p)                              
! Close the file and check that it has been closed before proceeding    
    status = nf90_close(ncid)
    call check(status, 'close')
    timeend = omp_get_wtime()
! Report the time taken    
    Write(*,"(A30,I8,A13,F6.1,A8)") "Writing NetCDF file for ", iidim*jjdim*kkdim," points took ", timeend - timestart," seconds"    
    
  
contains    

subroutine check(status, operation)
    use netcdf
    implicit none
    integer, intent(in) :: status
    character(len=*), intent(in)     :: operation
    if (status == NF90_NOERR) return
    print *, "Error encountered during ", operation
    print *, nf90_strerror(status)
    STOP 1
end subroutine check

end program writeFile_nc
