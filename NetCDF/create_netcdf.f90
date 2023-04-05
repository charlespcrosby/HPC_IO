program create_netcdf
    Use ISO_FORTRAN_ENV
    use netcdf
    implicit none
    integer                              :: status, ncid
    integer                              :: ii, jj
    integer                              :: dimid_lon, dimid_lat
    integer                              :: varid_lon, varid_lat, varid_field
    integer, parameter                   :: nx=200
    integer, parameter                   :: ny=101
    real (kind=real64), parameter        :: pi=3.141592654
    real (kind=real64), dimension(nx,ny) :: field
    real (kind=real32), dimension(nx)    :: lon_array
    real (kind=real32), dimension(ny)    :: lat_array

    status = nf90_create('data.nc', NF90_NETCDF4, ncid)
    call check(status, 'open')
    
    status = nf90_def_dim(ncid, 'longitude', nx, dimid_lon)
    status = nf90_def_dim(ncid, 'latitude', ny, dimid_lat)
    status = nf90_def_var(ncid, 'longitude', NF90_FLOAT, [dimid_lon], varid_lon)
    status = nf90_def_var(ncid, 'latitude', NF90_FLOAT, [dimid_lat], varid_lat)
    status = nf90_def_var(ncid, 'field', NF90_DOUBLE, [dimid_lon, dimid_lat], varid_field)    
    status = nf90_def_var_chunking(ncid, varid_field, NF90_CHUNKED, [10, 101])
    status = nf90_def_var_deflate(ncid, varid_field,          &
                                  shuffle = 1,                &
                                  deflate = 1,                &
                                  deflate_level = 5  )
    status = nf90_put_att(ncid, NF90_GLOBAL, 'note', 'training file created with Fortran 90')
    status = nf90_put_att(ncid, varid_lon, 'units', 'degree_east')
    status = nf90_put_att(ncid, varid_lat, 'units', 'degree_north')
    status = nf90_put_att(ncid, varid_field, '_FillValue', -2e8)   
    
    lat_array = [(jj * (360./ny), jj=0, ny-1)]
    lon_array = [((ii * (180./(nx-1)) - 90.), ii=0, nx-1)]
    do jj = 1, ny
        do ii = 1, nx
            field(ii, jj) = sin(lon_array(ii) * pi/180.) * &
                cos(lat_array(jj) * pi/180.)
        end do
    end do         
    
    status = nf90_put_var(ncid, varid_lon, lon_array)
    status = nf90_put_var(ncid, varid_lat, lat_array)
    status = nf90_put_var(ncid, varid_field, field)                          
    
    status = nf90_close(ncid)
    call check(status, 'close')
  
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

end program create_netcdf
