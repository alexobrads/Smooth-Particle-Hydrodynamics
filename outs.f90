module output
  implicit none
  !integer, private :: nfile = 1

contains
  subroutine write_output(dx, x, u, nx, istep, t)
    real,    intent(in) :: dx, x, t
    integer, intent(in) :: nx, istep
    real,    intent(in) :: u(0:nx+1)
    integer :: iunit, i
    character(len=100) :: filename

    ! write to a sequence of files called snap_00000, snap_00001 etc
    write(filename,"(a,i5.5)") 'snap_',istep
    !nfile = nfile + 1

    print "(a,f8.3)", ' writing '//trim(filename)// ' t =',t
    open(newunit=iunit,file=filename,status='replace')
    do i=1,nx
      write(iunit,*) i*dx, u(i)
    enddo

    close(iunit)

  end subroutine write_output
end module output
