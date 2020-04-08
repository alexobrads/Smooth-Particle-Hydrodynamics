module utils
  use memory
  implicit none
  real, private, parameter :: pi = 4.*atan(1.)

  integer, private :: icount = 0

contains

  subroutine setup(blob, n)
    integer, intent(inout) :: n
    real, intent(inout) :: blob(:,:)

    if (problem < 1) then

      call setup_wave(blob, n)

    else
      call setup_tube(blob, n)

    end if

  end subroutine setup


  subroutine setup_wave(blob, n)
    integer, intent(inout) :: n
    real, intent(inout) :: blob(:,:)
    integer :: i
    real :: x, dx


    dx = 0.01

    n = nint((x_max - x_min)/dx)
    do i=1,n
      x = x_min + (i-0.5)*dx
      blob(i, 1) = x
      blob(i, 2) = (1*1e-4)*sin(x*2.*pi)
      blob(i, 3) = dx
      blob(i, 4) = 1.2*dx
      blob(i, 5) = 1.
      blob(i, 6) = 0.
      blob(i, 7) = 1
      blob(i, 8) = 1.
      blob(i, 9) = 0.
      blob(i, 10) = 0.
    enddo
  end subroutine setup_wave




  subroutine setup_tube(blob, n)
    integer, intent(out) :: n
    real, intent(inout) :: blob(:,:)
    real :: x, rho_l, rho_r, dx_l, dx_r
    real :: mass, pr, pl, ss
    real :: u_l, u_r, start, shock, end, new_start
    integer :: global_count, count_left, new_count_left
    integer :: count_right, new_count_right

    if (gamma>1.) then
      rho_l = 1.
      rho_r = 0.125
      pl = 1.
      pr = 0.1
      ss = 1.
      dx_l = 0.001
      dx_r = 0.008
      mass = rho_l*dx_l
      u_r = pr/((gamma - 1)*rho_r)
      u_l = pl/((gamma - 1)*rho_l)

    else
      rho_l = 1.
      rho_r = 0.1
      pr = 0.1
      pl = 1.
      ss = 1.
      dx_l = 0.001
      dx_r = 0.01
      mass = rho_l*dx_l
      u_r = 1.
      u_l = 1.


    endif



    global_count = 1


    !Set up particles to the right
    start = 0
    shock = 0.5
    end = 1.

    count_left = 1
    new_count_left = 1

    x = start
    do while (x<end)

      if (x<shock) then
        x = (count_left-0.5)*dx_l
        if (x>shock) then

        else
          blob(global_count, 1) = x
          blob(global_count, 2) = 0.
          blob(global_count, 3) = mass
          blob(global_count, 4) = 1.2*dx_l
          blob(global_count, 5) = rho_l
          blob(global_count, 6) = u_l
          blob(global_count, 7) = pl
          blob(global_count, 8) = 1.
          blob(global_count, 9) = 0.
          blob(global_count, 10) = 0.

          count_left = count_left + 1
          global_count = global_count + 1

        endif

      else

        new_start = blob(count_left - 1, 1)
        x = new_start + (new_count_left)*dx_r
        if (x>end) then

        else
          blob(global_count, 1) = x
          blob(global_count, 2) = 0.
          blob(global_count, 3) = mass
          blob(global_count, 4) = 1.2*dx_r
          blob(global_count, 5) = rho_r
          blob(global_count, 6) = u_r
          blob(global_count, 7) = pr
          blob(global_count, 8) = 1.
          blob(global_count, 9) = 0.
          blob(global_count, 10) = 0.

          new_count_left = new_count_left + 1
          global_count = global_count + 1
        endif

      endif

    enddo

    !Set up particles to the right
    start = 0
    shock = -0.5
    end = -1.

    count_right = 1
    new_count_right = 1

    x = start
    do while (x>end)

      if (x>shock) then
        x = -1*(count_right-0.5)*dx_l
        if (x<shock) then

        else
          blob(global_count, 1) = x
          blob(global_count, 2) = 0.
          blob(global_count, 3) = mass
          blob(global_count, 4) = 1.2*dx_l
          blob(global_count, 5) = rho_l
          blob(global_count, 6) = u_l
          blob(global_count, 7) = pl
          blob(global_count, 8) = 1.
          blob(global_count, 9) = 0.
          blob(global_count, 10) = 0.

          count_right = count_right + 1
          global_count = global_count + 1
        endif

      else
        new_start = blob((count_left+new_count_left+count_right)-3, 1)
        x = new_start - (new_count_right)*dx_r

        if (x<-1) then

        else
          blob(global_count, 1) = x
          blob(global_count, 2) = 0.
          blob(global_count, 3) = mass
          blob(global_count, 4) = 1.2*dx_r
          blob(global_count, 5) = rho_r
          blob(global_count, 6) = u_r
          blob(global_count, 7) = pr
          blob(global_count, 8) = 1.
          blob(global_count, 9) = 0.
          blob(global_count, 10) = 0.

          new_count_right = new_count_right + 1
          global_count = global_count + 1
        endif

      endif

    enddo

    n = global_count - 1

    blob(:n, 1) = blob(:n, 1)

  end subroutine setup_tube



  subroutine output(blob, n, dt)

    implicit none
    !integer, intent(in) :: n_ghosts
    integer, intent(in) :: n
    real, intent(in) :: blob(:,:), dt
    character(len=100) :: filename
    integer :: j

    write(filename, "(a,i5.5)") 'snap',icount

    open(1, file=filename, status='replace')
    write(1,*) '# x, v_x, m, sl, rho, u, P, ss, a, du_dt'
    write(1,*) dt
    do j=1,n
      write(1,*) blob(j,1), blob(j,2), blob(j,3), blob(j,4), &
                  blob(j,5), blob(j,6), blob(j,7), blob(j,8), &
                  blob(j, 9), blob(j, 10)
    enddo

    close(1)

    icount = icount+1

  end subroutine output



end module utils
