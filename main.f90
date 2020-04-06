
program SPH_method
  use memory
  use utils
  use calc_density
  use ghosts
  use equation_of_state
  use calc_accel
  use calc_derivs
  use stepping
  implicit none

  !Matrix of the form x, v, m, sl, p, u, P, ss, accel, du_dt
  integer :: n, ng, i, j
  real :: blob(n_max, 10)
  real :: time, dt, dt_new

  !call setup_wave(blob, n)
  call setup_sod_shock_tube(blob, n)
  call get_derivs(blob, n, ng, dt)
  call output(blob, out_n, tmin)

  i = 0
  time = tmin
  do while (time < tmax)
    i = i + 1

    call do_step(blob, n, ng, dt, dt_new)
    dt = dt_new
    time = time + dt
    print*, time
    t(i) = time
    ke(i) = sum(0.5*blob(:n, 2) + blob(:n, 2)**2)


    if (mod(i, 10)==0) then
      call output(blob, out_n, time)
      t(i) = time
      ke(i) = sum(0.5*blob(:n, 2) + blob(:n, 2)**2)
    endif
  enddo

  open(1, file='KE.dat', status='replace')
  write(1,*) '# t, ke'
  do j=1,i
    write(1,*) t(j), ke(j)
  enddo
  close(1)

end program SPH_method
