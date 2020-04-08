module stepping

  use memory
  use calc_derivs
  use ghosts
  use calc_density
  use equation_of_state
  use calc_accel

  implicit none

  real :: x_0(n_max), x_1(n_max)
  real :: v_0(n_max), v_1(n_max), v_star(n_max)
  real :: a_0(n_max), a_1(n_max)
  real :: u_0(n_max), u_star(n_max), u_1(n_max)
  real :: du_dt_0(n_max), du_dt_1(n_max)

contains
  subroutine do_step(blob, n, ng, dt, dt_new)
    real, intent(inout) :: blob(:,:)
    real, intent(in) :: dt
    real, intent(out) :: dt_new
    integer, intent(inout) :: n, ng

    x_0(:n) = blob(:n, 1)
    v_0(:n) = blob(:n, 2)
    a_0(:n) = blob(:n, 9)
    u_0(:n) = blob(:n, 6)
    du_dt_0(:n) = blob(:n, 10)

    x_1(:n) = x_0(:n) + dt*v_0(:n) + 0.5*(dt**2)*a_0(:n)
    v_star(:n) = v_0(:n) + dt*a_0(:n)
    u_star(:n) = u_0(:n) + dt*du_dt_0(:n)

    blob(:n, 1) = x_1(:n)
    blob(:n, 2) = v_star(:n)
    blob(:n, 6) = u_star(:n)

    call get_derivs(blob, n, ng, dt_new)
    du_dt_1 = blob(:n, 10)

    a_1(:n) = blob(:n, 9)
    v_1(:n) = blob(:n, 2) + 0.5*dt*(a_1(:n) - a_0(:n))
    u_1(:n) = blob(:n, 6) + 0.5*dt*(du_dt_1(:n) - du_dt_0(:n))

    blob(:n, 2) = v_1(:n)
    blob(:n, 9) = a_1(:n)
    blob(:n, 6) = u_1(:n)



  end subroutine do_step

end module stepping
