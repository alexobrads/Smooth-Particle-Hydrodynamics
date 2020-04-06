module calc_accel
  implicit none


contains


   function d_kernal(q)

     real :: d_kernal, q

     if (q.GE.0 .and. q.LT.1) then
       d_kernal = -1*(3./4)*(2-q)**2 + 3*(1-q)**2

     else if (q.GE.1 .and. q.LT.2) then
       d_kernal = -(3./4.)*(2.-q)**2
     else
       d_kernal = 0.
     end if

     d_kernal = d_kernal*(2./3)

   end function d_kernal


   function viscosity(rho, v_ab, u_vec, v_sig)
     real :: viscosity, v_ab, u_vec, crit, rho, v_sig

     crit = v_ab*u_vec

     if (crit.LT.0) then
       viscosity = -0.5*rho*v_sig*v_ab*u_vec

     else
       viscosity = 0.

    end if

  end function viscosity



  subroutine get_accel(blob, n, n_ghosts, dt_new)
    real, intent(inout) :: blob(:,:)
    integer, intent(in) :: n, n_ghosts
    real, intent(out) :: dt_new
    real, parameter :: alpha = 1., beta = 2.

    integer :: a, b
    real :: acc
    real :: ra, rb, rho_a, rho_b, pr_a, pr_b, h_a, h_b, m_b
    real :: term_a, term_b, d_wa, d_wb, u_vec, q_a, q_b
    real :: va, vb, v_sig_a, v_sig_b, cs_a, cs_b, v_ab
    real :: vis_a, vis_b, u_a, du_dt, term_3

    do a=1,n
      cs_a = blob(a, 8)
      va = blob(a, 2)
      ra = blob(a, 1)
      rho_a = blob(a, 5)
      pr_a = blob(a, 7)
      h_a = blob(a, 4)
      u_a = blob(a, 6)

      blob(a, 9) = 0.
      blob(a, 10) = 0.


      do b=1,n+n_ghosts
        cs_b = blob(b, 8)
        vb = blob(b, 2)
        rb = blob(b, 1)
        rho_b = blob(b, 5)
        pr_b = blob(b, 7)
        h_b = blob(b, 4)
        m_b = blob(b, 3)

        if (b.NE.a) then

          acc = 0.

          u_vec = (ra - rb)/abs(ra - rb)
          v_ab = va - vb

          q_a = abs(ra-rb)/h_a
          q_b = abs(ra-rb)/h_b

          v_sig_a = alpha*cs_a - beta*v_ab*u_vec
          v_sig_b = alpha*cs_b - beta*v_ab*u_vec

          vis_a = viscosity(rho_a, v_ab, u_vec, v_sig_a)
          vis_b = viscosity(rho_b, v_ab, u_vec, v_sig_b)

          term_a = (pr_a + vis_a)/(rho_a**2)
          term_b = (pr_b + vis_b)/(rho_b**2)

          d_wa = (1./(h_a**2))*d_kernal(q_a)*u_vec
          d_wb = (1./(h_b**2))*d_kernal(q_b)*u_vec

          acc = -1*m_b*(term_a*d_wa + term_b*d_wb)


          term_3 = (pr_a + vis_a)/rho_a**2

          du_dt = m_b*term_3*v_ab*d_wa

        else
          acc = 0
          du_dt = 0

        end if

        blob(a, 9) = blob(a, 9) + acc
        blob(a, 10) = blob(a, 10) + du_dt

      enddo

    enddo

  dt_new = minval(0.2*blob(:n, 4)/blob(:n, 8))

  end subroutine get_accel

end module calc_accel
