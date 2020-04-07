module calc_density
  implicit none

contains


  function kernal(q)
    real :: kernal, q

    if  (q.GE.0 .and. q.LT.1) then
      kernal = (0.25*(2-q)**3 - (1-q)**3)
    else if (q.GE.1 .and. q.LT.2) then
      kernal = (0.25*(2-q)**3)
    else
      kernal = 0
    end if

    kernal = kernal*(2./3)

  end function kernal


  subroutine get_density(blob, n, ng)

    real, intent(inout) :: blob(:,:)
    integer, intent(inout) :: n, ng

    integer :: a, b
    real :: q, rho, h_a, m_b, r_a, r_b

    do a=1, n

      blob(a, 5) = 0
      h_a = blob(a, 4)
      r_a = blob(a,1)

      do b=1, n+ng

        m_b = blob(b, 3)
        r_b = blob(b,1)

        rho = 0

        q = abs(r_a - r_b)/h_a

        rho = m_b*(1./h_a)*kernal(q)

        blob(a, 5) = blob(a, 5) + rho

        ! if (a>549 .and. a<551) then
        !
        !   print*, a, b ,(r_b - r_a) , r_a, r_b, rho, blob(a, 5)
        ! end if


      enddo
    enddo
  end subroutine get_density
end module calc_density
