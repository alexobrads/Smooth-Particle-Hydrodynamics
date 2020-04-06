module equation_of_state
  use memory
  implicit none

contains

  subroutine eos(blob)
    real, intent(inout) :: blob(:,:)

    if (abs(gamma - 1) < tiny(1.)) then
      call iso_eos(blob)

    else
      call adiabatic_eos(blob)

    endif

  end subroutine eos

  subroutine iso_eos(blob)
    real, intent(inout) :: blob(:,:)
    !pressure = (density)*(sound speed)**2
    print*, 'isothermal'
    blob(:, 7) = (blob(:, 5))*(blob(:, 8)**2)

  end subroutine iso_eos

  subroutine adiabatic_eos(blob)
    real, intent(inout) :: blob(:,:)

    print*, 'adiabatic'

    blob(:, 7) = (gamma-1)*blob(:, 5)*blob(:, 6)
    blob(:, 8) = sqrt(gamma*(blob(:, 7)/blob(:, 5)))

  end subroutine adiabatic_eos

end module equation_of_state
