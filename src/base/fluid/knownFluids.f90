!| 既知の流体の物性値の定数を提供する．
!
! 定数として宣言される流体には，水が含まれる．
!
module fluid_knownFluids
    use :: fluid_property
    implicit none
    private

    type(fluid_property_type), public, parameter :: &
        Water = fluid_property_type(density=1d+3, &
                                    viscosity=1d+3*1d-6, &
                                    kinetic_viscosity=1d-6)
        !! 水の物性値
        !! 既定の温度の値ではなく，近似値

end module fluid_knownFluids
