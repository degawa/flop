!>流体の物性値を取り扱う派生型を提供する．
!>
!>定義される派生型には，等温非圧縮性流体の物性値を扱う派生型が含まれる．
!>
module fluid_property
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    !>流体の物性値を取り扱う派生型．
    !>流体は，非圧縮性かつ等温を想定している．
    type, public :: fluid_property_type
        real(real64) :: density
            !! 密度 [kg/m^3]
        real(real64) :: viscosity
            !! 粘度 [Pa.s=kgm/s]
        real(real64) :: kinetic_viscosity
            !! 動粘度 [m^2/s] = 粘度/密度
    end type fluid_property_type
end module fluid_property
