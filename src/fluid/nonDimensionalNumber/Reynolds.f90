!>流体の無次元量に関係する手続を提供する．
!>
!>手続には，Reynolds数の定義\(\mathrm{Re} = \frac{ul}{\nu}\)に基づいて
!>レイノルズ数や代表速度等を計算する手続が含まれる．
!>
!>\(\mathrm{Re}\)はレイノルズ数，\(u\)は代表速度，\(l\)は代表長さ，
!>\(\nu\)は動粘度である．
!>
module fluid_nonDimensionalNumber_Reynolds
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: Reynolds__compute_Reynolds_number
    public :: Reynolds__compute_velocity
    public :: Reynolds__compute_length
    public :: Reynolds__compute_kinetic_viscosity

    !>レイノルズ数を計算する手続に
    !>接頭辞`Reynolds__`を付けるためのインタフェース
    interface Reynolds__compute_Reynolds_number
        procedure :: compute_Reynolds_number
    end interface

    !>レイノルズ数の定義から代表速度を計算する手続に
    !>接頭辞`Reynolds__`を付けるためのインタフェース
    interface Reynolds__compute_velocity
        procedure :: compute_velocity
    end interface

    !>レイノルズ数の定義から代表長さを計算する手続に
    !>接頭辞`Reynolds__`を付けるためのインタフェース
    interface Reynolds__compute_length
        procedure :: compute_length
    end interface

    !>レイノルズ数の定義から動粘度を計算する手続に
    !>接頭辞`Reynolds__`を付けるためのインタフェース
    interface Reynolds__compute_kinetic_viscosity
        procedure :: compute_kinetic_viscosity
    end interface
contains
    !>レイノルズ数の定義に従ってレイノルズ数
    !>\[
    !>\mathrm{Re} = \frac{ul}{\nu}
    !>\]
    !>を計算して返す．
    function compute_Reynolds_number(velocity, length, kinetic_viscosity) result(Re)
        implicit none
        real(real64), intent(in) :: velocity
            !! 代表速度
        real(real64), intent(in) :: length
            !! 代表長さ
        real(real64), intent(in) :: kinetic_viscosity
            !! 動粘度
        real(real64) :: Re
            !! レイノルズ数

        Re = velocity*length/kinetic_viscosity
    end function compute_Reynolds_number

    !>レイノルズ数の定義に従って代表速度
    !>\[
    !>u = \frac{\mathrm{Re}\nu}{l}
    !>\]
    !>を計算して返す．
    function compute_velocity(Re, length, kinetic_viscosity) result(velocity)
        implicit none
        real(real64), intent(in) :: Re
            !! レイノルズ数
        real(real64), intent(in) :: length
            !! 代表長さ
        real(real64), intent(in) :: kinetic_viscosity
            !! 動粘度
        real(real64) :: velocity
            !! 代表速度

        velocity = Re*kinetic_viscosity/length
    end function compute_velocity

    !>レイノルズ数の定義に従って代表長さ
    !>\[
    !>l = \frac{\mathrm{Re}\nu}{u}
    !>\]
    !>を計算して返す．
    function compute_length(Re, velocity, kinetic_viscosity) result(length)
        implicit none
        real(real64), intent(in) :: Re
            !! レイノルズ数
        real(real64), intent(in) :: velocity
            !! 代表速度
        real(real64), intent(in) :: kinetic_viscosity
            !! 動粘度
        real(real64) :: length
            !! 代表長さ

        length = Re*kinetic_viscosity/velocity
    end function compute_length

    !>レイノルズ数の定義に従って動粘度
    !>\[
    !>\nu= \frac{ul}{\mathrm{Re}}
    !>\]
    !>を計算して返す．
    function compute_kinetic_viscosity(Re, velocity, length) result(kinetic_viscosity)
        implicit none
        real(real64), intent(in) :: Re
            !! レイノルズ数
        real(real64), intent(in) :: velocity
            !! 代表速度
        real(real64), intent(in) :: length
            !! 代表長さ
        real(real64) :: kinetic_viscosity
            !! 動粘度

        kinetic_viscosity = velocity*length/Re
    end function compute_kinetic_viscosity
end module fluid_nonDimensionalNumber_Reynolds
