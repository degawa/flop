!>非圧縮性流れの特徴量に関係する手続を提供する．
!>
!>手続には，特徴量を管理する派生型から成分を取得する手続が含まれる．
!>
module incompressible_op_value
    use, intrinsic :: iso_fortran_env
    use :: incompressible_vars_characteristics
    use :: incompressible_op_vars_characteristicLength
    use :: incompressible_op_vars_characteristicVelocity
    use :: incompressible_op_vars_kineticViscosity
    use :: incompressible_op_vars_ReynoldsNumber
    implicit none
    private
    public :: operator(.value.)

    !>ユーザ定義演算子`.value.`を定義するインタフェース
    interface operator(.value.)
        procedure :: get_char_len
        procedure :: get_char_velo
        procedure :: get_kinetic_viscosity
        procedure :: get_Reynolds_number
    end interface

contains
    !>代表長さを返す．
    function get_char_len(characteristics, length) result(length_)
        implicit none
        !&<
        class(characteristics_type)         , intent(in) :: characteristics
            !! 特徴量
        class(characteristic_length_type)   , intent(in) :: length
            !! 代表長さ
        !&>
        real(real64) :: length_
            !! 代表長さ

        length_ = characteristics%get_length()

        return
        if (same_type_as(length, length)) continue ! 変数未使用警告の抑制
    end function get_char_len

    !>代表速度を返す．
    function get_char_velo(characteristics, velocity) result(velocity_)
        implicit none
        !&<
        class(characteristics_type)         , intent(in) :: characteristics
            !! 特徴量
        class(characteristic_velocity_type) , intent(in) :: velocity
            !! 代表長さ
        !&>
        real(real64) :: velocity_
            !! 代表長さ

        velocity_ = characteristics%get_velocity()

        return
        if (same_type_as(velocity, velocity)) continue ! 変数未使用警告の抑制
    end function get_char_velo

    !>動粘度を返す．
    function get_kinetic_viscosity(characteristics, kinetic_visc) result(kinetic_visc_)
        implicit none
        !&<
        class(characteristics_type)     , intent(in) :: characteristics
            !! 特徴量
        class(kinetic_viscosity_type)   , intent(in) :: kinetic_visc
            !! 代表長さ
        !&>
        real(real64) :: kinetic_visc_
            !! 代表長さ

        kinetic_visc_ = characteristics%get_kinetic_viscosity()

        return
        if (same_type_as(kinetic_visc, kinetic_visc)) continue ! 変数未使用警告の抑制
    end function get_kinetic_viscosity

    !>動粘度を返す．
    function get_Reynolds_number(characteristics, Re) result(Re_)
        implicit none
        !&<
        class(characteristics_type) , intent(in) :: characteristics
            !! 特徴量
        class(Reynolds_number_type) , intent(in) :: Re
            !! 代表長さ
        !&>
        real(real64) :: Re_
            !! 代表長さ

        Re_ = characteristics%get_Reynolds_number()

        return
        if (same_type_as(Re, Re)) continue ! 変数未使用警告の抑制
    end function get_Reynolds_number
end module incompressible_op_value
