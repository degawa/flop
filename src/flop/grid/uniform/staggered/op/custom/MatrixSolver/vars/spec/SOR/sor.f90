!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する派生型および手続を提供する．
!>
!>派生型には，連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)を
!>求解するSOR法の仕様を表す派生型が含まれる．
!>
!>手続には，SOR法の仕様のコンストラクタが含まれる．
!>
module grid_uniform_staggered_op_custom_solver_vars_solver_spec_sor
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_op_custom_solver_vars_solver_spec_adt
    implicit none
    private
    public :: SOR

    !>ソルバの仕様を表す型．
    type, public, extends(solver_spec_atype) :: SOR_spec_type
        real(real64) :: accel = 1d0
            !! 加速係数
    contains
        procedure, public, pass :: set_acceleration_coefficient
        !* 加速係数を設定
        procedure, public, pass :: get_acceleration_coefficient
        !* 加速係数を返却
    end type SOR_spec_type

    !>`SOR_spec_type`のコンストラクタを
    !>`SOR(acceleration coefficient)`と呼ぶためのインタフェース．
    interface SOR
        procedure :: construct_SOR_spec
    end interface

contains
    !>`SOR_spec_type`を返す．
    function construct_SOR_spec(acceleration_coefficient) result(new_sor_spec)
        implicit none
        real(real64), intent(in) :: acceleration_coefficient
            !! 加速係数
        type(SOR_spec_type) :: new_sor_spec
            !! 新しく設定された仕様

        call new_sor_spec%set_acceleration_coefficient(acceleration_coefficient)
    end function construct_SOR_spec

    !>加速係数を設定する．
    !>加速係数が0以下もしくは2以上の場合，初期値（1）が採用される．
    subroutine set_acceleration_coefficient(this, acceleration_coefficient)
        implicit none
        !&<
        class(SOR_spec_type), intent(inout) :: this
            !! 当該実体仮引数
        real(real64)        , intent(in)    :: acceleration_coefficient
            !! 加速係数
        !&>

        ! 加速係数が収束条件を満たさない（0以下もしくは2以上の）場合は
        ! 加速係数は設定しない．（未設定の場合は初期値1）
        if (acceleration_coefficient <= 0d0) return
        if (2d0 <= acceleration_coefficient) return

        ! <1の場合は減速緩和であることを通知
        if (0d0 <= acceleration_coefficient .and. acceleration_coefficient < 1d0) &
            print *, "under relaxation"

        this%accel = acceleration_coefficient
    end subroutine set_acceleration_coefficient

    !>仕様に設定された加速係数を返す．
    function get_acceleration_coefficient(this) result(accel)
        implicit none
        class(SOR_spec_type), intent(in) :: this
            !! 当該実体仮引数
        real(real64) :: accel
            !! 加速係数

        accel = this%accel
    end function get_acceleration_coefficient
end module grid_uniform_staggered_op_custom_solver_vars_solver_spec_sor
