!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する派生型および手続を提供する．
!>
!>派生型には，連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)を
!>求解するRed-Black SOR法の仕様を表す派生型が含まれる．
!>
!>手続には，Red-Black SOR法の仕様のコンストラクタが含まれる．
!>
module grid_uniform_staggered_op_custom_linEqs_vars_solver_spec_rbsor
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_op_custom_linEqs_vars_solver_spec_sor
    implicit none
    private
    public :: RBSOR

    !>ソルバの仕様を表す型．
    type, public, extends(SOR_spec_type) :: RBSOR_spec_type
    contains
    end type RBSOR_spec_type

    !>`RBSOR_spec_type`のコンストラクタを
    !>`RBSOR(acceleration coefficient)`と呼ぶためのインタフェース．
    interface RBSOR
        procedure :: construct_RBSOR_spec
    end interface

contains
    !>`RBSOR_spec_type`を返す．
    function construct_RBSOR_spec(acceleration_coefficient) result(new_rbsor_spec)
        implicit none
        real(real64), intent(in) :: acceleration_coefficient
            !! 加速係数
        type(RBSOR_spec_type) :: new_rbsor_spec
            !! 新しく設定された仕様

        call new_rbsor_spec%set_acceleration_coefficient(acceleration_coefficient)
    end function construct_RBSOR_spec
end module grid_uniform_staggered_op_custom_linEqs_vars_solver_spec_rbsor
