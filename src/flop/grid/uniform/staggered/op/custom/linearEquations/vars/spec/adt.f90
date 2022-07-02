!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する派生型を提供する．
!>
!>派生型には，連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)を
!>求解するソルバの仕様を表す抽象データ型が含まれる．
!>
module grid_uniform_staggered_op_custom_solver_vars_solver_spec_adt
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    !>ソルバの仕様を表す抽象データ型．
    type, public, abstract :: solver_spec_atype
    end type solver_spec_atype
end module grid_uniform_staggered_op_custom_solver_vars_solver_spec_adt
