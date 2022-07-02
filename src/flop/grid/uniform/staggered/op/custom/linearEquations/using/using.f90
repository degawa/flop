!>連立方程式\(\boldsymbol{Ax}=\boldsymbol{b}\)
!>の取り扱いに関する手続を提供する．
!>
module grid_uniform_stg_op_cust_linEqs_using
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_vars_scalar_2d
    use :: grid_uniform_stg_op_cust_linEqs_vars_AxEqB
    use :: grid_uniform_stg_op_cust_linEqs_vars_solver_spec_adt
    implicit none
    private
    public :: operator(.using.)

    !>ユーザ定義演算子`.using.`を定義するインタフェース
    interface operator(.using.)
        procedure :: set_matrix_solver
    end interface

contains
    !>連立方程式を取り扱う型`Ax_eq_b_type`に反復法の許容誤差を渡し，
    !>それらを反映した`Ax_eq_b_type`を返す．
    function set_matrix_solver(Ax_eq_b, solver_spec) result(new_ax_eq_b)
        implicit none
        !&<
        type(Ax_eq_b_type)      , intent(in) :: Ax_eq_b
            !! 連立方程式
        class(solver_spec_atype), intent(in) :: solver_spec
            !! 連立方程式の解法の情報
        !&>
        type(Ax_eq_b_type) :: new_Ax_eq_b
            !! 許容誤差を反映した連立方程式

        new_Ax_eq_b = Ax_eq_b

        call new_Ax_eq_b%Ax%destruct_solver()
        call new_Ax_eq_b%Ax%construct_solver(solver_spec)
    end function set_matrix_solver
end module grid_uniform_stg_op_cust_linEqs_using
