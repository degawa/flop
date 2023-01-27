!>\(l_2\)ノルムに関する手続を提供する．
!>
!>手続には，スカラ量に対する\(l_2\)ノルム
!>\[
!>\|f\|= \sqrt{\sum_{i}{f_i^2}}
!>\]
!>が含まれる．
!>
!>また，既存の関数と同じ名称`norm2`で利用できるようにするための
!>インタフェースも含まれる．
!>
module grid_uniform_stg_op_unary_norm_l2
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_2d
    use :: grid_uniform_stg_vars_scalar_2d
    implicit none
    private
    public :: norm2

    !>総称名`norm2`を定義するインタフェース
    interface norm2
        procedure :: norm_l2_scr
    end interface

contains
    !>\(l_2\)ノルムを返す．
    function norm_l2_scr(scr) result(norm_l2)
        implicit none

        type(scalar_2d_type), intent(in) :: scr
            !! 被演算子

        real(real64) :: norm_l2
            !! 計算された\(l_2\)ノルム

        type(staggered_uniform_grid_2d_type), pointer :: grid
        integer(int32) :: Ncx, Ncy

        grid => scr%get_base_grid()
        call grid%get_number_of_grid_center_to(Ncx, Ncy)

        norm_l2 = sqrt(sum(scr%val(1:Ncx, 1:Ncy)*scr%val(1:Ncx, 1:Ncy)))
    end function norm_l2_scr
end module grid_uniform_stg_op_unary_norm_l2
