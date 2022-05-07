!| 単項演算子divergence（∇･）に関する手続を提供する．
!
!手続には，ベクトル量に対するdivergence
!\[
!\nabla\cdot \boldsymbol{u} = \frac{\partial u_j}{\partial x_j}
!\]
!と，テンソル量に対するdivergence
!\[
!\nabla\cdot \boldsymbol{\tau} = \frac{\partial \tau_{ij}}{\partial x_j}
!\]
!が含まれる．
!
!また，ユーザ定義演算子`.div.`として公開するための
!インタフェースも含まれる．
!
module grid_uniform_staggered_op_unary_div_acc2
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_2d
    use :: grid_uniform_staggered_vars_scalar_2d
    use :: grid_uniform_staggered_vars_vector_2d
    use :: grid_uniform_staggered_vars_tensor_2d
    implicit none
    private
    public :: operator(.div.)

    !| ユーザ定義演算子`.div.`を定義するインタフェース
    interface operator(.div.)
        procedure :: div_vec
        procedure :: div_tsr
    end interface

contains
    !| 引数のベクトル量にdivergence
    !\[
    !\nabla\cdot \boldsymbol{u} = \frac{\partial u_j}{\partial x_j}
    !\]
    !を適用し，得られた結果をスカラ量で返す．
    function div_vec(vec) result(new_scr)
        use :: space_Cartesian, &
            x_dir => x_dir_index, y_dir => y_dir_index, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none

        type(vector_2d_type), intent(in) :: vec
            !! divergenceの被演算子

        type(scalar_2d_type) :: new_scr
            !! divergenceが適用された結果のスカラ量

        type(staggered_uniform_grid_2d_type), pointer :: grid
        integer(int32) :: sr(4)
        real(real64) :: dx, dy

        grid => vec%get_base_grid()

        call new_scr%construct(grid) ! 返値を構築

        call grid%get_interval_to(dx, dy) ! 格子幅を取得
        sr = grid%get_scalar_range() ! スカラ量の定義範囲

        block
            integer(int32) :: ic, jc, i, j
            do jc = sr(y_min), sr(y_max)
            do ic = sr(x_min), sr(x_max)
                i = ic
                j = jc
                new_scr%val(ic, jc) =  (-vec%x(i , jc) + vec%x(i +1, jc  ))/dx & !&
                                     + (-vec%y(ic, j ) + vec%y(ic  , j +1))/dy !&
            end do
            end do
        end block
    end function div_vec

    !| 引数のテンソル量にdivergence
    !\[
    !\nabla\cdot \boldsymbol{\tau} = \frac{\partial \tau_{ij}}{\partial x_j}
    !\]
    !を適用し，得られた結果をベクトル量で返す．
    function div_tsr(tsr) result(new_vec)
        use :: space_Cartesian, &
            x_dir => x_dir_index, y_dir => y_dir_index, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none

        type(tensor_2d_type), intent(in) :: tsr
            !! divergenceの被演算子
        type(vector_2d_type) :: new_vec
            !! divergenceが適用された結果のベクトル量

        type(staggered_uniform_grid_2d_type), pointer :: grid
        integer(int32) :: vr(2, 4)
        real(real64) :: dx, dy

        grid => tsr%get_base_grid() ! 返値を構築

        call new_vec%construct(grid)

        call grid%get_interval_to(dx, dy) ! 格子幅を取得
        vr = grid%get_vector_range() ! スカラ量の定義範囲

        block
            integer(int32) :: i, jc, ic, j
            !&<
            do jc = vr(x_dir, y_min)+1, vr(x_dir, y_max)-1
            do i  = vr(x_dir, x_min)+1, vr(x_dir, x_max)-1
                ic = i
                j  = jc
                new_vec%x(i , jc) =  (-tsr%xx(ic-1, jc) + tsr%xx(ic  , jc  ))/dx &
                                   + (-tsr%xy(i   , j ) + tsr%xy(i   , j +1))/dy
            end do
            end do
            !&>
        end block

        block
            integer(int32) :: ic, j, i, jc
            !&<
            do j  = vr(y_dir, y_min)+1, vr(y_dir, y_max)-1
            do ic = vr(y_dir, x_min)+1, vr(y_dir, x_max)-1
                i  = ic
                jc = j
                new_vec%y(ic, j) =  (-tsr%yx(i , j   ) + tsr%yx(i +1, j ))/dx &
                                  + (-tsr%yy(ic, jc-1) + tsr%yy(ic  , jc))/dy
            end do
            end do
            !&>
        end block
    end function div_tsr
end module grid_uniform_staggered_op_unary_div_acc2
