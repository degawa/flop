!>単項演算子laplacian（∇²）に関する手続を提供する．
!>
!>手続には，ベクトル量に対するlaplacian
!>\[
!>\nabla^2 \boldsymbol{u} = \frac{\partial u_i}{\partial x_j\partial x_j}
!>\]
!>と，スカラ量に対するlaplacian
!>\[
!>\nabla^2 p = \frac{\partial p}{\partial x_j\partial x_j}
!>\]
!>が含まれる．
!>
!>また，ユーザ定義演算子`.laplacian.`として公開するための
!>インタフェースも含まれる．
!>
module grid_uniform_stg_op_unary_laplacian_acc2
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_2d
    use :: grid_uniform_stg_variables
    implicit none
    private
    public :: operator(.laplacian.)

    !>ユーザ定義演算子`.laplacian.`を定義するインタフェース
    interface operator(.laplacian.)
        procedure :: laplacian_vec
        procedure :: laplacian_scr
    end interface

contains

    !>引数のベクトル量にlaplacian
    !>\[
    !>\nabla^2 \boldsymbol{u} = \frac{\partial u_i}{\partial x_j\partial x_j}
    !>\]
    !>を適用し，得られた結果をベクトル量で返す．
    function laplacian_vec(vec) result(new_vec)
        use :: space_vars_Cartesian, &
            x_dir => x_dir_index, y_dir => y_dir_index, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none

        type(vector_2d_type), intent(in) :: vec
            !! laplacianの被演算子

        type(vector_2d_type) :: new_vec
            !! laplacianが適用された結果のベクトル量

        type(staggered_uniform_grid_2d_type), pointer :: grid
        integer(int32) :: vr(2, 4)
        real(real64) :: dx, dy

        grid => vec%get_base_grid()
        call new_vec%construct(grid) ! 返値を構築

        call grid%get_interval_to(dx, dy) ! 格子幅を取得
        vr = grid%get_vector_range() ! ベクトル量の定義範囲

        block
            integer(int32) :: i, jc
            !&<
            do jc = vr(x_dir, y_min)+1, vr(x_dir, y_max)-1
            do i  = vr(x_dir, x_min)+1, vr(x_dir, x_max)-1
                new_vec%x(i, jc) = (vec%x(i-1,jc  ) -2d0*vec%x(i,jc) + vec%x(i+1,jc  ))/dx**2 &
                                  +(vec%x(i  ,jc-1) -2d0*vec%x(i,jc) + vec%x(i  ,jc+1))/dy**2
            end do
            end do
            !&>
        end block

        block
            integer(int32) :: ic, j
            !&<
            do j  = vr(y_dir, y_min)+1, vr(y_dir, y_max)-1
            do ic = vr(y_dir, x_min)+1, vr(y_dir, x_max)-1
                new_vec%y(ic, j) = (vec%y(ic-1, j  ) -2d0*vec%y(ic, j) + vec%y(ic+1, j  ))/dx**2 &
                                  +(vec%y(ic  , j-1) -2d0*vec%y(ic, j) + vec%y(ic  , j+1))/dy**2
            end do
            end do
            !&>
        end block
    end function laplacian_vec

    !>引数のスカラ量にlaplacian
    !>\[
    !>\nabla^2 p = \frac{\partial p}{\partial x_j\partial x_j}
    !>\]
    !>を適用し，得られた結果をスカラ量で返す．
    function laplacian_scr(scr) result(new_scr)
        use :: space_vars_Cartesian, &
            x_dir => x_dir_index, y_dir => y_dir_index, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none

        type(scalar_2d_type), intent(in) :: scr
            !! laplacianの被演算子

        type(scalar_2d_type) :: new_scr
            !! laplacianが適用された結果のスカラ量

        type(staggered_uniform_grid_2d_type), pointer :: grid
        integer(int32) :: sr(4)
        real(real64) :: dx, dy

        grid => scr%get_base_grid()

        call new_scr%construct(grid) ! 返値を構築

        call grid%get_interval_to(dx, dy) ! 格子幅を取得
        sr = grid%get_scalar_range() ! スカラ量の定義範囲

        block
            integer(int32) :: ic, jc
            !&<
            do jc = sr(y_min)+1, sr(y_max)-1
            do ic = sr(x_min)+1, sr(x_max)-1
                new_scr%val(ic, jc) = (scr%val(ic-1,jc  ) -2d0*scr%val(ic,jc) + scr%val(ic+1,jc  ))/dx**2 &
                                     +(scr%val(ic  ,jc-1) -2d0*scr%val(ic,jc) + scr%val(ic  ,jc+1))/dy**2
            end do
            end do
            !&>
        end block
    end function laplacian_scr
end module grid_uniform_stg_op_unary_laplacian_acc2
