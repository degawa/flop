!| スカラ量あるいはベクトル量に，境界条件を適用する手続を提供する．
!
!手続には，ベクトル量に境界条件を適用する手続と，
!スカラ量に境界条件を適用する手続が含まれる．
!
!`u .impose. BC_u`などと表現することを実現するための
!ユーザ定義演算子`.impose.`を定義するインタフェースも含まれる．
!
!また，スカラ量の境界条件をin-placeで適用するための手続`impose`も定義される．
!
!@note
!`.impose.`に限らず，演算子は被演算子を更新できない．
!そのため，連立方程式を反復法で解く場合，`.impose.`を利用すると
!反復の繰り返しの中でNeumann境界条件を適用する度に，
!`p = p .impose. BC_p`とする必要があり，一時変数の生成・解放が行われる．
!
!そのため，in-placeで境界条件を適用する`impose()`を定義し，
!`call impose(p, BC_p)`として更新することで
!一時変数の生成・解放を回避する．
!@endnote
!
module grid_uniform_staggered_op_custom_bc_impose
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_2d
    use :: grid_uniform_staggered_vars_vector_2d
    use :: grid_uniform_staggered_vars_scalar_2d
    use :: grid_uniform_staggered_op_custom_bc_type
    use :: grid_uniform_staggered_op_custom_bc_position
    use :: grid_uniform_staggered_op_custom_bc_vector
    use :: grid_uniform_staggered_op_custom_bc_scalar
    implicit none
    private
    public :: operator(.impose.)
    public :: impose

    !| ユーザ定義演算子`.impose.`を定義するインタフェース．
    !
    !ベクトル量およびスカラ量へのDirichlet境界条件
    !の適用のみ取り扱う．
    interface operator(.impose.)
        procedure :: impose_vec_vecbc_dirichlet_op
        procedure :: impose_scr_scrbc_dirichlet_op
    end interface

    !| `impose`手続の総称名．
    !
    !スカラ用のNeumann境界条件の適用を行う．
    interface impose
        procedure :: impose_scr_scrbc_neumann
    end interface
contains

    !| ベクトル量に対するDirichlet境界条件を適用し，
    !条件が適用されたベクトル量を返す．
    function impose_vec_vecbc_dirichlet_op(vec, vec_bc) result(new_vec)
        use :: space_Cartesian, &
            x_dir => x_dir_index, y_dir => y_dir_index, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none
        !&<
        class(vector_2d_type)                   , intent(in) :: vec
            !! 境界条件が適用されるベクトル量
        class(vector_boundary_condition_type)   , intent(in) :: vec_bc
            !! 境界条件
        !&>
        type(vector_2d_type) :: new_vec
            !! 境界条件が適用されたベクトル量

        type(staggered_uniform_grid_2d_type), pointer :: grid
        integer(int32) :: Nx, Ny, Ncx, Ncy, vr(2, 4), bnd_idx
        real(real64) :: vec_val(2)

        grid => vec%get_base_grid()

        ! 返値を構築，引数の値をコピーし，それに境界条件を適用する
        call new_vec%construct(grid)
        new_vec = vec

        call grid%get_number_of_grid_points_to(Nx, Ny)
        call grid%get_number_of_grid_center_to(Ncx, Ncy)
        vr = grid%get_vector_range()

        !境界B1（-x方向）
        block
            integer(int32) :: j, jc

            bnd_idx = B1%position
            ! Dirichlet境界の場合のみ境界条件を適用
            if (is_Dirichlet_boundary(vec_bc%boundary_type(bnd_idx))) then
                vec_val = vec_bc%get_boundary_value(bnd_idx)
                !&<
                do jc = vr(x_dir,y_min),vr(x_dir,y_max)
                    new_vec%x(1  , jc) = vec_val(x_dir) ! 境界
                    new_vec%x(1-1, jc) = new_vec%x(1+1, jc) ! 仮想点
                end do
                do j = vr(y_dir,y_min), vr(y_dir,y_max)
                    new_vec%y(1-1, j) = 2d0*vec_val(y_dir)-new_vec%y(1, j) ! 仮想点
                end do
                !&>
            end if
        end block

        !境界B2（+x方向）
        block
            integer(int32) :: j, jc

            bnd_idx = B2%position
            ! Dirichlet境界の場合のみ境界条件を適用
            if (is_Dirichlet_boundary(vec_bc%boundary_type(bnd_idx))) then
                vec_val = vec_bc%get_boundary_value(bnd_idx)
                !&<
                do jc = vr(x_dir,y_min),vr(x_dir,y_max)
                    new_vec%x(Nx  , jc) = vec_val(x_dir) ! 境界
                    new_vec%x(Nx+1, jc) = new_vec%x(Nx-1, jc) ! 仮想点
                end do
                do j = vr(y_dir,y_min), vr(y_dir,y_max)
                    new_vec%y(Ncx+1, j) = 2d0*vec_val(y_dir)-new_vec%y(Ncx, j) ! 仮想点
                end do
                !&>
            end if
        end block

        !境界B3（-y方向）
        block
            integer(int32) :: i, ic

            bnd_idx = B3%position

            if (is_Dirichlet_boundary(vec_bc%boundary_type(bnd_idx))) then
                vec_val = vec_bc%get_boundary_value(bnd_idx)
                !&<
                do i = vr(x_dir,x_min), vr(x_dir,x_max)
                    new_vec%x(i, 1-1) = 2d0*vec_val(x_dir)-new_vec%x(i, 1) ! 仮想点
                end do
                do ic = vr(y_dir,x_min), vr(y_dir,x_max)
                    new_vec%y(ic, 1  ) = vec_val(y_dir) ! 境界
                    new_vec%y(ic, 1-1) = new_vec%y(ic, 1+1) ! 仮想点
                end do
                !&>
            end if
        end block

        !境界B4（+y方向）
        block
            integer(int32) :: i, ic

            bnd_idx = B4%position

            if (is_Dirichlet_boundary(vec_bc%boundary_type(bnd_idx))) then
                vec_val = vec_bc%get_boundary_value(bnd_idx)
                !&<
                do i = vr(x_dir,x_min), vr(x_dir,x_max)
                    new_vec%x(i, Ncy+1) = 2d0*vec_val(x_dir)-new_vec%x(i, Ncy)
                end do
                do ic = vr(y_dir,x_min), vr(y_dir,x_max)
                    new_vec%y(ic, Ny  ) = vec_val(y_dir)
                    new_vec%y(ic, Ny+1) = new_vec%y(ic, Ny-1)
                end do
                !&>
            end if
        end block
    end function impose_vec_vecbc_dirichlet_op

    !| スカラ量に対するDirichlet境界条件を適用し，
    !条件が適用されたスカラ量を返す．
    function impose_scr_scrbc_dirichlet_op(scr, scr_bc) result(new_scr)
        use :: space_Cartesian, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none
        !&<
        class(scalar_2d_type)                   , intent(in) :: scr
            !! 境界条件が適用されるスカラ量
        class(scalar_boundary_condition_type)   , intent(in) :: scr_bc
            !! 境界条件
        !&>
        type(scalar_2d_type) :: new_scr
            !! 境界条件が適用されたスカラ量

        type(staggered_uniform_grid_2d_type), pointer :: grid
        integer(int32) :: Ncx, Ncy, sr(4), bnd_idx
        real(real64) :: scr_val

        grid => scr%get_base_grid()

        ! 返値を構築，引数の値をコピーし，それに境界条件を適用する
        call new_scr%construct(grid)
        new_scr = scr

        call grid%get_number_of_grid_center_to(Ncx, Ncy)
        sr = grid%get_scalar_range()

        !境界B1（-x方向）
        block
            integer(int32) :: jc

            bnd_idx = B1%position

            if (is_Dirichlet_boundary(scr_bc%boundary_type(bnd_idx))) then
                scr_val = scr_bc%get_boundary_value(bnd_idx)
                !&<
                do jc = sr(y_min)+1,sr(y_max)-1
                    new_scr%val(1-1, jc) = 2d0*scr_val - new_scr%val( 1, jc) ! 仮想点
                end do
                !&>
            end if
        end block

        !境界B2（+x方向）
        block
            integer(int32) :: jc

            bnd_idx = B2%position

            if (is_Dirichlet_boundary(scr_bc%boundary_type(bnd_idx))) then
                scr_val = scr_bc%get_boundary_value(bnd_idx)
                !&<
                do jc = sr(y_min)+1, sr(y_max)-1
                    new_scr%val(Ncx+1, jc) = 2d0*scr_val - new_scr%val(Ncx, jc) ! 仮想点
                end do
                !&>
            end if
        end block

        !境界B3（-y方向）
        block
            integer(int32) :: ic

            bnd_idx = B3%position

            if (is_Dirichlet_boundary(scr_bc%boundary_type(bnd_idx))) then
                scr_val = scr_bc%get_boundary_value(bnd_idx)
                !&<
                do ic = sr(x_min)+1, sr(x_max)-1
                    new_scr%val(ic, 1-1) = 2d0*scr_val - new_scr%val(ic,  1) ! 仮想点
                end do
                !&>
            end if
        end block

        !境界B4（+y方向）
        block
            integer(int32) :: ic

            bnd_idx = B4%position

            if (is_Dirichlet_boundary(scr_bc%boundary_type(bnd_idx))) then
                scr_val = scr_bc%get_boundary_value(bnd_idx)
                !&<
                do ic = sr(x_min)+1, sr(x_max)-1
                    new_scr%val(ic, Ncy+1) = 2d0*scr_val - new_scr%val(ic, Ncy) ! 仮想点
                end do
                !&>
            end if
        end block
    end function impose_scr_scrbc_dirichlet_op

    !| スカラ量に対するNeumann境界条件を適用し，in-placeで更新する．
    subroutine impose_scr_scrbc_neumann(scr, scr_bc)
        use :: space_Cartesian, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none
        !&<
        class(scalar_2d_type)                   , intent(inout) :: scr
            !! 境界条件が適用され，更新されるスカラ量
        class(scalar_boundary_condition_type)   , intent(in) :: scr_bc
            !! 境界条件
        !&>

        type(staggered_uniform_grid_2d_type), pointer :: grid
        integer(int32) :: Ncx, Ncy, sr(4), bnd_idx
        real(real64) :: dx, dy, scr_grad

        grid => scr%get_base_grid()

        call grid%get_number_of_grid_center_to(Ncx, Ncy)
        call grid%get_interval_to(dx, dy)
        sr = grid%get_scalar_range()

        !境界B1（-x方向）
        block
            integer(int32) :: jc

            bnd_idx = B1%position

            if (is_Neumann_boundary(scr_bc%boundary_type(bnd_idx))) then
                scr_grad = scr_bc%get_boundary_gradient(bnd_idx)
                !&<
                do jc = sr(y_min)+1,sr(y_max)-1
                    scr%val(1-1, jc) = scr%val( 1, jc)-scr_grad*dx ! 仮想点
                end do
                !&>
            end if
        end block

        !境界B2（+x方向）
        block
            integer(int32) :: jc

            bnd_idx = B2%position

            if (is_Neumann_boundary(scr_bc%boundary_type(bnd_idx))) then
                scr_grad = scr_bc%get_boundary_gradient(bnd_idx)
                !&<
                do jc = sr(y_min)+1, sr(y_max)-1
                    scr%val(Ncx+1, jc) = scr%val(Ncx, jc)+scr_grad*dx ! 仮想点
                end do
                !&>
            end if
        end block

        !境界B3（-y方向）
        block
            integer(int32) :: ic

            bnd_idx = B3%position

            if (is_Neumann_boundary(scr_bc%boundary_type(bnd_idx))) then
                scr_grad = scr_bc%get_boundary_gradient(bnd_idx)
                !&<
                do ic = sr(x_min)+1, sr(x_max)-1
                    scr%val(ic, 1-1) = scr%val(ic,  1)-scr_grad*dy ! 仮想点
                end do
                !&>
            end if
        end block

        !境界B4（+y方向）
        block
            integer(int32) :: ic

            bnd_idx = B4%position

            if (is_Neumann_boundary(scr_bc%boundary_type(bnd_idx))) then
                scr_grad = scr_bc%get_boundary_gradient(bnd_idx)
                !&<
                do ic = sr(x_min)+1, sr(x_max)-1
                    scr%val(ic, Ncy+1) = scr%val(ic, Ncy)+scr_grad*dy ! 仮想点
                end do
                !&>
            end if
        end block
    end subroutine impose_scr_scrbc_neumann
end module grid_uniform_staggered_op_custom_bc_impose
