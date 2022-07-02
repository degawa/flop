!>空間離散化情報である空間格子に関係する派生型を提供する．
!>
!>派生型には，2次元等間隔のスタガード格子を表す型が含まれる．
!>
module grid_uniform_stg_2d
    use, intrinsic :: iso_fortran_env
    use :: space_vars_Cartesian
    implicit none
    private

    integer(int32), private, parameter :: dim = 2
        !! 空間次元

    type, public :: staggered_uniform_grid_2d_type
        type(Cartesian_2d_type), private :: space
            !! 計算領域の情報
        integer(int32), private :: Nx
            !! \(x\)方向格子点数
        integer(int32), private :: Ny
            !! \(y\)方向格子点数
        real(real64), private :: dx
            !! \(x\)方向格子点間隔
        real(real64), private :: dy
            !! \(y\)方向格子点間隔
        real(real64), public, allocatable :: x(:)
            !! \(x\)方向格子点座標<br>
            !! 仮想点の座標も含む
        real(real64), public, allocatable :: y(:)
            !! \(y\)方向格子点座標<br>
            !! 仮想点の座標も含む

        ! staggered格子固有の情報
        integer(int32), private :: Ncx
            !! セル数
        integer(int32), private :: Ncy
            !! セル数
        integer(int32), private :: Nv = 1
            !! 4方向(-x,+x,-y,+y)に設けられる仮想点数
        real(real64), public, allocatable :: xc(:)
            !! \(x\)方向セル中心座標<br>
            !! 仮想点の座標も含む
        real(real64), public, allocatable :: yc(:)
            !! \(y\)方向セル中心座標<br>
            !! 仮想点の座標も含む
    contains
        !&<
        procedure, public, pass :: construct_by_length_and_num_grid_points
            !! 座標系情報と格子点数を用いて1次元等間隔格子を設定
        procedure, public, pass :: construct_by_length_and_interval
            !! 座標系情報と格子点間隔に基づいて1次元等間隔格子を設定
        procedure, public, pass :: construct_by_num_grid_points_and_interval
            !! 座標系情報，格子点数，格子点間隔に基づいて
            !! 1次元等間隔格子を設定
        generic :: construct => construct_by_length_and_num_grid_points, &
                                construct_by_length_and_interval, &
                                construct_by_num_grid_points_and_interval
        !&>
        procedure, public, pass :: get_number_of_grid_points
            !! \(x, y\)方向の格子点数を配列で返却<br>
            !! 仮想点の数は含まない
        procedure, public, pass :: get_number_of_grid_points_to
            !! \(x, y\)方向の格子点数を個々に取得<br>
            !! 仮想点の数は含まない
        procedure, public, pass :: get_interval
            !! \(x, y\)方向の格子点間隔を配列で返却
        procedure, public, pass :: get_interval_to
            !! \(x, y\)方向の格子点間隔を個々に取得
        procedure, public, pass :: get_length
            !! \(x, y\)方向の領域長さを配列で返却
        procedure, public, pass :: get_length_to
            !! \(x, y\)方向の領域長さを個々に取得
        procedure, public, pass :: get_number_of_grid_center
            !! \(x, y\)方向のセル数を配列で返却<br>
            !! 仮想点の数は含まない
        procedure, public, pass :: get_number_of_grid_center_to
            !! \(x, y\)方向のセル数を個々に取得<br>
            !! 仮想点の数は含まない
        procedure, public, pass :: get_number_of_virtual_points
            !! 仮想点数を返却
        procedure, public, pass :: get_grid_point_range
            !! 仮想点を含む格子点数の範囲を配列で返却
        procedure, public, pass :: get_grid_center_range
            !! 仮想点を含むセル数の範囲を配列で返却
        procedure, public, pass :: get_scalar_range
            !! スカラ量が定義されている定義点番号の範囲を返却<br>
            !! 仮想点を含む
        procedure, public, pass :: get_vector_range
            !! ベクトル量が定義されている定義点番号の範囲を返却<br>
            !! 仮想点を含む
        procedure, public, pass :: get_tensor_range
            !! テンソル量が定義されている定義点番号の範囲を返却<br>
            !! 仮想点を含む

        procedure, private, pass :: discretize_space
            !! 離散化を実行
    end type staggered_uniform_grid_2d_type

contains

    !>座標系情報と格子点数を用いて1次元等間隔格子を設定する．
    subroutine construct_by_length_and_num_grid_points(this, space, number_of_grid_points)
        implicit none
        !&<
        class(staggered_uniform_grid_2d_type)   , intent(inout) :: this
            !! 当該実体仮引数
        type(Cartesian_2d_type)                 , intent(in)    :: space
            !! 空間座標系の情報
        integer(int32)                          , intent(in)    :: number_of_grid_points(dim)
            !! 格子点数
        !&>

        real(real64) :: length(dim)

        ! 既知情報の代入
        this%Nx = number_of_grid_points(x_dir_index)
        this%Ny = number_of_grid_points(y_dir_index)
        this%space = space
        length(:) = space%get_length()

        ! 未知情報（格子点間隔）を計算
        this%dx = length(x_dir_index)/dble(number_of_grid_points(x_dir_index)-1) !&
        this%dy = length(y_dir_index)/dble(number_of_grid_points(y_dir_index)-1) !&

        call this%discretize_space()
    end subroutine construct_by_length_and_num_grid_points

    !>座標系情報と格子点間隔に基づいて1次元等間隔格子を設定する．
    subroutine construct_by_length_and_interval(this, space, interval)
        implicit none
        !&<
        class(staggered_uniform_grid_2d_type)   , intent(inout) :: this
           !! 当該実体仮引数
        type(Cartesian_2d_type)                 , intent(in)    :: space
           !! 空間座標系の情報
        real(real64)                            , intent(in)    :: interval(dim)
            !! 格子点間隔
        !&>

        real(real64) :: length(dim)

        ! 既知情報の代入
        this%space = space
        length(:) = space%get_length()
        this%dx = interval(x_dir_index)
        this%dy = interval(y_dir_index)

        ! 未知情報（格子点数）を計算
        this%Nx = nint(length(x_dir_index)/interval(x_dir_index))
        this%Ny = nint(length(y_dir_index)/interval(y_dir_index))

        call this%discretize_space()
    end subroutine construct_by_length_and_interval

    !>座標系情報，格子点数，格子点間隔に基づいて
    !>1次元等間隔格子を設定する．<br>
    !>整合性は確認しない．
    subroutine construct_by_num_grid_points_and_interval(this, space, &
                                                         number_of_grid_points, interval)
        implicit none
        class(staggered_uniform_grid_2d_type), intent(inout) :: this
        integer(int32), intent(in) :: number_of_grid_points(dim)
        real(real64), intent(in) :: interval(dim)
        type(Cartesian_2d_type), intent(in) :: space

        ! 既知情報の代入
        this%Nx = number_of_grid_points(x_dir_index)
        this%Ny = number_of_grid_points(y_dir_index)
        this%dx = interval(x_dir_index)
        this%dy = interval(y_dir_index)

        this%space = space
        ! space%get_length()と[(Nx-1)*dx,(Ny-1)*dy]との
        ! 整合性は確認しない．

        call this%discretize_space()
    end subroutine construct_by_num_grid_points_and_interval

    !------------------------------------------------------------------!
    !>仮想点の数を含まない，\(x, y\)方向の格子点数を配列で返す．
    function get_number_of_grid_points(this) result(number_of_grid_points)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        integer(int32) :: number_of_grid_points(dim)
            !! \(x, y\)方向の格子点数<br>
            !! `=[x, y]`の順に格納<br>
            !! 仮想点の数は含まない

        number_of_grid_points(x_dir_index) = this%Nx
        number_of_grid_points(y_dir_index) = this%Ny
    end function get_number_of_grid_points

    !>仮想点の数を含まない，\(x, y\)方向の格子点数を個々に取得する．
    subroutine get_number_of_grid_points_to(this, Nx, Ny)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        integer(int32), intent(out) :: Nx
            !! \(x\)方向の格子点数<br>仮想点の数は含まない
        integer(int32), intent(out) :: Ny
            !! \(y\)方向の格子点数<br>仮想点の数は含まない

        Nx = this%Nx
        Ny = this%Ny
    end subroutine get_number_of_grid_points_to

    !------------------------------------------------------------------!
    !>\(x, y\)方向の格子点間隔を配列で返す．
    function get_interval(this) result(interval_between_grid_points)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        real(real64) :: interval_between_grid_points(dim)
            !! \(x, y\)方向の格子点間隔<br>
            !! `=[x, y]`の順に格納

        interval_between_grid_points(x_dir_index) = this%dx
        interval_between_grid_points(y_dir_index) = this%dy
    end function get_interval

    !>\(x, y\)方向の格子点間隔を個々に取得する．
    subroutine get_interval_to(this, dx, dy)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        real(real64), intent(out) :: dx
            !! \(x\)方向の格子点間隔
        real(real64), intent(out) :: dy
            !! \(y\)方向の格子点間隔

        dx = this%dx
        dy = this%dy
    end subroutine get_interval_to

    !------------------------------------------------------------------!
    !>仮想点を含まない，\(x, y\)方向の領域長さを配列で返す．
    function get_length(this) result(length)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        real(real64) :: length(dim)
            !! \(x, y\)方向の計算領域長さ<br>
            !! `=[x, y]`の順に格納<br>
            !! 仮想点がある格子の長さは含まない

        length(x_dir_index) = dble(this%Nx-1)*this%dx !&
        length(y_dir_index) = dble(this%Ny-1)*this%dy !&
    end function get_length

    !>仮想点を含まない，\(x, y\)方向の領域長さを個々に取得する．
    subroutine get_length_to(this, Lx, Ly)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        real(real64), intent(out) :: Lx
            !! \(x\)方向の計算領域長さ<br>
            !! 仮想点がある格子の長さは含まない
        real(real64), intent(out) :: Ly
            !! \(y\)方向の計算領域長さ<br>
            !! 仮想点がある格子の長さは含まない

        Lx = dble(this%Nx-1)*this%dx !&
        Ly = dble(this%Ny-1)*this%dy !&
    end subroutine get_length_to

    !------------------------------------------------------------------!
    !>仮想点を含まない，\(x, y\)方向のセル数を配列で返す．
    function get_number_of_grid_center(this) result(number_of_grid_center)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        integer(int32) :: number_of_grid_center(dim)
            !! \(x, y\)方向のセル数<br>
            !! `=[x, y]`の順に格納<br>
            !! 仮想点があるセルは含まない

        number_of_grid_center(x_dir_index) = this%Ncx
        number_of_grid_center(y_dir_index) = this%Ncy
    end function get_number_of_grid_center

    !>仮想点の数を含まない，\(x, y\)方向のセル数を個々に取得する．
    subroutine get_number_of_grid_center_to(this, Ncx, Ncy)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        integer(int32), intent(out) :: Ncx
            !! \(x\)方向のセル数<br>仮想点があるセルは含まない
        integer(int32), intent(out) :: Ncy
            !! \(y\)方向のセル数<br>仮想点があるセルは含まない

        Ncx = this%Ncx
        Ncy = this%Ncy
    end subroutine get_number_of_grid_center_to

    !------------------------------------------------------------------!
    !>仮想点数を返す．
    function get_number_of_virtual_points(this) result(number_of_virtual_points)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        integer(int32) :: number_of_virtual_points
            !! 仮想点数

        number_of_virtual_points = this%Nv
    end function get_number_of_virtual_points

    !------------------------------------------------------------------!
    !>仮想点を含む格子点数の範囲を配列で返す．
    function get_grid_point_range(this) result(point_range)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        integer(int32) :: point_range(dim*2)
            !! 仮想点を含む格子点数の範囲<br>
            !! `=[x_min, y_min, x_max, y_max]`

        point_range(x_min_index) = 1 - this%Nv
        point_range(y_min_index) = 1 - this%Nv
        point_range(x_max_index) = this%Nx + this%Nv
        point_range(y_max_index) = this%Ny + this%Nv
    end function get_grid_point_range

    !>仮想点を含むセル数の範囲を配列で返す．
    function get_grid_center_range(this) result(center_range)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        integer(int32) :: center_range(dim*2)
            !! 仮想点を含むセル数の範囲<br>
            !! `=[x_min, y_min, x_max, y_max]`

        center_range(x_min_index) = 1 - this%Nv
        center_range(y_min_index) = 1 - this%Nv
        center_range(x_max_index) = this%Ncx + this%Nv
        center_range(y_max_index) = this%Ncy + this%Nv
    end function get_grid_center_range

    !------------------------------------------------------------------!
    !>スカラ量が定義されている定義点番号の範囲を配列で返す．
    function get_scalar_range(this) result(scalar_range)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        integer(int32) :: scalar_range(dim*2)
            !! スカラ量が定義されている定義点番号の範囲<br>
            !! `=[x_min, y_min, x_max, y_max]`

        ! スカラ量はセル中心に定義されるので，
        ! 定義点の範囲はセルの範囲と同じ
        scalar_range = this%get_grid_center_range()
    end function get_scalar_range

    !>ベクトル量が定義されている定義点番号の範囲を配列で返す．
    function get_vector_range(this) result(vector_range)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        integer(int32) :: vector_range(dim, dim*2)
            !! ベクトル量が定義されている定義点番号の範囲<br>
            !! `vector_range(方向成分=[x,y]，各軸方向の範囲=[x_min, y_min, x_max, y_max])`

        ! x方向成分は，x軸に対して垂直方向の面の中点に定義されるので
        ! 定義点番号は，x軸は格子点，y軸はセル中心と一致する
        vector_range(x_dir_index, x_min_index) = 1 - this%Nv
        vector_range(x_dir_index, y_min_index) = 1 - this%Nv
        vector_range(x_dir_index, x_max_index) = this%Nx + this%Nv
        vector_range(x_dir_index, y_max_index) = this%Ncy + this%Nv

        ! y方向成分は，y軸に対して垂直方向の面の中点に定義されるので
        ! 定義点番号は，x軸はセル中心，y軸は格子点と一致する
        vector_range(y_dir_index, x_min_index) = 1 - this%Nv
        vector_range(y_dir_index, y_min_index) = 1 - this%Nv
        vector_range(y_dir_index, x_max_index) = this%Ncx + this%Nv
        vector_range(y_dir_index, y_max_index) = this%Ny + this%Nv
    end function get_vector_range

    !>テンソル量が定義されている定義点番号の範囲を配列で返す．
    function get_tensor_range(this) result(tensor_range)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(in) :: this
            !! 当該実体仮引数

        integer(int32) :: tensor_range(dim*2, dim*2)
            !! テンソル量が定義されている定義点番号の範囲<br>
            !! `tensor_range(方向成分=[xx,xy,yx,yy]，各軸方向の範囲=[x_min, y_min, x_max, y_max])`

        tensor_range(xx_index, x_min_index) = 1 - this%Nv
        tensor_range(xx_index, y_min_index) = 1 - this%Nv
        tensor_range(xx_index, x_max_index) = this%Ncx + this%Nv
        tensor_range(xx_index, y_max_index) = this%Ncy + this%Nv

        tensor_range(xy_index, x_min_index) = 1 - this%Nv
        tensor_range(xy_index, y_min_index) = 1 - this%Nv
        tensor_range(xy_index, x_max_index) = this%Nx + this%Nv
        tensor_range(xy_index, y_max_index) = this%Ny + this%Nv

        tensor_range(yx_index, x_min_index) = 1 - this%Nv
        tensor_range(yx_index, y_min_index) = 1 - this%Nv
        tensor_range(yx_index, x_max_index) = this%Nx + this%Nv
        tensor_range(yx_index, y_max_index) = this%Ny + this%Nv

        tensor_range(yy_index, x_min_index) = 1 - this%Nv
        tensor_range(yy_index, y_min_index) = 1 - this%Nv
        tensor_range(yy_index, x_max_index) = this%Ncx + this%Nv
        tensor_range(yy_index, y_max_index) = this%Ncy + this%Nv
    end function get_tensor_range

    !------------------------------------------------------------------!
    !>設定された情報に基づいて，空間離散化を実行する．
    !>具体的には，格子点座標値，セル中心座標値を計算する．
    subroutine discretize_space(this)
        implicit none

        class(staggered_uniform_grid_2d_type), intent(inout) :: this
            !! 当該実体仮引数

        integer(int32) :: num_points(dim), is, ie, js, je
        real(real64) :: grid_interval(dim), min_coord_val(dim)
        integer(int32) :: bounds(dim*2)

        ! 格子点座標 x, yを計算

        ! 軸の最小値（仮想点を含む），格子点間隔，格子点数を取得
        block
            real(real64) :: coord_val(dim*2)
            coord_val = this%space%get_coordinate_values()

            min_coord_val(x_dir_index) = coord_val(x_min_index)
            min_coord_val(y_dir_index) = coord_val(y_min_index)
        end block
        grid_interval(:) = this%get_interval()
        num_points(:) = this%get_number_of_grid_points()

        ! 範囲の最小値
        ! 格子点，セル中心で共通
        is = 1 - this%Nv
        js = 1 - this%Nv

        grid_point: block
            ie = num_points(x_dir_index) + this%Nv
            je = num_points(y_dir_index) + this%Nv
            if (allocated(this%x)) deallocate (this%x)
            if (allocated(this%y)) deallocate (this%y)
            allocate (this%x(is:ie))
            allocate (this%y(js:je))

            bounds(x_min_index) = is; bounds(x_max_index) = ie
            bounds(y_min_index) = js; bounds(y_max_index) = je
            call compute_coordinate_values(bounds, &
                                           min_coord_val, &
                                           grid_interval, &
                                           this%x, this%y)
        end block grid_point

        cell_center: block
            this%Ncx = num_points(x_dir_index) - 1
            this%Ncy = num_points(y_dir_index) - 1
            ie = this%Ncx + this%Nv
            je = this%Ncy + this%Nv

            if (allocated(this%xc)) deallocate (this%xc)
            if (allocated(this%yc)) deallocate (this%yc)
            allocate (this%xc(is:ie))
            allocate (this%yc(js:je))

            bounds(x_min_index) = is; bounds(x_max_index) = ie
            bounds(y_min_index) = js; bounds(y_max_index) = je
            call compute_coordinate_values(bounds, &
                                           min_coord_val + grid_interval/2d0, &
                                           grid_interval, &
                                           this%xc, this%yc)
        end block cell_center

    contains
        subroutine compute_coordinate_values(bnds, min_vals, intervals, x, y)
            implicit none
            !&<
            integer(int32)  , intent(in)    :: bnds(dim*2)
                !! 配列の上下限 `=[x_min, y_min, x_max, y_max]`
            real(real64)    , intent(in)    :: min_vals(dim)
                !! 各軸の座標値の最小値 `=[min_val_x, min_val_y]`
            real(real64)    , intent(in)    :: intervals(dim)
                !! 各軸の格子点間隔 `=[interval_x, interval_y]`
            real(real64)    , intent(inout) :: x(bnds(x_min_index):bnds(x_max_index))
                !! x軸の座標値
            real(real64)    , intent(inout) :: y(bnds(y_min_index):bnds(y_max_index))
                !! y軸の座標値
            !&>

            x_dir: block
                integer(int32) :: i
                !
                !              dx
                !   x_min   |<--->|           x_i = (i-1)dx+x_min
                !     o-----o-----o-----o-----o...
                !  i= 1     2     3     4     5
                !
                do i = bnds(x_min_index), bnds(x_max_index)
                    x(i) = dble(i - 1)*intervals(x_dir_index) + min_vals(x_dir_index)
                end do
            end block x_dir

            y_dir: block
                integer(int32) :: j
                do j = bnds(y_min_index), bnds(y_max_index)
                    y(j) = dble(j - 1)*intervals(y_dir_index) + min_vals(y_dir_index)
                end do
            end block y_dir
        end subroutine compute_coordinate_values
    end subroutine discretize_space
end module grid_uniform_stg_2d
