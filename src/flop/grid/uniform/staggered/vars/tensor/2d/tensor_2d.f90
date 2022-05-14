!>テンソル量に関する型を提供する．
!>
!>定義される型には，Staggered格子[[staggered_uniform_grid_2d_type]]上で
!>定義されるテンソル量を取り扱う型が含まれる．
!>
module grid_uniform_staggered_vars_tensor_2d
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_staggered_2d
    implicit none
    private
    public :: tensor_2d_type

    !>2次元Staggered格子上で定義されるテンソル量型．
    type :: tensor_2d_type
        type(staggered_uniform_grid_2d_type), private, pointer :: grid
            !! テンソル量が定義されている格子の情報<br>
            !! `base_grid`として参照
        real(real64), allocatable, public :: xx(:, :)
           !! テンソル量の\(xx\)成分を格納する配列
        real(real64), allocatable, public :: xy(:, :)
           !! テンソル量の\(xy\)成分を格納する配列
        real(real64), allocatable, public :: yx(:, :)
           !! テンソル量の\(yx\)成分を格納する配列
        real(real64), allocatable, public :: yy(:, :)
           !! テンソル量の\(yy\)成分を格納する配列
    contains
        !&<
        procedure, public, pass :: construct_by_base_grid
            !! 成分`grid`に基づいて配列を割り付け
        procedure, public, pass :: construct_by_grid_pointer
            !! 格子の情報に基づいて配列を割り付け
        generic :: construct => construct_by_base_grid, &
                                construct_by_grid_pointer
        procedure, public, pass :: destruct
            !! 割り付けた配列を解放
        final :: finalize
            !! 後始末手続
        !&>

        procedure, public, pass :: get_base_grid
            !! `tensor_2d_type`に関連付けられている
            !! 格子へのポインタを返却
    end type tensor_2d_type
contains
    !>成分の`grid`の情報に基づいて，`tensor_2d_type`を構築する．
    !>
    !>成分の配列を動的に割り付ける．
    subroutine construct_by_base_grid(this)
        use :: space_vars_Cartesian, &
            xx => xx_index, xy => xy_index, &
            yx => yx_index, yy => yy_index, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none
        !&<
        class(tensor_2d_type), intent(inout) :: this
            !! 構築される`tensor_2d_type`<br>
            !! 当該実体仮引数
        !&>

        integer(int32) :: is, ie, js, je
        integer(int32) :: tensor_range(4, 4)
        tensor_range = this%grid%get_tensor_range()

        is = tensor_range(xx, x_min)
        ie = tensor_range(xx, x_max)
        js = tensor_range(xx, y_min)
        je = tensor_range(xx, y_max)
        allocate (this%xx(is:ie, js:je), source=0d0)

        is = tensor_range(xy, x_min)
        ie = tensor_range(xy, x_max)
        js = tensor_range(xy, y_min)
        je = tensor_range(xy, y_max)
        allocate (this%xy(is:ie, js:je), source=0d0)

        is = tensor_range(yx, x_min)
        ie = tensor_range(yx, x_max)
        js = tensor_range(yx, y_min)
        je = tensor_range(yx, y_max)
        allocate (this%yx(is:ie, js:je), source=0d0)

        is = tensor_range(yy, x_min)
        ie = tensor_range(yy, x_max)
        js = tensor_range(yy, y_min)
        je = tensor_range(yy, y_max)
        allocate (this%yy(is:ie, js:je), source=0d0)
    end subroutine construct_by_base_grid

    !>渡された格子（ポインタ）の情報に基づいて，`tensor_2d_type`を構築する．
    !>
    !>格子を`tensor_2d_type`のbase_gridとして関連付け，
    !>成分の配列を動的に割り付ける．
    !>
    !>@note テンソル量は，明示的に変数として宣言しないので，実体を受け取る手続は不要．
    !>
    subroutine construct_by_grid_pointer(this, grid)
        use :: space_vars_Cartesian, &
            xx => xx_index, xy => xy_index, &
            yx => yx_index, yy => yy_index, &
            x_min => x_min_index, x_max => x_max_index, &
            y_min => y_min_index, y_max => y_max_index
        implicit none
        !&<
        class(tensor_2d_type)               , intent(inout)             :: this
            !! 構築される`tensor_2d_type`<br>
            !! 当該実体仮引数
        type(staggered_uniform_grid_2d_type), intent(in)    , pointer   :: grid
            !! `tensor_2d_type`が定義される格子への**ポインタ**
        !&>

        ! bese_gridと格子を関連付け
        this%grid => grid ! `=`ではない
        ! 実際の割付は委譲
        call this%construct()
    end subroutine construct_by_grid_pointer

    !>構築した`tensor_2d_type`を破棄する．
    !>
    !>成分の配列を解放し，base_gridとの関連を絶つ．
    subroutine destruct(this)
        implicit none

        class(tensor_2d_type), intent(inout) :: this

        if (allocated(this%xx)) deallocate (this%xx)
        if (allocated(this%xy)) deallocate (this%xy)
        if (allocated(this%yx)) deallocate (this%yx)
        if (allocated(this%yy)) deallocate (this%yy)
        this%grid => null()
    end subroutine destruct

    !>動的に割り付けられた`tensor_2d_type`を始末する．
    !>
    !>成分の配列を解放し，base_gridとの関連を絶つ．
    subroutine finalize(this)
        implicit none
        type(tensor_2d_type), intent(inout) :: this
            !! 破棄される`tensor_2d_type`<br>
            !! 当該実体仮引数

        call this%destruct()
    end subroutine finalize

    !>テンソル量が定義される格子へのポインタを返す．
    function get_base_grid(this) result(grid)
        implicit none

        class(tensor_2d_type), intent(in) :: this
            !! 格子を取得するテンソル量<br>
            !! 当該実体仮引数

        type(staggered_uniform_grid_2d_type), pointer :: grid
            !! テンソル量が定義されている格子へのポインタ

        grid => this%grid
    end function get_base_grid
end module grid_uniform_staggered_vars_tensor_2d
