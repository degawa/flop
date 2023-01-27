!>物理量の出力に関係する派生型を提供する．
!>
!>派生型には，スカラ量をVTR形式で出力する出力子型が含まれる．
!>
!>@note
!>メインルーチンの中では明確に型宣言されない．
!>演算に使われる他の型の成分として宣言，演算子に渡す値として生成，
!>演算の結果として中間的に生成される．
!>@endnote
!>
module grid_uniform_stg_op_io_vars_scalar_writer_vtr
    use, intrinsic :: iso_fortran_env
    use :: grid_uniform_stg_op_io_vars_scalar_writer_adt
    use :: grid_uniform_stg_vars_scalar_2d
    implicit none
    private
    public :: output

    !>スカラ量をVTR形式で出力する出力子型．
    type, public, extends(scalar_2d_writer_atype) :: scalar_2d_vtr_writer_type
    end type scalar_2d_vtr_writer_type

    !>サブルーチン`output`を設定するインタフェース．
    interface output
        procedure :: output_scalar_2d_as_vtr
    end interface

contains
    !>スカラ量出力子内のスカラ量をVTR形式で出力する．
    subroutine write_scalar_2d_as_vtr(scalar_2d_vtr_writer, &
                                      unit, &
                                      io_status, io_message)
        implicit none
        !&<
        class(scalar_2d_vtr_writer_type), intent(in)    :: scalar_2d_vtr_writer
            !! VTR writer
        integer(int32)                  , intent(in)    :: unit
            !! 出力先装置番号
        integer(int32)                  , intent(out)   :: io_status
            !! 出力結果の状態（符号）
        character(*)                    , intent(inout) :: io_message
            !! 出力結果の状態（文字列）
        !&>

        ! 装置が開かれていない場合は，非ゼロの状態符号と
        ! 対応するメッセージを返す．
        ! ユーザ定義派生型IOとしてwrite文をオーバーロードする場合は不要だが，
        ! モジュール内の手続からサブルーチンとして呼ばれる場合を想定して実装．
        block
            logical :: is_unit_opened
            inquire (unit=unit, opened=is_unit_opened)
            if (.not. is_unit_opened) then
                io_status = -1
                io_message = 'output unit is not opened yet.'
                return
            end if
        end block

        block
            use :: grid_uniform_stg_2d
            integer(int32) :: Ncx, Ncy
            type(staggered_uniform_grid_2d_type), pointer :: grid
            character(256) :: filename

            ! 装置番号からファイル名"*.vtr"を取得
            inquire (unit, name=filename)

            ! vtk fortranの中で別途装置が開かれるので，
            ! 開かれている装置を閉じる
            ! 閉じないと権限に関係してエラーが出る
            close (unit)

            grid => scalar_2d_vtr_writer%scr%get_base_grid()
            call grid%get_number_of_grid_center_to(Ncx, Ncy)

            call output_scalar2d_vtr(trim(filename), scalar_2d_vtr_writer%scr%val(1:Ncx, 1:Ncy), &
                                     grid%xc(1:Ncx), grid%yc(1:Ncy), [1, 1], [Ncx, Ncy])
        end block
        io_status = 0
        io_message = ""
    contains
        subroutine output_scalar2d_vtr(filename, u, x, y, lbnd, ubnd)
            use :: vtk_fortran
            use :: space_vars_Cartesian, only: &
                x_dir => x_dir_index, y_dir => y_dir_index
            implicit none
            !&<
            character(*)    , intent(in)    :: filename
                !! 出力ファイル名
            integer(int32)  , intent(in)    :: lbnd(x_dir:y_dir)
                !! 出力するスカラ量の下限
            integer(int32)  , intent(in)    :: ubnd(x_dir:y_dir)
                !! 出力するスカラ量の上限
            real(real64)    , intent(in)    :: u(lbnd(x_dir):ubnd(x_dir), lbnd(y_dir):ubnd(y_dir))
                !! 出力するスカラ量
            real(real64)    , intent(in)    :: x(lbnd(x_dir):ubnd(x_dir))
                !! 出力するスカラ量の\(x\)座標
            real(real64)    , intent(in)    :: y(lbnd(y_dir):ubnd(y_dir))
                !! 出力するスカラ量の\(y\)座標
            !&>

            type(vtk_file) :: vtr
            integer(int32) :: stat

            ! ファイル情報 open
            stat = vtr%initialize(format="raw", &
                                  filename=filename, &
                                  mesh_topology="RectilinearGrid", &
                                  nx1=lbnd(x_dir), nx2=ubnd(x_dir), &
                                  ny1=lbnd(y_dir), ny2=ubnd(y_dir))

            if (stat == 0) then
                ! 配列の範囲の指定 open
                stat = vtr%xml_writer%write_piece(nx1=lbnd(x_dir), nx2=ubnd(x_dir), &
                                                  ny1=lbnd(y_dir), ny2=ubnd(y_dir))

                ! 各方向座標値の書き出し open-close
                stat = vtr%xml_writer%write_geo(x=x, y=y)

                ! 配列の書き出し
                stat = vtr%xml_writer%write_dataarray(location="node", action="open")
                stat = vtr%xml_writer%write_dataarray(x=u, data_name="scalar2d", one_component=.true.)
                stat = vtr%xml_writer%write_dataarray(location="node", action="close")

                ! 配列範囲の指定 close
                stat = vtr%xml_writer%write_piece()

                !ファイル情報 close
                stat = vtr%finalize()
            else
                print *, "error status = ", stat
                print *, "error message = ", vtr%get_error_message()
            end if
        end subroutine output_scalar2d_vtr
    end subroutine write_scalar_2d_as_vtr

    !>スカラ量出力子内のスカラ量をVTR形式で出力する．
    !>
    !>内部で`write_scalar_2d_as_vtr`を呼び出している．
    subroutine output_scalar_2d_as_vtr(scalar_2d_vtr_writer)
        implicit none
        type(scalar_2d_vtr_writer_type), intent(in) :: scalar_2d_vtr_writer
            !! VTR writer

        integer(int32) :: unit
            !! 出力先装置番号
        integer(int32) :: io_status
            !! 出力結果の状態（符号）
        character(256) :: io_message
            !! 出力結果の状態（文字列）

        unit = scalar_2d_vtr_writer%unit_number

        call write_scalar_2d_as_vtr(scalar_2d_vtr_writer, &
                                    unit, &
                                    io_status, io_message)

        ! unit `unit` is closed in write_scalar_2d_as_vtr
    end subroutine output_scalar_2d_as_vtr
end module grid_uniform_stg_op_io_vars_scalar_writer_vtr
