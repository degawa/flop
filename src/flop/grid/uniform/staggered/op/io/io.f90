!>物理量のIOに関わる型と演算子を一括して`use`するためのモジュール
module grid_uniform_stg_op_io
    ! supported format
    use :: grid_uniform_stg_op_io_vars_format
    ! write
    use :: grid_uniform_stg_op_io_vars_vector_writer_csv
    use :: grid_uniform_stg_op_io_vars_scalar_writer_csv
    use :: grid_uniform_stg_op_io_vars_vector_writer_vtr
    use :: grid_uniform_stg_op_io_vars_scalar_writer_vtr
    use :: grid_uniform_stg_op_io_vars_scalar_writer_npy
    use :: grid_uniform_stg_op_io_vars_vector_writer_npy
    use :: grid_uniform_stg_op_io_as
    use :: grid_uniform_stg_op_io_to
    public
end module grid_uniform_stg_op_io
