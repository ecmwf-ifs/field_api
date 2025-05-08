PROGRAM write_hdf5
  USE hdf5
  IMPLICIT NONE
  INTEGER(HID_T) :: file_id, dataspace_id, dataset_id
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  REAL(8), DIMENSION(:), ALLOCATABLE :: data
  INTEGER :: error

  CALL h5open_f(error)
  CALL h5fcreate_f("test.h5", H5F_ACC_TRUNC_F, file_id, error)

  dims = (/100/)
  ALLOCATE(data(100))
  data = 3.1415D0

  CALL h5screate_simple_f(1, dims, dataspace_id, error)
  CALL h5dcreate_f(file_id, "dataset", H5T_NATIVE_DOUBLE, dataspace_id, dataset_id, error)
  CALL h5dwrite_f(dataset_id, H5T_NATIVE_DOUBLE, data, dims, error)

  CALL h5dclose_f(dataset_id, error)
  CALL h5sclose_f(dataspace_id, error)
  CALL h5fclose_f(file_id, error)
  CALL h5close_f(error)

END PROGRAM write_hdf5
