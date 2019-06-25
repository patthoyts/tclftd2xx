#.rst:
# FindLibFtd2xx
# ---------------------
#
# Find FTD2xx library and headers
#
# ::
#
#   LIBFTD2XX_INCLUDE - Include path for ftd2xx.h
#   LIBFTD2XX_LIBRARY - Library to link to.
#

if(CMAKE_CL_64)
  set(LIBFTD2XX_PLATFORM x64)
else()
  set(LIBFTD2CC_PLATFORM i386)
endif()

find_path(LIBFTD2XX_INCLUDE
  NAMES ftd2xx.h
  HINTS ${FTDI_DIR}
)

find_library(LIBFTD2XX_LIBRARY
  NAMES ftd2xx
  HINTS ${FTDI_DIR}
  PATH_SUFFIXES ${LIBFTD2CC_PLATFORM}
)

mark_as_advanced(LIBFTD2XX_PLATFORM)

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(LibFtd2xx DEFAULT_MSG LIBFTD2XX_LIBRARY LIBFTD2XX_INCLUDE)
