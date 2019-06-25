# README

*ftd2xx* - a Tcl interface to the FTDI D2XX USB device library

This package provides a Tcl interface to the Future Technology Devices
International Ltd. D2XX driver library. This is a commonly used USB
device driver. See http://www.ftdichip.com/ for more details about
their products and the drivers themselves.

The author is not affiliated with Future Technology Devices
International Ltd and this code does not represent the above company
in any way.

The package provides some access to the commands exposed by the D2XX
library and in particular provides a Tcl channel interface to the
device over which you can send and receive data. The channel acts as a
standard Tcl channel and supports fileevents and nonblocking
reads. The device settings can be managed using the standard Tcl
channel configuration configuration command. These include the
timeouts and latency values along with buffersize and blocking mode.

FTDI provide a Linux driver for these devices and whilst this has not
been tested there is no intrinsic reason why this package should not
be easily ported to operate on that platform.


## COMMANDS

### ftd2xx open ?-serial? ?-description? ?-location? name

  open the named device and create a channel for it. The driver
  supports naming devices using one of the serial number, a
  descriptive device name or on windows a location (port number).
  See the `ftd2xx list` command to obtain a list of attached devices
  with their names and locations.

### ftd2xx list

  list all the supported devices currently connected. Each list
  element is itself a name-value list providing all the information
  available on the device including the serial number, location,
  description, device id, device handle and status.

### ftd2xx reset channel

  this command resets the device identified by the given channel.
  It is likely that after this call the channel will need to be
  closed and the device re-opened. (untested)

### ftd2xx purge channel

  purge the device buffers for the device identified by the channel

### ftd2xx version

  return the FTDI library version (eg: 3.1.16)


## INSTALLATION

The FTDI D2XX driver package should be downloaded from the website
(http://www.ftdichip.com/Drivers/D2XX.htm). Set the `FTDI_DIR` variable
when calling CMake to point to the driver directory that
contains the ftd2xx.h header.

    cmake -S. -Bbuild -DFTDI_DIR=..\ftd2xx-2.12.28 .
    cmake --build build --config Release

If the ftd2xx.dll is not installed already, it should be manually
copied to the installation folder (eg: `c:\tcl\lib\tclftd2xx`). It
is usually installed into the system32 folder when the device driver
is installed once you have plugged in a compatible device.

You may have to install drivers for your device (ftdibus.sys or
ftd2xx.sys). This should be provided by the device manufacturer as the
driver .INF file needs to be specifically configured for each device
type.
