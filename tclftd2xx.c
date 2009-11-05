/* tclftd2xx.c - 
 *
 *   Copyright (C) 2008 Pat Thoyts <patthoyts@users.sourceforge.net>
 *   Copyright (C) 2008 Leopold Gerlinger
 *   Copyright (C) 2009 Lars Unger
 *
 *	FTDI D2XX USB Device driver Tcl interface.
 *
 * ----------------------------------------------------------------------
 *	See the accompanying file 'licence.terms' for the software license.
 *	In essence - this is MIT licensed code.
 * ----------------------------------------------------------------------
 */

#define STRICT
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <tcl.h>
#include <errno.h>
#include "ftd2xx.h"

typedef FT_STATUS (WINAPI FT_CloseProc)(FT_HANDLE);
typedef FT_STATUS (WINAPI FT_CreateDeviceInfoListProc)(LPDWORD);
typedef FT_STATUS (WINAPI FT_GetDeviceInfoListProc)
    (FT_DEVICE_LIST_INFO_NODE*,LPDWORD);
typedef FT_STATUS (WINAPI FT_GetLatencyTimerProc)(FT_HANDLE,PUCHAR);
typedef FT_STATUS (WINAPI FT_GetBitModeProc)(FT_HANDLE,PUCHAR);
typedef FT_STATUS (WINAPI FT_GetStatusProc)(FT_HANDLE,LPDWORD,LPDWORD,LPDWORD);
typedef FT_STATUS (WINAPI FT_OpenExProc)(PVOID,DWORD,FT_HANDLE*);
typedef FT_STATUS (WINAPI FT_PurgeProc)(FT_HANDLE,ULONG);
typedef FT_STATUS (WINAPI FT_ReadProc)(FT_HANDLE,LPVOID,DWORD,LPDWORD);
typedef FT_STATUS (WINAPI FT_ResetPortProc)(FT_HANDLE);
typedef FT_STATUS (WINAPI FT_SetEventNotificationProc)(FT_HANDLE,DWORD,PVOID);
typedef FT_STATUS (WINAPI FT_SetLatencyTimerProc)(FT_HANDLE,UCHAR);
typedef FT_STATUS (WINAPI FT_SetBitModeProc)(FT_HANDLE,UCHAR,UCHAR);
typedef FT_STATUS (WINAPI FT_SetTimeoutsProc)(FT_HANDLE,ULONG,ULONG);
typedef FT_STATUS (WINAPI FT_WriteProc)(FT_HANDLE,LPVOID,DWORD,LPDWORD);
typedef FT_STATUS (WINAPI FT_GetLibraryVersionProc)(LPDWORD);
typedef FT_STATUS (WINAPI FT_RescanProc)(VOID);
typedef FT_STATUS (WINAPI FT_SetBaudRateProc)(FT_HANDLE,ULONG);
typedef FT_STATUS (WINAPI FT_SetDataCharacteristicsProc)
    (FT_HANDLE,UCHAR,UCHAR,UCHAR);
typedef FT_STATUS (WINAPI FT_SetFlowControlProc)(FT_HANDLE,USHORT,UCHAR,UCHAR);

typedef struct FTDIPROCS {
    FT_CloseProc *FT_Close;
    FT_CreateDeviceInfoListProc *FT_CreateDeviceInfoList;
    FT_GetDeviceInfoListProc *FT_GetDeviceInfoList;
    FT_GetLatencyTimerProc *FT_GetLatencyTimer;
    FT_GetBitModeProc *FT_GetBitMode;
    FT_GetStatusProc *FT_GetStatus;
    FT_OpenExProc *FT_OpenEx;
    FT_PurgeProc *FT_Purge;
    FT_ReadProc *FT_Read;
    FT_ResetPortProc *FT_ResetPort;
    FT_SetEventNotificationProc *FT_SetEventNotification;
    FT_SetLatencyTimerProc *FT_SetLatencyTimer;
    FT_SetBitModeProc *FT_SetBitMode;
    FT_SetTimeoutsProc *FT_SetTimeouts;
    FT_WriteProc *FT_Write;
    FT_GetLibraryVersionProc *FT_GetLibraryVersion;
    FT_RescanProc *FT_Rescan;
    FT_SetBaudRateProc *FT_SetBaudRate;
    FT_SetDataCharacteristicsProc *FT_SetDataCharacteristics;
    FT_SetFlowControlProc *FT_SetFlowControl;
} FTDIPROCS;

static FTDIPROCS procs;

#define FTD2XX_ASYNC   (1<<1)
#define FTD2XX_PENDING (1<<2)

struct Package;

typedef struct Channel {
    Tcl_Channel channel;
    struct Channel *nextPtr;
    struct Package *pkgPtr;
    int flags;
    int watchmask;
    int validmask;
    int baudrate;
    unsigned char parity;
    unsigned short handshake;
    char xonchar;
    char xoffchar;
    unsigned char databits;
    unsigned char stopbits;
    unsigned long rxtimeout;
    unsigned long txtimeout;
    FT_HANDLE handle;
    HANDLE event;
} Channel;

typedef struct ChannelEvent {
    Tcl_Event header;
    Channel *instPtr;
    int flags;
} ChannelEvent;

typedef struct Package {
    struct Channel *headPtr;
    unsigned long count;
    unsigned long uid;
    HMODULE hFtdi;
} Package;

static const char *ConvertError(FT_STATUS fts);

static Tcl_DriverCloseProc ChannelClose;
static Tcl_DriverInputProc ChannelInput;
static Tcl_DriverOutputProc ChannelOutput;
static Tcl_DriverSetOptionProc ChannelSetOption;
static Tcl_DriverGetOptionProc ChannelGetOption;
static Tcl_DriverWatchProc ChannelWatch;
static Tcl_DriverGetHandleProc ChannelGetHandle;
static Tcl_DriverBlockModeProc ChannelBlockMode;

static Tcl_ChannelType Ftd2xxChannelType = {
    "ftd2xx",
    (Tcl_ChannelTypeVersion)TCL_CHANNEL_VERSION_3,
    ChannelClose,
    ChannelInput,
    ChannelOutput,
    NULL /*ChannelSeek*/,
    ChannelSetOption,
    ChannelGetOption,
    ChannelWatch,
    ChannelGetHandle,
    NULL /*ChannelClose2*/,
    ChannelBlockMode,
    NULL /*ChannelFlush*/,
    NULL /*ChannelHandler*/,
    NULL /*ChannelWideSeek*/
};

/**
 * Debug tracing.
 */

#if defined(DEBUG) || defined(_DEBUG)
#define TRACE LocalTrace
#else
#define TRACE 1 ? ((void)0) : LocalTrace
#endif

static void
LocalTrace(const char *format, ...)
{
    va_list args;
    static char buffer[1024];

    va_start(args, format);
    _vsnprintf(buffer, 1023, format, args);
    OutputDebugStringA(buffer);
    va_end(args);
}

/**
 * Close the channel and clean up all allocated resources. This requires
 * removing the channel from the linked list (hence we need some way to
 * access the head of the list which is in the Package structure).
 * This function is called either from an explicit 'close' call from script
 * or when the interpreter is deleted.
 */

static int
ChannelClose(ClientData instance, Tcl_Interp *interp)
{
    Channel *instPtr = instance;
    Package *pkgPtr = instPtr->pkgPtr;
    Channel **tmpPtrPtr;
    int r = TCL_OK;
    FT_STATUS fts;

    TRACE("ChannelClose\n");
    CloseHandle(instPtr->event);
    if ((fts = procs.FT_Purge(instPtr->handle, FT_PURGE_RX | FT_PURGE_TX)) != FT_OK) {
	TRACE("ChannelClose error: %s", ConvertError(fts));
    }
    fts = procs.FT_Close(instPtr->handle);
    if (fts != FT_OK) {
	Tcl_AppendResult(interp, "error closing \"",
			 Tcl_GetChannelName(instPtr->channel), "\": ",
			 ConvertError(fts), NULL);
	r = TCL_ERROR;
    }
    /* remove this channel from the package list */
    tmpPtrPtr = &pkgPtr->headPtr;
    while (*tmpPtrPtr && *tmpPtrPtr != instPtr) {
	tmpPtrPtr = &(*tmpPtrPtr)->nextPtr;
    }
    *tmpPtrPtr = instPtr->nextPtr;
    --pkgPtr->count;
    ckfree((char *)instPtr);
    return r;
}

/**
 * Read data from the device. We support non-blocking reads by checking the 
 * amount available in the receive queue. Note that the FTD2XX devices implement
 * a read timeout (which we may set via fconfigure) and the blocking read will
 * terminate when the timeout triggers anyway.
 * If the device is disconnected then we will get a read error.
 */

static int
ChannelInput(ClientData instance, char *buffer, int toRead, int *errorCodePtr)
{
    Channel *instPtr = instance;
    DWORD cbRead = 0;
    FT_STATUS fts = FT_OK;

    if (instPtr->flags & FTD2XX_ASYNC) {
	/*
	 * Non-blocking: only read data that is available
	 */
	DWORD rx = 0, tx = 0, ev = 0;
	if ((fts = procs.FT_GetStatus(instPtr->handle, &rx, &tx, &ev)) == FT_OK) {
	    if ((int)rx < toRead) {
		toRead = rx;
	    }
	} else {
	    TRACE(ConvertError(fts));
	}
    }
    if (procs.FT_Read(instPtr->handle, buffer, toRead, &cbRead) != FT_OK) {
	TRACE("ChannelInput error: %s", ConvertError(fts));
	switch (fts) {
	    case FT_DEVICE_NOT_FOUND: *errorCodePtr = ENODEV; break;
	    default: *errorCodePtr = EINVAL; break;
	}
	cbRead = -1;
    }
    return (int)cbRead;
}

/**
 * Write to the device. We don't have any non-blocking handling for write as it
 * isnt obvious how to do this. However the devices implement a write timeout 
 * which likely cause us to return and retry.
 * If the device is disconnected we will get an error.
 */

static int
ChannelOutput(ClientData instance, const char *buffer, int toWrite, int *errorCodePtr)
{
    Channel *instPtr = instance;
    FT_STATUS fts = FT_OK;
    DWORD cbWrote = 0;
    DWORD dwStart = GetTickCount();
    if ((fts = procs.FT_Write(instPtr->handle, (void *)buffer, toWrite, &cbWrote)) != FT_OK) {
	TRACE("ChannelOutput error: %s", ConvertError(fts));
	switch (fts) {
	    case FT_DEVICE_NOT_FOUND: *errorCodePtr = ENODEV; break;
	    default: *errorCodePtr = EINVAL; break;
	}
	cbWrote = -1;
    }
    TRACE("ChannelOutput %lu bytes in %ld ms\n", cbWrote, GetTickCount()-dwStart);
    return (int)cbWrote;
}

/**
 * Implement device control via the Tcl 'fconfigure' command.
 * We can change the timeouts and the latency timer here.
 */

static int
ChannelSetOption(ClientData instance, Tcl_Interp *interp,
		 const char *optionName, const char *newValue)
{
    Channel *instPtr = instance;
    FT_STATUS fts = FT_OK;
    int r = TCL_OK, changeTimeouts = 0;

    if (!strcmp("-readtimeout", optionName)) {
	int tmp = 1;
	r = Tcl_GetInt(interp, newValue, &tmp);
	if (r == TCL_OK) {
	    fts = procs.FT_SetTimeouts(instPtr->handle, (DWORD)tmp, instPtr->txtimeout);
	    if (fts == FT_OK) {
		instPtr->rxtimeout = (unsigned long)tmp;
	    }
	}
    } else if (!strcmp("-writetimeout", optionName)) {
	int tmp = 1;
	r = Tcl_GetInt(interp, newValue, &tmp);
	if (r == TCL_OK) {
	    fts = procs.FT_SetTimeouts(instPtr->handle, instPtr->rxtimeout, (DWORD)tmp);
	    if (fts == FT_OK) {
		instPtr->txtimeout = (unsigned long)tmp;
	    }
	}
    } else if (!strcmp("-latency", optionName)) {
	int tmp = 1;
	r = Tcl_GetInt(interp, newValue, &tmp);
	if (r == TCL_OK) {
	    fts = procs.FT_SetLatencyTimer(instPtr->handle, (UCHAR)tmp);
	}
    } else if (!strcmp("-bitmode", optionName)) {
	int tmp = 1;
	r = Tcl_GetInt(interp, newValue, &tmp);
	if (r == TCL_OK) {
	    fts = procs.FT_SetBitMode(instPtr->handle, (UCHAR)(tmp >> 8), (UCHAR)(tmp & 0xff));
	}
    } else if (!strcmp("-mode", optionName)) {
	int baudrate = 19200, databits = 8, stop = 1;
	char parity = 'n';
	unsigned char wordlen = FT_BITS_8, stopbits = FT_STOP_BITS_1;

	sscanf(newValue, "%d,%c,%d,%d", &baudrate, &parity, &databits, &stop);

	switch (databits) {
	    case 8: wordlen = FT_BITS_8; break;
	    case 7: wordlen = FT_BITS_7; break;
	    case 6: wordlen = FT_BITS_6; break;
	    case 5: wordlen = FT_BITS_5; break;
	    default:
		Tcl_AppendResult(interp, "bad data value \"", newValue, 
		    "\": must be 5, 6, 7 or 8.", NULL);
		return TCL_ERROR;
	}

	switch (stop) {
	    case 1: stopbits = FT_STOP_BITS_1; break;
	    case 2: stopbits = FT_STOP_BITS_2; break;
	    default:
		Tcl_AppendResult(interp, "bad stop value \"", newValue,
		    "\"must be either 1 or 2.", NULL);
		return TCL_ERROR;
	}

	switch (parity) {
	    case 'n': parity = FT_PARITY_NONE; break;
	    case 'o': parity = FT_PARITY_ODD; break;
	    case 'e': parity = FT_PARITY_EVEN; break;
	    case 'm': parity = FT_PARITY_MARK; break;
	    case 's': parity = FT_PARITY_SPACE; break;
	    default:
		Tcl_AppendResult(interp, "bad parity value \"", newValue,
		    "\": must be one of n, o, e, m, or s.", NULL);
		return TCL_ERROR;
	}

	fts = procs.FT_SetBaudRate(instPtr->handle, baudrate);
	if (fts != FT_OK) {
	    Tcl_AppendResult(interp, "failed set baudrate: \"",
			     ConvertError(fts), NULL);
	    return TCL_ERROR;
	}

	fts = procs.FT_SetDataCharacteristics(instPtr->handle, wordlen, stopbits, parity);
	if (fts == FT_OK) {
	    instPtr->baudrate = baudrate;
	    instPtr->databits = wordlen;
	    instPtr->stopbits = stopbits;
	    instPtr->parity = parity;
	}
    } else if (!strcmp("-handshake", optionName)) {
	unsigned short handshake = 0xFFFF;
	if (!strcmp(newValue,"none")) {
	    handshake = FT_FLOW_NONE;
	} else if (!strcmp(newValue,"rtscts")) {
	    handshake = FT_FLOW_RTS_CTS;
	} else if (!strcmp(newValue,"dtrdsr")) {
	    handshake = FT_FLOW_DTR_DSR;
	} else if (!strcmp(newValue,"xonxoff")) {
	    handshake = FT_FLOW_XON_XOFF;
	} else {
	    Tcl_AppendResult(interp, "bad value \"", newValue, "\" for ", optionName,
		": must be one of none, rtscts, dtrdsr or xonxoff", NULL);
	    return TCL_ERROR;
	}
	fts = procs.FT_SetFlowControl(instPtr->handle, handshake, instPtr->xonchar, instPtr->xoffchar);
	if (fts == FT_OK) {
	    instPtr->handshake = handshake;
	}
    } else if (!strcmp("-xchar", optionName)) {
	int xrgc, n;
	char xonxoff[2];
	const char **xrgv;
	if (Tcl_SplitList(interp, newValue, &xrgc, &xrgv) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (xrgc != 2) {
	badxchar:
	    Tcl_AppendResult(interp, "bad value for -xchar: must be a list of two elements", NULL);
	    return TCL_ERROR;
	}
	for (n = 0; n < 2; ++n) {
	    /* check for extended utf-8 and handle like tcl serial channel */
	    if (xrgv[n][0] & 0x80) {
		Tcl_UniChar c;
		int len;
		len = Tcl_UtfToUniChar(xrgv[n], &c);
		if (xrgv[n][len]) {
		    goto badxchar;
		}
		xonxoff[n] = (char)c;
	    } else {
		xonxoff[n] = xrgv[n][0];
	    }
	}

	fts = procs.FT_SetFlowControl(instPtr->handle, instPtr->handshake, xonxoff[0], xonxoff[1]);
	if (fts == FT_OK) {
	    instPtr->xonchar = xonxoff[0];
	    instPtr->xoffchar = xonxoff[1];
	}
    }

    if (fts != FT_OK) {
	Tcl_AppendResult(interp, "error setting ", optionName,
			 ": ", ConvertError(fts), NULL);
	r = TCL_ERROR;
    }

    return r;
}

/**
 * Read the additional channel settings. The timeout values cannot be
 * read from the device so we maintain the values in the channel instance
 * data. The latency can be read back.
 */

static int
ChannelGetOption(ClientData instance, Tcl_Interp *interp, 
		 const char *optionName, Tcl_DString *optionValue)
{
    Channel *instPtr = instance;
    const char *options[] = {"readtimeout", "writetimeout", "latency",
			     "bitmode", "mode", "handshake", "xchar", NULL};
    int r = TCL_OK;

    if (optionName == NULL) {
	Tcl_DString ds;
	const char **p;

	Tcl_DStringInit(&ds);
	for (p = options; *p != NULL; ++p) {
	    char op[16];
	    sprintf(op, "-%s", *p);
	    Tcl_DStringSetLength(&ds, 0);
	    ChannelGetOption(instance, interp, op, &ds);
	    Tcl_DStringAppend(optionValue, " ", 1);
	    Tcl_DStringAppend(optionValue, op, -1);
	    Tcl_DStringAppend(optionValue, " ", 1);
	    Tcl_DStringAppendElement(optionValue, Tcl_DStringValue(&ds));
	}
	Tcl_DStringFree(&ds);
    } else {
	FT_STATUS fts = FT_OK;
	Tcl_DString ds;
	Tcl_DStringInit(&ds);

	if (!strcmp("-readtimeout", optionName)) {
	    Tcl_DStringSetLength(&ds, TCL_INTEGER_SPACE);
	    sprintf(Tcl_DStringValue(&ds), "%lu", instPtr->rxtimeout);
	} else if (!strcmp("-writetimeout", optionName)) {
	    Tcl_DStringSetLength(&ds, TCL_INTEGER_SPACE);
	    sprintf(Tcl_DStringValue(&ds), "%lu", instPtr->txtimeout);
	} else if (!strcmp("-latency", optionName)) {
	    UCHAR timer = 0;
	    Tcl_DStringSetLength(&ds, TCL_INTEGER_SPACE);
	    fts = procs.FT_GetLatencyTimer(instPtr->handle, &timer);
	    if (fts == FT_OK) {
		sprintf(Tcl_DStringValue(&ds), "%d", timer);
	    } else {
		Tcl_AppendResult(interp, "failed to read ", optionName, ": ",
				 ConvertError(fts), NULL);
		r = TCL_ERROR;
	    }
	} else if (!strcmp("-bitmode", optionName)) {
	    UCHAR bmode = 0;
	    fts = procs.FT_GetBitMode(instPtr->handle, &bmode);
	    if (fts == FT_OK) {
		Tcl_DStringSetLength(&ds, TCL_INTEGER_SPACE);
		sprintf(Tcl_DStringValue(&ds), "%d", bmode);
	    } else {
		Tcl_AppendResult(interp, "failed to read ", optionName, ": ",
				 ConvertError(fts), NULL);
		r = TCL_ERROR;
	    }
	} else if (!strcmp("-mode", optionName)) {
	    char parity = 0;
	    switch (instPtr->parity) {
		case FT_PARITY_NONE:  parity = 'n'; break;
		case FT_PARITY_ODD:   parity = 'o'; break;
		case FT_PARITY_EVEN:  parity = 'e'; break;
		case FT_PARITY_MARK:  parity = 'm'; break;
		case FT_PARITY_SPACE: parity = 's'; break;
		default:              parity = '?';
	    }
	    Tcl_DStringSetLength(&ds, TCL_INTEGER_SPACE * 3 + 6);
	    sprintf(Tcl_DStringValue(&ds), "%d,%c,%d,%d",
		    instPtr->baudrate, parity, instPtr->databits,
		    (instPtr->stopbits == FT_STOP_BITS_1) ? 1 : 2);
	} else if (!strcmp("-handshake", optionName)) {
	    switch (instPtr->handshake) {
		case FT_FLOW_NONE:
		    Tcl_DStringAppend(&ds, "none", 4);
		    break;
		case FT_FLOW_RTS_CTS:
		    Tcl_DStringAppend(&ds, "rtscts", 6);
		    break;
		case FT_FLOW_DTR_DSR:
		    Tcl_DStringAppend(&ds, "dtrdsr", 6);
		    break;
		case FT_FLOW_XON_XOFF:
		    Tcl_DStringAppend(&ds, "xonxoff", 7);
		    break;
		default:
		    Tcl_DStringAppend(&ds, "unknown", 7);
	    }
	} else if (!strcmp("-xchar", optionName)) {
	    char cbuf[2] = {0, 0};
	    cbuf[0] = instPtr->xonchar;
	    Tcl_DStringAppendElement(&ds, cbuf);
	    cbuf[0] = instPtr->xoffchar;
	    Tcl_DStringAppendElement(&ds, cbuf);
	} else {
	    const char **p;
	    for (p = options; *p != NULL; ++p) {
		Tcl_DStringAppendElement(&ds, *p);
	    }
	    r = Tcl_BadChannelOption(interp, optionName, Tcl_DStringValue(&ds));
	}

	if (r == TCL_OK) {
	    Tcl_DStringAppend(optionValue, Tcl_DStringValue(&ds), -1);
	}
	Tcl_DStringFree(&ds);
    }

    return r;
}

/**
 * This function is called by Tcl to setup fileevent notifications
 * on this channel. We only really support readable events (our channel
 * type is basically always writable).
 * Our channel state monitoring is actually done via the notifier. All
 * that occurs here is to reduce the blocking time if our channel has
 * readable events configured.
 */

static void
ChannelWatch(ClientData instance, int mask)
{
    Channel *instPtr = instance;
    Tcl_Time blockTime = {0, 10000}; /* 10 msec */
    TRACE("ChannelWatch %s 0x%08x\n",
	  Tcl_GetChannelName(instPtr->channel), mask);
    instPtr->watchmask = mask & instPtr->validmask;
    if (instPtr->watchmask) {
	Tcl_SetMaxBlockTime(&blockTime);
    }
}

/**
 * Provide access to the underlying device handle.
 */

static int
ChannelGetHandle(ClientData instance, int direction, ClientData *handlePtr)
{
    Channel *instPtr = instance;
    TRACE("ChannelGetHandle\n");
    *handlePtr = instPtr->handle;
    return TCL_OK;
}

/**
 * Control the blocking mode.
 */

static int
ChannelBlockMode(ClientData instance, int mode)
{
    Channel *instPtr = instance;
    TRACE("ChannelBlockMode\n");
    if (mode == TCL_MODE_NONBLOCKING) {
	instPtr->flags  |= FTD2XX_ASYNC;
    } else {
	instPtr->flags &= ~FTD2XX_ASYNC;
    }
    return TCL_OK;
}

/**
 * If a fileevent has occured on a channel then we end up in this event handler
 * function. We now notify the channel that an event is available. We also
 * remove the pending flag to permit more events to be raised as needed.
 */

static int
EventProc(Tcl_Event *evPtr, int flags)
{
    ChannelEvent *eventPtr = (ChannelEvent *)evPtr;
    Channel *chanPtr = eventPtr->instPtr;

    if (!(flags & TCL_FILE_EVENTS)) {
	return 0;
    }

    chanPtr->flags &= ~FTD2XX_PENDING;
    TRACE("EventProc mask 0x%08x\n", chanPtr->watchmask & eventPtr->flags);
    Tcl_NotifyChannel(chanPtr->channel, chanPtr->watchmask & eventPtr->flags);
    return 1;
}

/**
 * This function is called to setup the notifier to monitor our
 * channel for file events. Our CheckProc will be called anyway after some
 * interval so we really only need to ensure that it is called at some 
 * appropriate interval.
 */

static void
SetupProc(ClientData clientData, int flags)
{
    Package *pkgPtr = clientData;
    Channel *chanPtr = NULL;
    int msec = 10000;
    Tcl_Time blockTime = {0, 0};

    if (!(flags & TCL_FILE_EVENTS)) {
	return;
    }

    for (chanPtr = pkgPtr->headPtr; chanPtr != NULL; chanPtr = chanPtr->nextPtr) {
	msec = 10;
    }
    blockTime.sec = msec / 1000;
    blockTime.usec = (msec % 1000) * 1000;
    Tcl_SetMaxBlockTime(&blockTime);
}

/**
 * To support fileevents we have to check for any new data arriving. This
 * is done by polling the device at intervals. To avoid making calls to the
 * device we can use a Win32 event handle which will be signalled when
 * the device has data for us. When this occurs we raise a Tcl event
 * for this channel and queue it.
 * An alternative method would be to have a secondary thread wait on all the
 * event handles for all our channels. That would improve the latency at a
 * cost to code simplicity and maintainability. However a second thread might
 * help with non-blocking writes too so should be considered at some point.
 */

static void
CheckProc(ClientData clientData, int flags)
{
    Package *pkgPtr = clientData;
    Channel *chanPtr = NULL;

    if (!(flags & TCL_FILE_EVENTS)) {
	return;
    }

    for (chanPtr = pkgPtr->headPtr; chanPtr != NULL; chanPtr = chanPtr->nextPtr) {
	DWORD rx = 0, tx = 0, ev = 0;
	FT_STATUS fts = FT_OK;

	/* already has an event queued so move on */
	if (chanPtr->flags & FTD2XX_PENDING) {
	    continue;
	}

	/* Only test if there are active watches on this channel */
	if (chanPtr->watchmask == 0) {
	    continue;
	}

	if (WaitForSingleObject(chanPtr->event, 0) == WAIT_OBJECT_0) {
	    if ((fts = procs.FT_GetStatus(chanPtr->handle, &rx, &tx, &ev)) == FT_OK) {
		if (rx != 0 || tx != 0 || ev != 0) {
		    int mask = 0;
		    
		    mask = TCL_WRITABLE | ((rx) ? TCL_READABLE : 0);
		    //if (ev != 0) evPtr->flags |= TCL_EXCEPTION;
		    if (chanPtr->watchmask & mask) {
			ChannelEvent *evPtr = 
			    (ChannelEvent *)ckalloc(sizeof(ChannelEvent));
			chanPtr->flags |= FTD2XX_PENDING;
			evPtr->header.proc = EventProc;
			evPtr->instPtr = chanPtr;
			evPtr->flags = mask;
			Tcl_QueueEvent((Tcl_Event *)evPtr, TCL_QUEUE_TAIL);
		    }
		}
	    }
	}
    }
}

/**
 * Called to remove the event source when the interpreter exits.
 */

static void
DeleteProc(ClientData clientData)
{
    Package *pkgPtr = clientData;
    TRACE("Deleted FTD2xx command\n");
    Tcl_DeleteEventSource(SetupProc, CheckProc, pkgPtr);
    if (pkgPtr->hFtdi) FreeLibrary(pkgPtr->hFtdi);
    ckfree((char *)pkgPtr);
}

/**
 * Convert FTD2XX status errors into strings.
 */

static const char *
ConvertError(FT_STATUS fts)
{
    const char *s;
    static char other[80];
    switch (fts) {
	case FT_OK: s = "no error"; break;
	case FT_INVALID_HANDLE: s = "invalid handle"; break;
	case FT_DEVICE_NOT_FOUND: s = "device not found"; break;
	case FT_DEVICE_NOT_OPENED: s = "device not opened"; break;
	case FT_IO_ERROR: s = "io error"; break;
	case FT_INSUFFICIENT_RESOURCES: s = "insufficient resources"; break;
	case FT_INVALID_PARAMETER: s = "invalid parameter"; break;
	case FT_INVALID_BAUD_RATE: s = "invalid baud rate"; break;
	case FT_DEVICE_NOT_OPENED_FOR_ERASE: s = "device not opened for erase"; break;
	case FT_DEVICE_NOT_OPENED_FOR_WRITE: s = "device not opened for write"; break;
	case FT_FAILED_TO_WRITE_DEVICE: s = "failed to write device"; break;
	/* some EEPROM errors skipped */
	case FT_INVALID_ARGS: s = "invalid args"; break;
	case FT_NOT_SUPPORTED: s = "not supported"; break;
	case FT_DEVICE_LIST_NOT_READY: s = "device list not ready"; break;
	default:
	    sprintf(other, "unrecognised error 0x%08x", (ULONG)fts);
	    s = other;
	    break;
    }
    return s;
}

/**
 * Open a named device and create a Tcl channel to represent the open device
 * and to enable communications with Tcl programs. By default these channels
 * are configured to be binary and to have 500ms timeouts but all these can
 * be configured at runtime using the 'fconfigure' command.
 */

static int
OpenCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[])
{
    Package *pkgPtr = clientData;
    Channel *instPtr;
    char name[6+TCL_INTEGER_SPACE];
    const char *devname = NULL;
    FT_HANDLE handle = NULL;
    FT_STATUS fts = FT_OK;
    HANDLE hEvent = INVALID_HANDLE_VALUE;
    int r = TCL_OK, index, nameindex = 2, ftmode = FT_OPEN_BY_SERIAL_NUMBER;
    const unsigned long rxtimeout = 500, txtimeout = 500;
    enum {OPT_SERIAL, OPT_DESC, OPT_LOC};
    const char *options[] = {
	"-serial", "-description", "-location", NULL
    };

    if (objc < 3 || objc > 4) {
        Tcl_WrongNumArgs(interp, 1, objv, 
			 "?-serial? ?-description? ?-location? string");
        return TCL_ERROR;
    }
    
    if (objc == 4) {
	if (Tcl_GetIndexFromObj(interp, objv[2], options, 
				"option", 0, &index) != TCL_OK) {
	    return TCL_ERROR;
	}
	switch (index) {
	    case OPT_SERIAL: ftmode = FT_OPEN_BY_SERIAL_NUMBER; break;
	    case OPT_DESC: ftmode = FT_OPEN_BY_DESCRIPTION; break;
	    case OPT_LOC: ftmode = FT_OPEN_BY_LOCATION; break;
	}
	++nameindex;
    }
    devname = Tcl_GetString(objv[nameindex]);

    if ((fts = procs.FT_OpenEx((void *)devname, ftmode, &handle)) != FT_OK) {
        Tcl_AppendResult(interp, "failed open device \"",
			 devname, "\": ", ConvertError(fts), NULL);
        return TCL_ERROR;
    }

    if ((fts = procs.FT_SetBaudRate(handle, 19200)) != FT_OK) {
	Tcl_AppendResult(interp, "failed set baudrate: \"",
			 ConvertError(fts), NULL);
        return TCL_ERROR;
    }
    
    if ((fts = procs.FT_SetDataCharacteristics(handle, FT_BITS_8, FT_STOP_BITS_1, FT_PARITY_NONE)) != FT_OK) {
	Tcl_AppendResult(interp, "failed set data characteristics: \"",
			 ConvertError(fts), NULL);
        return TCL_ERROR;
    }
    
    if ((fts = procs.FT_SetFlowControl(handle, FT_FLOW_NONE, 0x11, 0x13)) != FT_OK) {
	Tcl_AppendResult(interp, "failed set flowcontrol: \"",
			 ConvertError(fts), NULL);
        return TCL_ERROR;
    }

    if ((fts = procs.FT_SetTimeouts(handle, rxtimeout, txtimeout)) != FT_OK) {
	procs.FT_Close(handle);
        Tcl_AppendResult(interp, "failed initialize timeouts: ",
			 ConvertError(fts), NULL);
        return TCL_ERROR;
    }
    hEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
    if ((fts = procs.FT_SetEventNotification(handle, FT_EVENT_RXCHAR, hEvent)) != FT_OK) {
	CloseHandle(hEvent);
	procs.FT_Close(handle);
        Tcl_AppendResult(interp, "failed configure event notifications: ",
			 ConvertError(fts), NULL);
        return TCL_ERROR;
    }

    sprintf(name, "ftd2xx%ld", pkgPtr->uid++);
    instPtr = (Channel *)ckalloc(sizeof(Channel));
    instPtr->flags = 0;
    instPtr->watchmask = 0;
    instPtr->validmask = TCL_READABLE | TCL_WRITABLE;
    instPtr->baudrate = 19200;
    instPtr->parity = FT_PARITY_NONE;
    instPtr->databits = FT_BITS_8;
    instPtr->stopbits = FT_STOP_BITS_1;
    instPtr->handshake = FT_FLOW_NONE;
    instPtr->xonchar = 0x11;
    instPtr->xoffchar = 0x13;
    instPtr->rxtimeout = rxtimeout;
    instPtr->txtimeout = txtimeout;
    instPtr->handle = handle;
    instPtr->event = hEvent;
    instPtr->channel = Tcl_CreateChannel(&Ftd2xxChannelType, name,
					 instPtr, instPtr->validmask);
    Tcl_SetChannelOption(interp, instPtr->channel, "-encoding", "binary");
    Tcl_SetChannelOption(interp, instPtr->channel, "-translation", "binary");
    Tcl_RegisterChannel(interp, instPtr->channel);
    
    /* insert at head of channels list */
    instPtr->pkgPtr = pkgPtr;
    instPtr->nextPtr = pkgPtr->headPtr;
    pkgPtr->headPtr = instPtr;
    ++pkgPtr->count;
    
    Tcl_SetObjResult(interp, Tcl_NewStringObj(name, -1));
    return TCL_OK;
}

/**
 * Purge the device transmit and receive buffers.
 */

static int
PurgeCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[])
{
    Tcl_Channel channel;
    Channel *instPtr;
    FT_STATUS fts;

    if (objc != 3) {
	Tcl_WrongNumArgs(interp, 2, objv, "channel");
	return TCL_ERROR;
    }

    channel = Tcl_GetChannel(interp, Tcl_GetString(objv[2]), NULL);
    if (channel == NULL) {
	return TCL_ERROR;
    }
    if (Tcl_GetChannelType(channel) != &Ftd2xxChannelType) {
	Tcl_AppendResult(interp, "wrong channel type: \"", Tcl_GetString(objv[2]),
			 "\" must be a ftd2xx channel", NULL);
	return TCL_ERROR;
    }

    instPtr = (Channel *)Tcl_GetChannelInstanceData(channel);
    if ((fts = procs.FT_Purge(instPtr->handle, FT_PURGE_RX | FT_PURGE_TX)) != FT_OK) {
	Tcl_AppendResult(interp, "error purging channel: ", ConvertError(fts), NULL);
	return TCL_ERROR;
    }

    return TCL_OK;
}

/**
 * Reset the device. This requires an open channel as a means of identifying the
 * device to reset.
 */

static int
ResetCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[])
{
    Tcl_Channel channel;
    Channel *instPtr;
    FT_STATUS fts;

    if (objc != 3) {
	Tcl_WrongNumArgs(interp, 2, objv, "channel");
	return TCL_ERROR;
    }

    channel = Tcl_GetChannel(interp, Tcl_GetString(objv[2]), NULL);
    if (channel == NULL) {
	return TCL_ERROR;
    }
    if (Tcl_GetChannelType(channel) != &Ftd2xxChannelType) {
	Tcl_AppendResult(interp, "wrong channel type: \"", Tcl_GetString(objv[2]),
			 "\" must be a ftd2xx channel", NULL);
	return TCL_ERROR;
    }

    instPtr = (Channel *)Tcl_GetChannelInstanceData(channel);
    if ((fts = procs.FT_ResetPort(instPtr->handle)) != FT_OK) {
	Tcl_AppendResult(interp, "error resetting channel: ", ConvertError(fts), NULL);
	return TCL_ERROR;
    }

    return TCL_OK;
}

/**
 * The implementation of the 'ftd2xx list' command. This function builds a list
 * of all the available D2XX compatible devices connected. Each list element
 * is a list of value-name pairs suitable for use with 'array set' or 'dict create'
 * that return the various bits of information provided by the D2XX device info
 * structure. Of note are the serial number and device description which may be
 * required when opening the device.
 */

static int
ListCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[])
{
    DWORD count = 0, node = 0;
    Tcl_Obj *listObj = NULL;
    FT_DEVICE_LIST_INFO_NODE *nodesPtr = NULL;
    FT_STATUS fts;
    const char *typeName;
    const char *typeNames[] = { "BM", "AM", "100AX", "UNKNOWN", "2232C", "232R"};

    if ((fts = procs.FT_CreateDeviceInfoList(&count)) != FT_OK) {
	Tcl_AppendResult(interp, "failed to enumerate devices: ",
			 ConvertError(fts), NULL);
	return TCL_ERROR;
    }

    nodesPtr = (FT_DEVICE_LIST_INFO_NODE*)
	ckalloc(sizeof(FT_DEVICE_LIST_INFO_NODE) * count);
    
    if ((fts = procs.FT_GetDeviceInfoList(nodesPtr, &count)) != FT_OK) {
	Tcl_AppendResult(interp, "failed to get device list: ",
			 ConvertError(fts), NULL);
	ckfree((char *)nodesPtr);
	return TCL_ERROR;
    }

    listObj = Tcl_NewListObj(0, NULL);
    for (node = 0; node < count; ++node) {
	Tcl_Obj *devObj = Tcl_NewListObj(0, NULL);
	
	Tcl_ListObjAppendElement(interp, devObj, Tcl_NewStringObj("id", -1));
	Tcl_ListObjAppendElement(interp, devObj, Tcl_NewLongObj(nodesPtr[node].ID));
	Tcl_ListObjAppendElement(interp, devObj, Tcl_NewStringObj("location", -1));
	Tcl_ListObjAppendElement(interp, devObj, Tcl_NewLongObj(nodesPtr[node].LocId));
	Tcl_ListObjAppendElement(interp, devObj, Tcl_NewStringObj("serial", -1));
	Tcl_ListObjAppendElement(interp, devObj, 
				 Tcl_NewStringObj(nodesPtr[node].SerialNumber, -1));
	Tcl_ListObjAppendElement(interp, devObj, Tcl_NewStringObj("description", -1));
	Tcl_ListObjAppendElement(interp, devObj, 
				 Tcl_NewStringObj(nodesPtr[node].Description, -1));
	Tcl_ListObjAppendElement(interp, devObj, Tcl_NewStringObj("type", -1));
	typeName = typeNames[FT_DEVICE_UNKNOWN];
	if (nodesPtr[node].Type < sizeof(typeNames)/sizeof(typeNames[0])) {
	    typeName = typeNames[nodesPtr[node].Type];
	}
	Tcl_ListObjAppendElement(interp, devObj, Tcl_NewStringObj(typeName, -1));
	Tcl_ListObjAppendElement(interp, devObj, Tcl_NewStringObj("handle", -1));
	Tcl_ListObjAppendElement(interp, devObj,
				 Tcl_NewWideIntObj((Tcl_WideInt)nodesPtr[node].ftHandle));
	Tcl_ListObjAppendElement(interp, devObj, Tcl_NewStringObj("opened", -1));
	Tcl_ListObjAppendElement(interp, devObj,
				 Tcl_NewBooleanObj(nodesPtr[node].Flags & 1));
	Tcl_ListObjAppendElement(interp, listObj, devObj);
    }
    ckfree((char*)nodesPtr);
    Tcl_SetObjResult(interp, listObj);
    return TCL_OK;
}

static int
VersionCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[])
{
    FT_STATUS fts;
    DWORD dwVersion = 0;

    if (objc != 2) {
	Tcl_WrongNumArgs(interp, 2, objv, "");
	return TCL_ERROR;
    }
    
    if ((fts = procs.FT_GetLibraryVersion(&dwVersion)) != FT_OK) {
	Tcl_AppendResult(interp, "failed to get version: ",
			 ConvertError(fts), NULL);
	return TCL_ERROR;
    }

    Tcl_SetObjResult(interp, Tcl_NewWideIntObj((Tcl_WideInt)dwVersion));
    return TCL_OK;
}

static int
RescanCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[])
{
    FT_STATUS fts;

    if (objc != 2) {
	Tcl_WrongNumArgs(interp, 2, objv, "");
	return TCL_ERROR;
    }
    
    if ((fts = procs.FT_Rescan()) != FT_OK) {
	Tcl_AppendResult(interp, "failed to rescan: ",
			 ConvertError(fts), NULL);
	return TCL_ERROR;
    }

    return TCL_OK;
}

typedef struct Ensemble {
    const char *name;
    Tcl_ObjCmdProc *command;
    struct Ensemble *ensemble;
} Ensemble;

static Ensemble Ftd2xxEnsemble[] = {
    { "open", OpenCmd, NULL },
    { "list", ListCmd, NULL },
    { "purge", PurgeCmd, NULL },
    { "rescan", RescanCmd, NULL},
    { "reset", ResetCmd, NULL },
    { "version", VersionCmd, NULL },
    { NULL, NULL, NULL }
};

static int
EnsembleCmd(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    Ensemble *ensemble = Ftd2xxEnsemble;
    int option = 1, index;
    while (option < objc) {
        if (Tcl_GetIndexFromObjStruct(interp, objv[option], ensemble, 
            sizeof(ensemble[0]), "command", 0, &index) != TCL_OK) 
        {
            return TCL_ERROR;
        }
        if (ensemble[index].command) {
            return ensemble[index].command(clientData, interp, objc, objv);
        }
        ensemble = ensemble[index].ensemble;
        ++option;
    }
    Tcl_WrongNumArgs(interp, option, objv, "option ?arg arg ...?");
    return TCL_ERROR;
}

/**
 * Package initialization function.
 */

int DLLEXPORT
Ftd2xx_Init(Tcl_Interp *interp)
{
    Package *pkgPtr;
    HMODULE hDll = NULL;
#ifdef WIN64
    LPCSTR szDllName = "FTD2XX64.dll";
#else
    LPCSTR szDllName = "FTD2XX.dll";
#endif

#ifdef USE_TCL_STUBS
    if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL) {
        return TCL_ERROR;
    }
#endif

    hDll = LoadLibraryA(szDllName);
    if (hDll == NULL) {
	Tcl_SetResult(interp, "failed to load the FTDI library", TCL_STATIC);
	Tcl_SetErrorCode(interp, "DLL_NOT_FOUND", szDllName, NULL);
	return TCL_ERROR;
    }

#define LOADPROC(name) \
    (0 == (procs.name = (name ## Proc *)GetProcAddress(hDll, #name) ))

    if (LOADPROC(FT_Close)
	|| LOADPROC(FT_CreateDeviceInfoList)
	|| LOADPROC(FT_GetDeviceInfoList)
	|| LOADPROC(FT_GetLatencyTimer)
	|| LOADPROC(FT_GetBitMode)
	|| LOADPROC(FT_GetStatus)
	|| LOADPROC(FT_OpenEx)
	|| LOADPROC(FT_Purge)
	|| LOADPROC(FT_Read)
	|| LOADPROC(FT_ResetPort)
	|| LOADPROC(FT_SetEventNotification)
	|| LOADPROC(FT_SetLatencyTimer)
	|| LOADPROC(FT_SetBitMode)
	|| LOADPROC(FT_SetTimeouts)
	|| LOADPROC(FT_Write)
	|| LOADPROC(FT_GetLibraryVersion)
	|| LOADPROC(FT_Rescan)
	|| LOADPROC(FT_SetBaudRate)
	|| LOADPROC(FT_SetDataCharacteristics)
	|| LOADPROC(FT_SetFlowControl)
	)
    {
	Tcl_SetResult(interp, "invalid ftd2xx.dll library!", TCL_STATIC);
	Tcl_SetErrorCode(interp, "DLL_INVALID", szDllName, NULL);
	FreeLibrary(hDll);
	return TCL_ERROR;
    }
#undef LOADPROC

    pkgPtr = (Package *)ckalloc(sizeof(Package));
    pkgPtr->headPtr = NULL;
    pkgPtr->count = 0;
    pkgPtr->uid = 0;
    pkgPtr->hFtdi = hDll;
    Tcl_CreateEventSource(SetupProc, CheckProc, pkgPtr);
    Tcl_CreateObjCommand(interp, "ftd2xx", EnsembleCmd, pkgPtr, DeleteProc);
    return Tcl_PkgProvide(interp, "ftd2xx", PACKAGE_VERSION);
}

/*
 * Local variables:
 *   mode: c
 *   c-basic-offset: 4
 *   fill-column: 78
 *   indent-tabs-mode: t
 *   tab-width: 8
 * End:
 */

