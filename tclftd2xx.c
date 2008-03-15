/* tclftd2xx.c - Copyright (C) 2008 Pat Thoyts <patthoyts@users.sourceforge.net>
 *
 *	FTDI D2XX USB Device driver Tcl interface.
 *
 * ----------------------------------------------------------------------
 *	See the accompanying file 'licence.terms' for the software license.
 *	In essence - this is MIT licencensed code.
 * ----------------------------------------------------------------------
 */

#define STRICT
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <tcl.h>
#include <errno.h>
#include "ftd2xx.h"

#if _MSC_VER >= 1000
#pragma comment(lib, "ftd2xx.lib")
#endif

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
    unsigned long rxtimeout;
    unsigned long txtimeout;
    FT_HANDLE handle;
} Channel;

typedef struct ChannelEvent {
    Tcl_Event header;
    Channel *instPtr;
    int flags;
} ChannelEvent;

typedef struct Package {
    struct Channel *headPtr;
    unsigned long uid;
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

static int
ChannelClose(ClientData instance, Tcl_Interp *interp)
{
    Channel *instPtr = instance;
    Package *pkgPtr = instPtr->pkgPtr;
    Channel **tmpPtrPtr;
    int r = TCL_OK;
    FT_STATUS fts;

    OutputDebugString("ChannelClose\n");
    fts = FT_Purge(instPtr->handle, FT_PURGE_RX | FT_PURGE_TX);
    fts = FT_Close(instPtr->handle);
    if (fts != FT_OK) {
	Tcl_AppendResult(interp, "error closing device: ",
			 ConvertError(fts), NULL);
	r = TCL_ERROR;
    }
    /* remove this channel from the package list */
    tmpPtrPtr = &pkgPtr->headPtr;
    while (*tmpPtrPtr && *tmpPtrPtr != instPtr) {
	tmpPtrPtr = &(*tmpPtrPtr)->nextPtr;
    }
    *tmpPtrPtr = instPtr->nextPtr;

    ckfree((char *)instPtr);
    return r;
}

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
	if ((fts = FT_GetStatus(instPtr->handle, &rx, &tx, &ev)) == FT_OK) {
	    if ((int)rx < toRead) {
		toRead = rx;
	    }
	} else {
	    OutputDebugString(ConvertError(fts));
	}
    }
    if (FT_Read(instPtr->handle, buffer, toRead, &cbRead) != FT_OK) {
	OutputDebugString(ConvertError(fts));
	*errorCodePtr = EINVAL;
    }
    return (int)cbRead;
}

static int
ChannelOutput(ClientData instance, const char *buffer, int toWrite, int *errorCodePtr)
{
    Channel *instPtr = instance;
    char sz[80];
    DWORD cbWrote = 0;
    DWORD dwStart = GetTickCount();
    if (FT_Write(instPtr->handle, (void *)buffer, toWrite, &cbWrote) != FT_OK) {
	*errorCodePtr = EINVAL;
    }
    sprintf(sz, "ChannelOutput %ld ms\n", GetTickCount()-dwStart);
    OutputDebugString(sz);
    return (int)cbWrote;
}

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
	    fts = FT_SetTimeouts(instPtr->handle, (DWORD)tmp, instPtr->txtimeout);
	    if (fts == FT_OK) {
		instPtr->rxtimeout = (unsigned long)tmp;
	    }
	}
    } else if (!strcmp("-writetimeout", optionName)) {
	int tmp = 1;
	r = Tcl_GetInt(interp, newValue, &tmp);
	if (r == TCL_OK) {
	    fts = FT_SetTimeouts(instPtr->handle, instPtr->rxtimeout, (DWORD)tmp);
	    if (fts == FT_OK) {
		instPtr->txtimeout = (unsigned long)tmp;
	    }
	}
    } else if (!strcmp("-latency", optionName)) {
	int tmp = 1;
	r = Tcl_GetInt(interp, newValue, &tmp);
	if (r == TCL_OK) {
	    fts = FT_SetLatencyTimer(instPtr->handle, (UCHAR)tmp);
	}
    }

    if (fts != FT_OK) {
	Tcl_AppendResult(interp, "error setting ", optionName,
			 ": ", ConvertError(fts), NULL);
	r = TCL_ERROR;
    }

    return TCL_OK;
}

static int
ChannelGetOption(ClientData instance, Tcl_Interp *interp, 
		 const char *optionName, Tcl_DString *optionValue)
{
    Channel *instPtr = instance;
    const char *options[] = {"readtimeout", "writetimeout", "latency", NULL};
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
	    fts = FT_GetLatencyTimer(instPtr->handle, &timer);
	    if (fts == FT_OK) {
		sprintf(Tcl_DStringValue(&ds), "%d", timer);
	    } else {
		Tcl_AppendResult(interp, "failed to read ", optionName, ": ",
				 ConvertError(fts), NULL);
		r = TCL_ERROR;
	    }
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

static void
ChannelWatch(ClientData instance, int mask)
{
    Channel *instPtr = instance;
    Tcl_Time blockTime = {0, 10000}; /* 10 msec */
    char sz[80];
    sprintf(sz, "ChannelWatch %s 0x%08x\n",
	    Tcl_GetChannelName(instPtr->channel), mask);
    OutputDebugString(sz);
    instPtr->watchmask = mask & instPtr->validmask;
    if (instPtr->watchmask) {
	Tcl_SetMaxBlockTime(&blockTime);
    }
}

static int
ChannelGetHandle(ClientData instance, int direction, ClientData *handlePtr)
{
    Channel *instPtr = instance;
    OutputDebugString("ChannelGetHandle\n");
    *handlePtr = instPtr->handle;
    return TCL_OK;
}

static int
ChannelBlockMode(ClientData instance, int mode)
{
    Channel *instPtr = instance;
    OutputDebugString("ChannelBlockMode\n");
    if (mode == TCL_MODE_NONBLOCKING) {
	instPtr->flags  |= FTD2XX_ASYNC;
    } else {
	instPtr->flags &= ~FTD2XX_ASYNC;
    }
    return TCL_OK;
}

static int
EventProc(Tcl_Event *evPtr, int flags)
{
    ChannelEvent *eventPtr = (ChannelEvent *)evPtr;
    Channel *chanPtr = eventPtr->instPtr;
    char sz[80];

    if (!(flags & TCL_FILE_EVENTS)) {
	return 0;
    }

    chanPtr->flags &= ~FTD2XX_PENDING;
    sprintf(sz, "EventProc mask 0x%08x\n", chanPtr->watchmask & eventPtr->flags);
    OutputDebugString(sz);
    Tcl_NotifyChannel(chanPtr->channel, chanPtr->watchmask & eventPtr->flags);
    return 1;
}

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

	if (chanPtr->flags & FTD2XX_PENDING) {
	    continue;
	}

	/* Only test if there are active watches on this channel */
	if (chanPtr->watchmask == 0) {
	    continue;
	}

	if ((fts = FT_GetStatus(chanPtr->handle, &rx, &tx, &ev)) == FT_OK) {
	    if (rx != 0 || tx != 0 || ev != 0) {
		int mask = 0;

		mask = TCL_WRITABLE | ((rx)?TCL_READABLE:0);
		//if (ev != 0) evPtr->flags |= TCL_EXCEPTION;
		if (chanPtr->watchmask & mask) {
		    ChannelEvent *evPtr = (ChannelEvent *)ckalloc(sizeof(ChannelEvent));
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

static void
DeleteProc(ClientData clientData)
{
    Package *pkgPtr = clientData;
    OutputDebugString("Deleted FTD2xx command\n");
    Tcl_DeleteEventSource(SetupProc, CheckProc, pkgPtr);
    ckfree((char *)pkgPtr);
}

static const char *
ConvertError(FT_STATUS fts)
{
    const char *s;
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
	default: s = "other error"; break;
    }
    return s;
}

static int
OpenCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[])
{
    Package *pkgPtr = clientData;
    const char *name = NULL;
    FT_HANDLE handle = 0;
    FT_STATUS fts = FT_OK;
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
    name = Tcl_GetString(objv[nameindex]);

    fts = FT_OpenEx((void *)name, ftmode, &handle);
    if (fts == FT_OK)
	fts = FT_SetTimeouts(handle, rxtimeout, txtimeout);
    if (fts == FT_OK) {
        Channel *instPtr;
        char name[6+TCL_INTEGER_SPACE];

        sprintf(name, "ftd2xx%ld", pkgPtr->uid++);
        instPtr = (Channel *)ckalloc(sizeof(Channel));
	instPtr->flags = 0;
        instPtr->watchmask = 0;
        instPtr->validmask = TCL_READABLE | TCL_WRITABLE;
	instPtr->rxtimeout = rxtimeout;
	instPtr->txtimeout = txtimeout;
        instPtr->handle = handle;
        instPtr->channel = Tcl_CreateChannel(&Ftd2xxChannelType, name,
					     instPtr, instPtr->validmask);
	Tcl_SetChannelOption(interp, instPtr->channel, "-encoding", "binary");
	Tcl_SetChannelOption(interp, instPtr->channel, "-translation", "binary");
        Tcl_RegisterChannel(interp, instPtr->channel);

	/* insert at head of channels list */
	instPtr->pkgPtr = pkgPtr;
	instPtr->nextPtr = pkgPtr->headPtr;
	pkgPtr->headPtr = instPtr;

        Tcl_SetObjResult(interp, Tcl_NewStringObj(name, -1));
        r = TCL_OK;
    } else {
        Tcl_AppendResult(interp, "failed to open device: \"",
			 name, "\": ", ConvertError(fts), NULL);
        r = TCL_ERROR;
    }
    return r;
}

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
    if ((fts = FT_Purge(instPtr->handle, FT_PURGE_RX | FT_PURGE_TX)) != FT_OK) {
	Tcl_AppendResult(interp, "error purging channel: ", ConvertError(fts), NULL);
	return TCL_ERROR;
    }

    return TCL_OK;
}

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
    if ((fts = FT_ResetPort(instPtr->handle)) != FT_OK) {
	Tcl_AppendResult(interp, "error resetting channel: ", ConvertError(fts), NULL);
	return TCL_ERROR;
    }

    return TCL_OK;
}

static int
ListCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const objv[])
{
    DWORD count = 0, node = 0;
    Tcl_Obj *listObj = NULL;
    FT_DEVICE_LIST_INFO_NODE *nodesPtr = NULL;
    FT_STATUS fts;
    const char *typeName;
    const char *typeNames[] = { "BM", "AM", "100AX", "UNKNOWN", "2232C", "232R"};

    if ((fts = FT_CreateDeviceInfoList(&count)) != FT_OK) {
	Tcl_AppendResult(interp, "failed to enumerate devices: ",
			 ConvertError(fts), NULL);
	return TCL_ERROR;
    }

    nodesPtr = (FT_DEVICE_LIST_INFO_NODE*)
	ckalloc(sizeof(FT_DEVICE_LIST_INFO_NODE) * count);
    
    if ((fts = FT_GetDeviceInfoList(nodesPtr, &count)) != FT_OK) {
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
				 Tcl_NewLongObj((long)nodesPtr[node].ftHandle));
	Tcl_ListObjAppendElement(interp, devObj, Tcl_NewStringObj("opened", -1));
	Tcl_ListObjAppendElement(interp, devObj,
				 Tcl_NewBooleanObj(nodesPtr[node].Flags & 1));
	Tcl_ListObjAppendElement(interp, listObj, devObj);
    }
    ckfree((char*)nodesPtr);
    Tcl_SetObjResult(interp, listObj);
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
    { "reset", ResetCmd, NULL },
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

int DLLEXPORT
Ftd2xx_Init(Tcl_Interp *interp)
{
    Package *pkgPtr;

#ifdef USE_TCL_STUBS
    if (Tcl_InitStubs(interp, "8.0", 0) == NULL) {
        return TCL_ERROR;
    }
#endif

    pkgPtr = (Package *)ckalloc(sizeof(Package));
    pkgPtr->headPtr = NULL;
    pkgPtr->uid = 0;
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

