/*
 * MIDI stream library for Tcl, Windows only.
 *
 * Copyright (c) 2008 by Jos Decoster <jos.decoster@gmail.com>
 */

/*
## (This license blatantly stolen from Tktable and Tcl/Tk license and adapted -
## thus assume it falls under similar license terms).
##
## This software is copyrighted by Jos Decoster <jos _dot_ decoster _at_ gmail 
## _dot_ com>.  The  following terms apply to all files associated with the 
## software unless explicitly disclaimed in individual files.
##
## The authors hereby grant permission to use, copy, modify, distribute, and
## license this software and its documentation for any purpose, provided that
## existing copyright notices are retained in all copies and that this notice
## is included verbatim in any distributions.  No written agreement, license,
## or royalty fee is required for any of the authorized uses.
##
## IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY FOR
## DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
## OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY DERIVATIVES THEREOF,
## EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
## THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
## INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE IS
## PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE NO
## OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
## MODIFICATIONS.
##
## RESTRICTED RIGHTS: Use, duplication or disclosure by the U.S. government
## is subject to the restrictions as set forth in subparagraph (c) (1) (ii)
## of the Rights in Technical Data and Computer Software Clause as DFARS
## 252.227-7013 and FAR 52.227-19.
*/

/*
 * TODO:
 *
 *  - add command delete procs
 */

#include "tcl.h"
#include "windows.h"

#define MidiStreamHeaderDataSize (0Xffff/sizeof(DWORD))
struct MidiStreamHeader 
{
  MIDIHDR hdr;
  DWORD data[MidiStreamHeaderDataSize];
  int size;
  HMIDISTRM* dev; /* Set when doing a Prepare in StreamOut */
};

int StreamOutHeaderObjCmd(ClientData     clientData, 
			  Tcl_Interp*    interp,
			  int            objc,
			  Tcl_Obj* const objv[]);

static void ReportMidiError(Tcl_Interp* interp, const char* msg, int err)
{
  char midiError[1024];
  Tcl_Obj* obj = Tcl_NewStringObj(msg, strlen(msg));
  midiOutGetErrorText(err, midiError, 1024);
  Tcl_AppendToObj(obj, ": ", 2);
  Tcl_AppendToObj(obj, midiError, strlen(midiError));
  Tcl_SetObjResult(interp, obj);  
}

int StreamOutObjCmd(ClientData     clientData, 
		    Tcl_Interp*    interp,
		    int            objc,
		    Tcl_Obj* const objv[]) 
{
  static const char* methods[] = {
    "close", "out", "pause", "play", "position", 
    "set", "short", "stop", NULL
  };
  enum StreamMethods {
    STRMTD_CLOSE, STRMTD_OUT, STRMTD_PAUSE, STRMTD_PLAY, STRMTD_POS,
    STRMTD_SET, STRMTD_SHORT, STRMTD_STOP
  };
  int index = -1;
  int err;
  HMIDISTRM* handlePtr = (HMIDISTRM*)clientData;

  if (objc < 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "method ?argument ...?");
    return TCL_ERROR;
  }

  if (Tcl_GetIndexFromObj(interp, objv[1], methods, "method", 0,
			  &index) != TCL_OK)
    return TCL_ERROR;

  switch((enum StreamMethods)index) {
  case STRMTD_CLOSE:
    if (err = midiStreamClose(*handlePtr)) {
      ReportMidiError(interp, "Failed to close midi stream", err);
      return TCL_ERROR;
    }
    Tcl_DeleteCommand(interp, Tcl_GetString(objv[0]));
    ckfree((char*)handlePtr);
    break;
  case STRMTD_OUT:
    {
      const char* hdrName;
      struct Tcl_CmdInfo hdrCmdInfo;
      struct MidiStreamHeader* hdrPtr;
      int err, i;

      /* check if valid header */
      if (objc < 3) {
	Tcl_WrongNumArgs(interp, 2, objv, "header");
	return TCL_ERROR;
      }

      hdrName = Tcl_GetString(objv[2]);

      if (!Tcl_GetCommandInfo(interp, hdrName, &hdrCmdInfo) != TCL_OK) {
	Tcl_SetResult(interp, "no header command", TCL_STATIC);
	return TCL_ERROR;
      }

      if (hdrCmdInfo.objProc != (Tcl_ObjCmdProc*)StreamOutHeaderObjCmd) {
	Tcl_SetResult(interp, "no midi header", TCL_STATIC);
	return TCL_ERROR;
      }

      hdrPtr = (struct MidiStreamHeader*)(hdrCmdInfo.objClientData);
      hdrPtr->dev = handlePtr;
      hdrPtr->hdr.dwFlags = 0;
      hdrPtr->hdr.lpData = (LPBYTE)(&(hdrPtr->data[0]));
      hdrPtr->hdr.dwBufferLength = hdrPtr->size*sizeof(DWORD);
      hdrPtr->hdr.dwBytesRecorded = hdrPtr->size*sizeof(DWORD);

      /* prepare header */
      if (err = midiOutPrepareHeader(*(HMIDIOUT*)handlePtr, &hdrPtr->hdr, 
				     sizeof(MIDIHDR))) {
	ReportMidiError(interp, "Failed to prepare header", err);
	return TCL_ERROR;    
      }

      /* out header */
      if (err = midiStreamOut(*handlePtr, &hdrPtr->hdr, 
			      sizeof(MIDIHDR))) {
	ReportMidiError(interp, "Failed to stream data out", err);
	return TCL_ERROR;    
      }

      break;
    }
  case STRMTD_PAUSE:
    if (err = midiStreamPause(*handlePtr)) {
      ReportMidiError(interp, "Failed to pause midi stream", err);
      return TCL_ERROR;
    }
    break;
  case STRMTD_PLAY:
    if (err = midiStreamRestart(*handlePtr)) {
      ReportMidiError(interp, "Failed to play midi stream", err);
      return TCL_ERROR;
    }
    break;
  case STRMTD_POS:
    {
      MMTIME mm;
      mm.wType = TIME_TICKS;
      if (err = midiStreamPosition(*handlePtr, &mm, sizeof(MMTIME))) {
	ReportMidiError(interp, "Failed to get position in midi stream", err);
	return TCL_ERROR;
      }
      Tcl_SetObjResult(interp, Tcl_NewIntObj(mm.u.ticks));
      break;
    }
  case STRMTD_SET:
    {
      static const char* options[] = {
	"timediv", NULL
      };
      enum SetOptions {
	STRMTD_SET_TIMEDIV
      };
      int setIndex = -1;

      if (objc < 3) {
	Tcl_WrongNumArgs(interp, 2, objv, "option ?argument ...?");
	return TCL_ERROR;
      }

      if (Tcl_GetIndexFromObj(interp, objv[2], options, "option", 0,
			      &setIndex) != TCL_OK)
	return TCL_ERROR;

      switch((enum SetOptions)setIndex) {
      case STRMTD_SET_TIMEDIV:
	{
	  MIDIPROPTIMEDIV prop;
	  int timeDiv;

	  if (objc != 4) {
	    Tcl_WrongNumArgs(interp, 3, objv, "value");
	    return TCL_ERROR;
	  }

	  if (Tcl_GetIntFromObj(interp, objv[3], &timeDiv) != TCL_OK)
	    return TCL_ERROR;

	  prop.cbStruct = sizeof(MIDIPROPTIMEDIV);
	  prop.dwTimeDiv = timeDiv;

	  if (err = midiStreamProperty(*handlePtr, (LPBYTE)&prop, 
				       MIDIPROP_SET|MIDIPROP_TIMEDIV)) {
	    ReportMidiError(interp, "Failed to set time division", err);
	    return TCL_ERROR;
	  }
	  break;
	}
      }
      break;
    }
  case STRMTD_SHORT:
  {	
      int i0 = 0;
      int i1 = 0;
      int i2 = 0;
      unsigned char b0, b1, b2;
      DWORD msg = 0;
      int err;
      
      if (objc < 3 || objc > 5) {
	  Tcl_WrongNumArgs(interp, 2, objv, "byte0 ?byte1 ?byte2??");
	  return TCL_ERROR;
      }

      if (objc > 0 && Tcl_GetIntFromObj(interp, objv[2], &i0) != TCL_OK)
	  return TCL_ERROR;
      
      if (objc > 1 && Tcl_GetIntFromObj(interp, objv[3], &i1) != TCL_OK)
	  return TCL_ERROR;
      
      if (objc > 2 && Tcl_GetIntFromObj(interp, objv[4], &i2) != TCL_OK)
	  return TCL_ERROR;
      
      b0 = i0 & 0Xff;
      b1 = i1 & 0Xff;
      b2 = i2 & 0Xff;

      msg = (b2 << 16) | (b1 << 8) | b0;
      
      if (err = midiOutShortMsg(*(HMIDIOUT*)handlePtr, msg)) {
	  ReportMidiError(interp, "Failed to send short event to midi stream",
			  err);
	  return TCL_ERROR;
      }
      
      break;
  }
  case STRMTD_STOP:
    if (err = midiStreamStop(*handlePtr)) {
      ReportMidiError(interp, "Failed to stop midi stream", err);
      return TCL_ERROR;
    }
    break;
  }
  
  return TCL_OK;
}

static int handleCnt = 0;

int StreamOutOpen(ClientData     clientData, 
		  Tcl_Interp*    interp,
		  int            objc,
		  Tcl_Obj* const objv[]) 
{
  int err;
  int iDevID;
  UINT wDevID;
  UINT wNumDevs;
  HMIDISTRM* handlePtr;
  char handleName[13 + TCL_INTEGER_SPACE];

  if (objc != 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "dev#");
    return TCL_ERROR;
  }

  if (Tcl_GetIntFromObj(interp, objv[1], &iDevID) != TCL_OK)
    return TCL_ERROR;

  wDevID = iDevID;
  wNumDevs = midiOutGetNumDevs();

  if ( wDevID >= wNumDevs ) {
    Tcl_SetResult(interp, "Invalid midi output dev#", TCL_STATIC);
    return TCL_ERROR;
  }
  
  handlePtr = (HMIDISTRM*)ckalloc(sizeof(HMIDISTRM));

  if (err = midiStreamOpen(handlePtr, &wDevID, 1, 0, 0, CALLBACK_NULL)) {
    ckfree((char*)handlePtr);
    ReportMidiError(interp, "Failed to open midi stream", err);
    return TCL_ERROR;
  }

  sprintf(handleName, "::midistream%d", handleCnt++);

  Tcl_CreateObjCommand(interp, handleName,
		       (Tcl_ObjCmdProc*) StreamOutObjCmd,
		       (ClientData) handlePtr, (Tcl_CmdDeleteProc *) NULL);

  Tcl_SetObjResult(interp, Tcl_NewStringObj(handleName, strlen(handleName)));

  return TCL_OK;
}

static int addMidiNopEvent(DWORD* midiData, 
			   int midiDataMaxSize, 
			   int* midiDataSize, 
			   DWORD deltaTime)
{
  if ((*midiDataSize) + 3 >= midiDataMaxSize)
    return 1;
  midiData[(*midiDataSize)++] = deltaTime; /* dwDeltaTime */
  midiData[(*midiDataSize)++] = 0; /* dwStreamID (must be 0) */
  midiData[(*midiDataSize)++] = 
    ((DWORD)MEVT_F_SHORT << 24) | 
    ((DWORD)MEVT_NOP << 24); /* dwEvent */
  return 0;
}

static int addMidiShortEvent(DWORD* midiData, 
			     int midiDataMaxSize, 
			     int* midiDataSize, 
			     DWORD deltaTime, 
			     unsigned char b0, 
			     unsigned char b1, 
			     unsigned char b2)
{
  if ((*midiDataSize) + 3 >= midiDataMaxSize)
    return 1;
  midiData[(*midiDataSize)++] = deltaTime; /* dwDeltaTime */
  midiData[(*midiDataSize)++] = 0; /* dwStreamID (must be 0) */
  midiData[(*midiDataSize)++] = 
    ((DWORD)MEVT_F_SHORT << 24) | 
    ((DWORD)MEVT_SHORTMSG << 24) | 
    ((DWORD)b2 << 16) | 
    ((DWORD)b1 << 8) | 
    ((DWORD)b0); /* dwEvent */
  return 0;
}

static int addMidiTempoEvent(DWORD* midiData, 
			     int midiDataMaxSize, 
			     int* midiDataSize, 
			     DWORD deltaTime, 
			     DWORD tempo)
{
  if ((*midiDataSize) + 3 >= midiDataMaxSize)
    return 1;
  midiData[(*midiDataSize)++] = deltaTime; /* dwDeltaTime */
  midiData[(*midiDataSize)++] = 0; /* dwStreamID (must be 0) */
  midiData[(*midiDataSize)++] = 
    ((DWORD)MEVT_F_SHORT << 24) | 
    ((DWORD)MEVT_TEMPO << 24) | 
    tempo; /* dwEvent */
  return 0;
}

static int addMidiLongEvent(Tcl_Interp* interp,
			    DWORD* midiData, 
			    int midiDataMaxSize, 
			    int* midiDataSize, 
			    DWORD deltaTime, 
			    int parmsSize, 
			    Tcl_Obj* parms[])
{
  int i;
  char* p;
  int s = 3 + (parmsSize + (parmsSize % sizeof(DWORD))) / sizeof(DWORD);
  if ((*midiDataSize) + s >= midiDataMaxSize)
    return 1;
  midiData[(*midiDataSize)++] = deltaTime; /* dwDeltaTime */
  midiData[(*midiDataSize)++] = 0; /* dwStreamID (must be 0) */
  midiData[(*midiDataSize)++] = 
    ((DWORD)MEVT_F_LONG << 24) | 
    ((DWORD)MEVT_LONGMSG << 24) | 
    parmsSize; /* dwEvent */
  p = (unsigned char*)&midiData[*midiDataSize];
  for(i = 0; i < parmsSize; i++) {
    int d;
    unsigned char u;
    if (Tcl_GetIntFromObj(interp, parms[i], &d) != TCL_OK)
      return 2;
    u = d & 0Xff;
    p[i] = u; /* dwParms[] */
  }
  *midiDataSize += (parmsSize + (parmsSize % sizeof(DWORD))) / sizeof(DWORD);
  return 0;
}

static int headerCnt = 0;

int StreamOutHeaderObjCmd(ClientData     clientData, 
			  Tcl_Interp*    interp,
			  int            objc,
			  Tcl_Obj* const objv[]) 
{
  static const char* methods[] = {
    "add", "destroy", NULL
  };
  enum StreamHeaderMethods {
    STRHMTD_ADD, STRHMTD_DESTROY
  };
  int index = -1;
  int err;
  struct MidiStreamHeader* hdrPtr = (struct MidiStreamHeader*)clientData;

  if (objc < 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "method ?argument ...?");
    return TCL_ERROR;
  }

  if (Tcl_GetIndexFromObj(interp, objv[1], methods, "method", 0,
			  &index) != TCL_OK)
    return TCL_ERROR;

  switch((enum StreamHeaderMethods)index) {
  case STRHMTD_ADD:
    {
      static const char* options[] = {
	"event", "long", "nop", "short", "tempo", NULL
      };
      enum SetOptions {
	STRHMTD_ADD_EVENT, STRHMTD_ADD_LONG, STRHMTD_ADD_NOP, STRHMTD_ADD_SHORT,
	STRHMTD_ADD_TEMPO
      };
      int addIndex = -1;

      if (objc < 3) {
	Tcl_WrongNumArgs(interp, 2, objv, "option ?argument ...?");
	return TCL_ERROR;
      }

      if (Tcl_GetIndexFromObj(interp, objv[2], options, "option", 0,
			      &addIndex) != TCL_OK)
	return TCL_ERROR;
      
      switch((enum SetOptions)addIndex) {
      case STRHMTD_ADD_EVENT: /* adds list of bytes */
	{
	  int idt = 0;
	  DWORD dt;
	  int bobjc;
	  Tcl_Obj** bobjv;

	  if (objc != 5) {
	    Tcl_WrongNumArgs(interp, 3, objv, 
			     "deltaTime byteList");
	    return TCL_ERROR;
	  }

	  if (Tcl_GetIntFromObj(interp, objv[3], &idt) != TCL_OK)
	    return TCL_ERROR;

	  if (Tcl_ListObjGetElements(interp, objv[4], &bobjc, &bobjv) != TCL_OK)
	    return TCL_ERROR;

	  dt = idt;

	  if (bobjc == 0) {
	      if (addMidiNopEvent(&hdrPtr->data[0], MidiStreamHeaderDataSize, 
				  &hdrPtr->size, dt)) {
		  Tcl_SetResult(interp, "could not add nop event", TCL_STATIC);
		  return TCL_ERROR;
	      }
	  }
	  else if (bobjc <= 3) {

	    int i0 = 0;
	    int i1 = 0;
	    int i2 = 0;
	    unsigned char b0, b1, b2;

	    if (bobjc > 0 && Tcl_GetIntFromObj(interp, bobjv[0], &i0) != TCL_OK)
	      return TCL_ERROR;

	    if (bobjc > 1 && Tcl_GetIntFromObj(interp, bobjv[1], &i1) != TCL_OK)
	      return TCL_ERROR;

	    if (bobjc > 2 && Tcl_GetIntFromObj(interp, bobjv[2], &i2) != TCL_OK)
	      return TCL_ERROR;

	    b0 = i0 & 0Xff;
	    b1 = i1 & 0Xff;
	    b2 = i2 & 0Xff;
	  
	    if (addMidiShortEvent(&hdrPtr->data[0], MidiStreamHeaderDataSize, 
				  &hdrPtr->size, dt, b0, b1, b2)) {
	      Tcl_SetResult(interp, "could not add short event", TCL_STATIC);
	      return TCL_ERROR;
	    }
	  } else {
	    if (err = addMidiLongEvent(interp, &hdrPtr->data[0], 
				       MidiStreamHeaderDataSize, 
				       &hdrPtr->size, dt, bobjc, bobjv)) {
	      if (err == 2) /* non-int in byte list */
		return TCL_ERROR;
	      else {
		Tcl_SetResult(interp, "could not add long event", TCL_STATIC);
		return TCL_ERROR;
	      }
	    }
	  }

	  break;
	}
      case STRHMTD_ADD_LONG: /* adds list of bytes */
	{
	  int idt = 0;
	  DWORD dt;
	  int bobjc;
	  Tcl_Obj** bobjv;

	  if (objc != 5) {
	    Tcl_WrongNumArgs(interp, 3, objv, 
			     "deltaTime byteList");
	    return TCL_ERROR;
	  }

	  if (Tcl_GetIntFromObj(interp, objv[3], &idt) != TCL_OK)
	    return TCL_ERROR;

	  if (Tcl_ListObjGetElements(interp, objv[4], &bobjc, &bobjv) != TCL_OK)
	    return TCL_ERROR;

	  dt = idt;
	  
	  if (err = addMidiLongEvent(interp, &hdrPtr->data[0], 
				     MidiStreamHeaderDataSize, 
				     &hdrPtr->size, dt, bobjc, bobjv)) {
	    if (err == 2) /* non-int in byte list */
	      return TCL_ERROR;
	    else {
	      Tcl_SetResult(interp, "could not add long event", TCL_STATIC);
	      return TCL_ERROR;
	    }
	  }

	  break;
	}
      case STRHMTD_ADD_NOP: /* adds nop event */
	{
	  int idt = 0;
	  DWORD dt;

	  if (objc != 4) {
	    Tcl_WrongNumArgs(interp, 3, objv, 
			     "deltaTime");
	    return TCL_ERROR;
	  }

	  if (Tcl_GetIntFromObj(interp, objv[3], &idt) != TCL_OK)
	    return TCL_ERROR;

	  dt = idt;
	  
	  if (addMidiNopEvent(&hdrPtr->data[0], MidiStreamHeaderDataSize, 
			      &hdrPtr->size, dt)) {
	    Tcl_SetResult(interp, "could not add nop event", TCL_STATIC);
	    return TCL_ERROR;
	  }

	  break;
	}
      case STRHMTD_ADD_SHORT: /* adds 1, 2 or 3 bytes */
	{
	  int idt = 0;
	  int i0 = 0;
	  int i1 = 0;
	  int i2 = 0;
	  DWORD dt;
	  unsigned char b0, b1, b2;

	  if (objc < 5 || objc > 7) {
	    Tcl_WrongNumArgs(interp, 3, objv, 
			     "deltaTime byte0 ?byte1 ?byte2??");
	    return TCL_ERROR;
	  }

	  if (Tcl_GetIntFromObj(interp, objv[3], &idt) != TCL_OK)
	    return TCL_ERROR;

	  if (Tcl_GetIntFromObj(interp, objv[4], &i0) != TCL_OK)
	    return TCL_ERROR;

	  if (objc > 5 && Tcl_GetIntFromObj(interp, objv[5], &i1) != TCL_OK)
	    return TCL_ERROR;

	  if (objc > 6 && Tcl_GetIntFromObj(interp, objv[6], &i2) != TCL_OK)
	    return TCL_ERROR;

	  dt = idt;
	  b0 = i0 & 0Xff;
	  b1 = i1 & 0Xff;
	  b2 = i2 & 0Xff;
	  
	  if (addMidiShortEvent(&hdrPtr->data[0], MidiStreamHeaderDataSize, 
				&hdrPtr->size, dt, b0, b1, b2)) {
	    Tcl_SetResult(interp, "could not add short event", TCL_STATIC);
	    return TCL_ERROR;
	  }

	  break;
	}
      case STRHMTD_ADD_TEMPO: /* sets tempo */
	{
	  int idt = 0;
	  int itempo;
	  DWORD dt;
	  DWORD tempo;

	  if (objc != 5) {
	    Tcl_WrongNumArgs(interp, 3, objv, 
			     "deltaTime tempo");
	    return TCL_ERROR;
	  }

	  if (Tcl_GetIntFromObj(interp, objv[3], &idt) != TCL_OK)
	    return TCL_ERROR;

	  if (Tcl_GetIntFromObj(interp, objv[4], &itempo) != TCL_OK)
	    return TCL_ERROR;

	  dt = idt;
	  tempo = itempo;
	  
	  if (addMidiTempoEvent(&hdrPtr->data[0], MidiStreamHeaderDataSize, 
				&hdrPtr->size, dt, tempo)) {
	    Tcl_SetResult(interp, "could not add tempo event", TCL_STATIC);
	    return TCL_ERROR;
	  }

	  break;
	}
      }
      break;
    }
  case STRHMTD_DESTROY:
    if (hdrPtr->dev)
      if (err = midiOutUnprepareHeader(*(HMIDIOUT*)(hdrPtr->dev), 
				       &(hdrPtr->hdr), sizeof(MIDIHDR))) {
	ReportMidiError(interp, "Failed to unprepare header", err);
	return TCL_ERROR;  
      }
    Tcl_DeleteCommand(interp, Tcl_GetString(objv[0]));
    ckfree((char*)hdrPtr);
    break;
  }
  
  return TCL_OK;
}

int StreamOutHeader(ClientData     clientData, 
		    Tcl_Interp*    interp,
		    int            objc,
		    Tcl_Obj* const objv[]) 
{
  struct MidiStreamHeader* hdrPtr = 
    (struct MidiStreamHeader*)ckalloc(sizeof(struct MidiStreamHeader));
  char hdrName[18 + TCL_INTEGER_SPACE];

  hdrPtr->hdr.dwFlags = 0;
  hdrPtr->hdr.lpData = 0;
  hdrPtr->hdr.dwBufferLength = 0;
  hdrPtr->size = 0;
  hdrPtr->dev = 0;

  sprintf(hdrName, "::midistreamheader%d", headerCnt++);

  Tcl_CreateObjCommand(interp, hdrName,
		       (Tcl_ObjCmdProc*)StreamOutHeaderObjCmd,
		       (ClientData) hdrPtr, (Tcl_CmdDeleteProc *) NULL);

  Tcl_SetObjResult(interp, Tcl_NewStringObj(hdrName, strlen(hdrName)));

  return TCL_OK;
}

int Devs(ClientData     clientData, 
	 Tcl_Interp*    interp,
	 int            objc,
	 Tcl_Obj* const objv[]) 
{
    UINT numDevs;
    UINT i;
    Tcl_Obj* devObjv;
    MIDIOUTCAPS cap; 
    numDevs = midiOutGetNumDevs();
    devObjv = Tcl_NewListObj(0, NULL);
    for (i = 0; i < numDevs; i++)
	if (!midiOutGetDevCaps(i, &cap, sizeof(MIDIOUTCAPS)))
	    Tcl_ListObjAppendElement(interp, devObjv, Tcl_NewStringObj(cap.szPname, strlen(cap.szPname)));
    Tcl_SetObjResult(interp, devObjv);
    return TCL_OK;
}

DLLEXPORT int Midistreamdll_Init(Tcl_Interp *interp)
{
  if (Tcl_InitStubs(interp, "8.1", 0) == NULL) {
    return TCL_ERROR;
  }
  if (Tcl_PkgRequire(interp, "Tcl", "8.1", 0) == NULL) {
    return TCL_ERROR;
  }
  if (Tcl_PkgProvide(interp, "midistreamdll", "0.4") != TCL_OK) {
    return TCL_ERROR;
  }

  Tcl_CreateObjCommand(interp, "midistreamdll::open",
		       (Tcl_ObjCmdProc*)StreamOutOpen,
		       (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);

  Tcl_CreateObjCommand(interp, "midistreamdll::header",
		       (Tcl_ObjCmdProc*)StreamOutHeader,
		       (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);

  Tcl_CreateObjCommand(interp, "midistreamdll::devs",
		       (Tcl_ObjCmdProc*)Devs,
		       (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);

  return TCL_OK;
}
 
