/* use -module netica.ffi */
/* %module netica */

%{
#include "Netica.h"
/* #include "NeticaEx.h" */
%}

%include "typemaps.i"

/* Handling output parameters */

/* NOTE: typemap doesn't work, manually fixed */
%typemap(ffitype) char *OUTPUT "(:array :char *mesg-len-ns*)";

%apply char *OUTPUT { char* mesg };
%inline %{
	int InitNetica2_bn (environ_ns* env, char* mesg);
	int CloseNetica_bn (environ_ns* env, char* mesg);
%}
%clear char* mesg;

%apply int *OUTPUT { int* length };
%inline %{
	const char* GetNetUserField_bn (const net_bn* net, const char* name, int* length, int kind);
	void GetNetNthUserField_bn (const net_bn* net, int index, const char** OUTPUT, const char** OUTPUT, int* length, int kind); /* NOTE: doesn't work, manually fixed */
	const char* GetNodeUserField_bn (const node_bn* node, const char* name, int* length, int kind);
	void GetNodeNthUserField_bn (const node_bn* node, int index, const char** OUTPUT, const char** OUTPUT, int* length, int kind); /* NOTE: doesn't work, manually fixed */ 
%}
%clear int* length;

int GetNeticaVersion_bn (const environ_ns* env, const char** OUTPUT); /* NOTE: doesn't work, manually fixed */

void GetAppWindowPosition_ns (environ_ns* env, int* OUTPUT, int* OUTPUT, int* OUTPUT, int* OUTPUT, int* OUTPUT);

const char* GetStreamContents_ns (stream_ns* stream, long* OUTPUT);

void ReadNetFindings2_bn (caseposn_bn* case_posn, stream_ns* file, bool_ns add, const nodelist_bn* nodes, long* INOUT, double* INOUT);

bool_ns HasNodeTable_bn (const node_bn* node, bool_ns* OUTPUT);

void GetNodeVisPosition_bn (const node_bn* node, void* vis, double* OUTPUT, double* OUTPUT);

/* NOTE: better let user allocate the output array */
/* void MapStateList_bn (const state_bn* src_states,  const nodelist_bn* src_nodes, state_bn* OUTPUT, const nodelist_bn* dest_nodes); */
/* int GetChars_ns (const char* str, int index, unsigned short* OUTPUT, int num); */
/* double GenerateRandomNumbers_ns (randgen_ns* rand, double* OUTPUT, int num, const char* options); */
/* void MostProbableConfig_bn (const nodelist_bn* nodes, state_bn* OUTPUT, int nth); */


%include "Netica.h"
/* %include "NeticaEx.h" */

