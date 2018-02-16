
// Copyright 2011 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


#ifdef COMPRESS_STARTUP_DATA_BZ2
#include <bzlib.h>
#endif
#include <errno.h>
#include <stdlib.h>

#include "v8.h"

#include "d8.h"
#include "d8-debug.h"
#include "debug.h"
#include "api.h"
#include "natives.h"
#include "platform.h"


namespace v8 {


const char* Shell::kHistoryFileName = ".d8_history";
const char* Shell::kPrompt = "d8> ";


LineEditor *LineEditor::first_ = NULL;


LineEditor::LineEditor(Type type, const char* name)
    : type_(type),
      name_(name),
      next_(first_) {
  first_ = this;
}


LineEditor* LineEditor::Get() {
  LineEditor* current = first_;
  LineEditor* best = current;
  while (current != NULL) {
    if (current->type_ > best->type_)
      best = current;
    current = current->next_;
  }
  return best;
}


class DumbLineEditor: public LineEditor {
  public:
    DumbLineEditor() : LineEditor(LineEditor::DUMB, "dumb") { }
   virtual i::SmartPointer<char> Prompt(const char* prompt);
};


  static DumbLineEditor dumb_line_editor;


i::SmartPointer<char> DumbLineEditor::Prompt(const char* prompt) {
  static const int kBufferSize = 256;
  char buffer[kBufferSize];
  printf("%s", prompt);
  char* str = fgets(buffer, kBufferSize, stdin);
  return i::SmartPointer<char>(str ? i::StrDup(str) : str);
}


CounterMap* Shell::counter_map_;
i::OS::MemoryMappedFile* Shell::counters_file_ = NULL;
CounterCollection Shell::local_counters_;
CounterCollection* Shell::counters_ = &local_counters_;
Persistent<Context> Shell::utility_context_;
Persistent<Context> Shell::evaluation_context_;


bool CounterMap::Match(void* key1, void* key2) {
  const char* name1 = reinterpret_cast<const char*>(key1);
  const char* name2 = reinterpret_cast<const char*>(key2);
  return strcmp(name1, name2) == 0;
}


  // Converts a V8 value to a C string.
const char* Shell::ToCString(const v8::String::Utf8Value& value) {
  return *value ? *value : "<string conversion failed>";
}


// Executes a string within the current v8 context.
bool Shell::ExecuteString(Handle<String> source,
                          Handle<Value> name,
                          bool print_result,
                          bool report_exceptions) {
  HandleScope handle_scope;
  TryCatch try_catch;
  if (i::FLAG_debugger) {
    // When debugging make exceptions appear to be uncaught.
    try_catch.SetVerbose(true);
  }
  Handle<Script> script = Script::Compile(source, name);
  if (script.IsEmpty()) {
    // Print errors that happened during compilation.
    if (report_exceptions && !i::FLAG_debugger)
      ReportException(&try_catch);
    return false;
  } else {
    Handle<Value> result = script->Run();
    if (result.IsEmpty()) {
      ASSERT(try_catch.HasCaught());
      // Print errors that happened during execution.
      if (report_exceptions && !i::FLAG_debugger)
        ReportException(&try_catch);
      return false;
    } else {
      ASSERT(!try_catch.HasCaught());
      if (print_result && !result->IsUndefined()) {
        // If all went well and the result wasn't undefined then print
        // the returned value.
        v8::String::Utf8Value str(result);
        const char* cstr = ToCString(str);
        printf("%s\n", cstr);
      }
      return true;
    }
  }
}


Handle<Value> Shell::Print(const Arguments& args) {
  Handle<Value> val = Write(args);
  printf("\n");
  return val;
}


 Handle<Value> Shell::Write(const Arguments& args) {
  for (int i = 0; i < args.Length(); i++) {
    HandleScope handle_scope;
    if (i != 0) {
      printf(" ");
    }
    v8::String::Utf8Value str(args[i]);
     int n = fwrite(*str, sizeof(**str), str.length(), stdout);
    if (n != str.length()) {
      printf("Error in fwrite\n");
      exit(1);
    }
  }
  return Undefined();
 }


 Handle<Value> Shell::Read(const Arguments& args) {
  String::Utf8Value file(args[0]);
  if (*file == NULL) {
   return ThrowException(String::New("Error loading file"));
  }
  Handle<String> source = ReadFile(*file);
  if (source.IsEmpty()) {
    return ThrowException(String::New("Error loading file"));
  }
  return source;
 }


Handle<Value> Shell::ReadLine(const Arguments& args) {
  i::SmartPointer<char> line(i::ReadLine(""));
  if (*line == NULL) {
    return Null();
  }
 size_t len = strlen(*line);
  if (len > 0 && line[len - 1] == '\n') {
    --len;
  }
  return String::New(*line, len);
}


Handle<Value> Shell::Load(const Arguments& args) {
  for (int i = 0; i < args.Length(); i++) {
    HandleScope handle_scope;
    String::Utf8Value file(args[i]);
    if (*file == NULL) {
      return ThrowException(String::New("Error loading file"));
    }
    Handle<String> source = ReadFile(*file);
    if (source.IsEmpty()) {
      return ThrowException(String::New("Error loading file"));
    }
    if (!ExecuteString(source, String::New(*file), false, false)) {
      return ThrowException(String::New("Error executing file"));
    }
  }
  return Undefined();
}


 Handle<Value> Shell::CreateExternalArray(const Arguments& args,
                                         ExternalArrayType type,
                                         int element_size) {
  if (args.Length() != 1) {
    return ThrowException(
        String::New("Array constructor needs one parameter."));
  }
  int length = args[0]->Int32Value();
  void* data = malloc(length * element_size);
  memset(data, 0, length * element_size);
  Handle<Object> array = Object::New();
  Persistent<Object> persistent_array = Persistent<Object>::New(array);
  persistent_array.MakeWeak(data, ExternalArrayWeakCallback);
  persistent_array.MarkIndependent();
  array->SetIndexedPropertiesToExternalArrayData(data, type, length);
  array->Set(String::New("length"), Int32::New(length), ReadOnly);
  array->Set(String::New("BYTES_PER_ELEMENT"), Int32::New(element_size));
  return array;
 }


void Shell::ExternalArrayWeakCallback(Persistent<Value> object, void* data) {
  free(data);
  object.Dispose();
}


Handle<Value> Shell::Int8Array(const Arguments& args) {
  return CreateExternalArray(args, v8::kExternalByteArray, sizeof(int8_t));
}


Handle<Value> Shell::Uint8Array(const Arguments& args) {
  return CreateExternalArray(args, kExternalUnsignedByteArray, sizeof(uint8_t));
}


Handle<Value> Shell::Int16Array(const Arguments& args) {
  return CreateExternalArray(args, kExternalShortArray, sizeof(int16_t));
}


Handle<Value> Shell::Uint16Array(const Arguments& args) {
  return CreateExternalArray(args, kExternalUnsignedShortArray,
                             sizeof(uint16_t));
}


Handle<Value> Shell::Int32Array(const Arguments& args) {
  return CreateExternalArray(args, kExternalIntArray, sizeof(int32_t));
}


Handle<Value> Shell::Uint32Array(const Arguments& args) {
  return CreateExternalArray(args, kExternalUnsignedIntArray, sizeof(uint32_t));
}


Handle<Value> Shell::Float32Array(const Arguments& args) {
  return CreateExternalArray(args, kExternalFloatArray,
                             sizeof(float));  // NOLINT
}


Handle<Value> Shell::Float64Array(const Arguments& args) {
  return CreateExternalArray(args, kExternalDoubleArray,
                             sizeof(double));  // NOLINT
}


Handle<Value> Shell::PixelArray(const Arguments& args) {
  return CreateExternalArray(args, kExternalPixelArray, sizeof(uint8_t));
}


Handle<Value> Shell::Yield(const Arguments& args) {
  v8::Unlocker unlocker;
  return Undefined();
}


Handle<Value> Shell::Quit(const Arguments& args) {
  int exit_code = args[0]->Int32Value();
  OnExit();
  exit(exit_code);
  return Undefined();
}


Handle<Value> Shell::Version(const Arguments& args) {
  return String::New(V8::GetVersion());
}


void Shell::ReportException(v8::TryCatch* try_catch) {
  HandleScope handle_scope;
  v8::String::Utf8Value exception(try_catch->Exception());
  const char* exception_string = ToCString(exception);
  Handle<Message> message = try_catch->Message();
  if (message.IsEmpty()) {
    // V8 didn't provide any extra information about this error; just
    // print the exception.
    printf("%s\n", exception_string);
  } else {
    // Print (filename):(line number): (message).
    v8::String::Utf8Value filename(message->GetScriptResourceName());
    const char* filename_string = ToCString(filename);
    int linenum = message->GetLineNumber();
    printf("%s:%i: %s\n", filename_string, linenum, exception_string);
    // Print line of source code.
    v8::String::Utf8Value sourceline(message->GetSourceLine());
    const char* sourceline_string = ToCString(sourceline);
    printf("%s\n", sourceline_string);
    // Print wavy underline (GetUnderline is deprecated).
    int start = message->GetStartColumn();
    for (int i = 0; i < start; i++) {
      printf(" ");
    }
    int end = message->GetEndColumn();
    for (int i = start; i < end; i++) {
      printf("^");
    }
    printf("\n");
    v8::String::Utf8Value stack_trace(try_catch->StackTrace());
    if (stack_trace.length() > 0) {
      const char* stack_trace_string = ToCString(stack_trace);
      printf("%s\n", stack_trace_string);
    }
  }
}


Handle<Array> Shell::GetCompletions(Handle<String> text, Handle<String> full) {
  HandleScope handle_scope;
  Context::Scope context_scope(utility_context_);
  Handle<Object> global = utility_context_->Global();
  Handle<Value> fun = global->Get(String::New("GetCompletions"));
  static const int kArgc = 3;
  Handle<Value> argv[kArgc] = { evaluation_context_->Global(), text, full };
  Handle<Value> val = Handle<Function>::Cast(fun)->Call(global, kArgc, argv);
  return handle_scope.Close(Handle<Array>::Cast(val));
}


#ifdef ENABLE_DEBUGGER_SUPPORT
Handle<Object> Shell::DebugMessageDetails(Handle<String> message) {
  Context::Scope context_scope(utility_context_);
  Handle<Object> global = utility_context_->Global();
  Handle<Value> fun = global->Get(String::New("DebugMessageDetails"));
  static const int kArgc = 1;
  Handle<Value> argv[kArgc] = { message };
  Handle<Value> val = Handle<Function>::Cast(fun)->Call(global, kArgc, argv);
  return Handle<Object>::Cast(val);
}


Handle<Value> Shell::DebugCommandToJSONRequest(Handle<String> command) {
  Context::Scope context_scope(utility_context_);
  Handle<Object> global = utility_context_->Global();
  Handle<Value> fun = global->Get(String::New("DebugCommandToJSONRequest"));
  static const int kArgc = 1;
  Handle<Value> argv[kArgc] = { command };
  Handle<Value> val = Handle<Function>::Cast(fun)->Call(global, kArgc, argv);
  return val;
}
#endif


int32_t* Counter::Bind(const char* name, bool is_histogram) {
  int i;
  for (i = 0; i < kMaxNameSize - 1 && name[i]; i++)
    name_[i] = static_cast<char>(name[i]);
  name_[i] = '\0';
  is_histogram_ = is_histogram;
  return ptr();
}


void Counter::AddSample(int32_t sample) {
  count_++;
  sample_total_ += sample;
}


CounterCollection::CounterCollection() {
  magic_number_ = 0xDEADFACE;
  max_counters_ = kMaxCounters;
  max_name_size_ = Counter::kMaxNameSize;
  counters_in_use_ = 0;
}


Counter* CounterCollection::GetNextCounter() {
  if (counters_in_use_ == kMaxCounters) return NULL;