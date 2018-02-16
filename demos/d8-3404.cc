
// Copyright 2009 the V8 project authors. All rights reserved.
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


#include <stdlib.h>
#include <errno.h>

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
const char* ToCString(const v8::String::Utf8Value& value) {
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
      // Print errors that happened during execution.
      if (report_exceptions && !i::FLAG_debugger)
        ReportException(&try_catch);
      return false;
    } else {
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
