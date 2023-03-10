
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
  return &counters_[counters_in_use_++];
}


void Shell::MapCounters(const char* name) {
  counters_file_ = i::OS::MemoryMappedFile::create(name,
    sizeof(CounterCollection), &local_counters_);
  void* memory = (counters_file_ == NULL) ?
      NULL : counters_file_->memory();
  if (memory == NULL) {
    printf("Could not map counters file %s\n", name);
    exit(1);
  }
  counters_ = static_cast<CounterCollection*>(memory);
  V8::SetCounterFunction(LookupCounter);
  V8::SetCreateHistogramFunction(CreateHistogram);
  V8::SetAddHistogramSampleFunction(AddHistogramSample);
}


int CounterMap::Hash(const char* name) {
  int h = 0;
  int c;
  while ((c = *name++) != 0) {
    h += h << 5;
    h += c;
  }
  return h;
}


Counter* Shell::GetCounter(const char* name, bool is_histogram) {
  Counter* counter = counter_map_->Lookup(name);

  if (counter == NULL) {
    counter = counters_->GetNextCounter();
    if (counter != NULL) {
      counter_map_->Set(name, counter);
      counter->Bind(name, is_histogram);
    }
  } else {
    ASSERT(counter->is_histogram() == is_histogram);
  }
  return counter;
}


int* Shell::LookupCounter(const char* name) {
  Counter* counter = GetCounter(name, false);

  if (counter != NULL) {
    return counter->ptr();
  } else {
    return NULL;
  }
}


void* Shell::CreateHistogram(const char* name,
                             int min,
                             int max,
                             size_t buckets) {
  return GetCounter(name, true);
}


void Shell::AddHistogramSample(void* histogram, int sample) {
  Counter* counter = reinterpret_cast<Counter*>(histogram);
  counter->AddSample(sample);
}

void Shell::InstallUtilityScript() {
  Locker lock;
  HandleScope scope;
  // If we use the utility context, we have to set the security tokens so that
  // utility, evaluation and debug context can all access each other.
  utility_context_->SetSecurityToken(Undefined());
  evaluation_context_->SetSecurityToken(Undefined());
  Context::Scope utility_scope(utility_context_);

#ifdef ENABLE_DEBUGGER_SUPPORT
  // Install the debugger object in the utility scope
  i::Debug* debug = i::Isolate::Current()->debug();
  debug->Load();
  i::Handle<i::JSObject> js_debug
      = i::Handle<i::JSObject>(debug->debug_context()->global());
  utility_context_->Global()->Set(String::New("$debug"),
                                  Utils::ToLocal(js_debug));
  debug->debug_context()->set_security_token(HEAP->undefined_value());
#endif

  // Run the d8 shell utility script in the utility context
  int source_index = i::NativesCollection<i::D8>::GetIndex("d8");
  i::Vector<const char> shell_source =
      i::NativesCollection<i::D8>::GetRawScriptSource(source_index);
  i::Vector<const char> shell_source_name =
      i::NativesCollection<i::D8>::GetScriptName(source_index);
  Handle<String> source = String::New(shell_source.start(),
      shell_source.length());
  Handle<String> name = String::New(shell_source_name.start(),
      shell_source_name.length());
  Handle<Script> script = Script::Compile(source, name);
  script->Run();

  // Mark the d8 shell script as native to avoid it showing up as normal source
  // in the debugger.
  i::Handle<i::Object> compiled_script = Utils::OpenHandle(*script);
  i::Handle<i::Script> script_object = compiled_script->IsJSFunction()
     ? i::Handle<i::Script>(i::Script::cast(
         i::JSFunction::cast(*compiled_script)->shared()->script()))
     : i::Handle<i::Script>(i::Script::cast(
         i::SharedFunctionInfo::cast(*compiled_script)->script()));
  script_object->set_type(i::Smi::FromInt(i::Script::TYPE_NATIVE));
}


#ifdef COMPRESS_STARTUP_DATA_BZ2
class BZip2Decompressor : public v8::StartupDataDecompressor {
 public:
  virtual ~BZip2Decompressor() { }

 protected:
  virtual int DecompressData(char* raw_data,
                             int* raw_data_size,
                             const char* compressed_data,
                             int compressed_data_size) {
    ASSERT_EQ(v8::StartupData::kBZip2,
              v8::V8::GetCompressedStartupDataAlgorithm());
    unsigned int decompressed_size = *raw_data_size;
    int result =
        BZ2_bzBuffToBuffDecompress(raw_data,
                                   &decompressed_size,
                                   const_cast<char*>(compressed_data),
                                   compressed_data_size,
                                   0, 1);
    if (result == BZ_OK) {
      *raw_data_size = decompressed_size;
    }
    return result;
  }
};
#endif

Handle<ObjectTemplate> Shell::CreateGlobalTemplate() {
  Handle<ObjectTemplate> global_template = ObjectTemplate::New();
  global_template->Set(String::New("print"), FunctionTemplate::New(Print));
  global_template->Set(String::New("write"), FunctionTemplate::New(Write));
  global_template->Set(String::New("read"), FunctionTemplate::New(Read));
  global_template->Set(String::New("readline"),
                       FunctionTemplate::New(ReadLine));
  global_template->Set(String::New("load"), FunctionTemplate::New(Load));
  global_template->Set(String::New("quit"), FunctionTemplate::New(Quit));
  global_template->Set(String::New("version"), FunctionTemplate::New(Version));

  // Bind the handlers for external arrays.
  global_template->Set(String::New("Int8Array"),
                       FunctionTemplate::New(Int8Array));
  global_template->Set(String::New("Uint8Array"),
                       FunctionTemplate::New(Uint8Array));
  global_template->Set(String::New("Int16Array"),
                       FunctionTemplate::New(Int16Array));
  global_template->Set(String::New("Uint16Array"),
                       FunctionTemplate::New(Uint16Array));
  global_template->Set(String::New("Int32Array"),
                       FunctionTemplate::New(Int32Array));
  global_template->Set(String::New("Uint32Array"),
                       FunctionTemplate::New(Uint32Array));
  global_template->Set(String::New("Float32Array"),
                       FunctionTemplate::New(Float32Array));
  global_template->Set(String::New("Float64Array"),
                       FunctionTemplate::New(Float64Array));
  global_template->Set(String::New("PixelArray"),
                       FunctionTemplate::New(PixelArray));

#ifdef LIVE_OBJECT_LIST
  global_template->Set(String::New("lol_is_enabled"), Boolean::New(true));
#else
  global_template->Set(String::New("lol_is_enabled"), Boolean::New(false));
#endif

  Handle<ObjectTemplate> os_templ = ObjectTemplate::New();
  AddOSMethods(os_templ);
  global_template->Set(String::New("os"), os_templ);

  return global_template;
}


void Shell::Initialize(bool test_shell) {
#ifdef COMPRESS_STARTUP_DATA_BZ2
  BZip2Decompressor startup_data_decompressor;
  int bz2_result = startup_data_decompressor.Decompress();
  if (bz2_result != BZ_OK) {
    fprintf(stderr, "bzip error code: %d\n", bz2_result);
    exit(1);
  }
#endif

  Shell::counter_map_ = new CounterMap();
  // Set up counters
  if (i::StrLength(i::FLAG_map_counters) != 0)
    MapCounters(i::FLAG_map_counters);
  if (i::FLAG_dump_counters) {
    V8::SetCounterFunction(LookupCounter);
    V8::SetCreateHistogramFunction(CreateHistogram);
    V8::SetAddHistogramSampleFunction(AddHistogramSample);
  }

  if (test_shell) return;

  Locker lock;
  HandleScope scope;
  Handle<ObjectTemplate> global_template = CreateGlobalTemplate();
  utility_context_ = Context::New(NULL, global_template);

#ifdef ENABLE_DEBUGGER_SUPPORT
  // Start the debugger agent if requested.
  if (i::FLAG_debugger_agent) {
    v8::Debug::EnableAgent("d8 shell", i::FLAG_debugger_port, true);
  }

  // Start the in-process debugger if requested.
  if (i::FLAG_debugger && !i::FLAG_debugger_agent) {
    v8::Debug::SetDebugEventListener(HandleDebugEvent);
  }
#endif
}


void Shell::RenewEvaluationContext() {
  // Initialize the global objects
  HandleScope scope;
  Handle<ObjectTemplate> global_template = CreateGlobalTemplate();

  // (Re-)create the evaluation context
  if (!evaluation_context_.IsEmpty()) {
    evaluation_context_.Dispose();
  }
  evaluation_context_ = Context::New(NULL, global_template);
  Context::Scope utility_scope(evaluation_context_);

  i::JSArguments js_args = i::FLAG_js_arguments;
  i::Handle<i::FixedArray> arguments_array =
      FACTORY->NewFixedArray(js_args.argc());
  for (int j = 0; j < js_args.argc(); j++) {
    i::Handle<i::String> arg =
        FACTORY->NewStringFromUtf8(i::CStrVector(js_args[j]));
    arguments_array->set(j, *arg);
  }
  i::Handle<i::JSArray> arguments_jsarray =
      FACTORY->NewJSArrayWithElements(arguments_array);
  evaluation_context_->Global()->Set(String::New("arguments"),
                                     Utils::ToLocal(arguments_jsarray));
}


void Shell::OnExit() {
  if (i::FLAG_dump_counters) {
    ::printf("+----------------------------------------+-------------+\n");
    ::printf("| Name                                   | Value       |\n");
    ::printf("+----------------------------------------+-------------+\n");
    for (CounterMap::Iterator i(counter_map_); i.More(); i.Next()) {
      Counter* counter = i.CurrentValue();
      if (counter->is_histogram()) {
        ::printf("| c:%-36s | %11i |\n", i.CurrentKey(), counter->count());
        ::printf("| t:%-36s | %11i |\n",
                 i.CurrentKey(),
                 counter->sample_total());
      } else {
        ::printf("| %-38s | %11i |\n", i.CurrentKey(), counter->count());
      }
    }
    ::printf("+----------------------------------------+-------------+\n");
  }
  if (counters_file_ != NULL)
    delete counters_file_;
}


static char* ReadChars(const char* name, int* size_out) {
  v8::Unlocker unlocker;  // Release the V8 lock while reading files.
  FILE* file = i::OS::FOpen(name, "rb");
  if (file == NULL) return NULL;

  fseek(file, 0, SEEK_END);
  int size = ftell(file);
  rewind(file);

  char* chars = new char[size + 1];
  chars[size] = '\0';
  for (int i = 0; i < size;) {
    int read = fread(&chars[i], 1, size - i, file);
    i += read;
  }
  fclose(file);
  *size_out = size;
  return chars;
}


static char* ReadToken(char* data, char token) {
  char* next = i::OS::StrChr(data, token);
  if (next != NULL) {
    *next = '\0';
    return (next + 1);
  }

  return NULL;
}


static char* ReadLine(char* data) {
  return ReadToken(data, '\n');
}


static char* ReadWord(char* data) {
  return ReadToken(data, ' ');
}


// Reads a file into a v8 string.
Handle<String> Shell::ReadFile(const char* name) {
  int size = 0;
  char* chars = ReadChars(name, &size);
  if (chars == NULL) return Handle<String>();
  Handle<String> result = String::New(chars);
  delete[] chars;
  return result;
}


void Shell::RunShell() {
  LineEditor* editor = LineEditor::Get();
  printf("V8 version %s [console: %s]\n", V8::GetVersion(), editor->name());
  if (i::FLAG_debugger) {
    printf("JavaScript debugger enabled\n");
  }

  editor->Open();
  while (true) {
    Locker locker;
    HandleScope handle_scope;
    Context::Scope context_scope(evaluation_context_);
    i::SmartPointer<char> input = editor->Prompt(Shell::kPrompt);
    if (input.is_empty())
      break;
    editor->AddHistory(*input);
    Handle<String> name = String::New("(d8)");
    ExecuteString(String::New(*input), name, true, true);
  }
  editor->Close();
  printf("\n");
}


class ShellThread : public i::Thread {
 public:
  ShellThread(int no, i::Vector<const char> files)
    : Thread("d8:ShellThread"),
      no_(no), files_(files) { }
  virtual void Run();
 private:
  int no_;
  i::Vector<const char> files_;
};


void ShellThread::Run() {
  // Prepare the context for this thread.
  Locker locker;
  HandleScope scope;
  Handle<ObjectTemplate> global_template = Shell::CreateGlobalTemplate();

  char* ptr = const_cast<char*>(files_.start());
  while ((ptr != NULL) && (*ptr != '\0')) {
    // For each newline-separated line.
    char* next_line = ReadLine(ptr);

    if (*ptr == '#') {
      // Skip comment lines.
      ptr = next_line;
      continue;
    }

    Persistent<Context> thread_context = Context::New(NULL, global_template);
    Context::Scope context_scope(thread_context);

    while ((ptr != NULL) && (*ptr != '\0')) {
      char* filename = ptr;
      ptr = ReadWord(ptr);

      // Skip empty strings.
      if (strlen(filename) == 0) {
        break;
      }

      Handle<String> str = Shell::ReadFile(filename);
      if (str.IsEmpty()) {
        printf("WARNING: %s not found\n", filename);
        break;
      }

      Shell::ExecuteString(str, String::New(filename), false, false);
    }

    thread_context.Dispose();
    ptr = next_line;
  }
}

int Shell::RunMain(int argc, char* argv[], bool* executed) {
  // Default use preemption if threads are created.
  bool use_preemption = true;

  // Default to use lowest possible thread preemption interval to test as many
  // edgecases as possible.
  int preemption_interval = 1;

  i::List<i::Thread*> threads(1);

  {
    // Since the thread below may spawn new threads accessing V8 holding the
    // V8 lock here is mandatory.
    Locker locker;
    RenewEvaluationContext();
    Context::Scope context_scope(evaluation_context_);
    for (int i = 1; i < argc; i++) {
      char* str = argv[i];
      if (strcmp(str, "--preemption") == 0) {
        use_preemption = true;
      } else if (strcmp(str, "--no-preemption") == 0) {
        use_preemption = false;
      } else if (strcmp(str, "--preemption-interval") == 0) {
        if (i + 1 < argc) {
          char* end = NULL;
          preemption_interval = strtol(argv[++i], &end, 10);  // NOLINT
          if (preemption_interval <= 0 || *end != '\0' || errno == ERANGE) {
            printf("Invalid value for --preemption-interval '%s'\n", argv[i]);
            return 1;
          }
        } else {
          printf("Missing value for --preemption-interval\n");
          return 1;
        }
      } else if (strcmp(str, "-f") == 0) {
        // Ignore any -f flags for compatibility with other stand-alone
        // JavaScript engines.
        continue;
      } else if (strncmp(str, "--", 2) == 0) {
        printf("Warning: unknown flag %s.\nTry --help for options\n", str);
      } else if (strcmp(str, "-e") == 0 && i + 1 < argc) {
        // Execute argument given to -e option directly.
        v8::HandleScope handle_scope;
        v8::Handle<v8::String> file_name = v8::String::New("unnamed");
        v8::Handle<v8::String> source = v8::String::New(argv[++i]);
        (*executed) = true;
        if (!ExecuteString(source, file_name, false, true)) {
          OnExit();
          return 1;
        }
      } else if (strcmp(str, "-p") == 0 && i + 1 < argc) {
        int size = 0;
        const char* files = ReadChars(argv[++i], &size);
        if (files == NULL) return 1;
        ShellThread* thread =
            new ShellThread(threads.length(),
                            i::Vector<const char>(files, size));
        thread->Start();
        threads.Add(thread);
        (*executed) = true;
      } else {
        // Use all other arguments as names of files to load and run.
        HandleScope handle_scope;
        Handle<String> file_name = v8::String::New(str);
        Handle<String> source = ReadFile(str);
        (*executed) = true;
        if (source.IsEmpty()) {
          printf("Error reading '%s'\n", str);
          return 1;
        }
        if (!ExecuteString(source, file_name, false, true)) {
          OnExit();
          return 1;
        }
      }
    }

    // Start preemption if threads have been created and preemption is enabled.
    if (threads.length() > 0 && use_preemption) {
      Locker::StartPreemption(preemption_interval);
    }
  }

  for (int i = 0; i < threads.length(); i++) {
    i::Thread* thread = threads[i];
    thread->Join();
    delete thread;
  }
  OnExit();
  return 0;
 }


int Shell::Main(int argc, char* argv[]) {
  // Figure out if we're requested to stress the optimization
  // infrastructure by running tests multiple times and forcing
  // optimization in the last run.
  bool FLAG_stress_opt = false;
  bool FLAG_stress_deopt = false;
  bool FLAG_interactive_shell = false;
  bool FLAG_test_shell = false;
  bool script_executed = false;

  for (int i = 0; i < argc; i++) {
    if (strcmp(argv[i], "--stress-opt") == 0) {
      FLAG_stress_opt = true;
      argv[i] = NULL;
    } else if (strcmp(argv[i], "--stress-deopt") == 0) {
      FLAG_stress_deopt = true;
      argv[i] = NULL;
    } else if (strcmp(argv[i], "--noalways-opt") == 0) {
      // No support for stressing if we can't use --always-opt.
      FLAG_stress_opt = false;
      FLAG_stress_deopt = false;
    } else if (strcmp(argv[i], "--shell") == 0) {
      FLAG_interactive_shell = true;
      argv[i] = NULL;
    } else if (strcmp(argv[i], "--test") == 0) {
      FLAG_test_shell = true;
      argv[i] = NULL;
    }
  }

  v8::V8::SetFlagsFromCommandLine(&argc, argv, true);

  Initialize(FLAG_test_shell);

  int result = 0;
  if (FLAG_stress_opt || FLAG_stress_deopt) {
    v8::Testing::SetStressRunType(
        FLAG_stress_opt ? v8::Testing::kStressTypeOpt
            : v8::Testing::kStressTypeDeopt);
    int stress_runs = v8::Testing::GetStressRuns();
    for (int i = 0; i < stress_runs && result == 0; i++) {
      printf("============ Stress %d/%d ============\n", i + 1, stress_runs);
      v8::Testing::PrepareStressRun(i);
      result = RunMain(argc, argv, &script_executed);
    }
    printf("======== Full Deoptimization =======\n");
    v8::Testing::DeoptimizeAll();
  } else {
    result = RunMain(argc, argv, &script_executed);
  }

#ifdef ENABLE_DEBUGGER_SUPPORT
  // Run remote debugger if requested, but never on --test
  if (i::FLAG_remote_debugger && !FLAG_test_shell) {
    InstallUtilityScript();
    RunRemoteDebugger(i::FLAG_debugger_port);
    return 0;
  }
#endif

  // Run interactive shell if explicitly requested or if no script has been
  // executed, but never on --test
  if ((FLAG_interactive_shell || !script_executed) && !FLAG_test_shell) {
    InstallUtilityScript();
    RunShell();
  }

  v8::V8::Dispose();

  return result;
}

 }  // namespace v8


#ifndef GOOGLE3
int main(int argc, char* argv[]) {
  return v8::Shell::Main(argc, argv);
}
#endif