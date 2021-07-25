
#include "src/Codegen.h"

#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
namespace Kaleidoscope {

CodegenDriver::CodegenDriver(const char* src, size_t len) :
    src_(src), ctx_(), parser_(src, len) {}


void CodegenDriver::HandleToplevelExpression() {
  std::unique_ptr<FunctionAST> funcAst = parser_.ParseToplevelExpr();
  if (funcAst) {
    Function* funcIR = funcAst->codegen(ctx_);
    if (!funcIR)  return;
    fprintf(stderr, "Read toplevel expression.");
    funcIR->print(errs());
    fprintf(stderr, "\n");

    ctx_.JITCompileToplevel(funcIR->getName().begin());

    // remove the anonymous expression
  } else {
    parser_.get_next_token();
  }
}

void CodegenDriver::HandleExtern() {
  std::unique_ptr<PrototypeAST> externAst = parser_.ParseExtern();
  if (externAst) {
    Function* funcIR = externAst->codegen(ctx_);
    if (!funcIR)  return;
    fprintf(stderr, "Read extern prototype.");
    funcIR->print(errs());
    fprintf(stderr, "\n");
    // record the prototype in Codegen Context
    ctx_.add_protos(std::move(externAst));
  } else {
    parser_.get_next_token();
  }
}

void CodegenDriver::HandleDefinition() {
  std::unique_ptr<FunctionAST> funcAst = parser_.ParseDefinition();
  if (funcAst) {
    Function* funcIR = funcAst->codegen(ctx_);
    if (!funcIR)  return;
    fprintf(stderr, "Read definition.");
    funcIR->print(errs());
    fprintf(stderr, "\n");

    ctx_.add_module();
  } else {
    parser_.get_next_token();
  }
}

void CodegenDriver::run() {
  parser_.get_next_token();
  while (parser_.cur_token != Lexer::token_eof) {
    switch(parser_.cur_token) {
      case Lexer::token_def:
        HandleDefinition();
        break;
      case Lexer::token_extern:
        HandleExtern();
        break;
      case ';':
        parser_.get_next_token();
        break;
      default:
        HandleToplevelExpression();
    }
  }
}

void CodegenContext::InitializeModuleAndPassManager() {
  // open a new module
  // this may be optimized
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("MYJIT", *TheContext);
  TheModule->setDataLayout(TheJIT->getDataLayout());

  Builder = std::make_unique<IRBuilder<>>(*TheContext);

  TheFPM = std::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());

  TheFPM->add(createInstructionCombiningPass());
  TheFPM->add(createReassociatePass());
  TheFPM->add(createGVNPass());
  TheFPM->add(createCFGSimplificationPass());

  TheFPM->doInitialization();
}

void CodegenContext::doOptimization(Function& func) {
  TheFPM->run(func);
}

void CodegenContext::JITCompileToplevel(std::string func_name) {
  auto RT = TheJIT->getMainJITDylib().createResourceTracker();

  auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
  ExitOnErr(TheJIT->addModule(std::move(TSM), RT));

  InitializeModuleAndPassManager();

  auto ExprSymbol = ExitOnErr(TheJIT->lookup(func_name));
  // the LLVM JIT compiler matches the native platform ABI, this means that
  // you can just cast the result pointer to a function pointer of that type
  // and call it directly
  double (*fp)() = (double(*)())static_cast<intptr_t>(ExprSymbol.getAddress());
  fprintf(stderr, "Evaluated to %f\n", fp());

  ExitOnErr(RT->remove());
}

Function* CodegenContext::get_function(std::string name) {
  // check the module 
  // every time we do a jit compilation, TheContext and TheModule
  // will be reinitialized again, thus the parsed functions will be 
  // abandon. Therefore, we maintain a map to restore the Prototype.
  Function* result = TheModule->getFunction(name);
  if (result) return result;

  // check the FunctionProtos map
  auto entry = FunctionProtos.find(name);
  if (entry != FunctionProtos.end())
    return entry->second->codegen(*this);

  return nullptr;
}

void CodegenContext::add_protos(std::unique_ptr<PrototypeAST> proto) {
  FunctionProtos[proto->getName()] = std::move(proto);
}

void CodegenContext::add_module() {
  ExitOnErr(TheJIT->addModule(
      ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
  InitializeModuleAndPassManager();
}

// Question:
// 1> is a compiled function still available by calling TheJIT->lookup
//    after the module is reinitialized?
// -  The answer is yes. The Function is added to TheJIT after addModule
// -  in JITCompileToplevel. TheJIT can search functions across modules.

} // namespace Kaleidoscope
