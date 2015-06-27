package org.ensime.jerk

import java.io.File

import org.scalatest._

import org.ensime.api._

import spray.json._
import org.ensime.json._

import pimpathon.file._

class JerkFormatsSpec extends FlatSpec with Matchers
    with SprayJsonTestSupport with EnsimeTestData {
  import JerkFormats._

  // workaround the fact that we have tests on the contents of the
  // envelope, but marshallers at the higher level (really the tests
  // should be updated).
  def roundtrip(
    value: RpcRequest,
    via: String
  ): Unit = {
    val json = RpcRequestEnvelope(value, 999).toJson.asJsObject
    json.fields("req") shouldBe via.parseJson
    json.convertTo[RpcRequestEnvelope].req shouldBe value
  }

  def roundtrip(
    value: RpcResponse,
    via: String
  ): Unit = {
    val json = (RpcResponseEnvelope(999, value): EnsimeServerMessage).toJson.asJsObject
    json.fields("payload") shouldBe via.parseJson
    json.convertTo[EnsimeServerMessage] should matchPattern {
      case RpcResponseEnvelope(999, recovered) if recovered == value =>
    }
  }

  def roundtrip(
    value: EnsimeEvent,
    via: String
  ): Unit = roundtrip(value: EnsimeServerMessage, Some(via))

  "Jerk Formats" should "roundtrip startup messages" in {
    roundtrip(
      ConnectionInfoReq,
      """{"typehint":"ConnectionInfoReq"}"""
    )
  }

  it should "unmarshal RpcSearchRequests" in {
    roundtrip(
      PublicSymbolSearchReq(List("foo", "bar"), 10),
      """{"typehint":"PublicSymbolSearchReq","keywords":["foo","bar"],"maxResults":10}"""
    )

    roundtrip(
      ImportSuggestionsReq(file1, 1, List("foo", "bar"), 10),
      s"""{"point":1,"maxResults":10,"names":["foo","bar"],"typehint":"ImportSuggestionsReq","file":"$file1"}"""
    )
  }

  it should "unmarshal RpcAnalyserRequests" in {
    roundtrip(
      RemoveFileReq(file1),
      s"""{"typehint":"RemoveFileReq","file":"$file1"}"""
    )

    roundtrip(
      TypecheckFileReq(sourceFileInfo),
      s"""{"typehint":"TypecheckFileReq","fileInfo":{"file":"$file1","contents":"{/* code here */}","contentsIn":"$file2"}}"""
    )

    roundtrip(
      TypecheckFilesReq(List(file1, file2)),
      s"""{"typehint":"TypecheckFilesReq","files":["$file1","$file2"]}"""
    )

    roundtrip(
      UnloadAllReq,
      """{"typehint":"UnloadAllReq"}"""
    )

    roundtrip(
      TypecheckAllReq,
      """{"typehint":"TypecheckAllReq"}"""
    )

    roundtrip(
      FormatSourceReq(List(file1, file2)),
      s"""{"typehint":"FormatSourceReq","files":["$file1","$file2"]}"""
    )

    roundtrip(
      FormatOneSourceReq(sourceFileInfo),
      s"""{"typehint":"FormatOneSourceReq","file":{"file":"$file1","contents":"{/* code here */}","contentsIn":"$file2"}}"""
    )

    roundtrip(
      DocUriAtPointReq(file1, OffsetRange(1, 10)),
      s"""{"typehint":"DocUriAtPointReq","file":"$file1","point":{"from":1,"to":10}}"""
    )

    roundtrip(
      DocUriForSymbolReq("foo.bar", Some("Baz"), None),
      """{"typehint":"DocUriForSymbolReq","typeFullName":"foo.bar","memberName":"Baz"}"""
    )

    roundtrip(
      CompletionsReq(sourceFileInfo, 10, 100, true, false),
      s"""{"point":10,"maxResults":100,"typehint":"CompletionsReq","caseSens":true,"fileInfo":{"file":"$file1","contents":"{/* code here */}","contentsIn":"$file2"},"reload":false}"""
    )

    roundtrip(
      PackageMemberCompletionReq("foo", "bar"),
      """{"typehint":"PackageMemberCompletionReq","path":"foo","prefix":"bar"}"""
    )

    roundtrip(
      CallCompletionReq(13),
      """{"typehint":"CallCompletionReq","id":13}"""
    )

    roundtrip(
      UsesOfSymbolAtPointReq(file1, 100),
      s"""{"typehint":"UsesOfSymbolAtPointReq","file":"$file1","point":100}"""
    )

    roundtrip(
      TypeByIdReq(13),
      """{"typehint":"TypeByIdReq","id":13}"""
    )

    roundtrip(
      TypeByNameReq("foo.bar"),
      """{"typehint":"TypeByNameReq","name":"foo.bar"}"""
    )

    roundtrip(
      TypeByNameAtPointReq("foo.bar", file1, OffsetRange(1, 10)),
      s"""{"typehint":"TypeByNameAtPointReq","name":"foo.bar","file":"$file1","range":{"from":1,"to":10}}"""
    )

    roundtrip(
      TypeAtPointReq(file1, OffsetRange(1, 100)),
      s"""{"typehint":"TypeAtPointReq","file":"$file1","range":{"from":1,"to":100}}"""
    )

    roundtrip(
      InspectTypeAtPointReq(file1, OffsetRange(1, 100)),
      s"""{"typehint":"InspectTypeAtPointReq","file":"$file1","range":{"from":1,"to":100}}"""
    )

    roundtrip(
      InspectTypeByIdReq(13),
      """{"typehint":"InspectTypeByIdReq","id":13}"""
    )

    roundtrip(
      InspectTypeByNameReq("foo.Bar"),
      """{"typehint":"InspectTypeByNameReq","name":"foo.Bar"}"""
    )

    roundtrip(
      SymbolAtPointReq(file1, 101),
      s"""{"typehint":"SymbolAtPointReq","file":"$file1","point":101}"""
    )

    roundtrip(
      SymbolByNameReq("foo.Bar", Some("baz"), None),
      """{"typehint":"SymbolByNameReq","typeFullName":"foo.Bar","memberName":"baz"}"""
    )

    roundtrip(
      InspectPackageByPathReq("foo.bar"),
      """{"typehint":"InspectPackageByPathReq","path":"foo.bar"}"""
    )

    roundtrip(
      PrepareRefactorReq(1, 'ignored, RenameRefactorDesc("bar", file1, 1, 100), false),
      s"""{"tpe":"ignored","procId":1,"params":{"newName":"bar","typehint":"RenameRefactorDesc","end":100,"file":"$file1","start":1},"typehint":"PrepareRefactorReq","interactive":false}"""
    )

    roundtrip(
      ExecRefactorReq(1, RefactorType.Rename),
      """{"typehint":"ExecRefactorReq","procId":1,"tpe":{"typehint":"Rename"}}"""
    )

    roundtrip(
      CancelRefactorReq(1),
      """{"typehint":"CancelRefactorReq","procId":1}"""
    )

    roundtrip(
      SymbolDesignationsReq(
        file1, 1, 100,
        List(ObjectSymbol, ValSymbol)
      ),
      s"""{"requestedTypes":[{"typehint":"ObjectSymbol"},{"typehint":"ValSymbol"}],"typehint":"SymbolDesignationsReq","end":100,"file":"$file1","start":1}"""
    )

    roundtrip(
      ExpandSelectionReq(file1, 100, 200),
      s"""{"typehint":"ExpandSelectionReq","file":"$file1","start":100,"end":200}"""
    )

    roundtrip(
      ImplicitInfoReq(file1, OffsetRange(0, 123)),
      s"""{"typehint":"ImplicitInfoReq","file":"$file1","range":{"from":0,"to":123}}"""
    )
  }

  it should "roundtrip RpcDebugRequests" in {
    roundtrip(
      DebugActiveVmReq,
      """{"typehint":"DebugActiveVmReq"}"""
    )

    roundtrip(
      DebugStartReq("blah blah blah"),
      """{"typehint":"DebugStartReq","commandLine":"blah blah blah"}"""
    )

    roundtrip(
      DebugAttachReq("mylovelyhorse", "13"),
      """{"typehint":"DebugAttachReq","hostname":"mylovelyhorse","port":"13"}"""
    )

    roundtrip(
      DebugStopReq,
      """{"typehint":"DebugStopReq"}"""
    )

    roundtrip(
      DebugSetBreakReq(file1, 13),
      s"""{"typehint":"DebugSetBreakReq","file":"$file1","line":13}"""
    )

    roundtrip(
      DebugClearBreakReq(file1, 13),
      s"""{"typehint":"DebugClearBreakReq","file":"$file1","line":13}"""
    )

    roundtrip(
      DebugClearAllBreaksReq,
      """{"typehint":"DebugClearAllBreaksReq"}"""
    )

    roundtrip(
      DebugListBreakpointsReq,
      """{"typehint":"DebugListBreakpointsReq"}"""
    )

    roundtrip(
      DebugRunReq,
      """{"typehint":"DebugRunReq"}"""
    )

    roundtrip(
      DebugContinueReq(dtid),
      """{"typehint":"DebugContinueReq","threadId":13}"""
    )

    roundtrip(
      DebugStepReq(dtid),
      """{"typehint":"DebugStepReq","threadId":13}"""
    )

    roundtrip(
      DebugNextReq(dtid),
      """{"typehint":"DebugNextReq","threadId":13}"""
    )

    roundtrip(
      DebugStepOutReq(dtid),
      """{"typehint":"DebugStepOutReq","threadId":13}"""
    )

    roundtrip(
      DebugLocateNameReq(dtid, "foo"),
      """{"typehint":"DebugLocateNameReq","threadId":13,"name":"foo"}"""
    )

    roundtrip(
      DebugValueReq(debugLocationArray),
      """{"typehint":"DebugValueReq","loc":{"typehint":"DebugArrayElement","objectId":{"id":13},"index":14}}"""
    )

    roundtrip(
      DebugToStringReq(dtid, debugLocationArray),
      """{"typehint":"DebugToStringReq","threadId":13,"loc":{"typehint":"DebugArrayElement","objectId":{"id":13},"index":14}}"""
    )

    roundtrip(
      DebugSetValueReq(debugLocationArray, "bar"),
      """{"typehint":"DebugSetValueReq","loc":{"typehint":"DebugArrayElement","objectId":{"id":13},"index":14},"newValue":"bar"}"""
    )

    roundtrip(
      DebugBacktraceReq(dtid, 100, 200),
      """{"typehint":"DebugBacktraceReq","threadId":13,"index":100,"count":200}"""
    )

  }

  it should "roundtrip EnsimeGeneralEvent as EnsimeEvent" in {
    roundtrip(
      SendBackgroundMessageEvent("ABCDEF", 1),
      """{"typehint":"SendBackgroundMessageEvent","detail":"ABCDEF","code":1}"""
    )

    roundtrip(
      AnalyzerReadyEvent,
      """{"typehint":"AnalyzerReadyEvent"}"""
    )

    roundtrip(
      FullTypeCheckCompleteEvent,
      """{"typehint":"FullTypeCheckCompleteEvent"}"""
    )

    roundtrip(
      IndexerReadyEvent,
      """{"typehint":"IndexerReadyEvent"}"""
    )

    roundtrip(
      NewScalaNotesEvent(
        isFull = false,
        List(new Note("foo.scala", "testMsg", NoteWarn, 50, 55, 77, 5))
      ),
      """{"typehint":"NewScalaNotesEvent","isFull":false,"notes":[{"beg":50,"line":77,"col":5,"end":55,"file":"foo.scala","msg":"testMsg","severity":{"typehint":"NoteWarn"}}]}"""
    )

    roundtrip(
      ClearAllScalaNotesEvent,
      """{"typehint":"ClearAllScalaNotesEvent"}"""
    )
  }

  it should "roundtrip DebugEvent as EnsimeEvent" in {
    roundtrip(
      DebugOutputEvent("XXX"),
      """{"typehint":"DebugOutputEvent","body":"XXX"}"""
    )

    roundtrip(
      DebugStepEvent(DebugThreadId(207), "threadNameStr", sourcePos1.file, sourcePos1.line),
      // why is the typehint not the first entry?
      s"""{"line":57,"typehint":"DebugStepEvent","file":"$file1","threadName":"threadNameStr","threadId":207}"""
    )

    roundtrip(
      DebugBreakEvent(DebugThreadId(209), "threadNameStr", sourcePos1.file, sourcePos1.line),
      s"""{"line":57,"typehint":"DebugBreakEvent","file":"$file1","threadName":"threadNameStr","threadId":209}"""
    )

    roundtrip(
      DebugVMStartEvent,
      """{"typehint":"DebugVMStartEvent"}"""
    )
    roundtrip(
      DebugVMDisconnectEvent,
      """{"typehint":"DebugVMDisconnectEvent"}"""
    )
    roundtrip(
      DebugExceptionEvent(33L, dtid, "threadNameStr", Some(sourcePos1.file), Some(sourcePos1.line)),
      s"""{"line":57,"exception":33,"typehint":"DebugExceptionEvent","file":"$file1","threadName":"threadNameStr","threadId":13}"""
    )
    roundtrip(
      DebugExceptionEvent(33L, dtid, "threadNameStr", None, None),
      """{"typehint":"DebugExceptionEvent","exception":33,"threadId":13,"threadName":"threadNameStr"}"""
    )

    roundtrip(
      DebugThreadStartEvent(dtid),
      """{"typehint":"DebugThreadStartEvent","threadId":13}"""
    )
    roundtrip(
      DebugThreadDeathEvent(dtid),
      """{"typehint":"DebugThreadDeathEvent","threadId":13}"""
    )
  }

  it should "roundtrip DebugLocation" in {
    roundtrip(
      DebugObjectReference(57L),
      """{"typehint":"DebugObjectReference","objectId":{"id":57}}"""
    )

    roundtrip(
      DebugArrayElement(DebugObjectId(58L), 2),
      """{"typehint":"DebugArrayElement","objectId":{"id":58},"index":2}"""
    )

    roundtrip(
      DebugObjectField(DebugObjectId(58L), "fieldName"),
      """{"typehint":"DebugObjectField","objectId":{"id":58},"field":"fieldName"}"""
    )

    roundtrip(
      DebugStackSlot(DebugThreadId(27), 12, 23),
      """{"typehint":"DebugStackSlot","threadId":27,"frame":12,"offset":23}"""
    )
  }

  it should "roundtrip DebugValue" in {
    roundtrip(
      DebugPrimitiveValue("summaryStr", "typeNameStr"),
      """{"typehint":"DebugPrimitiveValue","summary":"summaryStr","typeName":"typeNameStr"}"""
    )

    roundtrip(
      DebugStringInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)),
      """{"typehint":"DebugStringInstance","typeName":"typeNameStr","fields":[{"index":19,"name":"nameStr","typeName":"typeNameStr","summary":"summaryStr"}],"objectId":{"id":5},"summary":"summaryStr"}"""
    )

    roundtrip(
      DebugObjectInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)),
      """{"typehint":"DebugObjectInstance","typeName":"typeNameStr","fields":[{"index":19,"name":"nameStr","typeName":"typeNameStr","summary":"summaryStr"}],"objectId":{"id":5},"summary":"summaryStr"}"""
    )

    roundtrip(
      DebugNullValue("typeNameStr"),
      """{"typehint":"DebugNullValue","typeName":"typeNameStr"}"""
    )

    roundtrip(
      DebugArrayInstance(3, "typeName", "elementType", DebugObjectId(5L)),
      """{"elementTypeName":"elementType","typehint":"DebugArrayInstance","typeName":"typeName","length":3,"objectId":{"id":5}}"""
    )

    roundtrip(
      debugClassField,
      """{"typehint":"DebugClassField","index":19,"name":"nameStr","typeName":"typeNameStr","summary":"summaryStr"}"""
    )

    roundtrip(
      debugStackLocal1,
      """{"typehint":"DebugStackLocal","index":3,"name":"name1","summary":"summary1","typeName":"type1"}"""
    )

    roundtrip(
      debugStackFrame,
      s"""{"typehint":"DebugStackFrame","thisObjectId":{"id":7},"methodName":"method1","locals":[{"index":3,"name":"name1","summary":"summary1","typeName":"type1"},{"index":4,"name":"name2","summary":"summary2","typeName":"type2"}],"pcLocation":{"file":"$file1","line":57},"className":"class1","numArgs":4,"index":7}"""
    )

    roundtrip(
      DebugBacktrace(List(debugStackFrame), dtid, "thread1"),
      s"""{"typehint":"DebugBacktrace","frames":[{"thisObjectId":{"id":7},"methodName":"method1","locals":[{"index":3,"name":"name1","summary":"summary1","typeName":"type1"},{"index":4,"name":"name2","summary":"summary2","typeName":"type2"}],"pcLocation":{"file":"$file1","line":57},"className":"class1","numArgs":4,"index":7}],"threadId":13,"threadName":"thread1"}"""
    )

    roundtrip(
      sourcePos1,
      s"""{"typehint":"LineSourcePosition","file":"$file1","line":57}"""
    )
    roundtrip(
      sourcePos2,
      s"""{"typehint":"LineSourcePosition","file":"$file1","line":59}"""
    )
    roundtrip(
      sourcePos3,
      """{"typehint":"EmptySourcePosition"}"""
    )
    roundtrip(
      sourcePos4,
      s"""{"typehint":"OffsetSourcePosition","file":"$file1","offset":456}"""
    )

    roundtrip(
      breakPoint1,
      s"""{"typehint":"Breakpoint","file":"$file1","line":57}"""
    )

    roundtrip(
      BreakpointList(List(breakPoint1), List(breakPoint2)),
      s"""{"typehint":"BreakpointList","active":[{"file":"$file1","line":57}],"pending":[{"file":"$file1","line":59}]}"""
    )

    roundtrip(
      DebugVmSuccess(),
      """{"typehint":"DebugVmSuccess","status":"success"}"""
    )

    roundtrip(
      DebugVmError(303, "xxxx"),
      """{"typehint":"DebugVmError","errorCode":303,"details":"xxxx","status":"error"}"""
    )
  }

  it should "roundtrip various informational types" in {
    roundtrip(
      note1,
      """{"typehint":"Note","beg":23,"line":19,"col":8,"end":33,"file":"file1","msg":"note1","severity":{"typehint":"NoteError"}}"""
    )

    roundtrip(
      completionInfo,
      """{"typehint":"CompletionInfo","name":"name","typeId":88,"typeSig":{"sections":[[["abc","def"],["hij","lmn"]]],"result":"ABC"},"relevance":90,"isCallable":false,"toInsert":"BAZ"}"""
    )

    roundtrip(
      completionInfo2,
      """{"typehint":"CompletionInfo","name":"name2","typeId":90,"typeSig":{"sections":[[["abc","def"]]],"result":"ABC"},"relevance":91,"isCallable":true}"""
    )

    roundtrip(
      CompletionInfoList("fooBar", List(completionInfo)),
      """{"typehint":"CompletionInfoList","prefix":"fooBar","completions":[{"name":"name","typeId":88,"typeSig":{"sections":[[["abc","def"],["hij","lmn"]]],"result":"ABC"},"relevance":90,"isCallable":false,"toInsert":"BAZ"}]}"""
    )

    roundtrip(
      new SymbolInfo("name", "localName", None, typeInfo, false, Some(2)),
      """{"typehint":"SymbolInfo","name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false,"ownerTypeId":2}"""
    )

    roundtrip(
      new NamedTypeMemberInfo("typeX", typeInfo, None, None, DeclaredAs.Method),
      """{"typehint":"NamedTypeMemberInfo","name":"typeX","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"declAs":{"typehint":"Method"}}"""
    )

    roundtrip(
      entityInfo,
      """{"resultType":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"name":"Arrow1","paramSections":[{"params":[["ABC",{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}]],"isImplicit":false}],"typehint":"ArrowTypeInfo","typeId":8}"""
    )

    roundtrip(
      typeInfo,
      """{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}"""
    )

    roundtrip(
      packageInfo,
      """{"typehint":"PackageInfo","name":"name","fullName":"fullName","members":[]}"""
    )

    roundtrip(
      new CallCompletionInfo(typeInfo, List(paramSectionInfo)),
      """{"typehint":"CallCompletionInfo","resultType":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"paramSections":[{"params":[["ABC",{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}]],"isImplicit":false}]}"""
    )

    roundtrip(
      interfaceInfo,
      """{"typehint":"InterfaceInfo","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"viaView":"DEF"}"""
    )

    roundtrip(
      new TypeInspectInfo(typeInfo, Some(1), List(interfaceInfo)),
      """{"typehint":"TypeInspectInfo","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"companionId":1,"interfaces":[{"type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"viaView":"DEF"}],"infoType":"typeInspect"}"""
    )
  }

  it should "support search related responses" in {
    roundtrip(
      new SymbolSearchResults(List(methodSearchRes, typeSearchRes)),
      s"""{"typehint":"SymbolSearchResults","syms":[{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"MethodSearchResult","ownerName":"ownerStr","declAs":{"typehint":"Method"}},{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"TypeSearchResult","declAs":{"typehint":"Trait"}}]}"""
    )

    roundtrip(
      new ImportSuggestions(List(List(methodSearchRes, typeSearchRes))),
      s"""{"typehint":"ImportSuggestions","symLists":[[{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"MethodSearchResult","ownerName":"ownerStr","declAs":{"typehint":"Method"}},{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"TypeSearchResult","declAs":{"typehint":"Trait"}}]]}"""
    )

    roundtrip(
      methodSearchRes,
      s"""{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"MethodSearchResult","ownerName":"ownerStr","declAs":{"typehint":"Method"}}"""
    )

    roundtrip(
      typeSearchRes,
      s"""{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"TypeSearchResult","declAs":{"typehint":"Trait"}}"""
    )
  }

  it should "support ranges and semantic highlighting" in {
    roundtrip(
      ERangePositions(ERangePosition(batchSourceFile, 75, 70, 90) :: Nil),
      s"""{"typehint":"ERangePositions","positions":[{"file":"/abc","offset":75,"start":70,"end":90}]}"""
    )

    roundtrip(
      FileRange("/abc", 7, 9),
      s"""{"typehint":"FileRange","file":"/abc","start":7,"end":9}"""
    )

    roundtrip(
      SymbolDesignations(
        symFile, List(
        SymbolDesignation(7, 9, VarFieldSymbol),
        SymbolDesignation(11, 22, ClassSymbol)
      )
      ),
      s"""{"typehint":"SymbolDesignations","file":"$symFile","syms":[{"start":7,"end":9,"symType":{"typehint":"VarFieldSymbol"}},{"start":11,"end":22,"symType":{"typehint":"ClassSymbol"}}]}"""
    )

    roundtrip(
      ImplicitInfos(List(ImplicitConversionInfo(5, 6, symbolInfo))),
      """{"typehint":"ImplicitInfos","infos":[{"typehint":"ImplicitConversionInfo","start":5,"end":6,"fun":{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false,"ownerTypeId":2}}]}"""
    )

    roundtrip(
      ImplicitInfos(List(ImplicitParamInfo(5, 6, symbolInfo, List(symbolInfo, symbolInfo), true))),
      """{"typehint":"ImplicitInfos","infos":[{"params":[{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false,"ownerTypeId":2},{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false,"ownerTypeId":2}],"typehint":"ImplicitParamInfo","fun":{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeId":7,"outerTypeId":8,"typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false,"ownerTypeId":2},"funIsImplicit":true,"end":6,"start":5}]}"""
    )

  }

  it should "support refactoring messages" in {
    roundtrip(
      RefactorFailure(7, "message"),
      """{"typehint":"RefactorFailure","procedureId":7,"reason":"message","status":"failure"}"""
    )

    roundtrip(
      refactorEffect,
      s"""{"typehint":"RefactorEffect","procedureId":9,"refactorType":{"typehint":"AddImport"},"changes":[{"text":"aaa","typehint":"TextEdit","to":7,"from":5,"file":"$file3"}],"status":"success"}"""
    )

    roundtrip(
      refactorResult,
      s"""{"typehint":"RefactorResult","procedureId":7,"refactorType":{"typehint":"AddImport"},"touchedFiles":["$file3","$file1"],"status":"success"}"""
    )

  }

  it should "support legacy raw response types" in {
    roundtrip(
      FalseResponse,
      """{"typehint":"FalseResponse"}"""
    )

    roundtrip(
      TrueResponse,
      """{"typehint":"TrueResponse"}"""
    )

    roundtrip(
      StringResponse("wibble"),
      """{"typehint":"StringResponse","text":"wibble"}"""
    )

    roundtrip(
      VoidResponse,
      """{"typehint":"VoidResponse"}"""
    )
  }
}
