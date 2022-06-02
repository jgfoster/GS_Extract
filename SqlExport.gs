! ------------------- Class definition for SqlExport
expectvalue /Class
doit
Object subclass: 'SqlExport'
  instVarNames: #( counter debug
                    files fileSystem methodClass objects
                    path queue visited)
  classVars: #()
  classInstVars: #()
  poolDictionaries:	Array new
  inDictionary: UserGlobals
  constraints: Array new
  instancesInvariant: false
  isModifiable: false

%
expectvalue /Class
doit
SqlExport category: 'Kernel'
%
! ------------------- Remove existing behavior from SqlExport
doit
SqlExport removeAllMethods.
SqlExport class removeAllMethods.
%
! ------------------- Class methods for SqlExport
category: 'other'
classmethod: SqlExport
export: aGlobal to: aPath

	"
	SqlExport export: UserGlobals to: '/tmp/globals/'
	"

	^self export: aGlobal to: aPath gsFile: GsFile
%
category: 'other'
classmethod: SqlExport
export: aGlobal to: aPath gsFile: anObject

	^self export: aGlobal to: aPath with: anObject debug: false
%
category: 'other'
classmethod: SqlExport
export: aGlobal to: aPath gsFile: anObject debug: aBoolean

	"
	SqlExport exportTo: '/tmp/globals/'
	"

	self basicNew
		initialize: aGlobal to: aPath with: anObject debug: aBoolean;
		yourself.

	^true
%
category: 'other'
classmethod: SqlExport
exportTo: aPath

	"
	SqlExport exportTo: '/tmp/globals/'
	"

	^self export: UserGlobals to: aPath gsFile: GsFile debug: false
%
category: 'other'
classmethod: SqlExport
exportTo: aPath debug: aBoolean

	"
	SqlExport exportTo: '/tmp/globals/'
	"

	^self export: UserGlobals to: aPath gsFile: GsFile debug: aBoolean
%
category: 'other'
classmethod: SqlExport
exportTo: aPath gsFile: anObject

	"
	SqlExport exportTo: '/tmp/globals/'
	"

	^self export: UserGlobals to: aPath gsFile: anObject
%
! ------------------- Instance methods for SqlExport
category: 'other'
method: SqlExport
addObject: anObject toObjectTable: aFile

	| set |
	(self shouldIgnore: anObject) ifTrue: [^self].
	set := objects at: anObject class name ifAbsentPut: [IdentitySet new].
	(set includes: anObject) ifTrue: [^self].
	set add: anObject.
	aFile
		nextPutAll: 'o_';
		nextPutAll: anObject asOop printString;
		nextPut: Character tab;
		nextPutAll: anObject class name;
		cr.
	"Enqueue named instance variables"
	1 to: anObject class allInstVarNames size do: [:i |
		queue add: (anObject instVarAt: i).
	].
	"Enqueue numbered instance variables"
	(anObject class inheritsFrom: Collection) ifTrue: [
		(anObject class inheritsFrom: AbstractDictionary) ifTrue: [
			| token |
			token := Object new.
			anObject keys do: [:eachKey |
				| eachValue |
				eachValue := self
					from: anObject
					at: eachKey
					otherwise: token.
				queue add: eachKey.
				eachValue ~~ token ifTrue: [queue add: eachValue].
			].
		] ifFalse: [
			queue addAll: anObject asIdentitySet.
		].
	].
%
category: 'other'
method: SqlExport
exportDate: aDate to: aGsFile
  "yyyy-mm-dd"

	Exception category: GemStoneError number: nil do: [:ex :cat :num :args |
		aGsFile nextPutAll: 'd_' , aDate printString.
		^self
	].
	aGsFile nextPutAll: (aDate asStringUsingFormat: #(3 2 1 $- 1 1)).
%
category: 'other'
method: SqlExport
exportDateTime: aDateTime to: aGsFile
  "yyyy-mm-ddThh:mm:ss+zzzz"

  | string offset offsetHours offsetMinutes |

	Exception category: GemStoneError number: nil do: [:ex :cat :num :args |
		aGsFile nextPutAll: 'dt_' , (string isNil ifTrue: [aDateTime printString] ifFalse: [string]).
		^self
	].

  string := aDateTime asStringUsingFormat: #(3 2 1 $- 1 1 $: true true false true true).
  string at: 11 put: $T.
	offset := aDateTime _localOffset: aDateTime timeZone.
  string := string copyFrom: 1 to: 19.
  string add: (offset < 0
    ifTrue: [$-]
    ifFalse: [$+]).
  offset := offset abs // 60.
  offsetHours := offset // 60.
  offsetMinutes := offset \\ 60.
  offsetHours < 10 ifTrue: [string add: $0].
  string addAll: offsetHours printString.
  offsetMinutes < 10 ifTrue: [string add: $0].
  string addAll: offsetMinutes printString.
  aGsFile nextPutAll: string.
%
category: 'other'
method: SqlExport
exportDictionaryElements: anObject

	| file stream token |
	token := Object new.
	file := self openAppend: path , '/' , anObject class name , '_elements.txt' withHeader: [:f |
		f
			nextPutAll: 'OOP'; nextPut: Character tab;
			nextPutAll: 'key'; nextPut: Character tab;
			nextPutAll: 'value';
			cr.
	].
	stream := WriteStream on: String new.
	anObject keys do: [:eachKey |
		| eachValue |
		eachValue := self
			from: anObject
			at: eachKey
			otherwise: token.
		stream
			nextPutAll: 'o_';
			nextPutAll: anObject asOop printString;
			nextPut: Character tab.
		self exportObject: eachKey to: stream.
		stream nextPut: Character tab.
		eachValue == token ifTrue: [
			stream nextPutAll: '-1'
		] ifFalse: [
			self exportObject: eachValue to: stream.
		].
		stream cr.
	].
	file nextPutAll: stream contents.
%
category: 'other'
method: SqlExport
exportObject: anObject

	(anObject class inheritsFrom: SequenceableCollection) ifTrue: [
		self exportSequenceableCollectionElements: anObject.
		"For built-in collections we assume that the instance variables are private"
		(Globals includesKey: anObject class name) ifTrue: [^self].
		"If there are no instance variables, then no need to export object"
		anObject class allInstVarNames size == 0 ifTrue: [^self].
	].
	(anObject class inheritsFrom: AbstractDictionary) ifTrue: [
		self exportDictionaryElements: anObject.
		(Globals includesKey: anObject class name) ifTrue: [^self].
	].

	self exportRemainder: anObject.
%
category: 'other'
method: SqlExport
exportObject: anObject to: aStream

	Exception category: GemStoneError number: 2115 do: [:ex :cat :num :args |
		"This happens only if there is an error in the code that follows."
		aStream nextPutAll: 'hidden'.
		^self
	].

	aStream nextPut: Character tab.

	anObject == nil   ifTrue: [aStream nextPutAll: 'null'.  ^self].
	anObject == true  ifTrue: [aStream nextPutAll: 'true'.  ^self].
	anObject == false ifTrue: [aStream nextPutAll: 'false'. ^self].

	(anObject isKindOf: Number) ifTrue: [anObject printOn: aStream.	^self].

	(anObject isKindOf: DateTime) ifTrue: [
		self exportDateTime: anObject to: aStream.
		^self
	].

	(anObject isKindOf: Date) ifTrue: [
		self exportDate: anObject to: aStream.
		^self
	].

	(anObject class inheritsFrom: CharacterCollection) ifTrue: [
		self exportString: anObject to: aStream.
		^self
	].

	(anObject isKindOf: Character) ifTrue: [
		aStream nextPutAll: 'c_'.
		anObject printOn: aStream.
		^self
	].

	(anObject isKindOf: Fraction) ifTrue: [
		anObject asFloat printOn: aStream.
		^self
	].

	(anObject isKindOf: Time) ifTrue: [
		anObject printOn: aStream.
		^self
	].

	aStream nextPutAll: 'o_'.
	anObject asOop printOn: aStream.
	debug ifTrue: [
		aStream nextPut: $:; nextPutAll: anObject class name.
		((anObject isKindOf: Collection) and: [(anObject isKindOf: String) not]) ifTrue: [
			| classNames comma |
			classNames := IdentitySet new.
			anObject do: [:each | classNames add: each class name].
			comma := ''.
			aStream nextPut: $(.
			classNames do: [:each |
				aStream nextPutAll: comma; nextPutAll: each.
				comma := ','.
			].
			aStream nextPut: $).
		].
	]
%
category: 'other'
method: SqlExport
exportObjects

	self waitForPhaseOneToBeDone.
	"Phase two: export objects"
	[
		self exportOneSet.
	] whileTrue: [].
%
category: 'other'
method: SqlExport
exportOneSet
	"Answer true if we did an export, false if no (unlocked) sets left"

	self getDictionaryLock.
	objects do: [:eachSet |
		"Find an object set that isn't locked"
		(System writeLock: eachSet ifDenied: [#'denied'] ifChanged: [#'changed']) ~~ #'denied' ifTrue: [
			"Release the dictionary lock and update view"
			System commit ifFalse: [self error: 'Commit failed!'].
			eachSet do: [:each |
				self exportObject: each.
			].
			self getDictionaryLock.
			objects removeKey: (objects keyAtValue: eachSet).
			"Release dictionary and object sets lock"
			System commitAndReleaseLocks ifFalse: [self error: 'Commit failed!'].
			^true
		].
	].
	^false
%
category: 'other'
method: SqlExport
exportRemainder: anObject

	| file stream |
	file := self openAppend: path , '/' , anObject class name , '.txt' withHeader: [:f |
		f nextPutAll: 'OOP'.
		anObject class allInstVarNames do: [:eachName |
			f nextPut: Character tab; nextPutAll: eachName.
		].
		anObject class isIndexable ifTrue: [
			f nextPut: Character tab; nextPutAll: '_size'.
		].
		f cr.
	].
	stream := WriteStream on: String new.
	stream
		nextPutAll: 'o_';
		nextPutAll: anObject asOop printString.
	1 to: anObject class allInstVarNames size do: [:i |
		self exportObject: (anObject instVarAt: i) to: stream.
	].
	anObject class isIndexable ifTrue: [
		stream nextPut: Character tab; nextPutAll: anObject size printString.
	].
	stream cr.
	file nextPutAll: stream contents.
%
category: 'other'
method: SqlExport
exportSequenceableCollectionElements: anObject

	| file stream |
	file := self openAppend: path , '/' , anObject class name , '_elements.txt' withHeader: [:f |
		f
			nextPutAll: 'OOP'; nextPut: Character tab;
			nextPutAll: 'index'; nextPut: Character tab;
			nextPutAll: 'value';
			cr.
	].
	stream := WriteStream on: String new.
	1 to: anObject size do: [:i |
		stream
			nextPutAll: 'o_';
			nextPutAll: anObject asOop printString;
			nextPut: Character tab;
			nextPutAll: i printString;
			nextPut: Character tab;
			yourself.
		self exportObject: (anObject at: i) to: stream.
		stream cr.
	].
	file nextPutAll: stream contents.
%
category: 'other'
method: SqlExport
exportString: anObject to: aStream

	aStream nextPutAll: 's_'.
	1 to: anObject size do: [:i |
		| char |
		char := anObject at: i.
		char == $\ ifTrue: [
			aStream
				nextPutAll: '\\';
				yourself.
		] ifFalse: [
			| val |
			val := char asciiValue.
			(val < 32 or: [val > 126]) ifTrue: [
				aStream
					nextPut: $\;
					nextPutAll: val printString;
					nextPut: $;;
					yourself.
			] ifFalse: [
				aStream nextPut: char.
			].
		].
	].
%
category: 'other'
method: SqlExport
from: aDict at: aKey otherwise: anObject
	Exception category: GemStoneError number: nil do: [:ex :cat :num :args |
		| string |
		string := num == 2115 ifTrue: ['Hidden: '] ifFalse: ['Error ' , num printString , ': '].
		GsFile stdout nextPutAll: string , aDict class name , '(' , aDict asOop printString , ') at: ' , aKey printString; cr.
		^anObject
	].
	^aDict at: aKey
%
category: 'other'
method: SqlExport
getDictionaryLock

	[(System writeLock: objects ifDenied: [#'denied'] ifChanged: [#'changed']) == #'denied'] whileTrue: [
		(Delay forSeconds: 1) wait.
		System abortTransaction.
	].
	System commit ifFalse: [self error: 'Commit failed!'].
	System addToCommitReleaseLocksSet: objects.
%
category: 'other'
method: SqlExport
initialize: aGlobal to: aPath with: aFileSystem debug: aBoolean

	| objectTableFile time maxLength |
	UserGlobals at: #'sqlExport' put: self.
	System writeLock: self.
	counter := 0.
	debug := aBoolean.
	objects := SymbolDictionary new.
	queue := OrderedCollection with: aGlobal.
	methodClass := (Globals includesKey: #'GsNMethod')
		ifTrue: [Globals at: #'GsNMethod']
		ifFalse: [Globals at: #'GsMethod'].
	files := Dictionary new.
	fileSystem := aFileSystem.
	path := aPath.
	path last == $/ ifTrue: [path := path copyFrom: 1 to: path size - 1].
	System commitTransaction ifFalse: [self error: 'Commit failed!'].
	objectTableFile := self openAppend: path, '/object_table.txt' withHeader: [:f |
		f
			nextPutAll: 'OOP'; nextPut: Character tab;
			nextPutAll: 'ClassName';
			cr.
	].
	maxLength := 0.
	time := Time millisecondsElapsedTime: [
		[queue notEmpty] whileTrue: [
			maxLength < queue size ifTrue: [maxLength := queue size].
			"LIFO queue gives us depth-first traversal which should have fewer objects in the queue"
			self addObject: queue removeLast toObjectTable: objectTableFile.
			(counter := counter + 1) \\ 10000 == 0 ifTrue: [
				System commitTransaction ifFalse: [self error: 'Commit failed!'].
				counter \\ 100000 == 0 ifTrue: [
					GsFile stdoutServer nextPutAll: 'found object count: ' , counter printString; lf.
				].
			].
		].
	].
	objectTableFile close.
	GsFile stdoutServer nextPutAll: 'maximum queue length = ' , maxLength printString; lf.
	GsFile stdoutServer nextPutAll: 'export phase one took ' , (time // 60) printString , ' seconds'; lf.
	System commitAndReleaseLocks ifFalse: [self error: 'Commit failed!'].
	time := Time millisecondsElapsedTime: [
		self exportObjects.
	].
	GsFile stdoutServer nextPutAll: 'export phase two took ' , (time // 60) printString , ' seconds'; lf.
%
category: 'other'
method: SqlExport
openAppend: aFilePath withHeader: aBlock

	^files
		at: aFilePath
		ifAbsentPut: [
			| file hasHeader |
			hasHeader := GsFile existsOnServer: aFilePath.
			file := GsFile openAppendOnServer: aFilePath.
			file isNil ifTrue: [self error: GsFile serverErrorString].
			hasHeader ifFalse: [aBlock value: file].
			file
		]
%
category: 'other'
method: SqlExport
shouldIgnore: anObject

	Exception category: GemStoneError number: 2115 do: [:ex :cat :num :args |
		GsFile stdout nextPutAll: 'Object ignored due to security error'; cr.
		^true
	].
	anObject isSpecial ifTrue: [^true].
	anObject isBehavior ifTrue: [^true].
	(anObject isKindOf: methodClass) ifTrue: [^true].
	(anObject isKindOf: AbstractCollisionBucket) ifTrue: [^true].
	^false
%
category: 'other'
method: SqlExport
try: tryBlock ensure: ensureBlock

	| result |
	Exception category: GemStoneError number: nil do: [:ex :cat :num :args |
		GsFile stdout nextPutAll: 'A: Error number ' , num printString; nextPutAll: args printString; cr.
		ensureBlock value.
		^nil
	].

	result := tryBlock value.
	ensureBlock value.
	^result
%
category: 'other'
method: SqlExport
waitForPhaseOneToBeDone

	[(System lockKind: self) == #'write'] whileTrue: [
		(Delay forSeconds: 10) wait.
		System abortTransaction.
	].
%
