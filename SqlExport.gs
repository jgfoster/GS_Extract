! ------------------- Class definition for SqlExport
expectvalue /Class
doit
Object subclass: 'SqlExport'
  instVarNames: #( classes counter debug files fileSystem
                    methodClass objects objectTableFile path
                    visited)
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
addObject: anObject

	[
		Exception category: GemStoneError number: 2115 do: [:ex :cat :num :args |
			GsFile stdout nextPutAll: 'Object ignored due to security error'; cr.
			^self
		].
		anObject isSpecial ifTrue: [^self].
		anObject isBehavior ifTrue: [^self].
		(anObject isKindOf: methodClass) ifTrue: [^self].
		(anObject isKindOf: AbstractCollisionBucket) ifTrue: [^self].
	] value.

	(self haveSeen: anObject) ifTrue: [^self].

	objectTableFile
		nextPutAll: 'o_';
		nextPutAll: anObject asOop printString;
		nextPut: Character tab;
		nextPutAll: anObject class name;
		cr.

	self exportObject: anObject.
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

	objects add: anObject.
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
haveSeen: anObject

	| bitIndex byte byteIndex flag mod offset oop |
	SmallInteger maximumValue == 16r1FFFFFFF ifTrue: [
		mod := 4.
		offset := 0.
	] ifFalse: [
		mod := 8.
		offset := 1.
	].
	oop := anObject asOop.
	oop \\ mod == 1 ifFalse: [
		self error: 'We don''t understand oops'.
	].
	bitIndex := oop - 1 // mod.
	byteIndex := bitIndex // 8.
	bitIndex := bitIndex \\ 8.
	byte := visited at: byteIndex.
	flag := byte bitAt: bitIndex + offset.
	flag == 1 ifTrue: [
		^true
	].
	visited at: byteIndex put: (byte bitOr: (1 bitShift: bitIndex)).
	counter := counter + 1.
	counter \\ 10000 == 0 ifTrue: [
		GsFile stdout nextPutAll: 'Object count = ' , counter printString; cr.
	].
	^false
%
category: 'other'
method: SqlExport
initialize: aGlobal to: aPath with: aFileSystem debug: aBoolean

	| time |
	GsFile stdout nextPutAll: 'Max object count = ', (System _oopHighWaterMark // 4) printString; cr.
	debug := aBoolean.
	objects := OrderedCollection with: aGlobal.
	methodClass := (Globals includesKey: #'GsNMethod')
		ifTrue: [Globals at: #'GsNMethod']
		ifFalse: [Globals at: #'GsMethod'].
	counter := 0.
	fileSystem := aFileSystem.
	path := aPath.
	path last == $/ ifTrue: [path := path copyFrom: 1 to: path size - 1].
	visited := ByteArray new: 20000000.
	files := Dictionary new.
	objectTableFile := self openAppend: path, '/object_table.txt' withHeader: [:f |
			f
				nextPutAll: 'OOP'; nextPut: Character tab;
				nextPutAll: 'ClassName';
				cr.
		].
	time := Time millisecondsElapsedTime: [
		[objects notEmpty] whileTrue: [
			self addObject: objects removeLast.
		].
	].
	objectTableFile close.
	files do: [:each | each close].
	GsFile stdout nextPutAll: 'Elapsed time = ' , (time // 60) printString , ' seconds'; cr.
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
path
	^path
%
category: 'other'
method: SqlExport
path: aPath

	path := aPath
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
