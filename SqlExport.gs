! ------------------- Class definition for SqlExport
expectvalue /Class
doit
Object subclass: 'SqlExport'
  instVarNames: #( classes counter fileSystem methodClass
                    objects objectTableFile path visited)
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

	"
	SqlExport exportTo: '/tmp/globals/'
	"

	self basicNew
		initialize: aGlobal to: aPath with: anObject;
		yourself.

	^true
%
category: 'other'
classmethod: SqlExport
exportTo: aPath

	"
	SqlExport exportTo: '/tmp/globals/'
	"

	^self export: UserGlobals to: aPath gsFile: GsFile
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

	| file token |

	token := Object new.

	self try: [
		file := self openAppend: path, '/',anObject class name,'_elements.txt' withHeader: [:f |
			f
				nextPutAll: 'OOP'; nextPut: Character tab;
				nextPutAll: 'key'; nextPut: Character tab;
				nextPutAll: 'value';
				cr.
		].
		anObject keys do: [:eachKey |
			| eachValue |
			eachValue := self
				from: anObject
				at: eachKey
				otherwise: token.
			file
				nextPutAll: 'o_';
				nextPutAll: anObject asOop printString;
				nextPut: Character tab.
			self exportObject: eachKey to: file.
			file nextPut: Character tab.
			eachValue == token ifTrue: [
				file nextPutAll: '-1'
			] ifFalse: [
				self exportObject: eachValue to: file.
			].
			file cr.
		].
	] ensure: [file isNil ifFalse: [file close]].
%
category: 'other'
method: SqlExport
exportObject: anObject

	(anObject class inheritsFrom: CharacterCollection) ifTrue: [
		self exportStrings: anObject.
		^self
	].
	(anObject class inheritsFrom: SequenceableCollection) ifTrue: [
		self exportSequenceableCollectionElements: anObject.
	].
	(anObject class inheritsFrom: AbstractDictionary) ifTrue: [
		self exportDictionaryElements: anObject.
	].

	self exportRemainder: anObject.
%
category: 'other'
method: SqlExport
exportObject: anObject to: aGsFile

	Exception category: GemStoneError number: 2115 do: [:ex :cat :num :args |
		"This happens only if there is an error in the code that follows."
		aGsFile nextPutAll: 'hidden'.
		^self
	].

	aGsFile nextPut: Character tab.

	(anObject isKindOf: Integer) ifTrue: [
		anObject printOn: aGsFile.
		^self
	].

	(anObject isKindOf: Character) ifTrue: [
		aGsFile nextPutAll: 'c_'.
		anObject printOn: aGsFile.
		^self
	].

	(anObject isKindOf: Boolean) ifTrue: [
		anObject printOn: aGsFile.
		^self
	].

	anObject isNil ifTrue: [
		aGsFile nextPutAll: 'nil'.
		^self
	].

	objects add: anObject.
	aGsFile nextPutAll: 'o_'.
	anObject asOop printOn: aGsFile.
	aGsFile nextPut: $:; nextPutAll: anObject class name.
	((anObject isKindOf: Collection) and: [(anObject isKindOf: String) not]) ifTrue: [
		| classNames comma |
		classNames := IdentitySet new.
		anObject do: [:each | classNames add: each class name].
		comma := ''.
		aGsFile nextPut: $(.
		classNames do: [:each |
			aGsFile nextPutAll: comma; nextPutAll: each.
			comma := ','.
		].
		aGsFile nextPut: $).
	].
%
category: 'other'
method: SqlExport
exportRemainder: anObject
	| file |

	self try: [
		file := self openAppend: path, '/', anObject class name,'.txt' withHeader: [:f |
			f nextPutAll: 'OOP'.
			anObject class allInstVarNames do: [:eachName |
				f nextPut: Character tab; nextPutAll: eachName.
			].
			anObject class isIndexable ifTrue: [
				f nextPut: Character tab; nextPutAll: '_size'.
			].
			f cr.
		].
		file
			nextPutAll: 'o_';
			nextPutAll: anObject asOop printString.
		1 to: anObject class allInstVarNames size do: [:i |
			self exportObject: (anObject instVarAt: i) to: file.
		].
		anObject class isIndexable ifTrue: [
			file nextPut: Character tab; nextPutAll: anObject size printString.
		].
		file cr.
	] ensure: [file isNil ifFalse: [file close]].
%
category: 'other'
method: SqlExport
exportSequenceableCollectionElements: anObject

	| file |
	self try: [
		file := self openAppend: path, '/',anObject class name,'_elements.txt' withHeader: [:f |
			f
				nextPutAll: 'OOP'; nextPut: Character tab;
				nextPutAll: 'index'; nextPut: Character tab;
				nextPutAll: 'value';
				cr.
		].
		1 to: anObject size do: [:i |
			file
				nextPutAll: 'o_';
				nextPutAll: anObject asOop printString;
				nextPut: Character tab;
				nextPutAll: i printString;
				nextPut: Character tab;
				yourself.
			self exportObject: (anObject at: i) to: file.
			file cr.
		].
	] ensure: [file isNil ifFalse: [file close]].
%
category: 'other'
method: SqlExport
exportStrings: anObject

	| file |
	self try: [
		file := self openAppend: (path , '/' , anObject class name , '.txt') withHeader: [:f |
			f
				nextPutAll: 'OOP';
				nextPut: Character tab;
				nextPutAll: 'Value';
				cr.
		].
		file
			nextPutAll: 'o_';
			nextPutAll: anObject asOop printString;
			nextPut: Character tab;
			yourself.

		anObject do: [:char |
			| val |
			val := char asciiValue.
			val = 92 ifTrue: [
				file
					nextPut: val;
					nextPut: $\;
					yourself.
			] ifFalse: [
				(val < 32 or: [val > 126]) ifTrue: [
					file
						nextPut: $\;
						nextPutAll: val printString;
						nextPut: $;;
						yourself.
				] ifFalse: [
					file nextPut: val.
				].
			].
		].
		file cr.

	] ensure: [file isNil ifFalse: [file close]].
%
category: 'other'
method: SqlExport
from: aDict at: aKey otherwise: anObject
	Exception category: GemStoneError number: nil do: [:ex :cat :num :args |
		GsFile stdout nextPutAll: 'Error ' , num printString , ': OOP(' , aDict asOop printString , ') at: ' , aKey printString; cr.
		^anObject
	].
	^aDict at: aKey
%
category: 'other'
method: SqlExport
haveSeen: anObject

	| bitIndex byteIndex flag mod offset oop |
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
	flag := (visited at: byteIndex) bitAt: bitIndex + offset.
	flag == 1 ifTrue: [
		^true
	].
	visited at: byteIndex put: ((visited at: byteIndex) bitOr: (1 bitShift: bitIndex)).
	counter := counter + 1.
	counter \\ 10000 == 0 ifTrue: [
		System addAllToStoneLog: 'Object count = ' , counter printString.
	].
	^false
%
category: 'other'
method: SqlExport
initialize: aGlobal to: aPath with: aFileSystem

	System addAllToStoneLog: 'oopHighWaterMark = ', System _oopHighWaterMark printString.
	objects := OrderedCollection with: aGlobal.
	methodClass := (Globals includesKey: #'GsNMethod')
		ifTrue: [Globals at: #'GsNMethod']
		ifFalse: [Globals at: #'GsMethod'].
	counter := 0.
	fileSystem := aFileSystem.
	path := aPath.
	path last == $/ ifTrue: [path := path copyFrom: 1 to: path size - 1].
	visited := ByteArray new: 20000000.
	objectTableFile := self openAppend: path, '/object_table.txt' withHeader: [:f |
			f
				nextPutAll: 'OOP'; nextPut: Character tab;
				nextPutAll: 'ClassName';
				cr.
		].
	[objects notEmpty] whileTrue: [
		self addObject: objects removeLast.
	].
	objectTableFile close.
%
category: 'other'
method: SqlExport
openAppend: aFilePath withHeader: aBlock

	| file hasHeader |

	hasHeader := GsFile existsOnServer: aFilePath.
	file := GsFile openAppendOnServer: aFilePath.
	file isNil ifTrue: [self error: GsFile serverErrorString].
	hasHeader ifFalse: [aBlock value: file].

	^file
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
		GsFile stdout nextPutAll: 'A: Error number ' , num printString; cr.
		ensureBlock value.
		^nil
	].

	result := tryBlock value.
	ensureBlock value.
	^result
%
