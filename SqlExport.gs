! ------------------- Class definition for SqlExport
expectvalue /Class
doit
Object subclass: 'SqlExport'
  instVarNames: #( classes counter fileSystem objects
                    objectTableFile path visited)
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

	^self basicNew
		initialize: aPath with: anObject;
		yourself
%
! ------------------- Instance methods for SqlExport
category: 'other'
method: SqlExport
addObject: anObject

	[
		Exception category: GemStoneError number: 2115 do: [ :ex :cat :num :args | ^self ].
		anObject isSpecial ifTrue: [ ^self ].
		anObject isBehavior ifTrue: [ ^self ].
	] value.
	
	(self haveSeen: anObject) ifTrue: [ ^self ].

	objectTableFile
		nextPutAll: 'o_';
		nextPutAll: anObject asOop printString;
		nextPut: Character tab;
		nextPutAll: anObject class name;
		cr.

	(self exportObject: anObject) ifTrue: [

		1 to: anObject _primitiveSize do: [ :i |
			| obj |
			[
				Exception category: GemStoneError number: 2110 do: [ :ex :cat :num :args |].
				obj := anObject _primitiveAt: i.
			] value.
			self 
				try: [self addObject: obj] 
				on: Exception 
				do: [:ex | ex return].
		].
	].
%
category: 'other'
method: SqlExport
exportDictionaryElements: anObject

	| file token |
	
	token := Object new.

	self try: [
		file := self openAppend: path, '/',anObject class name,'_elements.txt' withHeader: [ :f |
			f 
				nextPutAll: 'OOP'; nextPut: Character tab;
				nextPutAll: 'key'; nextPut: Character tab;
				nextPutAll: 'value';
				cr.
		].
		anObject keys do: [ :eachKey |
			| eachValue |
			eachValue := self 
				try: [ anObject at: eachKey ] 
				on: Exception 
				do: [ :ex | ex return: token ].
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

	anObject isBehavior ifTrue: [ ^false ].
	(anObject isKindOf: GsMethod) ifTrue: [ ^false ].
	anObject isSpecial ifTrue: [ self error: 'Did not expect a special here' ].

	(anObject class inheritsFrom: CharacterCollection) ifTrue: [
		self exportStrings: anObject.
		^false
	].
	((anObject class inheritsFrom: SequenceableCollection) and: [(anObject class inheritsFrom: AbstractCollisionBucket) not]) ifTrue: [ 
		self exportSequenceableCollectionElements: anObject.
	].
	(anObject class inheritsFrom: AbstractDictionary) ifTrue: [ 
		self exportDictionaryElements: anObject.
	].
	
	self exportRemainder: anObject.

	^true
%
category: 'other'
method: SqlExport
exportObject: anObject to: aGsFile

	Exception category: GemStoneError number: 2115 do: [ :ex :cat :num :args |
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

	aGsFile nextPutAll: 'o_'.
	anObject asOop printOn: aGsFile.
%
category: 'other'
method: SqlExport
exportRemainder: anObject
	| file |
	
	self try: [
		file := self openAppend: path, '/', anObject class name,'.txt' withHeader: [ :f |
			f nextPutAll: 'OOP'.
			anObject class allInstVarNames do: [ :eachName |
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
		1 to: anObject class allInstVarNames size do: [ :i |
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
		file := self openAppend: path, '/',anObject class name,'_elements.txt' withHeader: [ :f |
			f
				nextPutAll: 'OOP	';
				nextPutAll: 'index	';
				nextPutAll: 'value';
				cr.
		].
		1 to: anObject size do: [ :i |
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
		file := self openAppend: path, '/',anObject class name,'.txt' withHeader: [ :f |
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
		
		anObject do: [ :char |
			| val |
			val := char asciiValue.
			val = 92 ifTrue: [
				file 
					nextPut: val;
					nextPut: $\;
					yourself.
			] ifFalse: [
				(val < 32 or: [ val > 126 ]) ifTrue: [
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
haveSeen: anObject

	| bitIndex byteIndex flag oop |
	oop := anObject asOop.
	oop \\ 4 == 1 ifFalse: [
		self error: 'We don''t understand oops'.
	].
	bitIndex := oop - 1 // 4.
	byteIndex := bitIndex // 8.
	bitIndex := bitIndex \\ 8.
	flag := (visited at: byteIndex) bitAt: bitIndex.
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
	counter := 0.
	fileSystem := aFileSystem.
	path := aPath.
	path last == $/ ifTrue: [ path := path copyFrom: 1 to: path size - 1 ].
	visited := ByteArray new: 10000000.
	objectTableFile := self openAppend: path, '/object_table.txt' withHeader: [ :f |
			f 
				nextPutAll: 'OOP	';
				nextPutAll: 'ClassName';
				cr;
				nextPutAll: 'o_';
				nextPutAll: nil asOop printString;
				nextPut: Character tab;
				nextPutAll: nil class name;
				cr.
		].
	self addObject: aGlobal.
	objectTableFile close.
	"self exportObjectTable."
%
category: 'other'
method: SqlExport
initialize: aPath with: aFileSystem
	
	fileSystem := aFileSystem.
	path := aPath.
	path last == $/ ifTrue: [ path := path copyFrom: 1 to: path size - 1 ].
	visited := ByteArray new: 10000000.
	objectTableFile := self openAppend: path, '/object_table.txt' withHeader: [ :f |
			f 
				nextPutAll: 'OOP	';
				nextPutAll: 'Class Name';
				cr;
				nextPutAll: 'o_';
				nextPutAll: nil asOop printString;
				nextPut: Character tab;
				nextPutAll: nil class name;
				cr.
		].
	self addObject: UserGlobals.
	"self exportObjectTable."
%
category: 'other'
method: SqlExport
openAppend: aFilePath withHeader: aBlock

	| file hasHeader |

	hasHeader := GsFile existsOnServer: aFilePath.
	file := GsFile openAppendOnServer: aFilePath.
	file isNil ifTrue: [self error: GsFile serverErrorString].
	hasHeader ifFalse: [ aBlock value: file ].

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
	Exception category: GemStoneError number: nil do: [ :ex :cat :num :args |
		System addAllToStoneLog: 'In ensure got error number ' , num printString.
		ensureBlock value.
		^nil
	].
	
	result := tryBlock value.
	ensureBlock value.
	^result
%
category: 'other'
method: SqlExport
try: tryBlock on: exception do: catchBlock

	Exception category: GemStoneError number: nil do: [ :ex :cat :num :args |
		Exception category: GemStoneError number: nil do: [ :ex :cat :num :args | ^nil ].
		System addAllToStoneLog: 'In try got error number ' , num printString.
		^catchBlock value
	].
	^tryBlock value
%
