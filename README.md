# GemStone Extract
Extract data from a GemStone database in (very) basic table format. This tool was created to run in a 5.1.5.1 system, so uses the legacy exception handling approach.

# Algorithm

```
initialize:to:with:
  open object_table.txt
  addObject
    ignore if security error, special, or behavior
    ignore if have seen
    add to object_table
    exportObject
      ignore behavior, method, or special
      exportStrings
      exportSequenceableCollectionElements
      exportDictitonaryElements
      exportRemainder
        open class name file
        for each named instance variable
          exportObject:to:
            print Integer, Character, Boolean, nil, object
        close class name file
    for each named and numbered instance variable
      addObject
  close object_table.txt
```
