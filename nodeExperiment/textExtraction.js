var fs = require('fs');

var path = "table.txt"

var file = fs.readFileSync(path, "utf8");

file = file.split("\\n")

console.log(file);

for (var i = 0; i < file.length; i++) {

  var line = file[i]


  var line_buffer = ""
  var data_buffer = ""

  for (var j = 0; j < line.length; j++) {
    line_buffer += line[j]

    // we are reading a potential piece of data or heading.
    if ( line[j] != " " ){
      data_buffer += line[j]
    }

    console.log(line_buffer)

  }

}
