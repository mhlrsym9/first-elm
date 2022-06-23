var fs = require('fs');
console.log('Incrementing build number...');
fs.readFile('src/metadata.json',function(err,content) {
    if (err) throw err;
    var metadata = JSON.parse(content);
    metadata.qaBuildRevision = metadata.qaBuildRevision + 1;
    metadata.buildRevision = metadata.qaBuildRevision;
    fs.writeFile('src/metadata.json',JSON.stringify(metadata),function(err){
        if (err) throw err;
        console.log(`Current QA build number: ${metadata.buildMajor}.${metadata.buildMinor}.${metadata.qaBuildRevision} ${metadata.buildTag}`);
    })
});