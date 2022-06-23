var fs = require('fs');
console.log('Incrementing build number...');
fs.readFile('src/metadata.json',function(err,content) {
    if (err) throw err;
    var metadata = JSON.parse(content);
    if (metadata.prodBuildRevision < metadata.qaBuildRevision) {
        metadata.prodBuildRevision = metadata.qaBuildRevision;
    }
    else {
        metadata.prodBuildRevision = metadata.prodBuildRevision + 1;
        metadata.qaBuildRevision = metadata.prodBuildRevision;
    }
    metadata.buildRevision = metadata.prodBuildRevision;
    fs.writeFile('src/metadata.json',JSON.stringify(metadata),function(err){
        if (err) throw err;
        console.log(`Current build number: ${metadata.buildMajor}.${metadata.buildMinor}.${metadata.buildRevision} ${metadata.buildTag}`);
    })
});