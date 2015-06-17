function doGet(e) {
  var template = HtmlService.createTemplateFromFile('Index');
  
  // Build and return HTML in IFRAME sandbox mode.
  return template.evaluate()
      .setSandboxMode(HtmlService.SandboxMode.NATIVE)
      .setTitle('CO2Meter panel');
}

function getContent() {
  var file;
  var content = '{"date":"","idate":"0","temperature":"0","co2level":"0","humidity":"0"}';
 
  var folders = DriveApp.getFoldersByName('CO2Meter'); 
  if (folders.hasNext()){
    var folder = folders.next();
    var files = folder.getFilesByName('current');
    if (files.hasNext()) {
      file = files.next();
      content = file.getAs(MimeType.PLAIN_TEXT).getDataAsString();
    }
  }
      
//  Logger.log('cnt: %s', content);
  return content;
}
