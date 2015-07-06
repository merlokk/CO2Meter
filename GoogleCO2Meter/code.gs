function doGet(e) {
  var template = HtmlService.createTemplateFromFile('Index');
  
  // Build and return HTML in IFRAME sandbox mode.
  return template.evaluate()
      .setSandboxMode(HtmlService.SandboxMode.IFRAME)
      .setTitle('CO2Meter panel');
}

function CheckLibs(){
  if (typeof DriveApp === 'undefined') {
    throw new Error('API DriveApp is undefined');
  }
  if (typeof SpreadsheetApp === 'undefined') {
    throw new Error('API SpreadsheetApp is undefined');
  }
}

function getForecast(){
  var response = UrlFetchApp.fetch('http://api.openweathermap.org/data/2.5/forecast/daily?id=706483&type=accurate&lang=ru&units=metric');
  
  if (response.getResponseCode() == 200){
    Logger.log('Get weather forecast OK');
    return response.getContentText();
  } else {
    Logger.log('Get weather forecast error:' + response.getResponseCode());
    return '{}';
  }
}

function getContent() {
  CheckLibs();
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

function GetSpreadsheet(date){
  var spreadsheet;

  var folders = DriveApp.getFoldersByName('CO2Meter'); 
  if (folders.hasNext()){
    var folder = folders.next();
    var fileName = Utilities.formatDate(date, "UTC", 'YYYY_MM');
//    fileName = 'C' + fileName;
    var files = folder.getFilesByName(fileName);
    if (files.hasNext()) {
      var file = files.next();
      spreadsheet = SpreadsheetApp.open(file);
      Logger.log('Opened spreadsheet: %s', spreadsheet.getId());
      return spreadsheet;
    } else {
      Logger.log('Cant found a file "%s"!', fileName);
      throw new Error('Cant found a file "' + fileName + '"!');
    }
  } else {
    Logger.log('Cant found a folder');
    throw new Error('Cant found a folder');
  }
  
  return spreadsheet; 
}

function deleteDuplicates(intDate, deleteDuplicates){
  CheckLibs();

  date = AZToDate(intDate); 
  var ss = GetSpreadsheet(date);
  
  var sheet = ss.getSheetByName("CO2Data");
  ss.setActiveSheet(sheet);
  
  // get range withouth header row (id=1)
  var range = sheet.getRange(2, 1, sheet.getLastRow() - 1, 5);
  Logger.log('range: %s, %s', range.getA1Notation(), range.getHeight());
  
  // sort by column "InternalDate"
  range.sort({column: 2, ascending: true});
  
  // find duplicates
  var duplicates = [];
  var rval = range.getValues();
  for(var i = 1; i < rval.length; i++) {
    if (rval[i][1] == rval[i - 1][1]) {
      duplicates.push(i);
    }
  }
  
  // delete duplicates (or mark)
  for (var i = duplicates.length - 1; i >= 0; --i) {
    Logger.log('err: %s', duplicates[i]);

    if (deleteDuplicates){
      // delete duplicates
      sheet.deleteRow(duplicates[i] + 1);
    } else {
      var cell = range.getCell(duplicates[i], 2);
      cell.setBackground("red");
    }
  };

  // save all the work 
  SpreadsheetApp.flush();
  
  return duplicates.length;
}

function AZToDate(intDate){
  return new Date(Date.UTC(2000, 00, 01) + intDate * 1000);
}

function MakeMesJSON(row){
  return '{"date":"' + row[0] + '","idate":"' + row[1] + '","temperature":"' + row[2] + '","co2level":"' + row[4] + '","humidity":"' + row[3] + '"}';
}

function getHistory(intDate1, intDate2, cntPoints) {
  CheckLibs();

  var res = [];

  var dates = [];
  var date1 = AZToDate(intDate1); 
  var date2 = AZToDate(intDate2); 
  Logger.log(intDate1.toString() + ' ' + typeof(date1) + ':' + date1);
  
  var vdate = date1;
  vdate.setUTCDate(02);
  dates.push(vdate);

  while ((vdate.getMonth() != date2.getMonth()) || (vdate.getFullYear() < date2.getFullYear())) {
    dates.push(new Date(vdate));
    vdate.setUTCMonth(vdate.getUTCMonth() + 1);
  }
  
  dates.forEach(function(date) {
    try {
      var ss = GetSpreadsheet(date);
  
      var sheet = ss.getSheetByName("CO2Data");
      ss.setActiveSheet(sheet);
  
      var range = sheet.getRange(2, 1, sheet.getLastRow() - 1, 5);
  
      var rval = range.getValues();
      for(var i = 1; i < rval.length; i++) {
        if ((rval[i][1] > intDate1) && (rval[i][1] < intDate2)) {
          res.push(rval[i]);
        }
      }
      Logger.log('data (' + Utilities.formatDate(date, "UTC", 'YYYY_MM') + ') len:' + res.length);
    }
    catch(err) { 
      Logger.log('Error getting month(' + Utilities.formatDate(date, "UTC", 'YYYY_MM_dd') + '): ' + err.message);
    }
  });
  
  // sort data
  res.sort(function(a, b){return a[1] - b[1]});
  
  // if we need to remove some data
  if (res.length <= cntPoints) {
    return res;
  } else {
    // delete some data here
    var fres = [];
    var sum = 0.0;
    var delta = res.length / cntPoints;
    
    for(var i = 1; i < res.length; i++) {
      if (i > sum) {
        fres.push(res[i]);
        sum = sum + delta;
      }
    }
  
    return fres;
  }
}
