var page = require('webpage').create();
page.open('http://financials.morningstar.com/income-statement/is.html?t=00700&region=hkg&culture=en-US', function () {
                   console.log(page.content); //page source
                   phantom.exit();
                   });
