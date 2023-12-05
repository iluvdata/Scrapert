self.RemotePDFTools = (() => {
  function upload() {
    clearErr();
    let files = $("#xpertfiles").get(0);
    let formData = new FormData();
    for (let i = 0; i < files.files.length; i++) {
      const file = files.files[i];
      if(file.type !== "application/pdf") throw "Must be pdf";
      formData.append("file" + i, file);
    }
    RemoteFetch("upload", { method: "POST", body: formData})
    .then(async response => {
      xpert = await response.json();
      if(xpert.message)  showErr(xpert.message.msg, xpert.message.err);
      updateTable(xpert.results);
      const sn = await Data.getPID(xpert.results.map(e => e.sample_id));
      Data.updateCRF(xpert.results.map(e => e.cartridge_sn)).then(() => {
        $(".file-message").text("or drag and drop files here");
        $("#processbtn").prop("disabled", true);
        $("#fileform").trigger("reset");
      });
    }).catch(error => console.error(error));
  }
  function getPDF(sn) {
    window.open("pdf?sn=" + sn, "scrapert");
  }
  function dlPDF(sn) {
    window.location = "pdf?dl=true&sn=" + sn;
  }
  return {
    upload: upload,
    getPDF: getPDF,
    dlPDF: dlPDF
  };
})();
self.LocalPDFTools = (() => {
  function upload() {
    clearErr();
    let xpertPromises = [];
    for(const file of $("#xpertfiles").get(0).files) {
      if(file.type !== "application/pdf") throw "Must be pdf";
      xpertPromises.push(fileReaderP(file));
    }
    Promise.all(Array.from($("#xpertfiles").get(0).files).map((file, fileindex) => {
      if(file.type !== "application/pdf") throw "Must be pdf";
      return fileReaderP(file, fileindex);
    })).then((result) => { // Files are loaded as array of arrayBuffers
      return Promise.all(
        result.map(res => {
          return pdfExtractTxt(res.ab).then(pages => {
            isXpert = pages.map(e => e.includes("Xpert HPV HR_16_18-45"));
            if (!isXpert.some(e => e)) throw "Not a valid xpert result pdf";
            return { 
              fileindex: res.fileindex,
              pages: pages.map((e,i) => {
                return {
                  xpertPage: isXpert[i],
                  xpert: e,
                };
              }) // end pages.map
            }; // End return
          }); // end pdfExtractTxt then
        }) // end result.map
      ); // 2nd promise all
    }).then(async (result) => {
      // parse
      result = result.map(o => {
        return {
          fileindex: o.fileindex,
          xpert: o.pages.map(e => {
            return e.xpertPage ? parseXpert(e.xpert) : null })
        }; });
      // Now try and load the pdfs
      result = Promise.all(
        await result.map(async (res) => {
          const file = await fileReaderP($("#xpertfiles").get(0).files[res.fileindex]);
          const pdf = await PDFLib.PDFDocument.load(file.ab);
          return Promise.all(
            res.xpert.map(async (xpertresult, i) => {
              if (xpertresult) {
                const xpertpdf = await PDFLib.PDFDocument.create();
                const page = await xpertpdf.copyPages(pdf, [i]);
                xpertpdf.addPage(page[0]);
                xpertresult.pdf = await xpertpdf.save();
                return xpertresult;
              } else return null;
            })
          ).then(xpert => {return xpert});
        })
      ).then(async (xpert) => {
        xpert =  xpert.flat().filter(x => x !== null);
        LocalData.write(xpert).then((ids) => {
          updateTable(xpert);
          // update the pids
          REDCap.getPID(xpert.map(e => {return e.sample_id})).then(sn => {
            // upload the crfs
            REDCap.updateCRF(sn).then(() => {
              $(".file-message").text("or drag and drop files here");
              $("#processbtn").prop("disabled", true);
              $("#fileform").trigger("reset");
            });
          });
        }).catch((e) => {
          if (e.name === "ConstraintError") showErr("Unable to import. Duplicate results.", e.name + ": " + e.message);
          else showErr(e.message, e.name);
        });
      });
    });
  }
  function pdfExtractTxt(pdfArray) {
    let loadingTask = pdfjsLib.getDocument(pdfArray);
    return loadingTask.promise.then((pdf) => {
      // Loop over pages
      var countPromises = [];
      for(let cPage = 1; cPage <= pdf.numPages; cPage++) {
        countPromises.push(
          pdf.getPage(cPage).then((page) => {
            return page.getTextContent().then((txt) => {
              txt = txt.items.map((item) => {
                return { 
                  str: item.str,
                  x: item.transform[4],
                  y: item.transform[5]
                };
              });
              txt.sort((a, b) => {
                let y = b.y - a.y;
                return y != 0 ? y : a.x - b.x;
              });
              txt = txt.reduce((acc, e, i, arr) => {
                let prev = arr[i-1];
                return acc += (prev !== undefined && prev.y != e.y ? "\n" : " ") + e.str;
              }, "");
              return txt;
            }); // end gettexcontent promise
          }) // end get page promise
        ); // end countPromises push
      } // End for pages
      return Promise.all(countPromises).then((values) => {return values});
    });
  }
  function fileReaderP(file, fileindex) {
    return new Promise((resolve, reject) => {
      var fr = new FileReader();  
      fr.onload = (e) => {
        resolve({ ab: e.target.result, fileindex: fileindex});
      };
      fr.onerror = reject;
      fr.readAsArrayBuffer(file);
    });
  }
  function keyed_value(key, txt) {
    return txt.match(new RegExp(`(?<=${key.replaceAll(/\s/g,"\\s")}:\\s{1,30})\\w+`))[0];
  }
  function keyed_ts(key, txt, tz) {
    return new luxon.DateTime.fromFormat(txt.match(new RegExp(`(?<=${key.replaceAll(/\s/g,"\\s")}:\\s{1,30})\\S{8}\\s\\S{8}`))[0],
      "MM/dd/yy hh:mm:ss", { zone: tz}).toJSDate();
  }
  function parseXpert(xpert) {
    // parse the results table
    tab = xpert.match(/SAC[\S\s\n]+(?=\n{1}User:)/)[0].replaceAll(/(?<=HPV)\s(?=\d{2})/g, "_");
    tab = tab.split("\n").map(e => e.split(/\s+/))
      .map(e => {
        obj = {};
        obj[`${e[0]}_result`] = e[3];
        obj[`${e[0]}_ct`] = Number.parseFloat(e[1]);
        return obj; 
      })
      .reduce((acc, e) => { return {...acc, ...e}});
    tab = {...tab,
      sample_id: keyed_value("Sample ID\\*?", xpert),
      test_result: xpert.match(/(?<=Test\sResult:\s+)HPV[\S\s\n]+(?=\n\-\nAnalyte)/)[0].replaceAll(/[\n\s{2,}]/g, " "),
      status: keyed_value("Status", xpert),
      error: keyed_value("Error Status", xpert),
      error_message: xpert.match(/(?<=Errors\n)\S+/)[0],
      start_time: keyed_ts("Start Time", xpert, config.expertTZ),
      end_time: keyed_ts("End Time", xpert, config.expertTZ),
      instrument_sn: keyed_value("Instrument S/N", xpert),
      cartridge_sn: keyed_value("Cartridge S/N\\*?", xpert),
      reagant_lot: keyed_value("Reagent Lot ID\\*?", xpert),
      notes: keyed_value("Notes", xpert)
    };
    return tab;
  }
  async function getPDFLink(sn) {
    const xpert = (await LocalData.dbGetAll([sn]))[0];
    let file = undefined;
    if (xpert.uploaded) {
      file =  await REDCap.getPDF(xpert.crf_id).catch(e => {
        clearErr();
        showErr("REDCap: Unable to download PDF file", e.message);
        throw new Error("Unable to get PDF file");
      });
    } else file = new Blob([xpert.pdf], {type: "application/pdf"});
    if (file === undefined) throw new Error("Unable to get PDF file");
    let element = document.createElement('a');
    element.setAttribute('href', URL.createObjectURL(file));
    element.setAttribute("scrapert-file", xpert.sample_id + ".pdf");
    element.style.display = 'none';
    return element;
  }
  function getPDF(sn) {
    getPDFLink(sn).then(element => {
      element.setAttribute('target', "scrapertjs");
      element.click();
      element.remove();
    });
  }
  function dlPDF(sn) {
    getPDFLink(sn).then(element => {
      element.setAttribute('download', element.getAttribute("scrapert-file"));
      element.click();
      element.remove();
    });
  }
  return{
    getPDF: getPDF,
    dlPDF: dlPDF,
    upload: upload
  }
})();